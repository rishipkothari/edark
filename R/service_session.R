#' Session Save and Load Service
#'
#' Pure functions (no Shiny) behind session files (§M8): describe the input
#' dataset, build a session list from the app's state, read and validate a
#' session file, check that it matches the dataset, and rebuild the Prepare
#' working dataset from it. The Shiny side is in `module_session.R`.
#'
#' A session stores decisions, not results. It is a plain named list saved as
#' `.edark.rds`: no functions, no environments, and nothing from it is ever
#' run as code.
#'
#' @name service_session
#' @keywords internal
NULL


# Bump when the file's structure changes. Pre-release there are no migrations:
# change the structure and this number together (see CLAUDE.md, Coding
# philosophy). A file with a higher number is refused.
.SESSION_SCHEMA_VERSION <- 1L

.SESSION_EXT <- ".edark.rds"

.SESSION_MSG_INVALID  <- "This file is not a valid EDARK session."
.SESSION_MSG_MISMATCH <- "This session does not match this dataset."


# Signal a session error: a plain condition with a user-facing message, so the
# module can show conditionMessage() as is.
.session_error <- function(msg) {
  stop(structure(
    class = c("edark_session_error", "error", "condition"),
    list(message = msg, call = NULL)
  ))
}


#' Describe a dataset as it was passed in
#'
#' Each column's R class as given to `edark()`, before the launch casts
#' (`cast_column_types()`). The input class decides whether a saved transform
#' or filter still lands, so it is what a session is matched on (§M8.4).
#'
#' @param dataset The data frame passed to `edark()`.
#' @return A named character vector, column -> class (several classes joined
#'   with "/", e.g. `"POSIXct/POSIXt"`), in column order.
#' @keywords internal
dataset_definition <- function(dataset) {
  vapply(dataset, function(x) paste(class(x), collapse = "/"), character(1))
}


#' Short hash of a dataset definition
#'
#' A hash of the column names and input classes, not of the data: extra or
#' different rows never change it. Column order does not matter.
#'
#' @param definition A named character vector from `dataset_definition()`.
#' @return A 16-character string.
#' @keywords internal
dataset_signature <- function(definition) {
  def <- definition[order(names(definition))]
  substr(digest::digest(as.list(def), algo = "sha256"), 1L, 16L)
}


#' Build a session list from the app's state
#'
#' @param dataset_input The data frame passed to `edark()` (before casting).
#' @param column_types EDARK types of the cast original dataset
#'   (`shared_state$original_column_types`).
#' @param prepare `shared_state$last_applied_specs` - the last Apply.
#' @param analysis_spec `shared_state$analysis_spec`, or `NULL` before Start
#'   Analysis.
#' @param custom_report_items `shared_state$custom_report_items`.
#' @param include_data Logical. Store `dataset_input` in the file.
#' @return A plain named list (§M8.3).
#' @keywords internal
build_session <- function(dataset_input, column_types, prepare, analysis_spec,
                          custom_report_items = list(), include_data = FALSE) {
  definition <- dataset_definition(dataset_input)
  list(
    session_schema_version = .SESSION_SCHEMA_VERSION,
    edark_version          = EDARK_VERSION,
    saved_at               = Sys.time(),
    dataset_definition = list(
      columns      = definition,
      signature    = dataset_signature(definition),
      column_types = unlist(column_types)
    ),
    prepare = list(
      included_columns       = prepare$included_columns,
      column_type_overrides  = prepare$column_type_overrides  %||% list(),
      column_transform_specs = prepare$column_transform_specs %||% list(),
      row_filter_specs       = prepare$row_filter_specs       %||% list()
    ),
    analysis            = .session_analysis_block(analysis_spec),
    custom_report_items = .session_pack_items(custom_report_items),
    data                = if (isTRUE(include_data)) as.data.frame(dataset_input) else NULL
  )
}


# The Analyze part of a session: Step 1 roles, model purpose and validation
# settings, and the Step 4 covariates if Step 4 was used. NULL before Start
# Analysis. reference_levels holds Step 1's levels overlaid with Step 4's, as
# the spec does; Step 1 restores them and Step 4 reads them from the spec.
.session_analysis_block <- function(spec) {
  if (is.null(spec)) return(NULL)
  vr <- spec$variable_roles
  list(
    roles = list(
      outcome_variable     = vr$outcome_variable,
      exposure_variable    = vr$exposure_variable,
      candidate_covariates = vr$candidate_covariates,
      cluster_variables    = vr$cluster_variables,
      reference_levels     = vr$reference_levels %||% list()
    ),
    purpose_specification = spec$purpose_specification,
    validation_settings   = spec$validation_settings,
    final_model_covariates = vr$final_model_covariates
  )
}


# Custom report items hold their thumbnail as a temp file path. The file goes
# into the session as PNG bytes and is written back to a new temp file on load.
.session_pack_items <- function(items) {
  lapply(items, function(it) {
    path <- it$thumb_path
    it$thumb_png <- if (!is.null(path) && nzchar(path) && file.exists(path))
      readBin(path, "raw", n = file.size(path)) else NULL
    it$thumb_path <- NULL
    it
  })
}

.session_unpack_items <- function(items) {
  lapply(items, function(it) {
    path <- ""
    if (is.raw(it$thumb_png) && length(it$thumb_png) > 0L) {
      path <- tempfile(pattern = "edark_thumb_", fileext = ".png")
      writeBin(it$thumb_png, path)
    }
    it$thumb_png  <- NULL
    it$thumb_path <- path
    it
  })
}


#' File name for a saved session
#' @keywords internal
session_file_name <- function(time = Sys.time()) {
  paste0("edark_session_", format(time, "%Y-%m-%d_%H%M%S"), .SESSION_EXT)
}


#' Read and validate a session file
#'
#' Refuses (with an `edark_session_error`) a file that cannot be read, does not
#' have the session structure, holds functions or environments, or was saved
#' by a newer schema.
#'
#' @param path Path to a `.edark.rds` file.
#' @return The session list.
#' @keywords internal
read_session <- function(path) {
  s <- tryCatch(readRDS(path), error = function(e) NULL)
  validate_session(s)
  s
}


#' @rdname read_session
#' @param s A session list.
#' @keywords internal
validate_session <- function(s) {
  if (!is.list(s) || is.null(s$session_schema_version) ||
      !is.numeric(s$session_schema_version) ||
      length(s$session_schema_version) != 1L || is.na(s$session_schema_version)) {
    .session_error(.SESSION_MSG_INVALID)
  }
  if (s$session_schema_version > .SESSION_SCHEMA_VERSION) {
    .session_error(sprintf(
      "This session was saved with a newer version of EDARK (%s). Update EDARK to load it.",
      s$edark_version %||% "unknown"))
  }
  cols <- s$dataset_definition$columns
  ok <- is.character(cols) && length(cols) > 0L && !is.null(names(cols)) &&
        is.list(s$prepare) &&
        all(c("included_columns", "column_type_overrides",
              "column_transform_specs", "row_filter_specs") %in% names(s$prepare)) &&
        (is.null(s$analysis) || is.list(s$analysis)) &&
        (is.null(s$data) || is.data.frame(s$data)) &&
        !.session_has_code(s[setdiff(names(s), "data")])
  if (!ok) .session_error(.SESSION_MSG_INVALID)
  invisible(TRUE)
}


# TRUE if anything in x is a function, environment or language object. A
# session is plain data; anything else did not come from build_session().
.session_has_code <- function(x) {
  if (is.function(x) || is.environment(x) || is.language(x)) return(TRUE)
  if (is.list(x)) {
    for (el in x) if (.session_has_code(el)) return(TRUE)
  }
  FALSE
}


#' Check a session against the current dataset
#'
#' A session loads only if the dataset has exactly the same columns with the
#' same input classes (§M8.4) and the launch casts read every column the same
#' way. There are no partial loads.
#'
#' @param s A session list.
#' @param dataset_input The data frame passed to `edark()`.
#' @param column_types EDARK types of the cast original dataset.
#' @return `NULL` if it matches, otherwise a character vector of reasons.
#' @keywords internal
session_dataset_mismatch <- function(s, dataset_input, column_types) {
  saved <- s$dataset_definition$columns
  now   <- dataset_definition(dataset_input)
  out   <- character(0)

  missing <- setdiff(names(saved), names(now))
  extra   <- setdiff(names(now), names(saved))
  common  <- intersect(names(saved), names(now))
  changed <- common[saved[common] != now[common]]

  .list <- function(x) paste(utils::head(x, 5L), collapse = ", ")
  .more <- function(x) if (length(x) > 5L) sprintf(" and %d more", length(x) - 5L) else ""
  if (length(missing)) out <- c(out, sprintf("Missing column(s): %s%s", .list(missing), .more(missing)))
  if (length(extra))   out <- c(out, sprintf("Column(s) not in the session: %s%s", .list(extra), .more(extra)))
  if (length(changed)) {
    out <- c(out, sprintf("Different type: %s%s",
                          .list(sprintf("%s (%s, was %s)", changed, now[changed], saved[changed])),
                          .more(changed)))
  }
  if (length(out)) return(out)

  # Same input classes, but a character column can still be cast differently:
  # to factor with few unique values, left as character with many
  # (cast_column_types(), max_factor_levels). A saved spec for it would not land.
  saved_types <- s$dataset_definition$column_types
  now_types   <- unlist(column_types)
  if (!is.null(saved_types)) {
    both <- intersect(names(saved_types), names(now_types))
    cast <- both[saved_types[both] != now_types[both]]
    if (length(cast)) {
      out <- c(out, sprintf("Read as a different type at launch: %s%s",
                            .list(sprintf("%s (%s, was %s)", cast, now_types[cast], saved_types[cast])),
                            .more(cast)))
    }
  }
  if (length(out)) out else NULL
}


#' Rebuild the Prepare working dataset from a session
#'
#' Runs the same pipeline Apply does, on `dataset_original`, with the session's
#' specs. Refuses a session whose transforms are invalid on this data or whose
#' filters leave no rows - nothing is skipped or adjusted.
#'
#' @param s A session list.
#' @param dataset_original The cast original dataset.
#' @return The working dataset.
#' @keywords internal
session_prepare_dataset <- function(s, dataset_original) {
  p <- s$prepare
  invalid <- .find_invalid_transforms_in(p$column_transform_specs %||% list(),
                                         dataset_original)
  if (length(invalid)) {
    .session_error(paste0(
      "The session's transforms do not fit this data: ",
      paste(sprintf("%s (%s)", names(invalid), unlist(invalid)), collapse = "; "), "."))
  }
  state <- list(
    dataset_original       = dataset_original,
    column_type_overrides  = p$column_type_overrides  %||% list(),
    included_columns       = p$included_columns,
    column_transform_specs = p$column_transform_specs %||% list(),
    row_filter_specs       = p$row_filter_specs       %||% list()
  )
  df <- tryCatch(apply_prepare_pipeline(state), error = function(e) {
    .session_error(paste("The session's data preparation failed on this data:",
                         conditionMessage(e)))
  })
  if (nrow(df) == 0L) .session_error("The session's row filters leave no rows in this data.")
  df
}
