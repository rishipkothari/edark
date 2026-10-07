#' Export Service
#'
#' Pure (no Shiny) assembly of the export zip (PRD/BUILD_Export.md): the item
#' registry that lists every file the zip can hold and whether it can be
#' exported now, the per-kind file writers, the per-folder notes documents, and
#' a build job that writes one file per step so the page can show progress and
#' cancel. The compiled report is in \code{service_export_report.R}.
#'
#' Export reads, never recomputes (X13): every table, figure and number comes
#' from \code{analysis_result} / the Prepare state; only formatting happens
#' here (data frame to flextable, ggplot to PNG).
#'
#' Every function takes \code{st}, a plain list snapshot of the app state -
#' see \code{export_state()} - so all of it runs without a Shiny session.
#'
#' @name service_export
NULL


# ── Constants ────────────────────────────────────────────────────────────────

# Zip folders, in tree order, with the label the tree shows.
.EXPORT_FOLDERS <- c(
  data               = "Working dataset",
  reproduce          = "Reproduce",
  table1             = "Table 1",
  variable_selection = "Variable selection",
  model              = "Model",
  diagnostics        = "Diagnostics",
  performance        = "Performance"
)

.EXPORT_DATA_FORMATS <- c(rds = "RDS (R, keeps types)", csv = "CSV",
                          sav = "SPSS (.sav)", dta = "Stata (.dta)", xlsx = "Excel (.xlsx)")
.EXPORT_REPORT_FORMATS <- c(docx = "Word", html = "HTML")

# Figures: inches at 300 dpi unless a plot sizes itself (X5)
.EXPORT_FIG_W   <- 8
.EXPORT_FIG_H   <- 6
.EXPORT_FIG_DPI <- 300

# Where each Analyze group is made, in nav wording
.EXPORT_GROUP_SOURCE <- c(
  table1       = "Analyze \u203a Table 1",
  univariable  = "Analyze \u203a Variables",
  selection    = "Analyze \u203a Variables",
  collinearity = "Analyze \u203a Variables",
  model        = "Analyze \u203a Model \u203a Create",
  results      = "Analyze \u203a Model \u203a Results",
  diagnostics  = "Analyze \u203a Model \u203a Diagnostics",
  performance  = "Analyze \u203a Model \u203a Performance"
)


# ── State snapshot ───────────────────────────────────────────────────────────

#' Snapshot the state the export reads
#'
#' @param dataset_input The data frame passed to \code{edark()} (before casting).
#' @param dataset_original,dataset_working,original_column_types,last_applied_specs,custom_report_items,analysis_data,analysis_spec,analysis_result
#'   The \code{shared_state} fields of the same names.
#' @param has_pending Logical. Prepare has unapplied changes.
#' @param working_sig The sha256 digest of \code{dataset_working}, if the
#'   caller already has it (the page caches it); computed when \code{NULL}.
#' @return A plain list. \code{prepare_changed} is TRUE when the working
#'   dataset no longer matches the dataset Analyze froze (the signature check
#'   Setup uses), which makes every Analyze output stale (O1).
#' @keywords internal
export_state <- function(dataset_input, dataset_original, dataset_working,
                         original_column_types, last_applied_specs,
                         custom_report_items = list(), analysis_data = NULL,
                         analysis_spec = NULL, analysis_result = NULL,
                         has_pending = FALSE, working_sig = NULL) {
  frozen_sig <- analysis_spec$specification_metadata$dataset_signature
  if (!is.null(frozen_sig) && is.null(working_sig) && !is.null(dataset_working)) {
    working_sig <- digest::digest(dataset_working, algo = "sha256")
  }
  prepare_changed <- !is.null(analysis_spec) && !is.null(frozen_sig) &&
    !is.null(working_sig) && !identical(working_sig, frozen_sig)
  list(
    dataset_input         = dataset_input,
    dataset_original      = dataset_original,
    dataset_working       = dataset_working,
    original_column_types = original_column_types,
    last_applied_specs    = last_applied_specs,
    custom_report_items   = custom_report_items,
    analysis_data         = analysis_data,
    analysis_spec         = analysis_spec,
    analysis_result       = analysis_result,
    has_pending           = isTRUE(has_pending),
    prepare_changed       = prepare_changed
  )
}


# ── Item registry ────────────────────────────────────────────────────────────

#' Every file the export zip can hold, and whether it can be exported now
#'
#' One row per file. The tree on the Export page and the build both read this,
#' so what is listed and what is written cannot disagree. Items for outputs
#' that have not been created yet are listed too, with status \code{"not_run"}
#' and the place that creates them; stale outputs are listed as
#' \code{"stale"} and cannot be exported (X9).
#'
#' @param st A snapshot from \code{export_state()}.
#' @param data_format,report_format Chosen formats; they set the extension of
#'   the data file and the report.
#' @return A data.frame: \code{id} (the path inside the zip without the
#'   extension, so a selection survives a format change), \code{path} (the
#'   path inside the zip), \code{folder}
#'   (a name of \code{.EXPORT_FOLDERS}, or \code{""} for the root),
#'   \code{sub} (\code{""}, \code{"tables"} or \code{"figures"}), \code{file}
#'   (file name with extension), \code{kind}, \code{key}, \code{set},
#'   \code{group}, \code{status} (\code{"available"}, \code{"stale"},
#'   \code{"not_run"}, \code{"coming_soon"}), \code{reason} and
#'   \code{default} (ticked when it first becomes available).
#' @export
export_items <- function(st, data_format = "rds", report_format = "docx") {
  spec <- st$analysis_spec
  res  <- st$analysis_result
  gs   <- analysis_output_status(spec, res, st$prepare_changed)

  rows <- list()
  .add <- function(folder, sub, name, ext, kind, group = NA_character_, key = "", set = "",
                   status = NULL, reason = "", default = TRUE) {
    if (is.null(status)) {
      g <- if (is.na(group)) list(status = "available", reason = "") else gs[[group]]
      status <- g$status
      reason <- g$reason
    }
    file <- paste0(name, ".", ext)
    dirs <- c(if (nzchar(folder)) folder, if (nzchar(sub)) sub)
    rows[[length(rows) + 1L]] <<- data.frame(
      id = paste(c(dirs, name), collapse = "/"), path = paste(c(dirs, file), collapse = "/"),
      folder = folder, sub = sub, file = file, kind = kind, key = key, set = set,
      group = group, status = status, reason = reason, default = default,
      stringsAsFactors = FALSE)
  }
  # An output the group ran without (e.g. a Results run that skipped the
  # forest plot): listed, but not created.
  .missing <- function(group, what) {
    g <- gs[[group]]
    if (!identical(g$status, "available")) return(list(status = g$status, reason = g$reason))
    list(status = "not_run", reason = what)
  }
  .add_opt <- function(present, folder, sub, name, ext, kind, group, key = "", set = "", what = "",
                       default = TRUE) {
    if (isTRUE(present)) {
      .add(folder, sub, name, ext, kind, group, key, set, default = default)
    } else {
      m <- .missing(group, what)
      .add(folder, sub, name, ext, kind, group, key, set, m$status, m$reason, default)
    }
  }

  # Root: the compiled report. Always available - it holds whatever is current.
  .add("", "", "analysis_report", report_format, "report")

  # Data and reproduction: describe the working dataset, always available.
  .add("data", "", "working_dataset", data_format, "data")
  .add("reproduce", "", "session", "edark.rds", "session")
  .add("reproduce", "", "prepare_steps", "txt", "prepare_steps")
  # The R script repeats whatever is current; stale steps are left out of it
  .add("reproduce", "", "analysis_script", "R", "script")

  # Table 1: the tables that exist, or the ones the roles will produce
  t1 <- res$result_tables
  t1_keys <- c(overall = "table1_overall", by_exposure = "table1_by_exposure",
               by_outcome = "table1_by_outcome")
  if (identical(gs$table1$status, "available")) {
    t1_keys <- t1_keys[vapply(t1_keys, function(k) !is.null(t1[[k]]), logical(1))]
  } else {
    vr <- spec$variable_roles
    # The tables Table 1 will make: a stratified one only for a groupable variable
    ad <- st$analysis_data
    t1_keys <- t1_keys[c(TRUE, .can_stratify(ad, vr$exposure_variable %||% ""),
                         .can_stratify(ad, vr$outcome_variable %||% ""))]
  }
  for (k in names(t1_keys)) .add("table1", "", t1_keys[[k]], "docx", "table1", "table1", key = t1_keys[[k]])
  .add("table1", "", "table1_notes", "docx", "notes", "table1", key = "table1")

  # Variable selection
  vi <- res$variable_investigation
  .add("variable_selection", "tables", "univariable_screen", "docx", "uni_table", "univariable")
  sel_keys <- intersect(c("stepwise", "lasso"), names(Filter(Negate(is.null), vi[c("stepwise", "lasso")])))
  if (length(sel_keys) == 0L) {
    m <- spec$variable_selection_specification$method
    sel_keys <- if (isTRUE(m %in% c("stepwise", "lasso"))) m else "stepwise"
  }
  for (k in sel_keys) {
    .add("variable_selection", "tables", paste0(k, "_selection"), "docx", "sel_table", "selection", key = k)
  }
  cp <- res$result_plots$collinearity_plots
  .add("variable_selection", "tables", "collinearity_flagged_pairs", "docx", "collin_table", "collinearity")
  collin_ok <- identical(gs$collinearity$status, "available")
  if (!collin_ok || !is.null(cp$cor_matrix)) {
    .add_opt(!is.null(cp$cor_matrix), "variable_selection", "figures", "correlation_heatmap", "png",
             "collin_fig", "collinearity", key = "cor",
             what = "Open Collinearity in Analyze \u203a Variables again to store the matrix")
  }
  if (!collin_ok || !is.null(cp$cramers_v_matrix)) {
    .add_opt(!is.null(cp$cramers_v_matrix), "variable_selection", "figures", "cramers_v_heatmap", "png",
             "collin_fig", "collinearity", key = "cramers",
             what = "Open Collinearity in Analyze \u203a Variables again to store the matrix")
  }
  vs_groups <- c("univariable", "selection", "collinearity")
  .add("variable_selection", "", "variable_selection_notes", "docx", "notes",
       vs_groups[which.max(vapply(vs_groups, function(g) gs[[g]]$status == "available", logical(1)))],
       key = "variable_selection")

  # Model
  rt  <- res$result_tables
  gen <- "Not created - tick it in Model \u203a Results and generate"
  .add_opt(!is.null(rt$main_results), "model", "tables", "results_table", "docx",
           "results_table", "results", what = gen)
  .add_opt(!is.null(rt$fit_statistics), "model", "tables", "fit_statistics", "docx",
           "fit_stats", "results", what = gen)
  .add_opt(!is.null(res$result_plots$coefficient_plot), "model", "figures", "forest_plot", "png",
           "forest_plot", "results", what = gen)
  .add_opt(!is.null(res$methods_paragraph), "model", "", "methods", "docx",
           "methods", "results", what = gen)
  .add("model", "", "model_notes", "docx", "notes", "model", key = "model")
  .add("model", "", "analysis_result", "rds", "result_rds", "model", default = FALSE)

  # Diagnostics
  dg <- res$diagnostics
  .add("diagnostics", "tables", "diagnostic_summary", "docx", "diag_summary", "diagnostics")
  if (is.data.frame(dg$vif)) .add("diagnostics", "tables", "vif", "docx", "diag_table", "diagnostics", key = "vif")
  if (!is.null(dg$influence$top)) {
    .add("diagnostics", "tables", "influential_rows", "docx", "diag_table", "diagnostics", key = "influence")
  }
  if (!is.null(dg$random_effects$components)) {
    .add("diagnostics", "tables", "random_effects", "docx", "diag_table", "diagnostics", key = "random_effects")
  }
  dplots <- Filter(Negate(is.null), res$result_plots$diagnostic_plots %||% list())
  for (k in names(dplots)) .add("diagnostics", "figures", k, "png", "diag_fig", "diagnostics", key = k)
  .add("diagnostics", "", "diagnostics_notes", "docx", "notes", "diagnostics", key = "diagnostics")

  # Performance
  pf <- res$performance
  .add("performance", "tables", "performance_summary", "docx", "perf_summary", "performance")
  op <- pf$sets$bootstrap$optimism
  if (is.data.frame(op) && nrow(op) > 0L) {
    .add("performance", "tables", "bootstrap_optimism", "docx", "perf_optimism", "performance")
  }
  pplots <- res$result_plots$performance_plots %||% list()
  for (s in names(pplots)) {
    for (k in names(Filter(Negate(is.null), pplots[[s]] %||% list()))) {
      .add("performance", "figures", paste0(k, "_", s), "png", "perf_fig", "performance", key = k, set = s)
    }
  }
  .add("performance", "", "performance_notes", "docx", "notes", "performance", key = "performance")

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}


# ── Formatting helpers ───────────────────────────────────────────────────────

# A data frame as a compact report table - the same look as the Explore
# report's section tables (.style_section_ft()), at a size that prints.
.export_ft <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # .style_section_ft() stripes rows, which fails on an empty table
  if (nrow(df) == 0L) df <- data.frame(Note = "None.", stringsAsFactors = FALSE)
  df[] <- lapply(df, function(x) {
    if (is.factor(x)) return(as.character(x))
    if (is.numeric(x) && !is.integer(x)) return(edark_format_est(x))
    x
  })
  ft <- .style_section_ft(df)
  ft <- flextable::fontsize(ft, size = 9, part = "body")
  flextable::fontsize(ft, size = 10, part = "header")
}

# One table, one Word file. Portrait when it fits, landscape when it is wide.
.export_save_table <- function(ft, path) {
  ft <- tryCatch(flextable::autofit(ft), error = function(e) ft)
  w  <- sum(dim(ft)$widths)
  landscape <- is.finite(w) && w > .DOCX_PORTRAIT_WIDTH * 1.15
  ft <- .docx_fit_ft(ft, avail_width = if (landscape) .DOCX_LANDSCAPE_WIDTH else .DOCX_PORTRAIT_WIDTH,
                     autofit = FALSE)
  flextable::save_as_docx(ft, path = path,
                          pr_section = .docx_section_props(if (landscape) "landscape" else "portrait"))
  invisible(path)
}

.export_save_plot <- function(p, path, width = .EXPORT_FIG_W, height = .EXPORT_FIG_H) {
  ggplot2::ggsave(path, plot = p, width = width, height = height, units = "in",
                  dpi = .EXPORT_FIG_DPI, bg = "white")
  invisible(path)
}

# The size a plot needs: some grow with their content (forest plot rows,
# linearity panels, heatmap variables).
.export_plot_size <- function(p, key, st) {
  if (identical(key, "forest_plot")) {
    n <- attr(p, "n_rows") %||% 10L
    return(c(10, min(max(3.5, 1.2 + 0.32 * n), 40)))
  }
  if (identical(key, "linearity_plot")) {
    n <- length(st$analysis_result$diagnostics$linearity$terms)
    return(c(10, max(4, 3.2 * ceiling(max(n, 1L) / 2) + 0.8)))
  }
  if (key %in% c("cor", "cramers")) {
    m <- if (key == "cor") st$analysis_result$result_plots$collinearity_plots$cor_matrix
         else st$analysis_result$result_plots$collinearity_plots$cramers_v_matrix
    s <- max(6, min(0.4 * NROW(m) + 3, 20))
    return(c(s, s))
  }
  c(.EXPORT_FIG_W, .EXPORT_FIG_H)
}

# Column names a stats package will accept: letters, digits and underscores,
# not starting with a digit or underscore, at most `max_len` characters,
# unique. Returns the new names with the renames as an attribute.
.export_safe_names <- function(x, max_len = 32L) {
  y <- gsub("[^A-Za-z0-9_]", "_", x)
  y <- ifelse(grepl("^[A-Za-z]", y), y, paste0("v", y))
  y <- substr(y, 1L, max_len)
  # Names that were already valid keep them; a renamed column yields
  ord <- c(which(y == x), which(y != x))
  y[ord] <- make.unique(y[ord], sep = "_")
  y <- substr(y, 1L, max_len)
  changed <- y != x
  attr(y, "renamed") <- if (any(changed)) data.frame(from = x[changed], to = y[changed],
                                                     stringsAsFactors = FALSE)
  y
}


# ── Data ─────────────────────────────────────────────────────────────────────

# The working dataset, unmodified (X6). Returns the renames a format forced.
.export_write_data <- function(df, path, format) {
  df <- as.data.frame(df)
  renamed <- NULL
  switch(format,
    rds  = saveRDS(df, path),
    csv  = utils::write.csv(df, path, row.names = FALSE, na = ""),
    xlsx = writexl::write_xlsx(df, path),
    sav  = {
      nm <- .export_safe_names(names(df), 64L)
      renamed <- attr(nm, "renamed")
      names(df) <- as.character(nm)
      haven::write_sav(df, path)
    },
    dta  = {
      nm <- .export_safe_names(names(df), 32L)
      renamed <- attr(nm, "renamed")
      names(df) <- as.character(nm)
      # Stata labels factors with value labels; ordered factors become plain
      # labelled integers, which keeps their level order.
      haven::write_dta(df, path, version = 15)
    },
    stop("Unknown data format: ", format, call. = FALSE)
  )
  renamed
}


# The Prepare steps behind the working dataset, as summary sections. Reuses
# Model › Summary's own builder so both describe a preparation the same way.
.export_prepare_sections <- function(st) {
  la   <- st$last_applied_specs %||% list()
  orig <- st$dataset_original
  wd   <- st$dataset_working
  snap <- c(la, list(
    original_columns = names(orig),
    original_dims    = if (!is.null(orig)) c(rows = nrow(orig), cols = ncol(orig)),
    working_dims     = if (!is.null(wd)) c(rows = nrow(wd), cols = ncol(wd))
  ))
  list(.summary_prepare(list(specification_metadata = list(prepare_snapshot = snap))))
}


# Sections as plain text: a title line per section, "label: value" per row,
# items indented beneath.
.export_sections_text <- function(sections) {
  unlist(lapply(sections, function(sec) {
    c(sec$title, strrep("-", nchar(sec$title)),
      unlist(lapply(sec$rows, function(r) {
        lvl <- if (!is.null(r$level) && r$level %in% c("error", "warning")) paste0("[", r$level, "] ") else ""
        c(sprintf("%s%s: %s", lvl, r$label, paste(r$value, collapse = " ")),
          if (length(r$items)) paste0("    - ", r$items))
      })),
      "")
  }))
}


# ── Notes documents (X10) ────────────────────────────────────────────────────
# Each builder returns sections in build_analysis_summary()'s shape
# (list(id, title, rows); rows from .srow()), so the notes, the compiled
# report and Model › Summary read the same values from the same place.

.export_fmt_metric <- function(value, format) {
  if (is.null(value) || is.na(value)) return("-")
  if (identical(format, "yesno")) return(if (isTRUE(value == 1)) "Yes" else "No")
  .ms_fmt_stat(value, format)
}

.export_cap <- function(x) paste0(toupper(substr(x, 1L, 1L)), substring(x, 2L))

.export_messages_section <- function(msgs, title = "Messages") {
  rows <- if (is.null(msgs) || nrow(msgs) == 0L) list(.srow("Messages", "None"))
          else lapply(seq_len(nrow(msgs)), function(i) {
            .srow(.export_cap(msgs$level[i]), msgs$message[i], level = msgs$level[i])
          })
  .ssection("messages", title, rows)
}

.export_notes_table1 <- function(st) {
  spec <- st$analysis_spec
  res  <- st$analysis_result
  t1   <- spec$table1_specification
  vr   <- spec$variable_roles
  .stat <- function(p, smd) if (isTRUE(smd)) "Standardised mean differences" else if (isTRUE(p)) "P-values" else "None"
  tabs <- c(Overall = "table1_overall", `By exposure` = "table1_by_exposure", `By outcome` = "table1_by_outcome")
  tabs <- tabs[vapply(tabs, function(k) !is.null(res$result_tables[[k]]), logical(1))]
  list(
    .ssection("table1", "Table 1", list(
      .srow("Tables", length(tabs), items = names(tabs)),
      .srow("Variables", length(vr$table1_variables), items = vr$table1_variables),
      .srow("Exposure", vr$exposure_variable %||% .none),
      .srow("Outcome", vr$outcome_variable %||% .none),
      if (!is.null(res$result_tables$table1_by_exposure))
        .srow("By exposure: statistic", .stat(t1$include_pvalues_exposure, t1$include_smd_exposure)),
      if (!is.null(res$result_tables$table1_by_outcome))
        .srow("By outcome: statistic", .stat(t1$include_pvalues_outcome, t1$include_smd_outcome)),
      .srow("Rows", format(nrow(st$analysis_data), big.mark = ","),
            items = "Table 1 describes the whole frozen dataset, including any test rows.")
    ))
  )
}

.export_notes_variable_selection <- function(st) {
  spec <- st$analysis_spec
  res  <- st$analysis_result
  vs   <- spec$variable_selection_specification
  vi   <- res$variable_investigation
  gs   <- analysis_output_status(spec, res, st$prepare_changed)
  .excl <- function(ex) if (is.data.frame(ex) && nrow(ex) > 0L) paste0(ex$variable, " - ", ex$reason)
  out <- list()

  pool <- spec$variable_roles$univariable_test_pool
  out[[length(out) + 1L]] <- .ssection("pool", "Candidates", list(
    .srow("Candidate pool", length(pool), items = pool),
    .srow("Held in every model", spec$variable_roles$exposure_variable %||% .none)
  ))

  if (identical(gs$univariable$status, "available")) {
    u   <- vi$univariable
    sug <- unique(u$variable[u$suggested %in% TRUE])
    out[[length(out) + 1L]] <- .ssection("univariable", "Univariable screen", list(
      .srow("P-value threshold", vs$univariable_p_threshold),
      .srow("Variables tested", length(unique(u$variable))),
      .srow("Suggested (p below threshold)", length(sug), items = sug),
      .srow("Excluded", if (is.null(.excl(vi$univariable_excluded))) .none else nrow(vi$univariable_excluded),
            items = .excl(vi$univariable_excluded))
    ))
  }
  if (!is.null(vi$stepwise) && identical(gs$selection$status, "available")) {
    s <- vi$stepwise
    out[[length(out) + 1L]] <- .ssection("stepwise", "Stepwise selection", list(
      .srow("Direction", s$direction %||% vs$stepwise_direction),
      .srow("Criterion", s$criterion %||% vs$stepwise_criterion),
      if (!is.null(s$error)) .srow("Error", s$error, level = "error"),
      .srow("Selected", length(s$selected_variables), items = s$selected_variables),
      .srow("Held", if (length(s$held_variables)) paste(s$held_variables, collapse = ", ") else .none),
      if (!is.null(s$n_used)) .srow("Rows used", sprintf("%s of %s", format(s$n_used, big.mark = ","),
                                                         format(s$n_total, big.mark = ","))),
      if (!is.null(s$final_formula)) .srow("Final formula", paste(deparse(s$final_formula, width.cutoff = 500L), collapse = " ")),
      .srow("Excluded", if (is.null(.excl(s$excluded_variables))) .none else nrow(s$excluded_variables),
            items = .excl(s$excluded_variables))
    ))
  }
  if (!is.null(vi$lasso) && identical(gs$selection$status, "available")) {
    l <- vi$lasso
    out[[length(out) + 1L]] <- .ssection("lasso", "LASSO", list(
      .srow("Lambda rule", l$lambda_type %||% vs$lasso_lambda),
      if (!is.null(l$lambda_selected)) .srow("Lambda", edark_format_est(l$lambda_selected)),
      .srow("Seed (cross-validation folds)", l$seed %||% vs$lasso_seed),
      if (!is.null(l$error)) .srow("Error", l$error, level = "error"),
      .srow("Selected", length(l$selected_variables), items = l$selected_variables),
      .srow("Held", if (length(l$held_variables)) paste(l$held_variables, collapse = ", ") else .none),
      if (!is.null(l$n_used)) .srow("Rows used", sprintf("%s of %s", format(l$n_used, big.mark = ","),
                                                         format(l$n_total, big.mark = ","))),
      .srow("Excluded", if (is.null(.excl(l$excluded_variables))) .none else nrow(l$excluded_variables),
            items = .excl(l$excluded_variables))
    ))
  }
  if (identical(gs$collinearity$status, "available")) {
    fp <- res$result_plots$collinearity_plots$flagged_pairs_table
    out[[length(out) + 1L]] <- .ssection("collinearity", "Collinearity", list(
      .srow("Pairs above 0.7", nrow(fp),
            items = if (nrow(fp)) sprintf("%s - %s: %s %s", fp$var1, fp$var2, fp$type, fp$value))
    ))
  }
  out
}

.export_notes_model <- function(st) {
  spec <- st$analysis_spec
  res  <- st$analysis_result
  rs   <- res$run_status
  fs   <- res$inference_summary$fit_statistics
  refs <- rs$reference_levels
  fm   <- rs$run_messages
  c(
    build_analysis_summary(spec, res, st$analysis_data),
    list(
      .ssection("fit", "Fitted model", list(
        .srow("Model", .ANALYSIS_MODEL_LABELS[[res$specification_snapshot$model_design$model_type]]),
        .srow("Formula", paste(deparse(rs$formula, width.cutoff = 500L), collapse = " ")),
        .srow("Rows used", sprintf("%s of %s", format(rs$n_used, big.mark = ","), format(rs$n_total, big.mark = ","))),
        if (!is.null(rs$outcome_event)) .srow("Modelling", sprintf("%s = %s (vs %s)", rs$outcome_event$variable,
                                                                   rs$outcome_event$event, rs$outcome_event$reference)),
        if (length(refs)) .srow("Reference levels", length(refs),
                                items = paste0(names(refs), ": ", vapply(refs, as.character, character(1)))),
        .srow("Fitted", format(rs$fitted_at, "%Y-%m-%d %H:%M:%S")),
        .srow("Inference", edark_inference_note(res$specification_snapshot$model_design$model_type))
      )),
      if (is.data.frame(fs) && nrow(fs) > 0L) .ssection("fit_statistics", "Fit statistics",
        lapply(seq_len(nrow(fs)), function(i) .srow(fs$label[i], .export_fmt_metric(fs$value[i], fs$format[i])))),
      .export_messages_section(if (is.data.frame(fm) && nrow(fm)) data.frame(level = fm$level, message = fm$message,
                                                                         stringsAsFactors = FALSE),
                               "Fitting messages")
    )
  )
}

.export_notes_diagnostics <- function(st) {
  dg <- st$analysis_result$diagnostics
  m  <- dg$metrics
  secs <- if (is.null(m)) character(0) else intersect(names(.DG_SECTIONS), unique(m$section))
  checks <- analysis_diagnostic_options(dg$model_type)
  ran <- checks$label[checks$id %in% dg$checks]
  out <- list(.ssection("run", "Diagnostics run", list(
    .srow("Checks", length(ran), items = ran),
    .srow("Run", format(dg$run_at, "%Y-%m-%d %H:%M:%S"))
  )))
  for (sec in secs) {
    mm <- m[m$section == sec, , drop = FALSE]
    out[[length(out) + 1L]] <- .ssection(sec, .DG_SECTIONS[[sec]], lapply(seq_len(nrow(mm)), function(i) {
      hint <- unname(.DG_HINTS[mm$key[i]])
      .srow(mm$label[i], .export_fmt_metric(mm$value[i], mm$format[i]),
            items = if (!is.na(hint)) hint,
            level = if (identical(mm$level[i], "warning")) "warning")
    }))
  }
  miss <- dg$sample$missing
  if (is.data.frame(miss) && any(miss$n_missing > 0L)) {
    miss <- miss[miss$n_missing > 0L, , drop = FALSE]
    out[[length(out) + 1L]] <- .ssection("missing", "Missing values by variable", list(
      .srow("Variables with missing values", nrow(miss),
            items = sprintf("%s: %d", miss$variable, miss$n_missing))))
  }
  if (isTRUE(dg$separation$detected)) {
    out[[length(out) + 1L]] <- .ssection("separation", "Separation", list(
      .srow("Terms with infinite estimates", length(dg$separation$terms), items = dg$separation$terms,
            level = "warning")))
  }
  c(out, list(.export_messages_section(dg$messages)))
}

.export_notes_performance <- function(st) {
  pf <- st$analysis_result$performance
  m  <- pf$metrics
  purpose <- st$analysis_spec$purpose_specification$model_purpose %||% "association"
  out <- list(.ssection("run", "Evaluation", list(
    .srow("Model purpose", if (identical(purpose, "prediction")) "Prediction" else "Association"),
    .srow("Validation", .pm_validation_text(pf$validation)),
    if (!is.null(.pm_basis_note(pf))) .srow("Predictions", .pm_basis_note(pf)),
    if (!is.null(pf$split)) .srow("Test set", sprintf("%s other than %s", pf$split$variable, pf$split$training_level),
                                   items = paste("Test levels:", paste(pf$split$test_levels, collapse = ", "))),
    .srow("Run", format(pf$run_at, "%Y-%m-%d %H:%M:%S"))
  )))
  keys <- if (is.null(m)) character(0) else setdiff(unique(m$key), c("auc_low", "auc_high"))
  for (s in names(pf$sets)) {
    v <- pf$sets[[s]]
    rows <- c(
      list(.srow("Rows", paste0(format(v$n, big.mark = ","),
                                if (!is.null(v$n_events)) sprintf(" (%d events)", v$n_events)))),
      if (!is.null(v$n_fits)) list(.srow("Model refits", v$n_fits,
                                         items = if (isTRUE(v$n_failed > 0L)) sprintf("%d skipped", v$n_failed),
                                         level = if (isTRUE(v$n_failed > 0L)) "warning")),
      lapply(keys[vapply(keys, function(k) any(m$set == s & m$key == k), logical(1))], function(k) {
        hint <- unname(.PM_HINTS[k])
        .srow(m$label[m$key == k][1L], .pm_fmt_cell(m, s, k), items = if (!is.na(hint)) hint)
      }),
      list(.srow("About this set", .pm_set_note(s, pf)))
    )
    out[[length(out) + 1L]] <- .ssection(s, v$label, rows)
  }
  c(out, list(.export_messages_section(pf$messages)))
}

.export_notes_sections <- function(key, st) {
  switch(key,
    table1             = .export_notes_table1(st),
    variable_selection = .export_notes_variable_selection(st),
    model              = .export_notes_model(st),
    diagnostics        = .export_notes_diagnostics(st),
    performance        = .export_notes_performance(st),
    stop("Unknown notes document: ", key, call. = FALSE))
}

.EXPORT_NOTES_TITLES <- c(
  table1 = "Table 1 - notes", variable_selection = "Variable selection - notes",
  model = "Model - notes", diagnostics = "Diagnostics - notes", performance = "Performance - notes"
)


# Sections as Word content: a heading per section and a two-column table of
# its rows. Items go beneath the value on their own lines; a warning or error
# row says so in words, since the screen's icons do not survive.
.export_sections_ft <- function(sec) {
  lab <- vapply(sec$rows, function(r) as.character(r$label), character(1))
  val <- vapply(sec$rows, function(r) {
    lvl <- if (!is.null(r$level) && r$level %in% c("error", "warning")) paste0(.export_cap(r$level), ": ") else ""
    v <- paste0(lvl, paste(r$value, collapse = " "))
    if (length(r$items)) v <- paste(c(v, paste0("\u2022 ", r$items)), collapse = "\n")
    v
  }, character(1))
  ft <- flextable::flextable(data.frame(Item = lab, Value = val, stringsAsFactors = FALSE))
  ft <- flextable::font(ft, fontname = "Arial", part = "all")
  ft <- flextable::fontsize(ft, size = 9, part = "all")
  ft <- flextable::delete_part(ft, part = "header")
  ft <- flextable::color(ft, j = 1, color = "#595959")
  ft <- flextable::valign(ft, valign = "top", part = "body")
  ft <- flextable::border_remove(ft)
  ft <- flextable::hline(ft, border = officer::fp_border(color = "#D9D9D9", width = 0.5))
  ft <- flextable::width(ft, j = 1, width = 2.1)
  ft <- flextable::width(ft, j = 2, width = .DOCX_PORTRAIT_WIDTH - 2.1)
  flextable::set_table_properties(ft, layout = "fixed", align = "left")
}

.export_docx <- function() {
  doc <- officer::read_docx(system.file("templates/word_docx_blank_template.docx", package = "edark"))
  tryCatch(officer::body_remove(officer::cursor_begin(doc)), error = function(e) doc)
}

.export_add_sections <- function(doc, sections, style = "heading 1") {
  for (sec in Filter(function(s) length(s$rows) > 0L, sections)) {
    if (nzchar(sec$title)) doc <- officer::body_add_par(doc, sec$title, style = style)
    doc <- flextable::body_add_flextable(doc, .export_sections_ft(sec))
    doc <- officer::body_add_par(doc, "", style = "Normal")
  }
  doc
}

.export_write_notes <- function(key, st, path) {
  doc <- .export_docx()
  doc <- officer::body_add_par(doc, .EXPORT_NOTES_TITLES[[key]], style = "Title")
  doc <- officer::body_add_par(doc, paste("EDARK", EDARK_VERSION, "\u00b7", format(Sys.time(), "%d %B %Y %H:%M")),
                               style = "Subtitle")
  doc <- .export_add_sections(doc, .export_notes_sections(key, st))
  doc <- officer::body_set_default_section(doc, .docx_section_props("portrait"))
  print(doc, target = path)
  invisible(path)
}

.export_write_paragraphs <- function(text, title, path) {
  doc <- .export_docx()
  doc <- officer::body_add_par(doc, title, style = "Title")
  for (p in strsplit(text, "\n\\s*\n")[[1]]) doc <- officer::body_add_par(doc, trimws(p), style = "Normal")
  doc <- officer::body_set_default_section(doc, .docx_section_props("portrait"))
  print(doc, target = path)
  invisible(path)
}


# ── Tables and figures, by kind ──────────────────────────────────────────────

# gtsummary's own Word conversion needs a newer flextable than the newest
# R 4.3 binary (0.9.7/0.9.8) - and fails inside a promise, so tryCatch is not
# reliable. Check the version first; otherwise build the table from gtsummary's
# display tibble: same rows and statistics, spanning headers become one header
# row above the columns they span.
.export_table1_ft <- function(tbl) {
  if (utils::packageVersion("flextable") >= "0.9.11") return(gtsummary::as_flex_table(tbl))
  .md <- function(x) gsub("\\*\\*|__", "", x)
  df  <- as.data.frame(gtsummary::as_tibble(tbl, col_labels = FALSE), stringsAsFactors = FALSE)
  sty <- tbl$table_styling$header
  sty <- sty[match(names(df), sty$column), , drop = FALSE]
  df[] <- lapply(df, function(x) .md(ifelse(is.na(x), "", as.character(x))))
  labels <- stats::setNames(.md(ifelse(is.na(sty$label), names(df), sty$label)), names(df))
  ft <- .style_section_ft(df)
  ft <- flextable::set_header_labels(ft, values = as.list(labels))
  ft <- flextable::fontsize(ft, size = 9, part = "body")
  ft <- flextable::fontsize(ft, size = 10, part = "header")
  sp <- tbl$table_styling$spanning_header
  if (is.data.frame(sp) && nrow(sp) > 0L && "spanning_header" %in% names(sp)) {
    top <- stats::setNames(rep("", ncol(df)), names(df))
    hit <- intersect(sp$column, names(df))
    top[hit] <- .md(sp$spanning_header[match(hit, sp$column)])
    if (any(nzchar(top))) {
      ft <- flextable::add_header_row(ft, values = unname(top), colwidths = rep(1L, ncol(df)))
      ft <- flextable::merge_h(ft, part = "header", i = 1)
      ft <- flextable::bold(ft, part = "header")
      ft <- flextable::align(ft, i = 1, align = "center", part = "header")
    }
  }
  ft
}

.export_univariable_ft <- function(u) {
  em  <- if (identical(u$effect_measure[1L], "odds_ratio")) "OR" else "Coefficient"
  # A factor's term is the variable name followed by its level
  lvl <- mapply(function(v, t, ref) {
    if (is.na(t) || identical(t, v)) return("")
    l <- if (startsWith(t, v)) substring(t, nchar(v) + 1L) else t
    if (is.na(ref)) l else paste0(l, " vs ", ref)
  }, u$variable, u$term, u$reference_level, USE.NAMES = FALSE)
  df <- data.frame(
    Variable  = u$variable,
    Level     = lvl,
    Estimate  = edark_format_ci(u$estimate, u$conf.low, u$conf.high),
    p         = edark_format_p(u$p.value),
    Suggested = ifelse(u$suggested %in% TRUE, "Yes", ""),
    stringsAsFactors = FALSE
  )
  names(df)[3] <- paste0(em, " (95% CI)")
  .export_ft(df)
}

.export_selection_ft <- function(x, key) {
  if (identical(key, "lasso")) {
    cd <- x$coef_data
    if (is.null(cd) || nrow(cd) == 0L) cd <- data.frame(Note = x$error %||% "No variable was selected.")
    else cd <- data.frame(Variable = cd$variable, Term = cd$term, Coefficient = edark_format_est(cd$estimate),
                          stringsAsFactors = FALSE)
    return(.export_ft(cd))
  }
  tr <- x$step_trace
  if (is.null(tr) || NROW(tr) == 0L) return(.export_ft(data.frame(Note = x$error %||% "No steps were taken.")))
  tr <- as.data.frame(tr)
  tr$Step <- trimws(as.character(tr$Step))
  .export_ft(tr)
}

.export_diag_summary_ft <- function(dg) {
  m <- dg$metrics
  if (is.null(m) || nrow(m) == 0L) return(.export_ft(data.frame(Note = "No values computed.")))
  .export_ft(data.frame(
    Section = unname(.DG_SECTIONS[m$section]),
    Measure = m$label,
    Value   = vapply(seq_len(nrow(m)), function(i) .export_fmt_metric(m$value[i], m$format[i]), character(1)),
    Flag    = ifelse(m$level %in% "warning", "Warning", ""),
    stringsAsFactors = FALSE
  ))
}

.export_diag_table_ft <- function(dg, key) {
  switch(key,
    vif = .export_ft(data.frame(Predictor = dg$vif$term, VIF = sprintf("%.2f", dg$vif$vif),
                                Flag = ifelse(dg$vif$flag == "ok", "", dg$vif$flag),
                                stringsAsFactors = FALSE)),
    influence = {
      top <- dg$influence$top
      names(top)[names(top) == ".edark_row_id"] <- "Row ID"
      names(top)[names(top) == "cooks"]         <- "Cook's D"
      names(top)[names(top) == "leverage"]      <- "Leverage"
      names(top)[names(top) == "std_resid"]     <- "Std. residual"
      .export_ft(top)
    },
    random_effects = {
      re <- dg$random_effects$components
      .export_ft(data.frame(`Cluster variable` = re$group, Variance = sprintf("%.3f", re$variance),
                            SD = sprintf("%.3f", re$sd), ICC = sprintf("%.3f", re$icc),
                            Clusters = re$n_groups, `Min rows` = re$min_size,
                            `Median rows` = re$median_size, `Max rows` = re$max_size,
                            check.names = FALSE, stringsAsFactors = FALSE))
    },
    stop("Unknown diagnostics table: ", key, call. = FALSE))
}

# Measure x set of rows, as Model › Performance's Overview shows it
.export_perf_summary_ft <- function(pf) {
  m <- pf$metrics
  if (is.null(m) || nrow(m) == 0L) return(.export_ft(data.frame(Note = "No values computed.")))
  sets <- names(pf$sets)
  keys <- setdiff(unique(m$key), c("auc_low", "auc_high"))
  df <- data.frame(Measure = vapply(keys, function(k) m$label[m$key == k][1L], character(1)),
                   stringsAsFactors = FALSE)
  for (s in sets) df[[pf$sets[[s]]$label]] <- vapply(keys, function(k) .pm_fmt_cell(m, s, k), character(1))
  .export_ft(df)
}

.export_perf_optimism_ft <- function(op) {
  .export_ft(data.frame(Measure = op$label, Apparent = .pm_est_vec(op$apparent),
                        Optimism = .pm_est_vec(op$optimism), Corrected = .pm_est_vec(op$corrected),
                        stringsAsFactors = FALSE))
}
.pm_est_vec <- function(x) vapply(x, .pm_est, character(1))


# ── Writing one item ─────────────────────────────────────────────────────────

# Writes one registry row into `root`. Returns a list of notes for the README
# (e.g. renamed columns), or NULL.
.export_write_item <- function(item, st, opts, root) {
  path <- file.path(root, item$path)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  res <- st$analysis_result

  switch(item$kind,
    data = {
      renamed <- .export_write_data(st$dataset_working, path, opts$data_format)
      if (!is.null(renamed)) return(list(renamed = renamed))
    },
    session = {
      s <- build_session(
        dataset_input       = st$dataset_input,
        column_types        = st$original_column_types,
        prepare             = st$last_applied_specs,
        analysis_spec       = st$analysis_spec,
        custom_report_items = st$custom_report_items %||% list(),
        include_data        = isTRUE(opts$session_include_data)
      )
      saveRDS(s, path)
    },
    script = writeLines(generate_analysis_script(st, utils::modifyList(opts$script %||% list(),
                                                       list(time = Sys.time())))$text,
                        path, useBytes = TRUE),
    prepare_steps = writeLines(c(
      "Data preparation - how the working dataset was made from the input dataset.",
      "Steps run in this order: type overrides, column selection, transforms, row filters.",
      "", .export_sections_text(.export_prepare_sections(st))), path, useBytes = TRUE),
    report = {
      if (identical(opts$report_format, "html")) export_report_html(st, path)
      else export_report_docx(st, path)
    },
    table1        = .export_save_table(.export_table1_ft(res$result_tables[[item$key]]), path),
    notes         = .export_write_notes(item$key, st, path),
    uni_table     = .export_save_table(.export_univariable_ft(res$variable_investigation$univariable), path),
    sel_table     = .export_save_table(.export_selection_ft(res$variable_investigation[[item$key]], item$key), path),
    collin_table  = {
      fp <- res$result_plots$collinearity_plots$flagged_pairs_table
      if (nrow(fp) == 0L) fp <- data.frame(Note = "No pair above 0.7.")
      else names(fp) <- c("Variable 1", "Variable 2", "Measure", "Value")
      .export_save_table(.export_ft(fp), path)
    },
    collin_fig    = {
      cp <- res$result_plots$collinearity_plots
      p  <- if (item$key == "cor") .plot_correlation_heatmap(cp$cor_matrix) else .plot_cramers_heatmap(cp$cramers_v_matrix)
      sz <- .export_plot_size(p, item$key, st)
      .export_save_plot(p, path, sz[1], sz[2])
    },
    results_table = .export_save_table(results_table_flextable(res$result_tables$main_results), path),
    fit_stats     = .export_save_table(.export_ft(res$result_tables$fit_statistics), path),
    forest_plot   = {
      p  <- res$result_plots$coefficient_plot
      sz <- .export_plot_size(p, "forest_plot", st)
      .export_save_plot(p, path, sz[1], sz[2])
    },
    methods       = .export_write_paragraphs(res$methods_paragraph, "Statistical methods", path),
    result_rds    = saveRDS(res, path),
    diag_summary  = .export_save_table(.export_diag_summary_ft(res$diagnostics), path),
    diag_table    = .export_save_table(.export_diag_table_ft(res$diagnostics, item$key), path),
    diag_fig      = {
      p  <- res$result_plots$diagnostic_plots[[item$key]]
      sz <- .export_plot_size(p, item$key, st)
      .export_save_plot(p, path, sz[1], sz[2])
    },
    perf_summary  = .export_save_table(.export_perf_summary_ft(res$performance), path),
    perf_optimism = .export_save_table(.export_perf_optimism_ft(res$performance$sets$bootstrap$optimism), path),
    perf_fig      = .export_save_plot(res$result_plots$performance_plots[[item$set]][[item$key]], path),
    stop("Unknown export item kind: ", item$kind, call. = FALSE)
  )
  NULL
}


# ── README ───────────────────────────────────────────────────────────────────

.EXPORT_FOLDER_NOTES <- c(
  data               = "The working dataset as Prepare left it - unmodified, no columns added.",
  reproduce          = paste("session.edark.rds restores this preparation and analysis setup:",
                             "edark(input_data, session = \"session.edark.rds\"), where input_data is the",
                             "dataset first given to edark(). prepare_steps.txt lists the preparation in words.",
                             "analysis_script.R repeats the preparation and every current analysis step in plain R:",
                             "set the path to the input dataset at its top and run it."),
  table1             = "Table 1 as Word tables, and notes on how it was built.",
  variable_selection = "Univariable screen, stepwise / LASSO selection and collinearity.",
  model              = "Results table, fit statistics, forest plot, methods paragraph and model notes.",
  diagnostics        = "Assumption checks: summary and tables (Word), figures (PNG) and notes.",
  performance        = "Discrimination and calibration per set of rows: tables, figures and notes."
)

.export_readme <- function(st, written, failed, notes, opts, time) {
  di  <- st$dataset_input
  wd  <- st$dataset_working
  mt  <- st$analysis_result$specification_snapshot$model_design$model_type
  sig <- tryCatch(dataset_signature(dataset_definition(di)), error = function(e) "-")
  folders <- intersect(names(.EXPORT_FOLDERS), unique(written$folder))
  renamed <- do.call(rbind, lapply(notes, `[[`, "renamed"))

  c(
    "EDARK export",
    "============",
    "",
    sprintf("Created:        %s", format(time, "%Y-%m-%d %H:%M:%S")),
    sprintf("EDARK version:  %s", EDARK_VERSION),
    sprintf("Input dataset:  %s rows x %s columns (signature %s)",
            format(nrow(di), big.mark = ","), ncol(di), sig),
    sprintf("Working dataset: %s rows x %s columns", format(nrow(wd), big.mark = ","), ncol(wd)),
    if (!is.null(mt)) sprintf("Model:          %s", .ANALYSIS_MODEL_LABELS[[mt]]),
    if (isTRUE(st$has_pending)) "Note: Prepare had unapplied changes; this export uses the last applied state.",
    "",
    "Contents",
    "--------",
    if (any(written$kind == "report"))
      sprintf("%s  Compiled report of everything below that was current at export.",
              written$file[written$kind == "report"][1L]),
    unlist(lapply(folders, function(f) {
      files <- written$path[written$folder == f]
      c("", sprintf("%s/", f), paste0("  ", .EXPORT_FOLDER_NOTES[[f]]),
        paste0("    ", sub(paste0("^", f, "/"), "", files)))
    })),
    "",
    "Tables are Word documents; figures are PNG at 300 dpi.",
    if (isTRUE(opts$session_include_data) && "reproduce/session" %in% written$id)
      "The session file includes the input dataset (patient-level data). Share only where data governance allows.",
    if ("data" %in% folders)
      "data/ holds patient-level data. Share only where data governance allows.",
    if (!is.null(renamed) && nrow(renamed) > 0L) c(
      "", "Renamed columns",
      "---------------",
      sprintf("The %s format needs plain column names; these were renamed in data/:", opts$data_format),
      sprintf("  %s -> %s", renamed$from, renamed$to)
    ),
    if (nrow(failed) > 0L) c(
      "", "Not exported",
      "------------",
      "These files could not be written:",
      sprintf("  %s: %s", failed$id, failed$message)
    )
  )
}


# ── The build job ────────────────────────────────────────────────────────────

#' Prepare an export build to run step by step
#'
#' Writes nothing yet. Advance with \code{.export_job_step()} until
#' \code{job$done}, then \code{.export_job_finish()}: one file per step, so a
#' page can repaint progress and honour Cancel between steps. Only selected
#' items whose status is \code{"available"} are written - a stale or not-run
#' item is never exported even if its id is passed (X9).
#'
#' @param items From \code{export_items()}.
#' @param selection Character vector of item ids to write.
#' @param st From \code{export_state()}.
#' @param opts \code{list(data_format, report_format, session_include_data,
#'   script)}; \code{script} is the options list for
#'   \code{generate_analysis_script()}.
#' @param time Build time; names the zip's root folder.
#' @return A job list.
#' @keywords internal
export_job <- function(items, selection, st, opts, time = Sys.time()) {
  todo <- items[items$id %in% selection & items$status == "available", , drop = FALSE]
  # The report last: it is the slowest step, and everything before it is quick
  todo <- todo[order(todo$kind == "report"), , drop = FALSE]
  name <- paste0("edark_export_", format(time, "%Y-%m-%d_%H%M%S"))
  dir  <- tempfile("edark_export_")
  dir.create(file.path(dir, name), recursive = TRUE)
  list(
    items = todo, i = 0L, n = nrow(todo), st = st, opts = opts, time = time,
    dir = dir, name = name, root = file.path(dir, name),
    written = todo[0, , drop = FALSE],
    failed  = data.frame(id = character(0), message = character(0), stringsAsFactors = FALSE),
    notes   = list(),
    done    = nrow(todo) == 0L
  )
}

.export_job_step <- function(job) {
  if (job$done) return(job)
  job$i <- job$i + 1L
  item  <- job$items[job$i, , drop = FALSE]
  note  <- tryCatch(
    .export_write_item(item, job$st, job$opts, job$root),
    error = function(e) {
      unlink(file.path(job$root, item$path))
      structure(list(message = conditionMessage(e)), class = "export_failure")
    })
  if (inherits(note, "export_failure")) {
    job$failed[nrow(job$failed) + 1L, ] <- list(item$path, note$message)
  } else {
    job$written <- rbind(job$written, item)
    if (!is.null(note)) job$notes[[length(job$notes) + 1L]] <- note
  }
  job$done <- job$i >= job$n
  job
}

.export_job_progress <- function(job) if (job$n == 0L) 1 else 0.05 + 0.85 * job$i / job$n

.export_job_detail <- function(job) {
  if (job$done) return("Compressing\u2026")
  nxt <- job$items[job$i + 1L, , drop = FALSE]
  if (identical(nxt$kind, "report")) "Compiling the report\u2026" else paste0("Writing ", nxt$path, "\u2026")
}

# Finish an export build: README, then zip. Returns list(zip, n_written,
# failed, dir) - `zip` is the built archive, inside `dir`, a temp directory the
# caller owns and deletes.
.export_job_finish <- function(job) {
  writeLines(.export_readme(job$st, job$written, job$failed, job$notes, job$opts, job$time),
             file.path(job$root, "README.txt"), useBytes = TRUE)
  zipfile <- file.path(job$dir, paste0(job$name, ".zip"))
  zip::zip(zipfile, files = job$name, root = job$dir, mode = "mirror")
  list(zip = zipfile, n_written = nrow(job$written), failed = job$failed, dir = job$dir)
}
