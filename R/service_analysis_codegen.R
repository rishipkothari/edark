#' Analysis R Code Generation Service
#'
#' Writes a self-contained, executable R script that repeats the work done in
#' EDARK (PRD §A7.9): it rebuilds the working dataset from the input dataset
#' (launch type casting, then Prepare's type changes, columns, transforms and
#' row filters) and repeats every Analyze step that has been run and is
#' current - Table 1, the univariable screen, stepwise, LASSO, collinearity,
#' the model, diagnostics, performance (apparent, held-out test set,
#' cross-validation, bootstrap) and the Model › Results outputs - with the
#' settings and random seeds the app used. A step that was not run is left
#' out; a stale one is left out with a message (the Export rule, X9).
#'
#' The script is generated on demand from the export state snapshot
#' (\code{export_state()}), never cached: the 4 · Export › R Code page builds
#' it when opened, and the export zip writes it as
#' \code{reproduce/analysis_script.R}. Pure - no Shiny.
#'
#' The analysis is written out step by step (data preparation, formulas, the
#' fits, step(), cv.glmnet, the fold and resample loops, each with its seed);
#' every reported number, table and figure comes from EDARK's own functions,
#' which go in a second file, \code{edark_functions.R}, printed from the
#' running app (\code{.cg_functions_file()}). Nothing is copied by hand, so the
#' script cannot drift from the app (§N6.14).
#'
#' @name service_analysis_codegen
NULL


# ── Literals ──────────────────────────────────────────────────────────────────
# The script is plain ASCII: non-ASCII characters in strings become \u escapes.

.cg_esc <- function(s) {
  if (is.na(s)) return("NA_character_")
  s  <- enc2utf8(as.character(s))
  cp <- utf8ToInt(s)
  out <- vapply(cp, function(c) {
    if (c == 92L) return("\\\\")
    if (c == 34L) return("\\\"")
    if (c == 10L) return("\\n")
    if (c == 9L)  return("\\t")
    if (c == 13L) return("\\r")
    if (c < 32L)  return(sprintf("\\%03o", c))
    if (c < 128L) return(intToUtf8(c))
    if (c <= 0xFFFF) sprintf("\\u%04X", c) else sprintf("\\U{%X}", c)
  }, character(1))
  paste0("\"", paste(out, collapse = ""), "\"")
}

# A character vector as R code, wrapped to `width` after `indent` spaces
# (the indent of the line the vector starts on)
.cg_chr <- function(x, indent = 0L, width = 78L) {
  if (is.null(x) || length(x) == 0L) return("character(0)")
  x <- vapply(x, .cg_esc, character(1), USE.NAMES = FALSE)
  if (length(x) == 1L) return(x)
  .cg_wrap_c(x, indent, width)
}

.cg_num <- function(x, indent = 0L, width = 78L) {
  if (is.null(x) || length(x) == 0L) return("numeric(0)")
  x <- vapply(x, function(v) {
    if (is.na(v)) return("NA")
    if (is.infinite(v)) return(if (v > 0) "Inf" else "-Inf")
    format(v, digits = 15, scientific = FALSE, trim = TRUE)
  }, character(1), USE.NAMES = FALSE)
  if (length(x) == 1L) return(x)
  .cg_wrap_c(x, indent, width)
}

# c(a, b, ...) on one line when it fits, else one wrapped block
.cg_wrap_c <- function(items, indent, width) {
  one <- paste0("c(", paste(items, collapse = ", "), ")")
  if (nchar(one) + indent <= width) return(one)
  pad <- strrep(" ", indent + 2L)
  lines <- character(0)
  cur <- ""
  for (i in seq_along(items)) {
    piece <- paste0(items[i], if (i < length(items)) ",")
    if (nzchar(cur) && nchar(cur) + nchar(piece) + 1L + nchar(pad) > width) {
      lines <- c(lines, cur)
      cur <- piece
    } else {
      cur <- if (nzchar(cur)) paste(cur, piece) else piece
    }
  }
  lines <- c(lines, cur)
  paste0("c(\n", paste0(pad, lines, collapse = "\n"), "\n", strrep(" ", indent), ")")
}

# A column name as R code: bare when syntactic, else in backticks
.cg_name <- function(x) {
  ok <- make.names(x) == x
  ifelse(ok, x, paste0("`", gsub("`", "\\\\`", x), "`"))
}

# A named list of strings as R code, e.g. reference levels
.cg_named_chr <- function(x, indent = 0L) {
  if (length(x) == 0L) return("list()")
  items <- paste0(.cg_name(names(x)), " = ",
                  vapply(x, function(v) .cg_esc(as.character(v)), character(1)))
  one <- paste0("list(", paste(items, collapse = ", "), ")")
  if (nchar(one) + indent <= 78L) return(one)
  pad <- strrep(" ", indent + 2L)
  paste0("list(\n", paste0(pad, items, collapse = ",\n"), "\n", strrep(" ", indent), ")")
}

# Terms joined by " + ", wrapped with the "+" at the end of each line (a line
# that starts with "+" would end the expression above it). `start` is the
# column the first term starts at; continuation lines are indented by `pad`.
.cg_terms <- function(terms, start = 0L, pad = 4L, width = 78L) {
  if (length(terms) == 0L) return("1")
  lines <- character(0)
  cur <- terms[1L]
  col <- start
  for (t in terms[-1L]) {
    if (col + nchar(cur) + nchar(t) + 3L > width) {
      lines <- c(lines, paste0(cur, " +"))
      cur <- paste0(strrep(" ", pad), t)
      col <- 0L
    } else {
      cur <- paste(cur, "+", t)
    }
  }
  paste(c(lines, cur), collapse = "\n")
}

# A model formula as R code: outcome ~ a + b + (1 | cluster), wrapped
.cg_formula <- function(outcome, preds, clusters = character(0), start = 0L) {
  terms <- c(.cg_name(preds), if (length(clusters)) paste0("(1 | ", .cg_name(clusters), ")"))
  head <- paste(.cg_name(outcome), "~ ")
  paste0(head, .cg_terms(if (length(terms)) terms else "1", start + nchar(head)))
}


# ── Script building blocks ────────────────────────────────────────────────────

.cg_lines <- function(x) {
  x <- gsub("\r", "", paste(x, collapse = "\n"), fixed = TRUE)
  strsplit(x, "\n", fixed = TRUE)[[1]]
}

# A template with {{key}} placeholders. Templates are raw strings; the first
# and last line breaks are trimmed.
.cg_fill <- function(tpl, ...) {
  vals <- list(...)
  tpl <- gsub("\r", "", tpl, fixed = TRUE)
  tpl <- sub("^\n", "", tpl)
  tpl <- sub("\n[ \t]*$", "", tpl)
  for (k in names(vals)) tpl <- gsub(paste0("{{", k, "}}"), paste(vals[[k]], collapse = "\n"), tpl, fixed = TRUE)
  if (grepl("{{", tpl, fixed = TRUE)) stop("Unfilled placeholder in a code template: ", tpl, call. = FALSE)
  .cg_lines(tpl)
}

.cg_comment <- function(text, prefix = "# ") {
  unlist(lapply(text, function(t) if (!nzchar(t)) sub(" $", "", prefix) else strwrap(t, width = 78L, prefix = prefix)))
}

.cg_heading <- function(n, title) {
  bar <- sprintf("# ---- %d. %s ", n, title)
  c("", "", paste0(bar, strrep("-", max(4L, 79L - nchar(bar)))))
}

.cg_mixed <- function(mt) isTRUE(mt %in% c("linear_mixed", "logistic_mixed"))
.cg_logit <- function(mt) isTRUE(mt %in% c("logistic", "logistic_mixed"))

# The fitting call for a model type, as R code
.cg_fit_call <- function(mt, formula, data, optimizer, satterthwaite = TRUE) {
  switch(mt,
    linear   = sprintf("lm(%s, data = %s)", formula, data),
    logistic = sprintf("glm(%s, data = %s, family = binomial())", formula, data),
    linear_mixed = sprintf("%s(%s, data = %s,\n  control = lme4::lmerControl(optimizer = %s))",
                           if (satterthwaite) "lmerTest::lmer" else "lme4::lmer", formula, data,
                           .cg_esc(optimizer)),
    logistic_mixed = sprintf("lme4::glmer(%s, data = %s, family = binomial(),\n  control = lme4::glmerControl(optimizer = %s))",
                             formula, data, .cg_esc(optimizer)))
}


# ── What the script covers ────────────────────────────────────────────────────

.CG_SECTIONS <- c(
  prepare       = "Input data and Prepare",
  analysis_data = "Analysis dataset and roles",
  table1        = "Table 1",
  univariable   = "Univariable screen",
  stepwise      = "Stepwise selection",
  lasso         = "LASSO",
  collinearity  = "Collinearity",
  model         = "Model",
  diagnostics   = "Diagnostics",
  performance   = "Performance",
  results       = "Results"
)

# Each section's status, from the same status the Export page uses
.cg_plan <- function(st) {
  spec <- st$analysis_spec
  res  <- st$analysis_result
  gs   <- analysis_output_status(spec, res, st$prepare_changed)
  vi   <- res$variable_investigation
  .row <- function(id, status, reason = "") {
    data.frame(id = id, title = .CG_SECTIONS[[id]], status = status, reason = reason,
               stringsAsFactors = FALSE)
  }
  .from <- function(id, g, present = TRUE) {
    if (!identical(g$status, "available")) return(.row(id, g$status, g$reason))
    if (!isTRUE(present)) return(.row(id, "not_run", "Not run"))
    .row(id, "included")
  }
  ad <- if (is.null(spec)) .row("analysis_data", "not_run", "Start in Analyze \u203a Setup")
        else if (isTRUE(st$prepare_changed)) .row("analysis_data", "stale", "Prepare changed since Analyze \u203a Setup froze the dataset")
        else .row("analysis_data", "included")
  rows <- list(
    .row("prepare", "included"),
    ad,
    .from("table1", gs$table1),
    .from("univariable", gs$univariable),
    .from("stepwise", gs$selection, !is.null(vi$stepwise)),
    .from("lasso", gs$selection, !is.null(vi$lasso)),
    .from("collinearity", gs$collinearity),
    .from("model", gs$model),
    .from("diagnostics", gs$diagnostics),
    .from("performance", gs$performance),
    .from("results", gs$results)
  )
  if (identical(gs$selection$status, "not_run")) {
    rows[[5]]$reason <- "Run stepwise in Analyze \u203a Variables"
    rows[[6]]$reason <- "Run LASSO in Analyze \u203a Variables"
  }
  sections <- do.call(rbind, rows)

  msgs <- data.frame(level = character(0), message = character(0), stringsAsFactors = FALSE)
  stale <- sections[sections$status == "stale", , drop = FALSE]
  if (nrow(stale) > 0L) {
    msgs[nrow(msgs) + 1L, ] <- list("stale", sprintf(
      "Left out because out of date: %s. %s.", paste(stale$title, collapse = ", "), stale$reason[1L]))
  }
  if (isTRUE(st$has_pending)) {
    msgs[nrow(msgs) + 1L, ] <- list("pending",
      "Prepare has unapplied changes. The script rebuilds the last applied state.")
  }
  list(sections = sections, messages = msgs)
}


# ── Input data, types, Prepare ────────────────────────────────────────────────

.cg_input <- function(st, opts) {
  di <- st$dataset_input
  read <- if (identical(opts$data_source, "liver_tx")) {
    "  input_data <- edark::liver_tx"
  } else {
    sprintf("  input_data <- readRDS(%s)", .cg_esc(opts$data_path %||% "input_data.rds"))
  }
  c(
    .cg_comment(c(
      sprintf("The dataset given to edark(): %s rows x %d columns.", format(nrow(di), big.mark = ","), ncol(di)),
      if (!identical(opts$data_source, "liver_tx"))
        "Save it once with saveRDS(your_data, \"input_data.rds\") and set the path below - or assign it to `input_data` before running this script."
      else "The built-in demo dataset. Assign another copy to `input_data` first to use it instead.")),
    "if (!exists(\"input_data\")) {",
    read,
    "}",
    sprintf("stopifnot(nrow(input_data) == %d, ncol(input_data) == %d)", nrow(di), ncol(di))
  )
}

# The conversions cast_column_types() made at launch, column by column
.cg_types <- function(st) {
  di <- st$dataset_input
  do <- st$dataset_original
  conv <- character(0)
  for (col in names(do)) {
    x <- di[[col]]
    y <- do[[col]]
    nm <- .cg_name(col)
    expr <- if (inherits(x, "Date") && inherits(y, "POSIXct")) {
      sprintf("as.POSIXct(as.character(%s), tz = \"UTC\")", nm)
    } else if (is.logical(x) && is.factor(y)) {
      sprintf("factor(%s, levels = c(\"FALSE\", \"TRUE\"))", nm)
    } else if (is.character(x) && is.numeric(y)) {
      sprintf("as.numeric(%s)", nm)
    } else if (is.character(x) && is.factor(y)) {
      sprintf("as.factor(%s)", nm)
    }
    if (!is.null(expr)) conv <- c(conv, sprintf("    %s = %s", nm, expr))
  }
  head <- .cg_comment(paste("EDARK converts some columns when it starts: dates to date-times,",
                            "true/false and short text columns to factors, numeric text to numbers."))
  if (length(conv) == 0L) {
    return(c(head, "# No column needed converting.", "data_original <- as.data.frame(input_data)"))
  }
  conv[-length(conv)] <- paste0(conv[-length(conv)], ",")
  c(head, "data_original <- as.data.frame(input_data) %>%", "  mutate(", conv, "  )")
}

# Prepare's steps, in the app's order (§P7.2). Values that depend on the data
# (cut-points inside the range, a column's spread) are resolved here, against
# the same intermediate dataset the app saw.
.cg_prepare <- function(st, ctx) {
  la <- st$last_applied_specs %||% list()
  ds <- st$dataset_original
  out <- .cg_comment(paste("The steps applied in Prepare, in EDARK's order: type changes, columns,",
                           "transforms, row filters."))
  out <- c(out, "working_data <- data_original")

  # Type changes
  ov <- la$column_type_overrides %||% list()
  ov <- ov[intersect(names(ov), names(ds))]
  if (length(ov) > 0L) {
    lines <- vapply(names(ov), function(col) {
      nm <- .cg_name(col)
      sprintf("    %s = %s", nm, switch(ov[[col]],
        numeric   = sprintf("suppressWarnings(as.numeric(%s))", nm),
        factor    = sprintf("as.factor(%s)", nm),
        datetime  = sprintf("suppressWarnings(as.POSIXct(as.character(%s), tz = \"UTC\"))", nm),
        character = sprintf("as.character(%s)", nm),
        nm))
    }, character(1))
    lines[-length(lines)] <- paste0(lines[-length(lines)], ",")
    out <- c(out, "", "# Type changes", "working_data <- working_data %>%", "  mutate(", lines, "  )")
    ds <- .apply_column_type_overrides(ds, ov)
  }

  # Columns
  inc  <- la$included_columns %||% names(ds)
  keep <- intersect(inc, names(ds))
  drop <- setdiff(names(ds), keep)
  if (identical(keep, names(ds))) {
    out <- c(out, "", sprintf("# Columns: all %d kept", length(keep)))
  } else if (identical(keep, setdiff(names(ds), drop))) {
    out <- c(out, "", sprintf("# Columns: %d of %d kept", length(keep), ncol(ds)),
             sprintf("working_data <- working_data %%>%% select(-all_of(%s))", .cg_chr(drop)))
  } else {
    out <- c(out, "", sprintf("# Columns: %d of %d kept, in this order", length(keep), ncol(ds)),
             sprintf("working_data <- working_data %%>%% select(all_of(%s))", .cg_chr(keep)))
  }
  ds <- ds[, keep, drop = FALSE]

  # Transforms
  # One block per transform: a note and/or the mutate() argument (lines at
  # 4 spaces). Notes on transforms that change nothing stay outside mutate().
  tf <- la$column_transform_specs %||% list()
  blocks <- list()
  .blk <- function(note = NULL, code = NULL) list(note = note, code = code)
  for (col in names(tf)) {
    if (!col %in% names(ds)) next
    sp <- tf[[col]]
    nm <- .cg_name(col)
    x  <- ds[[col]]
    b <- switch(sp$method %||% "",
      auto = .blk(sprintf("%s: each value becomes an ordered level", col),
                  sprintf("    %s = factor(%s, levels = sort(unique(%s[!is.na(%s)])), ordered = TRUE)", nm, nm, nm, nm)),
      cutpoints = {
        br <- sp$breakpoints
        if (length(br) == 0L) NULL else {
          rng <- c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))
          use <- sort(br[br > rng[1L] & br < rng[2L]])
          if (length(use) == 0L) {
            .blk(sprintf("%s: no cut-point falls inside its range, so it is left unchanged", col))
          } else {
            labs <- sp$labels
            if (is.null(labs) || length(labs) != length(use) + 1L) labs <- .make_range_labels(use)
            .blk(sprintf("%s: grouped at %s", col, paste(format(use, trim = TRUE), collapse = ", ")),
                 .cg_lines(c(sprintf("    %s = cut(%s,", nm, nm),
                             sprintf("      breaks = %s,", .cg_num(c(-Inf, use, Inf), 6L)),
                             sprintf("      labels = %s,", .cg_chr(labs, 6L)),
                             "      include.lowest = TRUE, right = FALSE, ordered_result = TRUE",
                             "    )")))
          }
        }
      },
      log = .blk(code = sprintf("    %s = %s(%s)", nm,
                                switch(sp$log_base %||% "ln", log10 = "log10", log2 = "log2", "log"), nm)),
      winsorize = {
        lo <- .winsor_lower(sp$lower_pct)
        hi <- .winsor_upper(sp$upper_pct, lo)
        .blk(sprintf("%s: values beyond the %s and %s percentiles are pulled in to them", col, format(lo), format(hi)),
             c(sprintf("    %s = pmin(pmax(%s, quantile(%s, %s, na.rm = TRUE)),", nm, nm, nm, .cg_num(lo / 100)),
               sprintf("      quantile(%s, %s, na.rm = TRUE))", nm, .cg_num(hi / 100))))
      },
      round = .blk(code = sprintf("    %s = round(%s, digits = %d)", nm, nm,
                                  max(0L, as.integer(sp$decimal_places %||% 0)))),
      standardize = {
        s <- stats::sd(x, na.rm = TRUE)
        if (isTRUE(s > 0)) .blk(code = sprintf("    %s = (%s - mean(%s, na.rm = TRUE)) / sd(%s, na.rm = TRUE)", nm, nm, nm, nm))
        else .blk(sprintf("%s: no spread, so standardising leaves it unchanged", col))
      },
      NULL)
    if (!is.null(b)) blocks[[length(blocks) + 1L]] <- b
    ds <- .apply_column_transforms(ds, tf[col])
  }
  coded <- Filter(function(b) length(b$code) > 0L, blocks)
  idle  <- Filter(function(b) length(b$code) == 0L, blocks)
  if (length(blocks)) out <- c(out, "", "# Transforms", if (length(idle)) .cg_comment(vapply(idle, `[[`, "", "note")))
  if (length(coded)) {
    args <- unlist(lapply(seq_along(coded), function(i) {
      code <- coded[[i]]$code
      if (i < length(coded)) code[length(code)] <- paste0(code[length(code)], ",")
      c(if (!is.null(coded[[i]]$note)) paste0("    # ", coded[[i]]$note), code)
    }))
    out <- c(out, "working_data <- working_data %>%", "  mutate(", args, "  )")
  }

  # Row filters
  fl <- la$row_filter_specs %||% list()
  f_lines <- character(0)
  for (col in names(fl)) {
    if (!col %in% names(ds)) next
    sp <- fl[[col]]
    nm <- .cg_name(col)
    if (identical(sp$type, "numeric") != is.numeric(ds[[col]])) {
      f_lines <- c(f_lines, sprintf("# The filter on %s no longer matches its type and was skipped, as in EDARK", col))
      next
    }
    f_lines <- c(f_lines, if (identical(sp$type, "numeric")) {
      sprintf("  filter(!is.na(%s), %s >= %s, %s <= %s)", nm, nm, .cg_num(sp$min), nm, .cg_num(sp$max))
    } else {
      sprintf("  filter(!is.na(%s), as.character(%s) %%in%% %s)", nm, nm, .cg_chr(sp$levels_selected, 2L))
    })
    ds <- .apply_row_filters(ds, fl[col])
  }
  code <- grepl("^  filter", f_lines)
  if (any(code)) {
    idx <- which(code)
    f_lines[idx[-length(idx)]] <- paste0(f_lines[idx[-length(idx)]], " %>%")
    out <- c(out, "", "# Row filters", "working_data <- working_data %>%", f_lines)
  } else if (length(f_lines)) {
    out <- c(out, "", "# Row filters", f_lines)
  }

  wd <- st$dataset_working
  c(out, "", sprintf("# The working dataset: %s rows x %d columns", format(nrow(wd), big.mark = ","), ncol(wd)),
    sprintf("stopifnot(nrow(working_data) == %d, ncol(working_data) == %d)", nrow(wd), ncol(wd)))
}


# ── Public entry point ────────────────────────────────────────────────────────

#' Generate the analysis R script
#'
#' @param st A snapshot from \code{export_state()}.
#' @param opts A list: \code{data_source} (\code{"file"} reads the input
#'   dataset with \code{readRDS(data_path)}; \code{"liver_tx"} uses the
#'   built-in dataset), \code{data_path}, \code{figures} (logical: print the
#'   figures as the script runs), \code{time} (shown in the headers).
#' @return A list: \code{text} and \code{lines} (the analysis script,
#'   \code{analysis_script.R}), \code{functions} (\code{list(text, lines, n)}:
#'   \code{edark_functions.R}, the EDARK functions the script calls and the
#'   number of objects in it), \code{sections} (data.frame: id, title, status -
#'   \code{"included"}, \code{"stale"}, \code{"not_run"} - and reason),
#'   \code{packages}, \code{seeds} (data.frame: what, seed) and
#'   \code{messages} (data.frame: level, message).
#' @export
generate_analysis_script <- function(st, opts = list()) {
  opts <- utils::modifyList(list(data_source = "file", data_path = "input_data.rds",
                                 figures = TRUE, time = Sys.time()), opts)
  plan <- .cg_plan(st)
  inc  <- function(id) identical(plan$sections$status[plan$sections$id == id], "included")
  ctx  <- .cg_context(st, plan, opts)

  body <- list()
  n <- 0L
  .add <- function(title, lines) {
    n <<- n + 1L
    body[[length(body) + 1L]] <<- c(.cg_heading(n, title), lines)
  }

  .add("Input data", .cg_input(st, opts))
  .add("Column types", .cg_types(st))
  .add("Prepare", .cg_prepare(st, ctx))
  if (inc("analysis_data")) .add("Analysis dataset (Analyze \u203a Setup)", .cg_analysis_data(st, ctx))
  if (inc("table1"))        .add("Table 1 (Analyze \u203a Table 1)", .cg_table1(st, ctx))
  if (any(vapply(c("univariable", "stepwise", "lasso", "collinearity"), inc, logical(1)))) {
    .add("Variable selection (Analyze \u203a Variables)", .cg_variables(st, ctx, inc))
  }
  if (inc("model"))         .add("Model (Analyze \u203a Model \u203a Create)", .cg_model(st, ctx))
  if (inc("diagnostics"))   .add("Diagnostics (Analyze \u203a Model \u203a Diagnostics)", .cg_diagnostics(st, ctx))
  if (inc("performance"))   .add("Performance (Analyze \u203a Model \u203a Performance)", .cg_performance(st, ctx))
  if (inc("results"))       .add("Results (Analyze \u203a Model \u203a Results)", .cg_results(st, ctx, inc))
  body <- gsub("\u203a", ">", unlist(body), fixed = TRUE)

  # The EDARK functions the analysis calls, and everything they call
  fns <- .cg_functions_file(.cg_entry_points(body), opts$time)
  pkgs <- .cg_packages(c(body, fns$lines), inc)

  lines <- c(.cg_header(st, plan, opts), .cg_setup(pkgs, fns$n), body, "")
  list(
    text      = paste(lines, collapse = "\n"),
    lines     = lines,
    functions = fns,
    sections  = plan$sections,
    packages  = pkgs,
    seeds     = ctx$seeds,
    messages  = plan$messages
  )
}

# Values shared by several sections
.cg_context <- function(st, plan, opts) {
  spec <- st$analysis_spec
  res  <- st$analysis_result
  snap <- res$specification_snapshot
  mt   <- snap$model_design$model_type
  vr   <- snap$variable_roles
  pf   <- res$performance
  vi   <- res$variable_investigation
  seeds <- data.frame(what = character(0), seed = integer(0), stringsAsFactors = FALSE)
  if (identical(plan$sections$status[plan$sections$id == "lasso"], "included")) {
    seeds[nrow(seeds) + 1L, ] <- list("LASSO cross-validation folds", as.integer(vi$lasso$seed %||% lasso_seed(spec)))
  }
  if (identical(plan$sections$status[plan$sections$id == "performance"], "included") &&
      pf$validation$method %in% c("cv", "bootstrap")) {
    seeds[nrow(seeds) + 1L, ] <- list(
      if (pf$validation$method == "cv") "Cross-validation folds" else "Bootstrap resamples",
      as.integer(pf$validation$seed))
  }
  list(
    figures    = isTRUE(opts$figures),
    model_type = mt,
    logit      = .cg_logit(mt),
    mixed      = .cg_mixed(mt),
    outcome    = vr$outcome_variable,
    preds      = .safe_preds(vr$exposure_variable, vr$final_model_covariates),
    clusters   = if (.cg_mixed(mt)) vr$cluster_variables[nzchar(vr$cluster_variables)] else character(0),
    optimizer  = if (isTRUE(snap$model_design$optimizer %in% .ANALYSIS_OPTIMIZERS)) snap$model_design$optimizer else "bobyqa",
    perf       = pf,
    diag       = res$diagnostics,
    split      = analysis_split(spec),
    seeds      = seeds
  )
}

# Print a list of plots, when figures are on
.cg_print <- function(ctx, expr) {
  if (!ctx$figures) return(NULL)
  sprintf("for (p in %s) print(p)", expr)
}

# Packages: those the code calls as pkg::fun(), plus dplyr (its verbs and
# %>% are used without a prefix) and Table 1's test packages
.CG_BASE_PKGS <- c("base", "stats", "utils", "graphics", "grDevices", "methods", "tools", "grid", "splines", "edark")
.cg_packages <- function(lines, inc) {
  used <- unlist(regmatches(lines, gregexpr("\\b[A-Za-z][A-Za-z0-9.]*(?=::)", lines, perl = TRUE)))
  unique(c("dplyr", setdiff(sort(unique(used)), c(.CG_BASE_PKGS, "dplyr", "pacman")),
           if (inc("table1")) c("cardx", "smd")))
}


# ── EDARK's own functions, printed from the running app ──────────────────────
# The script does not carry copies: it carries the functions themselves,
# deparsed from the loaded edark namespace, so it computes the numbers and
# draws the figures exactly as the app does (§N6.14).

# The names an expression uses, leaving out both sides of pkg::name - the
# `edark` in edark::liver_tx is not the edark() function
.cg_symbols <- function(e) {
  if (is.symbol(e)) return(as.character(e))
  if (is.call(e)) {
    if (identical(e[[1L]], as.name("::")) || identical(e[[1L]], as.name(":::"))) return(character(0))
    return(unlist(lapply(as.list(e), .cg_symbols)))
  }
  if (is.expression(e) || is.pairlist(e) || is.list(e)) return(unlist(lapply(as.list(e), .cg_symbols)))
  character(0)
}

# The edark functions a script refers to by name
.cg_entry_points <- function(lines) {
  ns <- asNamespace("edark")
  ids <- .cg_symbols(parse(text = lines, keep.source = FALSE))
  ids <- intersect(unique(ids), ls(ns, all.names = TRUE))
  ids[vapply(ids, function(n) is.function(get(n, envir = ns)), logical(1))]
}

# Names of edark objects an object refers to
.cg_refs <- function(obj, ns_objs) {
  nm <- if (is.function(obj)) {
    c(all.names(body(obj)), unlist(lapply(formals(obj), function(f) if (is.language(f)) all.names(f))))
  } else if (is.list(obj)) {
    unlist(lapply(obj, function(x) {
      if (is.function(x)) all.names(body(x)) else if (is.language(x)) all.names(x)
    }))
  }
  intersect(unique(nm), ns_objs)
}

# Every edark object the entry points need, directly or through each other
.cg_closure <- function(entry) {
  ns   <- asNamespace("edark")
  objs <- ls(ns, all.names = TRUE)
  seen <- character(0)
  todo <- entry
  while (length(todo)) {
    n <- todo[1L]
    todo <- todo[-1L]
    if (n %in% seen) next
    obj <- get(n, envir = ns)
    if (is.environment(obj)) stop("The R script cannot carry '", n, "': it is an environment.", call. = FALSE)
    seen <- c(seen, n)
    todo <- c(todo, setdiff(.cg_refs(obj, objs), seen))
  }
  seen
}

# Non-ASCII characters as \u escapes. Deparsed code keeps no comments, so they
# can only be inside strings, where the escape means the same character.
.cg_ascii <- function(lines) {
  vapply(lines, function(l) {
    cp <- utf8ToInt(enc2utf8(l))
    if (all(cp < 128L)) return(l)
    paste(vapply(cp, function(c) {
      if (c < 128L) intToUtf8(c) else if (c <= 0xFFFF) sprintf("\\u%04X", c) else sprintf("\\U{%X}", c)
    }, character(1)), collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

# edark_functions.R: constants, then functions, each as `name <- <deparse>`
.cg_functions_file <- function(entry, time = Sys.time()) {
  ns  <- asNamespace("edark")
  all <- .cg_closure(entry)
  is_fn <- vapply(all, function(n) is.function(get(n, envir = ns)), logical(1))
  ord <- c(sort(all[!is_fn], method = "radix"), sort(all[is_fn], method = "radix"))
  defs <- unlist(lapply(ord, function(n) {
    d <- deparse(get(n, envir = ns), width.cutoff = 70L)
    d[1L] <- paste(.cg_name(n), "<-", d[1L])
    c("", d)
  }))
  # deparse() leaves a space after `function (...)`; strings never span lines
  defs <- sub("[ \t]+$", "", .cg_ascii(defs))
  if (any(grepl("shiny::", defs, fixed = TRUE))) {
    stop("An EDARK function the R script needs calls Shiny; the script must not depend on it.", call. = FALSE)
  }
  lines <- c(
    "# ==============================================================================",
    "# EDARK functions used by analysis_script.R",
    sprintf("# Printed from EDARK %s on %s", EDARK_VERSION, format(time, "%Y-%m-%d %H:%M")),
    "#",
    .cg_comment(c(
      paste("These are the app's own functions, exactly as they ran, so the script",
            "computes the same numbers and draws the same figures as EDARK. Names that",
            "start with a dot are internal to EDARK. Comments are not kept (an installed",
            "R package does not keep its source). Do not edit this file - generate the",
            "script again instead."),
      if (length(ord) == 0L) "None yet: the analysis script only prepares the data. EDARK's functions appear here once an Analyze step has run."
      else sprintf("%d objects: %d constants, %d functions.", length(ord), sum(!is_fn), sum(is_fn)))),
    "# ==============================================================================",
    defs,
    ""
  )
  list(text = paste(lines, collapse = "\n"), lines = lines, n = length(ord))
}


# ── Header and setup ──────────────────────────────────────────────────────────

.cg_header <- function(st, plan, opts) {
  s   <- plan$sections
  inc <- s$title[s$status == "included"]
  out <- s[s$status != "included", , drop = FALSE]
  c(
    "# ==============================================================================",
    "# EDARK analysis script",
    sprintf("# Generated %s by EDARK %s", format(opts$time, "%Y-%m-%d %H:%M"), EDARK_VERSION),
    "#",
    .cg_comment(paste("Rebuilds the working dataset from the input dataset and repeats each EDARK",
                      "step that has been run, with the same settings and random seeds:")),
    paste0("#   - ", inc),
    if (nrow(out) > 0L) c("# Not included:", paste0("#   - ", out$title, " (", tolower(gsub("_", " ", out$status)), ")")),
    "#",
    .cg_comment(paste("Keep edark_functions.R next to this file and run this script from that",
                      "folder, top to bottom, in a fresh R session. The analysis steps are",
                      "written out here; every reported number, table and figure comes from",
                      "EDARK's own functions in edark_functions.R. If this script and EDARK",
                      "disagree, EDARK has a bug.")),
    "# =============================================================================="
  )
}

.cg_setup <- function(pkgs, n_functions) {
  other <- setdiff(pkgs, "dplyr")
  c("", .cg_fill(r"---(
# Binary packages install without compilers (Windows and macOS)
if (.Platform$OS.type == "windows" || Sys.info()[["sysname"]] == "Darwin") {
  options(pkgType = "binary")
}
options(repos = c(
  RSPM = "https://packagemanager.posit.co/cran/latest",
  CRAN = "https://cloud.r-project.org"
))
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(dplyr)
)---"),
    if (length(other)) c(
      "# Called as pkg::fun(), so installed if missing but not attached (attaching",
      "# pROC or lmerTest would mask functions from stats)",
      sprintf("for (pkg in %s) {", .cg_chr(other)),
      "  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)",
      "}"),
    "",
    if (n_functions > 0L) c(
      sprintf("# EDARK's own functions (%d objects), exactly as the app ran them", n_functions),
      "source(\"edark_functions.R\")")
    else "# No EDARK function is needed yet (edark_functions.R is empty until an Analyze step has run).")
}


# ── Settings as R code ────────────────────────────────────────────────────────

# A value from the spec as R code: NULL, atomic vectors, (named) lists
.cg_value <- function(x, indent = 0L, lead = 0L) {
  w <- 78L - lead   # `lead`: characters before the value on its first line (a name)
  if (is.null(x)) return("NULL")
  if (is.list(x) && !is.data.frame(x)) {
    if (length(x) == 0L) return("list()")
    nms <- names(x)
    labs <- vapply(seq_along(x), function(i) {
      if (!is.null(nms) && nzchar(nms[i])) paste0(.cg_name(nms[i]), " = ") else ""
    }, character(1))
    # Each item on its own line at indent + 2; a value that wraps closes at
    # that indent too, under its name
    items <- paste0(labs, vapply(seq_along(x), function(i) .cg_value(x[[i]], indent + 2L, nchar(labs[i])), character(1)))
    one <- paste0("list(", paste(items, collapse = ", "), ")")
    if (nchar(one) + indent <= w && !grepl("\n", one, fixed = TRUE)) return(one)
    pad <- strrep(" ", indent + 2L)
    return(paste0("list(\n", paste0(pad, items, collapse = ",\n"), "\n", strrep(" ", indent), ")"))
  }
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) return(.cg_chr(x, indent, w))
  if (is.logical(x)) {
    if (length(x) == 0L) return("logical(0)")
    v <- ifelse(is.na(x), "NA", ifelse(x, "TRUE", "FALSE"))
    return(if (length(v) == 1L) v else .cg_wrap_c(v, indent, w))
  }
  if (is.integer(x)) {
    if (length(x) == 0L) return("integer(0)")
    v <- ifelse(is.na(x), "NA_integer_", paste0(x, "L"))
    return(if (length(v) == 1L) v else .cg_wrap_c(v, indent, w))
  }
  if (is.numeric(x)) return(.cg_num(x, indent, w))
  stop("The R script cannot write a value of class ", class(x)[1L], ".", call. = FALSE)
}

# The settings EDARK's functions read, from analysis_spec
.cg_spec <- function(spec) {
  keep <- c("variable_roles", "table1_specification", "variable_selection_specification",
            "model_design", "purpose_specification", "validation_settings")
  out <- spec[intersect(keep, names(spec))]
  out[!vapply(out, is.null, logical(1))]
}


# ── Analyze › Setup ───────────────────────────────────────────────────────────

.cg_analysis_data <- function(st, ctx) {
  spec <- st$analysis_spec
  ps   <- spec$purpose_specification %||% .default_purpose_specification()
  out <- c(
    "# Analyze works on a frozen copy of the working dataset with a row ID added.",
    "analysis_data <- working_data",
    "analysis_data$.edark_row_id <- seq_len(nrow(analysis_data))",
    "",
    "# The settings chosen in EDARK (its analysis_spec): roles, Table 1, variable",
    "# selection, model, purpose and validation. EDARK's functions below read them.",
    paste("spec <-", .cg_value(.cg_spec(spec), 0L)),
    "",
    "outcome  <- spec$variable_roles$outcome_variable",
    "exposure <- spec$variable_roles$exposure_variable",
    "reference_levels <- spec$variable_roles$reference_levels"
  )
  sp <- analysis_split(spec)
  purpose <- if (identical(ps$model_purpose, "prediction")) "prediction" else "association"
  out <- c(out, "", sprintf("# Model purpose: %s%s", purpose,
                            if (purpose == "prediction") sprintf(", validated by %s",
                              tolower(.PERF_METHOD_LABELS[[analysis_validation(spec)$method]])) else ""))
  if (!is.null(sp)) {
    out <- c(out, .cg_fill(r"---(
# Held-out test set: models are built on the training rows ({{var}} = {{lvl}});
# rows with any other level are the test set; rows missing {{var}} are in neither.
split_value <- as.character(analysis_data[[{{var_q}}]])
in_training <- !is.na(split_value) & split_value == {{lvl_q}}
in_test     <- !is.na(split_value) & split_value != {{lvl_q}}
model_data  <- analysis_data[in_training, , drop = FALSE]
test_data   <- analysis_data[in_test, , drop = FALSE]
)---", var = sp$variable, lvl = sp$training_level, var_q = .cg_esc(sp$variable), lvl_q = .cg_esc(sp$training_level)))
  } else {
    out <- c(out, "# Models are built on every row.", "model_data <- analysis_data")
  }
  out
}


# ── Table 1 ───────────────────────────────────────────────────────────────────

.cg_table1 <- function(st, ctx) {
  rt <- st$analysis_result$result_tables
  t1 <- st$analysis_spec$table1_specification
  .stat <- function(p, smd) if (isTRUE(smd)) "standardised mean differences" else if (isTRUE(p)) "p-values" else "no test"
  made <- c(
    if (!is.null(rt$table1_overall)) "table1$overall",
    if (!is.null(rt$table1_by_exposure)) "table1$by_exposure",
    if (!is.null(rt$table1_by_outcome)) "table1$by_outcome")
  c(.cg_comment(c(
      "Every row of the analysis dataset, including any test rows; exposure, outcome, then the rest in dataset order.",
      if (!is.null(rt$table1_by_exposure))
        sprintf("By exposure: %s.", .stat(t1$include_pvalues_exposure, t1$include_smd_exposure)),
      if (!is.null(rt$table1_by_outcome))
        sprintf("By outcome: %s.", .stat(t1$include_pvalues_outcome, t1$include_smd_outcome)))),
    "table1 <- build_table1(analysis_data, spec)",
    made)
}


# ── Variable selection ────────────────────────────────────────────────────────

.cg_variables <- function(st, ctx, inc) {
  spec <- st$analysis_spec
  vr   <- spec$variable_roles
  vs   <- spec$variable_selection_specification %||% list()
  res  <- st$analysis_result
  vi   <- res$variable_investigation
  md   <- analysis_model_data(spec, st$analysis_data)
  outcome  <- vr$outcome_variable
  exposure <- vr$exposure_variable
  if (is.null(exposure) || !nzchar(exposure) || !exposure %in% names(md)) exposure <- character(0)
  pool <- vr$univariable_test_pool %||% character(0)
  y <- md[[outcome]]
  binary <- is.factor(y) && length(levels(droplevels(y))) == 2L
  fit_fn <- if (binary) "glm(%s, data = %s, family = binomial())" else "lm(%s, data = %s)"

  out <- c(.cg_comment(paste("Run on the rows models are built from", if (!is.null(analysis_split(spec))) "(the training set)." else "(every row).")),
           "candidates <- spec$variable_roles$univariable_test_pool",
           if (inc("stepwise") || inc("lasso")) c(
             "# Stepwise and LASSO use the rows complete for all of these",
             "selection_vars <- intersect(c(outcome, exposure, candidates), names(model_data))"))

  if (inc("univariable")) {
    ord <- intersect(names(md), pool)
    if (length(exposure) && exposure %in% ord) ord <- c(exposure, setdiff(ord, exposure))
    out <- c(out, "", .cg_fill(r"---(
# Univariable screen: outcome ~ candidate, one {{kind}} model per candidate, on
# the rows complete for both. {{measure}}
screen_data <- apply_reference_levels(model_data, reference_levels)
univariable <- do.call(rbind, lapply(candidates, function(v) {
  rows <- screen_data[complete.cases(screen_data[, c(outcome, v)]), , drop = FALSE]
  if (nrow(rows) == 0L || nrow(.partition_modelable(rows, v)$excluded) > 0L) return(NULL)
  rows <- .droplevels_cols(rows, v)
  fit <- tryCatch({{fit}}, error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  ct <- edark_coef_table(fit, rows)
  ct <- ct[ct$term != "(Intercept)", , drop = FALSE]
  data.frame(variable = v, term = ct$term, estimate = ct$effect, conf.low = ct$effect.low,
             conf.high = ct$effect.high, p.value = ct$p.value, stringsAsFactors = FALSE)
}))
# Exposure first, then dataset order
screen_order <- {{ord}}
univariable <- univariable[order(match(univariable$variable, screen_order), univariable$term,
                                 method = "radix"), , drop = FALSE]
univariable$suggested <- !is.na(univariable$p.value) &
  univariable$p.value < spec$variable_selection_specification$univariable_p_threshold
rownames(univariable) <- NULL
univariable
)---", kind = if (binary) "logistic" else "linear",
    measure = if (binary) "Estimates are odds ratios." else "Estimates are regression coefficients.",
    fit = sprintf(fit_fn, "as.formula(paste(outcome, \"~\", v))", "rows"),
    ord = .cg_chr(ord)))
  }

  .sel_prep <- function(r, obj) {
    keep <- setdiff(intersect(pool, names(md)), exposure)
    keep <- setdiff(keep, r$excluded_variables$variable)
    ex <- r$excluded_variables
    c(.cg_comment(sprintf("Rows complete for the outcome%s and every candidate (%s of %s).",
                          if (length(exposure)) ", the exposure" else "",
                          format(r$n_used, big.mark = ","), format(r$n_total, big.mark = ","))),
      sprintf("%s_data <- model_data[complete.cases(model_data[, selection_vars]), , drop = FALSE]", obj),
      sprintf("%s_data <- apply_reference_levels(%s_data, reference_levels)", obj, obj),
      if (is.data.frame(ex) && nrow(ex) > 0L)
        .cg_comment(paste0("Left out - cannot be modelled on these rows: ",
                           paste(sprintf("%s (%s)", ex$variable, ex$reason), collapse = "; "), ".")),
      sprintf("%s_candidates <- %s", obj, .cg_chr(keep)),
      sprintf("%s_data <- .droplevels_cols(%s_data, c(%s_candidates, exposure))", obj, obj, obj))
  }
  .commented <- function(lines, why) c(.cg_comment(why), paste0("# ", .cg_lines(lines)))

  if (inc("stepwise")) {
    s <- vi$stepwise
    dir  <- s$direction %||% vs$stepwise_direction %||% "backward"
    crit <- s$criterion %||% vs$stepwise_criterion %||% "BIC"
    keep <- setdiff(setdiff(intersect(pool, names(md)), exposure), s$excluded_variables$variable)
    lower <- if (length(exposure)) .cg_name(exposure) else "1"
    upper <- .cg_terms(.cg_name(c(exposure, keep)), start = 17L, pad = 17L)
    start <- if (identical(dir, "backward")) .cg_formula(outcome, c(exposure, keep), start = 16L)
             else paste(.cg_name(outcome), "~", lower)
    code <- c(
      .sel_prep(s, "stepwise"),
      sprintf("stepwise_start <- %s", sub(", data = ", ",\n                      data = ",
                                          sprintf(fit_fn, start, "stepwise_data"), fixed = TRUE)),
      "stepwise_fit <- stats::step(",
      "  stepwise_start,",
      sprintf("  scope = list(lower = ~ %s,", lower),
      sprintf("               upper = ~ %s),", upper),
      sprintf("  direction = %s, k = %s, trace = 0", .cg_esc(dir),
              if (identical(crit, "BIC")) "log(nrow(stepwise_data))" else "2"),
      ")",
      "stepwise_selected <- intersect(stepwise_candidates, attr(terms(stepwise_fit), \"term.labels\"))",
      "stepwise_selected"
    )
    head <- sprintf("Stepwise selection: %s, by %s.%s", dir, crit,
                    if (length(exposure)) " The exposure is held in every model, so candidates are chosen for what they add alongside it." else "")
    out <- c(out, "", .cg_comment(head),
             if (!is.null(s$error)) .commented(code, paste("EDARK reported an error for this run, so the code is commented out:", s$error))
             else code)
  }

  if (inc("lasso")) {
    l <- vi$lasso
    seed <- as.integer(l$seed %||% lasso_seed(spec))
    lam  <- l$lambda_type %||% vs$lasso_lambda %||% "lambda.1se"
    keep <- setdiff(setdiff(intersect(pool, names(md)), exposure), l$excluded_variables$variable)
    xf <- paste("~", .cg_terms(.cg_name(c(exposure, keep)), start = 31L, pad = 31L))
    code <- c(
      .sel_prep(l, "lasso"),
      "lasso_vars <- c(exposure, lasso_candidates)",
      sprintf("lasso_matrix <- model.matrix(%s,\n                             data = lasso_data)", xf),
      "lasso_columns <- lasso_vars[attr(lasso_matrix, \"assign\")[-1L]]   # variable behind each column",
      "lasso_x <- lasso_matrix[, -1L, drop = FALSE]",
      sprintf("lasso_y <- %s", if (binary) "as.numeric(lasso_data[[outcome]]) - 1L" else "as.numeric(lasso_data[[outcome]])"),
      "# The exposure's columns are not penalised: always in the model, never selected",
      "lasso_penalty <- ifelse(lasso_columns %in% exposure, 0, 1)",
      "# The 10 cross-validation folds are random; this seed is the one EDARK used",
      sprintf("set.seed(%d)", seed),
      sprintf("lasso_cv <- glmnet::cv.glmnet(lasso_x, lasso_y, family = %s, alpha = 1,",
              .cg_esc(if (binary) "binomial" else "gaussian")),
      "                              nfolds = 10, penalty.factor = lasso_penalty)",
      sprintf("lasso_lambda <- lasso_cv$%s", if (identical(lam, "lambda.min")) "lambda.min" else "lambda.1se"),
      "lasso_coef <- as.numeric(glmnet::coef.glmnet(lasso_cv$glmnet.fit, s = lasso_lambda))[-1L]",
      "# A factor is selected when any of its columns is non-zero",
      "lasso_selected <- intersect(lasso_candidates, lasso_columns[lasso_coef != 0 & !lasso_columns %in% exposure])",
      "lasso_selected"
    )
    out <- c(out, "", sprintf("# LASSO (%s)", lam),
             if (!is.null(l$error)) .commented(code, paste("EDARK reported an error for this run, so the code is commented out:", l$error))
             else code)
  }

  if (inc("collinearity")) {
    cp <- res$result_plots$collinearity_plots
    out <- c(out, "",
             .cg_comment("Collinearity between candidates: Pearson r for numeric pairs, Cramer's V for factor pairs; pairs above 0.7 are flagged."),
             "collinearity <- compute_collinearity(model_data, candidates)",
             "collinearity$flagged_pairs",
             if (ctx$figures && !is.null(cp$cor_matrix)) "print(.plot_correlation_heatmap(collinearity$cor_matrix))",
             if (ctx$figures && !is.null(cp$cramers_v_matrix)) "print(.plot_cramers_heatmap(collinearity$cramers_v_mat))")
  }
  out
}


# ── Model ─────────────────────────────────────────────────────────────────────

.cg_model <- function(st, ctx) {
  rs <- st$analysis_result$run_status
  mt <- ctx$model_type
  ev <- rs$outcome_event
  c(
    sprintf("# %s on %s of %s rows%s.", .ANALYSIS_MODEL_LABELS[[mt]], format(rs$n_used, big.mark = ","),
            format(rs$n_total, big.mark = ","), if (!is.null(ctx$split)) " of the training set" else ""),
    if (!is.null(ev)) sprintf("# Modelling %s = %s (vs %s).", ev$variable, ev$event, ev$reference),
    sprintf("logistic   <- %s", if (ctx$logit) "TRUE" else "FALSE"),
    sprintf("mixed      <- %s", if (ctx$mixed) "TRUE" else "FALSE"),
    sprintf("predictors <- %s   # exposure first, then the covariates", .cg_chr(ctx$preds)),
    sprintf("clusters   <- %s", .cg_chr(ctx$clusters)),
    "",
    .cg_comment(paste("The model's rows: complete for every model variable, ordered factors made plain",
                      "factors, reference levels set, unused levels dropped.")),
    "model_rows <- .prepare_model_rows(model_data, outcome, predictors, clusters, reference_levels)",
    "",
    sprintf("model_formula <- %s", .cg_formula(ctx$outcome, ctx$preds, ctx$clusters, start = 17L)),
    sprintf("fit_model <- function(formula, data) %s", .cg_fit_call(mt, "formula", "data", ctx$optimizer)),
    "model <- fit_model(model_formula, model_rows)",
    "summary(model)",
    "",
    .cg_comment(edark_inference_note(mt)),
    "model_coefficients <- edark_coef_table(model, model_rows)",
    "model_coefficients",
    sprintf("model_fit_statistics <- .fit_statistics(model, %s, model_rows, outcome, clusters)", .cg_esc(mt)),
    "model_fit_statistics",
    "",
    "# What EDARK keeps about the fit (its analysis_result); its output builders read it",
    "model_result <- list(",
    "  specification_snapshot = spec,",
    "  fitted_models = list(primary_model = model),",
    "  inference_summary = list(",
    "    coefficients     = model_coefficients,",
    "    fit_statistics   = model_fit_statistics,",
    "    predicted_values = data.frame(.edark_row_id = model_rows$.edark_row_id)",
    "  ),",
    "  run_status = list(",
    "    n_used = nrow(model_rows), n_total = nrow(model_data),",
    "    outcome_event = if (logistic) list(variable = outcome, event = levels(model_rows[[outcome]])[2L],",
    "                                       reference = levels(model_rows[[outcome]])[1L]),",
    "    reference_levels = Filter(Negate(is.null), sapply(predictors, function(v) {",
    "      if (is.factor(model_rows[[v]])) levels(model_rows[[v]])[1L]",
    "    }, simplify = FALSE))",
    "  )",
    ")"
  )
}


# ── Diagnostics ───────────────────────────────────────────────────────────────

.cg_diagnostics <- function(st, ctx) {
  ck <- ctx$diag$checks
  mt <- ctx$model_type
  lg <- "logistic"
  out <- c(sprintf("# Checks run: %s", paste(ck, collapse = ", ")),
           "mf <- model.frame(model)   # the model's own rows",
           "row_ids <- model_rows$.edark_row_id",
           "diagnostics <- list()",
           "diagnostics$sample <- .diag_sample(spec, model_data, mf, outcome, logistic, mixed)")
  .chk <- function(title, call, key, plots = TRUE, guard = FALSE) {
    c("", paste("#", title),
      sprintf("diagnostics$%s <- %s", key, call),
      if (plots) {
        p <- .cg_print(ctx, sprintf("diagnostics$%s$plots", key))
        if (!is.null(p) && guard) sprintf("if (is.list(diagnostics$%s)) %s", key, p) else p
      },
      if (!plots) sprintf("diagnostics$%s", key))
  }
  if ("residuals" %in% ck) out <- c(out, .chk(
    if (ctx$logit) "Residuals: binned response residuals (about 95% of bins should include 0)"
    else if (mt == "linear") "Residuals: vs fitted, Q-Q, scale-location and the Breusch-Pagan test"
    else "Residuals: conditional residuals vs fitted, Q-Q and scale-location",
    sprintf(".diag_residuals(model, %s)", .cg_esc(mt)), "residuals"))
  if ("linearity" %in% ck) out <- c(out, .chk(
    "Linearity: residuals against each continuous predictor (a note when there is none)",
    ".diag_linearity(model, mf, spec, logistic)", "linearity", guard = TRUE))
  if ("influence" %in% ck) out <- c(out, .chk(
    "Influence: Cook's distance and leverage; the cut-off is 4 / n",
    ".diag_influence(model, mf, row_ids, logistic)", "influence"),
    "diagnostics$influence$top")
  if ("vif" %in% ck) out <- c(out, .chk(
    "Collinearity: variance inflation factors (5 to 10 moderate, above 10 high)",
    ".diag_vif(model)", "vif", plots = FALSE))
  if ("separation" %in% ck) out <- c(out, .chk(
    paste0("Separation: does a predictor perfectly predict the outcome?", if (ctx$mixed) " (fixed effects)" else ""),
    ".diag_separation(model, mf, mixed)", "separation", plots = FALSE))
  if ("random_effects" %in% ck) out <- c(out, .chk(
    "Random effects: variance, SD and ICC per cluster variable, cluster sizes",
    ".diag_random_effects(model, mf, logistic)", "random_effects"),
    "diagnostics$random_effects$components")
  out
}


# ── Performance ───────────────────────────────────────────────────────────────

.cg_performance <- function(st, ctx) {
  pf <- ctx$perf
  vl <- pf$validation
  mt <- ctx$model_type
  vl_list <- vl[intersect(c("method", "cv_folds", "cv_repeats", "bootstrap_reps", "seed"), names(vl))]
  out <- c(
    sprintf("# Measures: %s. Validation: %s.", paste(pf$checks, collapse = ", "),
            tolower(.PERF_METHOD_LABELS[[vl$method]])),
    if (ctx$mixed) "# Mixed models predict from the fixed effects alone (re.form = NA), as for a patient from an unseen cluster.",
    "mf <- model.frame(model)   # the model's own rows",
    "",
    "# What EDARK's performance functions work on",
    "job <- list(",
    sprintf("  run_at = Sys.time(), model_type = %s, checks = %s,", .cg_esc(mt), .cg_chr(pf$checks, 2L)),
    sprintf("  basis = %s, split = NULL, sets = list(), plots = list(),", .cg_esc(pf$basis %||% if (ctx$mixed) "marginal" else "fixed")),
    sprintf("  validation = %s,", .cg_value(vl_list, 15L)),
    sprintf("  outcome = outcome, logit = logistic, mixed = mixed, optimizer = %s,", .cg_esc(ctx$optimizer)),
    "  preds = predictors, clusters = intersect(clusters, names(mf)), formula = model_formula, mf = mf,",
    "  step = 0L, n_steps = 0L, done = TRUE, n_failed = 0L, n_warned = 0L,",
    "  fail_reasons = character(0), warn_reasons = character(0),",
    "  msgs = data.frame(level = character(0), message = character(0), stringsAsFactors = FALSE)",
    ")",
    "note <- function(level, text) job$msgs[nrow(job$msgs) + 1L, ] <<- list(level, text)",
    "",
    "# Apparent: the rows the model was fitted to - always optimistic",
    "job$pred_apparent <- .perf_predict(model, NULL, mixed)",
    "apparent <- .perf_measures(job$pred_apparent, mf[[outcome]], logistic, job$checks, outcome, \"apparent\",",
    "                           with_slope = FALSE)",
    "job$sets$apparent  <- apparent$values",
    "job$plots$apparent <- apparent$plots"
  )

  if (identical(vl$method, "split")) {
    out <- c(out, "", .cg_fill(r"---(
# Test set: prepared like the model's rows; rows with a factor level the model
# never saw cannot be predicted and are dropped
job$split <- list(variable = {{var}}, training_level = {{lvl}},
                  test_levels = sort(unique(split_value[in_test])))
test_rows <- .perf_test_rows(model, spec, test_data, outcome, logistic, mixed, note)
if (!is.null(test_rows)) {
  test <- .perf_measures(.perf_predict(model, test_rows, mixed), test_rows[[outcome]], logistic,
                         job$checks, outcome, "test", with_slope = TRUE)
  job$sets$test  <- test$values
  job$plots$test <- test$plots
}
)---", var = .cg_esc(ctx$split$variable), lvl = .cg_esc(ctx$split$training_level)))
  }

  multi <- ctx$mixed && length(ctx$clusters) > 1L
  if (identical(vl$method, "cv")) {
    out <- c(out, "", .cg_fill(r"---(
# Cross-validation: {{k}} folds x {{reps}} repeats, every fold drawn now with the seed
# EDARK used. Each step refits the model without one fold (same covariates) and
# predicts the rows left out; measures are computed per repeat on the pooled
# out-of-fold predictions and averaged over repeats.
set.seed({{seed}})
job$plan <- .perf_cv_plan(mf, {{k}}, {{reps}}, outcome, logistic, job$clusters, note)
job$n_steps <- nrow(job$plan$steps)
job$validation$cv_folds <- job$plan$k
job$oof <- matrix(NA_real_, nrow(mf), job$plan$repeats)
)---", k = vl$cv_folds, reps = vl$cv_repeats, seed = vl$seed),
      if (multi) "note(\"note\", sprintf(\"Resampling is grouped by %s, the first cluster variable.\", job$clusters[1L]))",
      "for (i in seq_len(job$n_steps)) job <- .perf_cv_step(job, i)")
  }

  if (identical(vl$method, "bootstrap")) {
    out <- c(out, "", .cg_fill(r"---(
# Bootstrap optimism correction (Harrell): {{B}} resamples, every one drawn now with
# the seed EDARK used. Each step refits the model on a resample and scores it on
# the resample and on the original rows; the mean difference is the optimism.
set.seed({{seed}})
job$plan <- .perf_boot_plan(mf, {{B}}, job$clusters)
job$n_steps <- length(job$plan)
job$boot_app <- matrix(NA_real_, job$n_steps, length(.PERF_SCORE_KEYS),
                       dimnames = list(NULL, .PERF_SCORE_KEYS))
job$boot_test <- job$boot_app
job$grid <- .perf_curve_grid(job$pred_apparent)
job$curve_app <- matrix(NA_real_, job$n_steps, length(job$grid))
job$curve_test <- job$curve_app
)---", B = vl$bootstrap_reps, seed = vl$seed),
      if (multi) "note(\"note\", sprintf(\"Resampling is grouped by %s, the first cluster variable.\", job$clusters[1L]))",
      "for (b in seq_len(job$n_steps)) job <- .perf_boot_step(job, b)")
  }

  c(out, "",
    "# Every set's measures, the summary table and the figures, as EDARK stores them",
    "performance <- .perf_job_finish(job)",
    "performance$metrics",
    if (ctx$figures) "for (set in performance$plots) for (p in set) print(p)")
}


# ── Results ───────────────────────────────────────────────────────────────────

.cg_results <- function(st, ctx, inc) {
  res  <- st$analysis_result
  rg   <- res$results_generation
  outs <- rg$outputs %||% character(0)
  unadj <- isTRUE(rg$include_unadjusted)
  # The fit statistics table reports AUC only if Performance had run when it was made
  with_perf <- inc("performance") && !is.null(res$performance$run_at) && !is.null(rg$generated_at) &&
    res$performance$run_at <= rg$generated_at
  out <- c(sprintf("# Outputs generated: %s", paste(outs, collapse = ", ")),
           if (with_perf) "model_result$performance <- performance[setdiff(names(performance), \"plots\")]")
  if (unadj) {
    out <- c(out, "",
             .cg_comment(paste("Unadjusted estimates: one model per predictor holding that predictor alone,",
                               "fitted to the adjusted model's own rows so both share one n",
                               if (ctx$mixed) "(and the same random intercepts)." else ".")),
             "unadjusted <- fit_unadjusted_models(model_result)",
             "unadjusted$status")
  }
  if (any(c("results_table", "forest_plot") %in% outs)) {
    out <- c(out, "",
             sprintf("# Results table: %s (95%% CI) and p for each predictor", if (ctx$logit) "odds ratio" else "coefficient"),
             sprintf("results_table <- build_results_table(model_result, %s)", if (unadj) "unadjusted" else "NULL"),
             if ("results_table" %in% outs) "results_table_gt(results_table)")
  }
  if ("forest_plot" %in% outs) {
    out <- c(out, "", "# Forest plot of the adjusted estimates", "forest_plot <- build_forest_plot(results_table)",
             if (ctx$figures) "print(forest_plot)")
  }
  if ("fit_statistics" %in% outs) {
    out <- c(out, "", "fit_statistics_table <- build_fit_statistics_table(model_result)", "fit_statistics_table")
  }
  if (!is.null(res$methods_paragraph)) {
    out <- c(out, "", "# Methods paragraph, as written by EDARK:", "#",
             .cg_comment(strsplit(res$methods_paragraph, "\n\\s*\n")[[1]]))
  }
  out
}
