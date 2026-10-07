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
#' Where the app's own code decides a number, the script carries a copy of
#' that function (\code{inst/codegen/helpers.R}), so the script and the app
#' compute the same values. If they disagree, the app has a bug - or one of
#' the copies has drifted (§N6.14).
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

# Helper chunks from inst/codegen/helpers.R, by name
.cg_helper_chunks <- function() {
  path <- system.file("codegen", "helpers.R", package = "edark")
  if (!nzchar(path)) stop("inst/codegen/helpers.R not found.", call. = FALSE)
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  starts <- grep("^# @chunk ", lines)
  ends <- c(starts[-1L] - 1L, length(lines))
  chunks <- lapply(seq_along(starts), function(i) {
    x <- lines[(starts[i] + 1L):ends[i]]
    while (length(x) && !nzchar(trimws(x[length(x)]))) x <- x[-length(x)]
    x
  })
  stats::setNames(chunks, sub("^# @chunk ", "", lines[starts]))
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


# ── Public entry point ────────────────────────────────────────────────────────

#' Generate the analysis R script
#'
#' @param st A snapshot from \code{export_state()}.
#' @param opts A list: \code{data_source} (\code{"file"} reads the input
#'   dataset with \code{readRDS(data_path)}; \code{"liver_tx"} uses the
#'   built-in dataset), \code{data_path}, \code{figures} (logical: include the
#'   ggplot code for figures), \code{time} (shown in the header).
#' @return A list: \code{text} (the script, one string), \code{lines},
#'   \code{sections} (data.frame: id, title, status - \code{"included"},
#'   \code{"stale"}, \code{"not_run"} - and reason), \code{packages},
#'   \code{seeds} (data.frame: what, seed) and \code{messages}
#'   (data.frame: level, message).
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
  if (inc("results"))       .add("Results (Analyze \u203a Model \u203a Results)", .cg_results(st, ctx))

  helpers <- .cg_helpers_needed(ctx, inc)
  chunks  <- .cg_helper_chunks()
  helper_lines <- if (length(helpers)) {
    c("", "", "# ---- Helpers ------------------------------------------------------------------",
      .cg_comment(c("Copies of the EDARK functions that decide the numbers below, so this script computes exactly what the app shows.")),
      unlist(lapply(helpers, function(h) c("", chunks[[h]]))))
  }

  pkgs <- .cg_packages(ctx, inc)
  # ggplot2 only when a figure was written (a script with no fitted output has none)
  if (!any(grepl("ggplot(", unlist(body), fixed = TRUE) | grepl("ggroc(", unlist(body), fixed = TRUE))) {
    pkgs <- setdiff(pkgs, "ggplot2")
  }
  lines <- c(
    .cg_header(st, plan, opts),
    .cg_setup(pkgs),
    helper_lines,
    unlist(body),
    ""
  )
  # Plain ASCII throughout (headings carry the nav's "›")
  lines <- gsub("\u203a", ">", lines, fixed = TRUE)

  list(
    text     = paste(lines, collapse = "\n"),
    lines    = lines,
    sections = plan$sections,
    packages = pkgs,
    seeds    = ctx$seeds,
    messages = plan$messages
  )
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

# Values shared by several sections
.cg_context <- function(st, plan, opts) {
  spec <- st$analysis_spec
  res  <- st$analysis_result
  snap <- res$specification_snapshot
  mt   <- snap$model_design$model_type
  vr   <- snap$variable_roles
  pf   <- res$performance
  dg   <- res$diagnostics
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
    diag       = dg,
    split      = analysis_split(spec),
    seeds      = seeds
  )
}

.cg_helpers_needed <- function(ctx, inc) {
  h <- character(0)
  if (inc("table1")) h <- c(h, "format_p", "with_seed", "categorical_test")
  if (inc("univariable") || inc("stepwise") || inc("lasso")) h <- c(h, "set_reference_levels", "droplevels_cols", "can_model")
  if (inc("model")) h <- c(h, "set_reference_levels", "prepare_model_rows", "coef_table", "fit_statistics")
  if (inc("univariable")) h <- c(h, "coef_table")
  if (inc("performance")) {
    h <- c(h, "predict_response", "performance_measures")
    if (ctx$perf$validation$method %in% c("cv", "bootstrap")) h <- c(h, "performance_scores", "resample_refit")
    if (identical(ctx$perf$validation$method, "cv")) h <- c(h, "cv_folds")
    if (identical(ctx$perf$validation$method, "bootstrap")) h <- c(h, "bootstrap_samples")
    if (ctx$logit) h <- c(h, "calibration_bins")
  }
  order <- names(.cg_helper_chunks())
  intersect(order, unique(h))
}

.cg_packages <- function(ctx, inc) {
  dg_checks <- ctx$diag$checks
  t1_extra <- if (inc("table1")) c("cardx", "smd")
  unique(c(
    "dplyr",
    if (ctx$figures) "ggplot2",
    if (inc("table1")) "gtsummary", t1_extra,
    if (inc("lasso")) "glmnet",
    if (inc("model") && ctx$mixed) c("lme4", if (ctx$model_type == "linear_mixed") "lmerTest", "performance"),
    if (inc("diagnostics") && any(c("vif", "residuals", "linearity") %in% dg_checks)) "performance",
    if (inc("diagnostics") && identical(ctx$model_type, "linear") && "residuals" %in% dg_checks) "lmtest",
    if (inc("diagnostics") && "separation" %in% dg_checks) "detectseparation",
    if (inc("performance") && ctx$logit && "discrimination" %in% ctx$perf$checks) "pROC"
  ))
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
    .cg_comment(paste("Run it top to bottom in a fresh R session. Each object is named in the",
                      "comment above it. If this script and EDARK disagree, EDARK has a bug.")),
    "# =============================================================================="
  )
}

.cg_setup <- function(pkgs) {
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
pacman::p_load({{pkgs}})
)---", pkgs = paste(pkgs, collapse = ", ")))
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


# ── Analyze › Setup ───────────────────────────────────────────────────────────

.cg_analysis_data <- function(st, ctx) {
  spec <- st$analysis_spec
  vr   <- spec$variable_roles
  ps   <- spec$purpose_specification %||% .default_purpose_specification()
  exposure <- vr$exposure_variable
  refs <- vr$reference_levels %||% list()
  out <- c(
    "# Analyze works on a frozen copy of the working dataset with a row ID added.",
    "analysis_data <- working_data",
    "analysis_data$.edark_row_id <- seq_len(nrow(analysis_data))",
    "",
    "# Roles (Step 1)",
    sprintf("outcome  <- %s", .cg_esc(vr$outcome_variable %||% NA_character_)),
    sprintf("exposure <- %s", if (is.null(exposure) || !nzchar(exposure)) "NULL" else .cg_esc(exposure)),
    sprintf("reference_levels <- %s", .cg_named_chr(refs))
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
  spec <- st$analysis_spec
  vr   <- spec$variable_roles
  res  <- st$analysis_result
  data <- st$analysis_data
  t1v <- setdiff(vr$table1_variables, ".edark_row_id")
  t1v <- intersect(names(data), t1v)
  pri <- c(vr$exposure_variable, vr$outcome_variable)
  pri <- intersect(pri[nzchar(pri)], t1v)
  t1v <- c(pri, setdiff(t1v, pri))

  out <- c(.cg_comment("Every row of the analysis dataset, including any test rows. Variables: exposure, outcome, then the rest in dataset order."),
           sprintf("table1_vars <- %s", .cg_chr(t1v)))

  .steps <- function(tbl) names(tbl$call_list %||% list())
  tabs <- list(overall = res$result_tables$table1_overall,
               by_exposure = res$result_tables$table1_by_exposure,
               by_outcome = res$result_tables$table1_by_outcome)
  if (!is.null(tabs$overall)) {
    out <- c(out, "", "# Whole cohort",
             "table1_overall <- tbl_summary(analysis_data[, table1_vars, drop = FALSE], missing = \"no\") %>%",
             "  bold_labels()",
             "table1_overall")
  }
  for (k in c("by_exposure", "by_outcome")) {
    tbl <- tabs[[k]]
    if (is.null(tbl)) next
    by  <- if (k == "by_exposure") vr$exposure_variable else vr$outcome_variable
    st_ <- .steps(tbl)
    vars <- intersect(unique(c(t1v, by)), names(data))
    obj <- paste0("table1_", k)
    chain <- c(
      sprintf("%s <- tbl_summary(analysis_data[, %s, drop = FALSE],",
              obj, if (identical(vars, t1v)) "table1_vars" else sprintf("c(table1_vars, %s)", .cg_esc(by))),
      sprintf("                         by = %s, missing = \"no\") %%>%%", .cg_esc(by)),
      "  add_overall() %>%",
      "  bold_labels()"
    )
    if ("add_p" %in% st_) {
      chain[length(chain)] <- paste(chain[length(chain)], "%>%")
      chain <- c(chain,
        "  # Kruskal-Wallis for numbers; chi-squared, or Fisher's exact test when an",
        "  # expected count is below 5, for categories",
        "  add_p(",
        "    test = list(all_continuous() ~ \"kruskal.test\", all_categorical() ~ categorical_test),",
        "    pvalue_fun = function(x) ifelse(is.na(x), NA_character_, format_p(x))",
        "  )")
    }
    if ("add_difference" %in% st_) {
      chain[length(chain)] <- paste(chain[length(chain)], "%>%")
      chain <- c(chain, "  add_difference(test = list(all_continuous() ~ \"smd\", all_categorical() ~ \"smd\"))")
    }
    if ("modify_spanning_header" %in% st_) {
      chain[length(chain)] <- paste(chain[length(chain)], "%>%")
      chain <- c(chain, sprintf("  modify_spanning_header(all_stat_cols(stat_0 = FALSE) ~ %s)",
                                .cg_esc(paste0("**", by, "**"))))
    }
    out <- c(out, "", sprintf("# By %s", if (k == "by_exposure") "exposure" else "outcome"), chain, obj)
  }
  out
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
           sprintf("candidates <- %s", .cg_chr(pool)),
           if (inc("stepwise") || inc("lasso")) c(
             "# Stepwise and LASSO use the rows complete for all of these",
             "selection_vars <- intersect(c(outcome, exposure, candidates), names(model_data))"))

  if (inc("univariable")) {
    ord <- intersect(names(md), pool)
    if (length(exposure) && exposure %in% ord) ord <- c(exposure, setdiff(ord, exposure))
    thr <- vs$univariable_p_threshold %||% 0.2
    out <- c(out, "", .cg_fill(r"---(
# Univariable screen: outcome ~ candidate, one {{kind}} model per candidate, on
# the rows complete for both. {{measure}}
univariable_threshold <- {{thr}}
screen_data <- set_reference_levels(model_data, reference_levels)
univariable <- do.call(rbind, lapply(candidates, function(v) {
  rows <- screen_data[complete.cases(screen_data[, c(outcome, v)]), , drop = FALSE]
  if (nrow(rows) == 0L || !can_model(rows[[v]])) return(NULL)
  rows <- droplevels_cols(rows, v)
  fit <- tryCatch({{fit}}, error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  ct <- coef_table(fit, rows)
  ct <- ct[ct$term != "(Intercept)", , drop = FALSE]
  data.frame(variable = v, term = ct$term, estimate = ct$effect, conf.low = ct$effect.low,
             conf.high = ct$effect.high, p.value = ct$p.value, stringsAsFactors = FALSE)
}))
# Exposure first, then dataset order
screen_order <- {{ord}}
univariable <- univariable[order(match(univariable$variable, screen_order), univariable$term,
                                 method = "radix"), , drop = FALSE]
univariable$suggested <- !is.na(univariable$p.value) & univariable$p.value < univariable_threshold
rownames(univariable) <- NULL
univariable
)---", kind = if (binary) "logistic" else "linear",
    measure = if (binary) "Estimates are odds ratios." else "Estimates are regression coefficients.",
    thr = .cg_num(thr), fit = sprintf(fit_fn, "as.formula(paste(outcome, \"~\", v))", "rows"),
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
      sprintf("%s_data <- set_reference_levels(%s_data, reference_levels)", obj, obj),
      if (is.data.frame(ex) && nrow(ex) > 0L)
        .cg_comment(paste0("Left out - cannot be modelled on these rows: ",
                           paste(sprintf("%s (%s)", ex$variable, ex$reason), collapse = "; "), ".")),
      sprintf("%s_candidates <- %s", obj, .cg_chr(keep)),
      sprintf("%s_data <- droplevels_cols(%s_data, c(%s_candidates, exposure))", obj, obj, obj))
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
      sprintf("stepwise_start <- %s", sprintf(fit_fn, start, "stepwise_data")),
      "stepwise_fit <- step(",
      "  stepwise_start,",
      sprintf("  scope = list(lower = ~ %s,", lower),
      sprintf("               upper = ~ %s),", upper),
      sprintf("  direction = %s, k = %s, trace = 0", .cg_esc(dir),
              if (identical(crit, "BIC")) "log(nrow(stepwise_data))" else "2"),
      ")",
      "stepwise_selected <- intersect(stepwise_candidates, attr(terms(stepwise_fit), \"term.labels\"))",
      "stepwise_selected"
    )
    head <- sprintf("# Stepwise selection: %s, by %s.%s", dir, crit,
                    if (length(exposure)) " The exposure is held in every model, so candidates are chosen for what they add alongside it." else "")
    out <- c(out, "", .cg_comment(sub("^# ", "", head)),
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
    cd <- md[, intersect(pool, names(md)), drop = FALSE]
    nv <- names(cd)[vapply(cd, is.numeric, logical(1))]
    fv <- names(cd)[vapply(cd, is.factor, logical(1))]
    out <- c(out, "", .cg_comment("Collinearity between candidates: Pearson r for numeric pairs, Cramer's V for factor pairs; pairs above 0.7 are flagged."),
             "collin_data <- model_data[, intersect(candidates, names(model_data)), drop = FALSE]")
    if (length(nv) >= 2L) {
      out <- c(out, sprintf("cor_matrix <- cor(collin_data[, %s, drop = FALSE], use = \"pairwise.complete.obs\")",
                            .cg_chr(nv, 2L)))
    }
    if (length(fv) >= 2L) {
      out <- c(out, .cg_fill(r"---(
factor_vars <- {{fv}}
cramers_v_matrix <- matrix(NA_real_, length(factor_vars), length(factor_vars),
                           dimnames = list(factor_vars, factor_vars))
diag(cramers_v_matrix) <- 1
for (i in seq_along(factor_vars)) for (j in seq_along(factor_vars)) {
  if (j <= i) next
  tb <- table(collin_data[[factor_vars[i]]], collin_data[[factor_vars[j]]])
  chi <- suppressWarnings(chisq.test(tb, correct = FALSE))
  k <- min(dim(tb))
  v <- if (k <= 1L || sum(tb) == 0L) NA_real_ else sqrt(unname(chi$statistic) / (sum(tb) * (k - 1L)))
  cramers_v_matrix[i, j] <- cramers_v_matrix[j, i] <- v
}
)---", fv = .cg_chr(fv)))
    }
    if (length(nv) >= 2L || length(fv) >= 2L) {
      out <- c(out, .cg_fill(r"---(
flagged_pairs <- do.call(rbind, lapply(list({{mats}}), function(m) {
  idx <- which(upper.tri(m) & !is.na(m) & abs(m) > 0.7, arr.ind = TRUE)
  data.frame(var1 = rownames(m)[idx[, 1]], var2 = colnames(m)[idx[, 2]], value = round(m[idx], 3))
}))
flagged_pairs
)---", mats = paste(c(if (length(nv) >= 2L) "cor_matrix", if (length(fv) >= 2L) "cramers_v_matrix"), collapse = ", ")))
    } else {
      out <- c(out, "# Fewer than two numeric and two factor candidates: nothing to compare.")
    }
    if (ctx$figures && length(nv) >= 2L) {
      out <- c(out, .cg_fill(r"---(
ggplot(as.data.frame(as.table(cor_matrix)), aes(Var1, Var2, fill = Freq)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(Freq, 2)), size = 3) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#d6604d", limits = c(-1, 1), name = "r") +
  labs(title = "Pearson correlation matrix", x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
)---"))
    }
  }
  out
}


# ── Model ─────────────────────────────────────────────────────────────────────

.cg_model <- function(st, ctx) {
  res  <- st$analysis_result
  snap <- res$specification_snapshot
  vr   <- snap$variable_roles
  rs   <- res$run_status
  mt   <- ctx$model_type
  refs <- vr$reference_levels %||% list()
  refs <- refs[intersect(names(refs), c(ctx$outcome, ctx$preds))]

  ev   <- rs$outcome_event

  out <- c(
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
    "model_rows <- prepare_model_rows(model_data, outcome, predictors, clusters,",
    sprintf("                                 reference_levels = %s)", .cg_named_chr(refs, 33L)),
    "",
    sprintf("model_formula <- %s", .cg_formula(ctx$outcome, ctx$preds, ctx$clusters, start = 17L)),
    sprintf("fit_model <- function(formula, data) %s", .cg_fit_call(mt, "formula", "data", ctx$optimizer)),
    "model <- fit_model(model_formula, model_rows)",
    "summary(model)",
    "",
    sprintf("# %s", edark_inference_note(mt)),
    "model_coefficients <- coef_table(model, model_rows)",
    "model_coefficients",
    "",
    sprintf("model_fit_statistics <- fit_statistics(model, %s, model_rows, outcome)", .cg_esc(mt)),
    "model_fit_statistics"
  )
  out
}


# ── Diagnostics ───────────────────────────────────────────────────────────────

.cg_diagnostics <- function(st, ctx) {
  dg <- ctx$diag
  ck <- dg$checks
  mt <- ctx$model_type
  out <- c(sprintf("# Checks run: %s", paste(ck, collapse = ", ")),
           "diagnostics <- list()",
           "row_ids <- model_rows$.edark_row_id")

  if ("residuals" %in% ck) {
    if (ctx$logit) {
      out <- c(out, "", .cg_fill(r"---(
# Residuals: binned response residuals (y - p). About 95% of bins should include 0.
binned <- as.data.frame(performance::binned_residuals(model, residuals = "response"))
diagnostics$binned_inside <- mean(binned$group == "yes", na.rm = TRUE)
)---"), if (ctx$figures) .cg_fill(r"---(
ggplot(binned, aes(xbar, ybar, colour = group == "yes")) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0) +
  geom_point(size = 2) +
  scale_colour_manual(values = c(`TRUE` = "#2c7be5", `FALSE` = "#d6604d"),
                      labels = c(`TRUE` = "Includes 0", `FALSE` = "Excludes 0"), name = NULL) +
  labs(title = "Binned residuals", x = "Predicted probability", y = "Average residual") +
  theme_minimal()
)---"))
    } else {
      out <- c(out, "", "# Residuals",
               "resid_df <- data.frame(fitted = as.numeric(fitted(model)), resid = as.numeric(residuals(model)))",
               if (mt == "linear") "resid_df$std <- as.numeric(rstandard(model))"
               else "resid_df$std <- resid_df$resid / sigma(model)   # conditional residuals",
               if (mt == "linear") c("diagnostics$breusch_pagan <- lmtest::bptest(model)   # constant variance",
                                     "diagnostics$breusch_pagan"),
               if (ctx$figures) .cg_fill(r"---(
ggplot(resid_df, aes(fitted, resid)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_point(alpha = 0.45, colour = "#2c7be5") +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "#d6604d") +
  labs(title = "Residuals vs fitted", x = "Fitted value", y = "Residual") +
  theme_minimal()
ggplot(resid_df, aes(sample = std)) +
  stat_qq(alpha = 0.55, colour = "#2c7be5") +
  geom_abline(slope = 1, intercept = 0, colour = "#d6604d") +
  labs(title = "Normal Q-Q (standardised residuals)", x = "Theoretical quantile", y = "Sample quantile") +
  theme_minimal()
ggplot(resid_df, aes(fitted, sqrt(abs(std)))) +
  geom_point(alpha = 0.45, colour = "#2c7be5") +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "#d6604d") +
  labs(title = "Scale-location", x = "Fitted value", y = "sqrt(|standardised residual|)") +
  theme_minimal()
)---"))
    }
  }

  if ("linearity" %in% ck) {
    terms <- dg$linearity$terms
    if (length(terms) == 0L) {
      out <- c(out, "", "# Linearity: the model has no continuous predictor, so there is nothing to check.")
    } else if (ctx$logit) {
      out <- c(out, "", "# Linearity: binned residuals against each continuous predictor",
               sprintf("linearity_terms <- %s", .cg_chr(terms)),
               "linearity <- do.call(rbind, lapply(linearity_terms, function(v) {",
               "  d <- as.data.frame(performance::binned_residuals(model, term = v, residuals = \"response\"))",
               "  d$term <- v",
               "  d",
               "}))",
               "diagnostics$linearity_inside <- tapply(linearity$group == \"yes\", linearity$term, mean)",
               "diagnostics$linearity_inside",
               if (ctx$figures) c(
                 "ggplot(linearity, aes(xbar, ybar, colour = group == \"yes\")) +",
                 "  geom_hline(yintercept = 0, linetype = \"dashed\", colour = \"grey55\") +",
                 "  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0) +",
                 "  geom_point(size = 1.8) +",
                 "  facet_wrap(~ term, scales = \"free_x\") +",
                 "  scale_colour_manual(values = c(`TRUE` = \"#2c7be5\", `FALSE` = \"#d6604d\"), guide = \"none\") +",
                 "  labs(title = \"Binned residuals vs each continuous predictor\", x = NULL, y = \"Average residual\") +",
                 "  theme_minimal()"))
    } else {
      out <- c(out, "", "# Linearity: residuals against each continuous predictor",
               sprintf("linearity_terms <- %s", .cg_chr(terms)),
               if (ctx$figures) c(
                 "mf <- model.frame(model)",
                 "linearity <- do.call(rbind, lapply(linearity_terms, function(v) {",
                 "  data.frame(term = v, x = as.numeric(mf[[v]]), resid = as.numeric(residuals(model)))",
                 "}))",
                 "ggplot(linearity, aes(x, resid)) +",
                 "  geom_hline(yintercept = 0, linetype = \"dashed\", colour = \"grey55\") +",
                 "  geom_point(alpha = 0.35, colour = \"#2c7be5\") +",
                 "  geom_smooth(method = \"loess\", formula = y ~ x, se = FALSE, colour = \"#d6604d\") +",
                 "  facet_wrap(~ term, scales = \"free_x\") +",
                 "  labs(title = \"Residuals vs each continuous predictor\", x = NULL, y = \"Residual\") +",
                 "  theme_minimal()")
               else "# (a figure only - turn on figures to include its code)")
    }
  }

  if ("influence" %in% ck) {
    out <- c(out, "", .cg_fill(r"---(
# Influence: Cook's distance and leverage; the cut-off is 4 / n
influence <- data.frame(.edark_row_id = row_ids,
                        cooks     = as.numeric(cooks.distance(model)),
                        leverage  = as.numeric(hatvalues(model)),
                        std_resid = as.numeric(rstandard(model)))
diagnostics$cooks_threshold <- 4 / nrow(influence)
diagnostics$cooks_max <- max(influence$cooks, na.rm = TRUE)
diagnostics$n_above <- sum(influence$cooks > diagnostics$cooks_threshold, na.rm = TRUE)
most_influential <- head(influence[order(-influence$cooks), , drop = FALSE], 10)
most_influential
)---"), if (ctx$figures) .cg_fill(r"---(
ggplot(influence, aes(seq_along(cooks), cooks, colour = cooks > diagnostics$cooks_threshold)) +
  geom_segment(aes(xend = seq_along(cooks), yend = 0)) +
  geom_hline(yintercept = diagnostics$cooks_threshold, linetype = "dashed", colour = "#d6604d") +
  scale_colour_manual(values = c(`FALSE` = "#2c7be5", `TRUE` = "#d6604d"), guide = "none") +
  labs(title = "Cook's distance", x = "Observation", y = "Cook's distance") +
  theme_minimal()
)---"))
  }

  if ("vif" %in% ck) {
    if (is.data.frame(dg$vif)) {
      out <- c(out, "", "# Collinearity: variance inflation factors (5 to 10 moderate, above 10 high)",
               "vif <- as.data.frame(performance::check_collinearity(model))",
               "vif[, c(\"Term\", \"VIF\")]")
    } else {
      out <- c(out, "", "# Collinearity: VIF needs at least two predictors, so it is not computed.")
    }
  }

  if ("separation" %in% ck) {
    out <- c(out, "", .cg_fill(r"---(
# Separation: does a predictor perfectly predict the outcome?{{note}}
separation <- glm({{fmla}}, data = model.frame(model), family = binomial(),
                  method = detectseparation::detect_separation)
diagnostics$separation <- isTRUE(separation$outcome)
separation
)---", note = if (ctx$mixed) " Checked on the fixed effects." else "",
    fmla = if (ctx$mixed) "formula(model, fixed.only = TRUE)" else "formula(model)"))
  }

  if ("random_effects" %in% ck) {
    out <- c(out, "", .cg_fill(r"---(
# Random effects: variance, SD and ICC per cluster variable. The residual
# variance is {{resid}}.
vc <- as.data.frame(lme4::VarCorr(model))
vc <- vc[is.na(vc$var2) & vc$grp != "Residual", , drop = FALSE]
residual_variance <- {{resid_code}}
random_effects <- data.frame(group = vc$grp, variance = vc$vcov, sd = vc$sdcor,
                             icc = vc$vcov / (sum(vc$vcov) + residual_variance))
random_effects$n_clusters <- vapply(random_effects$group, function(g) length(unique(model_rows[[g]])), integer(1))
random_effects
)---", resid = if (ctx$logit) "pi^2 / 3 (the latent scale of a logistic model)" else "the model's sigma^2",
    resid_code = if (ctx$logit) "pi^2 / 3" else "sigma(model)^2"))
  }
  c(out, "diagnostics")
}


# ── Performance ───────────────────────────────────────────────────────────────

.cg_performance <- function(st, ctx) {
  pf <- ctx$perf
  vl <- pf$validation
  mt <- ctx$model_type
  out <- c(
    sprintf("# Measures: %s. Validation: %s.", paste(pf$checks, collapse = ", "),
            tolower(.PERF_METHOD_LABELS[[vl$method]])),
    if (ctx$mixed) "# Mixed models predict from the fixed effects alone (re.form = NA), as for a patient from an unseen cluster.",
    sprintf("performance_checks <- %s", .cg_chr(pf$checks)),
    "mf <- model.frame(model)   # the model's own rows",
    "y  <- mf[[outcome]]",
    "",
    "# Apparent: the rows the model was fitted to - always optimistic",
    "pred_apparent <- predict_response(model, mixed = mixed)",
    "apparent <- performance_measures(pred_apparent, y, logistic, performance_checks, with_slope = FALSE)"
  )

  if (identical(vl$method, "split")) {
    out <- c(out, "", .cg_fill(r"---(
# Test set: prepared like the model's rows; rows with a factor level the model
# never saw cannot be predicted and are dropped
test_rows <- prepare_model_rows(test_data, outcome, predictors, character(0), reference_levels)
unseen <- rep(FALSE, nrow(test_rows))
for (v in c(outcome, predictors)) {
  if (is.factor(mf[[v]])) unseen <- unseen | !as.character(test_rows[[v]]) %in% levels(mf[[v]])
}
test_rows <- test_rows[!unseen, , drop = FALSE]
for (v in c(outcome, predictors)) {
  if (is.factor(mf[[v]])) test_rows[[v]] <- factor(as.character(test_rows[[v]]), levels = levels(mf[[v]]))
}
pred_test <- predict_response(model, test_rows, mixed)
test <- performance_measures(pred_test, test_rows[[outcome]], logistic, performance_checks, with_slope = TRUE)
)---"))
  }

  if (vl$method %in% c("cv", "bootstrap")) {
    out <- c(out, "",
             "# Resampling refits the model with the same covariates (the variable selection is not repeated).",
             sprintf("refit_model <- function(formula, data) %s", .cg_fit_call(mt, "formula", "data", ctx$optimizer, satterthwaite = FALSE)),
             sprintf("resample_clusters <- %s%s", .cg_chr(ctx$clusters),
                     if (length(ctx$clusters) > 0L) "   # whole clusters of the first are resampled" else ""))
  }

  if (identical(vl$method, "cv")) {
    out <- c(out, "", .cg_fill(r"---(
# Cross-validation: {{k}} folds x {{reps}} repeats. Each fold's model predicts the rows it
# left out; measures are computed per repeat on the pooled out-of-fold predictions and
# averaged over repeats.
set.seed({{seed}})
cv_plan <- cv_folds(mf, {{k}}, {{reps}})
oof <- matrix(NA_real_, nrow(mf), {{reps}})
cv_failed <- 0L
for (r in seq_len({{reps}})) {
  for (f in seq_len(cv_plan$k)) {
    out  <- cv_plan$folds[[r]] == f
    fit  <- refit(mf[!out, , drop = FALSE])
    left <- mf[out, , drop = FALSE]
    ok   <- if (!is.null(fit)) predictable(fit$data, left) else FALSE
    p    <- if (any(ok)) tryCatch(predict_response(fit$model, align_levels(fit$data, left[ok, , drop = FALSE]), mixed),
                                  error = function(e) NULL)
    if (is.null(p)) { cv_failed <- cv_failed + 1L; next }
    oof[which(out)[ok], r] <- p
  }
}
cv_scores <- t(vapply(seq_len(ncol(oof)), function(r) performance_scores(oof[, r], y, logistic),
                      numeric(length(score_names))))
cv_keys <- performance_keys(performance_checks, logistic)
cv <- list(n = as.integer(round(mean(colSums(is.finite(oof))))), folds = cv_plan$k, failed = cv_failed,
           mean = colMeans(cv_scores[, cv_keys, drop = FALSE], na.rm = TRUE),
           sd   = if (nrow(cv_scores) > 1L) apply(cv_scores[, cv_keys, drop = FALSE], 2, sd, na.rm = TRUE))
cv
)---", k = vl$cv_folds, reps = vl$cv_repeats, seed = vl$seed))
  }

  if (identical(vl$method, "bootstrap")) {
    cal <- "calibration" %in% pf$checks
    out <- c(out, "", .cg_fill(r"---(
# Bootstrap optimism correction (Harrell): each resample's model is scored on the
# resample and on the original rows; the mean difference is the optimism.
set.seed({{seed}})
boot_plan <- bootstrap_samples(mf, {{B}})
boot_apparent <- boot_test <- matrix(NA_real_, {{B}}, length(score_names), dimnames = list(NULL, score_names))
{{curve_init}}
for (b in seq_len({{B}})) {
  rows <- mf[boot_plan[[b]]$rows, , drop = FALSE]
  if (!is.null(boot_plan[[b]]$copy)) {
    for (cl in resample_clusters) rows[[cl]] <- paste(as.character(rows[[cl]]), boot_plan[[b]]$copy, sep = "#")
  }
  fit <- refit(rows)
  if (is.null(fit) || !all(predictable(fit$data, mf))) next
  p_boot <- tryCatch(predict_response(fit$model, NULL, mixed), error = function(e) NULL)
  p_orig <- tryCatch(predict_response(fit$model, align_levels(fit$data, mf), mixed), error = function(e) NULL)
  if (is.null(p_boot) || is.null(p_orig)) next
  boot_apparent[b, ] <- performance_scores(p_boot, fit$data[[outcome]], logistic)
  boot_test[b, ]     <- performance_scores(p_orig, y, logistic){{curve_step}}
}
boot_ok <- complete.cases(boot_apparent[, "brier"]) | complete.cases(boot_apparent[, "rmse"])
boot_keys <- performance_keys(performance_checks, logistic)
apparent_scores <- performance_scores(pred_apparent, y, logistic)
optimism <- colMeans(boot_apparent[boot_ok, , drop = FALSE] - boot_test[boot_ok, , drop = FALSE], na.rm = TRUE)
bootstrap <- data.frame(measure = boot_keys, apparent = apparent_scores[boot_keys],
                        optimism = optimism[boot_keys], corrected = (apparent_scores - optimism)[boot_keys],
                        row.names = NULL)
bootstrap <- bootstrap[is.finite(bootstrap$corrected), , drop = FALSE]
boot_usable <- sum(boot_ok)   # resamples that could be fitted and scored
bootstrap
)---", seed = vl$seed, B = vl$bootstrap_reps,

    curve_init = if (cal) paste(
      "# Calibration curve points: the central 98% of the apparent predictions",
      "curve_range <- quantile(pred_apparent, c(0.01, 0.99), na.rm = TRUE, names = FALSE)",
      "curve_grid  <- if (all(is.finite(curve_range)) && curve_range[1] != curve_range[2]) seq(curve_range[1], curve_range[2], length.out = 50) else numeric(0)",
      paste0("curve_apparent <- curve_test <- matrix(NA_real_, ", vl$bootstrap_reps, ", length(curve_grid))"),
      "y_num <- function(v) if (logistic) as.integer(v == levels(v)[2L]) else as.numeric(v)",
      sep = "\n") else "",
    curve_step = if (cal) paste0(
      "\n  curve_apparent[b, ] <- smooth_calibration(p_boot, y_num(fit$data[[outcome]]), curve_grid)",
      "\n  curve_test[b, ]     <- smooth_calibration(p_orig, y_num(y), curve_grid)") else ""))
    if (cal) {
      out <- c(out, .cg_fill(r"---(
# Bias-corrected calibration curve
calibration_curve <- data.frame(predicted = curve_grid,
                                apparent  = smooth_calibration(pred_apparent, y_num(y), curve_grid))
calibration_curve$corrected <- calibration_curve$apparent -
  colMeans(curve_apparent[boot_ok, , drop = FALSE] - curve_test[boot_ok, , drop = FALSE], na.rm = TRUE)
{{clamp}}
)---", clamp = if (ctx$logit) "calibration_curve$corrected <- pmin(pmax(calibration_curve$corrected, 0), 1)" else ""),
      if (ctx$figures) .cg_fill(r"---(
ggplot(calibration_curve, aes(predicted)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey55") +
  geom_line(aes(y = apparent, colour = "Apparent"), linetype = "dotted") +
  geom_line(aes(y = corrected, colour = "Bias-corrected")) +
  scale_colour_manual(values = c(Apparent = "grey55", `Bias-corrected` = "#2c7be5"), name = NULL) +
  labs(title = "Calibration curve (bootstrap-corrected)", x = "Predicted", y = "Observed") +
  theme_minimal()
)---"))
    }
  }

  # Figures for the apparent / test set
  if (ctx$figures) {
    sets <- c("apparent", if (identical(vl$method, "split")) "test")
    for (s in sets) {
      if (ctx$logit && "discrimination" %in% pf$checks) {
        out <- c(out, "", sprintf("if (!is.null(%s$roc)) pROC::ggroc(%s$roc) + labs(title = \"ROC curve (%s)\") + theme_minimal()", s, s, s))
      }
      if (ctx$logit && "calibration" %in% pf$checks) {
        out <- c(out, sprintf(paste0(
          "ggplot(%s$calibration, aes(predicted, observed)) +\n",
          "  geom_abline(slope = 1, intercept = 0, linetype = \"dashed\", colour = \"grey55\") +\n",
          "  geom_errorbar(aes(ymin = low, ymax = high), width = 0, colour = \"#2c7be5\") +\n",
          "  geom_point(colour = \"#2c7be5\", size = 2.5) +\n",
          "  labs(title = \"Calibration by decile (%s)\", x = \"Mean predicted probability\", y = \"Observed proportion\") +\n",
          "  theme_minimal()"), s, s))
      }
    }
  }
  sets <- c("apparent = apparent", if (identical(vl$method, "split")) "test = test",
            if (identical(vl$method, "cv")) "cross_validated = cv",
            if (identical(vl$method, "bootstrap")) "bootstrap = bootstrap")
  c(out, "", sprintf("performance <- list(%s)", paste(sets, collapse = ", ")))
}


# ── Results ───────────────────────────────────────────────────────────────────

.cg_results <- function(st, ctx) {
  res <- st$analysis_result
  rg  <- res$results_generation
  outs <- rg$outputs %||% character(0)
  mt  <- ctx$model_type
  out <- c(sprintf("# Outputs generated: %s", paste(outs, collapse = ", ")))
  unadj <- isTRUE(rg$include_unadjusted)
  if (unadj) {
    out <- c(out, "", .cg_fill(r"---(
# Unadjusted estimates: one model per predictor holding that predictor alone,
# fitted to the adjusted model's own rows so both share one n{{mixed_note}}
mf <- model.frame(model)
unadjusted <- do.call(rbind, lapply(predictors, function(v) {
  rows <- mf[, intersect(c(outcome, v, clusters), names(mf)), drop = FALSE]
  if (is.factor(rows[[v]])) rows[[v]] <- droplevels(rows[[v]])
  rhs <- paste(c(v, if (mixed) paste0("(1 | ", clusters, ")")), collapse = " + ")
  fit <- tryCatch(fit_model(as.formula(paste(outcome, "~", rhs)), rows), error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  ct <- coef_table(fit, rows)
  ct[ct$variable != "(Intercept)", , drop = FALSE]
}))
)---", mixed_note = if (ctx$mixed) " (and the same random intercepts)" else ""))
  }
  if (any(c("results_table", "forest_plot") %in% outs)) {
    cols <- "c(\"variable\", \"level\", \"effect\", \"effect.low\", \"effect.high\", \"p.value\")"
    out <- c(out, "", sprintf("# Results table: %s (95%% CI) and p for each predictor", if (ctx$logit) "odds ratio" else "coefficient"),
             sprintf("adjusted <- model_coefficients[model_coefficients$variable != \"(Intercept)\", %s]", cols),
             if (unadj && "results_table" %in% outs) c(
               sprintf("results_table <- merge(unadjusted[, %s], adjusted,", cols),
               "                       by = c(\"variable\", \"level\"), all.y = TRUE, sort = FALSE,",
               "                       suffixes = c(\".unadjusted\", \".adjusted\"))")
             else "results_table <- adjusted",
             "results_table")
  }
  if ("forest_plot" %in% outs && ctx$figures) {
    out <- c(out, "", .cg_fill(r"---(
# Forest plot of the adjusted estimates
forest <- adjusted
forest$label <- ifelse(is.na(forest$level), forest$variable, paste0(forest$variable, ": ", forest$level))
forest$label <- factor(forest$label, levels = rev(forest$label))
ggplot(forest, aes(effect, label)) +
  geom_vline(xintercept = {{null}}, linetype = "dashed", colour = "grey55") +
  geom_errorbar(aes(xmin = effect.low, xmax = effect.high), width = 0.25, orientation = "y") +
  geom_point(shape = 15, size = 2.6) +{{scale}}
  labs(x = {{xlab}}, y = NULL) +
  theme_minimal()
)---", null = if (ctx$logit) "1" else "0",
    scale = if (ctx$logit) "\n  scale_x_log10() +" else "",
    xlab = .cg_esc(if (ctx$logit) "Odds ratio (95% CI, log scale)" else "Coefficient (95% CI)")))
  }
  if ("fit_statistics" %in% outs) {
    out <- c(out, "", "# Fit statistics: model_fit_statistics (section above); AUC and calibration slope: performance")
  }
  if (!is.null(res$methods_paragraph)) {
    out <- c(out, "", "# Methods paragraph, as written by EDARK:", "#",
             .cg_comment(strsplit(res$methods_paragraph, "\n\\s*\n")[[1]]))
  }
  out
}
