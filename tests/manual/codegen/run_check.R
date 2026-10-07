# End-to-end check of the generated R script (PRD §A7.9).
#
# For each scenario in scenarios.R:
#   1. build the app state with the service functions the modules call;
#   2. build the export zip (every available item, including
#      reproduce/analysis_script.R + edark_functions.R) and unzip it;
#   3. run the exported script in a fresh R process, from the input dataset,
#      without EDARK loaded;
#   4. compare what the script computed with what the export holds -
#      data/working_dataset.rds and model/analysis_result.rds: every number,
#      every table, and every figure (the data of each layer and the labels,
#      as ggplot builds them).
#
# Run from the package root:
#   Rscript tests/manual/codegen/run_check.R [scenario ...]
# Writes the scripts, zips and a results CSV to tempdir()/edark_codegen_check
# (or $EDARK_CHECK_DIR). Exit status 1 if any comparison fails.

suppressMessages(devtools::load_all(quiet = TRUE))
source("tests/manual/codegen/scenarios.R")

args <- commandArgs(trailingOnly = TRUE)
which_sc <- if (length(args)) args else names(cg_scenarios)
out_dir <- Sys.getenv("EDARK_CHECK_DIR", file.path(tempdir(), "edark_codegen_check"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

TOL <- 1e-8
checks <- list()
.check <- function(sc, item, app, script, tol = TOL) {
  ok <- FALSE
  diff <- NA_real_
  if (is.numeric(app) && is.numeric(script) && length(app) == length(script) && is.null(dim(app))) {
    app <- unname(app); script <- unname(script)
    both_na <- is.na(app) & is.na(script)
    d <- abs(app - script)[!both_na]
    diff <- if (length(d)) max(c(0, d), na.rm = TRUE) else 0
    ok <- !any(xor(is.na(app), is.na(script))) && (length(d) == 0 || all(d <= tol * pmax(1, abs(app[!both_na]))))
  } else {
    r <- all.equal(app, script, tolerance = tol, check.environment = FALSE)
    ok <- isTRUE(r)
    if (!ok) attr(ok, "why") <- paste(utils::head(r, 3), collapse = " | ")
  }
  checks[[length(checks) + 1L]] <<- data.frame(scenario = sc, item = item, ok = isTRUE(ok), max_diff = diff,
                                               n = length(app), stringsAsFactors = FALSE)
  if (!isTRUE(ok)) {
    cat(sprintf("  FAIL %s\n", item))
    if (!is.null(attr(ok, "why"))) cat("       ", attr(ok, "why"), "\n")
    else cat(sprintf("       app: %s\n       script: %s\n", paste(utils::head(format(app), 6), collapse = " "),
                     paste(utils::head(format(script), 6), collapse = " ")))
  }
  invisible(ok)
}

# What a figure is: the data of every layer, as ggplot builds it, and its
# labels. A patchwork (the forest plot) is the list of its plots.
plot_digest <- function(p) {
  if (inherits(p, "patchwork")) return(lapply(seq_len(length(p)), function(i) plot_digest(p[[i]])))
  # ggplot2 >= 4 builds S7 objects; `$` still reads them, `@` is the fallback
  get <- function(x, n) { v <- tryCatch(x[[n]], error = function(e) NULL); if (is.null(v)) methods::slot(x, n) else v }
  b  <- ggplot2::ggplot_build(p)
  d  <- get(b, "data")
  lb <- get(get(b, "plot"), "labels")
  d <- lapply(d, function(x) { x <- as.data.frame(x); rownames(x) <- NULL; x })
  list(data = d, labels = lapply(unclass(lb), function(v) if (is.language(v)) deparse(v) else v))
}
# Values without their plots (the app stores them apart)
no_plots <- function(x) if (is.list(x)) x[setdiff(names(x), c("plots", "measures"))] else x

for (nm in which_sc) {
  cat(sprintf("\n== %s ==\n", nm))
  t0 <- Sys.time()
  input_path <- file.path(out_dir, paste0(nm, "_input.rds"))
  saveRDS(cg_input(cg_scenarios[[nm]]), input_path)
  st <- cg_build_state(cg_scenarios[[nm]])
  cat(sprintf("  app state built (%.1fs)\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

  # ── Export: everything available ──
  opts <- list(data_format = "rds", report_format = "html", session_include_data = FALSE,
               script = list(data_source = "file", data_path = gsub("\\\\", "/", input_path), figures = TRUE))
  it <- export_items(st, "rds", "html")
  stopifnot(identical(it$status[it$id == "reproduce/analysis_script"], "available"))
  sel <- it$id[it$status == "available" & it$kind != "report"]
  job <- export_job(it, sel, st, opts)
  while (!job$done) job <- .export_job_step(job)
  fin <- .export_job_finish(job)
  if (nrow(fin$failed)) print(fin$failed)
  zip_copy <- file.path(out_dir, paste0(nm, ".zip"))
  file.copy(fin$zip, zip_copy, overwrite = TRUE)
  ex_dir <- file.path(out_dir, nm)
  unlink(ex_dir, recursive = TRUE)
  utils::unzip(zip_copy, exdir = ex_dir)
  root <- list.dirs(ex_dir, recursive = FALSE)[1L]
  repro <- file.path(root, "reproduce")
  script_path <- file.path(repro, "analysis_script.R")
  .check(nm, "zip holds analysis_script.R and edark_functions.R", TRUE,
         all(file.exists(file.path(repro, c("analysis_script.R", "edark_functions.R")))))
  .check(nm, "README lists edark_functions.R", TRUE,
         any(grepl("edark_functions.R", readLines(file.path(root, "README.txt")), fixed = TRUE)))
  file.copy(script_path, file.path(out_dir, paste0(nm, ".R")), overwrite = TRUE)
  file.copy(file.path(repro, "edark_functions.R"), file.path(out_dir, paste0(nm, "_functions.R")), overwrite = TRUE)
  ex_data <- readRDS(file.path(root, "data", "working_dataset.rds"))
  ex_res  <- readRDS(file.path(root, "model", "analysis_result.rds"))

  # ── Run the script in a fresh R session, from its folder, EDARK not loaded ──
  t0 <- Sys.time()
  got <- callr::r(function(dir, plot_digest, no_plots) {
    setwd(dir)
    assign("plot_digest", plot_digest, envir = globalenv())   # it calls itself
    env <- new.env(parent = globalenv())
    grDevices::pdf(NULL)   # figures go nowhere
    sys.source("analysis_script.R", envir = env, toplevel.env = env)
    edark_loaded <- "edark" %in% loadedNamespaces()
    keep <- c("working_data", "table1", "univariable", "stepwise_selected", "stepwise_fit", "lasso_selected",
              "lasso_lambda", "collinearity", "model_coefficients", "model_fit_statistics", "diagnostics",
              "performance", "unadjusted", "results_table", "forest_plot", "fit_statistics_table")
    out <- mget(intersect(keep, ls(env)), envir = env)
    # Figures as digests; everything else without its plots or models
    plots <- list()
    for (k in names(out$diagnostics)) {
      for (p in names(out$diagnostics[[k]]$plots)) plots[[paste0("diag/", p)]] <- plot_digest(out$diagnostics[[k]]$plots[[p]])
      out$diagnostics[[k]] <- no_plots(out$diagnostics[[k]])
    }
    for (s in names(out$performance$plots)) for (p in names(out$performance$plots[[s]])) {
      plots[[paste0("perf/", s, "/", p)]] <- plot_digest(out$performance$plots[[s]][[p]])
    }
    if (!is.null(out$performance)) out$performance$plots <- NULL
    if (!is.null(out$forest_plot)) plots[["forest_plot"]] <- plot_digest(out$forest_plot)
    out$forest_plot <- NULL
    if (!is.null(out$unadjusted)) out$unadjusted$models <- NULL
    if (!is.null(out$stepwise_fit)) out$stepwise_fit <- attr(stats::terms(out$stepwise_fit), "term.labels")
    if (!is.null(out$table1)) out$table1 <- lapply(out$table1, function(t) if (!is.null(t)) as.data.frame(gtsummary::as_tibble(t)))
    c(out, list(plots = plots, edark_loaded = edark_loaded))
  }, args = list(dir = repro, plot_digest = plot_digest, no_plots = no_plots), show = FALSE)
  cat(sprintf("  script ran in a fresh R session (%.1fs)\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  .check(nm, "script ran without EDARK", FALSE, got$edark_loaded)

  # ── Compare: data ──
  wd <- got$working_data
  rownames(wd) <- NULL
  ed <- as.data.frame(ex_data)
  rownames(ed) <- NULL
  .check(nm, "working dataset == export", ed, wd)

  # ── Table 1 ──
  rt <- ex_res$result_tables
  for (k in c("overall", "by_exposure", "by_outcome")) {
    a <- rt[[paste0("table1_", k)]]
    if (is.null(a) && is.null(got$table1[[k]])) next
    .check(nm, paste("Table 1", k), if (!is.null(a)) as.data.frame(gtsummary::as_tibble(a)), got$table1[[k]])
  }

  # ── Variable selection ──
  vi <- ex_res$variable_investigation
  if (!is.null(vi$univariable)) {
    u <- as.data.frame(vi$univariable)
    attr(u, "excluded_variables") <- NULL
    g <- got$univariable
    .check(nm, "univariable screen", u[, c("variable", "term", "estimate", "conf.low", "conf.high", "p.value", "suggested")],
           g[, c("variable", "term", "estimate", "conf.low", "conf.high", "p.value", "suggested")])
  }
  if (!is.null(vi$stepwise)) {
    .check(nm, "stepwise: selected", vi$stepwise$selected_variables, got$stepwise_selected)
    .check(nm, "stepwise: final terms", attr(stats::terms(vi$stepwise$final_formula), "term.labels"), got$stepwise_fit)
  }
  if (!is.null(vi$lasso)) {
    .check(nm, "LASSO: selected", vi$lasso$selected_variables, got$lasso_selected)
    .check(nm, "LASSO: lambda", vi$lasso$lambda_selected, got$lasso_lambda)
  }
  cp <- ex_res$result_plots$collinearity_plots
  if (!is.null(cp$flagged_pairs_table)) {
    .check(nm, "collinearity: Pearson r", cp$cor_matrix, got$collinearity$cor_matrix)
    .check(nm, "collinearity: Cramer's V", cp$cramers_v_matrix, got$collinearity$cramers_v_mat)
    .check(nm, "collinearity: flagged pairs", as.data.frame(cp$flagged_pairs_table), as.data.frame(got$collinearity$flagged_pairs))
    if (!is.null(cp$cor_matrix)) .check(nm, "figure: correlation heat map", plot_digest(.plot_correlation_heatmap(cp$cor_matrix)),
                                        plot_digest(.plot_correlation_heatmap(got$collinearity$cor_matrix)))
  }

  # ── Model ──
  co <- ex_res$inference_summary$coefficients
  if (!is.null(co)) {
    .check(nm, "model: coefficient table", co, got$model_coefficients)
    .check(nm, "model: fit statistics", ex_res$inference_summary$fit_statistics, got$model_fit_statistics)
  }

  # ── Diagnostics: every check's values, then every figure ──
  dg <- ex_res$diagnostics
  if (!is.null(dg)) {
    gd <- got$diagnostics
    .check(nm, "diagnostics: sample", dg$sample, gd$sample)
    for (k in intersect(c("residuals", "linearity", "influence", "vif", "separation", "random_effects"), dg$checks)) {
      a <- dg[[k]]
      if (is.null(a)) next   # a note (e.g. no continuous predictor): not stored as values
      .check(nm, paste("diagnostics:", k), no_plots(a), gd[[k]])
    }
    for (p in names(Filter(Negate(is.null), ex_res$result_plots$diagnostic_plots))) {
      .check(nm, paste("figure: diagnostics", p), plot_digest(ex_res$result_plots$diagnostic_plots[[p]]),
             got$plots[[paste0("diag/", p)]])
    }
  }

  # ── Performance: every set, the summary table, messages, every figure ──
  pf <- ex_res$performance
  if (!is.null(pf)) {
    gp <- got$performance
    .check(nm, "performance: validation used", pf$validation, gp$validation)
    .check(nm, "performance: sets", pf$sets, gp$sets)
    .check(nm, "performance: summary table", pf$metrics, gp$metrics)
    .check(nm, "performance: messages", pf$messages, gp$messages)
    pp <- ex_res$result_plots$performance_plots
    for (s in names(pp)) for (p in names(Filter(Negate(is.null), pp[[s]]))) {
      .check(nm, sprintf("figure: performance %s %s", s, p), plot_digest(pp[[s]][[p]]), got$plots[[paste0("perf/", s, "/", p)]])
    }
  }

  # ── Results ──
  mr <- rt$main_results
  if (!is.null(mr)) .check(nm, "results table (with footnotes)", mr, got$results_table)
  if (!is.null(ex_res$results_generation$unadjusted_status)) {
    .check(nm, "results: unadjusted model status", ex_res$results_generation$unadjusted_status, got$unadjusted$status)
  }
  if (!is.null(rt$fit_statistics)) .check(nm, "results: fit statistics table", rt$fit_statistics, got$fit_statistics_table)
  if (!is.null(ex_res$result_plots$coefficient_plot)) {
    .check(nm, "figure: forest plot", plot_digest(ex_res$result_plots$coefficient_plot), got$plots[["forest_plot"]])
  }

  k <- do.call(rbind, checks)
  k <- k[k$scenario == nm, , drop = FALSE]
  cat(sprintf("  %d of %d comparisons match (%d figures)\n", sum(k$ok), nrow(k), sum(grepl("^figure", k$item))))
}

res <- do.call(rbind, checks)
utils::write.csv(res, file.path(out_dir, "results.csv"), row.names = FALSE)
cat(sprintf("\nAll scenarios: %d of %d comparisons match (%d figures). Details: %s\n", sum(res$ok), nrow(res),
            sum(grepl("^figure", res$item)), file.path(out_dir, "results.csv")))
if (!all(res$ok)) quit(status = 1)
