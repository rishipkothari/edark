# End-to-end check of the generated R script (PRD §A7.9).
#
# For each scenario in scenarios.R:
#   1. build the app state with the service functions the modules call;
#   2. build the export zip (every available item, including
#      reproduce/analysis_script.R) and unzip it;
#   3. run the exported script in a fresh R process, from the input dataset;
#   4. compare what the script computed with what the export holds -
#      data/working_dataset.rds and model/analysis_result.rds.
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
  app <- unname(app); script <- unname(script)
  ok <- FALSE
  diff <- NA_real_
  if (is.numeric(app) && is.numeric(script) && length(app) == length(script)) {
    both_na <- is.na(app) & is.na(script)
    d <- abs(app - script)[!both_na]
    diff <- if (length(d)) max(c(0, d), na.rm = TRUE) else 0
    ok <- !any(xor(is.na(app), is.na(script))) && (length(d) == 0 || all(d <= tol * pmax(1, abs(app[!both_na]))))
  } else {
    ok <- identical(app, script)
  }
  checks[[length(checks) + 1L]] <<- data.frame(scenario = sc, item = item, ok = ok, max_diff = diff,
                                               n = length(app), stringsAsFactors = FALSE)
  if (!ok) {
    cat(sprintf("  FAIL %-45s app: %s\n       %-45s script: %s\n", item,
                paste(utils::head(format(app), 6), collapse = " "), "",
                paste(utils::head(format(script), 6), collapse = " ")))
  }
  invisible(ok)
}

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
  script_path <- file.path(root, "reproduce", "analysis_script.R")
  stopifnot(file.exists(script_path))
  file.copy(script_path, file.path(out_dir, paste0(nm, ".R")), overwrite = TRUE)
  ex_data <- readRDS(file.path(root, "data", "working_dataset.rds"))
  ex_res  <- readRDS(file.path(root, "model", "analysis_result.rds"))

  # ── Run the script in a fresh R session ──
  t0 <- Sys.time()
  got <- callr::r(function(path) {
    env <- new.env(parent = globalenv())
    grDevices::pdf(NULL)   # figures go nowhere
    sys.source(path, envir = env, toplevel.env = env)
    keep <- c("working_data", "table1_overall", "table1_by_exposure", "table1_by_outcome",
              "univariable", "stepwise_selected", "stepwise_fit", "lasso_selected", "lasso_lambda",
              "cor_matrix", "cramers_v_matrix", "model_coefficients", "model_fit_statistics",
              "diagnostics", "vif", "random_effects", "apparent", "test", "cv", "bootstrap",
              "unadjusted", "results_table")
    out <- mget(intersect(keep, ls(env)), envir = env)
    if (!is.null(out$apparent)) out$apparent$roc <- NULL
    if (!is.null(out$test)) out$test$roc <- NULL
    if (!is.null(out$stepwise_fit)) out$stepwise_fit <- attr(stats::terms(out$stepwise_fit), "term.labels")
    out
  }, args = list(path = script_path), wd = root, show = FALSE)
  cat(sprintf("  script ran in a fresh R session (%.1fs)\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

  # ── Compare ──
  wd <- got$working_data
  rownames(wd) <- NULL
  ed <- as.data.frame(ex_data)
  rownames(ed) <- NULL
  .check(nm, "working dataset (identical)", TRUE, isTRUE(all.equal(ed, wd, check.attributes = TRUE)))
  .check(nm, "working dataset == app", TRUE, isTRUE(all.equal({ x <- st$dataset_working; rownames(x) <- NULL; x }, wd)))

  rt <- ex_res$result_tables
  for (k in c("overall", "by_exposure", "by_outcome")) {
    a <- rt[[paste0("table1_", k)]]
    s <- got[[paste0("table1_", k)]]
    if (is.null(a) && is.null(s)) next
    .check(nm, paste("Table 1", k), TRUE,
           !is.null(a) && !is.null(s) && identical(as.data.frame(gtsummary::as_tibble(a)), as.data.frame(gtsummary::as_tibble(s))))
  }

  vi <- ex_res$variable_investigation
  if (!is.null(vi$univariable)) {
    u <- vi$univariable
    g <- got$univariable
    .check(nm, "univariable: variable/term", paste(u$variable, u$term), paste(g$variable, g$term))
    for (col in c("estimate", "conf.low", "conf.high", "p.value")) .check(nm, paste("univariable:", col), u[[col]], g[[col]])
    .check(nm, "univariable: suggested", u$suggested, g$suggested)
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
  if (!is.null(cp$cor_matrix)) .check(nm, "collinearity: Pearson r", as.vector(cp$cor_matrix), as.vector(got$cor_matrix))
  if (!is.null(cp$cramers_v_matrix)) .check(nm, "collinearity: Cramer's V", as.vector(cp$cramers_v_matrix), as.vector(got$cramers_v_matrix))

  co <- ex_res$inference_summary$coefficients
  if (!is.null(co)) {
    g <- got$model_coefficients
    .check(nm, "model: terms", co$term, g$term)
    .check(nm, "model: levels", co$level, g$level)
    for (col in c("estimate", "std.error", "statistic", "df", "p.value", "conf.low", "conf.high", "effect", "effect.low", "effect.high")) {
      .check(nm, paste("model:", col), co[[col]], g[[col]])
    }
    .check(nm, "model: fit statistics", ex_res$inference_summary$fit_statistics$value, got$model_fit_statistics$value)
  }

  dg <- ex_res$diagnostics
  if (!is.null(dg) && !is.null(got$diagnostics)) {
    m <- dg$metrics
    .mv <- function(key) m$value[m$key == key]
    gd <- got$diagnostics
    if ("binned_inside" %in% m$key) .check(nm, "diagnostics: binned residuals inside", .mv("binned_inside"), gd$binned_inside)
    if ("bp_stat" %in% m$key) {
      .check(nm, "diagnostics: Breusch-Pagan statistic", .mv("bp_stat"), unname(gd$breusch_pagan$statistic))
      .check(nm, "diagnostics: Breusch-Pagan p", .mv("bp_p"), gd$breusch_pagan$p.value)
    }
    lin <- m[startsWith(m$key, "lin_"), , drop = FALSE]
    if (nrow(lin)) .check(nm, "diagnostics: linearity inside", lin$value, as.numeric(gd$linearity_inside[sub("^lin_", "", lin$key)]))
    if ("cooks_max" %in% m$key) {
      .check(nm, "diagnostics: Cook's max", .mv("cooks_max"), gd$cooks_max)
      .check(nm, "diagnostics: Cook's cut-off", .mv("cooks_threshold"), gd$cooks_threshold)
      .check(nm, "diagnostics: rows above cut-off", .mv("n_above"), gd$n_above)
    }
    if (is.data.frame(dg$vif)) .check(nm, "diagnostics: VIF", dg$vif$vif, as.numeric(got$vif$VIF))
    if (!is.null(dg$separation)) .check(nm, "diagnostics: separation", dg$separation$detected, gd$separation)
    if (!is.null(dg$random_effects)) {
      comp <- dg$random_effects$components
      .check(nm, "diagnostics: random-effect ICC", comp$icc, got$random_effects$icc)
      .check(nm, "diagnostics: random-effect SD", comp$sd, got$random_effects$sd)
    }
  }

  pf <- ex_res$performance
  if (!is.null(pf)) {
    keys <- c("n", "n_events", "auc", "auc_low", "auc_high", "brier", "brier_null", "cal_intercept", "cal_slope", "rmse", "mae", "r2")
    for (s in intersect(c("apparent", "test"), names(pf$sets))) {
      a <- pf$sets[[s]]
      g <- got[[s]]
      for (k in keys) if (!is.null(a[[k]])) .check(nm, sprintf("performance %s: %s", s, k), a[[k]], g[[k]])
      if (!is.null(a$calibration)) .check(nm, sprintf("performance %s: calibration bins", s),
                                          unlist(a$calibration), unlist(g$calibration))
    }
    if (!is.null(pf$sets$cv)) {
      a <- pf$sets$cv
      for (k in names(got$cv$mean)) {
        .check(nm, sprintf("performance cv: %s (mean)", k), a[[k]], got$cv$mean[[k]])
        if (!is.null(a$sd[[k]])) .check(nm, sprintf("performance cv: %s (sd)", k), a$sd[[k]], got$cv$sd[[k]])
      }
      .check(nm, "performance cv: rows", a$n, got$cv$n)
    }
    if (!is.null(pf$sets$bootstrap)) {
      op <- pf$sets$bootstrap$optimism
      b  <- got$bootstrap
      .check(nm, "performance bootstrap: measures", op$key, b$measure)
      for (col in c("apparent", "optimism", "corrected")) .check(nm, paste("performance bootstrap:", col), op[[col]], b[[col]])
    }
  }

  mr <- ex_res$result_tables$main_results
  if (!is.null(mr) && isTRUE(attr(mr, "include_unadjusted"))) {
    rows <- mr[mr$row_type %in% c("level", "continuous"), , drop = FALSE]
    u <- got$unadjusted
    key_a <- paste(rows$variable, ifelse(is.na(rows$level), "", rows$level))
    key_s <- paste(u$variable, ifelse(is.na(u$level), "", u$level))
    .check(nm, "results: unadjusted estimates", rows$unadj_est, u$effect[match(key_a, key_s)])
    .check(nm, "results: unadjusted p", rows$unadj_p, u$p.value[match(key_a, key_s)])
    .check(nm, "results: adjusted estimates", rows$adj_est,
           got$results_table$effect.adjusted[match(key_a, paste(got$results_table$variable,
                                                              ifelse(is.na(got$results_table$level), "", got$results_table$level)))])
  }
  k <- do.call(rbind, checks)
  k <- k[k$scenario == nm, , drop = FALSE]
  cat(sprintf("  %d of %d comparisons match\n", sum(k$ok), nrow(k)))
}

res <- do.call(rbind, checks)
utils::write.csv(res, file.path(out_dir, "results.csv"), row.names = FALSE)
cat(sprintf("\nAll scenarios: %d of %d comparisons match. Details: %s\n", sum(res$ok), nrow(res),
            file.path(out_dir, "results.csv")))
if (!all(res$ok)) quit(status = 1)
