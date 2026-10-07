#' Analysis Performance Service
#'
#' Predictive performance of the Step 5 model, for Model › Performance (PRD §A5.3 Step 7).
#' Pure functions — no Shiny. \code{analysis_performance_options()} lists the
#' measures a model type offers; \code{run_analysis_performance()} computes
#' the selected ones on each set of rows that applies:
#' \itemize{
#'   \item \strong{Apparent} — the rows the model was fitted to. Always
#'     computed, and always optimistic.
#'   \item \strong{Test} — validation method \code{"split"}: the held-out rows
#'     (Step 1). Prepared exactly like the model rows
#'     (\code{.prepare_model_rows()}); rows whose factor levels the model never
#'     saw cannot be predicted and are dropped with a note.
#'   \item \strong{Cross-validated} — method \code{"cv"}: k-fold, repeated,
#'     stratified by outcome (logistic) or grouped by the first cluster
#'     variable (mixed). Each fold's model predicts the rows it left out;
#'     measures are computed on the pooled out-of-fold predictions of each
#'     repeat and averaged over repeats (SD across repeats).
#'   \item \strong{Bootstrap-corrected} — method \code{"bootstrap"}: Harrell's
#'     optimism correction. Each resample's model is scored on the resample
#'     (apparent) and on the original rows (test); the mean difference is the
#'     optimism, subtracted from the apparent value. Mixed models resample
#'     whole clusters of the first cluster variable.
#' }
#' Resampling refits the model with the \strong{same covariates} — the Step 4
#' selection is not repeated in each resample (by design: EDARK discourages
#' automated selection). Mixed models use marginal predictions
#' (\code{re.form = NA}): the fixed effects alone, as for a new patient from an
#' unseen cluster.
#'
#' The work is split into steps (\code{analysis_performance_job()},
#' \code{.perf_job_step()}, \code{.perf_job_finish()}) so the app can run it
#' a few resamples at a time and stay responsive to Cancel;
#' \code{run_analysis_performance()} runs every step in one call.
#'
#' @importFrom magrittr %>%
#'
#' @name service_analysis_performance
NULL


.PERF_CAL_BINS <- 10L
.PERF_CURVE_POINTS <- 50L
# Bootstrap resamples: default, default for mixed models (each refit is slow),
# and the number of mixed-model refits above which the app asks first.
.PERF_BOOT_DEFAULT       <- 200L
.PERF_BOOT_MIXED_DEFAULT <- 100L
.PERF_MIXED_FIT_WARN     <- 100L

.PERF_SET_LABELS <- c(apparent = "Apparent (model rows)", test = "Test set",
                      cv = "Cross-validated", bootstrap = "Bootstrap-corrected")
# Short form for plot titles, e.g. "ROC curve (test set)"
.PERF_SET_SHORT  <- c(apparent = "apparent", test = "test set",
                      cv = "cross-validated", bootstrap = "bootstrap-corrected")

.PERF_METHOD_LABELS <- c(none = "None (apparent performance only)", split = "Held-out test set",
                         cv = "Cross-validation", bootstrap = "Bootstrap optimism correction")

# Every scalar measure: key, label (logistic, linear), display format
.PERF_METRIC_DEFS <- list(
  list("auc", "AUC", "AUC", "number"),
  list("brier", "Brier score", "Brier score", "number"),
  list("brier_null", "Brier score, no-information", "Brier score, no-information", "number"),
  list("cal_intercept", "Calibration intercept (log-odds)", "Calibration-in-the-large (mean residual)", "number"),
  list("cal_slope", "Calibration slope", "Calibration slope", "number"),
  list("rmse", "RMSE", "RMSE", "number"),
  list("mae", "MAE", "MAE", "number"),
  list("r2", "R\u00b2 (predictions)", "R\u00b2 (predictions)", "number")
)
.PERF_SCORE_KEYS <- c("auc", "brier", "cal_intercept", "cal_slope", "rmse", "mae", "r2")

.perf_metric_label <- function(key, logit) {
  d <- Filter(function(d) d[[1L]] == key, .PERF_METRIC_DEFS)
  if (length(d) == 0L) key else d[[1L]][[if (logit) 2L else 3L]]
}


#' Performance measures available for a model type
#'
#' @param model_type One of \code{"linear"}, \code{"logistic"},
#'   \code{"linear_mixed"}, \code{"logistic_mixed"}.
#' @return A \code{data.frame(id, label, description)}, or \code{NULL}.
#' @export
analysis_performance_options <- function(model_type) {
  if (is.null(model_type)) return(NULL)
  logit <- model_type %in% c("logistic", "logistic_mixed")

  rows <- if (logit) {
    list(
      list("discrimination", "Discrimination (ROC / AUC)",
           "How well predicted risks separate events from non-events"),
      list("calibration", "Calibration",
           "Predicted vs observed risk, calibration slope, and the Brier score"),
      list("predicted_distribution", "Predicted probabilities",
           "Distribution of predicted risk by outcome group")
    )
  } else {
    list(
      list("prediction_error", "Prediction error",
           "RMSE, MAE and R\u00b2 of the predictions"),
      list("calibration", "Calibration", "Observed vs predicted values and calibration slope")
    )
  }
  data.frame(
    id          = vapply(rows, `[[`, character(1), 1L),
    label       = vapply(rows, `[[`, character(1), 2L),
    description = vapply(rows, `[[`, character(1), 3L),
    stringsAsFactors = FALSE
  )
}


#' Run the performance evaluation
#'
#' @param result The \code{analysis_result} holding a fitted
#'   \code{primary_model} and its \code{specification_snapshot}.
#' @param data The frozen analysis dataset (all rows; the test set is taken
#'   from it with the snapshot's train/test split).
#' @param checks Character vector of ids from
#'   \code{analysis_performance_options()}. Unknown ids are ignored.
#' @param validation Output of \code{analysis_validation()}; \code{NULL} reads
#'   it from the snapshot.
#' @param progress_fn Optional \code{function(fraction, detail)}.
#'
#' @return A named list: \code{run_at}, \code{model_type}, \code{checks},
#'   \code{basis} (\code{"marginal"} for mixed models, else \code{"fixed"}),
#'   \code{validation} (method and the settings actually used), \code{split}
#'   (\code{NULL}, or list(variable, training_level, test_levels)),
#'   \code{sets} (named list — \code{apparent} and, per method, \code{test},
#'   \code{cv} or \code{bootstrap} — each list(label, n, n_events, auc,
#'   auc_low, auc_high, brier, brier_null, calibration, cal_intercept,
#'   cal_slope, rmse, mae, r2); \code{cv} adds \code{sd} (named list),
#'   \code{bootstrap} adds \code{optimism} (data.frame: key, label, apparent,
#'   optimism, corrected); both add \code{n_fits}, \code{n_failed}; absent
#'   measures are \code{NULL}), \code{metrics} (long data.frame: set, key,
#'   label, value, sd, format), \code{messages} (data.frame: level, message)
#'   and \code{plots} (named list per set of ggplot objects).
#' @export
run_analysis_performance <- function(result, data, checks, validation = NULL, progress_fn = NULL) {
  job <- analysis_performance_job(result, data, checks, validation)
  while (!job$done) {
    job <- .perf_job_step(job)
    if (is.function(progress_fn)) progress_fn(.perf_job_progress(job), .perf_job_detail(job))
  }
  .perf_job_finish(job)
}


#' Prepare a performance evaluation to be run step by step
#'
#' Computes the apparent (and test set) performance at once and draws every
#' resample up front with the seed, so the result does not depend on how the
#' steps are batched. Advance with \code{.perf_job_step()} until
#' \code{job$done}, then \code{.perf_job_finish()}.
#'
#' @inheritParams run_analysis_performance
#' @return A job list; \code{n_steps} is the number of model refits.
#' @export
analysis_performance_job <- function(result, data, checks, validation = NULL) {
  model <- result$fitted_models$primary_model
  if (is.null(model)) stop("No fitted model to evaluate.")
  spec       <- result$specification_snapshot
  model_type <- spec$model_design$model_type
  outcome    <- spec$variable_roles$outcome_variable
  logit      <- model_type %in% c("logistic", "logistic_mixed")
  mixed      <- model_type %in% c("linear_mixed", "logistic_mixed")
  checks     <- intersect(checks, analysis_performance_options(model_type)$id)
  if (is.null(validation)) validation <- analysis_validation(spec, mixed)
  vr        <- spec$variable_roles
  optimizer <- spec$model_design$optimizer
  if (is.null(optimizer) || !optimizer %in% .ANALYSIS_OPTIMIZERS) optimizer <- "bobyqa"

  msgs <- data.frame(level = character(0), message = character(0), stringsAsFactors = FALSE)
  .msg <- function(level, text) msgs[nrow(msgs) + 1L, ] <<- list(level, text)

  mf <- stats::model.frame(model)
  job <- list(
    run_at = Sys.time(), model_type = model_type, checks = checks,
    basis = if (mixed) "marginal" else "fixed", validation = validation, split = NULL,
    sets = list(), plots = list(),
    outcome = outcome, logit = logit, mixed = mixed, optimizer = optimizer,
    preds = .safe_preds(vr$exposure_variable, vr$final_model_covariates),
    clusters = if (mixed) intersect(vr$cluster_variables, names(mf)) else character(0),
    formula = build_analysis_formula(spec), mf = mf,
    step = 0L, n_steps = 0L, done = TRUE,
    n_failed = 0L, n_warned = 0L, fail_reasons = character(0), warn_reasons = character(0)
  )

  # ── Apparent: the model's own rows ────────────────────────────────────────
  job$pred_apparent <- .perf_predict(model, NULL, mixed)
  app <- .perf_measures(job$pred_apparent, mf[[outcome]], logit, checks, outcome, "apparent",
                        with_slope = FALSE)
  job$sets$apparent  <- app$values
  job$plots$apparent <- app$plots

  # ── Test: the held-out rows of a train/test split ─────────────────────────
  sr <- if (identical(validation$method, "split")) analysis_split_rows(spec, data)
  if (!is.null(sr)) {
    job$split <- list(variable = sr$variable, training_level = sr$training_level,
                      test_levels = sr$test_levels)
    tst <- .perf_test_rows(model, spec, data[sr$test, , drop = FALSE], outcome, logit, mixed, .msg)
    if (!is.null(tst)) {
      pred_t <- tryCatch(.perf_predict(model, tst, mixed), error = function(e) {
        .msg("warning", paste("Test set predictions failed:", conditionMessage(e)))
        NULL
      })
      if (!is.null(pred_t)) {
        te <- .perf_measures(pred_t, tst[[outcome]], logit, checks, outcome, "test", with_slope = TRUE)
        job$sets$test  <- te$values
        job$plots$test <- te$plots
      }
    }
  } else if (identical(validation$method, "split")) {
    .msg("warning", "Held-out test set chosen in Step 1, but no variable and training level are set.")
  }

  # ── Resampling plan: every resample drawn now, with the seed ──────────────
  if (validation$method %in% c("cv", "bootstrap")) {
    plan <- .with_seed(validation$seed, if (validation$method == "cv") {
      .perf_cv_plan(mf, validation$cv_folds, validation$cv_repeats, outcome, logit, job$clusters, .msg)
    } else {
      .perf_boot_plan(mf, validation$bootstrap_reps, job$clusters)
    })
    if (!is.null(plan)) {
      job$plan    <- plan
      job$n_steps <- if (validation$method == "cv") nrow(plan$steps) else length(plan)
      job$done    <- job$n_steps == 0L
      if (validation$method == "cv") {
        job$validation$cv_folds <- plan$k
        job$oof <- matrix(NA_real_, nrow(mf), plan$repeats)
      } else {
        job$boot_app  <- matrix(NA_real_, job$n_steps, length(.PERF_SCORE_KEYS),
                                dimnames = list(NULL, .PERF_SCORE_KEYS))
        job$boot_test <- job$boot_app
        job$grid <- .perf_curve_grid(job$pred_apparent)
        job$curve_app  <- matrix(NA_real_, job$n_steps, length(job$grid))
        job$curve_test <- job$curve_app
      }
      if (mixed && length(vr$cluster_variables) > 1L) {
        .msg("note", sprintf("Resampling is grouped by %s, the first cluster variable.", job$clusters[1L]))
      }
    }
  }

  job$msgs <- msgs
  job
}


# Run the next resample (one model refit). Failures are counted, never thrown.
.perf_job_step <- function(job) {
  if (job$done) return(job)
  i <- job$step + 1L
  job <- if (job$validation$method == "cv") .perf_cv_step(job, i) else .perf_boot_step(job, i)
  job$step <- i
  job$done <- i >= job$n_steps
  job
}

.perf_job_progress <- function(job) {
  if (job$n_steps == 0L) return(0.95)
  0.05 + 0.9 * job$step / job$n_steps
}

.perf_job_detail <- function(job) {
  if (job$n_steps == 0L) return("Finishing\u2026")
  if (job$validation$method == "cv") {
    s <- job$plan$steps[min(job$step + 1L, job$n_steps), ]
    sprintf("Cross-validation: repeat %d of %d, fold %d of %d", s$rep, job$plan$repeats, s$fold, job$plan$k)
  } else {
    sprintf("Bootstrap resample %d of %d", min(job$step + 1L, job$n_steps), job$n_steps)
  }
}


# Summarise the resamples into the final performance result.
.perf_job_finish <- function(job) {
  msgs <- job$msgs
  .msg <- function(level, text) msgs[nrow(msgs) + 1L, ] <<- list(level, text)

  if (job$validation$method %in% c("cv", "bootstrap") && job$n_steps > 0L) {
    what <- if (job$validation$method == "cv") "fold" else "resample"
    if (job$n_failed > 0L) {
      top <- utils::head(names(sort(table(job$fail_reasons), decreasing = TRUE)), 3L)
      .msg("warning", sprintf("%d of %d %s models could not be fitted or used and were skipped (%s).",
                              job$n_failed, job$n_steps, what, paste(top, collapse = "; ")))
    }
    if (job$n_warned > 0L) {
      top <- utils::head(names(sort(table(job$warn_reasons), decreasing = TRUE)), 3L)
      .msg("note", sprintf("%d of %d %s models gave fitting warnings (%s).",
                           job$n_warned, job$n_steps, what, paste(top, collapse = "; ")))
    }
    res <- if (job$validation$method == "cv") .perf_cv_summary(job, .msg) else .perf_boot_summary(job, .msg)
    if (!is.null(res)) {
      job$sets[[job$validation$method]]  <- res$values
      job$plots[[job$validation$method]] <- res$plots
    }
  }

  list(
    run_at     = job$run_at,
    model_type = job$model_type,
    checks     = job$checks,
    basis      = job$basis,
    validation = job$validation,
    split      = job$split,
    sets       = job$sets,
    metrics    = .perf_metrics_table(job$sets, job$logit),
    messages   = msgs,
    plots      = job$plots
  )
}


# ── Resampling ───────────────────────────────────────────────────────────────

# Resamples are drawn inside .with_seed() (stats_inference.R), which leaves the
# session's random stream as it was.

# Fold assignment for each repeat. Mixed: whole clusters of the first cluster
# variable go to a fold (the left-out rows then come from unseen clusters, as
# in use). Logistic: stratified by outcome so every fold has events.
.perf_cv_plan <- function(mf, k, repeats, outcome, logit, clusters, msg) {
  n <- nrow(mf)
  if (length(clusters) > 0L) {
    g   <- as.character(mf[[clusters[1L]]])
    ids <- unique(g)
    if (length(ids) < 2L) {
      msg("warning", sprintf("Cross-validation needs at least two values of %s.", clusters[1L]))
      return(NULL)
    }
    if (length(ids) < k) {
      msg("note", sprintf("%d folds requested but %s has %d clusters: using %d folds (leave-one-cluster-out).",
                          k, clusters[1L], length(ids), length(ids)))
      k <- length(ids)
    }
    assign_folds <- function() sample(rep_len(seq_len(k), length(ids)))[match(g, ids)]
  } else if (logit) {
    y <- mf[[outcome]]
    min_class <- min(table(y))
    if (min_class < k) {
      k2 <- max(2L, min_class)
      msg("note", sprintf("%d folds requested but the rarer outcome class has only %d rows: using %d folds.",
                          k, min_class, k2))
      k <- k2
    }
    assign_folds <- function() {
      f <- integer(n)
      for (lv in levels(y)) {
        i <- which(y == lv)
        f[i] <- sample(rep_len(seq_len(k), length(i)))
      }
      f
    }
  } else {
    k <- min(k, n)
    assign_folds <- function() sample(rep_len(seq_len(k), n))
  }
  folds <- lapply(seq_len(repeats), function(r) assign_folds())
  list(k = k, repeats = repeats, folds = folds,
       steps = expand.grid(fold = seq_len(k), rep = seq_len(repeats)))
}


# Bootstrap resamples: rows with replacement, or (mixed) whole clusters of the
# first cluster variable. `copy` numbers each drawn cluster so a cluster drawn
# twice becomes two clusters.
.perf_boot_plan <- function(mf, B, clusters) {
  if (length(clusters) > 0L) {
    g       <- as.character(mf[[clusters[1L]]])
    ids     <- unique(g)
    rows_by <- split(seq_len(nrow(mf)), factor(g, levels = ids))
    lapply(seq_len(B), function(b) {
      draw <- sample(ids, length(ids), replace = TRUE)
      list(rows = unlist(rows_by[draw], use.names = FALSE),
           copy = rep(seq_along(draw), lengths(rows_by[draw])))
    })
  } else {
    lapply(seq_len(B), function(b) list(rows = sample.int(nrow(mf), replace = TRUE), copy = NULL))
  }
}


# Refit the model to a set of rows. Factor levels absent from the rows are
# dropped first (an empty level would give an inestimable coefficient).
.perf_refit <- function(job, d) {
  for (v in c(job$outcome, job$preds)) if (is.factor(d[[v]])) d[[v]] <- droplevels(d[[v]])
  if (job$logit && nlevels(d[[job$outcome]]) < 2L) {
    return(list(model = NULL, error = "only one outcome class in the rows"))
  }
  for (cl in job$clusters) d[[cl]] <- factor(d[[cl]])
  ft <- .fit_engine(job$model_type, job$formula, d, job$optimizer, satterthwaite = FALSE)
  if (is.null(ft$model)) ft$error <- .short_fit_warning(ft$error)
  ft$data <- d
  ft
}

# Rows of `eval` the refit can predict: every factor level seen when fitting.
.perf_predictable <- function(fit_data, eval, preds) {
  ok <- rep(TRUE, nrow(eval))
  for (v in preds) {
    if (is.factor(fit_data[[v]])) ok <- ok & as.character(eval[[v]]) %in% levels(fit_data[[v]])
  }
  ok
}

# Factor predictors of `eval` re-levelled to the refit's levels
.perf_align <- function(fit_data, eval, preds) {
  for (v in preds) {
    if (is.factor(fit_data[[v]])) eval[[v]] <- factor(as.character(eval[[v]]), levels = levels(fit_data[[v]]))
  }
  eval
}

.perf_note_fit <- function(job, ft) {
  w <- ft$warnings
  if (job$mixed && !is.null(ft$model) && isTRUE(lme4::isSingular(ft$model))) w <- c(w, "singular fit")
  if (length(w) > 0L) {
    job$n_warned     <- job$n_warned + 1L
    job$warn_reasons <- c(job$warn_reasons, unique(vapply(w, .short_fit_warning, character(1))))
  }
  job
}

.perf_fail <- function(job, reason) {
  job$n_failed     <- job$n_failed + 1L
  job$fail_reasons <- c(job$fail_reasons, reason %||% "unknown problem")
  job
}


.perf_cv_step <- function(job, i) {
  s    <- job$plan$steps[i, ]
  fold <- job$plan$folds[[s$rep]]
  out  <- fold == s$fold
  ft   <- .perf_refit(job, job$mf[!out, , drop = FALSE])
  if (is.null(ft$model)) return(.perf_fail(job, ft$error))
  job  <- .perf_note_fit(job, ft)

  test <- job$mf[out, , drop = FALSE]
  ok   <- .perf_predictable(ft$data, test, job$preds)
  if (!any(ok)) return(.perf_fail(job, "no left-out rows could be predicted"))
  p <- tryCatch(.perf_predict(ft$model, .perf_align(ft$data, test[ok, , drop = FALSE], job$preds), job$mixed),
                error = function(e) NULL)
  if (is.null(p)) return(.perf_fail(job, "prediction failed"))
  job$oof[which(out)[ok], s$rep] <- p
  job
}


.perf_boot_step <- function(job, b) {
  pl <- job$plan[[b]]
  d  <- job$mf[pl$rows, , drop = FALSE]
  if (!is.null(pl$copy)) {
    for (cl in job$clusters) d[[cl]] <- paste(as.character(d[[cl]]), pl$copy, sep = "#")
  }
  ft <- .perf_refit(job, d)
  if (is.null(ft$model)) return(.perf_fail(job, ft$error))
  job <- .perf_note_fit(job, ft)

  # The resample model is scored on the original rows; a level it never saw
  # makes that impossible, so the whole resample is skipped.
  if (!all(.perf_predictable(ft$data, job$mf, job$preds))) {
    return(.perf_fail(job, "a factor level was missing from the resample"))
  }
  p_boot <- tryCatch(.perf_predict(ft$model, NULL, job$mixed), error = function(e) NULL)
  p_orig <- tryCatch(.perf_predict(ft$model, .perf_align(ft$data, job$mf, job$preds), job$mixed),
                     error = function(e) NULL)
  if (is.null(p_boot) || is.null(p_orig)) return(.perf_fail(job, "prediction failed"))

  y_boot <- ft$data[[job$outcome]]
  y_orig <- job$mf[[job$outcome]]
  job$boot_app[b, ]   <- .perf_scores(p_boot, y_boot, job$logit)
  job$boot_test[b, ]  <- .perf_scores(p_orig, y_orig, job$logit)
  job$curve_app[b, ]  <- .perf_smooth(p_boot, .perf_y_numeric(y_boot, job$logit), job$grid)
  job$curve_test[b, ] <- .perf_smooth(p_orig, .perf_y_numeric(y_orig, job$logit), job$grid)
  job
}


.perf_cv_summary <- function(job, msg) {
  oof  <- job$oof
  y    <- job$mf[[job$outcome]]
  keys <- .perf_keys(job$checks, job$logit)
  per_rep <- t(vapply(seq_len(ncol(oof)), function(r) .perf_scores(oof[, r], y, job$logit),
                      numeric(length(.PERF_SCORE_KEYS))))
  n_pred <- colSums(is.finite(oof))
  if (all(n_pred == 0L)) {
    msg("warning", "Cross-validation produced no predictions.")
    return(NULL)
  }
  if (any(n_pred < nrow(oof))) {
    msg("note", sprintf("%d of %d rows could not be predicted in at least one repeat (a factor level missing from the training folds).",
                        sum(rowSums(!is.finite(oof)) > 0L), nrow(oof)))
  }

  v <- list(label = .PERF_SET_LABELS[["cv"]], n = as.integer(round(mean(n_pred))), n_events = NULL,
            n_fits = job$n_steps, n_failed = job$n_failed, sd = list())
  if (job$logit) v$n_events <- sum(as.integer(y == levels(y)[2L]))
  for (k in keys) {
    v[[k]] <- mean(per_rep[, k], na.rm = TRUE)
    if (nrow(per_rep) > 1L) v$sd[[k]] <- stats::sd(per_rep[, k], na.rm = TRUE)
  }
  if (job$logit && "calibration" %in% job$checks) {
    yi <- as.integer(y == levels(y)[2L])
    v$brier_null <- mean(yi) * (1 - mean(yi))
  }

  # Plots from every out-of-fold prediction, all repeats pooled
  ok     <- is.finite(oof)
  pooled <- .perf_measures(oof[ok], rep(y, ncol(oof))[ok], job$logit, job$checks, job$outcome, "cv",
                           with_slope = FALSE, roc_subtitle = "Out-of-fold predictions, all repeats pooled")
  v$calibration <- pooled$values$calibration
  list(values = v, plots = pooled$plots)
}


.perf_boot_summary <- function(job, msg) {
  ok <- stats::complete.cases(job$boot_app[, "brier"]) | stats::complete.cases(job$boot_app[, "rmse"])
  n_ok <- sum(ok)
  if (n_ok == 0L) {
    msg("warning", "Every bootstrap resample failed, so optimism could not be estimated.")
    return(NULL)
  }
  if (n_ok < 50L) {
    msg("warning", sprintf("Only %d usable bootstrap resamples: the optimism estimate is imprecise.", n_ok))
  }
  y    <- job$mf[[job$outcome]]
  keys <- .perf_keys(job$checks, job$logit)
  app  <- .perf_scores(job$pred_apparent, y, job$logit)
  opt  <- colMeans(job$boot_app[ok, , drop = FALSE] - job$boot_test[ok, , drop = FALSE], na.rm = TRUE)
  cor  <- app - opt

  v <- list(label = .PERF_SET_LABELS[["bootstrap"]], n = nrow(job$mf), n_events = NULL,
            n_fits = job$n_steps, n_failed = job$n_failed)
  if (job$logit) v$n_events <- sum(as.integer(y == levels(y)[2L]))
  for (k in keys) if (is.finite(cor[[k]])) v[[k]] <- unname(cor[[k]])
  if (job$logit && "calibration" %in% job$checks) {
    yi <- as.integer(y == levels(y)[2L])
    v$brier_null <- mean(yi) * (1 - mean(yi))
  }
  keys <- keys[is.finite(cor[keys])]
  v$optimism <- data.frame(
    key       = keys,
    label     = vapply(keys, .perf_metric_label, character(1), logit = job$logit),
    apparent  = unname(app[keys]),
    optimism  = unname(opt[keys]),
    corrected = unname(cor[keys]),
    stringsAsFactors = FALSE
  )

  plots <- list()
  if ("calibration" %in% job$checks) {
    app_curve <- .perf_smooth(job$pred_apparent, .perf_y_numeric(y, job$logit), job$grid)
    opt_curve <- colMeans(job$curve_app[ok, , drop = FALSE] - job$curve_test[ok, , drop = FALSE], na.rm = TRUE)
    curve <- data.frame(predicted = job$grid, apparent = app_curve, corrected = app_curve - opt_curve)
    if (job$logit) curve$corrected <- pmin(pmax(curve$corrected, 0), 1)
    v$calibration_curve <- curve
    if (any(is.finite(curve$corrected))) {
      plots$calibration_curve <- .plot_calibration_curve(curve, job$pred_apparent, job$logit, job$outcome,
                                                         .PERF_SET_SHORT[["bootstrap"]])
    }
  }
  list(values = v, plots = plots)
}


# ── Scoring ──────────────────────────────────────────────────────────────────

# Which measure keys the selected checks produce
.perf_keys <- function(checks, logit) {
  unique(c(
    if ("discrimination" %in% checks) "auc",
    if ("calibration" %in% checks) c(if (logit) "brier", "cal_intercept", "cal_slope"),
    if ("prediction_error" %in% checks) c("rmse", "mae", "r2")
  ))
}

.perf_y_numeric <- function(y, logit) {
  if (logit) as.integer(y == levels(y)[2L]) else as.numeric(y)
}

# Every scalar measure for one vector of predictions (NA where it does not
# apply). Fast: used once per resample.
.perf_scores <- function(pred, y, logit) {
  out <- stats::setNames(rep(NA_real_, length(.PERF_SCORE_KEYS)), .PERF_SCORE_KEYS)
  ok  <- is.finite(pred)
  pred <- pred[ok]
  y    <- y[ok]
  if (length(pred) < 3L) return(out)
  .try <- function(expr) tryCatch(suppressWarnings(expr), error = function(e) NA_real_)
  if (logit) {
    yi <- .perf_y_numeric(y, TRUE)
    out[["brier"]] <- mean((pred - yi)^2)
    if (length(unique(yi)) == 2L) {
      out[["auc"]] <- .fast_auc(pred, yi)
      lp <- stats::qlogis(pmin(pmax(pred, 1e-8), 1 - 1e-8))
      out[["cal_intercept"]] <- .try(unname(stats::coef(stats::glm(yi ~ 1, offset = lp, family = stats::binomial()))[1L]))
      out[["cal_slope"]]     <- .try(unname(stats::coef(stats::glm(yi ~ lp, family = stats::binomial()))[2L]))
    }
  } else {
    y <- as.numeric(y)
    out[["rmse"]] <- sqrt(mean((y - pred)^2))
    out[["mae"]]  <- mean(abs(y - pred))
    out[["r2"]]   <- 1 - sum((y - pred)^2) / sum((y - mean(y))^2)
    out[["cal_intercept"]] <- mean(y - pred)
    if (length(unique(pred)) > 1L) out[["cal_slope"]] <- .try(unname(stats::coef(stats::lm(y ~ pred))[2L]))
  }
  out
}

# AUC as the Mann-Whitney statistic (same value as pROC, without the ROC object)
.fast_auc <- function(pred, yi) {
  n1 <- sum(yi == 1L)
  n0 <- sum(yi == 0L)
  (sum(rank(pred)[yi == 1L]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# Points at which calibration curves are compared: the central 98% of the
# apparent predictions.
.perf_curve_grid <- function(pred) {
  r <- stats::quantile(pred, c(0.01, 0.99), na.rm = TRUE, names = FALSE)
  if (!all(is.finite(r)) || r[1L] == r[2L]) return(numeric(0))
  seq(r[1L], r[2L], length.out = .PERF_CURVE_POINTS)
}

# Smoothed observed-vs-predicted curve (lowess, as rms::calibrate) at `grid`
.perf_smooth <- function(pred, y, grid) {
  na <- rep(NA_real_, length(grid))
  ok <- is.finite(pred) & is.finite(y)
  if (length(grid) == 0L || sum(ok) < 10L || length(unique(pred[ok])) < 5L) return(na)
  sm <- tryCatch(stats::lowess(pred[ok], y[ok], iter = 0L), error = function(e) NULL)
  if (is.null(sm)) return(na)
  stats::approx(sm$x, sm$y, xout = grid, ties = mean, rule = 2)$y
}


# ── Helpers ──────────────────────────────────────────────────────────────────

# Response-scale predictions; mixed models use the fixed effects only.
.perf_predict <- function(model, newdata, mixed) {
  args <- list(model, type = "response")
  if (!is.null(newdata)) args$newdata <- newdata
  if (mixed) args$re.form <- NA
  as.numeric(do.call(stats::predict, args))
}


# Test rows prepared like the model rows, restricted to factor levels the
# model knows. Returns NULL (with a message) when nothing is left.
.perf_test_rows <- function(model, spec, test, outcome, logit, mixed, msg) {
  vr    <- spec$variable_roles
  preds <- .safe_preds(vr$exposure_variable, vr$final_model_covariates)
  n_raw <- nrow(test)
  if (n_raw == 0L) {
    msg("warning", "The test set is empty.")
    return(NULL)
  }
  # Marginal predictions do not use the cluster columns, so a test row
  # missing its cluster ID can still be predicted.
  tst <- .prepare_model_rows(test, outcome, preds, character(0), vr$reference_levels)
  n_missing <- n_raw - nrow(tst)
  if (n_missing > 0L) {
    msg("note", sprintf("%d of %d test rows excluded for missing data in a model variable.",
                        n_missing, n_raw))
  }

  mf <- stats::model.frame(model)
  unseen <- rep(FALSE, nrow(tst))
  for (v in c(outcome, preds)) {
    if (!is.factor(mf[[v]])) next
    known <- levels(mf[[v]])
    bad <- !as.character(tst[[v]]) %in% known
    if (any(bad)) {
      msg("warning", sprintf("%d test row%s excluded: '%s' level%s not seen when fitting (%s).",
                             sum(bad), if (sum(bad) == 1L) "" else "s", v,
                             if (length(unique(tst[[v]][bad])) == 1L) "" else "s",
                             paste(unique(as.character(tst[[v]][bad])), collapse = ", ")))
    }
    unseen <- unseen | bad
  }
  tst <- tst[!unseen, , drop = FALSE]
  if (nrow(tst) == 0L) {
    msg("warning", "No test rows can be predicted.")
    return(NULL)
  }
  # Same levels, in the same order, as the model
  for (v in c(outcome, preds)) {
    if (is.factor(mf[[v]])) tst[[v]] <- factor(as.character(tst[[v]]), levels = levels(mf[[v]]))
  }
  if (logit && length(unique(tst[[outcome]])) < 2L) {
    msg("warning", "The test set has only one outcome class, so discrimination and calibration cannot be assessed.")
  }
  tst
}


# Every selected measure for one set of rows, with its plots. `y` is the
# outcome as in the model frame (a two-level factor for logistic models).
.perf_measures <- function(pred, y, logit, checks, outcome, set, with_slope, roc_subtitle = NULL) {
  short <- .PERF_SET_SHORT[[set]]
  v <- list(label = .PERF_SET_LABELS[[set]], n = length(pred), n_events = NULL)
  plots <- list()
  .try <- function(expr) tryCatch(expr, error = function(e) NULL)

  if (logit) {
    yi <- as.integer(y == levels(y)[2L])
    v$n_events <- sum(yi)
    both <- length(unique(yi)) == 2L
    if ("discrimination" %in% checks && both) {
      roc <- .try(pROC::roc(yi, pred, levels = c(0L, 1L), direction = "<", quiet = TRUE))
      if (!is.null(roc)) {
        ci <- as.numeric(pROC::ci.auc(roc))
        v$auc      <- as.numeric(pROC::auc(roc))
        v$auc_low  <- ci[1L]
        v$auc_high <- ci[3L]
        plots$roc_curve <- .plot_roc(roc, v$auc, ci, short, subtitle = if (!is.null(roc_subtitle)) {
          sprintf("%s (AUC %.3f)", roc_subtitle, v$auc)
        })
      }
    }
    if ("calibration" %in% checks) {
      v$brier      <- mean((pred - yi)^2)
      v$brier_null <- mean(yi) * (1 - mean(yi))   # predicting the prevalence for everyone
      bins <- .calibration_bins(pred, yi)
      v$calibration <- bins
      plots$calibration_plot <- .plot_calibration_logistic(bins, short)
      if (with_slope && both) {
        # In-sample these are 0 and 1 by construction, so only new rows get them
        lp <- stats::qlogis(pmin(pmax(pred, 1e-8), 1 - 1e-8))
        v$cal_intercept <- .try(unname(stats::coef(stats::glm(yi ~ 1, offset = lp, family = stats::binomial()))[1L]))
        v$cal_slope     <- .try(unname(stats::coef(stats::glm(yi ~ lp, family = stats::binomial()))[2L]))
      }
    }
    if ("predicted_distribution" %in% checks) {
      plots$predicted_probs <- .plot_predicted_probs(pred, y, short)
    }
  } else {
    y <- as.numeric(y)
    if ("prediction_error" %in% checks) {
      v$rmse <- sqrt(mean((y - pred)^2))
      v$mae  <- mean(abs(y - pred))
      v$r2   <- 1 - sum((y - pred)^2) / sum((y - mean(y))^2)
    }
    if ("calibration" %in% checks) {
      plots$calibration_plot <- .plot_observed_predicted(pred, y, outcome, short)
      if (with_slope && length(unique(pred)) > 1L) {
        v$cal_intercept <- mean(y - pred)
        v$cal_slope     <- .try(unname(stats::coef(stats::lm(y ~ pred))[2L]))
      }
    }
  }
  list(values = v, plots = plots)
}


# Long table of every computed value, one row per set and measure. `sd` is
# the spread across cross-validation repeats (NA elsewhere).
.perf_metrics_table <- function(sets, logit) {
  defs <- c(
    list(list("n", "Rows", "integer")),
    if (logit) list(list("n_events", "Events", "integer")),
    list(list("auc", "AUC", "number"),
         list("auc_low", "AUC 95% CI (lower)", "number"),
         list("auc_high", "AUC 95% CI (upper)", "number")),
    lapply(Filter(function(d) d[[1L]] != "auc", .PERF_METRIC_DEFS), function(d) {
      list(d[[1L]], d[[if (logit) 2L else 3L]], d[[4L]])
    })
  )
  rows <- list()
  for (s in names(sets)) {
    for (d in defs) {
      val <- sets[[s]][[d[[1L]]]]
      if (is.null(val) || length(val) != 1L || !is.finite(val)) next
      sd <- sets[[s]]$sd[[d[[1L]]]]
      rows[[length(rows) + 1L]] <- data.frame(set = s, key = d[[1L]], label = d[[2L]],
                                              value = as.numeric(val),
                                              sd = if (is.null(sd)) NA_real_ else as.numeric(sd),
                                              format = d[[3L]], stringsAsFactors = FALSE)
    }
  }
  if (length(rows) == 0L) return(NULL)
  do.call(rbind, rows)
}


# Decile bins of predicted risk: mean predicted vs observed proportion, with a
# Wilson 95% interval for the observed proportion.
.calibration_bins <- function(pred, y) {
  k   <- min(.PERF_CAL_BINS, length(unique(pred)))
  bin <- dplyr::ntile(pred, k)
  out <- do.call(rbind, lapply(sort(unique(bin)), function(b) {
    i <- bin == b
    n <- sum(i)
    obs <- mean(y[i])
    z <- stats::qnorm(0.975)
    centre <- (obs + z^2 / (2 * n)) / (1 + z^2 / n)
    half   <- z * sqrt(obs * (1 - obs) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
    data.frame(bin = b, n = n, predicted = mean(pred[i]), observed = obs,
               low = max(0, centre - half), high = min(1, centre + half))
  }))
  out
}
