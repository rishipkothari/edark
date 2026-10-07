#' Analysis Model Fitting Service
#'
#' Fitting engines for all four model types: linear regression
#' (\code{stats::lm}), logistic regression (\code{stats::glm}), linear mixed
#' model (\code{lmerTest::lmer}), and logistic mixed model
#' (\code{lme4::glmer}), plus the rules for which model a spec can use.
#' Pure functions — no Shiny. See PRD §4.4 and §7.1\enc{–}{-}7.5.
#'
#' Coefficients, CIs and p-values come from \code{edark_coef_table()}
#' (\code{stats_inference.R}): Wald-type CIs whose critical value matches the
#' model-native p-value — t (lm), t with Satterthwaite df (lmerTest), z (glm,
#' glmer).
#'
#' @importFrom magrittr %>%
#'
#' @name service_analysis_models
NULL


.ANALYSIS_MODEL_LABELS <- c(
  linear         = "Linear regression",
  logistic       = "Logistic regression",
  linear_mixed   = "Linear mixed model",
  logistic_mixed = "Logistic mixed model"
)

.ANALYSIS_OPTIMIZERS <- c("bobyqa", "Nelder_Mead", "nlminbwrap")


#' Classify an outcome column for model selection
#'
#' @param x The outcome column.
#' @return \code{"continuous"} (numeric), \code{"binary"} (factor with exactly
#'   two observed levels) or \code{"unsupported"}.
#' @export
analysis_outcome_type <- function(x) {
  if (is.null(x)) return("unsupported")
  if (is.numeric(x)) return("continuous")
  if (is.factor(x) && nlevels(droplevels(x[!is.na(x)])) == 2L) return("binary")
  "unsupported"
}


#' Which model types a spec can use
#'
#' The outcome type picks the family (linear / logistic); whether cluster
#' variables are assigned picks mixed vs. standard. Exactly one type is
#' available for a supported outcome.
#'
#' @param spec An \code{analysis_spec} list.
#' @param data The frozen analysis dataset.
#'
#' @return A \code{data.frame(model_type, label, available, reason)} with one
#'   row per model type; \code{reason} is \code{""} for available types.
#'   \code{attr(, "recommended")} holds the available type, or \code{NULL}.
#' @export
analysis_model_options <- function(spec, data) {
  roles    <- spec$variable_roles
  outcome  <- roles$outcome_variable
  clusters <- intersect(roles$cluster_variables, names(data))

  kind <- if (is.null(outcome) || is.null(data) || !outcome %in% names(data)) {
    "none"
  } else {
    analysis_outcome_type(data[[outcome]])
  }
  has_clusters <- length(clusters) > 0L

  types  <- names(.ANALYSIS_MODEL_LABELS)
  family <- c(linear = "continuous", logistic = "binary",
              linear_mixed = "continuous", logistic_mixed = "binary")
  mixed  <- c(linear = FALSE, logistic = FALSE, linear_mixed = TRUE, logistic_mixed = TRUE)

  reason <- vapply(types, function(t) {
    if (kind == "none") return("Assign an outcome variable in Step 1")
    if (kind == "unsupported") return("Outcome must be numeric or a two-level factor")
    if (family[[t]] != kind) {
      return(if (kind == "continuous") "Outcome is continuous" else "Outcome is binary")
    }
    if (mixed[[t]] && !has_clusters) return("No cluster variable assigned in Step 1")
    if (!mixed[[t]] && has_clusters) return("Cluster variables assigned; use a mixed model")
    ""
  }, character(1))

  out <- data.frame(
    model_type = types,
    label      = unname(.ANALYSIS_MODEL_LABELS[types]),
    available  = reason == "",
    reason     = unname(reason),
    stringsAsFactors = FALSE
  )
  rec <- out$model_type[out$available]
  attr(out, "recommended") <- if (length(rec) == 1L) rec else NULL
  out
}


#' Which outcome level a logistic model treats as the event
#'
#' \code{glm}/\code{glmer} model the probability of the second factor level;
#' the first level is the reference. With the spec's reference level applied,
#' the event is the other level.
#'
#' @param spec An \code{analysis_spec} list.
#' @param data The frozen analysis dataset.
#' @return \code{list(variable, event, reference)} for a binary outcome,
#'   else \code{NULL}.
#' @export
analysis_outcome_event <- function(spec, data) {
  outcome <- spec$variable_roles$outcome_variable
  if (is.null(outcome) || is.null(data) || !outcome %in% names(data)) return(NULL)
  y <- data[[outcome]]
  if (analysis_outcome_type(y) != "binary") return(NULL)
  lv  <- levels(droplevels(y[!is.na(y)]))
  ref <- spec$variable_roles$reference_levels[[outcome]]
  if (is.null(ref) || !ref %in% lv) ref <- lv[1L]
  list(variable = outcome, event = setdiff(lv, ref), reference = ref)
}


# The parts of the spec a fitted model depends on.
.model_inputs <- function(spec) {
  vr    <- spec$variable_roles
  vars  <- c(vr$outcome_variable, vr$exposure_variable, vr$final_model_covariates)
  refs  <- vr$reference_levels[intersect(sort(names(vr$reference_levels)), vars)]
  list(outcome    = vr$outcome_variable,
       exposure   = vr$exposure_variable,
       covariates = sort(as.character(vr$final_model_covariates)),
       clusters   = sort(as.character(vr$cluster_variables)),
       references = refs,
       model_type = spec$model_design$model_type,
       optimizer  = spec$model_design$optimizer,
       split      = analysis_split(spec))
}

#' Has the spec changed since the model was fitted?
#'
#' @param spec The current \code{analysis_spec}.
#' @param result The \code{analysis_result}; its \code{specification_snapshot}
#'   is the spec the model was fitted with.
#' @return \code{TRUE} when a fitted model exists and any model input
#'   (roles, covariates, reference levels, model type, optimizer, train/test
#'   split) differs.
#' @export
analysis_fit_is_stale <- function(spec, result) {
  if (is.null(result$fitted_models$primary_model)) return(FALSE)
  snap <- result$specification_snapshot
  if (is.null(snap)) return(TRUE)
  !identical(.model_inputs(spec), .model_inputs(snap))
}


#' Is each Analyze output available, stale or not run?
#'
#' One answer per output group, for the Export page (PRD/BUILD_Export.md §3)
#' and anything else that must know whether an output can be used. Stale
#' outputs cannot be exported.
#'
#' Diagnostics, Performance and Results are cleared on every refit
#' (\code{reset_analysis_pipeline(from_step = 4)}), so for them stale only ever
#' means the spec moved since the fit (\code{analysis_fit_is_stale()}).
#' Performance is also stale when its validation settings changed after the
#' run. When Prepare has changed since Analyze › Setup froze the dataset,
#' every group is stale.
#'
#' @param spec The current \code{analysis_spec} (may be \code{NULL}).
#' @param result The \code{analysis_result} (may be \code{NULL}).
#' @param prepare_changed Logical. The working dataset no longer matches the
#'   frozen one (Setup's signature check).
#' @return A named list - \code{table1}, \code{univariable}, \code{selection}
#'   (stepwise or LASSO), \code{collinearity}, \code{model}, \code{results},
#'   \code{diagnostics}, \code{performance} - each \code{list(status, reason)}
#'   with \code{status} one of \code{"available"}, \code{"stale"},
#'   \code{"not_run"}.
#' @export
analysis_output_status <- function(spec, result, prepare_changed = FALSE) {
  .st <- function(status, reason = "") list(status = status, reason = reason)
  groups <- c("table1", "univariable", "selection", "collinearity",
              "model", "results", "diagnostics", "performance")

  if (is.null(spec)) {
    out <- lapply(groups, function(g) .st("not_run", "Start in Analyze \u203a Setup"))
    return(stats::setNames(out, groups))
  }
  if (isTRUE(prepare_changed)) {
    out <- lapply(groups, function(g) .st("stale", "Prepare changed since Analyze \u203a Setup froze the dataset"))
    return(stats::setNames(out, groups))
  }

  vi        <- result$variable_investigation
  has_model <- !is.null(result$fitted_models$primary_model) &&
               identical(result$run_status$status, "success")
  fit_stale <- has_model && analysis_fit_is_stale(spec, result)
  refit     <- "The spec changed since the model was fitted - refit in Model \u203a Create"

  # Model-derived groups: not run, stale with the fit, else available
  .model_group <- function(present, where) {
    if (!has_model)     return(.st("not_run", "Fit a model in Model \u203a Create"))
    if (!isTRUE(present)) return(.st("not_run", paste("Run in", where)))
    if (fit_stale)      return(.st("stale", refit))
    .st("available")
  }

  perf <- .model_group(!is.null(result$performance), "Model \u203a Performance")
  if (identical(perf$status, "available") &&
      !.validation_matches(result$performance$validation, spec,
                           result$specification_snapshot$model_design$model_type)) {
    perf <- .st("stale", "Validation settings changed since the run - re-run in Model \u203a Performance")
  }

  list(
    table1 = if (is.null(result$result_tables$table1_overall))
      .st("not_run", "Run in Analyze \u203a Table 1") else .st("available"),
    univariable = if (is.null(vi$univariable))
      .st("not_run", "Run the univariable screen in Analyze \u203a Variables") else .st("available"),
    selection = if (is.null(vi$stepwise) && is.null(vi$lasso))
      .st("not_run", "Run stepwise or LASSO in Analyze \u203a Variables") else .st("available"),
    collinearity = if (is.null(result$result_plots$collinearity_plots$flagged_pairs_table))
      .st("not_run", "Open Collinearity in Analyze \u203a Variables") else .st("available"),
    model = if (!has_model) .st("not_run", "Fit a model in Model \u203a Create")
            else if (fit_stale) .st("stale", refit) else .st("available"),
    results     = .model_group(!is.null(result$results_generation), "Model \u203a Results"),
    diagnostics = .model_group(!is.null(result$diagnostics), "Model \u203a Diagnostics"),
    performance = perf
  )
}

# Were these validation settings (from a performance run) made with what the
# spec asks for now? Only the settings the method uses are compared, so a
# change to the CV folds does not stale a bootstrap run.
.validation_matches <- function(used, spec, model_type) {
  if (is.null(used)) return(TRUE)
  now <- analysis_validation(spec, mixed = isTRUE(model_type %in% c("linear_mixed", "logistic_mixed")))
  if (!identical(used$method, now$method)) return(FALSE)
  keys <- switch(now$method,
                 cv        = c("cv_folds", "cv_repeats", "seed"),
                 bootstrap = c("bootstrap_reps", "seed"),
                 character(0))
  all(vapply(keys, function(k) identical(as.integer(used[[k]]), as.integer(now[[k]])), logical(1)))
}


#' Fit the analysis model described by a spec
#'
#' Builds the model data (the training rows when a train/test split applies —
#' \code{analysis_model_data()}; complete cases over the model variables,
#' clusters included only for mixed models; ordered factors converted to
#' unordered so every factor uses treatment contrasts; reference levels
#' applied; unused levels dropped; cluster variables converted to factors —
#' \code{.prepare_model_rows()}), fits the model
#' named by \code{spec$model_design$model_type}, and extracts coefficients,
#' fit statistics and fitted values. Warnings and messages raised while
#' fitting are captured, never thrown.
#'
#' @param spec An \code{analysis_spec} list.
#' @param data The frozen analysis dataset.
#'
#' @return A named list: \code{status} (\code{"success"} / \code{"failed"}),
#'   \code{error}, \code{model_type}, \code{model} (the fitted object),
#'   \code{formula}, \code{coefficients} (data.frame: variable, term, level,
#'   estimate, std.error, statistic, p.value, conf.low, conf.high, effect,
#'   effect.low, effect.high, effect_measure), \code{fit_statistics}
#'   (data.frame: key, label, value, format), \code{predicted_values},
#'   \code{n_total} (rows available to the model: the training set when split),
#'   \code{n_used}, \code{outcome_event} (binary outcomes:
#'   list(variable, event, reference)), \code{reference_levels} (factor
#'   predictors actually used), and \code{messages}
#'   (data.frame: level, stage, message).
#' @export
fit_analysis_model <- function(spec, data) {
  split <- analysis_split_rows(spec, data)
  data  <- analysis_model_data(spec, data)
  roles      <- spec$variable_roles
  model_type <- spec$model_design$model_type
  outcome    <- roles$outcome_variable
  exposure   <- roles$exposure_variable
  preds      <- .safe_preds(exposure, roles$final_model_covariates)
  is_mixed   <- isTRUE(model_type %in% c("linear_mixed", "logistic_mixed"))
  is_logit   <- isTRUE(model_type %in% c("logistic", "logistic_mixed"))
  clusters   <- if (is_mixed) intersect(roles$cluster_variables, names(data)) else character(0)
  optimizer  <- spec$model_design$optimizer
  if (is.null(optimizer) || !optimizer %in% .ANALYSIS_OPTIMIZERS) optimizer <- "bobyqa"

  msgs <- data.frame(level = character(0), stage = character(0),
                     message = character(0), stringsAsFactors = FALSE)
  .msg <- function(level, stage, text) {
    msgs[nrow(msgs) + 1L, ] <<- list(level, stage, text)
  }
  .fail <- function(err) {
    .msg("error", "fit", err)
    list(status = "failed", error = err, model_type = model_type, model = NULL,
         formula = NULL, coefficients = NULL, fit_statistics = NULL,
         predicted_values = NULL, n_total = nrow(data), n_used = NA_integer_,
         outcome_event = NULL, reference_levels = list(), messages = msgs)
  }

  if (is.null(model_type) || !model_type %in% names(.ANALYSIS_MODEL_LABELS)) {
    return(.fail("No model type selected."))
  }
  if (is.null(outcome) || !outcome %in% names(data)) {
    return(.fail("No outcome variable assigned."))
  }

  # ── Model data ────────────────────────────────────────────────────────────
  vars    <- unique(c(outcome, preds, clusters))
  missing <- setdiff(vars, names(data))
  if (length(missing) > 0L) {
    return(.fail(paste("Not in the analysis dataset:", paste(missing, collapse = ", "))))
  }
  if (!is.null(split) && nrow(data) == 0L) {
    return(.fail(sprintf("No rows have the training level '%s' of '%s'.",
                         split$training_level, split$variable)))
  }
  cc <- .prepare_model_rows(data, outcome, preds, clusters, roles$reference_levels)
  n_total <- nrow(data)
  n_used  <- nrow(cc)
  if (n_used == 0L) return(.fail("No complete cases remain."))

  outcome_event <- NULL
  if (is_logit) {
    y <- cc[[outcome]]
    if (!is.factor(y) || nlevels(y) != 2L) {
      return(.fail(sprintf("Logistic models need a two-level factor outcome; '%s' is not.", outcome)))
    }
    outcome_event <- list(variable = outcome, event = levels(y)[2L], reference = levels(y)[1L])
  } else if (!is.numeric(cc[[outcome]])) {
    return(.fail(sprintf("Linear models need a numeric outcome; '%s' is not.", outcome)))
  }

  if (!is.null(split)) {
    .msg("note", "data", sprintf(
      "Fitted on the training set (%s = %s): %d rows. %d test rows are held out for Performance.",
      split$variable, split$training_level, sum(split$training), sum(split$test)))
  }
  .msg("note", "data", sprintf("%d of %d %srows used (%d excluded for missing data).",
                               n_used, n_total, if (!is.null(split)) "training " else "",
                               n_total - n_used))

  fmla <- build_analysis_formula(spec)

  # ── Fit ───────────────────────────────────────────────────────────────────
  ft    <- .fit_engine(model_type, fmla, cc, optimizer)
  model <- ft$model
  warns <- ft$warnings
  notes <- ft$messages
  if (is.null(model)) return(.fail(paste("Model fitting failed:", ft$error)))

  for (w in unique(warns)) .msg("warning", "fit", .explain_fit_warning(w))
  singular <- is_mixed && isTRUE(lme4::isSingular(model))
  notes <- notes[!grepl("singular", notes, ignore.case = TRUE)]
  for (n in unique(notes)) .msg("note", "fit", n)
  if (singular) {
    .msg("warning", "fit", paste(
      "Singular fit: a random-effect variance is estimated at or near zero.",
      "The random-effects structure may be too complex for the data -",
      "consider removing a cluster variable."))
  }

  # ── Extract ───────────────────────────────────────────────────────────────
  coefs <- tryCatch(edark_coef_table(model, cc), error = function(e) {
    .msg("warning", "extract", paste("Coefficient table failed:", conditionMessage(e)))
    NULL
  })
  stats_tbl <- tryCatch(.fit_statistics(model, model_type, cc, outcome, clusters),
                        error = function(e) {
    .msg("warning", "extract", paste("Fit statistics failed:", conditionMessage(e)))
    NULL
  })
  predicted <- tryCatch({
    pv <- data.frame(
      .edark_row_id = if (".edark_row_id" %in% names(cc)) cc$.edark_row_id else seq_len(n_used),
      .fitted       = as.numeric(stats::fitted(model)),
      .resid        = as.numeric(stats::residuals(model, type = "response"))
    )
    if (model_type == "logistic_mixed") {
      pv$.fitted_marginal <- as.numeric(stats::predict(model, type = "response", re.form = NA))
    }
    pv
  }, error = function(e) NULL)

  ref_levels <- list()
  for (v in preds) if (is.factor(cc[[v]])) ref_levels[[v]] <- levels(cc[[v]])[1L]

  list(
    status           = "success",
    error            = NULL,
    model_type       = model_type,
    model            = model,
    formula          = fmla,
    coefficients     = coefs,
    fit_statistics   = stats_tbl,
    predicted_values = predicted,
    n_total          = n_total,
    n_used           = n_used,
    outcome_event    = outcome_event,
    reference_levels = ref_levels,
    messages         = msgs
  )
}


# Fit one model with the engine for its type, capturing warnings and messages
# instead of throwing them. Used by the primary fit, the unadjusted models and
# the Performance resamples. `satterthwaite = FALSE` fits a linear mixed model
# with lme4::lmer — same estimates, faster, for refits that need no p-values.
# Returns list(model, warnings, messages, error); model is NULL on failure.
.fit_engine <- function(model_type, fmla, data, optimizer = "bobyqa", satterthwaite = TRUE) {
  warns <- character(0)
  notes <- character(0)
  err   <- NULL
  model <- withCallingHandlers(
    tryCatch(
      switch(model_type,
        linear   = stats::lm(fmla, data = data),
        logistic = stats::glm(fmla, data = data, family = stats::binomial()),
        linear_mixed = if (satterthwaite) {
          lmerTest::lmer(fmla, data = data, control = lme4::lmerControl(optimizer = optimizer))
        } else {
          lme4::lmer(fmla, data = data, control = lme4::lmerControl(optimizer = optimizer))
        },
        logistic_mixed = lme4::glmer(
          fmla, data = data, family = stats::binomial(),
          control = lme4::glmerControl(optimizer = optimizer))
      ),
      error = function(e) { err <<- conditionMessage(e); NULL }
    ),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      notes <<- c(notes, trimws(conditionMessage(m)))
      invokeRestart("muffleMessage")
    }
  )
  list(model = model, warnings = warns, messages = notes, error = err)
}


# Model rows from a data set: complete cases over the model variables (the
# row id is carried along), ordered factors made unordered (e.g. Auto-factor /
# cut-points would otherwise get polynomial contrasts; clinical tables expect
# each level against a reference), reference levels applied, unused levels
# dropped, clusters made factors. Used for the fit and, in Performance, for the
# test set, so both are prepared the same way.
.prepare_model_rows <- function(data, outcome, preds, clusters, reference_levels) {
  vars <- unique(c(outcome, preds, clusters))
  keep <- intersect(c(vars, ".edark_row_id"), names(data))
  cc   <- compute_complete_cases(data[, keep, drop = FALSE], vars)$data
  for (v in c(outcome, preds)) {
    if (is.ordered(cc[[v]])) cc[[v]] <- factor(cc[[v]], levels = levels(cc[[v]]), ordered = FALSE)
  }
  cc <- apply_reference_levels(cc, reference_levels)
  for (v in c(outcome, preds)) {
    if (is.factor(cc[[v]])) cc[[v]] <- droplevels(cc[[v]])
  }
  for (cl in clusters) cc[[cl]] <- factor(cc[[cl]])
  cc
}


# Add a plain-language hint to the fitting warnings users most often see.
.explain_fit_warning <- function(w) {
  if (grepl("converge", w, ignore.case = TRUE)) {
    return(paste0(w, " - The estimates may not be reliable. Try a different ",
                  "optimizer under Advanced; if the estimates agree, the warning can ",
                  "usually be ignored."))
  }
  if (grepl("fitted probabilities numerically 0 or 1", w, fixed = TRUE)) {
    return(paste0(w, " - Usually (quasi-)separation: a predictor perfectly ",
                  "predicts the outcome for some rows. Check sparse factor levels."))
  }
  if (grepl("Rescale variables", w, fixed = TRUE)) {
    return(paste0(w, " - A predictor is on a much larger scale than the others ",
                  "(e.g. mL next to units). Estimates are usually fine; rescaling it ",
                  "in Prepare (e.g. mL \u2192 L, or Standardize) removes the warning."))
  }
  if (grepl("algorithm did not converge", w, fixed = TRUE)) {
    return(paste0(w, " - Often caused by separation or very sparse data."))
  }
  w
}


# Fit statistics per model type (PRD §7.2–7.5) as a long table so the UI and
# export can show them without knowing the model type.
.fit_statistics <- function(model, model_type, data, outcome, clusters) {
  rows <- list()
  .add <- function(key, label, value, format = "number") {
    if (is.null(value) || length(value) == 0L) return()
    value <- suppressWarnings(as.numeric(value[1L]))
    if (is.na(value)) return()
    rows[[length(rows) + 1L]] <<- data.frame(key = key, label = label, value = value,
                                             format = format, stringsAsFactors = FALSE)
  }
  .quiet <- function(expr) suppressWarnings(suppressMessages(tryCatch(expr, error = function(e) NULL)))

  n <- stats::nobs(model)
  .add("n_obs", "Observations", n, "integer")

  if (model_type %in% c("linear_mixed", "logistic_mixed")) {
    ng <- lme4::ngrps(model)
    for (cl in names(ng)) .add(paste0("n_groups_", cl), sprintf("Clusters (%s)", cl), ng[[cl]], "integer")
  }

  if (model_type %in% c("logistic", "logistic_mixed")) {
    y <- data[[outcome]]
    n_events <- sum(y == levels(y)[2L])
    .add("n_events", sprintf("Events (%s = %s)", outcome, levels(y)[2L]), n_events, "integer")
    .add("event_rate", "Event rate", n_events / n, "percent")
  }

  if (model_type == "linear") {
    s <- summary(model)
    .add("r2", "R\u00b2", s$r.squared)
    .add("adj_r2", "Adjusted R\u00b2", s$adj.r.squared)
    .add("rmse", "Residual SE", stats::sigma(model))
    if (!is.null(s$fstatistic)) {
      f <- s$fstatistic
      .add("f_stat", "F statistic", f[["value"]])
      .add("f_p", "F-test p-value",
           stats::pf(f[["value"]], f[["numdf"]], f[["dendf"]], lower.tail = FALSE), "pvalue")
    }
  }

  if (model_type == "logistic") {
    dev  <- model$deviance
    ndev <- model$null.deviance
    .add("r2_mcfadden", "Pseudo R\u00b2 (McFadden)", 1 - dev / ndev)
    cs   <- 1 - exp((dev - ndev) / n)
    .add("r2_nagelkerke", "Pseudo R\u00b2 (Nagelkerke)", cs / (1 - exp(-ndev / n)))
  }

  if (model_type %in% c("linear_mixed", "logistic_mixed")) {
    # Both return a bare NA, not a list, when a variance component is ~0
    # (singular fit) - `$` on it would fail the whole table (\u00a7N6.8)
    r2 <- .quiet(performance::r2_nakagawa(model))
    if (is.list(r2)) {
      .add("r2_marginal", "Marginal R\u00b2", r2$R2_marginal)
      .add("r2_conditional", "Conditional R\u00b2", r2$R2_conditional)
    }
    icc <- .quiet(performance::icc(model))
    if (is.list(icc)) .add("icc", "ICC (adjusted)", icc$ICC_adjusted)

    vc <- as.data.frame(lme4::VarCorr(model))
    for (i in seq_len(nrow(vc))) {
      if (vc$grp[i] == "Residual") {
        .add("sd_residual", "Residual SD", vc$sdcor[i])
      } else {
        .add(paste0("sd_", vc$grp[i]), sprintf("Random intercept SD (%s)", vc$grp[i]), vc$sdcor[i])
      }
    }
  }

  .add("aic", "AIC", stats::AIC(model))
  .add("bic", "BIC", stats::BIC(model))
  .add("loglik", "Log-likelihood", as.numeric(stats::logLik(model)))

  do.call(rbind, rows)
}


#' Fit the unadjusted (one-variable) models for Model › Results
#'
#' One model per predictor of the fitted primary model (exposure first, then
#' covariates), each containing that predictor alone. Every model is fitted
#' to the primary model's own rows (its model frame), so both columns of the
#' results table share one n, and uses the same engine: mixed models keep the
#' same random intercepts and optimizer. Estimates come from
#' \code{edark_coef_table()}, like every other model in the app.
#'
#' @param result The \code{analysis_result} holding \code{primary_model} and
#'   its \code{specification_snapshot}.
#' @param progress_fn Optional \code{function(fraction, detail)}.
#'
#' @return \code{list(coefficients, status, models)}: \code{coefficients} is
#'   the stacked \code{edark_coef_table()} output (intercepts removed);
#'   \code{status} is a data.frame(variable, status, message) with status
#'   \code{"ok"}, \code{"warning"} (fitted, with a convergence / singular-fit
#'   or other warning) or \code{"failed"}; \code{models} is a named list of
#'   the fitted models.
#' @export
fit_unadjusted_models <- function(result, progress_fn = NULL) {
  model <- result$fitted_models$primary_model
  if (is.null(model)) stop("No fitted model.")
  spec       <- result$specification_snapshot
  roles      <- spec$variable_roles
  model_type <- spec$model_design$model_type
  outcome    <- roles$outcome_variable
  preds      <- .safe_preds(roles$exposure_variable, roles$final_model_covariates)
  is_mixed   <- model_type %in% c("linear_mixed", "logistic_mixed")
  clusters   <- if (is_mixed) roles$cluster_variables else character(0)
  optimizer  <- spec$model_design$optimizer
  if (is.null(optimizer) || !optimizer %in% .ANALYSIS_OPTIMIZERS) optimizer <- "bobyqa"

  mf <- stats::model.frame(model)
  coefs  <- list()
  status <- data.frame(variable = character(0), status = character(0),
                       message = character(0), stringsAsFactors = FALSE)
  models <- list()

  for (i in seq_along(preds)) {
    v <- preds[i]
    if (is.function(progress_fn)) {
      progress_fn(0.1 + 0.8 * (i - 1) / length(preds), sprintf("Unadjusted model: %s\u2026", v))
    }
    dat <- mf[, intersect(c(outcome, v, clusters), names(mf)), drop = FALSE]
    if (is.factor(dat[[v]])) dat[[v]] <- droplevels(dat[[v]])
    rhs  <- paste(c(v, if (is_mixed) paste0("(1 | ", clusters, ")")), collapse = " + ")
    fmla <- stats::as.formula(paste(outcome, "~", rhs))

    ft    <- .fit_engine(model_type, fmla, dat, optimizer)
    fit   <- ft$model
    warns <- ft$warnings
    err   <- ft$error

    if (is.null(fit)) {
      status[nrow(status) + 1L, ] <- list(v, "failed", .short_fit_warning(err))
      next
    }
    if (is_mixed && isTRUE(lme4::isSingular(fit))) {
      warns <- c(warns, "Singular fit: a random-effect variance is estimated at or near zero.")
    }
    ct <- tryCatch(edark_coef_table(fit, dat), error = function(e) { err <<- conditionMessage(e); NULL })
    if (is.null(ct)) {
      status[nrow(status) + 1L, ] <- list(v, "failed", .short_fit_warning(err))
      next
    }
    coefs[[v]]  <- ct[ct$variable != "(Intercept)", , drop = FALSE]
    models[[v]] <- fit
    warns <- unique(vapply(warns, .short_fit_warning, character(1)))
    status[nrow(status) + 1L, ] <- if (length(warns) > 0L) {
      list(v, "warning", paste(warns, collapse = "; "))
    } else {
      list(v, "ok", NA_character_)
    }
  }

  list(
    coefficients = if (length(coefs) > 0L) do.call(rbind, unname(coefs)) else NULL,
    status       = status,
    models       = models
  )
}


# One short phrase per fitting warning, for table footnotes.
.short_fit_warning <- function(w) {
  if (is.null(w) || is.na(w)) return("unknown problem")
  w <- gsub("\\s+", " ", w)
  if (grepl("failed to converge|did not converge", w, ignore.case = TRUE)) return("did not converge")
  if (grepl("singular", w, ignore.case = TRUE)) return("singular fit (a random-effect variance near zero)")
  if (grepl("unidentifiable|Rescale variables", w)) return("a predictor on a very large scale (consider rescaling)")
  if (grepl("fitted probabilities numerically 0 or 1", w, fixed = TRUE)) return("possible separation")
  if (nchar(w) > 120) paste0(substr(w, 1, 117), "...") else w
}
