# Helper functions copied into the generated analysis script
# (R/service_analysis_codegen.R, PRD §A7.9).
#
# Each chunk starts with a "# @chunk <name>" line and runs to the next one. The
# generator copies only the chunks a script needs, in the order it lists them.
# Every helper mirrors the EDARK function named in its comment line by line -
# change one and change the other, or the script stops reproducing the app
# (§N6.14). This file is never sourced by the package.


# @chunk with_seed
# Run `expr` with a fixed seed, leaving the random number stream as it was
# (EDARK: .with_seed()).
with_seed <- function(seed, expr) {
  genv <- globalenv()
  old  <- if (exists(".Random.seed", envir = genv, inherits = FALSE)) get(".Random.seed", envir = genv)
  on.exit({
    if (is.null(old)) {
      if (exists(".Random.seed", envir = genv, inherits = FALSE)) rm(".Random.seed", envir = genv)
    } else {
      assign(".Random.seed", old, envir = genv)
    }
  })
  set.seed(seed)
  expr
}


# @chunk format_p
# P-values as EDARK shows them: "< 0.001", otherwise three decimals
# (EDARK: edark_format_p()).
format_p <- function(p) {
  p <- suppressWarnings(as.numeric(p))
  out <- ifelse(p < 0.001, "< 0.001", sprintf("%.3f", p))
  out[is.na(p)] <- "-"
  out
}


# @chunk set_reference_levels
# Put each factor's reference level first. Ordered factors and levels that
# are not present are left alone (EDARK: apply_reference_levels()).
set_reference_levels <- function(data, reference_levels) {
  for (v in names(reference_levels)) {
    if (!v %in% names(data)) next
    x <- data[[v]]
    if (!is.factor(x) || is.ordered(x) || !reference_levels[[v]] %in% levels(x)) next
    data[[v]] <- relevel(x, ref = reference_levels[[v]])
  }
  data
}


# @chunk droplevels_cols
# Drop unused levels from the factor columns named in `vars`.
droplevels_cols <- function(data, vars) {
  for (v in intersect(vars, names(data))) {
    if (is.factor(data[[v]])) data[[v]] <- droplevels(data[[v]])
  }
  data
}


# @chunk can_model
# Can a variable enter a model on these rows? It needs two or more distinct
# non-missing values (EDARK: .partition_modelable()).
can_model <- function(x) length(unique(x[!is.na(x)])) >= 2L


# @chunk coef_table
# Coefficients with Wald-type 95% confidence intervals - the critical value
# comes from the distribution of the model's own p-values: t with residual df
# (lm), t with Satterthwaite df (lmerTest), z (glm, glmer). Logistic models
# also get odds ratios (EDARK: edark_coef_table()).
coef_table <- function(model, data) {
  sm <- summary(model)$coefficients
  stat_col <- grep("value$", colnames(sm))[1L]
  p_col    <- grep("^Pr", colnames(sm))[1L]
  is_mixed <- inherits(model, "merMod")
  is_logit <- (inherits(model, "glm") || inherits(model, "glmerMod")) &&
    identical(family(model)$family, "binomial")

  if (is_mixed) {
    X      <- lme4::getME(model, "X")
    labels <- attr(terms(model, fixed.only = TRUE), "term.labels")
  } else {
    X      <- model.matrix(model)
    labels <- attr(terms(model), "term.labels")
  }
  col_var <- c("(Intercept)", labels)[attr(X, "assign") + 1L]
  names(col_var) <- colnames(X)

  term_names <- rownames(sm)
  var   <- unname(col_var[term_names])
  level <- vapply(seq_along(term_names), function(i) {
    v <- var[i]
    x <- if (!is.na(v) && v %in% names(data)) data[[v]]
    if (is.null(x) || !(is.factor(x) || is.character(x) || is.logical(x))) return(NA_character_)
    sub(paste0("^", gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", v)), "", term_names[i])
  }, character(1))

  if (inherits(model, "lmerModLmerTest")) {
    df <- unname(sm[, "df"]); crit <- qt(0.975, df = df)
  } else if (inherits(model, "merMod") || inherits(model, "glm")) {
    df <- NA_real_; crit <- qnorm(0.975)
  } else {
    df <- df.residual(model); crit <- qt(0.975, df = df)
  }
  est <- unname(sm[, 1L])
  se  <- unname(sm[, 2L])
  lo  <- est - crit * se
  hi  <- est + crit * se

  data.frame(
    variable       = var,
    term           = term_names,
    level          = level,
    estimate       = est,
    std.error      = se,
    statistic      = if (!is.na(stat_col)) unname(sm[, stat_col]) else NA_real_,
    df             = rep_len(df, length(est)),
    p.value        = if (!is.na(p_col)) unname(sm[, p_col]) else NA_real_,
    conf.low       = lo,
    conf.high      = hi,
    effect         = if (is_logit) exp(est) else est,
    effect.low     = if (is_logit) exp(lo) else lo,
    effect.high    = if (is_logit) exp(hi) else hi,
    effect_measure = if (is_logit) "odds_ratio" else "coefficient",
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}


# @chunk categorical_test
# Table 1 test for a categorical variable: Pearson's chi-squared without
# continuity correction, or Fisher's exact test when any expected count is
# below 5 (EDARK: .edark_categorical_test()). Used through gtsummary::add_p().
categorical_test <- function(data, variable, by, ...) {
  tb <- table(droplevels(factor(data[[variable]])), droplevels(factor(data[[by]])))
  if (nrow(tb) < 2L || ncol(tb) < 2L) return(data.frame(p.value = NA_real_, method = NA_character_))
  expected <- outer(rowSums(tb), colSums(tb)) / sum(tb)
  if (all(expected >= 5)) {
    p <- suppressWarnings(chisq.test(tb, correct = FALSE))$p.value
    return(data.frame(p.value = p, method = "Pearson's chi-squared test"))
  }
  p <- tryCatch(fisher.test(tb, workspace = 2e7)$p.value, error = function(e) NULL)
  if (!is.null(p)) return(data.frame(p.value = p, method = "Fisher's exact test"))
  p <- with_seed(20260918L, fisher.test(tb, simulate.p.value = TRUE, B = 10000)$p.value)
  data.frame(p.value = p, method = "Fisher's exact test (Monte Carlo, 10,000 replicates)")
}


# @chunk prepare_model_rows
# The rows a model is fitted to: complete for every model variable, ordered
# factors made plain factors (so each level is compared with the reference),
# reference levels set, unused levels dropped, cluster IDs as factors
# (EDARK: .prepare_model_rows()).
prepare_model_rows <- function(data, outcome, predictors, clusters, reference_levels) {
  vars <- unique(c(outcome, predictors, clusters))
  keep <- intersect(c(vars, ".edark_row_id"), names(data))
  rows <- data[, keep, drop = FALSE]
  rows <- rows[complete.cases(rows[, vars, drop = FALSE]), , drop = FALSE]
  for (v in c(outcome, predictors)) {
    if (is.ordered(rows[[v]])) rows[[v]] <- factor(rows[[v]], levels = levels(rows[[v]]), ordered = FALSE)
  }
  rows <- set_reference_levels(rows, reference_levels)
  for (v in c(outcome, predictors)) {
    if (is.factor(rows[[v]])) rows[[v]] <- droplevels(rows[[v]])
  }
  for (cl in clusters) rows[[cl]] <- factor(rows[[cl]])
  rows
}


# @chunk fit_statistics
# Fit statistics as EDARK reports them (EDARK: .fit_statistics()).
fit_statistics <- function(model, model_type, data, outcome) {
  out <- list(Observations = nobs(model))
  if (model_type %in% c("linear_mixed", "logistic_mixed")) {
    ng <- lme4::ngrps(model)
    for (cl in names(ng)) out[[sprintf("Clusters (%s)", cl)]] <- ng[[cl]]
  }
  if (model_type %in% c("logistic", "logistic_mixed")) {
    y <- data[[outcome]]
    out$Events       <- sum(y == levels(y)[2L])
    out$`Event rate` <- out$Events / nobs(model)
  }
  if (model_type == "linear") {
    s <- summary(model)
    out$`R2`          <- s$r.squared
    out$`Adjusted R2` <- s$adj.r.squared
    out$`Residual SE` <- sigma(model)
    if (!is.null(s$fstatistic)) {
      f <- s$fstatistic
      out$`F statistic`    <- f[["value"]]
      out$`F-test p-value` <- pf(f[["value"]], f[["numdf"]], f[["dendf"]], lower.tail = FALSE)
    }
  }
  if (model_type == "logistic") {
    n <- nobs(model)
    out$`Pseudo R2 (McFadden)`   <- 1 - model$deviance / model$null.deviance
    cs <- 1 - exp((model$deviance - model$null.deviance) / n)
    out$`Pseudo R2 (Nagelkerke)` <- cs / (1 - exp(-model$null.deviance / n))
  }
  if (model_type %in% c("linear_mixed", "logistic_mixed")) {
    # Both return a bare NA, not a list, after a singular fit
    r2 <- suppressWarnings(suppressMessages(tryCatch(performance::r2_nakagawa(model), error = function(e) NULL)))
    if (is.list(r2)) {
      out$`Marginal R2`    <- r2$R2_marginal
      out$`Conditional R2` <- r2$R2_conditional
    }
    icc <- suppressWarnings(suppressMessages(tryCatch(performance::icc(model), error = function(e) NULL)))
    if (is.list(icc)) out$`ICC (adjusted)` <- icc$ICC_adjusted
    vc <- as.data.frame(lme4::VarCorr(model))
    for (i in seq_len(nrow(vc))) {
      lab <- if (vc$grp[i] == "Residual") "Residual SD" else sprintf("Random intercept SD (%s)", vc$grp[i])
      out[[lab]] <- vc$sdcor[i]
    }
  }
  out$AIC              <- AIC(model)
  out$BIC              <- BIC(model)
  out$`Log-likelihood` <- as.numeric(logLik(model))
  out <- Filter(function(v) length(v) == 1L && !is.na(suppressWarnings(as.numeric(v))), out)
  data.frame(statistic = names(out), value = vapply(out, as.numeric, numeric(1)),
             row.names = NULL, stringsAsFactors = FALSE)
}


# @chunk predict_response
# Predictions on the response scale (probabilities for a logistic model).
# Mixed models use the fixed effects alone (re.form = NA), as for a new
# patient from an unseen cluster (EDARK: .perf_predict()).
predict_response <- function(model, newdata = NULL, mixed = FALSE) {
  args <- list(model, type = "response")
  if (!is.null(newdata)) args$newdata <- newdata
  if (mixed) args$re.form <- NA
  as.numeric(do.call(predict, args))
}


# @chunk calibration_bins
# Deciles of predicted risk: mean predicted vs observed proportion, with a
# Wilson 95% interval (EDARK: .calibration_bins()).
calibration_bins <- function(pred, y01) {
  k   <- min(10L, length(unique(pred)))
  bin <- dplyr::ntile(pred, k)
  do.call(rbind, lapply(sort(unique(bin)), function(b) {
    i <- bin == b
    n <- sum(i)
    obs <- mean(y01[i])
    z <- qnorm(0.975)
    centre <- (obs + z^2 / (2 * n)) / (1 + z^2 / n)
    half   <- z * sqrt(obs * (1 - obs) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
    data.frame(bin = b, n = n, predicted = mean(pred[i]), observed = obs,
               low = max(0, centre - half), high = min(1, centre + half))
  }))
}


# @chunk performance_measures
# Every selected performance measure for one set of predictions
# (EDARK: .perf_measures()). `y` is the outcome as the model saw it.
# Calibration intercept and slope are only computed on rows the model was
# not fitted to - on its own rows they are 0 and 1 by construction.
performance_measures <- function(pred, y, logistic, checks, with_slope) {
  out <- list(n = length(pred))
  .try <- function(expr) tryCatch(expr, error = function(e) NULL)
  if (logistic) {
    y01 <- as.integer(y == levels(y)[2L])
    out$n_events <- sum(y01)
    both <- length(unique(y01)) == 2L
    if ("discrimination" %in% checks && both) {
      roc <- .try(pROC::roc(y01, pred, levels = c(0L, 1L), direction = "<", quiet = TRUE))
      if (!is.null(roc)) {
        ci <- as.numeric(pROC::ci.auc(roc))   # DeLong
        out$auc      <- as.numeric(pROC::auc(roc))
        out$auc_low  <- ci[1L]
        out$auc_high <- ci[3L]
        out$roc      <- roc
      }
    }
    if ("calibration" %in% checks) {
      out$brier       <- mean((pred - y01)^2)
      out$brier_null  <- mean(y01) * (1 - mean(y01))   # predicting the prevalence for everyone
      out$calibration <- calibration_bins(pred, y01)
      if (with_slope && both) {
        lp <- qlogis(pmin(pmax(pred, 1e-8), 1 - 1e-8))
        out$cal_intercept <- .try(unname(coef(glm(y01 ~ 1, offset = lp, family = binomial()))[1L]))
        out$cal_slope     <- .try(unname(coef(glm(y01 ~ lp, family = binomial()))[2L]))
      }
    }
  } else {
    y <- as.numeric(y)
    if ("prediction_error" %in% checks) {
      out$rmse <- sqrt(mean((y - pred)^2))
      out$mae  <- mean(abs(y - pred))
      out$r2   <- 1 - sum((y - pred)^2) / sum((y - mean(y))^2)
    }
    if ("calibration" %in% checks && with_slope && length(unique(pred)) > 1L) {
      out$cal_intercept <- mean(y - pred)
      out$cal_slope     <- .try(unname(coef(lm(y ~ pred))[2L]))
    }
  }
  out
}


# @chunk performance_scores
# Every scalar measure for one vector of predictions, NA where it does not
# apply - fast, as it runs once per resample (EDARK: .perf_scores()). AUC is
# the Mann-Whitney statistic, the same value pROC gives.
score_names <- c("auc", "brier", "cal_intercept", "cal_slope", "rmse", "mae", "r2")
performance_scores <- function(pred, y, logistic) {
  out <- setNames(rep(NA_real_, length(score_names)), score_names)
  ok  <- is.finite(pred)
  pred <- pred[ok]
  y    <- y[ok]
  if (length(pred) < 3L) return(out)
  .try <- function(expr) tryCatch(suppressWarnings(expr), error = function(e) NA_real_)
  if (logistic) {
    y01 <- as.integer(y == levels(y)[2L])
    out[["brier"]] <- mean((pred - y01)^2)
    if (length(unique(y01)) == 2L) {
      n1 <- sum(y01 == 1L)
      n0 <- sum(y01 == 0L)
      out[["auc"]] <- (sum(rank(pred)[y01 == 1L]) - n1 * (n1 + 1) / 2) / (n1 * n0)
      lp <- qlogis(pmin(pmax(pred, 1e-8), 1 - 1e-8))
      out[["cal_intercept"]] <- .try(unname(coef(glm(y01 ~ 1, offset = lp, family = binomial()))[1L]))
      out[["cal_slope"]]     <- .try(unname(coef(glm(y01 ~ lp, family = binomial()))[2L]))
    }
  } else {
    y <- as.numeric(y)
    out[["rmse"]] <- sqrt(mean((y - pred)^2))
    out[["mae"]]  <- mean(abs(y - pred))
    out[["r2"]]   <- 1 - sum((y - pred)^2) / sum((y - mean(y))^2)
    out[["cal_intercept"]] <- mean(y - pred)
    if (length(unique(pred)) > 1L) out[["cal_slope"]] <- .try(unname(coef(lm(y ~ pred))[2L]))
  }
  out
}

# Which measures the selected checks report (EDARK: .perf_keys())
performance_keys <- function(checks, logistic) {
  unique(c(
    if ("discrimination" %in% checks) "auc",
    if ("calibration" %in% checks) c(if (logistic) "brier", "cal_intercept", "cal_slope"),
    if ("prediction_error" %in% checks) c("rmse", "mae", "r2")
  ))
}


# @chunk resample_refit
# Refit the model to a set of rows (a training fold or a bootstrap resample),
# dropping factor levels the rows do not have. Returns NULL when the model
# cannot be fitted (EDARK: .perf_refit()).
refit <- function(rows) {
  for (v in c(outcome, predictors)) if (is.factor(rows[[v]])) rows[[v]] <- droplevels(rows[[v]])
  if (logistic && nlevels(rows[[outcome]]) < 2L) return(NULL)
  for (cl in resample_clusters) rows[[cl]] <- factor(rows[[cl]])
  fit <- tryCatch(suppressWarnings(suppressMessages(refit_model(model_formula, rows))),
                  error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  list(model = fit, data = rows)
}

# Rows the refitted model can predict: every factor level seen when fitting
# (EDARK: .perf_predictable()).
predictable <- function(fit_data, rows) {
  ok <- rep(TRUE, nrow(rows))
  for (v in predictors) {
    if (is.factor(fit_data[[v]])) ok <- ok & as.character(rows[[v]]) %in% levels(fit_data[[v]])
  }
  ok
}

# Factor predictors re-levelled to the refitted model's levels (EDARK: .perf_align())
align_levels <- function(fit_data, rows) {
  for (v in predictors) {
    if (is.factor(fit_data[[v]])) rows[[v]] <- factor(as.character(rows[[v]]), levels = levels(fit_data[[v]]))
  }
  rows
}


# @chunk cv_folds
# Fold numbers for each repeat, all drawn up front (EDARK: .perf_cv_plan()).
# Mixed models: whole clusters of the first cluster variable go to a fold.
# Logistic models: stratified by outcome so every fold has events.
cv_folds <- function(mf, k, repeats) {
  n <- nrow(mf)
  if (length(resample_clusters) > 0L) {
    g   <- as.character(mf[[resample_clusters[1L]]])
    ids <- unique(g)
    k   <- min(k, length(ids))
    assign_folds <- function() sample(rep_len(seq_len(k), length(ids)))[match(g, ids)]
  } else if (logistic) {
    y <- mf[[outcome]]
    k <- if (min(table(y)) < k) max(2L, min(table(y))) else k
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
  list(k = k, folds = lapply(seq_len(repeats), function(r) assign_folds()))
}


# @chunk bootstrap_samples
# Bootstrap resamples, all drawn up front (EDARK: .perf_boot_plan()): rows
# with replacement or, for a mixed model, whole clusters of the first cluster
# variable - a cluster drawn twice becomes two clusters.
bootstrap_samples <- function(mf, B) {
  if (length(resample_clusters) > 0L) {
    g       <- as.character(mf[[resample_clusters[1L]]])
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

# Smoothed observed-vs-predicted curve (lowess, as rms::calibrate) at `grid`
# (EDARK: .perf_smooth()).
smooth_calibration <- function(pred, y, grid) {
  na <- rep(NA_real_, length(grid))
  ok <- is.finite(pred) & is.finite(y)
  if (length(grid) == 0L || sum(ok) < 10L || length(unique(pred[ok])) < 5L) return(na)
  sm <- tryCatch(lowess(pred[ok], y[ok], iter = 0L), error = function(e) NULL)
  if (is.null(sm)) return(na)
  approx(sm$x, sm$y, xout = grid, ties = mean, rule = 2)$y
}
