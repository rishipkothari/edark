#' Analysis Pipeline Reset Service
#'
#' Provides \code{reset_analysis_pipeline()}, the single function responsible
#' for clearing downstream \code{shared_state} fields when an upstream change
#' invalidates previously computed results. See PRD §8.6 for the authoritative
#' reset matrix.
#'
#' @name service_analysis_pipeline
NULL


#' Reset downstream analysis state after an upstream change
#'
#' Clears the appropriate \code{shared_state} fields depending on which step
#' triggered the reset. Does not show any modal — the calling module is
#' responsible for confirming with the user before calling this function.
#'
#' @param shared_state A Shiny \code{reactiveValues} object.
#' @param from_step Integer. The step that triggered the reset: \code{1}
#'   (role assignment change), \code{3} (the model's rows changed — the
#'   train/test split was set, changed or removed in Step 1), \code{4}
#'   (covariate change after a fit), or \code{5} (a Step 5 setting, e.g. the
#'   optimizer, changed after a fit).
#'
#' @return \code{invisible(NULL)}
#' @export
reset_analysis_pipeline <- function(shared_state, from_step) {
  from_step <- as.integer(from_step)

  if (from_step == 1L) {
    # Role change invalidates everything downstream of Setup.
    # Null out the entire result object and reset all spec fields
    # downstream of variable_roles (which has already been updated by the
    # calling module).
    shared_state$analysis_result <- NULL

    if (!is.null(shared_state$analysis_spec)) {
      shared_state$analysis_spec$variable_selection_specification <-
        .default_variable_selection_specification()
      shared_state$analysis_spec$model_design <- .default_model_design()
      # Covariates start unselected; Step 4 starts over
      shared_state$analysis_spec$variable_roles$final_model_covariates <- NULL
    }

  } else if (from_step %in% c(3L, 4L, 5L)) {
    # A covariate change (4) or a Step 5 setting change (5) invalidates the
    # fitted model, diagnostics, performance and results — but leaves Table 1
    # and variable investigation intact. A change of training rows (3) also
    # invalidates variable investigation, which ran on the old rows; Table 1
    # describes the whole dataset and the covariate selection is the user's,
    # so both are kept. Step 5 changes the spec itself after the reset, so
    # model_design is left alone here.
    if (!is.null(shared_state$analysis_result)) {
      res <- shared_state$analysis_result

      if (from_step == 3L) {
        res$variable_investigation                <- NULL
        res$result_tables$univariable_screen      <- NULL
        res$result_plots$collinearity_plots       <- NULL
      }

      res$fitted_models$primary_model      <- NULL
      res$fitted_models$univariable_models <- NULL
      res$run_status <- list(
        status       = NULL,
        fitted_at    = NULL,
        run_messages = tibble::tibble(
          level   = character(),
          stage   = character(),
          message = character()
        )
      )
      res$result_tables$main_results       <- NULL
      res$result_tables$fit_statistics     <- NULL
      res$result_tables$diagnostic_summary  <- NULL
      res$result_tables$performance_summary <- NULL
      res$result_plots$coefficient_plot     <- NULL
      res$result_plots$diagnostic_plots     <- NULL
      res$result_plots$performance_plots    <- NULL
      res$inference_summary <- list(
        coefficients       = NULL,
        fit_statistics     = NULL,
        predicted_values   = NULL,
        influence_measures = NULL
      )
      res$diagnostics        <- NULL
      res$performance        <- NULL
      res$methods_paragraph  <- NULL
      res$results_generation <- NULL

      shared_state$analysis_result <- res
    }
  }

  invisible(NULL)
}


# Fresh purpose_specification block for analysis_spec (PRD §A3.5). Step 1
# owns it; a role reset leaves it alone. validation_method ("bootstrap", "cv"
# or "split") applies to prediction models only; split_variable /
# training_level only to "split".
.default_purpose_specification <- function() {
  list(
    model_purpose     = "association",
    validation_method = "bootstrap",
    split_variable    = NULL,
    training_level    = NULL
  )
}


# Fresh variable_selection_specification block for analysis_spec (Step 3).
# lasso_seed fixes cv.glmnet's random fold assignment so a LASSO run can be
# reproduced (and restored with the spec).
.default_variable_selection_specification <- function() {
  list(
    method                  = "univariable",
    univariable_p_threshold = 0.2,
    stepwise_direction      = "backward",
    stepwise_criterion      = "BIC",
    lasso_lambda            = "lambda.1se",
    lasso_seed              = 20260919L,
    selected_variables      = NULL
  )
}


# Resampling settings for cross-validation and the bootstrap, edited in the
# Model › Performance sidebar. bootstrap_reps NULL = the default for the model
# type (.PERF_BOOT_DEFAULT, or .PERF_BOOT_MIXED_DEFAULT for mixed models).
.default_validation_settings <- function() {
  list(
    cv_folds       = 10L,
    cv_repeats     = 5L,
    bootstrap_reps = NULL,
    seed           = 20260919L
  )
}


# Fresh model_design block for analysis_spec (PRD §3.5). Random intercepts
# come from variable_roles$cluster_variables, so there is no field for them.
.default_model_design <- function() {
  list(
    model_type                 = NULL,
    confidence_interval_level  = 0.95,
    optimizer                  = "bobyqa",
    linked_model_specification = NULL
  )
}
