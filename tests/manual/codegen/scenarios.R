# Analysis states for checking the generated R script (PRD §A7.9).
#
# Builds the same shared_state fields the app builds - Prepare, the Analyze
# freeze, roles, Table 1, variable investigation, model, diagnostics,
# performance and Model > Results - by calling the service functions the
# modules call, in the order they call them. No Shiny.
#
# Used by run_check.R. Source after devtools::load_all().

cg_prepare <- function(orig, la) {
  ds <- .apply_column_type_overrides(orig, la$column_type_overrides)
  ds <- ds[, intersect(la$included_columns, names(ds)), drop = FALSE]
  ds <- .apply_column_transforms(ds, la$column_transform_specs)
  .apply_row_filters(ds, la$row_filter_specs)
}

# One scenario = Prepare settings + Analyze settings.
cg_scenarios <- list(
  logistic_bootstrap = list(
    drop      = c("patient_mrn", "preop_ferritin"),
    overrides = list(surgery_start_hour = "factor"),
    transforms = list(
      recipient_bmi  = list(method = "cutpoints", breakpoints = c(25, 30), labels = NULL),
      intraop_ebl_ml = list(method = "winsorize", lower_pct = 1, upper_pct = 99),
      preop_creatinine = list(method = "log", log_base = "ln"),
      preop_meld     = list(method = "standardize")
    ),
    filters = list(
      recipient_age = list(type = "numeric", min = 18, max = 80),
      referral_source = list(type = "categorical", levels_selected = c("internal", "external", "transfer"))
    ),
    outcome = "ead", exposure = "liver_donor_type",
    candidates = c("recipient_age", "recipient_bmi", "preop_meld", "preop_creatinine",
                   "cold_ischemia_time_hours", "donor_age", "ivc_clamp_type", "intraop_ebl_ml",
                   "postop_aki_stage", "donor_blood_type"),
    covariates = c("recipient_age", "preop_meld", "cold_ischemia_time_hours", "donor_age",
                   "recipient_bmi", "postop_aki_stage"),
    clusters = NULL,
    refs = list(liver_donor_type = "dcd", recipient_bmi = "25 – < 30"),
    table1 = list(stratify_by_exposure = TRUE, stratify_by_outcome = TRUE,
                  include_pvalues_exposure = FALSE, include_pvalues_outcome = TRUE,
                  include_smd_exposure = FALSE, include_smd_outcome = FALSE),
    vs = list(stepwise_direction = "backward", stepwise_criterion = "BIC",
              lasso_lambda = "lambda.1se", lasso_seed = 4242L),
    purpose = list(model_purpose = "prediction", validation_method = "bootstrap"),
    validation = list(bootstrap_reps = 60L, seed = 777L),
    results = c("results_table", "fit_statistics", "forest_plot", "methods")
  ),

  linear_cv = list(
    drop = c("patient_mrn"), overrides = list(),
    transforms = list(intraop_ebl_ml = list(method = "round", decimal_places = -2),
                      donor_age = list(method = "auto")),
    filters = list(),
    outcome = "postop_los_days", exposure = "ivc_clamp_type",
    candidates = c("recipient_age", "preop_meld_na", "preop_albumin", "intraop_rbc_units",
                   "liver_donor_type", "preop_icu", "intraop_ebl_ml"),
    covariates = c("recipient_age", "preop_meld_na", "intraop_rbc_units", "liver_donor_type", "preop_icu"),
    clusters = NULL, refs = list(),
    table1 = list(stratify_by_exposure = TRUE, stratify_by_outcome = FALSE,
                  include_pvalues_exposure = FALSE, include_smd_exposure = TRUE),
    vs = list(stepwise_direction = "forward", stepwise_criterion = "AIC",
              lasso_lambda = "lambda.min", lasso_seed = 99L),
    purpose = list(model_purpose = "prediction", validation_method = "cv"),
    validation = list(cv_folds = 5L, cv_repeats = 3L, seed = 31L),
    results = c("results_table", "fit_statistics", "forest_plot")
  ),

  linear_mixed_split = list(
    drop = c("patient_mrn"), overrides = list(), transforms = list(), filters = list(),
    outcome = "postop_los_days", exposure = "liver_donor_type",
    candidates = c("recipient_age", "preop_meld", "intraop_rbc_units", "preop_icu"),
    covariates = c("recipient_age", "preop_meld", "intraop_rbc_units"),
    clusters = "transplant_center", refs = list(),
    table1 = list(stratify_by_exposure = TRUE, include_pvalues_exposure = TRUE),
    vs = list(), skip_selection = TRUE,
    purpose = list(model_purpose = "prediction", validation_method = "split",
                   split_variable = "referral_source", training_level = "internal"),
    validation = list(),
    results = c("results_table", "fit_statistics")
  ),

  logistic_mixed_cv = list(
    drop = c("patient_mrn"), overrides = list(), transforms = list(), filters = list(),
    outcome = "ead", exposure = "ivc_clamp_type",
    candidates = c("recipient_age", "preop_meld", "cold_ischemia_time_hours"),
    covariates = c("recipient_age", "preop_meld", "cold_ischemia_time_hours"),
    clusters = "transplant_center", refs = list(),
    table1 = NULL, vs = list(), skip_selection = TRUE,
    purpose = list(model_purpose = "prediction", validation_method = "cv"),
    validation = list(cv_folds = 4L, cv_repeats = 2L, seed = 5L),
    results = c("results_table")
  ),

  logistic_split_text_input = list(
    # Text columns, so the launch casts (text -> number, text -> factor) are in the script
    input = function(d) {
      d$preop_sodium    <- as.character(d$preop_sodium)
      d$referral_source <- as.character(d$referral_source)
      d$ivc_clamp_type  <- as.character(d$ivc_clamp_type)
      d
    },
    drop = c("patient_mrn"), overrides = list(),
    transforms = list(recipient_age = list(method = "cutpoints", breakpoints = c(40, 60, 999), labels = NULL)),
    filters = list(preop_sodium = list(type = "numeric", min = 125, max = 150)),
    outcome = "ead", exposure = "ivc_clamp_type",
    candidates = c("recipient_age", "preop_meld", "preop_sodium", "liver_donor_type"),
    covariates = c("recipient_age", "preop_meld", "preop_sodium", "liver_donor_type"),
    clusters = NULL, refs = list(recipient_age = "40 – < 60"),
    table1 = list(stratify_by_exposure = TRUE, include_pvalues_exposure = TRUE,
                  stratify_by_outcome = TRUE, include_smd_outcome = TRUE),
    vs = list(stepwise_direction = "backward", stepwise_criterion = "AIC", lasso_seed = 11L),
    purpose = list(model_purpose = "prediction", validation_method = "split",
                   split_variable = "referral_source", training_level = "internal"),
    validation = list(),
    results = c("results_table", "forest_plot", "methods")
  ),

  linear_mixed_bootstrap = list(
    drop = c("patient_mrn"), overrides = list(), transforms = list(), filters = list(),
    outcome = "postop_los_days", exposure = "liver_donor_type",
    candidates = c("recipient_age", "preop_meld"), covariates = c("recipient_age", "preop_meld"),
    clusters = c("transplant_center", "or_room_number"), refs = list(),
    table1 = NULL, vs = list(), skip_selection = TRUE,
    purpose = list(model_purpose = "prediction", validation_method = "bootstrap"),
    validation = list(bootstrap_reps = 25L, seed = 2024L),
    results = c("results_table")
  ),

  association_only = list(
    drop = character(0), overrides = list(), transforms = list(), filters = list(),
    outcome = "ead", exposure = "liver_donor_type",
    candidates = c("recipient_age", "preop_meld"), covariates = c("recipient_age", "preop_meld"),
    clusters = NULL, refs = list(), table1 = NULL, vs = list(), skip_selection = TRUE,
    purpose = list(model_purpose = "association"), validation = list(),
    results = character(0), skip_diagnostics = TRUE, skip_performance = TRUE
  )
)


#' Build the export state for one scenario
cg_input <- function(sc) if (is.function(sc$input)) sc$input(liver_tx) else liver_tx

cg_build_state <- function(sc, input = cg_input(sc)) {
  orig  <- cast_column_types(input, 20)
  types <- detect_column_types(orig)
  la <- list(
    included_columns       = setdiff(names(orig), sc$drop),
    column_type_overrides  = sc$overrides,
    column_transform_specs = sc$transforms,
    row_filter_specs       = sc$filters
  )
  wd <- cg_prepare(orig, la)

  # Setup: freeze (module_analysis_setup.R .do_freeze)
  adata <- wd
  adata$.edark_row_id <- seq_len(nrow(adata))
  spec <- list(
    specification_metadata = list(study_type = "descriptive", created_at = Sys.time(),
                                  dataset_signature = digest::digest(wd, algo = "sha256"),
                                  roles_version = 1L,
                                  prepare_snapshot = c(la, list(original_columns = names(orig)))),
    variable_roles = list(
      outcome_variable       = sc$outcome,
      exposure_variable      = sc$exposure,
      candidate_covariates   = sc$candidates,
      table1_variables       = unique(c(sc$exposure, sc$outcome, sc$candidates)),
      univariable_test_pool  = sc$candidates,
      final_model_covariates = sc$covariates,
      cluster_variables      = sc$clusters,
      reference_levels       = sc$refs
    ),
    table1_specification = sc$table1 %||% list(stratify_by_exposure = FALSE),
    variable_selection_specification = utils::modifyList(.default_variable_selection_specification(), sc$vs),
    model_design = .default_model_design(),
    purpose_specification = utils::modifyList(.default_purpose_specification(), sc$purpose),
    validation_settings = utils::modifyList(.default_validation_settings(), sc$validation)
  )
  spec$model_design$model_type <- attr(analysis_model_options(spec, adata), "recommended")
  res <- list(result_tables = list(), result_plots = list(), variable_investigation = list())

  # Table 1
  if (!is.null(sc$table1)) {
    t1 <- build_table1(adata, spec)
    res$result_tables$table1_overall     <- t1$overall
    res$result_tables$table1_by_exposure <- t1$by_exposure
    res$result_tables$table1_by_outcome  <- t1$by_outcome
  }

  # Variables
  md <- analysis_model_data(spec, adata)
  if (!isTRUE(sc$skip_selection)) {
    u <- run_univariable_screen(md, spec)
    res$variable_investigation$univariable          <- u
    res$variable_investigation$univariable_excluded <- attr(u, "excluded_variables")
    res$result_tables$univariable_screen            <- u
    res$variable_investigation$stepwise <- run_stepwise(md, spec)
    res$variable_investigation$lasso    <- run_lasso(md, spec)
    cl <- compute_collinearity(md, sc$candidates)
    res$result_plots$collinearity_plots <- list(flagged_pairs_table = cl$flagged_pairs)
    res$result_plots$collinearity_plots["cor_matrix"]       <- list(cl$cor_matrix)
    res$result_plots$collinearity_plots["cramers_v_matrix"] <- list(cl$cramers_v_mat)
  }

  # Model (module_analysis_modelspec.R)
  fit <- fit_analysis_model(spec, adata)
  stopifnot(identical(fit$status, "success"))
  res$specification_snapshot <- spec
  res$run_status <- list(status = fit$status, fitted_at = Sys.time(), error = fit$error,
                         n_used = fit$n_used, n_total = fit$n_total, formula = fit$formula,
                         outcome_event = fit$outcome_event, reference_levels = fit$reference_levels,
                         run_messages = fit$messages, preflight = list())
  res$fitted_models <- list(primary_model = fit$model)
  res$inference_summary <- list(coefficients = fit$coefficients, fit_statistics = fit$fit_statistics,
                                predicted_values = fit$predicted_values, influence_measures = NULL)

  mt <- spec$model_design$model_type
  if (!isTRUE(sc$skip_diagnostics)) {
    dg <- run_analysis_diagnostics(res, analysis_model_data(spec, adata), analysis_diagnostic_options(mt)$id)
    res$diagnostics <- dg[setdiff(names(dg), c("plots", "influence_measures"))]
    res$result_plots$diagnostic_plots <- dg$plots
    res$result_tables["diagnostic_summary"] <- list(dg$metrics)
  }
  if (!isTRUE(sc$skip_performance)) {
    vl <- analysis_validation(spec, mixed = mt %in% c("linear_mixed", "logistic_mixed"))
    pf <- run_analysis_performance(res, adata, analysis_performance_options(mt)$id, vl)
    res$performance <- pf[setdiff(names(pf), "plots")]
    res$result_plots["performance_plots"] <- list(pf$plots)
    res$result_tables["performance_summary"] <- list(pf$metrics)
  }
  if (length(sc$results)) {
    want_unadj <- "results_table" %in% sc$results
    un  <- if (want_unadj) fit_unadjusted_models(res)
    tbl <- build_results_table(res, un)
    res$result_tables["main_results"]    <- list(if ("results_table" %in% sc$results) tbl)
    res$result_tables["fit_statistics"]  <- list(if ("fit_statistics" %in% sc$results) build_fit_statistics_table(res))
    res$result_plots["coefficient_plot"] <- list(if ("forest_plot" %in% sc$results) build_forest_plot(tbl))
    res["methods_paragraph"] <- list(if ("methods" %in% sc$results) build_methods_paragraph(res, !is.null(un)))
    res$fitted_models["univariable_models"] <- list(un$models)
    res$results_generation <- list(generated_at = Sys.time(), outputs = sc$results,
                                   include_unadjusted = !is.null(un), unadjusted_status = un$status)
  }

  export_state(
    dataset_input = input, dataset_original = orig, dataset_working = wd,
    original_column_types = types, last_applied_specs = la,
    analysis_data = adata, analysis_spec = spec, analysis_result = res
  )
}
