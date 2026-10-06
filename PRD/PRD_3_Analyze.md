# EDARK — Analyze PRD (Tab 3 · Analyze)

**Scope:** the nine-step guided workflow that freezes the working dataset and takes the user from study framing to a fitted, diagnosed, evaluated, reported and exported model.
**Master PRD:** [PRD_0_Master.md](PRD_0_Master.md) — principles (§M2–M3), state ownership (§M5), Prepare → Analyze hand-off (§M6.6), sessions (§M8).
**Implementation details and as-built mechanics:** [NOTE_implementation.md](NOTE_implementation.md) §N6. Statistical methods registry: §N2.
**Build sequence:** [BUILD_Analysis.md](BUILD_Analysis.md).

*Derived from the superseded monolithic Analyze PRD (now in `archive/`). Section numbers are unchanged apart from the `A` prefix (old §8.6 = §A8.6). The old Global Mandates section is §A0; the old Section 13 (sessions) moved to §M8.*

> **Step numbering:** this document describes nine steps. The code has five, with Diagnostics / Performance / Results nested as sub-tabs under Step 5 Model; Export is no longer an Analyze step but the top-level **4 · Export** page (§A10). Read "Step 6/7/8" below as those sub-tabs and "Step 9" as 4 · Export, until this document is renumbered.

---

## A0 — Module Conventions

**Pipe operator:** `magrittr` `%>%` only — never the base pipe `|>`. Applies to all Analyze code and all generated R scripts.

**Project conventions:** the PRD index, §M2–M5 (architecture), [NOTE_UI-principles.md](NOTE_UI-principles.md) (layout, actions, hierarchy).

**Dependencies:** the app's runtime packages are listed in §M9.1 (authoritative: `DESCRIPTION`). Packages loaded by generated scripts: §A4.3. `pacman` is used in generated scripts only, never by the app.

**Progress:** every run button uses the blocking progress modal (§M3.5) — never `withProgress` toasts.

**Downstream reset behaviour:** changes that trigger `reset_analysis_pipeline()` with a confirmation modal: Step 1 role changes, Step 1 train/test split changes once variable investigation or a model exists, Step 4 covariate changes after a model has been fitted, Step 5 optimizer change after a fit. Changes that do NOT trigger a reset: the model purpose alone (association ↔ prediction without a split change), Table 1 modifications, diagnostic or performance check add/remove, variable investigation reruns, export selections. See §A8.6.

---

## A1 — Overview and Scope

### A1.1 Purpose

The Analysis module extends EDARK with a structured, guided workflow for fitting and reporting statistical models appropriate for clinical observational research. It takes the working dataset produced by the Prepare stage and guides the user from study framing through model specification, diagnostics, results, and export — producing outputs suitable for direct use in academic manuscripts.

### A1.2 Relationship to Existing App

The Analysis module is Tab 3 (`3 · Analyze`) in the `page_navbar` structure. It receives `shared_state$dataset_working` as its input. It does not modify the working dataset, does not write back to any Prepare or Explore state, and has no upstream dependencies beyond the frozen dataset snapshot it takes at entry. It is a consumer, not a participant, in the upstream reactive graph.

### A1.3 Target User

Three archetypes, in ascending statistical fluency:

**Archetype A — The Clinician.** Understands the clinical question deeply. Has limited statistical vocabulary. Needs the app to frame decisions in clinical language, prevent obvious errors silently, and surface non-obvious errors explicitly. Will follow guided steps if they feel logical. Will abandon the workflow if it feels like a stats course.

**Archetype B — The Clinician-Researcher.** Runs studies regularly. Knows what a confounder is, has heard of mixed models, has used SPSS or basic R. Wants sensible defaults with the ability to override. Reads model output tables fluently. Will notice if something looks wrong.

**Archetype C — The Biostatistician.** Knows exactly what they want. Uses the app for speed and reproducibility, not guidance. Needs full access to model options, raw result objects, and exportable R code that they can audit and extend. Will distrust the app if it hides things or makes silent decisions.

The UI must serve all three without patronizing C or losing A.

### A1.4 Study Type Framing — The Central Design Principle

Every downstream decision in the analysis workflow — Table 1 stratification, appropriate model family, variable selection framing, output labeling — flows from one early question: **what kind of study is this?**

Rather than asking users to answer this abstractly, the app infers study type from role assignments and surfaces it as a soft label that guides subsequent steps. Study type is derived as follows:

| Exposure assigned | Outcome assigned | Study type |
|---|---|---|
| Yes | Yes | Exposure-outcome association study |
| No | Yes | Risk factor / descriptive association study |
| Yes | No | Descriptive (exposure distribution) |
| No | No | Descriptive cohort |

If exposure is assigned but no outcome is assigned, a soft nudge is shown: *"You have assigned an exposure but no outcome — most analyses require an outcome variable. Continue for descriptive summaries only."*

Study type is displayed persistently as a labeled badge throughout the workflow. It drives soft nudges and hard constraints. The exposure variable is structurally enforced as a single-select — it is impossible to assign more than one variable as exposure. Help text reads: *"One primary exposure or treatment variable — additional variables of interest are covariates."*

Study type framing drives:
- **Exposure-outcome study:** Table 1 stratified by exposure; model selection centers on the exposure-outcome relationship; covariate selection framed as confounder identification
- **Risk factor / descriptive association study:** Table 1 stratified by outcome; model selection centers on variable associations with the outcome; covariate selection framed as candidate predictor screening
- **Purely descriptive:** Table 1 unstratified; no inferential modeling expected

### A1.4a Model Purpose — Association or Prediction

Separate from study type (which comes from roles), the user states in Step 1 what the model is **for**:

| Model purpose | Question | Judged by | Step 7 Performance |
|---|---|---|---|
| **Association** (default) | How does the exposure / each risk factor relate to the outcome? | Its estimates, CIs and assumptions (Steps 5–6) | Optional context (e.g. a c-statistic) |
| **Prediction** | How well does the model predict the outcome for new patients? | Its discrimination and calibration on rows it has not seen | The point of the analysis |

Creating a model and checking its assumptions are the same for both purposes; evaluating predictive performance (Model › Performance) is a separate goal. Choosing **Prediction** asks for a **validation method** — one of three, mutually exclusive:

| Validation method | `validation_method` | Model built from | Validated set |
|---|---|---|---|
| **Bootstrap** (default) | `"bootstrap"` | all rows | Harrell optimism correction: refit in each resample, subtract the mean optimism |
| **Cross-validation** | `"cv"` | all rows | k-fold (repeated), pooled out-of-fold predictions |
| **Held-out test set** | `"split"` | the training rows | rows at the other levels of a chosen variable |

The method must be set in Step 1 because a held-out set changes the rows Steps 3–5 learn from. A **held-out test set** is data kept apart on purpose (e.g. other centres — close to external validation): the user picks a factor column (with no role) and its **training level**; rows at that level are the **training set**, rows at any other level the **test set**, rows missing the variable neither. It cannot be combined with resampling. **Bootstrap / cross-validation** are for a single, homogeneous sample. There is no random single train/test split — it is inefficient and unstable, so the app does not offer one.

With a held-out set, everything that learns from the data uses the training rows only — variable investigation (Step 3), covariate row counts and levels (Step 4), preflight and the model fit (Step 5), diagnostics — so the test set stays unseen until Performance measures it. Table 1 still describes the whole frozen dataset. The purpose alone changes no computation; switching between bootstrap and cross-validation does not either (the model is fitted to all rows under both); adding, changing or removing a held-out set does (§A5.3 Step 1).

**Resampling uses the covariates confirmed in Step 4, unchanged.** Variable selection is *not* repeated in each resample: EDARK discourages automated selection, so the covariate set is treated as fixed (pre-specified). If stepwise / LASSO did drive the selection, the resampled estimates are somewhat optimistic.

**Coefficients.** Every method yields one final model whose coefficients may be reported: with a held-out set, the model fitted to the training rows; with bootstrap or cross-validation, the model fitted to all rows (the fold / resample models are discarded — resampling estimates the performance of the *modelling procedure*). What undermines reported CIs and p-values is data-driven variable selection, not the validation method.

**Association models** are not validated. Apparent discrimination and calibration are still shown as a description of fit (useful e.g. for a propensity or confounder model), labelled as not evidence about the exposure effect.

### A1.5 Scope Boundary

The module is deliberately bounded. It is a structured analysis assistant for common clinical observational study designs, not a general statistical computing environment.

**In scope for v1:** descriptive summaries, unadjusted comparisons, linear regression, logistic regression, linear mixed models, logistic mixed models, variable selection helpers, manuscript-ready outputs, reproducible R code export.

**Planned for future versions:** propensity score matching, inverse probability treatment weighting, propensity score-adjusted regression, generalized estimating equations, multinomial logistic regression, ordinal logistic regression, Poisson and negative binomial regression, marginal effects and predicted value plots, analysis package import.

**Out of scope:** survival and time-to-event models, Bayesian models, machine learning classifiers, mediation and structural equation modeling, multiple imputation, automated model selection, arbitrary R code execution.

---

## A2 — User Stories

### A2.1 Archetype A — The Clinician

**Background:** Cardiac surgery fellow. Collected a retrospective dataset of 400 patients. Wants to know if intraoperative hypotension is associated with postoperative AKI. Has never run a regression outside of GraphPad Prism.

**Story:**

> I've cleaned my data in the Prepare tab. I go to Analyze. The app asks me to identify my outcome and my exposure. I pick AKI (binary) and hypotension (binary). The app tells me this looks like an exposure-outcome association study and shows me a Table 1 split by hypotension. It looks right. I move to variable selection. The app shows me a table of unadjusted associations between each candidate variable and AKI — odds ratios with confidence intervals for each one. Age, ASA class, and baseline creatinine have the strongest associations. I include them because they also make clinical sense to me. The app shows logistic regression as the appropriate model for a binary outcome — other options are visible but clearly secondary. I click Run. I get an odds ratio table that looks like something I've seen in a paper. I download the zip folder and send it to my supervisor.

**What the app must do for this user:**
- Never require statistical vocabulary to complete the workflow
- Make the right model family obvious without hiding alternatives
- Flag problems in plain language ("only 12 patients had the outcome — your model may be unreliable")
- Present unadjusted outcome-regressed associations clearly so the user can make their own covariate decisions
- Produce output that looks like a published paper, not an R console

### A2.2 Archetype B — The Clinician-Researcher

**Background:** Hepatology attending. Runs 3-4 studies per year. Comfortable with multivariable regression, knows what a confounder is, has collaborated with biostatisticians. Working on a longitudinal dataset of liver transplant recipients with repeated creatinine measurements.

**Story:**

> I've got a working dataset with one row per visit per patient. I go to Analyze. I assign creatinine as my continuous outcome, tacrolimus exposure group as my exposure, and patient ID as my cluster variable. I also assign a broad candidate pool — age, donor type, rejection episodes, tacrolimus level, bilirubin, albumin, and a few others I want to consider. Because I assigned a cluster, the app selects a linear mixed model with a random intercept per patient. I move to variable selection. The app shows me univariable linear regression results for each candidate against creatinine. I also run the LASSO helper on the candidate pool — it corroborates that bilirubin and albumin are strong contributors. I make my final covariate list based on those results plus clinical judgment. I check the VIF table — everything looks fine. I run the model. I review the fixed effects table and the residual plots. I notice a convergence warning — the app explains what it means and suggests trying a different optimizer. I switch optimizer and re-run. Clean results. I export the full zip with the report, figures, and R script and send the script to my statistician collaborator to verify.

**What the app must do for this user:**
- Recognize data structure and suggest appropriate model family without forcing it
- Make the two-stage variable workflow clear: broad candidate assignment first, then selection refinement using outcome-regressed univariable models
- Surface model warnings with enough context to act on them, not just flag them
- Produce an exportable R script that a statistician can read and audit

### A2.3 Archetype C — The Biostatistician

**Background:** Supports a clinical research group. Uses R daily. Being asked to use EDARK because the clinical team wants a reproducible, documented workflow. Skeptical of GUI tools that make silent decisions.

**Story:**

> A fellow on the team has done the data cleaning in Prepare. I take over at Analyze. I assign variable roles — no exposure, because this is a risk factor screen, so the app labels it as one. I assign a broad candidate pool deliberately. I move to variable selection. The univariable screen shows me unadjusted linear regression results for each candidate against the outcome. I run backward stepwise with BIC rather than the default AIC. I review the suggested covariate list and override two inclusions based on clinical reasoning. I assign site as a cluster, so the app fits a logistic mixed model with a random intercept for site. I expand the advanced options panel and verify the optimizer setting. I review the generated R code before running — it matches exactly what I configured, no surprises. I run the model, check the Wald CIs, review the calibration plot. I download the full zip. I also download the RDS file with the fitted object for post-hoc contrasts in my own R session, and export the frozen analysis dataset as a Stata `.dta` file for a collaborator.

**What the app must do for this user:**
- Expose all configuration options, collapsed by default but always accessible
- Never make a silent decision — every default must be visible and overridable
- Generate R code that is clean, auditable, and matches the fitted model exactly
- Provide raw fitted object as RDS for downstream use outside the app
- Export the frozen analysis dataset in multiple formats: CSV, RDS, SPSS `.sav`, Stata `.dta`, Excel `.xlsx`
- Allow override of soft nudges without friction

---

## A3 — Data Contract

### A3.1 What Enters the Analysis Module

The analysis module receives two objects from the upstream app: `shared_state$dataset_working` — the fully prepared, filtered, and transformed dataset produced by the Prepare stage — and `shared_state$column_types`, the named vector of detected column types produced by `detect_column_types()`. The module uses `column_types` to pre-populate variable type information at role assignment without re-detecting from scratch.

The module reads from `shared_state` but never writes back to any Prepare or Explore fields. It only writes to its own three designated fields defined in §A3.3.

### A3.2 Dataset Signature

When the analysis module freezes the working dataset, it computes and stores a **dataset signature** — a compact structural fingerprint used for cache invalidation, EDA config matching, and analysis package validation.

The signature captures:
- Column names and their order
- Column classes (numeric, factor, character, POSIXct, etc.)
- Factor levels per factor column, in order
- Row count

This is intentionally more than `str()` output but less than a full data hash. It detects: column additions and removals, type changes, factor level additions or removals caused by filtering, and meaningful row count shifts. It does not detect silent value edits within rows, which is an acceptable tradeoff.

**Signature validity rules when loading a saved config or analysis package:**
- Exact match → proceed silently
- Row count differs, all else matches → soft warning: *"Row count has changed since this analysis was configured. Results may differ."*
- Factor levels differ → hard warning: *"One or more factor variables have different levels than when this analysis was configured. Review role assignments before proceeding."*
- Column names, classes, or structure differ → blocking error: *"Dataset structure has changed. This analysis configuration is not compatible with the current working dataset."*

> **As built — differs from the spec above.** Step 1 stores `digest::digest(working_data, algo = "sha256")` — a full data hash — in `specification_metadata$dataset_signature`, and the mismatch banner compares hashes. Any value edit therefore triggers the banner. Session files (§M8.4) use a separate structural *dataset definition* and never this hash. Decide whether to change the code to the structural signature or this section to the hash (tracked in CLAUDE.md).

### A3.3 Fields Added to `shared_state`

Three fields are added to `shared_state` at app launch, initialized as `NULL`. These are the only fields the analysis module writes to:

```r
# Analysis module fields — added to shared_state initialisation in edark.R
analysis_data    = NULL,  # frozen data.frame with .edark_row_id column
analysis_spec    = NULL,  # named list: full declarative analysis specification
analysis_result  = NULL   # named list: fitted objects, tables, plots, summaries
```

### A3.4 Dataset Freeze Behavior

When the user clicks **"Start Analysis"**, the module copies `shared_state$dataset_working` into `shared_state$analysis_data`, appends a row-level internal identifier column (`.edark_row_id`), and computes the dataset signature. This frozen copy is what all downstream analysis operates on exclusively.

The frozen dataset does not update if the user returns to Prepare and modifies the working dataset. A persistent banner in the Analysis module detects signature mismatch and displays: *"Your working dataset has changed since this analysis was started. Restart analysis to use the updated data."* with a **Restart Analysis** button.

Restarting clears `analysis_spec`, `analysis_result`, and all analysis UI state. It does not affect anything in Prepare or Explore.

### A3.5 The `analysis_spec` Object

The spec is a nested named list built incrementally as the user progresses through the workflow steps. It is the single source of truth for model fitting, code generation, caching, and export. It is fully serializable to JSON for export and for the analysis package feature.

```r
analysis_spec <- list(

  specification_metadata = list(
    study_type         = "exposure_outcome",
                         # "exposure_outcome" | "risk_factor" |
                         # "descriptive_exposure" | "descriptive"
    created_at         = Sys.time(),
    dataset_signature  = list(),  # full signature object from §A3.2
    roles_version      = 0L,      # bumped on every Step 1 role write
    step1_roles        = NULL,    # Step 1's own snapshot of its fields
    prepare_snapshot   = list(    # read once from Prepare at freeze
      included_columns, column_type_overrides, column_transform_specs,
      row_filter_specs,           # = shared_state$last_applied_specs
      original_columns, original_dims, working_dims
    )
  ),

  variable_roles = list(
    outcome_variable        = NULL,
    exposure_variable       = NULL,
    candidate_covariates    = NULL,
    table1_variables        = NULL,  # exposure + outcome + all candidates
    univariable_test_pool   = NULL,  # all candidates; not user-adjustable
    final_model_covariates  = NULL,  # written live by Step 4 as boxes are checked
    cluster_variables       = NULL,  # character vector; one random intercept each
    reference_levels        = list() # named list: variable_name -> reference level
  ),

  table1_specification = list(
    stratify_by_exposure     = TRUE,
    stratify_by_outcome      = FALSE,
    include_pvalues_exposure = FALSE,
    include_pvalues_outcome  = TRUE,
    include_standardized_mean_difference = TRUE
  ),

  variable_selection_specification = list(
    method                  = "univariable",
    univariable_p_threshold = 0.2,
    stepwise_direction      = "backward",
    stepwise_criterion      = "BIC",
    lasso_lambda            = "lambda.1se",
    lasso_seed              = 20260919L,   # fixes cv.glmnet's random folds
    selected_variables      = NULL
  ),

  purpose_specification = list(   # Step 1; kept by a role reset (§A1.4a)
    model_purpose     = "association",  # "association" | "prediction"
    validation_method = "bootstrap",    # "bootstrap" | "cv" | "split" (prediction only)
    split_variable    = NULL,           # "split": factor column with no role
    training_level    = NULL            # "split": its level that marks the training rows
  ),
  # A split applies only when model_purpose == "prediction", validation_method
  # == "split" and both fields are set — analysis_split(spec).
  # analysis_validation(spec, mixed) returns the method ("none" for association)
  # with the settings below, defaults filled.

  validation_settings = list(     # Model › Performance sidebar; written live
    cv_folds       = 10L,         # 2–20; capped at the clusters / rarer outcome class
    cv_repeats     = 5L,          # 1–50
    bootstrap_reps = NULL,        # NULL = 200, or 100 for mixed models; 10–2000
    seed           = 20260919L
  ),

  model_design = list(
    model_type                 = NULL,
    confidence_interval_level  = 0.95,  # hardcoded
    optimizer                  = "bobyqa",
    linked_model_specification = NULL   # reserved for PS (v1.5)
  ),
  # Random intercepts come from variable_roles$cluster_variables — there is no
  # separate random-effects field. Random slopes are out of scope.

  analysis_options = list(
    missing_data_handling  = "complete_case",
    interaction_terms      = list()  # stubbed; unused in v1
  )
)
```

### A3.6 The `analysis_result` Object

Populated progressively as the user completes workflow steps. The UI reads exclusively from this object.

```r
analysis_result <- list(

  specification_snapshot = analysis_spec,  # spec the model was fitted with (stale check)

  run_status = list(
    status           = "success",          # or "failed"
    fitted_at        = Sys.time(),
    error            = NULL,               # message when failed
    n_used           = NULL,               # complete-case rows fitted
    n_total          = NULL,               # rows available (the training set when split)
    formula          = NULL,
    outcome_event    = NULL,               # list(variable, event, reference) — binary outcomes
    reference_levels = list(),             # factor predictors: reference level used
    preflight        = list(),             # preflight warnings at fit time
    run_messages = data.frame(             # level, stage, message
      level   = character(),
      stage   = character(),
      message = character()
    )
  ),

  fitted_models = list(
    primary_model      = NULL,
    univariable_models = list()
  ),

  variable_investigation = list(
    univariable = NULL,
    stepwise    = NULL,
    lasso       = NULL
  ),

  result_tables = list(
    table1_overall      = NULL,
    table1_by_exposure  = NULL,
    table1_by_outcome   = NULL,
    univariable_screen  = NULL,
    main_results        = NULL,   # Step 8: build_results_table() data.frame (display + export)
    fit_statistics      = NULL,   # Step 8: formatted fit statistics data.frame
    diagnostic_summary  = NULL,   # Step 6: long metrics table
    performance_summary = NULL    # Step 7: long metrics table (set, key, label, value, format)
  ),

  result_plots = list(
    coefficient_plot    = NULL,
    diagnostic_plots    = list(    # Step 6
      residuals_vs_fitted = NULL,
      qq_plot             = NULL,
      scale_location      = NULL,
      binned_residuals    = NULL,  # logistic models
      linearity_plot      = NULL,  # residuals vs each continuous predictor
      influence_plot      = NULL,  # Cook's distance
      leverage_plot       = NULL,
      random_effects_qq   = NULL,
      cluster_size_plot   = NULL
    ),
    performance_plots   = list(    # Step 7; one list per set of rows
      apparent = list(roc_curve = NULL, calibration_plot = NULL, predicted_probs = NULL),
      test     = list(roc_curve = NULL, calibration_plot = NULL, predicted_probs = NULL)
    ),
    collinearity_plots  = list(
      correlation_heatmap = NULL,
      cramers_v_matrix    = NULL,
      flagged_pairs_table = NULL
    ),
    lasso_plots = list(
      coefficient_path    = NULL,
      cross_validation    = NULL
    ),
    balance_plots         = NULL  # reserved for PS (v1.5)
  ),

  inference_summary = list(
    coefficients       = NULL,
    fit_statistics     = NULL,
    predicted_values   = NULL,
    influence_measures = NULL   # per row: .edark_row_id, cooks, leverage, std_resid
  ),

  diagnostics = NULL,  # Step 6: run_at, model_type, checks, sample, residuals,
                       # linearity, influence, vif, separation, random_effects,
                       # metrics (long table), messages — see run_analysis_diagnostics()

  performance = NULL,  # Step 7: run_at, model_type, checks, basis ("fixed" /
                       # "marginal"), split, sets (apparent, test: n, n_events,
                       # auc + CI, brier, brier_null, calibration bins,
                       # cal_intercept, cal_slope, rmse, mae, r2), metrics,
                       # messages — see run_analysis_performance()

  generated_r_script  = NULL,  # character string; cached from Step 5
  methods_paragraph   = NULL,  # character string; cached from Step 8
  results_generation  = NULL,  # Step 8: generated_at, outputs (ids), include_unadjusted,
                               # unadjusted_status (variable, status, message)

  linked_model_result = NULL   # reserved for PS (v1.5)
)
```

### A3.7 Table 1 Behavior by Study Type

| Study type | Default stratification | Default p-values |
|---|---|---|
| Exposure-outcome | By exposure | Off |
| Risk factor / descriptive association | By outcome | On |
| Descriptive exposure | By exposure | Off |
| Descriptive cohort | None (overall only) | Off |

When both exposure and outcome are assigned, both Table 1 versions are produced and presented as sub-tabs alongside the overall tab. Both are available for independent export.

P-value tooltip: *"These p-values describe distributional differences between groups. They are not used for variable selection. For outcome-regressed associations, see the Variable Selection step."*

### A3.8 Export Folder Structure

The export folder structure is defined in §A10.2. See §A10 for the authoritative specification.

---

## A4 — Analytic Scope

### A4.1 Model and Method Inventory

All analytical methods available in v1. Linear regression and logistic regression serve dual roles — both as the univariable outcome regression screen during variable selection and as the final multivariable model.

| Method | Purpose | Outcome type | Data structure | Clustered | Primary package | P-value method |
|---|---|---|---|---|---|---|
| Descriptive summary / Table 1 | Population description; baseline characteristics by group | Any | Any | Any | `gtsummary` | Optional group comparison tests via `stats`; see §A3.7 |
| Linear regression | Univariable outcome screen; multivariable adjusted model | Continuous | One row per subject | No | `stats::lm` | Native t-tests via `summary()` |
| Logistic regression | Univariable outcome screen; multivariable adjusted model | Binary (2-level factor) | One row per subject | No | `stats::glm` | Wald z-tests via `summary()` |
| Linear mixed model | Multivariable model for repeated measures or clustered continuous outcomes | Continuous | Multiple rows per subject | Yes | `lme4` + `lmerTest` | Satterthwaite approximated degrees of freedom via `lmerTest` |
| Logistic mixed model | Multivariable model for repeated measures or clustered binary outcomes | Binary (2-level factor) | Multiple rows per subject | Yes | `lme4::glmer` | Wald z-tests via `summary()`. Interpret with caution in small samples or with rare outcomes. |
| Stepwise selection | Variable selection helper; advisory only | Continuous or binary | One row per subject | No | `stats::step` | From final selected `lm` or `glm` object |
| LASSO penalized regression | Variable selection helper; advisory only | Continuous or binary | One row per subject | No | `glmnet` | None by design — no inference table produced |
| Collinearity summary | Pre-fit pairwise correlation screen; post-fit formal VIF | Continuous candidates | Any | Any | `performance`, `correlation` | Not applicable |

### A4.2 P-value and Confidence Interval Methods

**One definition per quantity, app-wide.** Every p-value and CI is computed by `R/stats_inference.R` — no other file computes them. Regression CIs are **Wald-type** for every model: estimate ± critical value × SE, where the critical value comes from **the same distribution as the model's p-value**. A CI therefore excludes the null exactly when p < 0.05. Wald CIs are instantaneous, never fail, and are the standard in clinical research reporting. Profile likelihood CIs and likelihood ratio test p-values are deferred to v1.5 as advanced options (see §A11).

| Method | P-value approach | Confidence intervals | Notes |
|---|---|---|---|
| Linear regression | t-tests (residual df), native from `summary()` | estimate ± t(0.975, residual df) × SE (= `confint.lm()`) | Standard OLS assumptions apply |
| Logistic regression | Wald z-tests, native from `summary()` | estimate ± 1.96 × SE on the log-odds scale, exponentiated (= `confint.default()`) | Acceptable for adequate sample sizes |
| Linear mixed model | t-tests with Satterthwaite df via `lmerTest` | estimate ± t(0.975, Satterthwaite df) × SE | Standard accepted approach in clinical research |
| Logistic mixed model | Wald z-tests, native from `summary()` | estimate ± 1.96 × SE, exponentiated | Interpret with caution in small samples, sparse data, or near-boundary random effects. Preflight validation flags inadequate event counts. |
| Univariable screen (Step 3), unadjusted models (Step 8) | As the matching model type above | As the matching model type above | Same function (`edark_coef_table()`) as the final model |
| Stepwise | Not reported | Not reported | Selection only |
| LASSO | None | None | Explicitly exploratory; coefficient path and selected variable list only |

**Other tests (group comparisons and correlation):**

| Quantity | Where | Method |
|---|---|---|
| Numeric variable across groups | Table 1, Report Table One, Report correlation sections | Kruskal-Wallis test (any number of groups) |
| Categorical variable across groups | Table 1, Report Table One, Report correlation sections | Pearson chi-square without continuity correction; Fisher's exact test when any expected count < 5 (Monte Carlo, 10,000 replicates, fixed seed, only if the exact algorithm runs out of memory) |
| Correlation of two numeric variables | Explore scatter plot, Report correlation sections | Pearson r; t-test p (n − 2 df); 95% CI by Fisher's z |
| Standardised mean difference | Table 1 | gtsummary `smd` (no p-value) |

**P-value display:** "< 0.001", otherwise three decimals (`edark_format_p()`), everywhere including gtsummary tables.

**Publication table footnotes** come from `edark_inference_note(model_type)` — the same sentence in Step 5, Step 8, the Summary and the methods paragraph:
- Linear: "Wald-type 95% confidence intervals using the t distribution with residual degrees of freedom. P-values from t-tests."
- Logistic: "Wald 95% confidence intervals (normal distribution), exponentiated to odds ratios. P-values from Wald z-tests."
- Linear mixed: "Wald-type 95% confidence intervals using the t distribution with Satterthwaite degrees of freedom. P-values from t-tests with Satterthwaite degrees of freedom (lmerTest)."
- Logistic mixed: as logistic, plus "Interpret with caution in small samples or with rare outcomes."

### A4.3 Full Package Dependencies

| Function | Packages |
|---|---|
| Modeling | `stats` (base R), `lme4`, `lmerTest`, `glmnet` |
| Tidy extraction | `broom`, `broom.mixed` |
| Parameter tables and inference | `parameters` |
| Diagnostics | `performance`, `insight` |
| Correlation and collinearity | `correlation` |
| Heteroskedasticity testing | `lmtest` |
| ROC / AUC | `pROC` |
| Separation detection | `detectseparation` |
| Publication tables | `gtsummary`, `flextable`, `gt` |
| Plots | `ggplot2`, `patchwork` |
| Dataset export | `haven`, `writexl` |
| Report generation | `officer`, `rmarkdown` |
| Serialization and packaging | `jsonlite`, `zip`, `digest` |
| Utilities | `dplyr`, `tidyr`, `tibble`, `magrittr` |

Note: `pacman` is used in generated R scripts for robust package loading. It is not a runtime dependency of EDARK itself.

### A4.4 Hard Constraints — Model Availability

Unavailable models are visibly disabled in the UI with a plain-language explanation. Never silently hidden.

| Outcome type | Cluster assigned | Available models | Disabled — reason shown |
|---|---|---|---|
| Continuous | No | Linear regression | Logistic: *"Outcome is continuous"*; Mixed: *"No cluster variable assigned"* |
| Continuous | Yes | Linear mixed model | Logistic: *"Outcome is continuous"*; Linear: *"Cluster variables are assigned — use a mixed model"* |
| Binary | No | Logistic regression | Linear: *"Outcome is binary"*; Mixed: *"No cluster variable assigned"* |
| Binary | Yes | Logistic mixed model | Linear: *"Outcome is binary"*; Logistic: *"Cluster variables are assigned — use a mixed model"* |
| Unrecognized | Any | None | All: *"Assign an outcome variable before selecting a model"* |

Assigning cluster variables commits the analysis to a mixed model: a non-mixed model with clusters assigned is a blocking preflight error (`PF_CLUSTERS_UNUSED`). To fit a plain regression, remove the cluster role in Step 1.

### A4.5 Soft Nudges — Contextual Guidance

Surfaced as dismissible banners at the relevant workflow step. Never blocking.

| Condition | Message | Stage |
|---|---|---|
| Multiple rows per subject, no cluster | "Your data may have repeated measures. Consider assigning the subject ID as a cluster variable and using a mixed model." | Role assignment |
| Single row per subject | "Data appears cross-sectional. Standard regression is appropriate." | Role assignment |
| Any missing data | "X rows contain missing values. Complete-case analysis will exclude these. Report in methods." | Preflight |
| Missing > 20% | "Complete-case analysis excludes more than 20% of data. Review missingness." | Preflight |
| Missing > 50% | "Complete-case analysis excludes more than 50% of data. Results may not be representative." | Preflight |
| Events per variable < 10 (logistic) | "Fewer than 10 outcome events per candidate variable. Reduce covariates." | Preflight |
| Events per variable < 5 (logistic) | "Fewer than 5 events per variable. High risk of overfitting." | Preflight |
| Pairwise correlation > 0.7 | "High correlation detected between some candidate variables." | Variable selection |
| Post-fit VIF 5–10 | "Moderate variance inflation. Interpret affected estimates cautiously." | Diagnostics |
| Post-fit VIF > 10 | "High variance inflation. Review the VIF table." | Diagnostics |
| Singular fit (mixed) | "Singular fit. Random effects structure may be too complex." | Diagnostics |
| Convergence warning (mixed) | "Model did not converge. Simplify random effects or change optimizer." | Diagnostics |
| Test set too small (split) | "The test set has N complete rows and fewer than 100 events or non-events. Its performance estimates will be imprecise." | Preflight |
| Rows missing the split variable | "N rows have no value for X and belong to neither the training nor the test set." | Preflight |
| Rare factor level (< 5 obs) | "Variable X has a level with fewer than 5 observations." | Preflight |
| Outcome prevalence < 5% or > 95% | "Outcome prevalence is [X]%. Wald inference is fragile with rare events." | Preflight |

**Note on VIF timing:** pre-fit: pairwise correlation screen (soft advisory). Post-fit: formal VIF via `performance::check_collinearity()`. VIF is a transparency tool — the app does not prompt refit.

### A4.6 Computational Batching — Independent Run Buttons

Each step has its own explicit run button. No step silently triggers another. Each launches the blocking modal.

| Step | Button label | Notes |
|---|---|---|
| Table 1 | "Generate Table 1" | `gtsummary` can be slow on wide datasets |
| Univariable screen | "Run Univariable Screen" | One model per candidate |
| Stepwise | "Run Stepwise Selection" | Iterative refitting |
| LASSO | "Run LASSO" | Single cross-validated fit |
| Model fit | "Run Model" | Fast for lm/glm; moderate for mixed |
| Diagnostics | "Run Diagnostics" | Post-fit extraction |
| Performance | "Run Performance" | Predictions on the model rows and, with a split, the test set |
| Results | "Generate Outputs" | Dependent on selections |

### A4.7 Deferred to Future Versions

**Planned for future versions:** PS methods, GEE, multinomial/ordinal/count regression, marginal effects, analysis package import, LRT p-values (v1.5), profile likelihood CIs (v1.5). See §A11 for complete deferral log.

**Out of scope:** survival models, Bayesian models, ML classifiers, mediation/SEM, multiple imputation, arbitrary R execution.

---

## A5 — Workflow and Sequencing

### A5.1 Overview

The analysis module is Tab 3 (`3 · Analyze`) in `page_navbar`. Nine steps as `bslib::navset_pill` — horizontal pills.

```
Tab 3: Analyze
└── navset_pill (9 steps, horizontal)
    ├── Step 1: Setup                  layout_sidebar(position = "right")
    ├── Step 2: Table 1                layout_sidebar(position = "left")
    ├── Step 3: Variable Investigation full-width, internal vertical navset_pill
    ├── Step 4: Covariate Confirmation layout_sidebar(position = "right")
    ├── Step 5: Model Creation         navset_underline: Summary | Run Model (sidebar left)
    ├── Step 6: Diagnostics            layout_sidebar(position = "left")
    ├── Step 7: Performance            layout_sidebar(position = "left")
    ├── Step 8: Results                layout_sidebar(position = "left")
    (Export moved to the top-level 4 · Export page - §A10)
```

Steps 5–6 create the model and check its assumptions; Step 7 evaluates how well it predicts — a separate goal that matters most when the model purpose is **Prediction** (§A1.4a). Steps 6–9 open once a model is fitted.

Non-blocking rail. Run Model disabled until an outcome is assigned and the Tier 2 preflight has no errors. Step 4 has no confirmation step — its selection is always current in the spec, so a model with the exposure alone (nothing checked in Step 4) is valid.

Status indicators: Not started (muted) / In progress (blue) / Complete (green check) / Stale (amber warning).

### A5.2 Global UI Principles

- Run/generate buttons in config panel only — never in output area
- No per-object download buttons — all downloads via 4 · Export
- Consistent message area at top of every output panel
- Placeholder cards for ungenerated outputs — never blank space
- Blocking modal with step-list progress for operations > ~2 seconds
- Run Model has two disabled states: spec incomplete (grey) vs preflight errors (grey + red inline message + pulse animation on click)

### A5.3 Step-by-Step Specification

#### Step 1 — Setup

**Module file:** `R/module_analysis_setup.R`

**Layout:** `layout_sidebar(position = "right")` — main panel: role assignment table; sidebar: read-only role summary.

**Entry state:** "Start Analysis" button + dataset summary. Click freezes dataset, appends `.edark_row_id`, computes signature, initializes `analysis_spec`.

**Role assignment table:** one row per variable. Columns: Variable, Type, Reference level (dropdown), Exposure (radio), Outcome (radio), Candidate (checkbox), Cluster (checkbox). Each variable holds at most one role. Outcome and exposure are single-select; candidate and cluster are multi-select. Column header "Clear" buttons; the Candidate header also has "All". Search/filter above table.

**Cluster role:** grouping variables such as patient ID or centre. Each checked cluster becomes its own random intercept `(1 | cluster)` in a mixed model; check several for nested (patients within centres) or crossed groupings. Any non-datetime column may be a cluster (numeric IDs are converted with `factor()` at fit time). There is no separate subject ID or time role, and no random slopes.

**Sidebar:** study type badge (color-coded), **Model Purpose**, role summary, dataset snapshot. Updates reactively.

**Model Purpose** (§A1.4a): radio **Association** (default) / **Prediction**, with a one-line explanation of each. Prediction reveals **Validation**: radio **Bootstrap** (default) / **Cross-validation** / **Held-out test set**, with a note that resampling settings live in Model › Performance. Held-out test set reveals a **Variable** select (factor columns with ≥ 2 levels and no role) and a **Training level** select (that variable's levels; defaults to a level named like "train", else the first). Written to `analysis_spec$purpose_specification` as the inputs change.
- The purpose alone never clears anything, nor does switching between bootstrap and cross-validation. A change that alters the training rows (choosing or leaving Held-out test set, or changing its variable or training level) asks "Clear Analysis Results?" when variable investigation or a model exists; Clear & Continue runs `reset_analysis_pipeline(shared_state, 3)` (Table 1 and the covariate selection are kept); Cancel puts the inputs back.
- A split variable cannot hold a role: if a role is later assigned to it, the split variable is cleared.
- With a split, Steps 3–6 use the training rows only; Step 7 measures performance on the test rows too.

**Defaults:** `table1_variables` = candidates + exposure + outcome; `univariable_test_pool` = candidates; `final_model_covariates` = none (covariates start unchecked in Step 4); `purpose_specification` = association, no split.

**Step complete when:** outcome assigned.

---

#### Step 2 — Table 1

**Module file:** `R/module_analysis_table1.R` | **Service file:** `R/service_analysis_tables.R`

**Layout:** `layout_sidebar(position = "left")` — sidebar: stratification checkboxes, p-value toggle, SMD toggle, generate button; main panel: `navset_card_tab` — Overall (always), By Exposure (conditional), By Outcome (conditional).

Variables: exposure + outcome + all candidates, fixed order. Placeholders for ungenerated tabs.

**Step complete when:** Table 1 generated at least once.

---

#### Step 3 — Variable Investigation

**Module file:** `R/module_analysis_varinvestigation.R` | **Service file:** `R/service_analysis_variable_selection.R`

**Layout:** full-width. Vertical `navset_pill` — three pills, each with `layout_sidebar(position = "left")`.

**Univariable Screen:** p-value threshold input (default 0.2) + run button. Auto-selects lm/glm by outcome type. Non-mixed. All candidates. Tier 1 validation banner (§A8.5).

**Collinearity:** auto-computed on entry (and recomputed when the train/test split changes). Heatmap, Cramér's V, flagged pairs tabs. No run button.

**Training rows:** with a train/test split (Step 1), every tool here runs on the training rows only (`analysis_model_data()`), so variable selection never sees the test set.

**Stepwise / LASSO:** `radioGroupButtons` toggle. Stepwise: direction + criterion + run. LASSO: lambda + random seed + run. State preserved on toggle. Advisory banner. When an exposure is assigned it is held in every model and never offered for selection (see §A7.7–A7.8), and the results say so.

**Step complete when:** at least one tool run.

---

#### Step 4 — Covariate Confirmation

**Module file:** `R/module_analysis_covariate_confirm.R`

**Layout:** `layout_sidebar(position = "right")` — main panel: covariate table; sidebar: live Model / Sample counts (including an "If mixed model" row count when clusters are assigned) and Checks.

**Table columns:** Include (checkbox; candidates start unchecked; header All / Clear), Variable, Type, Missing, Row cost, Univariable, Stepwise, LASSO (each with Add / Replace in the header and a parameter tooltip), Reference level. Role variables (outcome, exposure, clusters) are locked, checked rows at the top. Cell highlighting: green (suggested), pink (not suggested), grey (not run). See §A9.7.

**Training rows:** with a train/test split, the counts, missingness, row cost, EPV and surviving reference levels are computed on the training rows.

**No confirm step:** every change (checkbox, reference level, Add, Replace) is written to `analysis_spec` immediately — `final_model_covariates`, `reference_levels`, `variable_selection_specification$selected_variables`. The first change after a model has been fitted opens a "Clear Model Results?" modal; Cancel undoes the click, Clear & Continue runs `reset_analysis_pipeline(shared_state, 4)` and applies it. Errors in the Checks panel (e.g. a factor left with one level) do not stop the write — Step 5's preflight blocks the model. Step 3 reruns simply refresh the suggestion columns.

**Step complete when:** always usable; nothing to confirm.

---

#### Step 5 — Model Creation

**Module file:** `R/module_analysis_modelspec.R` (file name kept from when the step was "Model Specification") | **Service files:** `R/service_analysis_models.R`, `R/service_analysis_validation.R`, `R/service_analysis_summary.R` (`R/service_analysis_codegen.R` deferred)

**Layout:** two tabs (`navset_underline`) — **Summary** (first) and **Run Model**. See §A8.4.

**Summary tab:** read-only audit of the current state of every step, top to bottom: data preparation (from the Prepare snapshot taken at freeze), analysis dataset, study design and roles (incl. model purpose, the train/test split with its row counts, and "Modelling: outcome = event (vs reference)"), Table 1, variable investigation, covariates and sample, model (type, formula, random intercepts, optimizer, inference method, fit status), and **every** preflight check — errors, warnings, notes and passes. Built by the pure function `build_analysis_summary(spec, result, data, validation)` so export can reuse it. It shows current state, not a click history.

**Run Model tab — sidebar:** model dropdown (all four types listed; the one valid type — decided by outcome type and whether clusters are assigned — is selected and written to the spec automatically; the others are disabled with the reason in their label); Advanced accordion (mixed models only) with the optimizer; compact live preflight (errors and warnings only); Run Model button. Clicking the disabled button pulses the preflight box.

**Run Model tab — main:** model header (model type, "Modelling:" line for binary outcomes, "Fitted on: training set, variable = level (n rows); m test rows held out for Step 7" when split, formula); results after a fit — Primary result (exposure estimate(s) with 95% CI and p, one row per level vs the reference for a factor exposure; a note for risk-factor studies), Coefficients table (all terms except the intercept, with footnote), Fit statistics, Fitting notes (preflight warnings at fit time + fit warnings/notes); R Code Preview accordion (placeholder until the code generator is built).

**No warning modal:** Run Model proceeds whenever the preflight has no errors; warnings are already on screen and are replayed in Fitting notes.

**Training rows:** with a train/test split the model is fitted to the training rows only; preflight runs on them too (§A8.2).

**Changes after a fit:** every model-affecting change clears the model and everything built on it. Changing the optimizer after a fit asks "Clear Model Results?" (Cancel puts the dropdown back) and then calls `reset_analysis_pipeline(from_step = 5)`. Step 1 roles and split, and Step 4 covariates / reference levels, do the same from their own steps. `analysis_fit_is_stale()` (which includes the split) remains as a safety net for the Run Model results panel.

**Step complete when:** model run successfully.

---

#### Step 6 — Diagnostics

**Module file:** `R/module_analysis_diagnostics.R` | **Service files:** `R/service_analysis_diagnostics.R`, `R/service_analysis_plots.R`

**Purpose:** is the fitted model trustworthy — do its assumptions hold, is it stable? Predictive performance is Step 7.

**Layout:** `layout_sidebar(position = "left")` — sidebar: check list + Run Diagnostics; main: model header + `navset_card_tab` (Overview first, then one tab per computed check). Same sidebar-is-setup pattern as Steps 3 and 5.

**Always included (no checkbox):** sample accounting (rows the model was built from — the training set when split — rows used, missing values per model variable, events for logistic models) and fitting warnings (convergence, singular fit — read from the Step 5 fit).

**Checks** (`analysis_diagnostic_options(model_type)`, all ticked by default):

| Check | Linear | Logistic | Linear mixed | Logistic mixed |
|---|---|---|---|---|
| Residuals | resid vs fitted, Q-Q, scale-location, Breusch-Pagan | binned residuals (response scale) | same plots on conditional residuals | binned residuals |
| Linearity | residuals vs each continuous predictor (loess) | binned residuals vs each continuous predictor | as linear (conditional residuals) | as logistic |
| Influence | Cook's distance, leverage, top 10 rows | same | — (planned: cluster-level influence) | — |
| Collinearity | VIF / GVIF | same | same (fixed effects) | same |
| Separation | — | `detectseparation` | — | on the fixed effects |
| Random effects | — | — | variance, SD, ICC per cluster; cluster sizes; Q-Q of intercepts | same, latent scale (π²/3) |

A model with no continuous predictor gets a note instead of a Linearity tab.

**Advisory, never blocking:** diagnostics do not gate Steps 7–9.

**Step complete when:** diagnostics run at least once.

---

#### Step 7 — Performance

**Module file:** `R/module_analysis_performance.R` | **Service files:** `R/service_analysis_performance.R`, `R/service_analysis_plots.R`

**Purpose:** how well does the model predict? Central when the model purpose is Prediction; optional context (e.g. a c-statistic) for an association model — the header says which.

**Layout:** `layout_sidebar(position = "left")` — sidebar: **Measures** checkboxes (all ticked), **Validation** (Apparent, plus the Step 1 method with its settings: folds + repeats for cross-validation, resamples for the bootstrap, and a random seed — written live to `validation_settings`), Run Performance; main: model header (type, model purpose, time run) + `navset_card_tab`: **Overview** (messages, then one table with a column per set of rows and a reading guide per measure) and one tab per set with its values and plots.

**Measures** (`analysis_performance_options(model_type)`): logistic — discrimination (ROC, AUC with DeLong CI), calibration (deciles with Wilson CIs, Brier score and no-information Brier), predicted probabilities by outcome; linear — prediction error (RMSE, MAE, R²), calibration (observed vs predicted). On the test set, calibration adds the **calibration intercept** and **slope** (logistic: `glm(y ~ offset(lp))` and `glm(y ~ lp)`; linear: mean residual and the slope of observed on predicted) — in-sample they are 0 and 1 by construction, so they are not shown for the apparent set.

**Sets of rows:**
- **Apparent (model rows)** — always; the rows the model was fitted to; labelled optimistic.
- **Test set** — when Step 1 set a train/test split: the held-out rows, prepared exactly like the model rows (`.prepare_model_rows()`: complete cases, ordered → unordered, reference levels). Rows with a factor level the model never saw are dropped with a warning; rows missing a model variable are dropped with a note.
- **Cross-validated** — method `"cv"`: k folds × r repeats. Folds are stratified by outcome (logistic) or made of whole clusters of the first cluster variable (mixed; k capped at the number of clusters). Each fold's model is refitted to the other folds (same covariates) and predicts the rows left out. Measures are computed on the pooled out-of-fold predictions of each repeat and averaged over repeats, with the SD across repeats. Plots pool every out-of-fold prediction.
- **Bootstrap-corrected** — method `"bootstrap"`: Harrell's optimism correction. Each resample's model is scored on the resample (apparent) and on the original rows (test); optimism = mean(apparent − test); corrected = apparent − optimism, for AUC, Brier, calibration intercept and slope (logistic) or RMSE, MAE, R², calibration intercept and slope (linear). The corrected calibration slope doubles as a shrinkage factor. A smoothed calibration curve (lowess, as `rms::calibrate`) is shown apparent and bias-corrected, plus an Apparent | Optimism | Corrected table. Mixed models resample whole clusters of the first cluster variable (a cluster drawn twice becomes two clusters). No CI for corrected values (that needs a double bootstrap).

Resamples are drawn up front with the seed (`.with_seed()` restores the session's random stream), so results are reproducible. A refit that fails, or a bootstrap resample missing a factor level needed to predict the original rows, is skipped and counted; fitting warnings are counted. Refits of linear mixed models use `lme4::lmer` (no Satterthwaite p-values needed). Mixed models use marginal predictions (`re.form = NA`) for every set — the fixed effects alone, as for a patient from a new cluster.

**Running:** the job is computed in steps (`analysis_performance_job()` / `.perf_job_step()` / `.perf_job_finish()`); the module runs refits for ~0.4 s per reactive tick (`invalidateLater`), so the blocking progress modal's **Cancel** button is heard (previous results are kept). A mixed model asking for more than 100 refits (resamples, or folds × repeats) first shows a confirmation modal (Back / Proceed).

**Advisory, never blocking.** Stored in `analysis_result$performance`, `result_plots$performance_plots` and `result_tables$performance_summary`; any model-affecting change clears them.

**Step complete when:** performance run at least once.

---

#### Step 8 — Results

**Module file:** `R/module_analysis_results.R` | **Service files:** `R/service_analysis_tables.R`, `R/service_analysis_plots.R`

**Layout:** `layout_sidebar(position = "left")` — sidebar: output checkboxes + Generate Outputs; main panel: model header + `navset_card_tab`.

**Outputs** (catalogue `.RESULTS_OUTPUTS` — add future outputs there): Results table (sub-option: include unadjusted estimates), Fit statistics, Forest plot, Methods paragraph. All ticked by default. **Only ticked outputs are created and stored; unticked ones are not created, so 4 · Export cannot export them.** Fit statistics add the AUC (with CI) for each set of rows Step 7 evaluated (apparent, test). Each Generate replaces the previous set. The **Summary** tab is always shown, has no checkbox, and computes nothing — key numbers only, no prose (exposure estimates, model, sample, fit, checks, which outputs exist).

**Results table** (`build_results_table()` → one data.frame; `results_table_gt()` for the app, `results_table_flextable()` for Word): rows = exposure then final covariates (never unchosen candidates, so every cell is filled); a factor gets a header row, a *Reference* row and one row per other level; no intercept; random effects not shown (Fit statistics only). Columns: Unadjusted and Adjusted (n), each "OR (95% CI)" or "β (95% CI)" plus p. Exposure bold, reference rows italic. Footnotes: measure (and event), per-unit note, what "adjusted" and "unadjusted" mean, `edark_inference_note()`, and † / ‡ for unadjusted models fitted with a warning / not fitted. Built as our own table, not `tbl_regression()` — that needs broom.helpers and would re-process our numbers.

**Unadjusted estimates** (`fit_unadjusted_models()`): one model per variable, fitted to the final model's own rows (its model frame) with the same engine, random intercepts and optimizer; estimates from `edark_coef_table()`.

**Forest plot** (`build_forest_plot()`): every model term, adjusted estimates; three aligned panels — labels | estimates with 95% CI | "OR (95% CI)" and p; log-scale OR axis with a line at 1 (logistic) or a line at 0 (linear); exposure highlighted. Scaling continuous variables is the user's responsibility (Prepare).

**Methods paragraph** (`build_methods_paragraph()`): the model as fitted — design, outcome (event), exposure, covariates, random intercepts and estimation method (REML / Laplace, optimizer), reference categories, the train/test split (if any), complete-case n, unadjusted models (if generated), CI and p-value methods, assumptions checked in Step 6, predictive performance measured in Step 7 (apparent and/or test set), then a software sentence with R, EDARK and package versions. It does not describe variable selection.

**Table footnotes** per model type — Footnote text per model type is in §A4.2.

**Step complete when:** outputs generated at least once.

---

#### Step 9 — Export

Moved out of Analyze to the top-level **4 · Export** page (2026-10-05). See §A10 and [BUILD_Export.md](BUILD_Export.md).

### A5.4 Stale State Propagation

| Change | Marks stale |
|---|---|
| Step 1 role change | Table 1, var investigation, covariate selection, model, diagnostics, performance, results |
| Step 1 train/test split change | Var investigation, model, diagnostics, performance, results (Table 1 and covariate selection kept) |
| Step 1 model purpose alone | Nothing |
| Step 2 config change | Table 1 only |
| Step 3 investigation rerun | Nothing (Step 4 suggestion columns refresh) |
| Step 4 covariate change after a fit | Model, diagnostics, performance, results |
| Step 5 spec change | Model, diagnostics, performance, results |
| Step 6 diagnostics rerun | Nothing downstream |
| Step 7 performance rerun | Nothing downstream (Step 8 fit statistics read it when generated) |
| Step 8 display change | Results display only |
| Dataset restart | Everything |

### A5.5 Default Spec Population on Skip

| Field | Default | Warning |
|---|---|---|
| `table1_variables` | Candidates + exposure + outcome | None |
| `univariable_test_pool` | Candidates | None |
| `final_model_covariates` | None — the model uses the exposure alone | Preflight note: "No covariates selected — unadjusted model" |
| `model_type` | Pre-selected in Step 5 from outcome type + clusters | None |

---

## A6 — Screen-by-Screen UI Specification

### A6.1 Purpose

This section specifies precise UI components, input types, and rendering behavior. Claude Code reads this to build each step. No UI decisions should be needed during implementation.

### A6.2 Global UI Component Conventions

**Component library:** `bslib` for layout, `shinyWidgets` for enhanced inputs, `shiny` native for standard inputs.

**Theme:** inherit `bs_theme` — flatly, Bootstrap 5, primary `#2c7be5`. No new theme variables.

**Inputs:** full width in sidebars. Section labels: `tags$p(class = "mt-2 mb-1 fw-semibold", ...)`. Horizontal rules: `hr(class = "my-2")`.

**Icons:** `shiny::icon()` only. **Notifications:** `showNotification()` — 4s success, 8s error.

**Blocking modal:** `showModal(easyClose = FALSE)` with step-list icons. No `withProgress` toast.

**Message area:** `uiOutput` card at top of every output panel. **Placeholders:** muted card with italic text. **Stale:** amber icon + banner.

**Numeric display:** max 3 decimal places. < 0.001 shown as "< 0.001".

### A6.3 Step 1 — Setup

**Module file:** `R/module_analysis_setup.R`

`reactable` with raw HTML inputs in cells (state pushed from the server, see §N1.4). Column widths: Variable 140px, Type 80px, Reference 130px, Outcome/Exposure 80px, Candidate 90px, Cluster 70px. Built-in search for filtering. Column header Clear buttons. Reference levels in R factor order. Cluster cells show "—" for datetime columns.

**Sidebar:** study type badge, Model Purpose block (radio, conditional train/test checkbox, variable and training-level selects built with `selectize = FALSE`), role summary, dataset snapshot. Reactive. Downstream cleared via `reset_analysis_pipeline()` (§A8.6).

### A6.4 Step 2 — Table 1

**Module file:** `R/module_analysis_table1.R`

Sidebar: stratification checkboxes, p-value toggle with tooltip, SMD toggle, generate button. Main: `navset_card_tab` with Overall/By Exposure/By Outcome tabs.

### A6.5 Step 3 — Variable Investigation

**Module file:** `R/module_analysis_varinvestigation.R`

Vertical `navset_pill`. **Univariable:** p-threshold + run button; Tier 1 banner. **Collinearity:** auto-computed; heatmap/Cramér's V/flagged pairs. **Stepwise/LASSO:** toggle + configs; Tier 1 banner.

### A6.6 Step 4 — Covariate Confirmation

**Module file:** `R/module_analysis_covariate_confirm.R`

Table in the main panel, live counts and checks in a right sidebar. Add / Replace buttons in method column headers, suggestion indicators (green/pink/grey), parameter tooltips. No confirm button — changes are written as they happen (§A5.3 Step 4).

### A6.7 Step 5 — Model Creation

**Module file:** `R/module_analysis_modelspec.R`

Two tabs — Summary and Run Model — per §A5.3 Step 5 and §A8.4. Pulse animation: CSS keyframes on the preflight box, triggered by a click on the disabled Run Model button's wrapper (Bootstrap gives disabled buttons `pointer-events: none`).

### A6.8 Step 6 — Diagnostics

**Module file:** `R/module_analysis_diagnostics.R`

**Layout:** `layout_sidebar(position = "left")`.

Sidebar: "Model assumptions" checkbox group (Select all / Deselect all; each choice shows a one-line description), Run Diagnostics (`btn-primary w-100`), note that sample accounting and fitting warnings are always included.

Main: model header (type, formula, time run), then `navset_card_tab`:

```
Overview        Warnings card, then cards for each computed section:
                Sample · Fit [mixed] · Residuals · Linearity [logistic: % bins] ·
                Influence · Collinearity · Separation [logistic] · Random effects [mixed]
                Values with a short reading guide (e.g. "VIF 5–10 moderate, > 10 high")
Residuals       linear: resid vs fitted, Q-Q, scale-location, Breusch-Pagan
                linear mixed: same plots on conditional residuals (no BP)
                logistic (both): binned residuals + % of bins including 0
Linearity       one facet per continuous predictor: residuals + loess (linear) or
                binned residuals (logistic)
Influence       [non-mixed] Cook's distance, leverage vs std. residual, top 10 rows
                with their model-variable values
Collinearity    VIF table flagged OK / Moderate / High
Random effects  [mixed] per cluster variable: variance, SD, ICC, cluster count and
                sizes; random-intercept Q-Q; rows per cluster
```

### A6.9 Step 7 — Performance

**Module file:** `R/module_analysis_performance.R`

**Layout:** `layout_sidebar(position = "left")`.

Sidebar: "Measures" checkbox group (all ticked, one-line descriptions), "Validation" (Apparent — always; then the Step 1 method: No validation (association) / Held-out test set — its test levels and row count / Cross-validation — Folds + Repeats inputs / Bootstrap — Resamples input, with the mixed-model confirmation threshold noted; Random seed for either resampling method), Run Performance (`btn-primary w-100`).

Main: model header (type, model purpose — with a note for an association model — time run), then `navset_card_tab`:

```
Overview                Messages card; one table: rows = measures (with reading
                        guide), columns = sets of rows; AUC shown with its CI
Apparent (model rows)   optimism note, values, ROC / calibration / predicted-probability
                        plots (logistic) or observed vs predicted (linear)
Test set                [split] same, plus calibration intercept and slope
Cross-validated         [cv] method note, values ± SD across repeats, model refits
                        (skipped), pooled out-of-fold plots
Bootstrap-corrected     [bootstrap] method note, corrected values, Apparent |
                        Optimism | Corrected table, calibration curve (apparent vs
                        bias-corrected)
```

Plot titles carry the set, e.g. "ROC curve (test set)".

### A6.10 Step 8 — Results

**Module file:** `R/module_analysis_results.R`

Sidebar: "Outputs" checkboxes (Results table + indented "Include unadjusted estimates", Fit statistics, Forest plot, Methods paragraph — each with a one-line description), Generate Outputs (`btn-primary w-100`), note that only created outputs can be exported. Main: model header (type, formula, time generated), `navset_card_tab`: Summary (always) then one tab per created output. Methods tab has a Copy button.

### A6.11 Step 9 — Export

Now the top-level 4 · Export page (§A10): Build & Download left, a plain-language checklist of what to export in the centre (items ticked there, formats and the session option on their rows), what the zip will hold in the info pane.

---

## A7 — Statistical Specification per Model

**Service files:** `R/service_analysis_models.R` (fitting engines), `R/service_analysis_diagnostics.R` (diagnostic computation), `R/service_analysis_plots.R` (figure generation), `R/service_analysis_tables.R` (gtsummary tables), `R/service_analysis_variable_selection.R` (univariable/stepwise/LASSO), `R/service_analysis_codegen.R` (R code generator). **Shared helpers:** `R/analysis_utils.R` (formula assembly, reference leveling, complete case computation).

### A7.1 General Principles Applied to All Models

**Formula assembly:** constructed from `analysis_spec$variable_roles` at fit time. Pattern: `outcome ~ exposure + cov1 + cov2 + ...`. Mixed models append one random intercept per cluster variable: `+ (1 | cluster1) + (1 | cluster2)`. Nested groupings rely on IDs being unique across the parent grouping (preflight warns via `PF_CLUSTER_IDS_SHARED`). No random slopes. Implemented in `analysis_utils.R`.

**Model data** (`fit_analysis_model()` in `service_analysis_models.R`): the training rows when a train/test split applies (`analysis_model_data()`); complete cases over outcome + exposure + covariates (+ clusters for mixed models); **ordered factors are converted to unordered** (level order kept) so every factor uses treatment contrasts — each level vs the reference — rather than polynomial contrasts; reference levels applied; unused levels dropped; cluster variables converted with `factor()`.

**Reference levels:** applied once via `relevel()` per `analysis_spec$variable_roles$reference_levels` before any model fit. For a binary outcome the reference level is the non-event; the model estimates the probability of the other level, shown as "Modelling: outcome = event (vs reference)". Implemented in `analysis_utils.R`.

**Complete cases:** `na.action = na.omit`. Row count stored in `run_status$run_messages`. Implemented in `analysis_utils.R`.

**Confidence intervals:** Wald-type for all models, 95%, computed by `edark_coef_table()` (`R/stats_inference.R`) from `summary(model)$coefficients`: estimate ± critical value × SE, the critical value matching the p-value's distribution (t with residual df for lm, t with Satterthwaite df for lmerTest, z for glm/glmer). See §A4.2. `confint()` / `confint.default()` / `broom::tidy(conf.int = TRUE)` are not used — they differ by model class (profile likelihood for glm) and do not handle `merMod` fixed effects. Logistic models report odds ratios (exponentiated estimate and limits).

**Coefficient extraction:** `edark_coef_table()` — `summary(model)$coefficients` for all four engines (estimate, SE, statistic, native p-value); terms are mapped to variables through the model matrix `assign` attribute. Stored as a data.frame in `inference_summary$coefficients` (variable, term, level, estimate, std.error, statistic, df, p.value, conf.low, conf.high, effect, effect.low, effect.high, effect_measure). Fit statistics are a long data.frame (key, label, value, format) in `inference_summary$fit_statistics`. `broom` / `broom.mixed` / gtsummary tables are left to Phase 7.

**P-values:** method varies by model type. All displayed to 3 decimal places max; < 0.001 as "< 0.001".

**Warning capture:** `withCallingHandlers()` wraps all fitting. Warnings stored in `run_status$run_messages`.

**Publication tables:** Step 8 builds the results table itself from `edark_coef_table()` output (`build_results_table()` → `result_tables$main_results`); gtsummary is used for Table 1 only.

### A7.2 Linear Regression

**Fitting:** `model <- lm(formula, data = analysis_data_complete)`

**Extraction:**

| Extract | Call | Stored in |
|---|---|---|
| Coefficients | `broom::tidy(model, conf.int = TRUE, conf.level = 0.95)` | `inference_summary$coefficients` |
| Fit statistics | `broom::glance(model)` | `inference_summary$fit_statistics` |
| Fitted/residuals/Cook's D | `broom::augment(model)` | `inference_summary$predicted_values`, `influence_measures` |
| Publication table | `gtsummary::tbl_regression(model, exponentiate = FALSE)` | `result_tables$main_results` |
| VIF | `performance::check_collinearity(model)` | `result_tables$diagnostic_summary` |

**P-values:** t-tests from `summary()`. Footnote: "P-values from t-tests."

**Fit statistics:** N, R², adjusted R², RMSE, F-statistic, AIC, BIC.

**Diagnostics (Step 6):** residuals vs fitted, Q-Q plot, scale-location, Breusch-Pagan (`lmtest::bptest()`), residuals vs each continuous predictor (linearity), Cook's distance, leverage vs residuals, top 10 influential observations, VIF table.

**Performance (Step 7):** RMSE, MAE, R² and an observed-vs-predicted plot — apparent, and on the test set when split (plus calibration-in-the-large and slope).

### A7.3 Logistic Regression

**Fitting:** `model <- glm(formula, data = analysis_data_complete, family = binomial)`

**Extraction:**

| Extract | Call | Stored in |
|---|---|---|
| Coefficients (OR) | `broom::tidy(model, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE)` | `inference_summary$coefficients` |
| Coefficients (log-odds) | `broom::tidy(..., exponentiate = FALSE)` | Internal (forest plot) |
| Fit statistics | `broom::glance(model)` | `inference_summary$fit_statistics` |
| Predicted probabilities | `broom::augment(model, type.predict = "response")` | `inference_summary$predicted_values` |
| Separation check | `detectseparation::detect_separation(model)` | `run_status$run_messages` |

**P-values:** Wald z-tests. Footnote: "P-values from Wald z-tests."

**Fit statistics:** N, N events, event rate, pseudo R² (McFadden + Nagelkerke), AIC, BIC, log-likelihood.

**Diagnostics (Step 6):** binned residuals (`performance::binned_residuals(residuals = "response")`, Gelman & Hill — raw residuals of a 0/1 outcome are not interpretable, and performance's default deviance residuals do not average to zero), binned residuals against each continuous predictor (`term =`; linearity on the log-odds scale), Cook's distance, leverage, top influential observations, VIF, separation detection (`glm(method = detectseparation::detect_separation)`).

**Performance (Step 7):** ROC curve with AUC and DeLong CI (`pROC`), calibration plot (decile bins, Wilson CIs) with Brier score and the no-information Brier (prevalence × (1 − prevalence)), predicted probability distribution by outcome group — apparent, and on the test set when split. The calibration intercept (`glm(y ~ 1, offset = logit(p))`) and slope (`glm(y ~ logit(p))`) are shown for the test set only; in-sample they are 0 and 1 by construction.

### A7.4 Linear Mixed Model

**Fitting:** `model <- lmerTest::lmer(formula, data, control = lme4::lmerControl(optimizer = ...))`

**Extraction:**

| Extract | Call | Stored in |
|---|---|---|
| Fixed effects | `broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE)` | `inference_summary$coefficients` |
| Random effects | `broom.mixed::tidy(model, effects = "ran_pars")` | `inference_summary$fit_statistics` |
| Fitted/residuals | `broom.mixed::augment(model)` — conditional | `inference_summary$predicted_values` |
| R² | `performance::r2_nakagawa(model)` | `inference_summary$fit_statistics` |
| ICC | `performance::icc(model)` | `inference_summary$fit_statistics` |

**P-values:** Satterthwaite via `lmerTest`. Footnote: "P-values from Satterthwaite approximated degrees of freedom."

**Fit statistics:** N observations, N clusters, marginal R², conditional R², ICC, AIC, BIC, log-likelihood, random intercept SD, residual SD.

**Diagnostics (Step 6):** residuals vs fitted, Q-Q and scale-location (conditional residuals), residuals vs each continuous predictor, random effects Q-Q, per-cluster variance / ICC (from `VarCorr`; `performance::icc()` returns NA once any component is singular), cluster size distribution + summary, VIF, singular fit check (`isSingular()`), convergence check. Not yet: cluster-level influence (leave-one-cluster-out) — see §A11.2.

**Performance (Step 7):** as linear regression, using marginal predictions (`re.form = NA`).

### A7.5 Logistic Mixed Model

**Fitting:** `model <- lme4::glmer(formula, data, family = binomial, control = lme4::glmerControl(optimizer = ...))`

**Extraction:**

| Extract | Call | Stored in |
|---|---|---|
| Fixed effects (OR) | `broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE, exponentiate = TRUE)` | `inference_summary$coefficients` |
| Marginal predictions | `predict(model, type = "response", re.form = NA)` | `inference_summary$predicted_values` |
| ICC | `performance::icc(model)` | `inference_summary$fit_statistics` |

**P-values:** Wald z-tests. Footnote: "P-values from Wald z-tests. Interpret with caution in small samples or with rare outcomes."

**Fit statistics:** N observations, N clusters, N events, event rate, marginal AUC (apparent and test set, if Step 7 ran), AIC, BIC, log-likelihood, random intercept variance, ICC.

**Diagnostics (Step 6):** binned residuals, binned residuals against each continuous predictor, random effects Q-Q, per-cluster ICC (latent scale, residual variance π²/3), cluster size distribution + summary, VIF, separation (checked on the fixed effects), singular fit, convergence. Not yet: cluster-level influence.

**Performance (Step 7):** all use **marginal** predictions (`re.form = NA`). ROC/AUC, calibration, predicted probability distribution.

### A7.6 Univariable Regression Screen

**Service file:** `R/service_analysis_variable_selection.R`

Batch execution: one `lm` or `glm` per candidate. Always standard (non-mixed). Output: tibble (variable, estimate, CI, p-value) sorted by p-value. Stored in `result_tables$univariable_screen` and `fitted_models$univariable_models`.

**Combined table (Step 8):** side-by-side unadjusted + adjusted for the final model's variables only. The Step 3 screen is not reused: Step 8 refits each unadjusted model on the final model's rows with the same engine (§A5.3 Step 8).

### A7.7 Stepwise Selection

**Service file:** `R/service_analysis_variable_selection.R`

Backward: `stats::step(full_model, scope = ..., direction = "backward", k = ...)`. Forward: `stats::step(null_model, scope = ..., direction = "forward", k = ...)`. `k = log(n)` for BIC, `k = 2` for AIC.

**Exposure held:** when an exposure is assigned, the scope's lower bound is `~ exposure` (otherwise `~ 1`) and the null model is `outcome ~ exposure`. Covariates are then chosen for what they add alongside the exposure, which is how the final model uses them. Risk-factor studies (no exposure) are unchanged. The univariable screen stays unadjusted.

Output: selected formula, selection path tibble, suggested variable list (never includes the exposure), `held_variables`.

### A7.8 LASSO Penalized Regression

**Service file:** `R/service_analysis_variable_selection.R`

`glmnet::cv.glmnet(x, y, family = ..., alpha = 1, nfolds = 10, penalty.factor = ...)`. Lambda selection: `lambda.1se` (default) or `lambda.min`. Factor variables: report original name if any dummy level has non-zero coefficient.

**Exposure held:** when an exposure is assigned, its columns get `penalty.factor = 0` — never shrunk out, never reported as selected.

**Seed:** the 10 cross-validation folds are random. They are drawn with `lasso_seed(spec)` (`variable_selection_specification$lasso_seed`, default 20260919) inside `.with_seed()`, which restores the session's random stream, so the same seed and data give the same lambda and selection. The seed is set in the LASSO sidebar (Random Seed, filled with the stored value), written to the spec on Run, returned as `seed` in the result, and shown in the Step 5 Summary and the Step 4 LASSO column tooltip. The Phase 5b script reproduces the run with `set.seed(<lasso_seed>)` immediately before `cv.glmnet()`.

Output: coefficient path plot data, cross-validation plot data, suggested variable list.

### A7.9 R Code Generation

**Service file:** `R/service_analysis_codegen.R`

Dynamically assembled via `paste0` / `glue` from `analysis_spec`. Generated at model specification time (Step 5), cached in `analysis_result$generated_r_script`. Displayed in Step 5 R Code Preview accordion. Retrieved at export time — not regenerated.

**Script setup:**
```r
options(
  pkgType = "binary",
  repos = c(
    RSPM = "https://packagemanager.posit.co/cran/latest",
    CRAN = "https://cloud.r-project.org"
  )
)
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(gtsummary, ggplot2, broom, performance, magrittr)
```

**Scope — broader than what was run:**
- **Table 1:** only stratifications actually generated
- **Variable selection:** ALL three methods. Run methods use actual parameters. Unrun methods fully commented out with default parameters.
- **Model fitting:** hardcoded formula with the Step 4 covariates. No dependency on selection output.
- **Diagnostics:** ALL diagnostics available for model type, regardless of what was checked.
- **Results:** all extraction code.

Uses `%>%` throughout. If generated code and app produce different results, the app has a bug.

### A7.10 Summary of Dependencies by Model Type

| Package | Linear | Logistic | Linear Mixed | Logistic Mixed | Stepwise | LASSO |
|---|---|---|---|---|---|---|
| `stats` | ✓ | ✓ | | | ✓ | |
| `lme4` | | | ✓ | ✓ | | |
| `lmerTest` | | | ✓ | | | |
| `glmnet` | | | | | | ✓ |
| `broom` | ✓ | ✓ | | | | |
| `broom.mixed` | | | ✓ | ✓ | | |
| `gtsummary` | ✓ | ✓ | ✓ | ✓ | | |
| `performance` | ✓ | ✓ | ✓ | ✓ | | |
| `lmtest` | ✓ | | | | | |
| `pROC` | | ✓ | | ✓ | | |
| `detectseparation` | | ✓ | | | | |
| `ggplot2` | ✓ | ✓ | ✓ | ✓ | | ✓ |

---

## A8 — Preflight Validation Layer

**Service file:** `R/service_analysis_validation.R`

### A8.1 Architecture

Pure function `validate_analysis(spec, data, tier = "full", verbose = FALSE)`. Returns `list(validity_flag, messages, display_messages)`. `validity_flag`: "valid" | "warnings" | "invalid".

### A8.2 Validation Tiers

**Tier 1 — Core Data Validity** (runs before any analysis operation):

| Code | Level | Condition | Message |
|---|---|---|---|
| `PF_NO_OUTCOME` | error | No outcome assigned | "No outcome variable assigned." |
| `PF_ZERO_COMPLETE` | error | Zero complete cases | "No complete cases remain." |
| `PF_OUTCOME_NO_VARIANCE_BINARY` | error | Binary outcome all 0 or all 1 | "Outcome has no events." |
| `PF_OUTCOME_NO_VARIANCE_CONTINUOUS` | error | Continuous outcome single value | "Outcome has only one unique value." |
| `PF_FACTOR_SINGLE_LEVEL` | error | Factor with one level post-complete-case | "Variable [X] has only one level remaining." |
| `PF_SPLIT_INVALID` | error | Train/test split set but its variable is missing, not a factor, holds a role, or has no rows at the training level | Names the problem |
| `PF_SPLIT_NO_TEST` | warning | Every row is at the training level | "…there is no test set. Only apparent (in-sample) performance can be measured." |
| `PF_SPLIT_MISSING` | warning | Rows missing the split variable | "N rows … belong to neither the training nor the test set." |

**Train/test split:** when a split applies (§A1.4a), the split checks run first and **every other check runs on the training rows** — the rows the model is built from. The validator subsets the data itself, so callers pass the whole frozen dataset.

**Tier 2 — Model Specification** (adds to Tier 1, runs before multivariable model):

Errors: `PF_NO_PREDICTORS`, `PF_OUTCOME_MODEL_MISMATCH`, `PF_MIXED_NO_CLUSTER` (mixed model, no cluster assigned), `PF_MIXED_SINGLE_CLUSTER` (a cluster variable with one value), `PF_CLUSTERS_UNUSED` (clusters assigned, non-mixed model selected), `PF_FACTOR_SINGLE_LEVEL` (covariates, on the full-model complete cases).

Also an error: `PF_OUTCOME_UNSUPPORTED` (outcome is neither numeric nor a two-level factor — e.g. a 3-level factor; no model is available).

Warnings: `PF_LOW_EPV_10`, `PF_LOW_EPV_5`, `PF_MISSING_ANY`, `PF_MISSING_GT20`, `PF_MISSING_GT50`, `PF_RARE_FACTOR_LEVEL`, `PF_HIGH_CORRELATION`, `PF_FEW_CLUSTERS`, `PF_UNBALANCED_CLUSTERS` (both per cluster variable), `PF_CLUSTER_IDS_SHARED` (values of one cluster variable recur under several values of another — merged clusters if IDs are only unique within the parent; harmless if crossed), `PF_RARE_OUTCOME`, `PF_EXPOSURE_NOT_IN_MODEL`, `PF_SMALL_TEST_SET` (split: fewer than 100 events or non-events — binary — or 100 rows — continuous — among the complete test rows; Riley et al., 2021), `PF_LOOKS_CATEGORICAL` (a numeric outcome or predictor with ≤ 10 distinct whole-number values — e.g. a 0/1 flag or ASA class — will be modelled as continuous; the message points to Prepare › Transforms › Auto-factor. A 0/1 outcome gets a sharper message: linear regression will be fitted to it. Factoring is the user's responsibility; the app never converts silently).

Cluster variables enter the complete-case set only for mixed models.

Notes: `PF_SINGLE_COVARIATE` (incl. "No covariates selected — unadjusted model"), `PF_SPLIT_SUMMARY` (training and complete test row counts), `PF_SAMPLE_SUMMARY`, `PF_MODEL_SUMMARY`, `PF_DATA_STRUCTURE`, `PF_REFERENCE_LEVELS`.

**Passes:** `validate_analysis()` also returns `checks_run` and `passed` (one pass message per check that ran and raised nothing; stricter variants such as `PF_LOW_EPV_5` fold onto their head code). The Step 5 Summary lists them.

### A8.3 When Validation Runs

| Trigger | Tier | Display | Blocks |
|---|---|---|---|
| Step 3 pill entry | Tier 1 | Banner at top of pill | Run button if errors |
| Any `analysis_spec` change (live) | Tier 2 | Step 5 sidebar (errors + warnings); Summary tab (everything, incl. passes) | Run Model if errors |
| Run Model click | Tier 2 | If invalid: halt with notification. Otherwise fit — no warning modal. | Errors halt |

### A8.4 Step 5 Layout (Authoritative)

```
[ Summary ]  [ Run Model ]                      ← navset_underline

Summary tab — stacked cards, one per section:
  Data preparation · Analysis dataset · Study design and roles · Table 1 ·
  Variable investigation · Covariates and sample · Model · Preflight checks

Run Model tab — layout_sidebar(position = "left"):
  Sidebar: MODEL dropdown (+ reason for the choice)
           Advanced ▸ Optimizer            [mixed models only]
           PREFLIGHT (errors + warnings)   [pulses on disabled Run click]
           [ Run Model ]
  Main:    Model header — type · Modelling: ead = TRUE (vs FALSE) ·
           Fitted on: training set [split] · Formula
           Primary result | Coefficients | Fit statistics + Fitting notes
           ▸ R Code Preview                [placeholder until codegen]
```

### A8.5 Step 3 Tier 1 Validation Display

Single `uiOutput` banner at top of each vertical pill's main panel. Red text if errors, nothing if clean. Run buttons disabled when errors present.

### A8.6 `reset_analysis_pipeline` Function

**Service file:** `R/service_analysis_pipeline.R`

| Change | From step | Clears | Modal |
|---|---|---|---|
| Role assignment change | 1 | Table 1, var investigation, covariate selection, model, diagnostics, performance, results | Yes if downstream results exist |
| Train/test split change (Step 1) | 3 | Var investigation (incl. collinearity), model, diagnostics, performance, results — Table 1, covariate selection, `purpose_specification` kept | Yes if var investigation or a model exists |
| Covariate change in Step 4 | 4 | Model, diagnostics, performance, results | Yes if model run (first change only — the model is then gone) |
| New Step 5 model run | 4 | Previous model, diagnostics, performance, results (replaced by the new fit) | No |
| Step 5 setting change (optimizer) after a fit | 5 | Model, diagnostics, performance, results — `model_design` is kept (the new setting is written after the reset) | Yes |

The model type can no longer change on its own — it follows Step 1 roles, whose reset already clears the model. A role reset (1) leaves `purpose_specification` alone; it is Step 1's own setting.

Does NOT trigger: the model purpose alone, Table 1 changes, diagnostic or performance check changes, investigation reruns, export changes.

Confirmation modal lists only items that actually exist and will be cleared.

---

## A9 — Variable Selection Subsystem

**Service file:** `R/service_analysis_variable_selection.R`

### A9.1–A9.4 Purpose, Inputs, Outputs, Method Specifications

The subsystem is advisory. Three methods operate on the candidate pool from Step 1. Each produces a `variable_selection_result` with `method`, `parameters`, `suggested_variables`, `full_output`, `timestamp`. See §A7.6–A7.8 for statistical specifications.

### A9.5 Spec Storage

`analysis_spec$variable_selection_specification` stores all parameters: `univariable_p_threshold`, `stepwise_direction`, `stepwise_criterion`, `lasso_lambda`, `lasso_seed`, `selected_variables`. Defaults: `.default_variable_selection_specification()` (used by Setup and by a role reset).

### A9.6 Method Independence

Three methods are independent. Running one does not affect others. Results coexist. Rerun replaces only that method's result.

### A9.7 Step 4 Confirmation Table Integration

Column headers contain: method name + tooltip (?) with parameters, and Add / Replace buttons (`btn-outline-secondary btn-sm`, disabled if not run).

Cell highlighting: green (suggested), pink (not suggested), grey (not run / `n/a` excluded). Univariable cells show the smallest term p-value.

### A9.8 Add / Replace Behavior

**Add** checks the method's suggestions on top of the current selection — no modal. **Replace** swaps the selection for the method's list, after a modal listing what will be checked and unchecked.

### A9.9 Edge Cases

No candidates → disabled. Single candidate → valid. All excluded → modal warns. LASSO/stepwise failure → `tryCatch`. Univariable per-variable failure → NA row with note.

### A9.10 Results Storage

`analysis_result$variable_investigation` — three slots (univariable, stepwise, lasso), each NULL or `variable_selection_result`.

---

## A10 — Export and Output Specification

**Built 2026-10-05 (untested).** Export is no longer an Analyze step: it is the top-level **4 · Export** page, because it works without any Analyze work. The build plan, decisions and zip layout are in [BUILD_Export.md](BUILD_Export.md); this section is the summary. Mechanics: §N8.

**Files:** `R/module_export.R` (page), `R/service_export.R` (registry, writers, notes, build job), `R/service_export_report.R` (compiled report), `inst/www/edark_export.js` (the tree).

### A10.1 Overview

Export reads from `shared_state` and the cached `analysis_result`; nothing is recomputed (formatting only). It produces **materials** - outputs for publication and reproduction - in one zip. Saving and resuming work is a session file (§M8), though the zip carries one too.

Every file the zip could hold is listed from the start. A file can be ticked only when its source exists and is **current**: not-run and stale outputs are listed greyed, with the place that makes them, and are never exported. Staleness comes from `analysis_output_status()` (`service_analysis_models.R`): model-derived outputs are stale when the spec changed since the fit (`analysis_fit_is_stale()`); Performance also when its validation settings changed since the run; **every** Analyze output is stale when Prepare changed after Setup froze the dataset.

### A10.2 Zip Layout (supersedes §A3.8)

```
edark_export_YYYY-MM-DD_HHMMSS/
├── README.txt                    always: contents, dataset, model, renamed columns, files left out
├── analysis_report.docx | .html  compiled report (optional)
├── data/working_dataset.{rds,csv,sav,dta,xlsx}
├── reproduce/  session.edark.rds, prepare_steps.txt, analysis_script.R (coming soon)
├── table1/     table1_overall|by_exposure|by_outcome.docx, table1_notes.docx
├── variable_selection/  tables/ figures/ variable_selection_notes.docx
├── model/      tables/ (results_table, fit_statistics), figures/forest_plot.png,
│               methods.docx, model_notes.docx, analysis_result.rds (unticked by default)
├── diagnostics/  tables/ figures/ diagnostics_notes.docx
└── performance/  tables/ figures/ performance_notes.docx
```

Tables are Word (`.docx`, one per table). Figures are PNG at 300 dpi, 8 x 6 in unless the plot sizes itself. Each folder's **notes** document holds the numbers that are not a table or a figure (every diagnostic metric and message, validation settings and per-set measures, selection settings, the model summary), built from the same structures the screens render.

### A10.3 Data

The working dataset as Prepare left it - unmodified, no split column, no `.edark_row_id`. The train/test split is reproducible from the spec. SPSS and Stata need plain column names; renames are listed in `README.txt`.

### A10.4 Compiled Report

Word (bundled template, title page, TOC) or HTML (one self-contained file). Order: data preparation → analysis summary + methods → Table 1 → results → performance (before results for a prediction model) → Appendix A variable selection → Appendix B diagnostics. A section appears only when its source is available. PDF is deferred.

### A10.5 Not Built

Presets, PDF, the R script (Phase 5b), an analysis package with a manifest (dropped - the session file does that job), import of an export.


## A11 — Versioning and Deferral Log

### A11.1 Version 1 Scope

- Descriptive summary and Table 1
- Unadjusted group comparisons
- Linear regression, logistic regression, linear mixed model, logistic mixed model
- Stepwise variable selection (forward/backward, AIC/BIC)
- LASSO variable selection
- Model assumption diagnostics (residuals, linearity of continuous predictors, influence, VIF, convergence, separation, random effects)
- Model purpose (association / prediction) with an optional train/test split variable
- Performance step: ROC/AUC, calibration (with test-set calibration intercept and slope), predicted probabilities, prediction error — apparent and on a held-out test set
- Manuscript-ready tables and figures
- Methods paragraph auto-generation
- Export zip (4 · Export, §A10): data, session file, tables, figures, notes, compiled report
- Reproducible R script generation
- Wald-based CIs and p-values for all model types

### A11.2 Deferred Features

**High Priority — v1.5:** propensity score methods (PSM, IPTW, PS-adjusted). (The Prepare pipeline session bundle is now in scope as session save/load — §M8.)

**Mid Priority — v1.5:** analysis package import, additional models (GEE, multinomial, ordinal, Poisson, NB), marginal effects, LRT p-values for glmer (advanced option), profile likelihood CIs (advanced option), elastic net (LASSO alpha), customizable report builder, NNT/ARR, dynamic rounding.

**Performance validation — built (Phase 7b, 2026-09-19):** cross-validation and bootstrap optimism correction (§A5.3 Model › Performance). Deferred from it: shrunk coefficients (applying the bootstrap shrinkage factor) as an optional output; decision curve analysis; replaying data-driven variable selection inside resamples (by design not done — see §A1.4a).

**Also deferred for Diagnostics (Step 6):** cluster-level influence for mixed models (leave-one-cluster-out change in the fixed effects).

**Low Priority — v2+:** survival models, stratified models, saved runs, async progress, NRI, IDI, decision curves.

### A11.3 Out of Scope Permanently

Bayesian models, ML classifiers, mediation/SEM, multiple imputation, automated model selection, arbitrary R execution, spline modeling, causal forests/TMLE/doubly robust, Hosmer-Lemeshow test.

---

## A12 — Implementation Architecture

### A12.1 File Structure

```
R/
├── module_analysis_main.R                ← orchestrator
├── module_analysis_setup.R               ← Step 1
├── module_analysis_table1.R              ← Step 2
├── module_analysis_varinvestigation.R    ← Step 3
├── module_analysis_covariate_confirm.R   ← Step 4
├── module_analysis_modelspec.R           ← Step 5 (Model Creation)
├── module_analysis_diagnostics.R         ← Step 6
├── module_analysis_performance.R         ← Step 7
├── module_analysis_results.R             ← Step 8
├── module_export.R                       ← 4 · Export (top level, §A10)
├── service_analysis_models.R             ← model fitting engines
├── service_analysis_diagnostics.R        ← diagnostic computation
├── service_analysis_performance.R        ← performance computation (apparent, test set)
├── service_analysis_tables.R             ← gtsummary table generation
├── service_analysis_plots.R              ← ggplot figure generation
├── service_analysis_validation.R         ← preflight validator
├── service_analysis_summary.R            ← Step 5 Summary builder (audit of every step)
├── service_analysis_variable_selection.R ← univariable, stepwise, LASSO
├── service_analysis_codegen.R            ← R code generator
├── service_export.R                      ← export registry, writers, build job
├── service_export_report.R               ← compiled export report
├── service_analysis_pipeline.R           ← reset_analysis_pipeline()
├── analysis_utils.R                      ← formula assembly, reference levels, complete cases, train/test rows
├── module_session.R                      ← session save/load menu, autosave, resume (§M8)
└── service_session.R                     ← session build/read/migrate/reconcile (§M8)
```

### A12.2 Module ↔ Service File Mapping

| Module | Service files used |
|---|---|
| `module_analysis_setup.R` | `analysis_utils.R`, `service_analysis_pipeline.R` |
| `module_analysis_table1.R` | `service_analysis_tables.R` |
| `module_analysis_varinvestigation.R` | `service_analysis_variable_selection.R`, `service_analysis_validation.R` (Tier 1) |
| `module_analysis_covariate_confirm.R` | (reads from `analysis_result$variable_investigation`) |
| `module_analysis_modelspec.R` | `service_analysis_models.R`, `service_analysis_validation.R` (Tier 2), `service_analysis_summary.R`, `service_analysis_codegen.R` (deferred) |
| `module_analysis_diagnostics.R` | `service_analysis_diagnostics.R`, `service_analysis_plots.R` |
| `module_analysis_performance.R` | `service_analysis_performance.R`, `service_analysis_plots.R` |
| `module_analysis_results.R` | `service_analysis_tables.R`, `service_analysis_plots.R` |
| `module_analysis_export.R` | `service_analysis_export.R` |
| `module_session.R` | `service_session.R` |

### A12.3 Coding Conventions

All code follows: §M2–M5 (architecture rules, module convention, reactivity discipline), §N (implementation pitfalls), [NOTE_UI-principles.md](NOTE_UI-principles.md) (layout, actions, hierarchy), and §A0 (`magrittr` `%>%`, no base R pipe).

### A12.4 Phased Build Plan

The implementation is organized into sequential phases, specified in [BUILD_Analysis.md](BUILD_Analysis.md) — phase definitions, acceptance criteria, "do not touch" lists, test datasets, and prompt briefs.

---


---

## A13 — Session Save and Load

Moved to §M8 (it spans Prepare and Analyze).
