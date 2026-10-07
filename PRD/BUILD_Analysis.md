# EDARK Analysis Module — Phased Build Plan
## Implementation Sequence for Claude Code

**Reference document:** the Analyze PRD. Section references (§) below use the old pre-split numbering; add the `A` prefix to find them (§5.3 = §A5.3). Session references (§13.x) are now §M8.x in the Master PRD.

**Step numbering (changed 2026-09-19):** the workflow now has **nine** steps. Step 5 was renamed **Model Creation**; a new **Step 7 Performance** was inserted (Phase 6b), so Results became **Step 8** and Export **Step 9**. Phase numbers are unchanged — Phase 7 builds Step 8, Phase 8 builds Step 9. Text written before the change (Phases 0–7 "as built" and "original plan" notes) keeps the step numbers of its time unless marked.

**Critical rule for all phases:** use `magrittr` `%>%` exclusively. No base R pipe `|>`.

---

## Phase 0 — Infrastructure and Scaffold

### What to build
1. Add all packages from the PRD Global Mandates table to the DESCRIPTION file under `Imports:`
2. Create all file stubs listed in §12.1 — empty module shells (`ui` + `server` functions that return placeholder UI) and empty service files with roxygen headers
3. Create `module_analysis_main.R` — the orchestrator:
   - `analysis_main_ui(id)`: returns `bslib::navset_pill` with 8 `nav_panel` stubs, each rendering a placeholder card with the step name
   - `analysis_main_server(id, shared_state)`: initializes `shared_state$analysis_data`, `shared_state$analysis_spec`, `shared_state$analysis_result` as NULL; calls each step module's server function
4. Wire `module_analysis_main` into `edark.R` as Tab 4 ("Analyze") in the `page_navbar`
5. Create `analysis_utils.R` with three functions:
   - `build_analysis_formula(spec)` — assembles formula string from `analysis_spec$variable_roles`
   - `apply_reference_levels(data, reference_levels)` — applies `relevel()` per spec
   - `compute_complete_cases(data, variables)` — returns filtered data + exclusion count
6. Create `service_analysis_pipeline.R` with:
   - `reset_analysis_pipeline(shared_state, from_step)` — clears downstream fields per §8.6
7. Create `service_analysis_validation.R` with:
   - `validate_analysis(spec, data, tier = "full", verbose = FALSE)` — implements all checks from §8.2, both tiers

### What NOT to touch
- Any existing module files (module_prepare_*, module_explore_*, module_report.R)
- Any existing service files
- The existing `shared_state` fields — only ADD the three new analysis fields

### Acceptance criteria
- `devtools::check()` passes with 0 errors, 0 warnings
- App launches, Tab 4 appears with 8 labeled placeholder pills
- `shared_state$analysis_data`, `analysis_spec`, `analysis_result` are initialized as NULL
- `build_analysis_formula()` produces correct formula strings for all model types (test with mock spec)
- `validate_analysis()` correctly returns errors, warnings, notes for each check (test with mock data)
- `reset_analysis_pipeline()` correctly nulls downstream fields (test with mock shared_state)

### PRD references
- §3.3 (shared_state fields), §3.5 (analysis_spec structure), §3.6 (analysis_result structure)
- §5.1 (layout overview), §12.1 (file structure)
- §7.1 (formula assembly, reference levels, complete cases)
- §8.1–8.2 (validator architecture and checks)
- §8.6 (reset_analysis_pipeline)
- Global Mandates (DESCRIPTION Imports)

---

## Phase 1 — Step 1: Setup

### What to build
- `module_analysis_setup.R` — full implementation per §5.3 Step 1 and §6.3
- "Start Analysis" button → dataset freeze → `.edark_row_id` → signature computation
- Role assignment table (DT::datatable with custom renderers)
- Column header Clear buttons for radio columns
- Mutual exclusivity enforcement
- Sidebar role summary with study type badge
- Reactive spec population (no confirm button — live updates)
- Integration with `reset_analysis_pipeline()` — role changes with downstream results trigger confirmation modal

### What NOT to touch
- Steps 2–8 module files (leave as placeholders)
- Service files other than `analysis_utils.R` and `service_analysis_pipeline.R`

### Acceptance criteria
- "Start Analysis" freezes dataset correctly with `.edark_row_id`
- All role columns work: outcome, exposure (single-select), candidate, cluster (multi-select). *(Subject ID and time roles were later removed — see PRD §5.3 Step 1.)*
- Clear buttons deselect radio columns
- Mutual exclusivity: assigning outcome unchecks candidate, etc.
- Reference level dropdowns populate with R factor levels in correct order
- Sidebar updates reactively showing roles and study type
- Study type badge derives correctly from all four combinations (§1.4)
- `analysis_spec$variable_roles` populates correctly
- Navigating to Step 2+ shows placeholder pills

### PRD references
- §1.4 (study type), §3.4 (freeze behavior), §3.5 (spec structure)
- §5.3 Step 1, §6.3

---

## Phase 2 — Step 2: Table 1

### What to build
- `module_analysis_table1.R` — full implementation per §5.3 Step 2 and §6.4
- `service_analysis_tables.R` — Table 1 generation via `gtsummary::tbl_summary()`
- Stratification checkboxes (By Exposure, By Outcome) with defaults per §3.7
- P-value toggle, SMD toggle
- Overall + conditional tabs in `navset_card_tab`
- Blocking modal progress on generate
- Results cached in `analysis_result$result_tables$table1_*`

### What NOT to touch
- Steps 3–8 module files
- Any service file other than `service_analysis_tables.R`

### Acceptance criteria
- Table 1 generates for all stratification combinations
- Overall tab always present
- By Exposure / By Outcome tabs appear/disappear based on checkboxes and role assignments
- P-value toggle works; SMD toggle works
- Table renders as `gtsummary` object in main panel
- Blocking modal shows during generation
- Cached correctly in `analysis_result`

### PRD references
- §3.7 (Table 1 behavior by study type), §5.3 Step 2, §6.4

---

## Phase 3 — Step 3: Variable Investigation

### What to build
- `module_analysis_varinvestigation.R` — full implementation per §5.3 Step 3 and §6.5
- `service_analysis_variable_selection.R` — all three methods per §7.6–7.8 and §9.4
- Vertical `navset_pill` with three pills
- Univariable Screen: p-threshold input + run button + results table + Tier 1 banner
- Collinearity: auto-computed heatmap + Cramér's V + flagged pairs
- Stepwise/LASSO: toggle + configs + run buttons + outputs
- Results stored in `analysis_result$variable_investigation`

### What NOT to touch
- Steps 4–8 module files
- Service files other than `service_analysis_variable_selection.R`

### Acceptance criteria
- Univariable screen runs lm for continuous outcome, glm for binary
- Results table: variable, estimate, CI, p-value, sorted by p-value
- P-value threshold filters suggested list correctly
- Collinearity heatmap renders on pill entry without button click
- Flagged pairs correctly identified at > 0.7 threshold
- Stepwise runs with backward/forward and BIC/AIC
- LASSO runs with lambda.1se/lambda.min; handles factor variables correctly
- Toggle preserves state between Stepwise and LASSO
- All results stored in `analysis_result$variable_investigation`
- Tier 1 validation banner shows errors when applicable
- Blocking modals for all run buttons

### PRD references
- §5.3 Step 3, §6.5, §7.6–7.8, §9.1–9.6, §8.5

---

## Phase 4 — Step 4: Covariate Confirmation — ✅ COMPLETE

Built with agreed deviations from the original plan (the PRD is updated to match):
candidates start **unchecked**; there is **no Confirm button and no pending state** —
every change is written to the spec immediately; "Import" became **Add** (adds checks,
no modal) and **Replace** (swaps the selection, modal); cluster variables are locked rows.

### What was built
- `module_analysis_covariate_confirm.R` per §5.3 Step 4, §6.6, §9.7–9.8
- Table: Include, Variable, Type, Missing, Row cost, Univariable / Stepwise / LASSO (Add / Replace + parameter tooltip in each header), Reference level
- Right sidebar: live Model / Sample counts ("If mixed model" row count when clusters exist) and Checks
- Live commit of `final_model_covariates`, `reference_levels` (effective levels), `variable_selection_specification$selected_variables`
- First change after a fit → "Clear Model Results?" modal; Cancel undoes the click
- `compute_covariate_sample()` in `analysis_utils.R`

### Acceptance criteria (as built)
- All candidates unchecked by default; role variables locked and checked at the top
- Suggestion columns: green ✓ suggested (univariable shows smallest p), pink — not suggested, grey not run / `n/a` excluded
- Add / Replace disabled if the method has not run; Replace shows a modal listing what will be checked / unchecked
- Every change is written to the spec immediately; reference levels fall back to the first surviving level when the preferred one drops out
- A Step 1 role change resets the selection; a Step 3 rerun only refreshes the suggestion columns
- With a fitted model, the first change asks before clearing it; Cancel restores the table

### PRD references
- §5.3 Step 4, §6.6, §9.7–9.8

---

## Phase 5 — Step 5: Model Specification + Preflight + Model Fitting — ✅ COMPLETE

Built with agreed deviations (the PRD is updated to match): **two tabs** (Summary + Run
Model) instead of stacked accordions; the **model type is automatic** (one valid type);
preflight is **live** (no Run Preflight button, no verbose checkbox); **no warning modal**;
optimizer change → **stale** results instead of a reset; the **R code generator was
deferred** to Phase 5b (built 2026-10-06).

### What was built
- `module_analysis_modelspec.R` per §5.3 Step 5, §6.7, §8.4
- `service_analysis_models.R` — `fit_analysis_model()` (lm, glm, `lmerTest::lmer`, `lme4::glmer`), `analysis_model_options()`, `analysis_outcome_type()`, `analysis_outcome_event()`, `analysis_fit_is_stale()`
- `service_analysis_summary.R` — `build_analysis_summary()` (pure; reusable by Phases 7–8)
- `service_analysis_validation.R` — added `PF_OUTCOME_UNSUPPORTED`, `PF_LOOKS_CATEGORICAL`, and `checks_run` / `passed`
- Step 1 freeze now stores `specification_metadata$prepare_snapshot`

### Acceptance criteria (as built)
- All four model types fit correctly; all three optimizers (bobyqa, Nelder_Mead, nlminbwrap) run
- Model dropdown lists all four types; the valid one is selected and written to the spec; the others are disabled with the reason in their label
- Optimizer (Advanced accordion) appears only for mixed models
- Live preflight: sidebar shows errors + warnings; Summary tab lists every check including passes
- Preflight errors (or no valid model) disable Run Model with an inline message; clicking it pulses the preflight box
- Run Model fits in the blocking modal; results show Primary result (exposure per level vs reference), Coefficients, Fit statistics, Fitting notes
- "Modelling: outcome = event (vs reference)" shown for binary outcomes
- Optimizer change after a fit shows the stale banner; a failed fit shows the error and stores no model
- Step 6–8 unlock once `fitted_models$primary_model` exists
- Summary tab covers: data preparation, dataset, roles, Table 1, variable investigation, covariates and sample, model, preflight checks

### PRD references
- §5.3 Step 5, §6.7, §7.1–7.5, §8.1–8.4

---

## Phase 5b — R Code Generator — ✅ COMPLETE (2026-10-06, branch `r-code-gen-v1`)

Built with decisions taken 2026-10-06 that change the original plan:
- **The script covers what was run, and is current** - not "all three selection methods,
  unrun ones commented out" or "every diagnostic for the model type". Model purpose,
  validation and performance are included. Stale steps are left out with a message.
- **Seeds are the ones the run used** (LASSO, cross-validation, bootstrap).
- **It lives on 4 · Export › R Code**, a pill beside Content, and is **generated live** when
  shown - not cached in `analysis_result$generated_r_script`. The Model › Create "R Code
  Preview" placeholder is gone.

### What was built
- `service_analysis_codegen.R` - `generate_analysis_script(st, opts)`, pure, from the export state snapshot
- `inst/codegen/helpers.R` - copies of the EDARK functions that decide numbers, one chunk each; the script carries only the chunks it uses
- `module_export.R` - `export_page_ui()` (Content / R Code pills), `export_code_ui()`, the R Code server part; the zip's `reproduce/analysis_script.R` is now available
- `tests/testthat/test-service_analysis_codegen.R` (fast) and `tests/manual/codegen/` - `run_check.R` (script vs app, end to end), `ui_check.R` (the page in headless Chrome)

### Acceptance criteria (as built)
- Script uses `pacman::p_load()` (only the packages it needs), `%>%`, RSPM repos; plain ASCII; it parses
- Rebuilds the working dataset from the input dataset (launch casts + Prepare) - identical to the app's and to the exported `data/working_dataset.rds`
- Rebuilds the model data exactly as `fit_analysis_model()` does
- **Every number matches the app** - Table 1, univariable screen, stepwise / LASSO selection and lambda, collinearity matrices, coefficients and fit statistics, diagnostics, performance (apparent, test set, CV mean and SD, bootstrap optimism), unadjusted estimates - for all four model types and all four validation methods: 7 scenarios, 280+ comparisons, all differences 0 (`run_check.R`, against the exported `model/analysis_result.rds`)
- The R Code pill shows the script on entry, regenerates on an option change, Copy and Download Script work (`ui_check.R`)

### PRD references
- §A7.9, §A10.6, §N6.14

---

## Phase 6 — Step 6: Diagnostics — ✅ COMPLETE

> **Superseded in part by Phase 6b:** the optional Prediction Performance group moved out of Step 6 into the new Step 7 Performance, and Step 6 gained a Linearity check.

Built with agreed deviations (the PRD is updated to match): **left** sidebar with the check
lists and Run button, and an **Overview tab** as the at-a-glance summary (instead of a right
summary sidebar); **Prediction Performance for all model types** (linear: observed vs
predicted, RMSE / MAE / R²), collapsed and optional, apparent performance only;
**binned residuals** for logistic models; sample accounting and fitting warnings always
included (no checkbox); diagnostics are advisory and never gate Steps 7–8. Also: changing the
optimizer after a fit now clears the model (modal) via `reset_analysis_pipeline(from_step = 5)`,
which also clears `analysis_result$diagnostics`.

### What was built
- `service_analysis_diagnostics.R` — `analysis_diagnostic_options(model_type)`, `run_analysis_diagnostics(result, data, checks, progress_fn)` (pure)
- `service_analysis_plots.R` — all diagnostic plots (ggplot, shared `.ap_theme()`)
- `module_analysis_diagnostics.R` — sidebar, blocking modal, Overview + per-check tabs
- Stored in `analysis_result$diagnostics`, `result_plots$diagnostic_plots`, `result_tables$diagnostic_summary` (long metrics table), `inference_summary$influence_measures`

### Original plan

#### What to build
- `module_analysis_diagnostics.R` — full implementation per §5.3 Step 6 and §6.8
- `service_analysis_diagnostics.R` — all diagnostic computations per §7.2–7.5
- `service_analysis_plots.R` — all diagnostic plot generation
- Two-category config (Model Assumptions / Prediction Performance)
- Run button + blocking modal
- `navset_card_tab` output tabs conditional on model type and selections
- At-a-glance sidebar summary with conditional sections

### Inputs from Phase 5
- `analysis_result$fitted_models$primary_model`, `inference_summary$predicted_values` (`.edark_row_id`, `.fitted`, `.resid`, and `.fitted_marginal` for logistic mixed), `run_status` (incl. fitting warnings)
- Mixed models have one random intercept per `variable_roles$cluster_variables` entry — random-effects Q-Q, ICC and cluster-size outputs must handle several grouping factors

### What NOT to touch
- Steps 7–8 module files
- `service_analysis_models.R` (already complete)

### Acceptance criteria
- Model Assumptions all checked by default; Prediction Performance all unchecked
- Prediction Performance only shown for logistic models
- Select all / Deselect all work per category
- All diagnostics per model type compute and render correctly:
  - Linear: residuals, Q-Q, scale-location, Breusch-Pagan, Cook's D, leverage, VIF
  - Logistic: Cook's D, VIF, separation; (if checked) ROC/AUC, calibration, predicted probs
  - Linear mixed: residuals (conditional), Q-Q, random effects Q-Q, ICC, cluster size, VIF, convergence
  - Logistic mixed: random effects Q-Q, ICC, cluster size, VIF, convergence; (if checked) marginal ROC/AUC, marginal calibration, marginal predicted probs
- Sidebar summary populates with raw values, conditional sections
- Prediction Performance section in sidebar only if computed

### PRD references
- §5.3 Step 6, §6.8, §7.2–7.5 (diagnostics sections)

---

## Phase 6b — Model Purpose, Train/Test Split, Step 7 Performance — ✅ COMPLETE (2026-09-19)

Separates the two goals that Step 6 used to mix: **checking the model's assumptions** (Step 6) and **evaluating how well it predicts** (new Step 7). PRD §A1.4a, §A5.3 Steps 1 / 6 / 7, §A8.2, §A8.6.

### What was built
- **Renumbering:** nine steps; Step 5 renamed "Model Creation" (file `module_analysis_modelspec.R` kept); Results → Step 8, Export → Step 9; gating `step6`–`step9` open once a model is fitted.
- **Model purpose (Step 1 sidebar):** Association (default) / Prediction. Prediction reveals "Train/test set variable" → Variable (factor, ≥ 2 levels, no role) + Training level. Stored in `analysis_spec$purpose_specification` (`model_purpose`, `use_split`, `split_variable`, `training_level`; `.default_purpose_specification()`). *(Phase 7b replaced `use_split` with `validation_method`.)*
- **Split helpers** (`analysis_utils.R`): `analysis_split()`, `analysis_split_rows()`, `analysis_model_data()`, `analysis_test_data()`. With a split, Steps 3–6 and preflight use the training rows only; Table 1 uses all rows.
- **Reset:** `reset_analysis_pipeline(from_step = 3)` for a change of training rows — clears variable investigation (incl. collinearity), model, diagnostics, performance, results; keeps Table 1 and the covariate selection. Asked via "Clear Analysis Results?" when variable investigation or a model exists; Cancel restores the inputs. The purpose alone changes nothing. Resets 4/5 also clear performance.
- **Preflight:** `PF_SPLIT_INVALID` (error), `PF_SPLIT_NO_TEST`, `PF_SPLIT_MISSING`, `PF_SMALL_TEST_SET` (warnings), `PF_SPLIT_SUMMARY` (note); all other checks run on the training rows.
- **Model fit:** `fit_analysis_model()` fits on the training rows; data preparation factored into `.prepare_model_rows()` so the test set is coded identically. `analysis_fit_is_stale()` includes the split.
- **Step 7 Performance:** `service_analysis_performance.R` (`analysis_performance_options()`, `run_analysis_performance()`) and `module_analysis_performance.R`. Sets: apparent (always) and test (split). Logistic: ROC/AUC (DeLong CI), calibration deciles + Brier + no-information Brier, predicted probabilities; linear: RMSE / MAE / R², observed vs predicted. Test set adds calibration intercept and slope. Mixed: marginal predictions. Unseen factor levels in the test set are dropped with a warning. Stored in `analysis_result$performance`, `result_plots$performance_plots`, `result_tables$performance_summary`.
- **Step 6 Diagnostics:** prediction group removed; new **Linearity** check (residuals vs each continuous predictor; binned residuals per predictor for logistic models).
- **Step 8 Results / Summary / methods:** fit statistics show AUC for each set; the Summary shows model purpose, split and training-row counts; the methods paragraph describes the split, the assumption checks and the performance measures.

### Diagnostics review (per model type) — findings
| Model | Covered | Gap |
|---|---|---|
| Linear | residuals, Q-Q, scale-location, Breusch-Pagan, influence, VIF, **linearity (new)** | — |
| Logistic | binned residuals, influence, VIF, separation, **linearity on the log-odds scale (new)** | — (Hosmer-Lemeshow is out of scope, §A11.3; calibration is in Step 7) |
| Linear mixed | conditional residual plots, VIF, random effects (variance, ICC, Q-Q, sizes), singular / convergence, **linearity (new)** | No influence check — cluster-level (leave-one-cluster-out) influence deferred |
| Logistic mixed | binned residuals, VIF, separation (fixed effects), random effects (latent-scale ICC), singular / convergence, **linearity (new)** | Same influence gap |

### Acceptance criteria (as built — verified 2026-09-19)
- All four model types fit, diagnose and evaluate with and without a split on `liver_tx` (plus a random `cohort` factor); every plot builds.
- Split validation: split variable holding a role, a numeric split variable, and a missing training level each give `PF_SPLIT_INVALID`; rows missing the split variable give `PF_SPLIT_MISSING`.
- A test-only factor level is dropped from the test set with a warning naming the level.
- `reset_analysis_pipeline(3)` keeps Table 1 and clears investigation, model and performance; `analysis_fit_is_stale()` is TRUE after a split change.
- In the browser (`chromote`): nine pills; Step 1 purpose controls; Step 5 header shows the training set; Step 7 shows apparent vs test; Step 8 Summary lists both AUCs; changing the training level after results opens the modal and Cancel restores the select. No new console errors.

---

## Phase 7 — Step 8: Results (built as "Step 7") — ✅ COMPLETE

Built per the decisions below (PRD §5.3 Step 7 and §6.9 updated to match). Deviation from the
original plan: the results table is **built by EDARK, not `gtsummary::tbl_regression()`** —
that needs the broom.helpers package and would re-process our numbers; one data.frame now feeds
both the gt table (app) and the flextable (Word export).

### What was built
- `service_analysis_models.R` — `fit_unadjusted_models(result)` (one model per variable, model's own rows, same engine)
- `service_analysis_tables.R` — `build_results_table()`, `results_table_gt()`, `results_table_flextable()`, `build_fit_statistics_table()`
- `service_analysis_plots.R` — `build_forest_plot()` (patchwork: labels | CIs | OR (95% CI) and p)
- `service_analysis_summary.R` — `build_methods_paragraph()` (with R / EDARK / package versions)
- `stats_inference.R` — `edark_format_est()` / `edark_format_ci()` (one number format; "to" when a limit is negative)
- `module_analysis_results.R` — outputs catalogue `.RESULTS_OUTPUTS`, Generate, Summary + per-output tabs
- `reset_analysis_pipeline()` (steps 4/5) also clears `fit_statistics`, `univariable_models`, `results_generation`

### Decisions (agreed 2026-09-18, before build)
- ✅ **Done ahead of Phase 7 — one statistics layer app-wide** (`R/stats_inference.R`, PRD §4.2, CLAUDE.md "Statistical methods registry"): Wald-type CIs whose critical value matches the p-value (t / Satterthwaite t / z); Step 3's univariable screen and Step 5 use the same `edark_coef_table()`; Table 1 and the Report use the same group tests (`edark_group_test()`); one p-value format (`edark_format_p()`); one footnote sentence per model type (`edark_inference_note()`). Step 7 must use these — no new CI/p code.
- **Results table rows:** exposure + final covariates only (not unchosen Step 3 candidates), so every cell is filled. Two column groups — Unadjusted and Adjusted — each with estimate (95% CI) and p-value. No intercept. Random effects are **not** in this table (Fit statistics only).
- **Summary tab:** key numbers only, no prose.
- **Forest plot:** every model term (exposure + all covariates), adjusted estimates.
- **Methods paragraph:** no variable-selection description; include R and package versions.
- **Output selection:** the checkboxes decide what is **generated**; unticked outputs are not created and cannot be exported in Step 8. More outputs will be added to the list later.
- **Forest plot** shows OR (95% CI) and p beside each row. Scaling continuous variables is the user's responsibility.
- **Unadjusted fits with a warning** are shown with † and a footnote; a failed fit shows "—" with ‡.
- **Results table built by EDARK** (not gtsummary): no broom.helpers dependency, and no re-processing of our numbers.
- **Unadjusted column is refit in Step 7** (not taken from Step 3): one model per variable (exposure + final covariates), on the **final model's complete-case sample** so both columns share one n, with the **same engine** as the final model — mixed models get univariable mixed models with the same cluster intercepts.
- **Summary tab** is a reactive render of what `analysis_result` already holds (no computation, no checkbox).
- Footnote text lives in PRD §4.2 (the Step 7 cross-reference pointed at itself).

### Original plan

#### What to build
- `module_analysis_results.R` — full implementation per §5.3 Step 7 and §6.9
- Extend `service_analysis_tables.R` — results tables, combined table via `tbl_merge()`, methods paragraph generation
- Extend `service_analysis_plots.R` — forest plot
- Sidebar: output checkboxes, generate button
- Summary tab (auto-generated on model run)
- Results Table tab (combined univariable + multivariable default)
- Fit Statistics tab
- Forest Plot tab
- Methods tab (selectable plain text)
- Methods paragraph cached in `analysis_result$methods_paragraph` — build it from `build_analysis_summary()` sections (Phase 5) rather than re-deriving the same facts
- gtsummary tables (`tbl_regression`) from the fitted model — Phase 5 stores only the plain coefficient data.frame

### What NOT to touch
- Step 8 module file
- `service_analysis_models.R`, `service_analysis_diagnostics.R`

### Acceptance criteria
- Summary tab appears immediately after model run without clicking generate
- Results table shows combined format with correct footnotes per model type
- Variables excluded from multivariable show "—" in adjusted columns
- Exposure row bolded, factor reference levels in italics
- Fit statistics table correct per model type
- Forest plot renders with exposure highlighted, null reference line
- Methods paragraph text is accurate for each model type
- Methods text uses correct p-value method and CI method language
- All outputs cached in `analysis_result`

### PRD references
- §5.3 Step 7, §6.9, §7.2–7.5 (fit statistics), §7.6 (combined table)

---

## Phase 7b — Model › Performance: Validation (cross-validation, bootstrap) — ✅ COMPLETE (as built 2026-09-19)

Extends Phase 6b's train/test option into internal validation. PRD §A1.4a, §A5.3 Model › Performance.

### As built
- **Step 1:** Prediction purpose → **Validation** radio: Bootstrap (default) / Cross-validation / Held-out test set (variable + training level). Mutually exclusive — a held-out set is data kept apart on purpose (e.g. other centres); resampling is for a single homogeneous sample. No random single split is offered. `purpose_specification$use_split` was replaced by `validation_method` ("bootstrap" / "cv" / "split"); `analysis_split()` applies for "split" only.
- **Settings** in the Performance sidebar (only for the chosen method): folds + repeats (CV), resamples (bootstrap), seed. Written live to `analysis_spec$validation_settings` (`.default_validation_settings()`); `analysis_validation(spec, mixed)` returns the method with defaults filled (bootstrap: 200, mixed 100).
- **Same covariates in every resample** — Step 3 selection is *not* replayed (by design; EDARK discourages automated selection). Stated in Step 1, the Performance notes and the methods paragraph.
- `service_analysis_performance.R`: `analysis_performance_job()` / `.perf_job_step()` / `.perf_job_finish()` (steps, so the app can cancel) and `run_analysis_performance()` (all at once). Sets `cv` (pooled out-of-fold predictions per repeat, averaged; SD across repeats; stratified by outcome, or grouped by the first cluster variable for mixed models) and `bootstrap` (Harrell optimism for AUC, Brier, calibration intercept / slope, RMSE, MAE, R²; `optimism` table; lowess calibration curve apparent vs bias-corrected; cluster bootstrap for mixed models). Refits via `.fit_engine()` (shared with the primary fit and the unadjusted models).
- Module: refits run ~0.4 s per reactive tick (`invalidateLater`), so the progress modal's **Cancel** works. Mixed models with > 100 refits ask first (Back / Proceed).
- Results: fit statistics and the Summary card show validated AUC and calibration slope; the methods paragraph describes the method, settings and seed, and notes that the coefficients are those of the model fitted to all rows.

### Acceptance criteria (verified on `liver_tx`, 2026-09-19)
- Logistic `ead ~ preop_meld + donor_age + recipient_age`: apparent AUC 0.617 → bootstrap-corrected 0.603 (slope 0.90–0.92), CV 0.593 (slope 0.69). ✅
- Grouped CV folds hold whole clusters; the cluster bootstrap resamples whole clusters. ✅
- Same seed → identical numbers; the seed is in the spec, the Summary and the methods paragraph. ✅
- A held-out set cannot be combined with resampling, so it is never used in any resample. ✅
- Overview table gains a column per set; fit statistics and methods paragraph describe the validation used. ✅
- Cancel stops a 2,000-resample run within a tick and keeps the previous results. ✅

### Not built (deferred)
- Shrunk coefficients (bootstrap shrinkage factor) as an optional output; decision curve analysis; replaying data-driven selection inside resamples.

### PRD references
- §A1.4a, §A5.3 Step 7, §A11.2

---

## Phase 8 — Step 9: Export

> **Superseded 2026-10-05 - moved to [BUILD_Export.md](BUILD_Export.md).** Export became the
> top-level 4 · Export page; presets, the analysis package / manifest and xlsx tables were
> dropped or deferred there. The plan below is kept for history only.

### What to build
- `module_analysis_export.R` — full implementation per §5.3 Step 9 and §6.11
- `service_analysis_export.R` — complete export assembly pipeline per §10.3–10.12
- Preset selector + item checklists with step-of-origin subheadings
- Self-contained analysis package checkbox
- Report generation via `officer` (Word) and `rmarkdown` (HTML) per §10.7
- Live zip preview
- Download button with validation
- Blocking modal with per-step progress
- Step 9 reachable once the dataset is frozen (change its gating in `module_analysis_main.R`); every item listed from the start, disabled with a "created in Step N" tooltip until its source exists in `analysis_result`
- Performance items (Step 7): the performance summary table (one column per set of rows) and its figures, per set
- Analysis dataset is an optional item

### What NOT to touch
- All other module and service files (complete at this point), except the Step 9 gating in `module_analysis_main.R`

### Acceptance criteria
- Each preset pre-checks correct items
- Custom Export reverts on manual change after preset
- Self-contained package: disables standalone R script/spec checkboxes, forces data export
- All table files open in Word: table1_overall.docx, multivariable_results.docx, etc.
- All figure files render: forest_plot.png, residuals_vs_fitted.png, roc_curve.png, etc.
- All data formats: RDS loads in R, CSV parses, .sav opens in SPSS, .dta opens in Stata, .xlsx opens in Excel
- R script is executable and reproduces analysis (requires Phase 5b)
- Analysis spec JSON is valid and readable
- Analysis spec RDS loads correctly
- Report contains correct sections, embedded tables and figures
- Analysis package zip contains all required files with valid manifest.json
- Zip preview updates reactively
- Blocking modal shows per-step progress
- Export folder structure matches §10.2 exactly
- Items for outputs not yet created are visible but disabled, and become selectable once created

### PRD references
- §5.3 Step 9, §6.11, §10.1–10.12

---

## Phase S — Session Save and Load

**Status: built 2026-09-28** on branch `session-manager-v1` (S1-S4 and the launch argument). Autosave (S5b) is deferred. Not yet verified in R or the browser - see "Verification" below.

**Independent of Phases 7-8** - touches Prepare, Step 1 and Step 4. Spec: §M8.

### What was built

**S1 - Service layer** (`service_session.R`)
- `dataset_definition()` (input classes), `dataset_signature()`, `build_session()`, `read_session()` / `validate_session()`, `.SESSION_SCHEMA_VERSION`, `session_dataset_mismatch()` (exact match, §M8.4), `session_prepare_dataset()` (refuses rather than adjusts, §M8.6), thumbnail pack / unpack
- No migration framework: pre-release, the schema changes in place (§M8.5)
- `testthat` unit tests: `tests/testthat/test-service_session.R`

**S2 - Save** (`module_session.R`)
- Session navbar menu, save modal with the "Include dataset" checkbox, `downloadHandler`

**S3 - Load: Prepare and custom report**
- Load modal, checks, confirm modal (with the custom-report warning), steps 3-4 of §M8.7
- One shared Apply commit: `.run_prepare_apply()` / `.commit_working_dataset()` in `module_prepare_confirm.R`, used by the Apply button, the tab-switch auto-apply in `edark.R`, and load

**S4 - Load: Analyze**
- `shared_state$session_restore` payload
- `module_analysis_setup.R`: take the payload in - freeze, apply roles and reference levels, purpose and validation settings; or unfreeze when the session has no analysis
- `module_analysis_covariate_confirm.R`: take the covariates on a `roles_key` change when the roles match
- Navigate and show the toast

**S5a - Launch argument**
- `edark(session = )` per §M8.8; a mismatch stops in the console before the app starts

**S5b - Autosave (deferred)**
- Writer, file limit and resume prompt per §M8.9, once the cost of writing on every change is measured

### What NOT to touch
- Steps 2, 3, 5-6 module and service files
- Model fitting, validation, and reset logic (`service_analysis_models.R`, `service_analysis_validation.R`, `service_analysis_pipeline.R`)

### Acceptance criteria
- Round trip on `liver_tx` with transforms, filters, roles, clusters, a prediction purpose with non-default validation settings, covariates and custom report items: after loading, every Prepare tab, the Step 1 table and purpose inputs, the Step 4 checkboxes and the Report › Custom list (with thumbnails) show the saved state; the Step 5 preflight equals the preflight before saving; nothing has been fitted
- A session saved with nothing checked in Step 4 restores roles, and Step 4 opens with nothing checked
- A session saved before Start Analysis restores Prepare only, and unfreezes an analysis that was frozen
- Loading onto `liver_tx[1:300, ]` succeeds (same columns and types, fewer rows)
- Loading onto `liver_tx` with a column dropped, added or retyped is refused with the reasons; nothing changes
- Loading onto an unrelated dataset (e.g. `mtcars`) is refused
- A newer-schema file is refused with the update message
- Data in a session file is used only by `edark(session = )` with no dataset argument, never by an in-app load
- Verified in the browser with `chromote`, not only `testServer` (the Step 1 and Step 4 tables are patched client-side)
- `devtools::check()` passes with 0 errors, 0 warnings

### Verification
Written without an R installation. Still to do on a desktop: `devtools::document()` (new internal Rd files), `devtools::test()`, `devtools::check()`, and a `chromote` run through the acceptance criteria.

### PRD references
- §M8.1-M8.10, §A10 (materials vs session)

---

## Phase 9 — Integration, Polish, and Testing

### What to build
1. Verify `reset_analysis_pipeline()` wiring end to end (already wired: Step 1 role changes, Step 4 first change after a fit, Step 5 new run)
2. Implement stale state propagation per §5.4 — step status indicators update correctly
3. Step status indicators in pill labels (not started / in progress / complete / stale)
4. Consistent blocking modal pattern across all run buttons
5. Edge case handling: empty datasets, single variable, single cluster, zero events, all missing
6. Refactor existing `withProgress` toast in `module_report.R` to blocking modal (the global mandate)

### Test datasets

**Dataset 1 — Full-featured clustered dataset** (for testing all model types):
- ~500 observations across ~40 clusters (patient IDs), nested in a handful of sites — exercises two cluster variables
- ~8–15 observations per cluster
- Binary outcome (event rate ~15%)
- Continuous outcome (e.g. creatinine)
- Binary exposure
- Mix of numeric covariates (age, BMI, lab values) and factor covariates (ASA class, sex, site)
- Reference levels intentionally non-alphabetical for at least one factor
- ~5% missingness scattered across variables
- At least one factor with a rare level (< 5 observations)
- At least one pair of covariates with correlation > 0.7

**Dataset 2 — Sparse clustering dataset** (for testing mixed model warnings):
- Same structure but most IDs have only 1–2 observations
- < 10 clusters
- Should trigger `PF_FEW_CLUSTERS` warning and near-zero ICC post-fit

Both datasets should be generated as R scripts in `inst/test_data/` and documented.

### Acceptance criteria
- Full end-to-end workflow: Setup → Table 1 → Variable Investigation → Covariate Confirmation → Model Creation → Diagnostics → Performance → Results → Export produces correct zip, for an association model and for a prediction model with a train/test split
- All four model types complete the full pipeline
- Role changes in Step 1 trigger reset modal and clear downstream correctly
- A Step 4 covariate change after a fit (after the modal) clears model/diagnostics/performance/results
- A train/test split change in Step 1 (after the modal) clears investigation/model/diagnostics/performance/results and keeps Table 1
- Step status indicators update correctly through the workflow
- Stale indicators appear when upstream changes invalidate downstream results
- Dataset 2 triggers appropriate warnings throughout the pipeline
- All blocking modals display correctly and prevent interaction during processing
- `module_report.R` progress bar refactored to blocking modal
- `devtools::check()` passes with 0 errors, 0 warnings

### PRD references
- §5.4 (stale propagation), §8.6 (reset pipeline)
- Global Mandates (withProgress refactor reminder)
- All sections — this is the integration phase
