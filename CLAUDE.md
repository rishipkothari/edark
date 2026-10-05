# CLAUDE.md — EDARK v0.9

## What this is
An R package providing an interactive Shiny GUI for exploratory data analysis of tabular datasets, focused on clinical research workflows. A researcher calls `edark(dataset)`, prepares the data, explores variables interactively, and optionally exports a report.

Full spec: `PRD/PRD_0_Master.md` (plus the per-stage PRDs alongside it).

---

## How to run

```r
options(pkgType = "binary")  # always set before installing packages
devtools::install_deps()
devtools::load_all()         # load R/ during development (instead of library())
edark(liver_tx)              # launch with the built-in dataset
edark(your_dataframe)

devtools::document()         # rebuild roxygen docs
devtools::check()            # must pass with 0 errors, 0 warnings
```

Browser-side behaviour (reactable patching, JS handlers) can only be tested by driving the app with `chromote` (§N1.4).

---

## File map

```
R/
├── edark.R                     Entry point: validates input, casts types, builds UI+server
├── edark_report.R              Programmatic API: edark_report() — no Shiny required
├── validate_input.R            Input guard called before Shiny launches
├── cast_column_types.R         Auto-cast rules — runs once at launch
├── detect_column_types.R       Returns named char vector: col → "numeric"/"factor"/"datetime"/"character"
├── route_plot_type.R           Type combo → plot type string (Analyze tab only)
├── build_plot_spec.R           build_univariate_plot_spec() / build_bivariate_plot_spec() / build_trend_plot_spec()
├── render_plot.R               All 11 plot types; dispatches from a spec list
├── build_variable_summary.R    Summary stats table for a single variable
├── stats_inference.R           THE place p-values and CIs are computed — see "Statistical methods registry"
├── generate_report.R           Core report generation: builds sections, assembles PPT/Word/HTML
├── service_session.R           Session files (§M8): dataset definition + signature, build / read / validate, exact dataset match, rebuild Prepare from a session; pure
├── module_session.R            Session navbar menu - Save (download .edark.rds) / Load (checks, confirm, load sequence); applies edark(session = ) at launch
│
├── module_column_manager.R     Prepare › Columns tab — include/exclude only
├── module_column_transform.R   Pipeline helpers: .apply_column_transforms, .make_range_labels, .transform_spec_is_valid
├── module_transform_variables.R Prepare › Transforms tab — flat table, one row per numeric col
├── module_row_filter.R         Prepare › Row Filters tab
├── module_prepare_confirm.R    Prepare config pane (Apply/Reset) + info pane (dimensions, itemised pending list) + messages; pipeline, validation, navigation
├── module_data_preview.R       Prepare › Data Preview tab — two toggles (Original/Working, Data/Summary) over one of four reactables
│
├── module_explore_controls.R   Explore › Describe + Relationship tab sidebars — describe_controls_ui/server + relationship_controls_ui/server
├── module_trend_controls.R     Explore › Trend tab sidebar — timestamp/resolution/variable/stat pickers
├── module_explore_output.R     Explore result pane — plot + action toolbar; variable summary and custom-report state go to the info pane
├── module_appearance.R         Explore › Appearance panel — the app's only plot-aesthetics controls; sole writer of the five aesthetic shared_state fields
├── module_report.R             Report tab — Full / Custom underline tabs; Full has a config pane + resolved section list, Custom has no centre (D11)
├── module_export.R             4 · Export page - formats + Build & Download (config), the zip as a folder tree (centre), zip contents (info); ticked build with Cancel, then auto-download (§A10, §N8)
├── service_export.R            Export registry export_items() (every file + available / stale / not run), file writers, per-folder notes docs, README, build job; pure
├── service_export_report.R     Compiled export report - one block list rendered to Word (template + .docx_* helpers) or self-contained HTML; pure
│
├── ui_helpers.R                Shared UI component library — lock reasons, buttons, badges, section labels, empty states, messages, info rows, model header, aesthetics controls
│
├── module_analysis_main.R          Analyze tab — orchestrator; 5-step navset_pill (Step 5 nests Model sub-tabs) + JS progress handler + step/sub-tab gating
├── module_analysis_setup.R         Analyze › Step 1: Setup — dataset freeze, role assignment (outcome/exposure/candidates/clusters), study type, model purpose + train/test split, reset modal with undo (Phases 1, 6b)
├── module_analysis_table1.R        Analyze › Step 2: Table 1 — gtsummary descriptive table (Phase 2)
├── module_analysis_varinvestigation.R   Analyze › Step 3: Variable Investigation — univariable screen, collinearity, stepwise/LASSO (Phase 3)
├── module_analysis_covariate_confirm.R  Analyze › Step 4: Covariate Confirmation — final covariates, reference levels, live missingness; writes to spec on every change (Phase 4)
├── module_analysis_modelspec.R     Analyze › Step 5 Model › Summary (audit) + Create (auto model type, optimizer, live preflight, fit on training rows, results) (Phase 5)
├── module_analysis_diagnostics.R   Analyze › Step 5 Model › Diagnostics — assumption checks + Run; Overview + per-check tabs (Phase 6)
├── module_analysis_performance.R   Analyze › Step 5 Model › Performance — measures + Run; Overview (one column per set of rows) + per-set tabs (Phase 6b)
├── module_analysis_results.R       Analyze › Step 5 Model › Results — output checkboxes + Generate; Summary + per-output tabs (Phase 7)
│
├── analysis_utils.R                build_analysis_formula() / apply_reference_levels() / compute_complete_cases() / compute_covariate_sample() / analysis_split() / analysis_split_rows() / analysis_model_data() / analysis_test_data()
├── service_analysis_pipeline.R     reset_analysis_pipeline(shared_state, from_step) — clears downstream state per PRD §8.6
├── service_analysis_validation.R   validate_analysis(spec, data, tier, verbose) — all Tier 1 + Tier 2 preflight checks
├── service_analysis_models.R       Model fitting engines: lm / glm / lmerTest::lmer / lme4::glmer; model options, outcome event, stale check (Phase 5)
├── service_analysis_summary.R      build_analysis_summary() — Step 5 Summary sections, pure, reusable by export (Phase 5)
├── service_analysis_diagnostics.R  analysis_diagnostic_options() / run_analysis_diagnostics() — pure (Phase 6)
├── service_analysis_performance.R  analysis_performance_options() / run_analysis_performance() / analysis_performance_job() — apparent, held-out test set, CV, bootstrap; pure (Phases 6b, 7b)
├── service_analysis_tables.R       Table 1 (gtsummary); Model › Results table (own data.frame → gt / flextable), fit statistics (Phases 2, 7)
├── service_analysis_plots.R        ggplot figures from plain data — diagnostic plots (Phase 6); performance plots (Phase 6b); forest plot (Phase 7)
├── service_analysis_variable_selection.R  Univariable screen / stepwise / LASSO (Phase 3)
├── service_analysis_codegen.R      Reproducible R script generator (Phase 5)
│
└── data.R                      Roxygen docs for built-in liver_tx dataset

data/
└── liver_tx.rda                500 × 36 synthetic liver transplant dataset (default for edark())

data-raw/
└── liver_tx_sample.R           Regenerates data/liver_tx.rda — run with Rscript; seeded, reproducible

inst/
├── report_template.Rmd             Rmd template for HTML report output
├── templates/
│   ├── ppt_16x9_blank_template.pptx   Bundled slide template for PPT output
│   └── word_docx_blank_template.docx  Bundled Word template for DOCX output - Title,
│                                      Subtitle and heading 1-9 styles; the TOC and the
│                                      section furniture are built in .assemble_docx()
└── www/
    ├── edark.css
    └── edark_export.js    Export page zip tree: tri-state folders, selection / open folders to the server, download trigger (§N8.3)
### What each file is for

| File | Holds | Read it when |
|---|---|---|
| `CLAUDE.md` (this file) | The app at a glance, plus **open** TO-DOs | Always - it loads every session |
| `PRD/CLAUDE.md` | The index: where everything lives, the non-negotiables, current state, doc/code discrepancies | Starting any task |
| `PRD/PRD_*.md` | The spec - what the app *should* do (§M master, §P Prepare, §E Explore, §A Analyze) | Changing behaviour |
| `PRD/NOTE_implementation.md` (§N) | How the code does it, and the traps found building it | **Before editing a module** |
| `PRD/RESOLVED.md` | Closed TO-DOs with root cause and lessons | A bug smells familiar |
| `PRD/BUILD_*.md` | Stage plans and acceptance criteria for one piece of work | Working through a planned build |
| `PRD/NOTE_UI-principles.md` | Layout, action placement, visual hierarchy | Any UI work |

```

### What to update, when, and why

- **Behaviour changed** → the stage PRD (§M if cross-stage). *Why:* the PRD wins over code, so a stale PRD makes the code wrong by definition.
- **Hit a trap or wrote something non-obvious** → §N, in that stage's section. *Why:* §N is keyed to code and read prospectively - it is the only doc someone opens *before* touching a module.
- **Closed a TO-DO** → delete it from the list below, write it up in `PRD/RESOLVED.md` (root cause, fix, lesson). If it produced a rule that should change how future code is written, **also** add that rule to §N and cross-link the two. *Why:* this file is loaded into every session, so it carries only live work.
- **New TO-DO** → the list below, under its area and magnitude.
- **Doc contradicts code** → `PRD/CLAUDE.md` › "Doc Discrepancies to Resolve", with the decision still to make.
- **Added or renamed a file in `R/`** → the file map above, *and* the "Which Sections Govern Which File" table in `PRD/CLAUDE.md`.
- Cite sections by prefix (§P7.2, §A8.6, §N1.12) in code comments and docs.

### §N vs RESOLVED.md - the split that matters

They can hold the same fact for different jobs, and both are needed:

- **§N answers "what must I know before I edit this file?"** - keyed to code, no dates, read prospectively.
- **RESOLVED.md answers "have we hit this before?"** - keyed to date, carries the symptom and the hunt, read while debugging.

A fix that yields a forward-looking rule goes in **both**: the rule in §N, the history in RESOLVED.md with a `**Durable rule: §NX.Y**` pointer. A one-off with no reusable rule goes in RESOLVED.md only. Never let §N carry the narrative or RESOLVED.md carry the rule alone - a rule filed only under a date is a rule nobody reads in time.
main-specific questions.
- Move resolved TODOs with durable lessons to `PRD/RESOLVED.md` for future reference.

---

## Coding philosophy
- do not use /u2014 dashes, use hyphens or other simple ASCII characters where appropriate
- do not commit without permission
- if you are instructed to commit, add a message of at most 6 sentences, be brief. also, push after commits
- remind me to push after commits
- when a commit changes what a session saves (`build_session()` in `R/service_session.R`, or the shape of any state it copies - `last_applied_specs`, `custom_report_items`, `variable_roles`, `purpose_specification`, `validation_settings`), evaluate whether `.SESSION_SCHEMA_VERSION` needs a bump and an upgrade function. Pre-release: change the structure in place, no migrations (§M8.5); after release: bump + `.session_migrate_vN_to_vM()`
- before every commit, update `EDARK_LAST_UPDATE` in `R/ui_helpers.R` to that day's date - it is shown on the splash screen. Keep `EDARK_VERSION` in step with `Version:` in DESCRIPTION.

---

## TO-DOs

### High priority
- session autosave (save / load on click is built on `session-manager-v1`, §M8) - evaluate the cost of building + writing a session on every change first; resume prompt on or inside the splash card (§M8.9)
- Propensity score weighted/adjusted/matched models
- Alternative plot types per variable combination (heat map, balloon plot, etc.) (plot 3 vars in 2 dimensions or new types of plots for existing 2 vars)

### Prepare

#### High magnitude

#### Mid magnitude
- Warnings section in the Apply pane — mimic the "Stratify by" section header in Report › Full Report.
- varaible labels - in Prepare phase, column in master table that has a textbox for custom column labels. Buttons to apply some function (str to title, capitalize first only, variable name) to change all labels quickly for basic presentation purposes.

#### Low magnitude

### Explore

#### High magnitude
- Alternative plot types per variable combination (heat map, balloon plot, etc.)

#### Mid magnitude
- Statistical tests in the Explore › Relationship summary panel (num × fac → Kruskal-Wallis; fac × fac → chi-square / Fisher's). Reports already have these via the table helpers; the Explore summary does not.
- Async report generation (synchronous now; cancel needs `future` / `promises`).
- Custom Report's config pane now holds only Output Format + Generate, so it has room for a "Report Contents" box like Full Report's (Dataset Summary, Table One). Probably a shared component between the two rather than two copies.
- **Drag-and-drop reordering of Custom Report items.** The list is in the centre and reorders via the toolbar's Move Up / Move Down, which needs no JS. Drag would need `sortable` (a SortableJS wrapper) attached to the row container - `sortable_js()`, not `rank_list()`, which is text-labels-only. The fiddly part is not the drag: it is that the drop rewrites the DOM while `renderUI` re-renders from `shared_state$custom_report_items`, so the input -> server reorder -> re-render round trip has to land on the same order or the row snaps back. Needs `chromote` to verify.

#### Low magnitude
- add options in reporting for including table 1 (with checkboxes for overall, by primary variable (if a correlate report) and by stratify variable if that is selected) (and this table 1 would have a second set of radios for all variables or selected variables), dataset summary (radio option for all variables or only selected variables); this should show up in custom report also, except for table one has no options in the stratified variety, but will maintain the radio option for all variables or selected variables (whcih includes any variable present in any plot either as exposure outcome or stratify by), and the dataset summary would offer the same variable inclusion radio selector
- **Bug — centre tables in PPT + HTML reports:** `flextable::set_table_properties(align = "center")` is set in both `.style_dataset_summary_ft()` and `.style_section_ft()` in `generate_report.R`, but tables still render left-aligned in PPT and HTML (DOCX may work). Investigate `officer` slide content alignment for PPT and the Rmd template's table rendering for HTML.
- appearance should be a pill next to explore data and report; full screen for config; this may change to "settings" later but we can leave it as appearance for now. main panel will house container for settings.

### Analyze

### In progress
Phases 0–7 and 6b complete; Export moved to the top-level 4 · Export page (built 2026-10-05, `PRD/BUILD_Export.md`). Phase 5b code generator (`service_analysis_codegen.R`) deferred — Step 5's R Code Preview is a placeholder; it should consume `prepare_snapshot` + the spec (incl. `purpose_specification`). Phase definitions and acceptance criteria: `PRD/BUILD_Analysis.md`.
- **Performance validation follow-ups** (Phase 7b built 2026-09-19): optional shrunk-coefficient output from the bootstrap calibration slope; decision curve analysis; CV / bootstrap for mixed models with several cluster variables groups by the first one only.

#### High magnitude
- Propensity score model subtypes - matching, score adjusted, IPTW, etc

#### Mid magnitude
- **Collinearity misses numeric x factor pairs.** `compute_collinearity()` measures numeric pairs (Pearson r) and factor pairs (Cramer's V) only, so a numeric variable that tracks a factor strongly (e.g. age by a factor) is never flagged - in Step 3 or in the Explore report's Collinearity section, which reuses it. Needs a third measure such as the correlation ratio (eta); changes Step 3's pill and the report together.
- Univariable screen flags a multi-level factor as suggested if *any* level term clears the threshold; an overall per-variable likelihood-ratio p would be more correct (`service_analysis_variable_selection.R`).
- **Covariates and Model › Summary have config panes with nothing to configure.** Both pages keep their configuration in the table (Covariates) or have none at all (Summary), so after the Stage 4 page contract their left panes carry orientation text rather than controls. Honest but thin. Either give them real global controls - Covariates has an obvious candidate in table-level Select all / Clear, and a search - or decide those two pages are a deliberate exception to D6 and say so in the plan.

#### Low magnitude
- Collinearity plot base size should scale with the number of variables; still too small with few.
- **Mixed models have no influence check** — Model › Diagnostics offers Cook's distance / leverage for lm / glm only. A cluster-level (leave-one-cluster-out) influence check would close the gap (§A11.2).
- generate table 1 spinner - specify which table its working on, e.g. if it has 3 to generate (overall, by exposure, by outcome) have 3 stops on the bar and change text to say which is being created
- need to think about what we want table 1 RHS to show; for now, it doesn't accurately reflect what it's stratifying by, it just picks one of the vars i think maybe exposure by defualt?

### Other

#### High magnitude
- investigate reset pipeline and what it looks like
    - also with UI refresh, might be able to eliminate some of the click to lock in steps, should evaluate

#### Mid magnitude
- **Export: two checks left (branch `export_v1`).** Verified 2026-10-05 except (a) the stale path driven through the UI (Prepare change after the freeze, covariate change after a fit - the logic is unit-tested) and (b) the Word report's layout opened in Word (§N8, `PRD/BUILD_Export.md` §7).
- Export presets (Manuscript / Full archive / Data only) - deferred from `PRD/BUILD_Export.md`.
- Export PDF report - recommended route HTML → `pagedown::chrome_print` (needs Chrome / Edge; disable with a reason when absent).
- Export `reproduce/analysis_script.R` - listed as coming soon; needs Phase 5b.

### Low magnitude
- Splash card overflows on mid-width windows. After the 2x scale-up its min-width is 800px, but the fallback rule in `edark.css` section 9 only triggers below 460px, so roughly 460-840px of window width overflows horizontally. Raise that breakpoint to about 880px.
- add color palettes that have more colors in them than 8 - shoot for maybe 20?
- `shinytest2` module tests + `testthat` unit tests.
- **Install Rtools 4.5 and get `devtools::check()` to 0/0/0.** `devtools::check()`
  currently dies at "Could not find tools necessary to compile a package" - Rtools is
  not installed (https://cran.r-project.org/bin/windows/Rtools/). The package has no
  compiled code, so the gate can be skipped with
  `options(buildtools.check = function(action) TRUE)`, but installing Rtools is the real
  fix. With the gate skipped on 2026-09-24 the check ran 0 errors, 2 warnings, 1 note;
  the roxygen errors are closed (`PRD/RESOLVED.md`) and what remains is undeclared
  imports - `@importFrom` for the `stats` / `utils` functions used (`median`, `sd`,
  `IQR`, `na.omit`, `quantile`, `setNames`, `modifyList`, `str`, ...) plus a
  `utils::globalVariables()` entry for `.data` and the NSE column names.
