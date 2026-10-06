# CLAUDE.md — EDARK v0.9

EDARK is an R package: an interactive Shiny GUI for preparing, exploring, reporting on and modelling tabular clinical research data. `edark(dataset)` → **1 · Prepare** → **2 · Explore** (Plot + Report) → **3 · Analyze** → **4 · Export**.

This file is the **index**: where to find things, the rules that must never be broken, current state, and doc/code discrepancies. Specs and mechanics live in the documents below — read the relevant section before changing code. What each `R/` file *does* is in the root `CLAUDE.md`; which `§` sections govern it is below.

---

## Where to Find What

| Document | Prefix | Read it for |
|---|---|---|
| [PRD_0_Master.md](PRD_0_Master.md) | §M | Design and functional principles, app layout, `shared_state` ownership, how stages interact, outputs overview, **session save/load** (§M8), packages, scope |
| [PRD_1_Prepare.md](PRD_1_Prepare.md) | §P | Launch validation and auto-cast, Columns, Transforms, Row Filters, Apply pipeline, Data Preview |
| [PRD_2_Explore.md](PRD_2_Explore.md) | §E | Explore › Plot (Describe, Correlate, Trend, plot types, aesthetics) and Explore › Report (Full / Custom, formats, API) |
| [PRD_3_Analyze.md](PRD_3_Analyze.md) | §A | The analysis workflow: roles and model purpose, Table 1, variable investigation, covariates, model creation, preflight, diagnostics, performance, results, export |
| [NOTE_implementation.md](NOTE_implementation.md) | §N | Pitfalls, the **statistical methods registry** (§N2), and as-built mechanics per stage |
| [NOTE_UI-principles.md](NOTE_UI-principles.md) | — | Layout, action placement, visual hierarchy. Read before any UI work so it isn't reinvented each time |
| [BUILD_Analysis.md](BUILD_Analysis.md) | — | Analyze build phases and acceptance criteria (incl. Phase 5b code generator, Phase S sessions; Phase 8 export superseded by BUILD_Export.md) |
| [BUILD_Export.md](BUILD_Export.md) | — | The 4 · Export page: decisions, zip layout, item registry, notes documents, tree UI, compiled report, stage status |
| [BUILD_UI-redesign.md](BUILD_UI-redesign.md) | - | **The single UI plan** (Claude + Codex assessments merged 2026-09-22; revised 2026-09-23 from user feedback, §1.3): settled decisions, assessment, and Stages 0-6 - honest locking first, component library, CSS theme, config / result / info page contract with a messages area, flatter navigation. `bslib` + R + plain CSS only. Stage status table at the top; work one stage per session. **§1.3 holds the UI principles taken from real use (button scale, placement by scope, shared settings, no redundant surfaces) - read it before any UI work, alongside `NOTE_UI-principles.md`** |
| [RESOLVED.md](RESOLVED.md) | — | Closed to-dos with root cause and lessons. Check here when a bug smells familiar, before re-deriving a fix |
| [Codex proofing.md](Codex%20proofing.md) | — | Briefing notes for an external proofing agent |
| [tools/PRD_section_map.md](tools/PRD_section_map.md) | — | Old → new section numbers (migration aid), plus the scripts that generated it |

`BUILD_*` files are build plans for a specific piece of functionality, added as that work is planned. `archive/` holds the superseded monolithic PRDs (`EDARK V0.2 - PRD.md`, `EDARK_Analysis_Module_PRD.md`) and the superseded Codex UI assessment (`BUILD_UI-redesign_CODEX.md`) - kept for history only; never cite them.

**Quick lookup**

| Question | Go to |
|---|---|
| Which module owns / may write a `shared_state` field? | §M5.2 |
| Why does this recompute live when everything else waits for a click? | §M2.3 |
| Pipeline order for Apply | §P7.2 |
| Which plot type does a variable pair produce? | §E4, §E8 |
| Where must a p-value or CI be computed? | §N2 (spec §A4.2) |
| What does `reset_analysis_pipeline()` clear? | §A8.6, §N6.13 |
| Preflight check codes | §A8.2; adding a check §N6.12 |
| A reactable table loses its checkbox state | §N1.4 |
| A pane or the whole page scrolls when it should not | §N1.12 |
| `devtools::document()` errors on lines that look fine | §N1.13 |
| An input still has a value after its UI disappeared | §N1.3 |
| Test dataset features (collinearity, noise, missingness) | §N7 |
| Association vs prediction; validation method (bootstrap / CV / held-out set) | §A1.4a; helpers §N6.2 |

**Precedence:** stage PRD for its stage; Master for anything cross-stage; the PRD wins over code and over §N unless the mismatch is listed under "Doc discrepancies" below.

---

## Non-Negotiables

Full rationale in the linked sections.

1. **One state object.** All session state in `shared_state` (created in `edark.R`'s `server()`); no `<<-`, no globals; modules talk only through it (§M5).
2. **Module convention.** `foo_ui(id)` + `foo_server(id, shared_state)`; siblings only; all server calls in `edark.R` (§M5.1).
3. **Analyze fields are private.** `analysis_data` / `analysis_spec` / `analysis_result` are written only by Analyze modules; besides Analyze, only the Session and Export modules read them, read-only. Analyze reads Prepare state only at freeze (§M5.3).
4. **Original data is immutable.** `dataset_original` and `original_column_types` are never overwritten. After Apply, `column_types` is updated from the working dataset — the two diverge on purpose, and Columns shows Orig. vs Curr. type (§P2.3).
5. **Compute on click; stage Prepare changes.** Nothing touches `dataset_working` until Apply. Exceptions are only those in §M2.3.
6. **Prepare pipeline order:** start from `dataset_original` → type overrides → column selection → transforms → row filters. Do not reorder (§P7.2).
7. **One statistics engine.** p-values and CIs only via `R/stats_inference.R`; never call `confint*`, `chisq.test`, `fisher.test`, `kruskal.test`, `cor.test` or format a p-value elsewhere (§N2).
8. **`%>%` only**, never `|>` — in app code and generated scripts (§M9.3).
9. **Guard every observer that writes `shared_state` with `!identical()`** (§N1.2); never trust a conditionally rendered input (§N1.3).
10. **One definition per function** — no duplicates across files (§N1.8).

---

## Which Sections Govern Which File

What each file does is in the root `CLAUDE.md`. This is the § lookup.

| File | Sections |
|---|---|
| `edark.R` | §M4, §M5 |
| `edark_report.R` | §E15 |
| `edark-package.R` | §N1.17 |
| `validate_input.R` | §P2.1 |
| `cast_column_types.R` | §P2.2 |
| `detect_column_types.R` | §P2.3 |
| `route_plot_type.R` | §E4 |
| `build_plot_spec.R` | §E3–E5 |
| `render_plot.R` | §E8, §N4 |
| `build_variable_summary.R` | §E6.1 |
| `stats_inference.R` | §N2 |
| `generate_report.R` | §E10–E15, §N5 |
| `module_column_manager.R` | §P4 |
| `module_column_transform.R` | §N3.4 |
| `module_transform_variables.R` | §P5 |
| `module_row_filter.R` | §P6 |
| `module_prepare_confirm.R` | §P7 |
| `module_data_preview.R` | §P8 |
| `module_explore_controls.R` | §E3, §E4 |
| `module_trend_controls.R` | §E5 |
| `module_explore_output.R` | §E6 |
| `module_appearance.R` | §E7 |
| `ui_helpers.R` | §N1.12 (BUILD_UI-redesign Stages 1-2) |
| `inst/www/edark.css` | §N1.12 |
| `module_report.R` | §E10–E12 |
| `module_analysis_main.R` | §A5.1, §N6.3 |
| `module_analysis_setup.R` | §A5.3, §N6.4 |
| `module_analysis_table1.R` | §A5.3 |
| `module_analysis_varinvestigation.R` | §A5.3, §N6.5 |
| `module_analysis_covariate_confirm.R` | §A5.3, §N6.6 |
| `module_analysis_modelspec.R` | §A5.3, §A8.4, §N6.7 |
| `module_analysis_diagnostics.R` | §A5.3, §N6.9 |
| `module_analysis_performance.R` | §A5.3, §N6.9a |
| `module_analysis_results.R` | §A5.3, §N6.10 |
| `module_export.R` | §A10, §N8 |
| `analysis_utils.R` | §N6.2 |
| `service_analysis_pipeline.R` | §A8.6, §N6.13 |
| `service_analysis_validation.R` | §A8, §N6.12 |
| `service_analysis_models.R` | §A7, §N6.8 |
| `service_analysis_summary.R` | §N6.11 |
| `service_analysis_diagnostics.R` | §N6.9 |
| `service_analysis_performance.R` | §N6.9a |
| `service_analysis_tables.R` | §N6.10 |
| `service_analysis_variable_selection.R` | §A9, §N6.5 |
| `service_analysis_codegen.R` | §A7.9 |
| `service_export.R` | §A10, §N8 |
| `service_export_report.R` | §A10.4, §N8.6 |
| `inst/www/edark_export.js` | §N8.3 |
| `service_session.R` | §M8, §N3.5 |
| `module_session.R` | §M8.7-M8.8, §N3.5 |
| `data/liver_tx.rda` | §N7 |

---

## Current State

- **Prepare, Explore (Plot + Report):** built.
- **Analyze:** Phases 0–7 and 6b complete — Setup (incl. model purpose + train/test split), Table 1, Variable Investigation, Covariate Confirmation, Model Creation, Diagnostics, Performance, Results.
- **Built 2026-09-19:** Phase 7b performance validation — Step 1 validation method (bootstrap / cross-validation / held-out test set, mutually exclusive), settings and Cancel-able runs in Model › Performance (§A1.4a, §A5.3).
- **Built 2026-10-05 (branch `export_v1`, not yet run in R):** the top-level **4 · Export** page - one zip of the working dataset, session file, Analyze tables / figures / notes and a compiled Word / HTML report; only current outputs exportable ([BUILD_Export.md](BUILD_Export.md), §A10, §N8). Analyze is now five steps.
- **Stubs and deferrals:** Phase 5b's R code generator (`service_analysis_codegen.R`) is deferred — the R Code Preview is a placeholder; it should consume `prepare_snapshot` + the spec (incl. `purpose_specification`).
- **Built 2026-09-23:** UI consistency Stage 1 - honest locking. `R/ui_helpers.R`
  (`EDARK_LOCK_REASON`, `edark_run_button()`, `edark_run_gate()`) and
  `inst/www/edark.css` now exist; every gated Analyze nav item explains itself in a
  popover and every Run button is disabled with its reason visible
  ([BUILD_UI-redesign.md](BUILD_UI-redesign.md) Stage 1).
- **Built 2026-09-23:** UI consistency Stage 2 - component library + one home for aesthetics. `R/ui_helpers.R` gained `edark_section_label()`, `edark_button()`, `edark_action_toolbar()`, `edark_empty_state()`, `edark_message()`, `edark_info_row()`, `edark_model_header()` and `edark_aesthetics_controls()`; the four aesthetics accordions became one `R/module_appearance.R` panel (D9 as amended); Full and Custom reports now generate with the aesthetics on screen (D7)
- **Built 2026-09-23:** UI consistency Stage 3 - the theme file. `inst/www/edark.css` now carries named tokens (status scale, four column-type colours), a calm navbar, one pill treatment per nav level with Analyze stepper states, focus rings, scroll containment and the three absorbed inline `tags$style()` blocks. Dark mode is `bslib::input_dark_mode()` flipping `data-bs-theme` instead of a `flatly <-> darkly` preset swap, so it needs no server code
- **Built 2026-09-23:** UI consistency Stage 4 - the page contract. Every page is now `edark_page()`: config left at 340 px, messages + result in the centre, info right at 300 px (Report › Custom has no centre, D11). Prepare's dimensions and an itemised pending list moved to its info pane and its warnings to the messages slot; Setup and Covariates flipped from a right-hand mixed sidebar to config left / info right; Model › Summary and Step 6 gained both panes; the six sidebar widths collapsed to `EDARK_CONFIG_WIDTH` / `EDARK_INFO_WIDTH`
- **Built 2026-09-23:** UI consistency Stage 5 - flatter navigation, one look per level. Prepare and Explore page tabs became pills; Step 3's nested pills and Report's Full / Custom became underline tabs; Data Preview dropped from three tab levels to one pair of toggles; Analyze's six step pills fit one row at 1280 px with shortened labels. Card tabs are now used only for views of one generated result. **Open for the user:** Report's Full / Custom went to level 3b rather than the config-pane pills D8 names, because D11 leaves Custom with no result surface - see Stage 5 in BUILD_UI-redesign.md
- **Built 2026-09-23:** UI consistency Stage 6 - the small fixes. Explore gained an empty state and its plot toolbar is disabled until a plot exists; the variable-selection modal puts its actions above the list; the Row Filters row count moved into Prepare's info pane; copy pass ("Explore Data", "Clear", "Run Selection"); the button sweep is clean - `grep` for `"btn-` finds hits only in `R/ui_helpers.R`
- **UI consistency plan complete:** all six stages done ([BUILD_UI-redesign.md](BUILD_UI-redesign.md)). Report's Full / Custom sits at level 3b (underline tabs) rather than the config-pane pills D8 named - ruled 2026-09-23: it is the third nested level, so it follows the level, not the original wording
- **Built 2026-09-28 (branch `session-manager-v1`):** Phase S session save / load and `edark(session = )` (§M8). Exact dataset match on input classes, no partial loads; saves Prepare, custom report items, roles, purpose, validation settings and covariates. Not yet run in R - see Phase S › Verification in [BUILD_Analysis.md](BUILD_Analysis.md). Autosave deferred (§M8.9).

---

## Doc Discrepancies to Resolve

Each needs a decision: change the code or change the doc.

- **Analyze step count — §A and the build plan still say nine, code has five.** `module_analysis_main.R` has five top-level `nav_panel`s, with Diagnostics / Performance / Results nested as sub-tabs under **Step 5 Model**; Export moved out to the top-level **4 · Export** page (2026-10-05, §A10 rewritten). §N was renumbered to match the code (2026-09-21); the rest of `PRD_3_Analyze.md` and `BUILD_Analysis.md` were **not** — doing so touches §A references throughout. Until they are, read §A's "Step 6/7/8" as the Model › Diagnostics / Performance / Results sub-tabs and "Step 9" as 4 · Export.
- **Trend "count" mode.** Old docs described a "None" trend variable giving `trend_count`, and a `trend_proportion` type. Code: the trend variable is required, and types are `trend_numeric` / `trend_factor`. §E5 documents the code. Decide whether an event-count mode is wanted.
- **Dead renderer.** `render_plot()` dispatches `trend_mean`, which no spec builder produces.
- **Dataset signature.** §A3.2 specifies a structural signature; Step 1 stores a sha256 hash of the data (see the note in §A3.2).
- **Type overrides without UI.** `column_type_overrides` exists in state and pipeline, but no UI sets it (§P4). Keep as a hook or remove.
- **Two Table Ones.** Explore › Report has its own Table One (`.build_tableone_df()`); Analyze has the gtsummary Table 1. Decide: keep both deliberately, or converge. (The two *report systems* are kept separate on purpose - decided 2026-09-22: Explore › Report compiles one document, Analyze › Export produces individual files plus a compilation. See BUILD_UI-redesign.md D1.)
- **Zero baseline default.** `shared_state$trend_zero_baseline` initialises TRUE; the checkbox renders FALSE. §E5 documents FALSE.

---

## Keeping the Docs Current

The full policy - what to update, when and why - is in the root `CLAUDE.md` › Documentation. In short:

- Behaviour change → update the stage PRD (or §M if cross-stage).
- New pitfall or non-obvious mechanic → §N, in the relevant stage section.
- New to-do → root `CLAUDE.md`. Closed to-do → move it out of root `CLAUDE.md` into [RESOLVED.md](RESOLVED.md), in full. Discovered doc/code mismatch → this file.
- **A closed to-do that yields a forward-looking rule goes in both places**: the rule in §N (keyed to code, read before editing a module), the history in [RESOLVED.md](RESOLVED.md) (keyed to date, read while debugging), with a `**Durable rule: §NX.Y**` pointer joining them.
- Cite sections with their prefix (§P7.2, §A8.6) in code comments and docs.
