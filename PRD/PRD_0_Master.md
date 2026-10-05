# EDARK — Master Product Requirements Document

**Scope:** the whole application — what EDARK is, the principles every part follows, how the app is laid out, and how its parts interact.
**Stage PRDs:** [PRD_1_Prepare.md](PRD_1_Prepare.md) (§P) · [PRD_2_Explore.md](PRD_2_Explore.md) (§E, includes Report) · [PRD_3_Analyze.md](PRD_3_Analyze.md) (§A)
**Implementation details and pitfalls:** [NOTE_implementation.md](NOTE_implementation.md) (§N)

**Section references.** Every section ID carries its document's prefix: §M (this document), §P, §E, §A, §N. A reference such as §A8.6 is unambiguous anywhere in the repo.

**Precedence.** A stage PRD is authoritative for its stage. This document is authoritative for anything that spans stages. If code and PRD disagree, the PRD wins unless the discrepancy is recorded in CLAUDE.md as a known doc issue.

---

## M1 — Overview

### M1.1 Purpose

EDARK is an R package that provides an interactive Shiny GUI for preparing, exploring, reporting on and modelling tabular datasets, focused on clinical and medical research. A researcher calls `edark(dataset)`, shapes the data in **Prepare**, explores variables and relationships in **Explore** (and builds slide/document reports from it), and fits and reports statistical models in **Analyze** — without writing analysis code.

The core functions behind the GUI are pure R functions. The report and model pipelines can be called from a script without launching the app.

### M1.2 Target Users

- **Clinical researchers** who prefer a GUI for routine data work and need output that looks like a published paper.
- **Analysts** who need to characterise a new dataset quickly before scripting.
- **Biostatisticians** who use the app for speed and reproducibility, and need to audit what it did.

Analyze defines three user archetypes (Clinician, Clinician-Researcher, Biostatistician) that apply to the whole app — see §A1.3 and §A2. The rule that follows from them: **serve the Biostatistician without patronising them, and never lose the Clinician.**

### M1.3 The Workflow at a Glance

```
edark(dataset)
   │  validate → auto-cast column types → store original (immutable)
   ▼
1 · Prepare   choose columns, transform variables, filter rows  → Apply → working dataset
   ▼
2 · Explore   Plot:   describe / correlate / trend  (working dataset)
              Report: full or custom PPTX / DOCX / HTML report
   ▼
3 · Analyze   freeze working dataset → roles → Table 1 → variable investigation →
              covariates → model → diagnostics → performance → results
   ▼
4 · Export    one zip: working dataset, session file, Analyze tables / figures /
              notes, compiled report - only outputs that exist and are current
```

Navigation between the four tabs is free. The stages form an arc, not a locked sequence.

---

## M2 — Design Principles

### M2.1 Product Principles

1. **No code required.** Every task the app supports can be completed with clicks alone.
2. **Clinical language first.** Framing, labels and messages use the vocabulary of the research question, not of the statistics. Problems are explained in plain language with what to do about them.
3. **No silent decisions.** Every default is visible and can be overridden. The app never recodes, drops or converts data without saying so (e.g. it warns that a numeric variable looks categorical but never factors it silently).
4. **Guide, don't gate — except where a result would be wrong.** Soft nudges steer the user; hard constraints only block combinations that cannot produce a valid result (§A4.4–A4.5).
5. **Publication-ready output.** Tables, figures and reports are formatted to drop into a manuscript or slide deck.
6. **Reproducible.** Decisions are recorded as data (specs), so a run can be described, re-run and exported (§A7.9 code generation; §M8 sessions).

### M2.2 Engineering Principles

1. **Modular.** Every logical UI + server block is a Shiny module with an explicit interface (§M5.1).
2. **One state object.** All session state lives in one `reactiveValues` object, `shared_state`. No `<<-`, no session-global variables (§M5.2).
3. **Pure services.** Computation lives in plain functions that take plain arguments and never touch Shiny: report generation, model fitting, validation, summaries. Modules gather inputs, call services and render.
4. **Specs, not side effects.** User decisions are written to spec objects (`column_transform_specs`, `row_filter_specs`, `plot_specification`, `analysis_spec`). Outputs are computed from specs.
5. **Original data is immutable.** `dataset_original` is set once at launch. Every derived dataset is rebuilt from it.
6. **One statistical engine.** Every p-value and confidence interval is computed in one place (`R/stats_inference.R`) under one set of rules (§A4.2, §N2). Numbers shown in different parts of the app for the same quantity must agree.
7. **One plotting engine.** All plots are static `ggplot2` objects — the same object is shown on screen and written to reports. No plotly, no headless browser.
8. **Public API.** The report pipeline is callable without Shiny (`edark_report()`, `generate_report()`, `generate_custom_report()`).

### M2.3 Deliberate Exceptions

Some parts of the app depart from the defaults above on purpose. New work should follow the defaults unless it fits one of these cases.

| Default | Exception | Where | Why |
|---|---|---|---|
| Recompute on button click (§M3.2) | Aesthetics re-render the current plot live | Explore (§E7) | Cheap: the spec is reused, only styling changes |
| Recompute on button click | Step 4 writes covariates to the spec on every click | Analyze (§A5.3 Step 4) | Selection is cheap to record; the model itself still needs Run |
| Recompute on button click | Preflight validation runs live on every spec change | Analyze (§A8.3) | Users need to see blocking problems before they click Run |
| Free navigation (§M4.2) | Analyze steps are gated | Analyze (§A5) | Later steps have no meaning without a model |
| Staged changes (§M3.1) | Switching Prepare sub-tabs auto-applies staged changes | Prepare (§P7.3) | Keeps Data Preview and downstream tabs in sync with what the user sees |

---

## M3 — Functional Principles

These are behaviours the user can rely on in every part of the app.

### M3.1 Staging and Explicit Apply
Data-shaping changes are **staged** and take effect only when applied. In Prepare nothing touches the working dataset until Apply (§P7). Staged changes survive navigation to other tabs.

### M3.2 Compute on Click
Expensive work (plots, reports, Table 1, variable selection, model fitting, diagnostics, results) runs when the user clicks its button, never on every input change. The exceptions are listed in §M2.3.

### M3.3 Destructive Changes Ask First — and Cancel Undoes
A change that would discard results the user has produced asks for confirmation first:
- Prepare Apply / Reset / sub-tab switch while custom report items exist (§P7.5)
- Analyze role changes, covariate changes after a fit, optimizer changes after a fit (§A8.6)

**Cancel restores the previous state in the UI**, not just in the server, so the screen never shows a choice that was not applied.

### M3.4 Stale State Is Shown, Not Hidden
When upstream data changes, downstream views say so instead of silently showing old results or crashing:
- Explore shows a "dataset has changed" notice after an Apply (§E6).
- Analyze shows a mismatch banner when the working dataset no longer matches the frozen one (§A3.4).
- Custom report items whose columns were removed render a placeholder instead of aborting the report (§E12).

### M3.5 Progress for Long Operations
Report generation and every Analyze run use a **blocking progress modal**: a progress bar plus a detail line, with no close button, removed automatically when the work ends or fails. No `withProgress` toasts.

### M3.6 Consistent Statistics and Formatting
One method per quantity, one formatter per display: p-values display as "< 0.001" or three decimals; estimates and CIs use one formatting rule (§N2). Where two parts of the app report the same kind of test, they use the same function.

### M3.7 Graceful Handling of Imperfect Data
Missing columns, dropped factor levels, and variables that no longer exist are handled by skipping or falling back — with a plain-language message where the user needs to act, silently where they don't. A single bad item never aborts a whole report or load.

---

## M4 — Application Structure

### M4.1 Navigation Layout

`bslib::page_navbar` with four numbered tabs, plus two navbar utilities:

| Tab | Contents | Layout |
|---|---|---|
| **1 · Prepare** | Sub-tabs Columns · Transforms · Row Filters · Data Preview | Left sidebar (Apply) + `navset_card_tab` (§P3) |
| **2 · Explore** | Sub-tabs **Plot** (pills Describe · Correlate · Trend) and **Report** (pills Full Report · Custom Report) | Plot: left sidebar controls + output panel (§E2); Report: §E10 |
| **3 · Analyze** | Five numbered step pills: Setup · Table 1 · Variables · Covariates · Model (Model nests Summary · Create · Diagnostics · Performance · Results) | `navset_pill` orchestrator (§A5.1) |
| **4 · Export** | One page: formats and Build & Download (config), the zip as a folder tree (centre), what the zip will hold (info) | `edark_page()` (§A10, `PRD/BUILD_Export.md`) |
| *(navbar right)* | Debug button; light/dark theme toggle | — |

### M4.2 Navigation and Gating
- The four top-level tabs are always reachable. Export lists every file it could write and enables only those whose output exists and is current (§A10).
- Prepare sub-tab switches auto-apply staged changes; invalid transforms block the switch (§P7.3).
- Explore → Report navigation is requested through `shared_state$requested_tab` / `requested_report_subtab` (§M6.4).
- Analyze steps are gated by what exists (§A5, CLAUDE.md "Step gating"): Steps 1–4 always open; Step 5 once the dataset is frozen and an outcome is assigned; the Model sub-tabs after Create once a model is fitted. Locked steps show a tooltip explaining what unlocks them.

### M4.3 Entry Points

| Function | Purpose |
|---|---|
| `edark(dataset = liver_tx, max_factor_levels = 20)` | Launch the app. Validates input, auto-casts types (§P2), builds UI and server. |
| `edark(dataset, session = path)` | Launch and restore a saved session (§M8.8). |
| `edark_report(data, report_type, variables, primary_variable, primary_role, stratify_variable, report_format, output_path, max_factor_levels)` | Generate a Full Report without the app (§E15). |
| `generate_report()` / `generate_custom_report()` | Shiny-free report builders used by both the app and `edark_report()` (§E15). |

### M4.4 UI Conventions
Layout, action placement and visual hierarchy follow [NOTE_UI-principles.md](NOTE_UI-principles.md). Summary of the recurring patterns:
- Sidebars are flat: no card wrappers; section headers are small uppercase muted labels; one full-width primary button per sidebar.
- Tables with interactive cells use `reactable`, patched in place rather than re-rendered (§N1).
- Disabled controls explain themselves (tooltip or inline message).

---

## M5 — State and Module Architecture

### M5.1 Module Convention
- Every module is `foo_ui(id)` + `foo_server(id, shared_state)`.
- Modules are siblings. No module calls another module's server function; all server calls are in `edark.R`'s `server()`.
- Modules communicate **only** through `shared_state`.
- Private module state (`reactiveVal`) is allowed for UI mechanics (staged selections, pending modals). Anything another module or a saved session needs must live in `shared_state`.

### M5.2 `shared_state` Ownership

`shared_state` is created once per session in `edark.R`'s `server()`. Field groups and their owners:

| Group | Fields | Written by | Read by |
|---|---|---|---|
| Dataset | `dataset_original`, `original_column_types` (set once, never overwritten); `dataset_working`, `column_types` (updated on Apply) | launch; Prepare Apply | everyone |
| Prepare staging | `included_columns`, `column_type_overrides`, `column_transform_specs`, `row_filter_specs`, `has_pending_changes` | Prepare modules | Prepare; Analyze once at freeze (via `last_applied_specs`) |
| Prepare revert | `last_applied_specs`, `revert_trigger` | Prepare Apply / Reset / revert | Prepare modules; Analyze at freeze |
| Explore (Describe / Correlate) | `primary_variable`, `primary_variable_role`, `secondary_variable`, `stratify_variable`, `bar_display` | Explore controls | Explore output |
| Explore (Trend) | `trend_timestamp_variable`, `trend_variable`, `trend_summary_stat`, `trend_resolution`, `trend_stratify_variable`, `trend_zero_baseline`, `trend_impute_zero` | Trend controls | Explore output |
| Plot | `plot_specification`, `active_plot`, `variable_summary`, `explore_needs_refresh` | Explore controls / output; Prepare Apply sets the refresh flag | Explore output |
| Aesthetics | `ggplot_theme`, `color_palette`, `show_data_labels`, `show_legend`, `legend_position` | Explore controls | Explore output |
| Custom report | `custom_report_items`, `requested_tab`, `requested_report_subtab` | Explore output; Report | Report; `edark.R` navigation observer |
| Analyze | `analysis_data`, `analysis_spec`, `analysis_result` | Analyze modules only | Analyze modules, plus Session and Export read-only (§M5.3) |
| Session | `session_restore` | Session module | Analyze Steps 1 and 4, which clear it (§M8.7) |

Stage PRDs list their fields in detail: §P10, §E9, §A3.3.

### M5.3 Analysis-Reserved Fields
`analysis_data`, `analysis_spec` and `analysis_result` belong to Analyze. Prepare, Explore and Report never read or write them. Analyze reads Prepare state exactly once — at freeze — and never writes back (§A1.2, §M6.6). The exceptions are readers that never write: the session module reads `analysis_spec` to save it (§M8.10), and the Export page (`module_export.R`) reads all three to write the zip (§A10).

---

## M6 — Module Interactions and Data Flow

### M6.1 Launch
`edark(dataset)` → `validate_input()` → `cast_column_types()` → `detect_column_types()`. The cast dataset becomes both `dataset_original` and the initial `dataset_working`; `column_types` becomes both `original_column_types` and `column_types` (§P2).

### M6.2 Prepare → Working Dataset
Prepare modules stage specs. Apply runs the pipeline **from `dataset_original`** in a fixed order — type overrides → column selection → transforms → row filters (§P7.2) — then sets `dataset_working`, re-detects `column_types`, clears `has_pending_changes`, sets `explore_needs_refresh`, and snapshots `last_applied_specs`.

### M6.3 Explore and Report Consume the Working Dataset
Explore plots and the Report tab always read the current `dataset_working` and `column_types`. They never modify them.

### M6.4 Explore → Report
- **Add to Custom Report** (Explore output) appends a snapshot of the current plot spec plus a thumbnail to `custom_report_items`. Data is not snapshotted — reports re-render items from the working dataset at generation time.
- **View Report** sets `requested_tab` / `requested_report_subtab`; an observer in `edark.R` switches tabs and clears the request.

### M6.5 Stale-Data Guard and Revert
If custom report items exist, an Apply, Reset or Prepare sub-tab switch first warns that the report may be affected (§P7.5). Choosing to revert restores the staged specs from `last_applied_specs` and increments `revert_trigger`; each Prepare module observes it and resyncs its UI.

### M6.6 Prepare → Analyze
- **Start Analysis** (Step 1) copies `dataset_working` into `analysis_data`, appends `.edark_row_id`, stores a dataset signature, and copies `last_applied_specs` plus original/working dimensions into `analysis_spec$specification_metadata$prepare_snapshot`. This is the only time Analyze reads Prepare state.
- If `dataset_working` later changes, Analyze shows a mismatch banner offering to restart (§A3.4). It never follows upstream changes automatically.
- `prepare_snapshot` feeds the Step 5 Summary and (planned) the generated R script, so both can describe how the analysis dataset was produced.

### M6.7 Interaction Diagram

```
                     ┌──────────── dataset_original (immutable) ────────────┐
                     │                                                      │
   Prepare modules ──┴─ staged specs ── Apply ──► dataset_working ──┬──► Explore › Plot ──► custom_report_items ──► Explore › Report
        ▲                                   │                       │                                                   ▲
        │ revert_trigger                    │ last_applied_specs    ├──► Explore › Report (Full) ───────────────────────┘
        └──── stale-data guard ◄────────────┘        │              │
                                                     ▼              ▼
                                         prepare_snapshot ◄── Analyze Step 1 freeze ──► analysis_data / analysis_spec / analysis_result
```

---

## M7 — Outputs and Exports

EDARK produces four kinds of output. They are deliberately separate.

| Output | What it is | Where | Status |
|---|---|---|---|
| **Explore reports** | Full or custom PPTX / DOCX / HTML report of plots and summary tables | Explore › Report (§E10–E14) | Built |
| **Export materials** | One zip: working dataset (RDS / CSV / SPSS / Stata / Excel), session file + Prepare steps, Analyze tables (Word), figures (PNG) and per-folder notes, compiled report (Word / HTML); R script coming soon | 4 · Export (§A10, `PRD/BUILD_Export.md`) | Built 2026-10-05, untested |
| **Dataset export** | The working dataset alone | Now part of 4 · Export (data/ folder) | Folded into Export |
| **Session file** | Saved decisions for resuming work, optionally with data | Session menu (§M8) | Built (Phase S; autosave deferred) |

Single-plot exports (Save Plot, Copy to Clipboard) are in the Explore output panel (§E6).

---

## M8 — Session Save and Load

**Status:** built 2026-09-28 (Phase S, save / load / launch argument). Autosave is deferred - evaluate the cost of writing on every change first (§M8.9). Spans Prepare, Explore › Report and Analyze.

### M8.1 Purpose

Let a researcher save their setup and pick up where they left off - on the same dataset, or on a refreshed pull of it with exactly the same columns and column types. A session file stores **decisions, not results**. Loading one sets up the app; nothing is fitted or computed. Anything that needs to run (Table 1, variable investigation, the model) is re-run by the user.

This is separate from **materials** (4 · Export, §A10), which exports outputs for publication and reproduction. Export does put a session file in its zip (`reproduce/session.edark.rds`, written by the same `build_session()`), so the materials can be reopened.

### M8.2 What a Session Contains

| Area | Content | Source |
|---|---|---|
| Prepare | Included columns, type overrides, transforms, row filters | `shared_state$last_applied_specs` |
| Explore › Report | The custom report list: each item's plot spec, title, time added, and its thumbnail as PNG bytes | `shared_state$custom_report_items` |
| Analyze Step 1 | Outcome, exposure, candidate covariates, clusters, reference levels | `analysis_spec$variable_roles` |
| Analyze Step 1 / Model › Performance | Model purpose, validation method, train/test split variable and training level; all validation settings (folds, repeats, bootstrap resamples, seed) | `analysis_spec$purpose_specification`, `analysis_spec$validation_settings` |
| Analyze Step 4 | Checked covariates. `NULL` if none are checked | `analysis_spec$variable_roles$final_model_covariates` |

**Not saved:** Explore picks and Appearance, Report settings, Table 1 options, Step 3 settings and results, model settings (including the optimizer), and any fitted object, table, or plot.

Prepare settings that were staged but not yet applied are not saved. A session reflects the last Apply.

### M8.3 File Format

A single `.rds` file with the extension `.edark.rds`, containing a plain named list. It holds no functions, environments or language objects - `read_session()` refuses a file that does - and nothing from the file is ever run as code.

```r
list(
  session_schema_version = 1L,
  edark_version          = "0.9",
  saved_at               = <POSIXct>,
  dataset_definition = list(
    columns      = c(age_tx = "numeric", graft_type = "character", ...),  # input classes (§M8.4)
    signature    = "3f9c0a1b2d4e5f60",    # short hash of `columns`
    column_types = c(age_tx = "numeric", graft_type = "factor", ...)      # EDARK types after the launch casts
  ),
  prepare  = list(included_columns, column_type_overrides,
                  column_transform_specs, row_filter_specs),
  analysis = list(                        # NULL if Start Analysis was never clicked
    roles = list(outcome_variable, exposure_variable, candidate_covariates,
                 cluster_variables, reference_levels),
    purpose_specification, validation_settings,
    final_model_covariates                # NULL if Step 4 has nothing checked
  ),
  custom_report_items = list(list(id, plot_spec, title, added_at, thumb_png = <raw>), ...),
  data = NULL                             # or the input data.frame, if the user chose to include it
)
```

### M8.4 Dataset Definition and Matching

The dataset definition describes the dataset **as it was passed to `edark()`**, before the launch casts (§P2.2): each column's name and R class (`class(x)` joined with `/`, e.g. `POSIXct/POSIXt`). The input class is what decides whether a saved transform or filter still lands. The `signature` is a short hash of that name -> class map (column order ignored) - a hash of the structure, **not of the data**: different or additional rows never affect it.

A session loads only if the current dataset has **exactly** the same columns with the same input classes, **and** the launch casts read every column as the same EDARK type. The second check catches a character column that was cast to factor when saved (few unique values) but stays character now (more than `max_factor_levels`) - same input class, but a saved factor filter would not land.

This is separate from the full-data hash Step 1 stores at freeze (`specification_metadata$dataset_signature`), which drives the "working dataset has changed" banner within a single session.

### M8.5 Schema Version

`session_schema_version` describes the file format, not the app version. A file with a higher number than the app knows is refused: *"This session was saved with a newer version of EDARK (x.y). Update EDARK to load it."* An unreadable file, one without the session structure, or one holding code is refused: *"This file is not a valid EDARK session."*

**Pre-release, there are no migrations.** The structure changes in place and old files simply stop loading. Once EDARK is released, every schema change bumps the number and ships an upgrade function (`.session_migrate_v1_to_v2()`, ...) run in order on load. See the reminder in the root `CLAUDE.md` (Coding philosophy).

### M8.6 No Partial Loads

A session applies whole or not at all. It is refused, with the reasons, when:

| Check | Message |
|---|---|
| Columns or input classes differ (§M8.4) | *"This session does not match this dataset."* followed by the missing columns, columns not in the session, and columns of a different type (first five of each) |
| A column is cast to a different EDARK type | as above, "Read as a different type at launch: ..." |
| A saved transform is invalid on this data (`.find_invalid_transforms_in()`, e.g. log of values <= 0) | *"The session's transforms do not fit this data: ..."* |
| The Prepare pipeline errors | *"The session's data preparation failed on this data: ..."* |
| The saved row filters leave no rows | *"The session's row filters leave no rows in this data."* |

Everything else is applied exactly as saved. Numeric filter bounds are kept as set, not moved to a new data edge; a factor filter keeps only rows at its saved levels. A saved reference level that is no longer a level of the frozen data falls back to Step 1's default (the first level), and Step 4 then applies its usual effective-level rule.

### M8.7 Load Sequence

All checks run before anything is written, so a refused load changes nothing.

1. **Read and check** the file: structure, schema version, dataset match, and the Prepare pipeline run on `dataset_original` (`session_prepare_dataset()`).
2. **Confirm.** If the app has applied or staged Prepare changes, a frozen analysis, or custom report items, show *"Load session? This replaces your current data preparation and analysis setup. Analysis results are cleared."* with Cancel / Load. With items queued, a warning adds *"Your N custom report items will be discarded and replaced by the session's."*
3. **Prepare.** Write the staged Prepare fields, commit the working dataset the same way Apply does (`.commit_working_dataset()`, which also snapshots `last_applied_specs`), and increment `revert_trigger` so every Prepare tab resyncs its inputs. Filter pruning is skipped - the specs were applied together when saved.
4. **Custom report.** Replace `custom_report_items` with the session's, writing each thumbnail back to a new temp file. The old thumbnails are deleted.
5. **Analyze.** If the session has an `analysis` block, or an analysis is frozen now, write `shared_state$session_restore <- list(token, step1_done = FALSE, roles, purpose_specification, validation_settings, covariates)`.
   - **Step 1** sees the payload. With `roles`: it freezes the dataset (as Start Analysis does), sets the roles and reference levels in `roles_state` and applies them through its normal path (`.sync_spec()` + `.push_roles_to_table()`), then writes the purpose and validation settings into the spec. It sets `step1_done`, and clears the payload unless covariates are waiting. Without `roles` (the session predates Start Analysis): it unfreezes - `analysis_data`, `analysis_spec` and `analysis_result` go to `NULL` - and clears the payload. No "Clear Analysis Results?" dialog appears; step 2 already confirmed.
   - **Step 4**: on the `roles_key` change this causes, if the payload has `step1_done` and the spec's roles match the payload's, it starts its selection from the saved covariates instead of none, then clears the payload. Its live-write logic then writes them to the spec.
6. **Navigate** (after the flush, so Steps 1 and 4 have run): Analyze Step 4 if covariates were restored, Step 1 if roles were, otherwise Prepare.
7. **Notify** with a toast: *"Session loaded (saved 2026-09-18 14:02)."*

### M8.8 Entry Points

- **In the app:** a **Session** menu on the right of the navbar with *Save session...* and *Load session...*.
  - **Save** shows an "Include dataset" checkbox (off by default) with the note *"Includes patient-level data. Only share where your data governance allows."* The file downloads as `edark_session_YYYY-MM-DD_HHMMSS.edark.rds`. With the box ticked, the dataset stored is the one passed to `edark()`, before casting.
  - **Load** accepts a session file. Any data inside it is **ignored**; an app session never switches datasets.
- **At launch:** `edark(dataset, session = "path.edark.rds")`.
  - `dataset` given -> the session is applied to that dataset, and any data in the file is ignored.
  - `dataset` omitted and the file contains data -> that data is launched, then the session is applied.
  - `dataset` omitted and the file has no data -> error: *"This session has no data. Call edark(your_data, session = ...)."*
  - A session that does not match is refused in the console, before the app starts, with the reasons.

### M8.9 Autosave (deferred)

Not built. The plan, to be revisited once the cost of building and writing a session on every change is measured:

- **What:** a session without data. Data is never written automatically.
- **When:** whenever saved content changes (`last_applied_specs`, `variable_roles`, `final_model_covariates`, `purpose_specification`, `validation_settings`, `custom_report_items`), debounced ~2 s, only if the content differs from the last autosave; once more when the session ends.
- **Where:** `tools::R_user_dir("edark", "data")/autosave/`, named by the dataset signature (§M8.4), newest 10 kept per signature.
- **Resume:** at launch, if an autosave exists for this signature and no `session` argument was passed, offer *"Resume your previous session from 2026-09-18 14:02?"* with **Resume** / **Start fresh**, shown on or inside the splash card. Resume runs the load sequence in §M8.7.

### M8.10 Architecture Notes

- Pure functions (no Shiny) are in `R/service_session.R`: `dataset_definition()`, `dataset_signature()`, `build_session()`, `read_session()` / `validate_session()`, `session_dataset_mismatch()`, `session_prepare_dataset()`, and the thumbnail pack / unpack helpers. Errors are `edark_session_error` conditions whose message is shown to the user as is. Unit tests: `tests/testthat/test-service_session.R`.
- `R/module_session.R`: `session_ui()` (the navbar menu) / `session_server(id, shared_state, dataset_input, launch_session)` handle the save and load modals, the confirm, the load sequence and a launch session. It is wired last in `server()`, so a launch session is applied in the first flush after every module has registered its observers.
- **An exception to the analysis-field rule (§M5.3):** the session module reads `analysis_spec` to save it. It never writes analysis fields; Steps 1 and 4 apply their parts of `shared_state$session_restore` themselves.
- New `shared_state` field: `session_restore` (the waiting payload, `NULL` when idle).

---

## M9 — Technical Foundation

### M9.1 Packages

`DESCRIPTION` is the authoritative list. Grouped by role:

| Role | Packages |
|---|---|
| App framework | `shiny`, `bslib`, `shinyjs`, `shinyWidgets`, `waiter` |
| Data manipulation | `dplyr`, `tidyr`, `tibble`, `lubridate`, `stringr`, `forcats`, `magrittr` |
| Plotting | `ggplot2`, `scales`, `ggpubr`, `ggthemes`, `cowplot`, `see`, `patchwork` |
| Tables | `reactable`, `DT`, `gt`, `gtsummary`, `flextable` |
| Reports and file output | `officer`, `rvg`, `rmarkdown`, `knitr`, `base64enc`, `haven`, `writexl`, `jsonlite`, `zip` |
| Statistics and modelling | `e1071`, `smd`, `lme4`, `lmerTest`, `glmnet`, `broom`, `broom.mixed`, `parameters`, `performance`, `insight`, `correlation`, `lmtest`, `pROC`, `detectseparation` |
| Utilities | `digest` |
| Suggests (dev) | `testthat`, `shinytest2` |

Generated R scripts (planned, §A7.9) load their own packages with `pacman::p_load()`; `pacman` is not an app dependency.

### M9.2 Code Organisation
- `R/edark.R` — entry point, UI, `shared_state`, server wiring, cross-tab navigation.
- `module_*.R` — Shiny modules (one per UI block).
- `service_*.R`, `build_*.R`, `render_plot.R`, `generate_report.R`, `stats_inference.R`, `analysis_utils.R` — pure functions.
- `inst/report_template.Rmd`, `inst/templates/ppt_16x9_blank_template.pptx` — report templates.
- `data/liver_tx.rda` — built-in dataset (§M9.4); regenerated by `data-raw/liver_tx_sample.R`.

The per-file map is in CLAUDE.md.

### M9.3 Coding Conventions
- **Pipe:** `magrittr` `%>%` only — never the base pipe `|>`. Applies to app code and generated scripts.
- **No duplicate definitions:** each function is defined in exactly one file.
- **Documentation:** exported functions documented with roxygen2.

### M9.4 Non-Functional Requirements

| ID | Requirement |
|---|---|
| NF-01 | Installs as a standard R package (`devtools::install()`) |
| NF-02 | `devtools::check()` passes with 0 errors and 0 warnings |
| NF-03 | Exported functions documented with roxygen2 |
| NF-04 | No session-global state; all reactive state in session-scoped `reactiveValues` |
| NF-05 | Usable at a 1280 × 800 viewport |
| NF-06 | Long operations show progress (§M3.5) |
| NF-07 | Pure functions testable with `testthat`; modules testable with `shinytest2` / `chromote` *(test suite is backlog)* |

### M9.5 Built-in Dataset
`liver_tx` — 500 × 36 synthetic liver transplant dataset, the default for `edark()`. It is engineered to exercise every Analyze path (outcomes, clusters, collinearity tiers, a noise block, missingness patterns). Details: §N7.

---

## M10 — Scope

### M10.1 Out of Scope
- Loading data from files, databases or cloud storage — data is passed to `edark(dataset)` in R.
- Multi-dataset merging or joins.
- Real-time or streaming data.
- User accounts or multi-user sessions.
- PDF reports (HTML, PPTX and DOCX only).
- Analyze-specific permanent exclusions: §A11.3.

### M10.2 Deferred and Backlog
- Analyze deferrals by priority: §A11.2.
- App-wide backlog and known bugs: CLAUDE.md "To-dos".
