# EDARK - Export Build Plan

> Replaces Phase 8 of `BUILD_Analysis.md`. Written 2026-10-05 from the design conversation;
> every decision below is settled (the open questions were resolved the same day - §1).
> **All seven stages were built 2026-10-05 on branch `export_v1` and verified the same day in
> R 4.3.3**: every file of all four model types reopened, and the page driven with `chromote`
> (tree, re-render, Cancel, auto-download). Fixes and traps from that pass: §N8.8.

## Status

| Stage | Scope | Status |
|---|---|---|
| 1 | Move Export to a top-level `4 · Export` page; Analyze back to five steps | done, verified 2026-10-05 |
| 2 | Item registry + output status (pure) | done, verified |
| 3 | File writers + zip assembler (pure) | done, verified (4 model types x 2 format sets, 266 files reopened) |
| 4 | Section notes documents | done, verified |
| 5 | Export page UI: zip tree, info pane, build + download | done, verified with `chromote` |
| 6 | Compiled report (Word, HTML) | done; HTML checked visually, Word opens (layout not inspected) |
| 7 | Docs: PRDs, §N, file maps, TO-DOs | done |

Order: 1 → 2 → 3 → (4, 6 independent of each other) → 5 → 7. Stage 5 can start on a
stubbed registry once Stage 2 lands. Stage 7 is partly done inside each stage (file maps
change in Stage 1); the PRD rewrite is batched at the end.

---

## 1 - Decisions

| # | Decision |
|---|---|
| X1 | **Export is a top-level step, `4 · Export`**, after Analyze in the main navbar. It works without any Analyze work (data + reproduction files only). Analyze goes back to five steps (`1 · Setup` … `5 · Model`). |
| X2 | **Export is for materials, not Explore.** Explore › Report stays the only way out for Explore plots and reports (D1 stands; its "Analyze › Step 6" wording is amended). |
| X3 | **One zip, folders mirror the app's steps** (§2). Each folder holds `tables/`, `figures/` and one notes document. |
| X4 | **Tables are Word only** (`.docx`, one file per table, via flextable). One source of truth; merged headers survive. No xlsx tables. |
| X5 | **Figures are PNG**, 8 × 6 in at 300 dpi unless the plot carries its own size (forest plot height scales with rows, `attr(, "n_rows")`). |
| X6 | **Data is optional**: the full working dataset, unmodified (no added split column, no `.edark_row_id`), in one chosen format: RDS (keeps types and levels), CSV, SPSS `.sav`, Stata `.dta`, Excel `.xlsx`. The train/test split is reproducible from the spec (`analysis_split()` reads a column already in the data), so nothing is added. |
| X7 | **Reproduction = a session file + a readable change log.** `reproduce/session.edark.rds` written by `build_session()` (no second format), with its own "include input dataset" option (default off, same governance note as Session › Save). `reproduce/prepare_steps.txt` lists dropped columns, type overrides, transforms and filters in words. `reproduce/analysis_script.R` is listed as **coming soon** (disabled) until Phase 5b. |
| X8 | **No `manifest.json`, no nested analysis package.** A `README.txt` at the root says what each folder holds, when the zip was made, from which dataset (signature, dims) and which model. |
| X9 | **Only current outputs can be exported.** Every item is listed from the start; an item is selectable only when its source exists and is not stale. Not-run and stale items are shown greyed with a badge naming where they are made. |
| X10 | **Numbers that are not a table or a figure go in a per-folder notes document** (`<folder>_notes.docx`), optional and checked by default. It is built from the same structures the screen renders (the `metrics` and `messages` data frames of diagnostics and performance, `build_analysis_summary()` sections, run settings), so screen and export cannot disagree. See §4. |
| X11 | **Compiled report is optional**, Word or HTML. PDF is deferred. |
| X12 | **Selector is the zip tree** (§5): the centre pane is the zip's folder tree; ticking a file is previewing the zip. Presets are deferred. |
| X13 | **Export reads, never recomputes** (§A10.1). Everything comes from `analysis_result` / `shared_state`. The one exception is plain formatting (data frame → flextable, ggplot → PNG). |
| X14 | **Export reads whatever it needs.** `module_export.R` reads `analysis_data` / `analysis_spec` / `analysis_result` (never writes them); §M5.2 / §M5.3 and non-negotiable #3 now name it beside the session module. `export_server()` also takes `dataset_input`, as `session_server()` does. |
| X15 | **Build & Download is one click.** The build runs in ticks with a Cancel button (the Model › Performance pattern), then the page starts the download itself. A Download Last Build toolbar button above the tree re-downloads the last build. Small, purpose-built JavaScript is allowed (the tree and the download trigger) - "don't go bonkers". |
| X16 | **Prepare changed after the freeze makes every Analyze output stale** (resolves O1). Stale outputs cannot be exported, so the zip is consistent by construction: `data/`, the session file and `prepare_steps.txt` describe the current working dataset, and no Analyze file from the old one can be ticked. The messages slot says why. |

### Resolved 2026-10-05 (were open)

| # | Question | Resolution |
|---|---|---|
| O1 | Which dataset when Prepare changed after Setup froze it? | Every Analyze output becomes stale and cannot be exported (X16). |
| O2 | `analysis_result.rds` | The whole in-memory analysis result as one R object - fitted model objects, coefficient and result tables, plots, diagnostics, performance - for an R user to reload and keep working (`predict()`, `summary()`, re-plotting). Offered under `model/`, **unticked by default**: it is large and embeds model frames, i.e. patient-level rows. |
| O3 | HTML report engine | `htmltools`: figures inlined via `base64enc`, tables via `flextable::to_html()`; no pandoc; one self-contained file. Word reuses the `.docx_*` helpers in `generate_report.R`. |
| O4 | Zip root name | `edark_export_YYYY-MM-DD_HHMMSS/`. |

---

## 2 - Zip layout (supersedes §A10.2)

Folders appear only when they hold at least one selected file.

```
edark_export_2026-10-05_142530/
├── README.txt
├── analysis_report.docx | .html            compiled report (optional)
├── data/
│   └── working_dataset.rds | .csv | .sav | .dta | .xlsx
├── reproduce/
│   ├── session.edark.rds                    (+ input dataset if ticked)
│   ├── prepare_steps.txt
│   └── analysis_script.R                    coming soon - disabled
├── table1/
│   ├── table1_overall.docx
│   ├── table1_by_exposure.docx
│   ├── table1_by_outcome.docx
│   └── table1_notes.docx                    variables, stratifiers, p / SMD choice, n
├── variable_selection/
│   ├── tables/   univariable_screen.docx, collinearity_flagged_pairs.docx,
│   │             stepwise_selection.docx | lasso_selection.docx
│   ├── figures/  correlation_heatmap.png, cramers_v_heatmap.png
│   └── variable_selection_notes.docx       method, threshold / criterion / lambda, seed,
│                                            selected + excluded variables
├── model/
│   ├── tables/   results_table.docx, fit_statistics.docx
│   ├── figures/  forest_plot.png
│   ├── methods.docx                         methods paragraph
│   ├── model_notes.docx                     Model › Summary sections, formula, n used,
│   │                                        outcome event, reference levels, fit messages
│   └── analysis_result.rds                  unchecked by default (O2)
├── diagnostics/
│   ├── tables/   diagnostic_summary.docx, vif.docx, influential_rows.docx, random_effects.docx
│   ├── figures/  one PNG per stored plot (residuals_vs_fitted, qq, scale_location, linearity_*,
│   │             cooks, leverage, ranef_qq, cluster_sizes, binned_residuals, ...)
│   └── diagnostics_notes.docx               every metric + test result + message
└── performance/
    ├── tables/   performance_summary.docx (one column per set of rows), bootstrap_optimism.docx
    ├── figures/  <plot>_<set>.png (roc_curve_apparent, calibration_plot_test, ...)
    └── performance_notes.docx               validation method + settings actually used, seed,
                                             n / events per set, failed resamples, messages
```

Figure and table file names come from the stored list names (`result_plots$diagnostic_plots`,
`result_plots$performance_plots[[set]]`), so a check added later exports without touching
the registry.

---

## 3 - Item registry and output status (Stage 2)

### `export_items(state)` - pure, in `R/service_export.R`

Input: a plain list snapshot of what export needs (`dataset_working`, `analysis_data`,
`analysis_spec`, `analysis_result`, `last_applied_specs`, `original_column_types`,
`dataset_input`), so it is testable without Shiny.

Output: one row per exportable file.

As built (`export_items(st, data_format, report_format)`; `st` from `export_state()`):

| Column | Meaning |
|---|---|
| `id` | zip path **without** extension, e.g. `diagnostics/figures/qq_plot` - the selection key, so a format change keeps the tick |
| `path` | zip path with extension |
| `folder`, `sub`, `file` | path parts (`sub` is `""`, `tables` or `figures`) |
| `kind` | the writer: `data`, `session`, `prepare_steps`, `script`, `report`, `table1`, `notes`, `uni_table`, `sel_table`, `collin_table`, `collin_fig`, `results_table`, `fit_stats`, `forest_plot`, `methods`, `result_rds`, `diag_summary`, `diag_table`, `diag_fig`, `perf_summary`, `perf_optimism`, `perf_fig` |
| `key`, `set` | which table / plot / set of rows the writer takes |
| `group` | the `analysis_output_status()` group it follows |
| `status` | `available` / `stale` / `not_run` / `coming_soon` |
| `reason` | shown beside an unavailable file: "Run in Model › Diagnostics", "The spec changed since the model was fitted - refit in Model › Create" |
| `default` | ticked when first available (`TRUE` except `analysis_result.rds` and the coming-soon script) |

A writer is looked up by `kind` (Stage 3); the registry holds no functions, so it can be
compared with `identical()` to decide whether the tree must re-render (§5).

### `analysis_output_status(spec, result)` - pure, in `R/service_analysis_models.R`

One status per output group, reusable by the step gating and by Export:

| Group | Not run when | Stale when |
|---|---|---|
| Table 1 | no `result_tables$table1_overall` | - (a role change resets it, §A8.6) |
| Variable investigation | no `variable_investigation$*` | - (resets cover roles and rows) |
| Model, Results | no `primary_model` / no `results_generation` | `analysis_fit_is_stale(spec, result)` |
| Diagnostics | no `diagnostics` | fit stale |
| Performance | no `performance` | fit stale, or `analysis_validation(spec)` differs from `performance$validation` (settings changed since the run) |

Diagnostics / Performance / Results are already cleared on every refit
(`reset_analysis_pipeline(from_step = 4)`), so "stale" only ever means "the spec moved since
the fit" - the case `analysis_fit_is_stale()` already detects.

### One storage change

Collinearity matrices are computed on pill entry but only `flagged_pairs` is stored.
Stage 2 stores `cor_matrix` and `cramers_matrix` in `result_plots$collinearity_plots` too,
so the heatmaps export without recomputation (X13). `module_analysis_varinvestigation.R`,
three lines.

---

## 4 - Section notes documents (Stage 4)

One builder per folder returns **sections** in the shape `build_analysis_summary()` already
uses (`list(id, title, rows)`, rows of label / value / items / level). One shared writer turns
sections into a `.docx` (headings + two-column key-value flextables + message list), and the
compiled report reuses the same sections - so the notes, the report and the screen read the
same numbers from the same place.

| Notes doc | Built from |
|---|---|
| `table1_notes` | `table1_specification`, `variable_roles$table1_variables`, n per table |
| `variable_selection_notes` | `variable_selection_specification` (method, threshold, criterion, lambda, seed), `variable_investigation` (selected, held, excluded with reasons) |
| `model_notes` | `build_analysis_summary()` sections + `run_status` (formula, n used / total, outcome event, reference levels, preflight warnings, fit messages) |
| `diagnostics_notes` | `diagnostics$metrics` grouped by `section`, formatted by `format`, flagged by `level`; `diagnostics$messages`; checks run; run time |
| `performance_notes` | `performance$validation` (method, folds, repeats, reps, seed), `basis`, `split`, per-set n / events / failed fits, `performance$metrics` incl. SD, `messages` |

Prepare has no notes doc: `prepare_steps.txt` is its notes (plain text so it reads without
Word, next to the session file it describes).

---

## 5 - Export page UI (Stage 5)

Page contract (D6): `edark_page(config, result, info, messages)`. As built - mechanics in §N8.

**Config (left).** Data format (radio: RDS / CSV / SPSS / Stata / Excel). Report format
(radio: Word / HTML). Session file: "Include the input dataset" + governance note. Then the
primary action **Build & Download** at the bottom (D10), disabled with a reason when nothing
is ticked.

**Centre - the zip tree.** A right-aligned toolbar above it: "n of m available files
selected" on the left, **Download Last Build** (disabled until a build exists) on the right.
Then Select all · Clear · Expand all · Collapse all, the zip's root line, README.txt (always
included), the report, and one folder per stage with `tables/` and `figures/` inside.

- Folders are native `<details>`; collapsed by default; each shows "ticked of available".
- Ticks, tri-state folder boxes and counts are handled in `inst/www/edark_export.js`; the
  selection and the open folders are reported to the server, which re-renders the tree only
  when the registry changes and renders it with its own copy of both (§N8.3).
- Not-run, stale and coming-soon files are disabled, dimmed, with an `edark_badge()` and the
  reason.

**Info (right).** "The zip will hold": per folder "k of n", total files (README included).
"Source": working dataset dims, Analyze state (model type / stale / not started), data
format. After a build: time, files, size, files left out. (The size estimate before a build
was dropped - an honest number only exists after the build.)

**Messages.** Prepare changed since the freeze (X16); Prepare has unapplied changes (the
export uses the last applied state); files the last build could not write.

**Build flow (X15).** Build & Download → blocking progress modal with Cancel, one file per
step (`export_job()`) → README → `zip::zip()` → the page enables Download Last Build and
clicks it. A per-item failure does not abort the build: the item is skipped and listed in
the messages slot and in `README.txt` under "Not exported".

**Gating.** `4 · Export` is always reachable (there is always a working dataset). No lock.

---

## 6 - Compiled report (Stage 6)

`analysis_report.docx | .html`. Fixed order, sections omitted when their source is not
available (§A10.7, amended):

1. Title page: dataset, dims, date, EDARK version, model type
2. Data preparation (`prepare_steps`)
3. Model summary (`model_notes` sections) + methods paragraph
4. Table 1 (each available table)
5. Results: results table, fit statistics, forest plot
6. Performance (moved before Results for a prediction model)
7. Appendix A - variable selection (tables, heatmaps, notes)
8. Appendix B - diagnostics (summary, tables, figures, notes)

Word: reuse `.docx_add_title_page()`, `.docx_add_toc_page()`, `.docx_fit_ft()`,
`.docx_add_caption()`, `.docx_end_section()` from `generate_report.R` and the bundled
template. HTML: `htmltools` page with a sticky TOC, inline CSS, base64 PNGs (O3).

The report is built from what is **available**, not from what is ticked in the tree: ticking
selects files, the report checkbox selects the report.

---

## 7 - Stage details

### Stage 1 - Move

- `R/module_analysis_export.R` → `R/module_export.R` (`export_ui()` / `export_server()`);
  `R/service_analysis_export.R` → `R/service_export.R`. Keep the placeholder page for now.
- `R/edark.R`: add `bslib::nav_panel(value = "export", title = "4 · Export", export_ui("export"))`
  after Analyze; wire `export_server("export", shared_state, dataset_input)`.
- `R/module_analysis_main.R`: drop `step6`, its title output, gate, lock key and done flag;
  update the header comment ("5-step").
- `edark_lock_reason()` strings that name "6 · Export" (grep).
- File maps (`CLAUDE.md`, `PRD/CLAUDE.md` "Which Sections Govern Which File").
- `devtools::document()` (renamed exports).

**Accept:** navbar shows Prepare / Explore / Analyze / Export; Analyze shows five pills with
no lock glyph left over; session save/load unaffected.

### Stage 2 - Registry

`export_items()`, `analysis_output_status()`, collinearity matrix storage. Testable with
plain `Rscript` on hand-built `analysis_result` lists.

**Accept:** with no Analyze work, only `data/` and `reproduce/` items are `available`; after
a fit with diagnostics, changing a covariate turns model / diagnostics / results items
`stale` with a reason; `analysis_script.R` is always `coming_soon`.

### Stage 3 - Writers + assembler

`export_build(items, selection, state, options, dir, progress_fn)`: per-kind writers
(`flextable::save_as_docx`, `ggplot2::ggsave`, `saveRDS`, `utils::write.csv`,
`haven::write_sav` / `write_dta`, `writexl::write_xlsx`, `build_session()`, text), then
`README.txt`, then `zip::zip(mode = "cherry-pick")` on the root folder.
Table 1 via `gtsummary::as_flex_table()`; results via the existing
`results_table_flextable()`. Data frames without a styled builder go through one shared
`.export_ft()` (same look as `.style_section_ft()`).

Data notes: factor → `haven::as_labelled`-free export (`write_sav` keeps factors as labelled
integers); POSIXct kept; column names sanitised for Stata (32 chars, `[A-Za-z0-9_]`) with the
renames listed in `README.txt`.

**Accept:** each data format reopens in R with the same rows and columns (`haven::read_*`,
`readxl`); every `.docx` opens (officer `read_docx` round-trip); every PNG exists and is
non-empty; zip tree matches §2 for the selection.

### Stage 4 - Notes docs

Section builders + shared sections → docx writer (§4).

**Accept:** each notes doc contains every value the matching screen shows in its Overview /
Summary panel (manual check per model type: linear, logistic, linear mixed, logistic mixed).

### Stage 5 - UI

§5.

**Accept:** tree renders every item from the start; running Diagnostics while on Export
enables its items without collapsing open folders or losing ticks; folder glyph cycles
all / some / none; Build & Download starts the download by itself and Cancel stops it with
nothing saved; Download Last Build is disabled until a build exists; a build with one
failing item still produces a zip and reports the failure. Browser behaviour checked with
`chromote` (§N1.4).

### Stage 6 - Report

§6. **Accept:** Word and HTML both open; sections present exactly for available sources;
prediction model puts Performance before Results.

### Stage 7 - Docs

- `PRD_0_Master.md`: navbar gains `4 · Export`; §M7 (shared writer) points at
  `service_export.R` + `build_session()`.
- `PRD_3_Analyze.md`: §A10 rewritten from this plan (layout §2 supersedes §A10.2 and
  §A3.8); §A5.3 / §A6.11 "Step 9" removed (Export is not an Analyze step).
- `BUILD_Analysis.md`: Phase 8 → "moved to `BUILD_Export.md`".
- `BUILD_UI-redesign.md`: amend D1 wording (Export is top-level, still separate from
  Explore › Report).
- `NOTE_implementation.md`: new §N8 Export internals (registry, no-JS tree and its
  re-render trap, two-step build/download and why).
- `PRD/CLAUDE.md`: close the "nine vs six steps" discrepancy for Export; amend
  non-negotiable #3 (X14); intro line gains `4 · Export`.
- `CLAUDE.md`: remove "Export results functionality" and the §P9 Export TO-DO; add
  presets, PDF, R script (5b) as TO-DOs.

---

## 8 - Out of scope (TO-DOs after this build)

- Export presets (Manuscript / Full archive / Data only).
- PDF report (recommended route: HTML → `pagedown::chrome_print`).
- `analysis_script.R` (Phase 5b codegen).
- Import of an export back into EDARK.
- Explore outputs in the zip (X2 - by decision, not deferral).
