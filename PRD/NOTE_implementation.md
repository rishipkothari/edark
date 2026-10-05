# EDARK — Implementation Notes

**What this is:** post-build caveats, programmatic specifics and pitfalls — *how* the code does what the PRDs specify, and the traps found while building it. Read the relevant section before changing a module.
**What this is not:** a spec. Behaviour lives in the PRDs ([PRD_0_Master.md](PRD_0_Master.md) and the stage PRDs); if a note here contradicts a PRD, the PRD wins and the note needs updating.

Sections: §N1 general pitfalls · §N2 statistical methods registry · §N3 Prepare · §N4 Explore · §N5 Report · §N6 Analyze · §N7 test data · §N8 Export.

---

## N1 — Shiny and R Pitfalls (app-wide)

### N1.1 Named character vectors crash with `[[` on missing keys
`shared_state$column_types` is a **named character vector**, not a list. A missing key with `[[` throws `subscript out of bounds` (it does NOT return `NULL`). Always guard:
```r
curr_type <- if (col %in% names(types)) types[[col]] else fallback
```

### N1.2 Every `shared_state` observer must guard with `!identical()`
`renderUI` + input observers can loop: an input change triggers `renderUI`, which re-renders the input, which fires the observer again. Without the guard, brief `""` values during re-render overwrite valid data. Every `observeEvent` that writes to `shared_state` needs `if (!identical(old, new))`.

### N1.3 Inputs removed from the DOM keep their last value
When a `renderUI` stops emitting an input, Shiny **retains that input's last value** — `input$foo` keeps returning the stale value indefinitely. `updateXInput(session, "foo", ...)` cannot clear it either: the message targets an element that no longer exists and is silently dropped.

Never trust a conditionally rendered input on its own. Derive eligibility from state and gate every read:
```r
elig      <- strat_eligible()                                  # from spec + frozen data
strat_exp <- elig$exposure && isTRUE(input$strat_by_exposure)   # not the raw input
```
This caused the Table 1 bug where switching exposure/outcome from factor to numeric still produced tables stratified by a numeric variable. Pure functions downstream (`build_table1()`) should guard independently too — see `.can_stratify()`.

### N1.4 Interactive inputs inside reactable cells: patch, don't re-render
Analyze Steps 1 and 4 render raw HTML inputs in reactable cells and report changes via `Shiny.setInputValue`. Re-rendering the table on every click loses search text and scroll position, and reactable **remounts rows from their original HTML** when a search filter is cleared (so checkbox state silently resets).

Pattern: render the table only on structural change (route the trigger through a `reactiveVal`, which ignores identical values — a plain reactive re-fires on every `analysis_spec` write); push live state with `session$sendCustomMessage`; in JS keep the last payload and re-apply it from a `MutationObserver` on a stable wrapper div. Guard every DOM write with a has-it-changed check so the observer doesn't loop.

Two hard rules, because reactable converts cell tags into **React elements** (not HTML):
1. Never use valueless attributes like `checked = NA` / `disabled = NA` / `selected = NA` — `NA` serialises to `null` and crashes reactable's renderer, unmounting the whole table (blank panel, error only in the browser console). Use strings (`disabled = "disabled"`) and set state from the patch instead.
2. JS may only fill elements React rendered **empty** (e.g. an empty `<select>` or `<span>`) — replacing children React created breaks its next reconciliation.

Server-side `testServer` cannot catch either; drive the app with `chromote` (installed) to see client errors. Step 1 uses the same push to undo a role click when the user cancels the reset modal.

### N1.5 shinyjs needs `useShinyjs()` — it lives in the `page_navbar` header in `edark.R`
Every shinyjs helper depends on it, including `shinyjs::disabled()`, which only adds a `shinyjs-disabled` CSS class that shinyjs's JS turns into a real `disabled` attribute. Remove that call and all disable/toggle logic silently stops working (buttons render clickable).

### N1.6 Disabling one choice of a radio/select without shinyjs
`shinyjs::toggleState()` / `disable()` act on a whole input by id — they cannot disable a single choice. `htmltools::tagQuery` selectors do not support `[attr=value]`. `.disable_choice(tag, value)` in `module_analysis_table1.R` walks the tag tree and sets the boolean `disabled` attribute instead. `<option>` tags arrive as a **pre-rendered HTML string** (shiny builds a select's options via `selectOptions()`), so string children are patched textually — and the select must use `selectize = FALSE`, since selectize.js builds its list in JS and leaves no `<option>` markup to patch.

### N1.7 `shiny::isolate()` inside a reactive returns stale values
Read a `shared_state` value once and store it — that single read creates the dependency AND captures the current value. A separate dependency line plus a later `isolate()` re-read can return the pre-change value.

### N1.8 Duplicate function definitions silently use the wrong one
R loads whichever definition was sourced last — no warning. Never define the same function in two files. Shared Prepare pipeline helpers live in `module_column_transform.R` only.

### N1.9 Dynamic observers must be registered lazily
Row filters, transforms and the Custom Report gallery create observers per item. Each module keeps a registered-ids guard (`registered_cols` / `registered_item_ids`, via `setdiff`) and creates observers inside `local({})` closures. Without the guard, every update to the spec list re-registers all existing observers, causing duplicate writes.

### N1.10 Blocking progress modal
Report and Analyze share one pattern (§M3.5): `showModal(modalDialog(..., footer = NULL, easyClose = FALSE))` with a Bootstrap progress bar and a detail line; `on.exit(shiny::removeModal(), add = TRUE)`; progress pushed with `session$sendCustomMessage(handler, list(frac, detail))`. Handlers: `edark_report_progress` (registered in `report_ui`), `edark_analysis_progress` (registered in `analysis_main_ui`). Analyze's modal builder is `.analysis_progress_modal()` in `module_analysis_varinvestigation.R`, shared by every step. Its `stops` argument (a character vector) splits the bar into equal named segments - a tick between them, labels underneath. The stops must come from the same function that decides what gets built, or the bar lies: Table 1 draws them from `.table1_plan()`, which `build_table1()` also iterates.
```r
shiny::showModal(shiny::modalDialog(
  title = shiny::tagList(spinner, "Running..."),
  progress_bar_div,   # id = "edark_analysis_progress_bar"
  detail_p,           # id = "edark_analysis_progress_detail"
  footer = NULL, easyClose = FALSE
))
on.exit(shiny::removeModal(), add = TRUE)
session$sendCustomMessage("edark_analysis_progress", list(frac = 0.5, detail = "Fitting model..."))
```

### N1.11 `switch(NULL, ...)` crashes
`switch()` on `NULL` throws "EXPR must be a length 1 vector". `route_plot_type()` and `build_trend_plot_spec()` can return `NULL`; always NULL-guard `spec$plot_type` before dispatch.

### N1.12 Never cap height on a bslib `card_body`
bslib ships `.bslib-card .card-body { max-height: var(--bslib-card-body-max-height, none) }` — two classes. A scroll cap written as a single class (`.edark-scroll-table`) loses on specificity, so the content grows without limit, blows out the CSS grid row the centre and info panes share, and stretches every pane in that row to match (1660 px on Prepare › Columns), forcing the whole document to scroll.

**Cap the content, never the `card_body` itself.** Doing so fixes all three panes at once. The convention and the three containment mechanisms are in the header comment on `.edark-scroll-table` (`inst/www/edark.css`) and `EDARK_RESULT_HEIGHT` (`R/ui_helpers.R`).

### N1.13 roxygen: plain `%`, and no links to `@noRd` helpers
`DESCRIPTION` sets `Roxygen: list(markdown = TRUE)`, which has two consequences that break `devtools::document()` with errors pointing far from the real line:

1. **Markdown mode escapes `%` for you.** A hand-written `\%` reaches Rd as `\\%` — a literal backslash then an Rd comment that eats the rest of the line, including the closing `}` of the `\item{}` it sits in. Symptom: every *later* `\item` reported as an unknown macro and every later section header as unexpected. Write a plain `%`.
2. **Markdown links become real `\link{}` cross-references.** Every helper in `R/ui_helpers.R` is `@keywords internal` + `@noRd` and so has no man page to link to. Refer to an undocumented internal with a code span — `` `edark_run_gate()` `` — not `[edark_run_gate()]`.

### N1.14 Badges come from `edark_badge()` only
Every badge in the app is built by `edark_badge()` / `edark_type_badge()` in `R/ui_helpers.R`; sizing lives in `.edark-badge*` in `inst/www/edark.css`. Callers pass a *role* (`numeric`, `factor`, `role`, `count`, `study_*`, ...), never a Bootstrap variant and never an inline `font-size`. An unknown role falls back to `muted` rather than erroring, so a new column type degrades to a plain badge instead of breaking a table cell.

Two sizes only: `sm` inside a table cell, `md` for a badge standing alone in a pane. A column the Prepare pipeline has transformed (cast, or rescaled in place) keeps its type badge unchanged and gains a separate "T" badge after it (`edark_type_badge(type, transformed = TRUE)` -> `edark_transformed_badge()`, `.edark-badge-transformed`), in the Prepare tint, not the warning colour. The same badge marks transformed columns in the Data Preview headers. It was an amber ring until 2026-09-25 (read as "something is wrong"), then a leading arrow glyph on the type badge until 2026-09-28 (read as part of the type). No badge role names a text colour either; `text-bg-*` computes its own contrast.

The one type display that is deliberately *not* a badge is the italic sub-label under each column header in Prepare › Data Preview (`.make_col_defs()`): a badge in every header of a wide data table reads as noise.

### N1.14b Severity is one muted scale; amber never means anything else
- The theme's `warning` is set once, `bs_theme(warning = "#b7791f")` in `edark()`. Bootswatch flatly's own is `#f39c12`, a bright orange that landed on badges, dialog buttons and full-width alerts alike. Dark mode lifts it by redefining `--bs-warning` **and** `--bs-warning-rgb` under `[data-bs-theme="dark"]` - `.text-warning` reads the rgb form, so setting only the first leaves that class on the light value.
- **Alerts are restyled in `edark.css`, not per caller.** Flatly ships `.alert { color: #fff }` over a solid theme-colour fill, so one warning painted a band of orange across the page. Section 5 redraws every `.alert-*` as a tinted panel with a coloured rule down its start edge, from the `--edark-alert-*` tokens. These rules carry one class, the same as flatly's, so they win only because this stylesheet loads last - never move the alert block above an import.
- **"The pipeline touched this" is not a severity.** Prepare › Data Preview tints transformed columns with `--edark-tint-bg` (a neutral primary) and prefixes the type sub-label with an arrow, the same mark the changed-type badge uses. It was amber at 15%, which is how the warning colour ended up meaning three different things (§BUILD_UI-redesign 2.7).

### N1.15 Spacing around nav rows and the page frame lives in CSS, not in wrappers
- **Below a nav row:** `.nav-pills + .tab-content, .nav-underline + .tab-content` in `edark.css` gives one 0.75rem gap below every level-2 pill row and level-3b underline row (not inside a `.sidebar`, where level 3a has its own). Put a panel's content straight into `nav_panel()`. Do not wrap it in `div(class = "pt-2")`: those wrappers used to make some sub-tabs start 8px lower than their siblings, so the frame jumped when switching tabs.
- **One frame per page:** `edark_page()` gives its inner (info) `layout_sidebar()` `border = FALSE, border_radius = FALSE`, and the outer one `padding = 0` when there is an info pane. Config | centre | info are then three columns of one bordered box, not a box inset inside another. The centre's padding comes from the inner layout. A page without an info pane keeps the outer padding, because nothing else supplies it.

### N1.16 Startup progress cannot come from the server
Messages sent with `session$sendCustomMessage()` while the server function is running are queued and flushed only after it returns, so a per-stage startup bar driven from R would report every stage at once, after the work. `edark_splash()` therefore anchors its three equal segments on browser events - `shiny:connected`, the first `shiny:value`, then `shiny:idle` - and creeps slowly inside each segment without ever completing one the browser has not reported. It hides on the first `shiny:idle`, with a 0.8 s floor so it cannot flash and a 15 s ceiling so a failed start cannot trap the user behind it.

Measured startup, page request to `shiny:idle`: 2.4 s warm, 4.1 s cold (the extra is R lazily loading namespaces the first time module servers touch them). All of it is after the page arrives, which is why an in-page splash covers it. Do not time the gap from `edark()` being called - that includes however long the user takes to reach the browser.


---

## N2 — Statistical Methods Registry

**Every p-value and CI in the app is computed in `R/stats_inference.R`. Never call `confint()`, `confint.default()`, `broom::tidy(conf.int = TRUE)`, `chisq.test()`, `fisher.test()`, `kruskal.test()` or `cor.test()` anywhere else, and never format a p-value by hand.** §A4.2 is the spec.

| Quantity | Function | Method | Used by |
|---|---|---|---|
| Regression estimate / CI / p | `edark_coef_table(model, data)` | Wald-type: est ± crit × SE; crit = t(residual df) for lm, t(Satterthwaite df) for lmerTest, z for glm/glmer; p from `summary()`. OR = exp(est, limits) | Step 3 univariable screen, Model › Create fit, Model › Results unadjusted models + results table |
| Numeric across groups | `edark_group_test(x, g)` | Kruskal-Wallis | Table 1 (`add_p(test = "kruskal.test")`), Report Table One, Report num × fac |
| Categorical across groups | `edark_group_test(x, g)` / `.gts_categorical_test` | Chi-square (no Yates); Fisher's exact if any expected < 5 (Monte Carlo with a fixed seed only if the exact algorithm fails). Chi-square warnings suppressed there, never in callers | Table 1, Report Table One, Report fac × fac |
| Correlation | `edark_cor_test(x, y)` | Pearson r; t-test p; Fisher's z CI | Explore scatter label, Report num × num |
| p display | `edark_format_p(p)` | "< 0.001" else 3 decimals; NA → "—" (gtsummary gets a wrapper that keeps NA blank) | Everywhere |
| Estimate / CI display | `edark_format_est()`, `edark_format_ci()` | 1 dp from 100, 2 dp from 0.1, else 3 significant digits; "est (low–high)", or "low to high" when a limit is negative | Model › Create, Model › Results, Summary |
| CI / p wording | `edark_inference_note(model_type)` | One sentence per model type | Model › Create footnote, Summary, Model › Results table footnote + methods paragraph |

Not regression inference, so defined where computed: Breusch-Pagan (`lmtest::bptest`), AUC CI (DeLong, `pROC::ci.auc`), calibration-bin CI (Wilson), SMD (gtsummary), Explore trend `mean_ci` (t CI of a mean). The F-test p in Step 5 fit statistics is `summary.lm`'s.

---

## N3 — Prepare Internals

### N3.1 `apply_prepare_pipeline()` uses `isolate()`
Any `reactive({})` that only calls `apply_prepare_pipeline(shared_state)` will **never invalidate** — the function reads everything via `isolate()`. Callers must read the relevant `shared_state` fields on their own lines first to create dependencies.

### N3.2 Where Apply runs
- `module_prepare_confirm.R` — Apply Changes / Reset buttons.
- `edark.R` — the sub-tab navigation guard (`.do_nav_apply()`): auto-applies on sub-tab switch, blocks on invalid transforms (`.find_invalid_transforms()`), and shows the custom-report modal.
- The custom-report modal (`.custom_items_modal()`) offers three routes and each caller must wire all three: confirm, revert, and **discard the items** (`.clear_custom_report_items()`). Discard is what ends the dialog - without it the same modal fires on every Apply and every Prepare tab switch for the rest of the session, and Prepare has no other way to empty the queue.
- Both go through `.run_prepare_apply()` (in `module_prepare_confirm.R`): `.prune_conflicting_filter_specs()` first (drops filters on excluded columns or columns with a staged transform), then the pipeline, then `.commit_working_dataset()`, which sets the working dataset and types, clears pending, flags Explore and snapshots with `.snapshot_last_applied_specs()`. Session load (§N3.5) calls `.commit_working_dataset()` directly. Do not write a third copy of the commit.

### N3.3 Revert mechanics
`.revert_to_last_applied()` restores `included_columns`, `column_type_overrides`, `column_transform_specs`, `row_filter_specs` from `last_applied_specs` and increments `revert_trigger`. Modules resync on it: column manager via `updateCheckboxInput`, transform table via `updateSelectInput`, row filters by clearing `registered_cols`.

### N3.4 Transforms
- Specs persist through Apply; `eligible_cols` uses `original_column_types`, so every initially numeric column always appears.
- Pipeline helpers live in `module_column_transform.R` only: `.apply_column_transforms()`, `.make_range_labels()`, `.transform_spec_is_valid()`.
- **Adding a transform type:** (1) new dropdown option in `transform_variables_ui`; (2) new config `renderUI` case; (3) new branch in `.apply_column_transforms()` + `.transform_spec_is_valid()`. No other files need changes.
- Auto-factor and cut-point transforms produce **ordered** factors; Analyze converts them to unordered before fitting (§N6.6).
- **A `numericInput`'s `min` / `max` bound its spinner, not what can be typed.** Anything at all reaches the server, so a box with a real range must clamp in its observer and write the corrected value back with `updateNumericInput()`. The winsorize pair does this through `.winsor_lower()` / `.winsor_upper()`: lower lives in [1, 99], upper in [lower + 1, 100], and raising lower pushes upper up. Two things make that safe - the write-back re-fires the observer exactly once with a value that needs no correction, so it settles rather than looping; and an `NA` (an empty box mid-edit) returns early instead of snapping a number in under the cursor. Both boxes use `updateOn = "blur"` (shiny >= 1.8.1), as do the cut-points Breakpoints and Level labels text boxes; the per-column config `renderUI` reads the spec with `isolate()` (re-rendering only on a method change or `revert_trigger`), or each blur would rebuild the boxes and drop focus so the value is sent on blur or Enter only - with the default per-keystroke update, typing "95" sent "9" first and the clamp rewrote the box under the cursor. `.apply_column_transforms()` clamps as well, because a spec can arrive from `edark_report()` without passing through either box.
- **A removed transform is still a change.** Anything comparing staged transforms against the last applied set must key off `union(names(specs), names(last_tx))`, never `names(specs)` alone — a transform deleted since the last Apply is absent from the staged list and would go untested. This is why `.build_prepare_warnings()` and `.prune_conflicting_filter_specs()` both use the union, and why `.apply_row_filters()` skips a filter whose `type` disagrees with the column as a backstop (`RESOLVED.md`, Prepare, 2026-09-24).

---

### N3.5 Session load (`module_session.R`, `service_session.R`) - §M8
- **Check everything before writing anything.** `.check()` runs the dataset match and `session_prepare_dataset()` (the full pipeline on `dataset_original`) and only then does `.apply()` touch `shared_state`. A refused load must leave the app unchanged.
- **Load does not prune filters.** `.prune_conflicting_filter_specs()` compares staged transforms with `last_applied_specs`; a session's transforms all differ from the current ones, so pruning would drop every filter on a transformed column. Load writes the staged fields, then `.commit_working_dataset()` (which snapshots them as last applied), then bumps `revert_trigger` - the same resync Cancel uses.
- **The Analyze handoff is a payload, not a write.** The session module never writes analysis fields. It sets `shared_state$session_restore`; Step 1's `observeEvent` on it freezes and applies roles, sets `step1_done`, and Step 4 picks up the covariates on the `roles_key` change that follows (its `observe` reads the payload with `isolate()` and clears it). All of this happens in one flush, so navigation is done in `session$onFlushed(once = TRUE)` - by then Step 4 has run and the gate has updated.
- **No `ignoreInit` on Step 1's payload observer.** A launch session (`edark(session = )`) is written by a one-shot `observe` in the first flush; with `ignoreInit = TRUE` the observer's first run could be the one that sees it, and would skip it. `ignoreNULL = TRUE` is the guard. For the same reason `session_server()` is wired last in `server()`.
- **Match on input classes, not EDARK types** - plus a second check that the launch casts produced the same EDARK types, because a character column with few values becomes a factor and with many stays character (`cast_column_types()`). Keep `dataset_input` (the data frame as passed) for both saving and matching; `dataset_original` is already cast.
- **Custom report thumbnails are temp files.** They go into the file as PNG bytes (`thumb_png`) and come back as a new temp file; delete the old items' thumbnails when replacing the list.

## N4 — Explore Internals

### N4.1 Live aesthetics
Aesthetic values are read from `shared_state` inside `current_plot()` in `module_explore_output.R` and merged over the stored spec with `modifyList()` — the spec is not rebuilt. `trend_zero_baseline` is handled the same way. Read each value once (§N1.7).

### N4.2 Trend spec bypasses routing
`build_trend_plot_spec()` sets `plot_type` directly (`trend_numeric` / `trend_factor`) and `req()`s a trend variable. `render_plot()` also dispatches `trend_mean`, which no spec builder produces.

### N4.3 Trend impute-zero
Built with `expand.grid` + `left_join`, filling missing `n` with `0L`; for proportions, a timepoint with `sum(n) == 0` gets 0 via `dplyr::if_else` (not NaN).

### N4.4 Facets and titles
`.apply_plot_aesthetics()` sets the title. All `facet_wrap()` calls use `labeller = ggplot2::label_both` and `as.formula(paste("~", stratify))` — **not** `~ .data[[stratify]]`, which breaks `label_both` and produces `<unknown>` strip labels.

### N4.5 `annotate("label")` ignores `label.colour`
Use `geom_label()` with a one-row data frame and `inherit.aes = FALSE` (as in the scatter correlation label).

### N4.6 `histogram_density` two-panel object
Returns class `edark_two_panel` (`$left` / `$right`). With `split_panels = FALSE` (app) it is combined with patchwork at `widths = c(0.45, 0.55)`; with `split_panels = TRUE` (reports) it returns `list(left_p, right_p)`. Q-Q axes must be symmetric: `ax_range = range(c(z_range, qnorm(ppoints(n))))` applied to both `xlim` / `ylim` of `coord_cartesian()`.

### N4.7 Legend suppression
Applied in `module_explore_output.R` after `current_plot()` returns (not inside the plot function): `violin_jitter` always; `scatter_loess` when stratified.

### N4.8 Each Explore mode publishes every field it owns
Describe and Correlate write the same `shared_state` fields (`primary_variable`,
`stratify_variable`, `bar_display`, ...), so whichever wrote last wins and a pill switch
leaves the other pill's values behind. Each server in `module_explore_controls.R` has a
`publish_state()` that writes **all** of its fields from its own inputs and nulls the ones
it does not own (Describe clears `secondary_variable`), called both when `active_mode()`
becomes its own id and again on its Plot button. Add a field to one mode and it must be
added to both `publish_state()` bodies - set it or clear it - or the leftover comes back.

---

## N5 — Report Internals

### N5.1 Generators and assemblers
`generate_report()` and `generate_custom_report()` (`generate_report.R`) are Shiny-free and share three assemblers: `.assemble_pptx()`, `.assemble_docx()`, `.assemble_html()`. Custom report sections come from `.build_custom_report_sections()`, which traps errors per item and emits a placeholder.

**Progress is two passes on one bar.** Building sections and writing the file (PPT / Word) both count through the sections, so each pass gets its own share: `.progress_span(progress_fn, from, to)` rescales a pass's 0-1 onto the bar, and `.progress_split(format)` says where building ends (0.6 for PPT / Word; 0.9 for HTML, whose `rmarkdown::render()` is one call with no per-section progress). A new pass that reports progress must get a span too, or the bar fills twice.

### N5.2 Split panels
Assemblers detect two-panel plots with `is.list(x) && !inherits(x, "ggplot")` and render each panel independently.

### N5.3 patchwork + `rvg::dml()`
`rvg::dml()` only accepts plain ggplot objects. `histogram_density` uses `split_panels = TRUE` in reports so the patchwork is never built. Any other patchwork reaching an assembler is rasterised via `ggsave()` + `external_img()` / `body_add_img()`.

### N5.4 HTML anchors
`.make_html_anchor(x)` → `"sec-"` + lowercase, non-alphanumerics replaced with `-` — the same logic as `make_anchor()` in `report_template.Rmd`. For `primary_vs_others` the anchor is keyed on the secondary variable name (not the full title) so dataset-summary links resolve. The dataset summary uses a plain `<table>` so the Variable column can hold raw HTML links.

### N5.5 Table One and section tables
Report Table One: `.build_tableone_df()` / `.style_tableone_ft()`. Section tables: `.build_bivariate_fac_fac_table()` etc. All tests via §N2. Table styling: `.style_dataset_summary_ft()`, `.style_section_ft()`.
Report Collinearity: `.build_collinearity_report()` calls Analyze's `compute_collinearity()`, and its heatmaps are `.plot_correlation_heatmap()` / `.plot_cramers_heatmap()` in `service_analysis_plots.R` - the same builders Step 3 renders. Change a heatmap there, not in either caller.

### N5.6 Custom report items
Structure: `list(id, plot_spec, thumb_path, title, added_at)`. PNG thumbnails go to `tempdir()` and are deleted in `session$onSessionEnded` (`edark.R`). Gallery up / down / remove observers follow §N1.9. Navigation: `requested_tab` / `requested_report_subtab` observed in `edark.R`, which calls `bslib::nav_select()` and clears the request.

### N5.7 Word (`.docx`) assembly
`.assemble_docx()` builds on `inst/templates/word_docx_blank_template.docx`, which supplies Title / Subtitle / heading 1-9 styles. Structure: title page, a Word TOC field that populates from the headings (heading 1 = Table 1 / Dataset Summary / the section group, heading 2 = each variable), a running header, a page number bottom-right, and four page sections so the dataset summary is landscape and the rest portrait.

Two traps:
1. **`NUMPAGES` counts within a section**, not the document. With four sections a "Page N of M" footer reports the wrong M, so the footer is "Page N" only.
2. **Word's table autofit wrecks any table wider than the page.** Do not leave widths to Word: `.docx_fit_ft()` computes them in R, and `.docx_shrink_widths()` takes the overflow out of the widest columns only.

Closed to-do with the full history: `RESOLVED.md` (Explore, 2026-09-24).

### N5.8 In-app preview (both pills)
`report_server()` renders the HTML report into `tempdir()/edark-preview-<session token>/<full|custom>_NNN.html`, registers that folder with `shiny::addResourcePath()` under the same prefix, and shows it in an `<iframe>` in the centre pane. Rules:
- **An iframe, never `includeHTML()`.** The report is a full `html_document` with its own Bootstrap theme and scripts; inlined, it restyles the app.
- **A new file name per generation**, so the browser cannot show a cached copy. The previous file is deleted only after the new one succeeds.
- **One argument list per pill.** `full_report_args()` / `custom_report_args()` feed both the download and the preview; the preview stores the list it was built from, and `.preview_is_stale()` is `!identical()` against the current one. The list holds the dataset itself, and `identical()` short-circuits on the same object, so this is cheap.
- **One set of helpers.** `.preview_build()`, `.preview_discard()`, `.preview_view()` and `.preview_save_handler()` take the pill's `reactiveVal`, so the two pills differ only in their `run()` function and in their first tab: both put the view in a Preview tab (`full_result_tabs` / `custom_result_tabs`), beside a read-only Sections list in Full and the editable Items list in Custom.
- The resource path and folder are removed in `session$onSessionEnded`.
- Both pills' progress modals come from `.report_progress_modal()` (§M3.5).

---

## N6 — Analyze Internals

### N6.1 Architecture
- **Consumer only:** reads `dataset_working` and `column_types` at freeze; never writes Prepare or Explore fields.
- **Spec-driven:** `analysis_spec` is the single source of truth for fitting, code generation and export.
- **Result-cached:** `analysis_result` stores fitted objects, tables and plots; export reads the cache.
- **Frozen:** Start Analysis copies `dataset_working` → `analysis_data` with `.edark_row_id`.

### N6.2 Utility functions (`analysis_utils.R`, `service_analysis_pipeline.R`)
- `build_analysis_formula(spec)` — formula from `variable_roles`; mixed models append one `(1 | cluster)` per cluster variable. No random slopes.
- `apply_reference_levels(data, reference_levels)` — `stats::relevel()` per spec before any fit.
- `compute_complete_cases(data, variables)` — `list(data, n_excluded)`.
- `compute_covariate_sample(data, outcome, exposure, covariates, candidates, cluster_vars)` — Step 4's live listwise-deletion summary: row counts (base / fixed / mixed), per-variable row cost, surviving factor levels, EPV, and `issues` (`error`: outcome / exposure / checked covariate left with < 2 levels or no variation; `warning`: EPV < 10, > 20% rows dropped, a cluster with < 2 values).
- `.default_model_design()` — the one place the `model_design` block is built (freeze and resets). `.default_purpose_specification()` — the same for `purpose_specification` (freeze only; a role reset keeps it).
- **Train/test rows:** `analysis_split(spec)` → `list(variable, training_level)` or `NULL` (a split applies only for purpose "prediction" with `validation_method == "split"` and both fields set; `analysis_validation(spec, mixed)` gives the method — "none" / "split" / "cv" / "bootstrap" — with `validation_settings` defaults filled); `analysis_split_rows(spec, data)` → logical `training` / `test` vectors, `test_levels`, `n_missing`; `analysis_model_data(spec, data)` → the training rows (or all rows); `analysis_test_data(spec, data)`. **Anything that learns from the data goes through `analysis_model_data()`** — Step 3 services, Step 4's `compute_covariate_sample()`, the Summary's covariate section, `fit_analysis_model()`, Model › Diagnostics sample accounting. `validate_analysis()` subsets itself, so callers pass the whole frozen dataset. Table 1 deliberately uses all rows.

### N6.3 Step gating (`module_analysis_main.R`)
Six steps (`step1`…`step6`). **Step 5 · Model** nests five sub-tabs in a `navset_underline` (id `model_tabs`): Summary / Create / Diagnostics / Performance / Results. Step 6 is Export.

Steps 1–4 are always reachable; Step 5 opens once the dataset is frozen with an outcome assigned; within it, Summary and Create are always open, while Diagnostics / Performance / Results and Step 6 open once `analysis_result$fitted_models$primary_model` exists. `.apply_gate(navset_id, unlocked)` drives both navsets: locking adds Bootstrap's `disabled` class to the nav link via `shinyjs::toggleClass`, with a tooltip on the parent `<li>`, and falls back to the furthest open tab if the current one locks. The debug button keys Step 5 as `step5_<subtab>`.

The modules are unchanged siblings; `module_analysis_modelspec.R` exposes two UI fragments (`analysis_modelspec_summary_ui()`, `analysis_modelspec_create_ui()`) under one namespace.

### N6.4 Roles and Step 1 (`module_analysis_setup.R`)
- `variable_roles`: `outcome_variable`, `exposure_variable` (radios); `candidate_covariates`, `cluster_variables` (checkboxes). No subject ID, time or random-slope fields. Any non-datetime column may be a cluster (converted with `factor()` at fit time). Clusters commit the analysis to a mixed model; a non-mixed model with clusters is the error `PF_CLUSTERS_UNUSED`.
- `RADIO_ROLES` / `MULTI_ROLES`; JS mirrors them with `.edark-role-radio` / `.edark-role-checkbox` carrying `data-role`. Checking any role clears the variable's other roles. Cluster cells render "—" for datetime columns.
- A role change asks "Clear Analysis Results?" when `analysis_result` is non-NULL **or** `final_model_covariates` is non-empty; confirm → `reset_analysis_pipeline(shared_state, 1)` then apply.
- **Cancel undoes the click:** `.push_roles_to_table()` sends `roles_state` via the `sync_roles` message; JS restores every radio / checkbox / ref-level. The same push runs after every applied change and on reactable remount.
- `.sync_spec()` writes only when a Step 1-owned field changed, sets `final_model_covariates` to `NULL`, and bumps `specification_metadata$roles_version`. Freeze creates `roles_version = 0`, `step1_roles`, `created_at`, `dataset_signature` (sha256 — see §A3.2 note) and `prepare_snapshot`.
- Study type values: `exposure_outcome`, `risk_factor`, `descriptive_exposure`, `descriptive` (§A1.4).
- **Model purpose** (§A1.4a): `output$purpose_ui` renders from the spec (isolated) whenever `purpose_trigger` bumps — after every role write in `.sync_spec()` (refreshes the split choices) and on Cancel (puts the inputs back). One observer watches all four inputs and builds the proposal with `.purpose_from_inputs()`, which **derives the training level from state**: `input$training_level` keeps the previous variable's value while its select re-renders (§N1.3), so a level that is not a level of the chosen variable falls back to `.guess_training_level()`. Only a change of `analysis_split()` counts as a row change; it asks "Clear Analysis Results?" when variable investigation or a model exists and confirms with `reset_analysis_pipeline(shared_state, 3)`. `.sync_spec()` clears the split variable if it gains a role (assign `list(NULL)` to keep the field, don't `<- NULL` it).

### N6.5 Step 3 (`module_analysis_varinvestigation.R`, `service_analysis_variable_selection.R`)
- No candidates → red banner, Run buttons disabled (`.has_candidates()`); Collinearity shows a placeholder. The services return `NULL` silently in that case, so the UI must guard.
- **Training rows:** the univariable screen, stepwise, LASSO and collinearity get `analysis_model_data(spec, adata)`. A `split_key` reactiveVal clears and (if the pill is open) recomputes the collinearity view when the split changes; the other results are cleared by the step-3 reset.
- **Exposure held:** stepwise uses `~ exposure` as scope floor and null model; LASSO sets `penalty.factor = 0` on the exposure's columns. The exposure is never in `selected_variables`; results carry `held_variables`. The univariable screen stays unadjusted.
- Variable names map from `terms()` labels / `model.matrix` `assign` — never `startsWith()` (a candidate that prefixes another name would steal its terms).
- **Listwise deletion can collapse a factor to one level.** `.prepare_selection_data()` / `.partition_modelable()` exclude such candidates after complete-casing and return `excluded_variables`, `n_used`, `n_total` (amber alert in the UI). Any new fitting path must do the same.
- LASSO uses `cv.glmnet(nfolds = 10)` with no seed — suggestions can vary between runs.

### N6.6 Step 4 (`module_analysis_covariate_confirm.R`)
- Role variables are locked, checked rows at the top; candidates start unchecked; Include header has All / Clear.
- **Training rows:** `model_data()` = `analysis_model_data()` over a `split_spec` reactiveVal (holds only `purpose_specification`), so it does not re-fire on every Step 4 spec write. `sample_info()` and the table's counts use it; `table_struct` includes the split so the table rebuilds when the rows change.
- Row cost: checked → rows it costs now; unchecked → rows lost by adding it; cluster → rows lost to the mixed model.
- Method columns: green ✓ suggested (univariable shows the smallest term p), pink — not suggested, grey not run / `n/a` excluded. **Add** only adds checks; **Replace** swaps the selection (modal).
- Reference-level dropdowns list only levels present in the complete rows; a preferred level that drops out falls back to the first surviving level (amber icon). Clusters get no reference level.
- **Live commit:** writes `final_model_covariates`, `reference_levels` (effective levels) and `variable_selection_specification$selected_variables` whenever the staged selection or its surviving levels change. Errors are written anyway; Step 5's preflight blocks the model.
- **Every edit goes through `.propose()`.** If a model exists, the first edit opens "Clear Model Results?"; Cancel re-sends the current patch; Clear & Continue runs `reset_analysis_pipeline(shared_state, 4)` then applies.
- **Staged selection carries its `roles_key`** (`created_at` + `roles_version`). The reset observer clears the selection when the key changes; the commit observer writes only when `staged()$key` matches. Without this, observer ordering after a Step 1 change could write the old selection into the new spec. (Session load hooks in here — §M8.7.)

### N6.7 Model › Summary + Create (`module_analysis_modelspec.R`)
- Step title "5 · Model Creation" (file and function names keep `modelspec`). Tabs (`navset_underline`): **Summary** then **Run Model**. The model header adds a "Fitted on: training set …" line when split.
- Model type is automatic: `analysis_model_options(spec, data)` returns all four types with `available` / `reason`; an observer writes the one available type to `model_design$model_type`. The dropdown is display-only (`.disable_choice()`).
- **Optimizer** (mixed only, Advanced accordion) is the only setting. Changing it after a fit opens "Clear Model Results?": Clear & Continue → `reset_analysis_pipeline(shared_state, 5)` then write; Cancel → `updateSelectInput` back. `analysis_fit_is_stale()` remains as a safety net.
- **Preflight is live:** `validate_analysis(verbose = TRUE)` on every spec change.
- **Pulse:** Bootstrap gives disabled buttons `pointer-events: none`, so a click on disabled Run Model lands on its wrapper `#run_wrap`; JS adds `.edark-pulse` to `#preflight_box`.
- **Run:** `fit_analysis_model(spec, data)` in the progress modal → `reset_analysis_pipeline(shared_state, 4)` → writes `specification_snapshot`, `run_status` (status, fitted_at, error, n_used, n_total, formula, outcome_event, reference_levels, preflight warnings, run_messages), `fitted_models$primary_model`, `inference_summary$coefficients / fit_statistics / predicted_values`. A failed fit stores `status = "failed"` and no model.

### N6.8 Model fitting (`service_analysis_models.R`)
- Model data: training rows when split (`analysis_model_data()`) → `.prepare_model_rows()`: complete cases over outcome + predictors (+ clusters if mixed) → **ordered factors made unordered** (treatment contrasts, not `.L` / `.Q`) → reference levels → `droplevels` → clusters `factor()`. Model › Performance prepares the test set with the same function, so both are coded identically. `n_total` is the training-set size when split.
- `analysis_fit_is_stale()` compares `.model_inputs()`, which includes `analysis_split(spec)`.
- Coefficients via `edark_coef_table()` (§N2). **Don't use `confint.default()` on a merMod** — `coef()` of a merMod is per-group, not the fixed effects. Terms map to variables via the model-matrix `assign` attribute (`lme4::getME(model, "X")` for mixed).
- Always fit linear mixed models with `lmerTest::lmer` (Satterthwaite p-values exist only because its S3 method is registered), never `lme4::lmer`.
- Warnings / messages captured with `withCallingHandlers` + `tryCatch`, never thrown. Convergence, separation and "Rescale variables" warnings get plain-language hints (`.explain_fit_warning()`); lme4's "boundary (singular) fit" message is replaced by our own singular-fit warning.
- `analysis_outcome_event(spec, data)` → the event level for a binary outcome (the non-reference level).

| UI label | `model_type` | Outcome | Clusters |
|---|---|---|---|
| Linear regression | `linear` | continuous | none |
| Logistic regression | `logistic` | binary factor | none |
| Linear mixed model | `linear_mixed` | continuous | ≥ 1 |
| Logistic mixed model | `logistic_mixed` | binary factor | ≥ 1 |

### N6.9 Model › Diagnostics (`module_analysis_diagnostics.R`, `service_analysis_diagnostics.R`, `service_analysis_plots.R`)
- Sidebar: **Model assumptions** (all ticked, Select all / Deselect all); Run Diagnostics. Main: **Overview** (Warnings card + one card per section with a reading guide from `.DG_HINTS`) then per-section tabs. Prediction performance lives in the Performance sub-tab.
- Checks come from `analysis_diagnostic_options(model_type)` (`id, label, description`) for the *fitted* model's type. `run_analysis_diagnostics(result, data, checks)` ignores ids that don't apply — checkbox inputs keep stale values across model types (§N1.3); the `renderUI` uses `suspendWhenHidden = FALSE`. The module passes `analysis_model_data(snapshot, analysis_data)` so sample accounting counts training rows.
- **Linearity** (`.diag_linearity()`): numeric predictors of the model frame only; linear models facet residuals (conditional for mixed) against each; logistic models call `performance::binned_residuals(model, term = v, residuals = "response")` per predictor and report the share of bins inside the bounds. No continuous predictor → a note, no tab.
- Always computed: sample accounting; fitting warnings (`optinfo$conv$lme4$messages` minus the singular notice; `isSingular()`).
- Logistic binned residuals use `residuals = "response"` (performance's default deviance residuals don't average to 0). Influence for non-mixed only. Separation for both logistic types (glmer: on `formula(model, fixed.only = TRUE)`). Random effects from `VarCorr` (**`performance::icc()` returns NA once any component is singular**); residual variance σ² (linear) or π²/3 (logistic).
- Mixed models still have **no influence check** — cluster-level (leave-one-cluster-out) influence is a to-do.
- Stored in `analysis_result$diagnostics`, `result_plots$diagnostic_plots`, `result_tables$diagnostic_summary` (long `metrics` table: section, key, label, value, format, level), `inference_summary$influence_measures`. Advisory — never gates Performance, Results or Export.

### N6.9a Model › Performance (`module_analysis_performance.R`, `service_analysis_performance.R`)
- Sidebar: **Measures** (`analysis_performance_options(model_type)` → `id, label, description`, all ticked), **Validation** (Apparent always, plus the live Step 1 method with its settings — Folds + Repeats for CV, Resamples for bootstrap, Random seed — written live to `validation_settings`), Run Performance.
- `run_analysis_performance(result, data, checks, validation)` takes the **whole** frozen dataset and re-derives the test rows from the snapshot's split (`analysis_split_rows()`).
- Sets: `apparent` (model frame + `predict()`) always; `test` when split — `.perf_test_rows()` runs `.prepare_model_rows()` with no clusters (marginal predictions don't need them), drops rows whose factor level the model never saw (warning, with the levels), then re-levels every factor to the model frame's levels. Mixed models: `re.form = NA` for both sets.
- Test set only: calibration intercept (logistic `glm(y ~ 1, offset = lp)`; linear mean residual) and slope (`glm(y ~ lp)` / `lm(y ~ pred)`). `.calibration_bins()` (decile bins, Wilson CI) lives here now.
- Plot titles carry a short set label via `.ap_title()` (e.g. "ROC curve (test set)"). Keep titles short — the plots sit two to a row and long titles are clipped.
- **Validation sets** (Phase 7b, 2026-09-19): `cv` — k folds × r repeats, stratified by outcome (logistic) or whole clusters of the first cluster variable (mixed; k capped at the cluster count); measures computed on each repeat's pooled out-of-fold predictions, averaged, with SD across repeats. `bootstrap` — Harrell optimism correction (apparent − mean(resample − original)); an `optimism` table and a lowess calibration curve, apparent vs bias-corrected; mixed models resample whole clusters (a cluster drawn twice becomes two). Failed refits are skipped and counted.
- **Spec fields:** `purpose_specification$validation_method` ("bootstrap" default / "cv" / "split") replaced `use_split`. `validation_settings` (`cv_folds`, `cv_repeats`, `bootstrap_reps` — NULL = 200, or 100 for mixed — `seed`) is created at freeze and written live by the sidebar. That observer reads only the current method's inputs (removed inputs keep stale values, §N1.3) and writes nothing when a value equals the effective setting, so a re-rendered input never turns a NULL default into a stored number.
- **Engine:** `analysis_performance_job()` does the apparent (and test) sets immediately and draws every resample up front inside `.with_seed()` (restores `.Random.seed`), so results don't depend on how the steps are batched. `.perf_job_step()` does one refit; `.perf_job_finish()` summarises. Resampling works on `model.frame(primary_model)` and refits with `.fit_engine(..., satterthwaite = FALSE)`; factor levels absent from the refit rows are dropped first (an empty level gives an NA coefficient and a silently wrong prediction). CV drops left-out rows whose level the fold never saw (counted); a bootstrap resample that cannot predict every original row is skipped (counted). `.fast_auc()` (Mann-Whitney) per resample; pROC only for the reported apparent / test AUC with its DeLong CI.
- **Cancel:** Shiny cannot see a click while an observer runs, so the module holds the job in a plain environment and an `observe()` with `invalidateLater(10)` runs refits for `.PM_TICK_SECS` (0.4 s) per tick. Cancel (`.analysis_progress_modal(cancel_id = )`) sets a flag read at the next tick. On finish the result is stored only if `run_status$fitted_at` is unchanged. Mixed models with more than `.PERF_MIXED_FIT_WARN` (100) refits get a Back / Proceed modal first.
- **Timings on liver_tx (440 rows):** glm ~2.5 ms per refit (200 resamples < 1 s); glmer with 12 centres ~0.75 s per refit (100 resamples ≈ 75 s).
- Stored in `analysis_result$performance` (sets, metrics, messages, split, basis, validation), `result_plots$performance_plots` (`list(apparent, test | cv | bootstrap)`), `result_tables$performance_summary` (long: set, key, label, value, sd, format). Results' fit statistics, the Summary tab and the methods paragraph read `performance`. Module helpers use the `.pm_` / `.PM_` prefix (`.PF_` belongs to the preflight validator). Advisory — never gates Results or Export.

### N6.10 Model › Results (`module_analysis_results.R`)
- Outputs from `.RESULTS_OUTPUTS`. **Ticked = generated**; unticked outputs are set to NULL so Step 6 (Export) cannot export them. Adding an output: an entry in `.RESULTS_OUTPUTS`, a branch in the Generate handler, a tab in `output$tabs_ui`.
- Summary tab always present, computes nothing.
- `fit_unadjusted_models(result)` refits one model per variable on `model.frame(primary_model)` with the same engine / random intercepts / optimizer. Status per variable: ok / warning (†) / failed (‡), shortened by `.short_fit_warning()`.
- `build_results_table(result, unadjusted)` is the single source for the table; `results_table_gt()` (app) and `results_table_flextable()` (Word) only format it. **Not gtsummary** — `tbl_regression()` needs broom.helpers and would re-tidy our numbers.
- `build_forest_plot(tbl)` — patchwork of labels | CIs | text; `attr(, "n_rows")` sets height. `build_methods_paragraph(result, include_unadjusted)` — software sentence reads installed versions.
- Stored: `result_tables$main_results`, `result_tables$fit_statistics`, `result_plots$coefficient_plot`, `methods_paragraph`, `fitted_models$univariable_models`, `results_generation`.

### N6.11 Summary (`service_analysis_summary.R`)
`build_analysis_summary(spec, result, data, validation)` → sections `list(id, title, rows)`, rows `list(label, value, items, level)`. Current state only. Data preparation comes from `prepare_snapshot`.

### N6.12 Validator (`service_analysis_validation.R`)
`validate_analysis(spec, data, tier = "full", verbose = FALSE)` — pure. Returns `validity_flag` (`valid` / `warnings` / `invalid`), `messages`, `display_messages`, `checks_run`, `passed`. Codes and tiers: §A8.2. Tier 1 complete-cases over outcome + exposure; Tier 2 adds predictors, plus clusters only when mixed.

**Split first:** when `analysis_split(spec)` applies, `PF_SPLIT_INVALID` (returns early), `PF_SPLIT_NO_TEST` and `PF_SPLIT_MISSING` run before Tier 1, then `data` is replaced by the training rows for every other check; `PF_SMALL_TEST_SET` (Tier 2) looks at the complete test rows.

**Adding a check:** call `.ran("PF_CODE")` where it is evaluated, add a pass label to `.PF_PASS_LABELS`, and if it is a stricter variant of another code map it in `.PF_GROUP_HEAD`. `PF_LOOKS_CATEGORICAL` threshold: `.PF_CATEGORICAL_MAX_VALUES` (10).

### N6.13 Pipeline reset (`service_analysis_pipeline.R`)
`reset_analysis_pipeline(shared_state, from_step)` never shows its own modal.

| `from_step` | Clears |
|---|---|
| `1` | Entire `analysis_result`; resets `variable_selection_specification` and `model_design`; `final_model_covariates` → `NULL` |
| `3` | Everything `4` clears, plus `variable_investigation`, `result_tables$univariable_screen`, `result_plots$collinearity_plots` (the training rows changed). Table 1 and the spec are kept |
| `4` or `5` | Fitted and unadjusted models, run status, result tables / plots (incl. `main_results`, `fit_statistics`, forest plot, `diagnostic_plots`, `performance_plots`, `performance_summary`), inference summary, `diagnostics`, `performance`, generated script, methods paragraph, `results_generation`. Spec untouched |

---

## N7 — Test Data (`liver_tx`)

500 × 36, regenerated by `Rscript data-raw/liver_tx_sample.R` (seeded).

- **Outcomes:** `ead` (logical → 2-level factor, ~28% prevalence) for logistic; `postop_los_days` for linear.
- **Cluster:** `transplant_center` — 12 unbalanced centres with real random intercepts. Deliberately above `PF_FEW_CLUSTERS` (< 10) and below `PF_UNBALANCED_CLUSTERS` (size CV > 1); induce both with a row filter.
- **Collinearity tiers:** `preop_meld` ↔ `preop_meld_na` (r 0.92, VIF ~10); `recipient_bmi` = weight / height² (VIF 41–151); `intraop_ebl_ml` ↔ `intraop_rbc_units` (r 0.92); `preop_meld` vs its three component labs (VIF ~5 — MELD is on the log scale, so only moderately *linearly* redundant).
- **Noise block (zero effect on every outcome):** `donor_blood_type`, `donor_height_cm`, `or_room_number`, `surgery_start_hour`, `preop_ferritin`, `referral_source`. Backward / forward stepwise (BIC) and LASSO (`lambda.1se`) all retain 0 of 6.
- **Weak but real:** `preop_sodium` survives a liberal univariable screen, dropped by BIC — tests p-threshold sensitivity.
- **Missingness:** `preop_ferritin` 35% (trips `PF_MISSING_GT20`), `donor_age` 12%, `preop_albumin` 8%, `intraop_max_lactate` 5%, `preop_inr` 3%. `postop_aki_stage` NA means *no AKI*, not missing — including it in a model guts the complete-case n.

---

## N8 - Export Internals (`module_export.R`, `service_export.R`, `service_export_report.R`, `inst/www/edark_export.js`)

Spec: §A10 and `BUILD_Export.md`. Built 2026-10-05; verified the same day in R 4.3.3 (all four model types, every file reopened, the page driven with `chromote`).

### N8.1 One registry, read by the tree and by the build
`export_items(st, data_format, report_format)` returns one row per file the zip can hold, with its status. The tree renders it and `export_job()` writes from it, and the job writes only rows whose status is `"available"`, whatever the selection says - so a stale output can never be exported, even through a selection kept from before it went stale. Add a new exportable output by adding a row in `export_items()` and a `kind` branch in `.export_write_item()`; nothing else needs to know.

`st` is a plain list from `export_state()`, so everything in `service_export.R` runs without Shiny (the registry, data / text writers, notes builders and README were exercised that way with base R; the Word / PNG / zip writers were not run).

### N8.2 Ids have no extension
An item's `id` is its zip path without the extension (`data/working_dataset`); `path` has it. The data and report formats change the extension, and the selection is keyed on `id`, so switching RDS → CSV keeps the tick.

### N8.3 The tree: the browser owns ticks, the server owns what to render
The tree is plain HTML (`.export_tree()`); folders are native `<details>`. `edark_export.js` handles every tick - folder boxes tick their files and show all / some / none (`indeterminate`), counts update - and reports the ticked ids as `input$selection` and the open folders as `input$open`. Nothing round-trips on a tick.

- **Re-render only when the registry changes.** `output$tree` depends on `tree_key` (ids, file names, statuses, reasons), set through an `identical()` guard (§N1.2). It renders from the server's `sel` and `open_dirs`, so a re-render (Diagnostics just ran, the format changed) keeps ticks and open folders.
- **Defaults before the first render.** An id is ticked the first time it becomes available (if `default`). `.take_defaults()` runs in an observer *and* inside `renderUI` before building the tree: if the tree rendered first with nothing ticked, the browser would report the empty selection back and wipe the defaults.
- **Unavailable ticks are kept server-side.** `input$selection` only ever holds available ids (disabled boxes are not reported), so the server keeps any tick on an id that is not available now; when it is re-run it comes back ticked.
- `toggle` does not bubble - the JS listens in the capture phase. A checkbox inside `<summary>` may or may not toggle the folder depending on the browser; the JS restores the folder's open state after the click either way.
- The JS initialises each newly rendered tree on `shiny:value` (marked `data-ready`), which also reports its selection once.

### N8.4 Build & Download: ticks, then a programmatic click
The build is a job (`export_job()` → `.export_job_step()` × n → `.export_job_finish()`), one file per step, advanced by an `invalidateLater` observer for `.EXPORT_TICK_SECS` per tick with a Cancel button - the Model › Performance pattern (§N6.9a). Progress uses the shared `edark_analysis_progress` modal (§N1.10).

Progress cannot come from inside a `downloadHandler` (its messages are not flushed until the download ends), so the build runs first and the download is started for the user: the server enables the toolbar's Download Last Build button and sends `edark_export_download`, whose handler clicks it. That button has `suspendWhenHidden = FALSE`, otherwise its `href` is not set while the page is hidden. One build is kept (temp dir); the previous one and the last one at session end are deleted.

A file that fails to write does not stop the build: it is skipped, listed in `README.txt` under "Not exported" and in the page's messages.

### N8.5 Staleness
`analysis_output_status(spec, result, prepare_changed)` (`service_analysis_models.R`) is the one place that decides. `prepare_changed` compares the sha256 of `dataset_working` with the frozen `dataset_signature`, as Setup does; the page computes that digest in its own reactive so it is not re-hashed per tick. Performance compares only the validation settings its method uses (`.validation_matches()`), so changing CV folds does not stale a bootstrap run.

### N8.6 Notes documents and the report share builders
Each folder's notes document is built from sections in `build_analysis_summary()`'s shape (`.export_notes_*()`), written by one sections → Word writer (`.export_add_sections()`); the compiled report reuses the same sections and the same flextable builders. The report is built once as blocks (`.export_report_blocks()`) and rendered to Word (template + `.docx_*` helpers from `generate_report.R`) or HTML (`htmltools`, base64 PNGs, `flextable::to_html()` - no pandoc).

### N8.7 Collinearity matrices are stored
Step 3 now stores `cor_matrix` and `cramers_v_matrix` in `result_plots$collinearity_plots` beside `flagged_pairs_table`, so Export draws the heatmaps without recomputing. A Step 3 run from before this change has no matrices; the heatmap items are then not listed until Collinearity is opened again.


### N8.8 Traps found verifying it
- **gtsummary's `as_flex_table()` needs flextable >= 0.9.11**, newer than the last R 4.3 binary (0.9.7 / 0.9.8), and the version check fails inside a promise, so `tryCatch` does not reliably catch it (the first table failed, later ones did not). `.export_table1_ft()` checks `packageVersion("flextable")` and otherwise builds the table from gtsummary's display tibble, stripping its `**` / `__` bold markers.
- **htmltools drops `tags$head` from `as.character()`** (it hoists head content for Shiny pages), so a page built as `tags$html(tags$head(...), tags$body(...))` loses its CSS and charset. `export_report_html()` writes the head as text.
- **A download button is an `<a>`.** `shinyjs::disabled()` neither greys it nor stops the click, and Shiny strips Bootstrap's `.disabled` from a download link as soon as its handler is ready. Download Last Build uses its own class, `.edark-export-no-build` (`pointer-events: none`), which the download trigger removes.
- An unzipped export under a deep folder can pass Windows' 260-character path limit (`variable_selection/tables/collinearity_flagged_pairs.docx` is the longest path inside the zip).
