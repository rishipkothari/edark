# EDARK v0.2 - UI Consistency Plan

> The single UI plan. It merges the Claude assessment and the competing Codex assessment
> (the Codex document is kept at `archive/BUILD_UI-redesign_CODEX.md` for history only).
> Work one stage per session and update the status table below as part of each stage.
>
> **Scope rule:** this is an academic project, not a shipped product. Every stage is
> achievable with `bslib` + plain R + plain CSS. **No SCSS / Sass and no build step.** CSS
> lives in one file (`inst/www/edark.css`); it may grow past the original ~80-line estimate
> when a rule is genuinely needed, but prefer a Bootstrap utility class or a `bslib` argument
> first. **Zero new JavaScript.** The existing reactable-embedded-input JS (§N1.4) and the
> progress-message handlers are the only JS and must not be touched.

## Status

| Stage | Scope | Effort | Status |
|---|---|---|---|
| 0 | Quick fixes that do not wait for the redesign | S | done 2026-09-22 (2 of 3, see §4) |
| 1 | Honest locking and one precondition affordance | S-M | done 2026-09-23 |
| 2 | Component library (`R/ui_helpers.R`) + Explore report aesthetics | M | done 2026-09-23 |
| 3 | Theme file, dark mode, nav polish (navbar, pills, stepper, font) | S-M | done 2026-09-23 |
| 4 | Page contract: config / result / info panes + messages area | M-L | done 2026-09-23 |
| 5 | Flatten navigation + one nav vocabulary (§1.2) | M | done 2026-09-23 |
| 6 | Small fixes: empty states, density, copy, dialog actions, button sweep | S | done 2026-09-23 |

Order: 1 first (the highest-value fix, and the user's priority). 2 and 3 are independent of
each other. 4 depends on 2. 5 is independent but touches `edark.R` and
`module_analysis_main.R` routing. 6 depends on 2.

**Revised 2026-09-23** after the user drove the app: D8 amended, D9-D11 added, and Stages 2,
3, 4, 5 and 6 all gained scope. The feedback and the principles taken from it are in §1.3 -
read it before starting any stage.

No screenshot baseline is needed before starting (decision D4).

---

## 1 - Decisions

Settled 2026-09-22. Do not reopen without the user.

| # | Decision |
|---|---|
| D1 | **Reports stay separate and stay where they are.** Explore › Report remains a sub-tab of Explore; it is *not* promoted to a top-level tab. Analyze › Step 6 Export is a different thing with a different job (see §1.1). The two share no UI. **Amended 2026-10-05:** Export is now the top-level **4 · Export** page (it works without any Analyze work - `BUILD_Export.md` X1); it still takes nothing from Explore (X2). |
| D2 | **Keep dedicated areas for config and for "what this results in".** Settings go in a left sidebar; a right-hand info pane says what the current settings produce (sample size, pending changes, counts, checks). This generalises what Setup and Covariates already do. See Stage 4. |
| D3 | **A dedicated messages area, separate from the info pane.** Warnings, errors, blockers and stale notices always appear in the same place, and never mixed into the neutral info pane. |
| D4 | **Honest locking first.** Stage 1. No screenshot baseline. |
| D5 | **CSS only.** No SCSS. More CSS is acceptable if needed. |
| D6 | **Sidebars on every page, same default width.** Including Model › Summary and Step 6 Export. Left config sidebar: 340 px. Right info pane: 300 px. Both are constants in `R/ui_helpers.R`. |
| D7 | **Explore's report uses the aesthetics on screen.** Report must not have its own aesthetics controls. See Stage 2 and D9. |
| D8 | **One navigation vocabulary for the whole app** (decided 2026-09-23): each nav level has one look everywhere. **Amended 2026-09-23 (user feedback, §1.3):** level 3 is no longer a single look. Modes at the top of the left config pane stay **pills** (Explore's Describe / Correlate / Trend - the user likes these and wants them kept). Anything a level deeper keeps **underline tabs across the top of the page** (Analyze Step 3 and Step 5, and Explore › Report's Full / Custom). **Amended again 2026-09-23** when built: the split is by *depth*, not by whether the sub-step owns its own output. Report's Full / Custom was named for the config pane, but D11 leaves Custom with no result surface, so the two readings collided; the user ruled that the third nested level should look the same everywhere. See §1.2 and Stage 5. |
| D9 | **One aesthetics control set, in one place** (2026-09-23). Aesthetics are shared by every Explore mode and by both report modes, so there is exactly one copy of the controls and one set of stored values (`shared_state$ggplot_theme`, `color_palette`, `show_legend`, `legend_position`, `show_data_labels`). It is not a mode of the analysis and it is not staged - it applies live to everything on screen and to both report flows, so stacking it inside a mode's config stack both duplicates it per mode and re-creates the staged-vs-live collision (§2.6). **Amended 2026-09-23** (user choice, on the "tab or a dialog" option in §1.3h): the controls live in **their own panel of the Explore config pane**, not in a dialog. A dialog would cover the plot the controls change; a panel leaves it visible. It is the fourth panel in the same pill row as Describe / Correlate / Trend rather than a tab level above them, because a level of its own would nest tabs inside pills - the depth problem in §2.3. Appearance is therefore *in* the mode row without *being* a mode: the three modes stage their pickers until their button is clicked, and the Appearance panel applies live. See "Built 2026-09-23" under Stage 2. **Superseded in principle 2026-09-23:** the user has since decided Appearance should be promoted to a level-2 page pill beside Explore Data and Report, with the controls in the main panel and possibly renamed "Settings" (see the root CLAUDE.md to-do). The argument below - that a tab level of its own would nest tabs inside pills - only applied while it sat *inside* the Explore page; as a sibling page it is level 2 and nests nothing. Amend D9 when that lands. |
| D10 | **One button scale, and placement follows the action's scope** (2026-09-23). Scale: a config-pane action is full width at default size (primary filled, secondary outline); an action on an already-produced artefact is `btn-sm btn-outline-*` in a right-aligned toolbar above that artefact; dialog actions are default size. No `w-75`, no unclassed `input_task_button`. Placement: actions that change the configuration live in the config pane; actions that act on the produced artefact (Save Plot, Copy to Clipboard, Add to Custom Report, Appearance…) live with the artefact; the info pane never holds an action. |
| D11 | **A page whose only deliverable is a file has no centre pane** (2026-09-23). Explore › Report › Custom drops its main-panel "Preview" card entirely - it restated the item list and showed nothing the item list did not. The page is config (left: the item list itself, then format, then Generate) plus info (right: what the generated file will contain). Deliberate exception to Stage 4's three-place contract; see Stage 4. |

### 1.1 Explore report vs Analyze export (D1)

| | Explore › Report | Analyze › Step 6 Export |
|---|---|---|
| What it is | A compiled document of plots and summary tables | The outputs of one analysis |
| Modes | **Full** (prespecified: every selected variable, fixed layout) and **Custom** (user-curated items added from Explore) | Individual files **and** a compiled document |
| Output | One PPTX / DOCX / HTML file - compilation only | A zip of individual files (data, spec, tables, figures, diagnostics) plus an optional compiled report |
| Lives in | Explore (sibling of Plot) | Analyze, last step |

Both pages follow the same page contract (Stage 4). Sharing low-level writers (flextable
styling, file assembly) below the UI layer is fine; sharing screens is not.

### 1.2 Navigation vocabulary (D8)

Today the app uses five nav looks, and the same level is drawn differently in each stage:

| Level | Meaning | Prepare | Explore | Analyze |
|---|---|---|---|---|
| 1 | Stage | navbar | navbar | navbar |
| 2 | Page within a stage | card tabs (Columns / Transforms / Filters / Preview) | plain tabs (Plot / Report) | pills (Steps 1-6) |
| 3 | Sub-step / mode of a page | - | **pills inside the sidebar** (Describe / Correlate / Trend); pills in the main panel (Report: Full / Custom) | **pills inside pills** (Step 3); **underline tabs** (Step 5) |
| 4 | Views of one result | tabs inside card tabs (Data Preview) | - | card tabs (Diagnostics, Performance, Results) |

Explore's sidebar pills, Report's Full / Custom pills and Analyze Step 3 / Step 5 sub-steps are
the same kind of thing - a mode of the page that changes both the settings and the output -
and are drawn three ways.

**Decided: one look per level, everywhere - with level 3 split by whether the sub-step owns
its own output (D8 as amended).**

| Level | One look | Notes |
|---|---|---|
| 1 Stage | navbar | Restyled in Stage 3 (light bar, active underline). |
| 2 Page | **pills**, in all three stages | Analyze's pills also get stepper states (number, done, current, locked - Stage 1 sets the state, Stage 3 styles it). Prepare and Explore pills are unnumbered: their pages are not a gated sequence. |
| 3a Mode of a page | **pills at the top of the left config pane** | One level down, inside the config pane, switching only the settings. Applies to Explore (Describe / Correlate / Trend) and the Appearance panel beside them. |
| 3b Sub-step, one level deeper | **underline tabs across the top of the page** | The third nested level, whatever it contains. Applies to Analyze Step 3 (Univariable / Collinearity / Stepwise-LASSO), Step 5 (Summary / Create / Diagnostics / Performance / Results) and Explore › Report (Full / Custom). |
| 4 Result view | card tabs, only here | Data Preview's inner tabs are removed (Stage 5). |

**Level 3 - revised 2026-09-23 (§1.3).** The 2026-09-23 decision was one look (underline tabs)
for everything at level 3. User feedback the same day: *"Pills inside LHS pane for describe,
correlate, trend - I kind of like these. Keep them for now."* The split above is the smallest
honest rule that keeps them: the *distinction that matters* is not depth but whether a
sub-step owns its own output. Explore's three modes feed one plot panel, so the mode selector
belongs with the settings it switches; Analyze's sub-steps each produce a different artefact,
so the selector belongs over the page.

Consequences: Report's Full / Custom pills (`module_report.R:47`) move from the main panel
*into* the left config pane rather than becoming underline tabs, and Explore's existing
sidebar pills (`edark.R:150`) stay put and are simply restyled (Stage 3) to match Report's.
Level 3a and 3b are styled as one family (Stage 3) so this reads as two placements of one
idea, not two idioms. Revisit in Stage 5 once both look settled: if 3a and 3b still feel
like different things on screen, reopen with the user.

Options considered earlier for level 3 (kept for reference):

- **A. Underline strip across the top of the page.** Now 3b only. Matches Step 5 today. For
  Explore it would have sat above the page layout and driven a `navset_hidden()` in the
  sidebar via `nav_select()` (pure R) rather than each sub-step owning a whole page.
- **D. Sections in the left sidebar** (the user's idea, 2026-09-22; not chosen, kept for reference).
  Each sub-step is a collapsible section (`bslib::accordion(multiple = FALSE)`) holding that
  sub-step's settings and its own primary button; the open section decides what the centre
  shows; the right info pane stays fixed for the whole page ("what this step hands on").
  Fits Explore most naturally (it is nearly what Explore does today, minus the pills).
  Costs: each Analyze sub-step module splits into a config part and a result part
  (`module_analysis_modelspec.R` already is); sub-step locking moves into the section body;
  the "one primary action" rule becomes one per open section; Model › Summary has almost no
  settings; check long sections (Performance, LASSO) at 1280 x 800.

(Earlier options B - a radio mode selector for Step 3 only - and C - promoting sub-steps to
steps, bringing back nine pills - were dropped.)

### 1.3 User feedback, 2026-09-23

Raw feedback from driving the app. The principles below are the durable part; the detail
table says where each item lands.

**Principles extracted** (these bind every stage, not only the pages named):

| # | Principle |
|---|---|
| F1 | **One button scale.** Two sizes only: full-width default-size for a config-pane action, `btn-sm` for an action on an already-produced artefact. Everything else is a bug. (D10) |
| F2 | **Placement follows scope, not convenience.** Config actions → config pane. Artefact actions → a toolbar with the artefact. Facts about the result → info pane. The info pane never holds an action; the config pane never holds a read-only fact that the info pane should carry. (D10) |
| F3 | **The primary action sits at the bottom of the config pane, in every mode of every page.** No mode may put it somewhere else. |
| F4 | **A setting shared by several screens has one copy and one home**, and one set of stored values that every screen and every export path reads. Duplicating a control per mode is how the app got four aesthetics accordions. (D9) |
| F5 | **Never render a surface that restates its own controls.** A "preview" that shows the list the user just built is dead weight - delete it and give the space to the thing that is actually produced. (D11) |
| F6 | **Actions go at the top of a scrolling dialog**, where they stay reachable, not below a list of unknown length. |
| F7 | **Every pane must earn its width.** If the info pane has nothing factual to say on a page, that is a sign the page is missing information the user wants - fill it, don't drop the pane. Stage 4's per-page table must name real content for every page. |
| F8 | **Labels name what the user sees.** "Explore Data", not "Plot"; "Clear", not "Deselect All"; report type names match Explore's mode names. |

**Detail items**

| # | Feedback | Lands in |
|---|---|---|
| a | Button sizes differ between and within panes - Describe / Correlate are `btn-primary w-100` (`module_explore_controls.R:114,227`) but Trend is an unclassed `input_task_button` (`module_trend_controls.R:50`); Full Report's Generate is `w-75` (`module_report.R:63`) and Custom's is `w-100` (`:262`) | Stage 2 (`edark_button()`), Stage 3 (CSS), Stage 6 (sweep) |
| b | Prepare: Columns / Transforms / Filters / Preview become pills | Stage 5 (already planned - confirmed) |
| c | Prepare: Apply Changes and Reset stay in the left pane | Stage 4 (already left today; the pane is *only* config after the split) |
| d | Prepare: dimensions move to the right info pane, with more information - what the staged transforms and row filters actually are | Stage 4 |
| e | Explore: rename the "Plot" page to "Explore Data" | Stage 6 |
| f | Explore: where do Save Plot / Copy to Clipboard / Add to Custom Report go? | Stage 4 - **answered**: they stay as the right-aligned `btn-sm` toolbar above the plot (`module_explore_output.R:28-46`). They act on the produced plot, so by F2 they belong with it; the info pane is barred from holding actions (D2) |
| g | Explore: the right pane could say something about custom report items | Stage 4 - info pane = variable summary **plus** a custom-report line (n items; whether this plot is already added) |
| h | Explore: aesthetics are shared, so make them a tab or a dialog - one copy, one set of values, applying to everything on screen and to both report flows | D9, Stage 2 - **dialog** |
| i | ~~Report › Full: report type "Describe Variables" → "Describe Variable"~~ - **withdrawn 2026-09-23**: the plural is correct, the report describes every variable picked in the selection dialog. No change | - |
| j | Report › Full: the variable-selection modal puts Select All / Clear and Done / Cancel at the **top** | Stage 6 |
| k | Report › Full: output format and Generate move to the bottom of the left pane | Stage 4 / Stage 6 |
| l | Report › Custom: the item list with its delete buttons is the main presentation and lives in the left pane; the main-panel "Preview" card is redundant - delete it | D11, Stage 4 |
| m | Report › Custom: the right pane shows report summary details (item count, format, …) | Stage 4 |

---

## 2 - Assessment

*(The durable part of this document. Line numbers are as of 2026-09-21 and drift; search
for the quoted code if a line has moved.)*

### 2.1 What already works (keep it)

- Prepare, Explore and Analyze are distinct tasks; Analyze is a gated, ordered workflow.
- Computation happens on an explicit action, not on every input change.
- Modules talk through `shared_state`, not through each other's UI.
- Setup and Covariates put configuration in the table where it belongs (one row per
  variable) and a live summary beside it. D2 makes this the pattern for every page.
- The same plot and statistics services drive both the app and the exported output.

### 2.2 No page contract

| Symptom | Evidence |
|---|---|
| Sidebar side flips mid-workflow | `module_analysis_setup.R:138` and `module_analysis_covariate_confirm.R:147` use `position = "right"`; every other page uses left |
| Sidebar vanishes | Model › Summary (`module_analysis_modelspec.R:80`) and Step 6 (`module_analysis_export.R:18`) have none |
| Six sidebar widths | 400 (Prepare, Explore), 380 (Report), 405, 390, 360, 340 - three different widths inside Step 3 alone (`module_analysis_varinvestigation.R:127,164,186`) |
| Config and info mixed in one sidebar | Setup's right sidebar holds config (action buttons, study type, purpose) *and* info (incoming snapshot, role summary, selected snapshot) (`module_analysis_setup.R:140-148`). Prepare's Apply pane holds info (dimensions), warnings, status and actions in one stack (`module_prepare_confirm.R:26-56`) |
| Four main-panel idioms in one tab strip | Prepare's sub-tabs are a `card` (`module_column_manager.R:18`), a bare `div` whose card appears at render time (`module_transform_variables.R:30` vs `:130`), a flat `tagList` (`module_row_filter.R:20`) and a nested `navset_card_tab` (`module_data_preview.R:19`) |

### 2.3 Navigation depth

- **Data Preview is 3 tab levels deep** (navbar > `prepare_tabs` > `navset_card_tab` > `navset_tab`).
- **Analyze › Model › Diagnostics is 9 containers deep.**
- **Three looks for one concept (sub-step / mode):** Explore's modes are pills inside the
  sidebar (`edark.R:150`), Report's are pills in the main panel (`module_report.R:47`), Step 3's
  are pills inside the step pills (`module_analysis_varinvestigation.R:116`), Step 5's are
  underline tabs (`module_analysis_main.R:64`). See D8 / §1.2.
- **Three looks for one level (page within a stage):** card tabs in Prepare (`edark.R:112`),
  plain tabs in Explore (`edark.R:142`), pills in Analyze.
- **Pill styling reaches one place only.** The only nav CSS (`edark.R:48-75`, from commit
  `63ced12`) is scoped to `.sidebar .nav-pills`, so it styles Explore's sidebar pills and
  nothing else; Analyze's step pills and the navbar are stock Flatly.
- **Step pills wrap to two rows** at ~1500 px (long labels, e.g. "3 · Variable Investigation").

(Report being a sub-tab of Explore was listed here as a problem in the first assessment. It
is intended - D1.)

### 2.4 Locking and state - the biggest functional gap

`.apply_gate()` (`module_analysis_main.R:146-165`) adds Bootstrap's `disabled` class and a
native `title=` tooltip to the parent `<li>`. No app CSS styles `.disabled`, so a locked step
is grey text with `pointer-events: none`: clicking it does *nothing*, and the tooltip appears
only after ~1 s hover.

- Lock wording disagrees with the in-panel placeholder for the same condition: tooltip says
  "Model › Create" (`module_analysis_main.R:135`), placeholders say "the Create tab"
  (`module_analysis_diagnostics.R:179`, `module_analysis_performance.R:364`,
  `module_analysis_results.R:163`).
- **Two affordances for one kind of precondition.** *(Corrected while building Stage 1:
  Diagnostics, Performance and Results were in fact already disabled through
  `shinyjs::toggleState()` - but silently, with no reason anywhere on the page, and they
  still toasted for their "nothing ticked" case. Table 1 was the only button that sat
  enabled and answered a click with a toast. The fix is the same either way.)*
  Run is disabled via `shinyjs::disabled()`
  in Step 3 (`:139`, `:201`) and Create (`module_analysis_modelspec.R:106`), but Table 1
  (`:29`), Diagnostics (`:64`), Performance (`:67`) and Results (`:66`) stay enabled and fail
  with a toast.
- **Four status vocabularies** for one severity scale: `alert-*` blocks
  (`module_prepare_confirm.R:125,131,159`), `badge bg-*` (`module_row_filter.R:84-90`),
  `card(class = "border-warning")` (`module_explore_output.R:98-106`) and transient toasts
  (`module_prepare_confirm.R:247`, `edark.R:290`).
- **Staged vs applied is invisible in Prepare.** A staged transform looks identical to an
  applied one (`module_transform_variables.R:104-128`); the pending badge is a bare count
  (`module_prepare_confirm.R:121-136`); blocking validation is a 6-second toast plus a
  forced tab switch (`:245-252`).
- ~~False warning about clearing custom report items~~ - fixed in Stage 0.

### 2.5 Duplication - the cause, not the symptom

No shared UI layer, so every convention is re-typed and drifts.

| Duplicated thing | Copies |
|---|---|
| Uppercase section label `"text-muted small text-uppercase fw-semibold mt-2 mb-1"` | Re-typed ~30x across 11 modules, plus four identical private `.hdr()` helpers (`module_analysis_modelspec.R:92`, `_diagnostics.R:49`, `_performance.R:56`, `_results.R:44`), plus an unused canonical `.sidebar_label()` (`module_explore_controls.R:86-88`), plus Prepare's variant `"mt-2 mb-1 fw-semibold"` (`module_prepare_confirm.R:29`) |
| Aesthetics accordion | 4 copies: `module_explore_controls.R:16-54`, `module_trend_controls.R:55-91`, `module_report.R:132-168` and `:203-239`. The Report copies use `custom_*` ids and are applied at generation time (`module_report.R:836-840`), so the report does not match what Explore showed (D7) |
| Model header card | 4 near-duplicates with different fields: `module_analysis_modelspec.R:380-398`, `_diagnostics.R:185-192`, `_performance.R:370-386` (no formula), `_results.R:170-176` |
| Empty state | Six grammars: `.ms_placeholder()` (`module_analysis_modelspec.R:474-478`), hand-rolled divs in Steps 2-4, no icon in collinearity (`:496-499`), card-with-header in Step 1 (`module_analysis_setup.R:517`), centered stub in Step 6 (`module_analysis_export.R:21-25`), full-width alert in Step 3 tier 1 (`:21-24`) |
| "Fit a model first" | Five wordings (`module_analysis_main.R:135`, `diagnostics.R:108,179`, `performance.R:123,364`, `results.R:163`, `modelspec.R:305`) in two classes (`text-muted` vs `text-danger`) |
| Custom-report modal | Two copies (`module_prepare_confirm.R` and `edark.R`) - merged into one helper in Stage 0 |

Button grammar drifts the same way: `btn-primary w-100`, `w-75` (`module_report.R:63`),
`btn-sm btn-outline-primary` (`module_row_filter.R:30`), unclassed `input_task_button`
(`module_trend_controls.R:50`); labels from "Generate Table 1" to a bare "Run"
(`module_analysis_varinvestigation.R:204`); Generate at the *top* of Full Report
(`module_report.R:60-64`) but the *bottom* of Custom Report (`:259-263`).

The drift is visible without leaving one pane. Explore's three modes put the same kind of
button in the same place at two sizes: Describe (`module_explore_controls.R:110-115`) and
Correlate (`:223-228`) are `actionButton(class = "btn-primary w-100")`; Trend
(`module_trend_controls.R:50`) is a `bslib::input_task_button()` with no class, so it renders
narrower and in the default variant. Confirmed by the user 2026-09-23 (§1.3a). Fixed by one
helper (Stage 2, F1 / D10), not by re-typing the classes a fourth time.

### 2.6 Interaction-model collisions

- **Explore stacks staged and live controls with no distinction.** Variable pickers are
  staged until Describe is clicked (`module_explore_controls.R:181-185`); the aesthetics
  accordion directly below applies live (`module_explore_output.R:112-135`).
- **Explore has no empty state.** Before any plot: an empty 500 px card, an empty summary
  card, and four enabled buttons that answer with a JS `alert()` (`:51`), a thrown download
  error (`:207`) or a toast (`:235`).
- **Controls far from their effect.** The row-count badge scrolls away as filters accumulate
  (`module_row_filter.R:36,40`); a long warnings list pushes Prepare's Apply below the fold
  (`module_prepare_confirm.R:26-56`).
- **No scroll containment** on the column manager (`module_column_manager.R:89-98`) or the
  transform table (`:130-139`); "Select all" scrolls away with the header.
- ~~Collinearity "Threshold" label with no control~~ - fixed in Stage 0.

### 2.7 Visual / theming

- Inline `tags$style` in three places: `edark.R:48-99`, `module_analysis_modelspec.R:41-56`,
  and a global `.form-check` override inside a *render function* (`module_column_manager.R:91-95`).
- Dark mode swaps the whole bootswatch preset (`flatly` <-> `darkly`, `edark.R:342-355`).
- Navbar controls are `rgba(255,255,255,0.75)` on primary - low contrast (`edark.R:90-98`).
- Amber means "transformed" in Data Preview (`module_data_preview.R:48`), "type changed" in
  the column manager (`module_column_manager.R:81`) and "warning" in four other places.

---

## 3 - Not doing

- A persistent left step rail or custom app shell (a router replacing `nav_select()`). The
  first Claude draft proposed one; both assessments rejected it: too much churn, and it
  crowds the 1280 x 800 viewport (§M9.4 NF-05). Stage 4's right info pane is a standard
  `bslib` sidebar, not a shell.
- SCSS / a token build step (D5).
- Promoting Report to a top-level tab (D1).
- A screenshot baseline (D4).
- A density toggle, virtualised tables, or a full WCAG audit. (Focus visibility and the
  navbar contrast fix are in scope.)

---

## 4 - Stages

Each stage is independently runnable and leaves the app working.

### Stage 0 - Quick fixes (done 2026-09-22)

- [x] **False custom-report warning.** The modal said Prepare changes "will clear custom
  report items" and offered "Apply and Clear Custom Report"; nothing clears them. Items store
  only a plot spec and are re-drawn from the working dataset when the custom report is
  generated. The copy now says so, the buttons say "Apply Changes" / "Reset to Original", and
  the two modal copies (`module_prepare_confirm.R`, `edark.R`) share one helper,
  `.custom_items_modal()`.
- [x] **Collinearity "Threshold" label** removed - it headed static text with no input. The
  0.7 cut-off is fixed in `service_analysis_variable_selection.R`; if it should become
  configurable, that is an Analyze feature, not a UI fix.
- [x] **One "fit a model first" wording** - done in Stage 1: the five placeholders and
  the nav popover all read `EDARK_LOCK_REASON["fit_model"]`.

### Stage 1 - Honest locking

Pure R plus the first few lines of `inst/www/edark.css`.

- **Lock-reason table.** One named character vector in `R/ui_helpers.R` (created in this
  stage with only what locking needs) holding every "do X first" message. The nav tooltip,
  the in-panel placeholder and the disabled-button reason all read from it, so they cannot
  disagree. Wording rule: name the destination exactly as the nav shows it ("Model › Create").
- **A locked step explains itself.** Keep `.apply_gate()` for state. Replace the native
  `title=` with `bslib::popover()` on the nav item so a *click* shows the reason. Create
  `inst/www/edark.css` (loaded via `shiny::addResourcePath("edark", system.file("www",
  package = "edark"))` + a `tags$link`) with a `.nav-link.disabled` treatment: lock glyph,
  `cursor: not-allowed`, and **not** `pointer-events: none` so the click reaches the popover.
  If `.apply_gate()` needs to stop the click navigating, do it in R (re-select the previous
  step), not with new JS.
- **Disabled-with-reason is the only precondition affordance.** `edark_run_button(id, label,
  enabled, reason)`: disabled when a precondition is unmet, with the reason shown as small
  text directly under the button (not only in a tooltip). Apply it to Table 1, Diagnostics,
  Performance and Results (currently enabled-then-toast) and to the already-disabled Step 3
  and Create buttons. Remove the validate-on-click toasts (`module_analysis_table1.R:233`,
  `module_analysis_modelspec.R:314`).
- **Step states for the Analyze pills.** Alongside `disabled`, `.apply_gate()` (or a sibling)
  adds `edark-step-done` to a step whose output exists and is current, from R. Stage 3 styles
  the states; until then the class is harmless.
- **Toasts only confirm.** `showNotification()` is kept for transient success ("Added to
  report", "Reset to original"). Any error or warning that toasts today moves to the
  messages area (Stage 4) - until then, to inline text beside the control.

**Done when:** clicking every locked step shows a reason; every Run button with an unmet
precondition is disabled with visible reason text; `grep` finds each "do X first" string
once, in `R/ui_helpers.R`.

**Built 2026-09-23.** Four things came out differently from the plan above, all verified by
driving the app with `chromote` (a full run: freeze `liver_tx`, outcome `ead`, three
covariates, fit, then each Model sub-tab):

- **The popover is attached by the server, not statically.** A popover wrapped around a
  static nav title would also open on an *unlocked* step and fight with navigation. So each
  gated nav title is a `shiny::uiOutput(..., inline = TRUE)` that renders either the plain
  label or the label with a lock glyph wrapped in `bslib::popover()`. Dynamic, whole-label
  click target, still zero new JS. Only the five gated items need it (`step5`, `step6`,
  `diagnostics`, `performance`, `results`); steps 1-4 and Model › Summary / Create are never
  locked, so their titles stay static. Labels live in `.ANALYSIS_NAV_STEPS` /
  `.ANALYSIS_NAV_MODEL` so they cannot drift from the reasons that name them.
- **The CSS has to out-specify Bootstrap.** `.nav-link.disabled` alone loses: Bootstrap's own
  rule is equally specific and loads *after* `inst/www/edark.css`, so `pointer-events: none`
  survived and the click never reached the popover. `.nav .nav-link.disabled` wins.
- **Bootstrap does *not* refuse to switch to a `.disabled` tab** once pointer events are
  restored, contrary to the assumption above. `.apply_gate()` now reads `input[[navset_id]]`
  *without* `isolate()` and puts the selection back to `last_ok[[navset_id]]`, a
  `reactiveValues` holding the last allowed selection per navset - in R, no new JS. This
  replaced the older "furthest open tab before it" fallback, which would have dropped a user
  who clicked locked Step 5 onto Step 4 rather than leaving them where they were.
- **Diagnostics has no "tick at least one check" precondition** - sample accounting and
  fitting warnings are always included, so an empty tick list is legitimate. Its only gate is
  the fitted model, so there is no `pick_check` reason. Performance (`pick_measure`) and
  Results (`pick_output`) do have one, and both replaced a toast.

Left alone deliberately: `module_analysis_covariate_confirm.R:892` still says "Resolve the
errors above before running the model" in its own words. It is Step 4's own checks panel, not
a button precondition, and the Step 5 `model_preflight` wording would misname them.
`.ms_js()`'s pulse-the-preflight-box click handler (`module_analysis_modelspec.R:60-71`) also
stays: it still works, and the visible reason under the button complements it.

### Stage 2 - Component library + Explore report aesthetics

Extend `R/ui_helpers.R`. Helpers return tag objects only - no reactive state, no module
servers. Names indicative:

| Helper | Replaces |
|---|---|
| `edark_section_label(text)` | ~30 re-typed label strings, the four `.hdr()` copies, `.sidebar_label()`, Prepare's variant |
| `edark_empty_state(icon, title, body)` | the six empty-state grammars |
| `edark_message(level, text, detail)` | alerts, badges and bordered cards used as status. Levels: `ok`, `info`, `warn`, `error`, `pending`, `stale`, `locked` - mapped to Bootstrap utility classes |
| `edark_info_row(label, value)` | the hand-built label/value rows in the Setup, Covariates and Prepare summaries |
| `edark_model_header(result, fields)` | the four model header cards; union of fields, show what applies |
| `edark_aesthetics_controls(ns)` | the four aesthetics accordions - now rendered once, in the Appearance panel (D9 as amended) |
| `edark_button(id, label, variant, size, ...)` | every hand-typed button class string; enforces F1 / D10 |
| `edark_action_toolbar(...)` | the right-aligned `btn-sm` row above a result (`module_explore_output.R:28-46`) |
| `edark_run_button()` | (from Stage 1) - a thin wrapper over `edark_button()` that adds the disabled reason |

**Button scale (F1 / D10).** `edark_button()` takes `variant = c("primary", "secondary",
"danger", "warning")` and `size = c("config", "toolbar", "dialog")`, and is the only place a
`btn-*` string is written:

- `config` → `class = "btn-<variant> w-100"`, default height. Used for every action inside a
  config pane, including Apply Changes / Reset, Describe / Correlate / Plot Trend, Generate &
  Download, and every Analyze Run / Generate / Fit button.
- `toolbar` → `class = "btn-sm btn-outline-<variant>"`, in an `edark_action_toolbar()`.
- `dialog` → default size, no width class, for modal footers/headers.

`bslib::input_task_button()` keeps its busy-state behaviour but must be passed the same
classes; give it its own branch in `edark_button()` (`type = "task"`) rather than an exception
at each call site. Sweep: `grep -n '"btn-' R/` must return hits only in `R/ui_helpers.R`.

**Explore report aesthetics (D7 + D9).** Delete both aesthetics accordions from `module_report.R`
and the `custom_*` aesthetic inputs. Full Report generates with the aesthetics currently set
in Explore (`shared_state$color_palette`, `ggplot_theme`, `legend_position`, ...). Custom
Report items already snapshot a `plot_spec`, but (a) Report's `custom_*` controls override it
at generation (`modifyList()` in `.build_custom_report_sections()`, `generate_report.R:1074`),
and (b) the snapshot is `shared_state$plot_specification`, which may predate live aesthetic
changes made after the plot was drawn. Fix both: at Add time write the aesthetics in force
into the item's spec (`module_explore_output.R:230-256`), and have the app call
`generate_custom_report()` without report-level aesthetics so each item looks exactly as it
did on screen. `edark_report()` / `generate_custom_report()` keep their aesthetic arguments
for programmatic use.

**The Appearance panel (D9 as amended).** The remaining copy of the controls also leaves the
mode panels. `edark_aesthetics_controls(ns)` renders once, inside a small module of its own
(`R/module_appearance.R`), as the fourth panel of the Explore config pane's pill row. The three
Explore mode panels each lose their aesthetics accordion (`module_explore_controls.R:16-54`,
`module_trend_controls.R:55-91`), which removes the staged-vs-live collision in the Explore
sidebar (§2.6): after this change every mode panel is staged until that mode's button is
clicked, and everything that applies live is in the Appearance panel. The module writes
straight to the `shared_state` aesthetic fields - one set of values read by the plot, by Full
Report and by each Custom Report item at Add time - and is their only writer.

Update §E (PRD_2_Explore.md) in the same change: aesthetics are a single app-level setting,
not a per-mode and not a per-report one.

**Done when:** `grep` for `"text-uppercase fw-semibold"`, `\.hdr <- `, `"btn-` and the
aesthetics accordion body each return one hit, in `R/ui_helpers.R`; the aesthetics controls
exist once, in the Appearance panel; Explore's three mode buttons are the same size; a report
generated from Explore matches the on-screen plot styling.

**Built 2026-09-23.** Verified by driving the app with `chromote` and by `testServer`:

- **Appearance is a panel, not a dialog** (D9 amended - see the decision table for why).
- **`edark_button()` needed two things the plan did not anticipate.** An explicit `outline`
  argument, because D10's "primary filled, secondary outline" is a property orthogonal to
  scale and encoding it implicitly per variant would have been invisible magic; and an
  explicit `class` argument, because passing a utility class such as `p-1` or `ms-2` through
  `...` emits a *second* `class` attribute on the tag rather than appending. `class` now
  rejects any `btn-*` string, so the helper cannot be routed around.
- **`generate_custom_report()`'s aesthetic defaults had to become NULL.** With concrete
  defaults, "call it without aesthetics" still restyled every item, so D7 could not be
  satisfied by dropping the arguments at the call site alone. NULLs are filtered out, so
  supplying none now means "leave each item as captured", while an explicit value still
  overrides every item for programmatic callers (`edark_report()`).
- **Three spacings collapsed to two.** Hand-typed labels used `mt-0`, `mt-2` and `mt-3`;
  `edark_section_label()` offers only `first` (`mt-0`) and the default (`mt-2`), so a few
  Analyze panes are marginally tighter than before. That is the consolidation, not a
  regression.
- **`shared_state$legend_position` initialised to `"right"` while every control shipped
  `"top"`** (`edark.R:239`), so the stored default was dead - the control's observer
  overwrote it on the first flush. Now `"top"` in both places.
- Not yet wired to call sites: `edark_message()` and `edark_info_row()`. Both exist and are
  tested, but the app has no messages area or info pane to put them in until Stage 4, which is
  where they land.

### Stage 3 - Theme file

Extend `inst/www/edark.css`:

- CSS custom properties on `:root` and `[data-bs-theme="dark"]`: the status colours and four
  column-type colours (`--edark-type-{numeric,factor,datetime,character}`), resolving the
  "amber means three things" problem by naming.
- **Navbar:** a calm bar instead of Flatly's solid primary - body background, 1 px bottom
  border, nav links in the secondary text colour, the active stage in primary with a 2 px
  primary underline. This also fixes the theme / debug button contrast. (Bar colour may be
  set with `page_navbar(navbar_options = ...)` instead of CSS.)
- **Pills (level 2):** promote the existing rule at `edark.R:52-75` from `.sidebar .nav-pills`
  to all level-2 pills - rounded, tertiary background with border when inactive, primary fill
  when active.
- **Analyze stepper states:** current = primary fill; done (`edark-step-done`) = light primary
  tint with a check glyph; locked (`.disabled`) = muted, lock glyph, `cursor: not-allowed`.
  Step numbers stay in the labels.
- **Level 3a pills (mode of a page, in the config pane):** the existing `.sidebar .nav-pills`
  rule already covers Explore's; make it the rule for Report's Full / Custom too once they
  move into the config pane (Stage 5). Visually lighter than the level-2 pills - smaller, no
  fill until active - so the two levels are not confusable when both are on screen.
- **Level 3b and 4 tabs:** one font weight (500) and one active colour (primary) for the
  underline sub-step idiom and card tabs, so all nav reads as one family; slightly tighter nav
  padding. 3a and 3b share the same active colour and weight - the placement differs, the
  family does not (D8).
- **Buttons (F1 / D10):** no new button CSS beyond what Bootstrap gives; the point of
  `edark_button()` is that the classes are already right. Add only a rule that makes
  `input_task_button`'s busy state match the normal button height so the swap is not visible.
- **Typography:** `bs_theme(base_font = bslib::font_collection("system-ui", "-apple-system",
  "Segoe UI", "Roboto", "sans-serif"))` - the native OS font, works offline. (Avoid
  `font_google()`: it needs internet on first launch, a problem on locked-down hospital
  machines.)
- **Motion and focus:** `transition: background-color .15s, color .15s` on nav links;
  visible `:focus-visible` outline on every interactive element.
- Scroll containment for the column manager and transform table (`max-height`,
  `overflow: auto`, `position: sticky` header).
- Pane styling for Stage 4 (info pane background, messages area spacing).

Also:

- Replace the `flatly <-> darkly` swap (`edark.R:342-355`) with one preset plus
  `bslib::input_dark_mode()` (`bslib >= 0.7.0` is already required).
- Move the inline `tags$style` blocks into the file and delete the leak inside the render
  function (`module_column_manager.R:91-95`).

**Done when:** dark mode toggles on every page with no layout shift or change of identity;
no `tags$style` remains inside a render function.

**Built 2026-09-23.** `inst/www/edark.css` grew from 33 to ~380 lines, in seven named
sections. Notes from building it:

- **`bootswatch = "flatly"` and `input_dark_mode()` do co-exist**, which the plan left open.
  Every colour in the stylesheet is a Bootstrap variable, so flipping `data-bs-theme` on
  `<html>` carries the whole app across: navbar and body both go to `rgb(33,37,41)`, with no
  layout shift and no change of identity. The preset swap (`flatly <-> darkly`) and its server
  observer are gone, along with `is_dark_theme()` - dark mode now needs no server code at all.
- **The navbar had to be overridden with `!important`.** Flatly sets `.navbar` colours at a
  specificity bslib's `navbar_options()` does not beat from R, and `navbar_options(bg = ...)`
  takes a literal colour, which would hardcode one mode. CSS with `var(--bs-body-bg)` is the
  only form that follows the theme.
- **All three inline `tags$style()` blocks are gone**, including the one inside
  `module_column_manager.R`'s render function, which re-emitted a *global* `.form-check`
  override on every re-render. It is now scoped to `.edark-checkbox-cell`, a class the
  checkbox cell carries, so it no longer reaches every checkbox in the app.
- **Two rules came from looking at screenshots rather than from the plan.**
  `.edark-action-toolbar .btn { white-space: nowrap }` - the four plot actions wrapped to two
  lines in a narrow centre column, doubling the row height. And a `min-height` on
  `input_task_button`, so its busy state does not change the button's height mid-run.
- **Six step pills fit one row at 1280 px**, so the wrap noted in §2.3 is not yet a problem at
  six. Shorter labels are still worth doing in Stage 5 for narrower windows.
- Deferred to their own stages, though visible in the screenshots: Prepare's "Up to date"
  status badge is still a full-width element that reads as a button (F2 - it is a fact, so it
  belongs in the info pane, Stage 4), and Explore still has no empty state before the first
  plot (Stage 6).

### Stage 4 - Page contract

Every page has the same four places. This is D2 + D3 + D6 made concrete.

```
+--------------+-------------------------------------+-------------+
| CONFIG       | MESSAGES (only when there are any)  | INFO        |
| left sidebar |  warnings, errors, blockers, stale  | right pane  |
| 340 px       +-------------------------------------+ 300 px      |
|              |                                     |             |
| required     | RESULT                              | what the    |
| optional     |  the one artefact this page         | current     |
| advanced >   |  produces (plot, table, model)      | settings    |
|              |                                     | produce     |
| [Primary]    |  - or a config table (see below)    |             |
+--------------+-------------------------------------+-------------+
```

- **Config (left sidebar, 340 px).** Mode pills at the top when the page has modes (level 3a,
  D8). Then inputs grouped required -> optional -> advanced (accordion), then **one** primary
  action at the bottom, full width, labelled verb + object ("Fit Model", never "Run"). The
  primary action is at the bottom in *every* mode of the page (F3): today Full Report puts
  Generate at the top (`module_report.R:60-64`) and Custom at the bottom (`:259-263`).
- **Result (centre).** The one artefact the page produces. Table-shaped configuration stays
  here when it is one row per variable - Setup roles, Covariates, Prepare's column and
  transform tables. Its table-level controls (Include all / Clear, search, legend) go in the
  left sidebar. Actions **on** the produced artefact (Save, Copy, Add to Custom Report,
  Appearance…) go in one right-aligned `edark_action_toolbar()` directly above it, `btn-sm`
  outline - not in a pane (F2 / D10). **Do not re-render these reactables:** the
  patch-don't-re-render contract (§N1.4) and the `MutationObserver` wrappers must survive.
- **Info (right pane, 300 px).** Neutral, factual, live: what the current settings produce.
  Never inputs, never actions, never warnings. A standard `bslib::sidebar(position = "right")`
  in a nested `layout_sidebar`, collapsible with bslib's own toggle. Every page owes it real
  content (F7); the table below names it per page.
- **Messages (top of the centre column).** One `edark_messages_ui(id)` slot per page,
  rendering `edark_message()` items. Empty = takes no space. Every warning, error, blocker
  and stale notice on the page goes here and nowhere else.
- **Exception - no centre (D11).** A page whose only deliverable is a downloaded file and
  whose "result" would merely restate its own controls drops the centre column: config left,
  info right, messages at the top of the info column. Explore › Report › Custom is the one
  case today; re-check Step 6 Export when Phase 8 lands (it has a real file list, so it
  probably keeps its centre). Nothing else may use this shape.

Per page:

| Page | Config (left) | Result (centre) | Info (right) | Messages |
|---|---|---|---|---|
| Prepare (all sub-tabs) | Controls for the active sub-tab; Apply Changes + Reset to Original at the bottom (§1.3c) | Columns / Transforms / Filters / Preview | **Dimensions** now -> after apply (moved out of the config pane, §1.3d), then an **itemised** pending list under one heading per kind: Columns ("2 excluded: `donor_id`, `notes`"), Transforms ("`log(preop_meld)`; `age_tx` -> 4 bands"), Row Filters ("`age_tx` >= 18; `graft_type` in {DBD, DCD}") with the row count each leaves | transform validation, stale report items; invalid rows also marked inline |
| Explore › Explore Data (renamed, §1.3e) | Mode pills (Describe / Correlate / Trend) at the top; that mode's pickers; its button at the bottom. **No aesthetics** - they live in the Appearance dialog (D9) | Plot, with the toolbar above it: Save Plot, Copy to Clipboard, Add to Custom Report, Appearance… (§1.3f) | Variable summary table, then a **Custom report** line: n items so far, and whether this exact plot is already among them (§1.3g) | plot errors, small-n warnings |
| Explore › Report › Full | Full / Custom pills at the top; report type, primary variable, variables (Select Variables… modal), options, report contents; then **Output format**, then **Generate & Download** last (§1.3k) | Report preview | What will be included: n sections, n variables of n eligible, stratify variable, format | empty report, stale data |
| Explore › Report › Custom | Full / Custom pills at the top; **Report Items** list - one row per item with reorder and delete, the page's main presentation (§1.3l); then Output format; then Generate & Download last | **none** (D11) - the "Preview" card is deleted | Report summary: n items, item types in order, output format, the working dataset the items were captured from and whether it has changed since; empty-state text when there are no items (§1.3m) | no items yet, stale data - rendered at the top of the info column since there is no centre |
| Analyze 1 Setup | Study type, purpose, split, action buttons, table controls | Role table | Incoming dataset snapshot, role summary, selected sample | role conflicts, tier-1 blockers |
| Analyze 2 Table 1 | Table options + Generate | Table 1 | Sample / strata counts | |
| Analyze 3 (each sub-step) | Method settings + Run | Results | Candidates in, flagged n | |
| Analyze 4 Covariates | Table controls | Covariate table | Sample size, EPV, missingness | checks that fail |
| Analyze 5 Model › * | Settings + one action | Summary / model / diagnostics / performance / results | Model header (`edark_model_header()`) | stale model, changed spec |
| Analyze 6 Export | Items and format + Download | File list / preview | What the zip will contain | missing outputs |

Viewport check (§M9.4 NF-05): at 1280 px the centre gets ~600 px with both panes open. Verify
Setup and Covariates tables at 1280 x 800. If too tight, the right pane starts collapsed on
those two pages only (`open = "closed"`) - same width, same place, just folded.

Also in this stage: flip the two right-hand config sidebars to left
(`module_analysis_setup.R:138`, `module_analysis_covariate_confirm.R:147`) and split their
contents into config (left) and info (right); give Model › Summary and Step 6 both panes;
collapse the six widths to the two constants.

Prepare's config pane is the clearest case of the split: `prepare_confirm_ui()` currently
stacks dimensions, warnings, a pending badge and two buttons in one column
(`module_prepare_confirm.R:26-56`). Dimensions and the pending list go right, the warnings go
to the messages slot, and only Apply Changes / Reset to Original stay left - which also fixes
the "long warnings list pushes Apply below the fold" problem (§2.6) without moving Apply.

**Done when:** walking Prepare -> Explore -> Analyze 1-6 -> Explore › Report, config is
always left at 340 px, info always right at 300 px (except Report › Custom, which has no
centre by D11), the primary action is at the bottom of the config pane in every mode, and
every warning appears in the messages slot.

**Built 2026-09-23.** `edark_page()` and `edark_messages_ui()` /
`edark_messages_server()` in `R/ui_helpers.R` are the whole contract; every page now calls
`edark_page()` and `grep` finds `bslib::layout_sidebar` only inside it. Verified by walking
the app at 1280 x 800 with `chromote`: the config pane measures 340 px on every page and no
page raised a JS exception.

- **Two pages have a config pane with no controls to hold**, because their configuration
  *is* their table: Covariates (row per candidate, with its own checkbox and reference
  select) and Model › Summary (read-only audit). Rather than invent controls or leave the
  pane blank, both carry orientation - how to read the table, and which step owns each part
  of the spec. Covariates' is the thinnest pane in the app and is the first place real
  table-level actions should go.
- **Setup and Covariates start with the info pane collapsed** (`info_open = "closed"`).
  Both centres are wide one-row-per-variable tables and the viewport check (§M9.4 NF-05)
  left them too tight at 1280 px with both panes open. Same width, same place, just folded -
  exactly the fallback this stage allowed for.
- **`main_message` had to stay outside `cc_table_wrap`.** `.cc_js()` hangs a
  `MutationObserver` on that div to re-patch the embedded inputs, so anything else
  re-rendering inside it wakes the observer for nothing (§N1.4).
- **The pending-changes badge is gone rather than moved.** It said "3 pending change(s)" and
  never which three; the info pane now itemises them per kind, with the dimensions the
  pipeline would produce. `.count_pending_changes()` was the badge's only caller, so it went
  with it; `.describe_pending_changes()` (plus `.describe_one_transform()` /
  `.describe_one_filter()`) is what replaced it.
- **Full Report keeps a centre, Custom does not.** Full's centre is the *resolved* section
  list - which variables survive eligibility, the primary variable and the stratify variable
  - which the controls do not state, so it is not the restatement F5 forbids. Custom's
  "Preview" card was exactly that restatement and is deleted (D11).
- Two smaller things fixed in passing: `module_explore_output.R` used the native pipe `|>`,
  against non-negotiable #8, and its trend-summary branch still tested the dead type names
  `trend_count` / `trend_proportion` (a known discrepancy in `PRD/CLAUDE.md`), so a
  `trend_factor` plot summarised the timestamp column instead of the trend variable.

### Stage 5 - Flatten navigation

- **Apply the §1.2 vocabulary.** Level 2: Prepare's `navset_card_tab` (`edark.R:112`) and
  Explore's `navset_tab` (`edark.R:142`) become pills (§1.3b). Level 3b (underline tabs):
  Step 3's `navset_pill` (`module_analysis_varinvestigation.R:116`) becomes the sub-step
  idiom, matching Step 5. Level 3a (pills in the config pane): Explore's sidebar
  `navset_pill` (`edark.R:150`) **stays as it is** (D8 as amended, §1.3) and Report's
  Full / Custom `navset_pill` (`module_report.R:47`) moves from the main panel into the
  config pane to match it. Keep the Explore -> Report hop and `requested_report_subtab`
  working - Report's pills change position, not id, so `nav_select("report_mode_tabs", ...)`
  (`module_report.R:288-292`) still works.
  - Moving Report's pills inside the sidebar means the two modes share one `layout_sidebar`
    instead of owning one each: one sidebar holding a `navset_hidden()` (or pill set) whose
    panels are the two config stacks, one centre for Full's preview, and no centre for
    Custom (D11). Check that Custom's centre-less shape and Full's three-pane shape can live
    in one page without the layout jumping when the mode changes; if they cannot, give each
    mode its own `layout_sidebar` and keep the pills duplicated in both sidebars via one
    helper.
- **Flatten Data Preview** from 3 tab levels to 1: Original/Working and Data/Summary become
  two `radioGroupButtons` in the left sidebar driving one output (`module_data_preview.R:19`).
- Shorten Analyze step labels so the pills fit on one row at 1280 px: "1 · Setup",
  "2 · Table 1", "3 · Variables", "4 · Covariates", "5 · Model", "6 · Export".
- Card tabs (`navset_card_tab`) are used only for views of one generated result
  (diagnostic checks, performance sets, result outputs).

**Done when:** each nav level has one look everywhere (§1.2 table) - no pills inside pills,
level-3a pills only at the top of a config pane, and card tabs only for result views; gating
and the Explore -> Report hop (`requested_tab`, `edark.R:432-442`) still work.

**Built 2026-09-23.** All twelve checks pass under `chromote` at 1280 x 800, with no JS
exceptions, including the Explore -> View Report hop landing on Custom Report.

- **Report's Full / Custom became level 3b (underline tabs), not 3a pills in the config
  pane. Ruled 2026-09-23: they stay underlines** - it is the third nested level, and a level
  should look the same wherever it appears, which is D8's whole point. D8 puts them in the
  config pane because they are "modes that share one result surface". D11, decided the same
  day, gives Custom *no* result surface at all. The two decisions cannot both hold: Full has
  a centre and Custom does not, so they do not share one, and D8's own stated test - does
  the sub-step own its own output? - puts them in 3b with Analyze's sub-steps. The
  alternative the plan offered (each mode keeps its own `layout_sidebar`, pills duplicated
  "via one helper") does not survive contact with Shiny: two copies of one `navset_pill` id
  is a duplicate input id, so `nav_select("report_mode_tabs", ...)` would break, and that is
  what the Explore -> Report hop rides on. Underline tabs keep one navset, one id, one
  working hop. D8 is amended accordingly: level 3 splits by depth, not by whether the
  sub-step owns its output. D11 stands, and Custom keeps no centre.
- **Data Preview went from three tab levels to one**, as planned, but its two toggles sit
  above the table rather than in the left sidebar. Prepare's config pane is shared by all
  four Prepare pages and holds only Apply / Reset; putting one page's view switches there
  would mean rendering them conditionally on the active page, which is more machinery than
  the flattening is worth. The four reactables are wrapped in `conditionalPanel`, so a
  toggle shows and hides - it never re-renders a table.
- **Six step pills fit one row at 1280 px** with "3 · Variables" and "4 · Covariates", so
  the wrap in §2.3 is closed.
- Card tabs now appear only at level 4 - diagnostic checks, performance sets, result
  outputs, Table 1 views, Step 3 results - which is the rule this stage set.

### Stage 6 - Small fixes

- Explore empty state; disable Save / Copy / Add to Report until a plot exists
  (`module_explore_output.R:28-46`), with reasons from the Stage 1 table.
- Move the row-count badge below the filter list (`module_row_filter.R:36,40`) - or into the
  Prepare info pane if Stage 4 has landed.
- Replace any remaining native `title=` attributes with `bslib::tooltip()`.
- **Variable-selection modal (§1.3j).** In `module_report.R:385-405`, set `footer = NULL` and
  put the actions in a header row above the checkbox list: Select All / Clear on the left
  (`dialog` size, outline), Done (primary) / Cancel on the right. The list scrolls under them
  (`max-height`, `overflow: auto`). Same shape for any other dialog holding a long list (F6).
- **Copy pass (F8).**
  - Explore's level-2 pill "Plot" -> **"Explore Data"** (`edark.R:146`). The Report pill is
    unchanged.
  - Full Report's report type stays **"Describe Variables"**, plural (`module_report.R:84`) -
    the report describes every variable chosen in the selection dialog, so the plural is
    right. Confirmed 2026-09-23; do not "fix" it.
  - "Deselect All" -> **"Clear"** in the variable modal (`module_report.R:396`).
  - Button labels verb + object; empty-state and message wording consistent.
- **Button sweep (F1 / D10).** Last pass over every call site that Stage 2's helper did not
  reach: `grep -n '"btn-\|w-75\|w-50' R/` returns hits only in `R/ui_helpers.R`, and every
  `input_task_button` goes through the helper.

**Built 2026-09-23.** Thirteen checks pass under `chromote` at 1280 x 800, no JS exceptions.

- **Hiding the empty plot card deadlocked the page, and the fix is worth knowing.** The
  first version gated the card on `shared_state$active_plot`. But `active_plot` is written
  *by* the plot's render, and Shiny suspends an output inside a hidden element - so no card
  meant no render, no render meant no `active_plot`, and no `active_plot` meant no card. The
  plot never appeared. `has_plot()` is now keyed on `plot_specification`, which the mode
  buttons write directly, and `main_plot` is additionally marked
  `suspendWhenHidden = FALSE`. Caught by driving the app; nothing in `check()` or a
  `testServer` run would have shown it.
- **Two native `title=` attributes survive deliberately**
  (`module_analysis_covariate_confirm.R:529,597`). Both sit inside reactable cells, where
  the patch-don't-re-render contract applies (§N1.4): `bslib::tooltip()` needs JS
  initialisation and adds a wrapper React did not create, so converting them risks the
  embedded-input patching for a cosmetic gain. The nav popovers and every tooltip outside a
  reactable already use `bslib`.
- **The row-count badge moved rather than being repositioned.** The plan offered "below the
  filter list, or into the Prepare info pane if Stage 4 has landed". Stage 4 had, so it is
  now a "Rows retained: N of M" line under the info pane's Row Filters heading, which also
  ends the scrolls-away problem outright.
- Copy pass done: Explore's level-2 pill reads **Explore Data**; "Deselect all" is **Clear**
  in all three places it appeared (the variable modal, the column manager, Diagnostics);
  Step 3's bare **Run** is **Run Selection**. "Describe Variables" was left plural, as
  §1.3i instructs.
- Button sweep clean: `grep -n '"btn-\|w-75\|w-50' R/` returns hits only in
  `R/ui_helpers.R`, and every `input_task_button` goes through `edark_button(type = "task")`.

---

## 5 - Verification

Launch: `devtools::load_all(); edark(liver_tx)` (headless recipe:
`memory/project_running_edark_headless.md` - full `Rscript` path, port 7788, do not kill `ark`).

- **Every stage:** `devtools::check()` at 0 errors / 0 warnings; walk
  Prepare -> Explore -> Analyze 1-6 -> Explore › Report at 1280 x 800 and a wide window, light
  and dark.
- **1:** click each locked step - it says why. Each Run button with an unmet precondition is
  disabled with reason text visible.
- **2:** the four `grep`s return one hit each. The aesthetics controls exist once, in the
  Appearance dialog, and changing one there changes the plot, the Full report and a
  newly-added Custom item. Explore's Describe / Correlate / Plot Trend buttons are the same
  width and height. A Full and a Custom report generated from Explore match the on-screen
  styling. Regenerate all three formats with `edark_report()`.
- **3:** toggle dark mode on every page - no layout shift, no identity change.
- **4:** panes and widths per the contract on every page; the primary action is at the bottom
  of the config pane in every mode; no pane holds a fact that belongs in the other one
  (Prepare's dimensions are on the right, its warnings in the messages slot).
  **Setup and Covariates:** drive with `chromote` and watch the console - reactable-embedded
  inputs still patch, survive a search clear, and never re-render (`testServer` cannot catch
  this).
- **5:** gating and the Explore -> Report hop still work, including landing on the right
  Report mode now that the pills live in the config pane. Switching Full <-> Custom does not
  shift the layout.
- **6:** the variable modal's actions are reachable without scrolling with every variable
  listed; the button sweep `grep` is clean.
