# RESOLVED — EDARK v0.9

Closed TO-DOs, with the root cause and whatever was learned fixing them.

Open TO-DOs live in the root `CLAUDE.md`. When one closes, move it here in full and
delete it there — the root file is loaded into every session, so it carries only open
work. Keep entries newest-first within each area, headed with the close date.

Consult this file when a bug smells familiar, before re-deriving a fix.

---

## Prepare

### 2026-09-25 - the custom-report modal had no way out from Prepare

With items queued in the custom report, "Custom Report Will Use the Changed Data" fired on
Apply, on Reset and on every Prepare sub-tab switch. Both answers left the queue intact, so
a user who no longer wanted that report met the same dialog for the rest of the session -
and the only place to empty the queue was Report > Custom, three clicks away in another
stage.

The modal now offers a third route, "Discard Items & Continue": `.clear_custom_report_items()`
empties `custom_report_items` and the action proceeds. All three callers pass a `clear_id`
(Apply and Reset in `module_prepare_confirm.R`, the tab guard in `edark.R`). Verified in the
browser with `chromote`: after discarding, a further staged change and a tab switch no longer
raise the dialog.

Report > Custom needed no change - it resolves its selected row by id against the list and
falls back to no selection when the list is empty.

**Durable rule: §N3.2.**

### 2026-09-25 - winsorize percentile boxes accepted anything

`numericInput(min =, max =)` bounds the spinner arrows only; a typed value goes to the
server untouched. The two winsorize boxes therefore accepted 0, -4, 250 or a lower
percentile above the upper one, and the spec was stored as typed. A lower bound at or above
the upper one makes `quantile()` flatten the column to a constant, and Apply's validity
check caught only `lo >= hi`, not the out-of-range cases.

Bounds now live in `.winsor_lower()` / `.winsor_upper()` (`module_column_transform.R`) and
are enforced in three places: the two observers (which clamp and write back with
`updateNumericInput()`), `.apply_column_transforms()`, and `.transform_spec_is_valid()`.
Lower is the box the user drives: it clamps to [1, 99] and pushes the upper box up to stay
one percentile clear; upper clamps to [lower + 1, 100].

**Durable rule: §N3.4.**

### 2026-09-24 — transform → row filter → transform did not warn on stage

Root cause was one line shared by `.build_prepare_warnings()` and
`.prune_conflicting_filter_specs()`: both derived the changed-transform set from
`names(specs)`, the *currently staged* transforms, so a transform that had been REMOVED
since the last Apply was never tested.

Worst case was cutpoints -> filter on the bands -> remove the cutpoints: the categorical
filter survived onto a column that was numeric again, matched no rows, and Apply produced
an empty dataset with no warning.

Both now use `union(names(specs), names(last_tx))`, and `.apply_row_filters()` skips a
filter whose `type` disagrees with the column as a backstop.

**Durable rule: §N3.4.**

---

## Explore

### 2026-09-25 - report progress bar filled twice for Word and PowerPoint

Full Report counted to n twice for Word and PowerPoint ("Variable i of n", then
"Section i of n"), and once for HTML. Custom Report did the same ("Item", then "Slide").

Root cause: the section builders and the PPT / Word assemblers were handed the same
`progress_fn` and each reported `i / n` over the whole bar. `.assemble_html()` reports
nothing (one `rmarkdown::render()` call), which is why HTML looked right.

Fix: `.progress_span()` gives each pass its own share of the bar - building 0-60% and
writing 60-100% for PPT / Word, building 0-90% for HTML - and the labels name the pass
("Building plots: variable i of n", "Writing slides: section i of n").

**Durable rule: §N5.1.**

### 2026-09-25 - Report contents option: collinearity investigation

Full Report gains a **Collinearity** checkbox (default off). It runs Analyze's
`compute_collinearity()` over Table One's variable set and renders the three pieces of
Step 3's Collinearity pill: Pearson heatmap (numerics), Cramer's V heatmap (factors),
pairs above 0.7. Placed after Table One in PPT, Word and HTML.

The two heatmaps were inline in Step 3's `renderPlot()`s; they moved to
`service_analysis_plots.R` so the app and the report draw the same figure. Known gap
carried over from Step 3: numeric x factor pairs are not measured.

**Durable rule: §N5.5.**

### 2026-09-24 — Word report: reference `.docx` template with defined heading styles

`inst/templates/word_docx_blank_template.docx` now supplies the Title / Subtitle /
heading 1-9 styles, and `.assemble_docx()` builds a real document on top of them: title
page, a Word TOC field that populates from the headings (heading 1 = Table 1 / Dataset
Summary / the section group, heading 2 = each variable), a running header naming the
document, a page number bottom-right, and four page sections so the dataset summary is
landscape and everything else portrait. Per-variable summary tables are laid out at 18pt.

Two things worth knowing:

- Word's NUMPAGES field counts *within a section*, so the footer is "Page N" and not
  "Page N of M".
- Word's own table autofit wrecks a table wider than the page, so `.docx_fit_ft()`
  computes the widths in R and `.docx_shrink_widths()` takes the overflow out of the
  widest columns only.

**Durable rule: §N5.7.**

---

## Analyze

_Nothing closed yet._

---

## Other

### 2026-09-28 - session save / load on click (branch `session-manager-v1`)

The high-priority TO-DO "session save-load with autosave or on click". The on-click half is
built; autosave stays open in the root `CLAUDE.md`.

§M8 had been written before this build but did not match what was wanted, and was revised
with the user first: match the dataset on **input** classes (what decides whether a transform
lands) rather than EDARK types, and **no partial loads** - refuse unless every column and type
matches, where the old spec skipped whatever no longer fit. Model purpose, validation settings
and the custom report list (thumbnails as PNG bytes) were added to the file. Schema migrations
were dropped until release. Two gaps in the old spec were closed: `purpose_specification`
was in the build plan but not the file format, and Phase S cited sections as "§13.x".

The build also merged the two copies of the Apply commit (`do_apply()` and `.do_nav_apply()`)
into `.run_prepare_apply()` / `.commit_working_dataset()`, so load does not add a third.

Written without R; unit tests are in `tests/testthat/test-service_session.R`, and the browser
check is still to do (Phase S › Verification in `BUILD_Analysis.md`).

**Durable rule: §N3.5**

### 2026-09-25 - the warning colour was an alarm, and amber meant three things

Bootswatch flatly's `warning` is `#f39c12`. Flatly also fills `.alert` solid and sets its
text white, so a single Prepare warning ran a band of bright orange across the top of the
page; the same colour marked a cast type (an amber ring on the badge), tinted transformed
columns in Data Preview at 15%, and coloured dialog confirm buttons and `datetime` badges.
Loud, and overloaded: two of those uses are not warnings at all.

Four changes, all at the token level rather than per caller:

- `bs_theme(warning = "#b7791f")` in `edark()` - one muted ochre everywhere. Dark mode
  redefines `--bs-warning` *and* `--bs-warning-rgb` in `edark.css`; `.text-warning` reads
  the rgb form, so setting only the first left that class on the light value.
- Alerts are redrawn in `edark.css` section 5 as tinted panels with a coloured start rule,
  from new `--edark-alert-*` tokens. One class each, same as flatly's own rules, so they
  win on source order alone.
- The cast-type badge lost its amber ring for a leading arrow
  (`.edark-badge-changed::before`), and the three badge roles that carried `text-dark`
  (written for the old bright orange) now let `text-bg-*` compute its own contrast.
- Data Preview's amber tint became `--edark-tint-bg`, a neutral primary, with the same
  arrow on the type sub-label. It had no dark-mode value before, because it was a literal.

Contrast checked in both themes with `chromote`: light `#6f5010` on `#fdf6e6`, dark
`#e8d5a3` on `#3a2f13`.

**Durable rules: §N1.14, §N1.14b.**

### 2026-09-25 - first-launch delay: splash screen

The app took a visibly long time to become usable on launch. Temporary `[edark boot]`
instrumentation in `edark()` timed each phase, and the first reading was misread: the gap
from `edark()` to the browser's page request looked like 7-18 s of startup, but it was the
user walking from the R console to the browser to hit refresh. **Only the browser-side
numbers, which anchor on the page request, meant anything.**

Measured from the page request: 2.4 s warm, 4.1 s cold. Roughly 0.5 s of that is Shiny
building and sending the HTML (theme compile included), 0.5-2.3 s is wiring the module
servers, and ~1.1 s is computing the initial outputs. All of it lands after the page
arrives, so a splash in the page HTML covers effectively the whole gap - the theme
precompiling that was considered as an alternative would have bought ~0.5 s.

Built as `edark_splash()` (`R/ui_helpers.R`) plus section 9 of `edark.css`, not with
`waiter`, which is for busy-spinners over outputs. The instrumentation was removed.

**Durable rule: §N1.16.**


### 2026-09-24 — roxygen errors on every `devtools::document()`

Two unrelated causes.

1. `DESCRIPTION` sets `Roxygen: list(markdown = TRUE)`, and markdown mode escapes `%` for
   you, so a hand-written `\%` came out of roxygen as `\\%` - a literal backslash followed
   by an Rd comment that ate the rest of the line, including the closing `}` of the
   `\item{}` it sat in. That is what made `man/liver_tx.Rd` report every later `\item` as
   an unknown macro and every later section header as unexpected, and what roxygen called
   mismatched braces in `stats_inference.R`. **Write a plain `%` in roxygen comments.**
2. Markdown links like `[edark_run_gate()]` become real `\link{}` cross-references, but
   every helper in `R/ui_helpers.R` is `@keywords internal` + `@noRd` and so has no man
   page to link to. **Undocumented internals are referred to with a code span** -
   `` `edark_run_gate()` `` - not a link.

**Durable rule: §N1.13.**

### 2026-09-23 — RHS pane was tied to the main pane; three independent panes wanted

Root cause was not the pane structure: bslib ships
`.bslib-card .card-body { max-height: var(--bslib-card-body-max-height, none) }`, two
classes to `.edark-scroll-table`'s one, so any scroll cap put directly on a `card_body`
lost on specificity and the content grew without limit. That blew out the CSS grid row the
centre and info panes share, stretching the info pane to match (1660 px on Prepare ›
Columns) and forcing the whole document to scroll.

**Cap the content - never the `card_body` itself.** That fixes all three panes at once.
Convention and the three containment mechanisms are in the header comment on
`.edark-scroll-table` (`inst/www/edark.css`) and `EDARK_RESULT_HEIGHT` (`R/ui_helpers.R`).

**Durable rule: §N1.12.**

### 2026-09-23 — UI consistency plan (all six stages)

All six stages of [BUILD_UI-redesign.md](BUILD_UI-redesign.md) are done: honest
step-locking, a shared component library (`R/ui_helpers.R`), one plain-CSS theme file
(`inst/www/edark.css`), a config (left) / result / info (right) page contract with a
dedicated messages area, and flatter navigation. Report stays inside Explore. `bslib` + R
+ CSS only - no SCSS, no new JS.

The one decision left open by Stage 5 was **ruled 2026-09-23**: Explore › Report's Full /
Custom stay underline tabs (level 3b), not the config-pane pills D8 originally named,
because that is the third nested level and consistency at a level beats the D8 wording.
See Stage 5's build note in `BUILD_UI-redesign.md`.

### 2026-09-23 — nine step pills wrapped to two rows

There are six steps, and with "3 · Variables" / "4 · Covariates" they fit one row at
1280 px (Stage 5).

### 2026-09-25 - full report tables described the dataset, not the report

Dataset Summary was built from every numeric/factor column in the working dataset and
Table One from the selected variables only in Describe mode, where it was also the only
mode that offered the checkbox. Both now follow the report contents:

- `content_vars` = selected variables, plus the primary variable in Correlate mode
  (it appears in every section).
- **Table One** = `content_vars` minus the stratify variable, which is the column header.
- **Dataset Summary** = `content_vars` plus the stratify variable.

`.build_dataset_summary()` gained an optional `variables` argument; NULL keeps the
whole-dataset behaviour that Prepare > Data Preview and the custom report rely on.
Table One's `report_type == "all_vars"` gate is gone from `generate_report()` and from
the Report module's checkbox, info row, and call, so Correlate reports can carry one.

### 2026-09-25 - Setup and Covariates opened with the info pane folded

Both pages passed `info_open = "closed"` to `edark_page()`: at 1280 px their one-row-per-variable
tables were too tight with the config and info panes both open, so Stage 4 folded the info pane as
a viewport fallback (§M9.4 NF-05). At a normal window size there is room, and the fold made two
pages behave unlike every other one - the info pane's counts were there but invisible until clicked.

Both arguments are gone; the pages now match the rest of the app. `edark_page(info_open = )` stays
as a parameter, but nothing passes it.

**Lesson:** a viewport fallback tuned to the narrowest supported width becomes a permanent
inconsistency once nobody re-checks it. Prefer letting the pane scroll over closing it by default.

### 2026-09-25 - Explore pills plotted the other pill's variables

Switching Describe -> Correlate and clicking Plot Relationship drew Correlate's secondary
variable against Describe's leftover primary and stratify; going back the other way used
Correlate's primary and stratify. Both modes write the same `shared_state` fields, and
each only wrote the inputs the user had touched, so whichever mode wrote last won and the
untouched fields were whatever the other pill left behind.

Fixed in `module_explore_controls.R`: each mode has a `publish_state()` that writes every
field it owns from its own inputs and nulls what it does not own (Describe clears
`secondary_variable`), fired on `active_mode()` becoming its id and again on its Plot
button, so the spec is built from what is on screen. `explore_mode` is passed to both
servers in `edark.R`.

**Durable rule: §N4.8.**
