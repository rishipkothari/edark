# EDARK — Explore PRD (Tab 2 · Explore: Plot and Report)

**Scope:** Tab 2, which has two sub-tabs — **Plot** (interactive exploration: Describe, Correlate, Trend) and **Report** (Full and Custom PPTX / DOCX / HTML reports, plus the programmatic report API).
**Master PRD:** [PRD_0_Master.md](PRD_0_Master.md) — principles (§M2–M3), state ownership (§M5), Explore → Report flow (§M6.4).
**Implementation details:** [NOTE_implementation.md](NOTE_implementation.md) §N4 (Explore), §N5 (Report). Statistical rules: §A4.2 / §N2.

---

# Part I — Plot

## E1 — Purpose and Scope

Explore lets the user look at the working dataset one variable, one relationship, or one time trend at a time, with a summary table beside each plot. Everything reads `dataset_working`; nothing modifies it. Plots are static `ggplot2` objects — the same object is shown on screen and written to reports (§M2.2).

---

## E2 — Layout

- **Left sidebar (400 px):** `navset_pill` with three mode pills — **Describe** (§E3), **Correlate** (§E4), **Trend** (§E5) — plus an **Appearance** panel (§E7). Appearance sits in the same row without being a mode: the three modes stage their settings, Appearance applies live.
- **Main area:** one shared output panel (§E6). All three pills write `plot_specification`; the latest click wins.
- Sidebar convention: flat sections, small uppercase section labels, one full-width primary button per pill. Aesthetics are not per-pill - they have their own **Appearance** panel (§E7).

---

## E3 — Describe

| Element | Behaviour |
|---|---|
| Variable | Numeric and factor columns (datetime excluded) |
| Stratify by | Optional; factor columns |
| Factor statistic | Count or proportion — shown only for factor variables |
| **Describe** (primary) | Builds a univariate spec → `bar_count` (factor) or `histogram_density` (numeric) |

---

## E4 — Correlate

UI label **Correlate**; code calls it the relationship tab (`relationship_controls_*`).

| Element | Behaviour |
|---|---|
| Primary variable + role | Numeric and factor columns; role **Exposure (X)** or **Outcome (Y)** |
| Secondary variable | Numeric and factor columns, excluding the primary |
| Factor statistic | Count or proportion, when relevant |
| Stratify by | Optional; factor columns only; excludes the primary |
| **Plot Relationship** (primary) | Builds a bivariate spec |

**Axis assignment:** exposure → primary on X; outcome → primary on Y. Then, for factor × numeric, axes are normalised so the factor is always on X.

**Routing (`route_plot_type()`):**

| X type | Y type | Plot type |
|---|---|---|
| factor | factor | `bar_grouped` |
| factor | numeric | `violin_jitter` |
| numeric | factor | `violin_jitter` (axes normalised) |
| numeric | numeric | `scatter_loess` |

Datetime pairings are not routed; Trend handles time (§E5). An unsupported combination renders a warning card instead of a plot.

---

## E5 — Trend

| Element | Behaviour |
|---|---|
| Timestamp | Datetime columns only |
| Resolution | Hour · Day · Week · Month (default) · Quarter · Year |
| Trend variable | **Required.** Numeric or factor column |
| Statistic (numeric) | `mean_only`, `mean_sd` (default), `mean_se`, `mean_ci`, `median_only`, `median_iqr`, `count`, `sum`, `max`, `min` |
| Statistic (factor) | Count or proportion |
| Impute zero for missing timepoints | Factor trends only; default on |
| Stratify by | Optional; factor columns |
| Include zero baseline | Checkbox; default off |
| **Plot Trend** (primary) | Builds a trend spec directly (bypasses routing) |

**Plot types:** numeric variable → `trend_numeric`; factor variable → `trend_factor`.

**Behaviour:**
- Interval statistics (`mean_sd`, `mean_se`, `mean_ci`, `median_iqr`) draw a ribbon; the others draw line + points. `mean_ci` is the t-based CI of the mean.
- **Stratified numeric trend:** one coloured line per stratum on a single panel (no facets).
- **Stratified factor trend:** one facet per stratum; factor levels as coloured lines within each facet.
- **Impute zero:** builds the complete time × level (× stratum) grid and fills gaps with zero counts; a timepoint whose levels are all zero gets proportion 0, not NaN. Takes effect on the next Plot Trend click.
- **Zero baseline:** extends the y-axis to include 0. Re-renders live like an aesthetic (§E7).

---

## E6 — Output Panel

| Element | Behaviour |
|---|---|
| Plot | The current plot. Empty-state guidance before the first plot. |
| Summary table | `reactable` summary of the plotted variable (§E6.1). For trends, the trend variable. |
| **Save Plot** | Download the current plot as an image. |
| **Copy to Clipboard** | Copy the current plot image. |
| **Add to Custom Report** | Append the current spec + thumbnail to the Custom Report (§E12). |
| **View Report** | Switch to the Report sub-tab, Custom Report pill. |
| Stale notice | After a Prepare Apply, the panel says the dataset has changed and clears the old plot until the user re-plots (§M3.4). |

### E6.1 Variable Summary (`build_variable_summary()`)
- **Numeric:** n, missing, missing %, mean, median, SD, IQR, min, max, skewness, kurtosis.
- **Factor / character:** n, missing, missing %, unique levels, mode, mode frequency, mode %.
- **Datetime:** n, missing, missing %, earliest, latest, range in days.

---

## E7 — Aesthetics

One app-level setting, in one place: the **Appearance** panel of the Explore config pane, in the same pill row as Describe / Correlate / Trend (`R/module_appearance.R`). There is exactly one copy of the controls and one set of stored values, and the Appearance module is their only writer.

These are the only Explore settings that re-render **live** (§M2.3): the stored spec is reused and only styling changes. That is why they sit apart from the mode panels, whose pickers are all staged until that mode's button is clicked — the split removes a collision the old per-pill accordion created (BUILD_UI-redesign §2.6, D9).

The same values drive the plot on screen, **Full Report** generation, and each **Custom Report** item at the moment it is added (§E12). Report has no aesthetics controls of its own (D7): a generated document reproduces what was on screen.

| Setting | Options | Default |
|---|---|---|
| Plot theme | Minimal · Publication · Cowplot · Economist · FiveThirtyEight · Tufte · Modern | Minimal |
| Colour palette | Distinct, up to 10 levels: Set 2 · Set 1 · Dark 2 · Okabe-Ito · Tableau 10. Distinct, many levels: Paired (12) · Set 3 (12) · Tableau 20 · Kelly (20) · Alphabet (26) · Polychrome (36). Shades, any number: Viridis · Blues · Greens · Reds · Purples | Set 2 |
| Show data labels | on / off | off |
| Show legend | on / off | on |
| Legend position | right · left · top · bottom | top |

### E7.1 More levels than the palette has colours

A distinct-colour palette is never stretched: blending its colours gives muddy, look-alike ones. When the plot on screen colours a variable with more levels than the chosen palette has, it is drawn with the smallest palette that fits (Tableau 10, then Tableau 20, then Polychrome 36), and that palette becomes the session's palette: the Appearance picker moves to it and later plots and reports use it. The messages area says so - *"Set 2 has 8 colours, but transplant_center has 12 levels - switched to Tableau 20"* - for as long as that plot is on screen with that palette. Picking a small palette again for the same plot switches again.

Shade palettes interpolate to any number of levels and never switch. Past 36 levels (the plot's own 20-level cap normally prevents it) colours are evenly spaced hues. A report or `edark_report()` call made with a palette that is too small uses the fitting palette silently.

---

## E8 — Plot Types

### E8.1 Catalogue

| Plot type | Used for | Key behaviour |
|---|---|---|
| `bar_count` | One factor | NA dropped; natural level order (no frequency reordering); count or proportion |
| `histogram_density` | One numeric | Two panels: histogram + density, and a Q-Q plot with symmetric axes |
| `bar_grouped` | Factor × factor | Every X × fill combination shown, missing combinations as zero-height bars |
| `violin_jitter` | Factor × numeric | Violin + jittered points + median marker; NA shown as its own level; legend always hidden |
| `scatter_loess` | Numeric × numeric | Points + LOESS smooth; Pearson r label (p and CI from the shared engine, §N2). Stratified: colour + facet, legend hidden |
| `trend_numeric` | Datetime × numeric | Summary statistic per period (§E5) |
| `trend_factor` | Datetime × factor | Count or proportion per level per period (§E5) |

### E8.2 Titles and Facets
- Title: `X [× Y] [· stratified by S]`.
- Facet strips show `variable: value`.

### E8.3 Two-Panel Plots
`histogram_density` is shown as one side-by-side figure in the app (≈ 45 : 55). In reports its two panels are placed separately (§E13).

### E8.4 Stratification Summary

| Plot type | Stratified by |
|---|---|
| `bar_count`, `bar_grouped` | Fill colour |
| `violin_jitter` | Facets |
| `scatter_loess` | Colour + facets |
| `histogram_density` | Facets |
| `trend_numeric` | Coloured lines, one panel |
| `trend_factor` | Facets |

---

## E9 — Explore State Fields

| Group | Fields |
|---|---|
| Describe / Correlate | `primary_variable`, `primary_variable_role` (default `"exposure"`), `secondary_variable`, `stratify_variable`, `bar_display` (count / proportion) |
| Trend | `trend_timestamp_variable`, `trend_variable`, `trend_summary_stat` (default `"mean_sd"`), `trend_resolution` (default `"Month"`), `trend_stratify_variable`, `trend_zero_baseline`, `trend_impute_zero` (default TRUE) |
| Plot | `plot_specification`, `active_plot`, `variable_summary`, `explore_needs_refresh`, `palette_switch` (§E7.1) |
| Aesthetics | `ggplot_theme`, `color_palette`, `show_data_labels`, `show_legend`, `legend_position` |
| Custom report | `custom_report_items`, `requested_tab`, `requested_report_subtab` |

Trend fields are separate from Describe / Correlate fields; the pills do not share variable selections.

---

# Part II — Report

## E10 — Purpose and Layout

The Report sub-tab turns the working dataset into a slide deck or document. Two pills:
- **Full Report** (§E11) — generated automatically from chosen variables.
- **Custom Report** (§E12) — the plots the user added from Plot, in the user's order.

Each pill has a sidebar with **Generate & Download** (primary), **Output Format** (PowerPoint · Word · HTML) and its own options. Output Format also offers **In App** (§E11.4), which swaps the primary action for **Generate Preview**. Neither pill has aesthetics controls: both generate with the values in force in **Appearance** (§E7), so the document matches the plot on screen (D7). Generation runs in a blocking progress modal (§M3.5).

Datetime columns are excluded from reports.

---

## E11 — Full Report

### E11.1 Report Types

| UI label | `report_type` | Content |
|---|---|---|
| **Describe Variables** | `all_vars` | One section per selected numeric / factor variable: univariate plot + summary table. Optional stratify adds per-stratum columns. |
| **Correlation** | `primary_vs_others` | One section per secondary variable against a primary variable (with role). A single global stratify applies to all sections. |

Variables are chosen in a **Select Variables** modal.

### E11.2 Report Contents Options
- **Dataset Summary** (default on): one row per numeric / factor variable across the dataset, linked to its section in HTML.
- **Table One** (default off; Describe Variables only): classic clinical Table 1 — numeric rows mean ± SD with Kruskal-Wallis p; factor rows N per level with chi-square / Fisher's p. Columns: `Overall (N = x)`, one per stratum, `p-value`. Rendered before the dataset summary; stratified by the sidebar Stratify By.
- **Collinearity** (default off): the Analyze Step 3 collinearity view over the same variables as Table One - a Pearson r heatmap of the numeric variables, a Cramer's V heatmap of the factors (each needs at least two of its type), and a table of pairs above 0.7. Numeric x factor pairs are not measured. Rendered after Table One, before the dataset summary; omitted when fewer than two numeric or two factor variables qualify.

### E11.3 Overlap Guards
- Correlation skips a secondary variable equal to the stratify variable, and the variable modal hides it.
- Describe Variables drops the stratify variable from its section list.

### E11.4 In-App Preview
- **In App** builds the HTML report and shows it in the centre pane instead of downloading it. The centre has two tabs: **Sections**, the read-only list of sections the current settings resolve to, and **Preview**. Generate Preview switches to the Preview tab; Close Preview discards the preview and switches back to Sections.
- A toolbar above the preview carries **Save HTML** (downloads the file already built, no regeneration) and **Close Preview**.
- Changing any setting, the variables, the aesthetics or the working dataset after generating raises a stale message; the preview is not rebuilt until **Generate Preview** is clicked again. **Save HTML** saves the preview as shown.
- To export as PowerPoint or Word, pick that format and use **Generate & Download**.
- Custom Report has the same option and a Preview tab of its own; its first tab is the editable item list rather than a read-only summary (§E12).

---

## E12 — Custom Report

- **Add to Custom Report** in the Plot output panel snapshots the current plot spec and a thumbnail, with the aesthetics in force at that moment written into the item's own spec. Items added under different appearance settings therefore keep their own look; nothing restyles them at generation time.
- The centre has two tabs: **Items**, the queued list with move up / move down / remove (the info pane previews the selected item's thumbnail), and **Preview**, the In App report (§E11.4). Generate Preview switches to the Preview tab; Close Preview switches back to Items. Reordering, adding or removing items marks the preview stale.
- At generation time each item is **re-rendered from the current working dataset** — the data is not snapshotted.
- An item whose columns no longer exist renders a placeholder; the rest of the report still generates (§M3.7).
- Trend items contribute a plot only (no table).
- Prepare warns before applying changes while items exist (§P7.5).

---

## E13 — Output Formats

Every report may open with Table One, Collinearity and / or the Dataset Summary, then one section per item. **A plot and its table never share a slide or page.**

| Format | Engine | Notes |
|---|---|---|
| **HTML** | `rmarkdown` + `inst/report_template.Rmd` | Floating table of contents; section anchors; dataset-summary variable names link to their sections; back-to-top links |
| **PowerPoint** | `officer` + `rvg` (+ `inst/templates/ppt_16x9_blank_template.pptx`) | Plots as editable vector graphics; multi-panel figures rasterised |
| **Word** | `officer` + `flextable` | No reference template yet (backlog) |

Two-panel plots (`histogram_density`) are split into two figures in every format.

---

## E14 — Section Tables

| Report type | Variables | Table |
|---|---|---|
| Describe | numeric | Statistic · Overall · [stratum …] |
| Describe | factor | Level · N · % · [per stratum] |
| Correlation | numeric × numeric | r, R², p, 95% CI (Pearson) |
| Correlation | numeric × factor | N / mean / median / SD / IQR per level + Kruskal-Wallis p |
| Correlation | factor × factor | Cross-tab N (%) + chi-square / Fisher's p |

All tests come from the shared statistical engine (§N2).

---

## E15 — Programmatic API

The report pipeline runs without Shiny.

```r
edark_report(liver_tx, report_format = "html",
             output_path = tempfile(fileext = ".html"))

edark_report(liver_tx, report_type = "primary_vs_others",
             primary_variable = "age_tx", primary_role = "exposure",
             stratify_variable = "graft_type",
             report_format = "pptx", output_path = tempfile(fileext = ".pptx"))
```

`edark_report(data, report_type = "all_vars", variables = NULL, primary_variable = NULL, primary_role = "exposure", stratify_variable = NULL, report_format = "html", output_path = NULL, max_factor_levels = 20)` validates and casts the data like `edark()` (§P2), then calls `generate_report()`.

`generate_report()` and `generate_custom_report()` take plain arguments (dataset, column types, format, output path, aesthetics, optional `progress_fn(fraction, detail)`), so the same code serves the app's download buttons and scripts. `generate_custom_report()`'s aesthetic arguments default to `NULL`, meaning "keep the appearance each item was captured with" — which is how the app calls it. Passing a value overrides *every* item, for programmatic callers who want one consistent look. `generate_report()`'s `include_dataset_summary` / `include_tableone` / `include_collinearity` default to on / off / off.
