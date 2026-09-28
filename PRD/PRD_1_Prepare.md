# EDARK — Prepare PRD (Tab 1 · Prepare)

**Scope:** everything from the dataset handed to `edark()` up to the working dataset used by Explore and Analyze — launch validation, type casting, column selection, variable transforms, row filters, Apply, and data preview.
**Master PRD:** [PRD_0_Master.md](PRD_0_Master.md) — principles (§M2–M3), state ownership (§M5), cross-stage data flow (§M6).
**Implementation details:** [NOTE_implementation.md](NOTE_implementation.md) §N3.

---

## P1 — Purpose and Scope

Prepare turns the raw dataset into the **working dataset** — the cohort every later stage analyses. It covers three independent concerns: **which columns** are kept, **how variables are transformed**, and **which rows** are kept. All three are staged and take effect together on Apply (§M3.1). The original dataset is never modified; the working dataset is always rebuilt from it.

---

## P2 — Launch: Validation, Auto-Cast, Type Detection

### P2.1 Input Validation (`validate_input()`)
Runs before Shiny starts. Stops with a clear message if:
- `dataset` is not a `data.frame` (tibbles are data frames);
- it has 0 rows or 0 columns;
- `max_factor_levels` is not a single positive integer.

### P2.2 Auto-Cast Rules (`cast_column_types()`)
Applied once at launch, in priority order; each column takes the first rule that matches.

| Priority | Condition | Action |
|---|---|---|
| 1 | Column is `Date` | Convert to `POSIXct` at midnight UTC |
| 2 | Column is `logical` | Convert to factor with levels `c("FALSE", "TRUE")` |
| 3 | Column is `character` and every non-NA value parses as a number | Convert to numeric |
| 4 | Column is `character` with ≤ `max_factor_levels` distinct non-NA values | Convert to factor |
| 5 | Anything else | Leave unchanged |

`max_factor_levels` defaults to 20 and is an argument of `edark()` and `edark_report()`.

### P2.3 Type Detection (`detect_column_types()`)
Returns a named character vector mapping each column to one EDARK type: `"numeric"`, `"factor"`, `"datetime"` or `"character"`. At launch the result is stored twice: `original_column_types` (never changes) and `column_types` (re-detected from the working dataset after every Apply). The two diverge intentionally — e.g. a numeric column turned into a factor by a transform.

---

## P3 — Layout

- **Left sidebar (400 px):** the Apply panel (§P7).
- **Main area:** `navset_card_tab` with four sub-tabs — **Columns** (§P4), **Transforms** (§P5), **Row Filters** (§P6), **Data Preview** (§P8).
- Switching sub-tabs auto-applies staged changes (§P7.3).

---

## P4 — Columns

A compact table with one row per column of the original dataset.

| Element | Behaviour |
|---|---|
| Include checkbox (per row) | Checked = column kept. All checked by default. |
| Select all / Deselect all | Toggle every row. |
| Column name, type, unique count | Read-only description of the column. |

- Excluded columns disappear from every downstream picker after Apply.
- Excluding a column that has an active row filter removes that filter on Apply, with a warning in the Apply panel (§P7.1).
- **Type overrides:** the pipeline supports per-column type overrides (`column_type_overrides`, applied first in §P7.2), but no UI exposes them — the Columns tab is include/exclude only. Type changes are made through transforms.

---

## P5 — Transforms

### P5.1 Table
One row per numeric column. Eligibility uses `original_column_types`, so every column that was numeric at launch always appears (even after a transform has turned it into a factor). Each row has a **Transform** dropdown; methods that need settings show them inline below the row.

Transform specs persist through Apply: returning to the tab shows the currently applied transforms. Choosing **None** removes a column's transform.

### P5.2 Methods

| Method | Output type | Settings | Invalid when |
|---|---|---|---|
| **Auto-factor** (`auto`) | ordered factor — each unique value becomes a level | — | never |
| **Cut points** (`cutpoints`) | ordered factor | breakpoints (comma-separated); optional level labels | no valid breakpoint inside the data range |
| **Log transform** (`log`) | numeric | base: ln / log10 / log2 | any value ≤ 0 |
| **Winsorize** (`winsorize`) | numeric | lower percentile (1-99) and upper percentile (lower + 1 to 100) | lower percentile ≥ upper, or either outside its range |
| **Round** (`round`) | numeric | decimal places | never |
| **Standardize** (`standardize`) | numeric (z-score: mean 0, SD 1) | — | SD = 0 |

Both winsorize boxes clamp themselves: a value outside its range snaps back to the nearest allowed one, and raising the lower percentile pushes the upper one up to stay at least one percentile clear. The bounds live in one place, `.winsor_lower()` / `.winsor_upper()` in `module_column_transform.R`, so the pipeline clamps a spec that arrives from `edark_report()` the same way.

### P5.3 Cut-Point Labels
Breakpoints outside the data range are dropped silently. Default labels describe the ranges — e.g. breakpoints `25, 40` → `"< 25"`, `"25 – < 40"`, `"≥ 40"`. User-supplied labels replace them.

### P5.4 Validity and Navigation
An invalid transform is flagged in the Apply panel (§P7.1). Apply and sub-tab switches are blocked until it is fixed or removed, because applying it would mangle the column.

---

## P6 — Row Filters

- **Add filter:** pick any included column; a filter card is added for it.
- **Numeric columns:** lower and upper limit boxes (inclusive; sent on blur or Enter), prefilled with the observed range, with the column's original range shown for context (and the post-transform range when it differs). Lower above upper is allowed but flagged in the messages area (it keeps no rows).
- **Factor / character columns:** choose which levels to keep (full-size toggle buttons that wrap); all selected by default.
- Filters combine with **AND**.
- Filters can be changed or removed at any time before Apply.
- A filter on a column that is excluded, or that has a transform staged, is removed on Apply with a warning (its levels or range would no longer match the transformed column).

---

## P7 — Apply, Reset, and the Pipeline

### P7.1 Apply Panel (sidebar)
- **Dimensions:** Original, Current (applied) and Pending (what Apply would produce) rows × columns; Pending is highlighted when changes are staged.
- **Status:** pending-changes badge when staged ≠ applied.
- **Warnings**, grouped: transforms that need attention; transforms staged on filtered columns; excluded columns with active filters.
- **Apply Changes** (primary) and **Reset to Original**.

### P7.2 Pipeline Order (mandatory)
The working dataset is always rebuilt from the original, in this order:

1. Start from `dataset_original`
2. Apply column type overrides
3. Select included columns
4. Apply column transforms
5. Apply row filters

Row filters run last so they act on the final (possibly transformed) values.

### P7.3 When the Pipeline Runs
- **Apply Changes** button.
- **Switching Prepare sub-tabs** while changes are pending (auto-apply) — so Data Preview always shows applied data. Blocked, with an error, while any transform is invalid.

### P7.4 After a Successful Apply
`dataset_working` is replaced; `column_types` is re-detected; `has_pending_changes` is cleared; `explore_needs_refresh` is set (Explore shows a stale notice, §E6); `last_applied_specs` is snapshotted (used by revert, and by Analyze at freeze — §M6.6). A brief "Changes applied." notification confirms it.

### P7.5 Reset and the Custom-Report Guard
- **Reset to Original** clears all staged specs and restores the working dataset to the original.
- If Explore custom report items exist, Apply, Reset and auto-apply first show **"Custom Report Will Use the Changed Data"**: the items are kept and re-drawn from the changed dataset when the report is generated (their thumbnails still show the old data; an item whose columns were removed renders a placeholder, §E12). The user has three ways out: continue (**Apply Changes** / **Reset to Original**); **go back and revert**, which restores every staged spec to the last applied state and resyncs every Prepare tab (§M6.5); or **discard items and continue**, which empties `custom_report_items` and then performs the action. Discarding is the only one that settles the question - otherwise the modal fires again on the next Apply and on every later Prepare tab switch, with no way to dismiss it from Prepare. One modal helper, `.custom_items_modal()` in `module_prepare_confirm.R`, serves all three paths.

---

## P8 — Data Preview

Two panels — **Original** and **Working** — each with two sub-tabs:
- **Data:** interactive, searchable `reactable` of the dataset. In the Working view, transformed columns are tinted amber.
- **Summary:** one row per column — type, missing, unique, mean, median, skewness, kurtosis, top values.

---

## P9 — Dataset Export *(backlog, not built)*

Planned: download the working dataset (RDS / CSV), and optionally the original dataset plus the Prepare specs that reproduce it. This overlaps with Analyze Step 8's data export (§A10) and session files (§M8); the three should share one writer. See §M7.

---

## P10 — State Fields

| Field | Type | Meaning |
|---|---|---|
| `dataset_original` | data.frame | Cast dataset from launch. Immutable. |
| `original_column_types` | named chr | Types at launch. Never overwritten. |
| `dataset_working` | data.frame | Result of the last Apply. |
| `column_types` | named chr | Types of the working dataset. |
| `included_columns` | chr | Staged column selection. |
| `column_type_overrides` | named list | Staged type overrides (no UI; see §P4). |
| `column_transform_specs` | named list | Staged transforms, keyed by column. |
| `row_filter_specs` | named list | Staged filters, keyed by column. |
| `has_pending_changes` | logical | Staged ≠ applied. |
| `last_applied_specs` | list | Snapshot of the four staged specs at the last Apply / Reset. |
| `revert_trigger` | integer | Incremented on revert; Prepare modules resync their UI. |
