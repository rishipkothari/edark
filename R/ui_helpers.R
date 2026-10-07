#' Shared UI helpers
#'
#' Tag-building helpers, plus the single home for interface strings that must
#' agree in more than one place. Everything here returns tag objects or wires
#' one output; nothing here holds reactive state or calls a module server.
#'
#' Grown one stage at a time by the UI consistency plan
#' (`PRD/BUILD_UI-redesign.md`). Stage 1 adds only what honest locking needs:
#' the lock-reason table and the run button that shows its own reason. Stage 2
#' adds the rest of the component library (`edark_section_label()`,
#' `edark_empty_state()`, `edark_message()`, `edark_button()`, ...).
#'
#' @name ui_helpers
#' @keywords internal
#' @noRd
NULL


# ── Lock reasons ──────────────────────────────────────────────────────────────

#' Every "do X first" message, written once
#'
#' A locked step's popover, an in-panel placeholder and a disabled run button's
#' reason text all read from this vector, so the three cannot disagree
#' (§BUILD_UI-redesign Stage 1).
#'
#' Wording rules:
#' \itemize{
#'   \item Name the destination exactly as the navigation shows it -
#'     \code{Step 1 \\u00b7 Setup}, \code{Model \\u203a Create}.
#'   \item Say what to do, not what is wrong: "Fit a model ... first", not
#'     "no model fitted".
#'   \item One sentence, ending in a full stop - the same string has to read
#'     well in a popover, a placeholder and small text under a button.
#' }
#'
#' @keywords internal
#' @noRd
EDARK_LOCK_REASON <- c(
  # Step 1 preconditions
  analysis_start      = "Freeze a dataset in Step 1 \u00b7 Setup first.",
  analysis_roles      = "Assign at least one variable role in Step 1 \u00b7 Setup first.",
  analysis_outcome    = "Freeze a dataset and assign an outcome in Step 1 \u00b7 Setup first.",
  analysis_candidates = "Assign candidate covariates in Step 1 \u00b7 Setup first.",
  analysis_tier1      = "Resolve the Step 1 \u00b7 Setup errors shown above first.",

  # Model preconditions
  model_type_none     = "No model type fits this outcome. Change the outcome in Step 1 \u00b7 Setup.",
  model_preflight     = "Resolve the preflight errors before fitting the model.",
  fit_model           = "Fit a model in Model \u203a Create first.",

  # Nothing selected to act on
  pick_measure        = "Tick at least one measure.",
  pick_output         = "Tick at least one output.",
  pick_export         = "Tick at least one file.",

  # Explore
  plot_first          = "Run a plot first."
)


#' Look up a lock reason
#'
#' Fails loudly on an unknown key so a typo cannot silently produce an empty
#' reason. Always use this rather than subsetting `EDARK_LOCK_REASON` directly.
#'
#' @param key Character. A name of `EDARK_LOCK_REASON`.
#'
#' @return Character scalar.
#' @keywords internal
#' @noRd
edark_lock_reason <- function(key) {
  if (length(key) != 1L || !key %in% names(EDARK_LOCK_REASON)) {
    stop("Unknown lock reason key: ", paste(key, collapse = ", "),
         ". Add it to EDARK_LOCK_REASON in R/ui_helpers.R.", call. = FALSE)
  }
  unname(EDARK_LOCK_REASON[[key]])
}


# ── Run buttons that explain themselves ──────────────────────────────────────

#' A primary run button with a slot for its unmet precondition
#'
#' Renders the button plus an output directly beneath it that carries the
#' reason the button is disabled. Pair it with `edark_run_gate()` in the
#' module server: the button is never re-rendered (so its click count is
#' stable), only its enabled state and the reason text change.
#'
#' Disabled-with-visible-reason is the app's only precondition affordance - a
#' button must not sit enabled and answer a click with a toast
#' (§BUILD_UI-redesign Stage 1).
#'
#' @param ns The module's namespace function (`session$ns` or `shiny::NS(id)`).
#' @param id Character. The button's bare (un-namespaced) id. The reason slot
#'   takes `paste0(id, "_reason")`.
#' @param label Character or tag. The button label - verb + object
#'   ("Fit Model", never "Run").
#' @param icon Character or NULL. Font Awesome name shown before the label.
#' @param variant Character. Bootstrap variant, without the `btn-` prefix.
#' @param enabled Logical. The state the button is *first rendered* in.
#'   Defaults to FALSE because a gated button's precondition is unmet at
#'   launch; `edark_run_gate()` takes over on the first flush. Rendering it
#'   enabled first would flash a clickable button.
#'
#' @return A `shiny::tagList`.
#' @keywords internal
#' @noRd
edark_run_button <- function(ns, id, label, icon = "play", variant = "primary",
                             enabled = FALSE) {
  body <- if (is.null(icon)) label else shiny::tagList(shiny::icon(icon), " ", label)
  btn  <- shiny::actionButton(ns(id), label = body,
                              class = sprintf("btn-%s w-100", variant))
  shiny::tagList(
    if (enabled) btn else shinyjs::disabled(btn),
    shiny::uiOutput(ns(paste0(id, "_reason")))
  )
}


#' Gate an `edark_run_button()` on a precondition
#'
#' Call once per run button inside `moduleServer()`. Disables the button while
#' `enabled` is false and writes `reason` into the button's reason slot, so the
#' reason is on screen rather than hidden in a tooltip or delivered as a toast
#' after the click.
#'
#' @param output The module's `output` object.
#' @param id Character. The same bare id passed to `edark_run_button()`.
#' @param enabled A reactive expression (or plain logical) - TRUE when the
#'   button may be clicked.
#' @param reason A reactive expression returning a character scalar, or a plain
#'   character scalar, from `edark_lock_reason()`. Only read while disabled;
#'   NULL or "" shows no text.
#'
#' @return Invisible NULL, called for its side effects.
#' @keywords internal
#' @noRd
edark_run_gate <- function(output, id, enabled, reason) {
  .value <- function(x) if (is.function(x)) x() else x

  ok <- shiny::reactive(isTRUE(.value(enabled)))

  shiny::observe(shinyjs::toggleState(id, condition = ok()))

  output[[paste0(id, "_reason")]] <- shiny::renderUI({
    if (ok()) return(NULL)
    txt <- .value(reason)
    if (is.null(txt) || !nzchar(txt)) return(NULL)
    shiny::tags$p(class = "edark-run-reason small text-muted mt-2 mb-0",
                  shiny::icon("lock"), " ", txt)
  })

  invisible(NULL)
}


# ── Section labels ────────────────────────────────────────────────────────────

#' The uppercase label that heads a group of controls
#'
#' The one place the label's classes are written. Before Stage 2 this string
#' was re-typed ~30 times across 11 modules, plus four private `.hdr()` copies
#' and one `.sidebar_label()`, and had already drifted into a second spelling
#' in Prepare (§BUILD_UI-redesign 2.5).
#'
#' @param text Character. The label, in sentence case - the CSS uppercases it.
#' @param first Logical. TRUE for the first label in a pane, which drops the
#'   top margin so the pane does not start with a gap.
#'
#' @return A `shiny::tags$p`.
#' @keywords internal
#' @noRd
edark_section_label <- function(text, first = FALSE) {
  shiny::tags$p(
    text,
    class = paste0(
      "edark-section-label text-muted small text-uppercase fw-semibold ",
      if (isTRUE(first)) "mt-0" else "mt-2", " mb-1"
    )
  )
}


# ── Buttons ───────────────────────────────────────────────────────────────────

#' Button classes, by the action's scope
#'
#' Two scales only (F1 / D10): an action that changes the configuration is
#' full width at default size in the config pane; an action on an
#' already-produced artefact is small and outlined, in an
#' `edark_action_toolbar()`. Dialog actions are default size and shrink to
#' their label.
#'
#' @keywords internal
#' @noRd
.edark_btn_class <- function(variant, size, outline = FALSE) {
  base <- if (isTRUE(outline)) sprintf("btn-outline-%s", variant)
          else                 sprintf("btn-%s", variant)
  switch(
    size,
    config  = paste(base, "w-100"),
    toolbar = paste("btn-sm", base),
    dialog  = base,
    stop("Unknown button size: ", size, call. = FALSE)
  )
}


#' The only place a Bootstrap button class is written
#'
#' Every button in the app goes through here, so the scale cannot drift the
#' way it had by Stage 2: `btn-primary w-100`, `w-75`, `btn-sm
#' btn-outline-primary` and an unclassed `input_task_button` were all in use,
#' two of them inside one pane (§BUILD_UI-redesign 2.5, 1.3a).
#'
#' @param ns The module's namespace function, or NULL outside a module.
#' @param id Character. The button's bare id.
#' @param label Character or tag.
#' @param icon Character or NULL. Font Awesome name.
#' @param variant Character. Bootstrap variant without the `btn-` prefix.
#' @param size One of `"config"`, `"toolbar"`, `"dialog"`.
#' @param outline Logical. Outlined rather than filled - the de-emphasised
#'   form. Defaults to TRUE for toolbar buttons, which are always outlined
#'   (D10), and FALSE elsewhere.
#' @param type One of `"action"`, `"download"`, `"task"`. `"task"` is
#'   `bslib::input_task_button()`, which keeps its busy state but is given the
#'   same scale as every other button rather than being an exception at the
#'   call site.
#' @param class Character or NULL. Extra utility classes (spacing, padding)
#'   appended to the scale classes. Never a `btn-*` class - those come from
#'   `variant`, `size` and `outline`, which is the point of the helper. Taken
#'   as a named argument rather than through `...` so it is appended rather
#'   than emitted as a second `class` attribute.
#' @param ... Passed to the underlying button function.
#'
#' @return A button tag.
#' @keywords internal
#' @noRd
edark_button <- function(ns, id, label, icon = NULL, variant = "primary",
                         size = "config", type = "action",
                         outline = identical(size, "toolbar"),
                         class = NULL, ...) {
  nsf <- if (is.null(ns)) identity else ns
  ico <- if (is.null(icon)) NULL else shiny::icon(icon)

  if (any(grepl("^btn-", strsplit(paste(class, collapse = " "), "\\s+")[[1]]))) {
    stop("edark_button(class=) must not carry a btn-* class; ",
         "use variant / size / outline instead.", call. = FALSE)
  }

  if (identical(type, "task")) {
    # input_task_button carries its own variant via `type`; only the layout
    # class is ours, or the two fight over btn-*.
    return(bslib::input_task_button(
      nsf(id), label = label, icon = ico, type = variant,
      class = paste(c(switch(size, config = "w-100", toolbar = "btn-sm", dialog = NULL),
                      class), collapse = " "),
      ...
    ))
  }

  cls <- paste(c(.edark_btn_class(variant, size, outline), class), collapse = " ")
  switch(
    type,
    action   = shiny::actionButton(nsf(id), label = label, icon = ico, class = cls, ...),
    download = shiny::downloadButton(nsf(id), label = label, icon = ico, class = cls, ...),
    stop("Unknown button type: ", type, call. = FALSE)
  )
}


#' The right-aligned row of actions that sits above a produced artefact
#'
#' Actions *on* a result (Save, Copy, Add to Custom Report, Appearance) live
#' with the result, never in a pane (F2 / D10).
#'
#' @param ... Buttons, built with `edark_button(size = "toolbar")`.
#'
#' @return A `shiny::div`.
#' @keywords internal
#' @noRd
edark_action_toolbar <- function(...) {
  shiny::div(
    class = "edark-action-toolbar d-flex justify-content-end align-items-center gap-2 mb-2",
    ...
  )
}


# ── Empty states ──────────────────────────────────────────────────────────────

#' What a panel shows before it has anything to show
#'
#' Replaces the six empty-state grammars counted in §BUILD_UI-redesign 2.5.
#'
#' @param title Character. What is missing, or what to do next.
#' @param body Character or tag. Optional second line.
#' @param icon Character or NULL. Font Awesome name.
#'
#' @return A `shiny::div`.
#' @keywords internal
#' @noRd
edark_empty_state <- function(title, body = NULL, icon = "circle-info") {
  shiny::div(
    class = "edark-empty-state text-center text-muted py-5",
    if (!is.null(icon))
      shiny::div(class = "edark-empty-state-icon mb-2", shiny::icon(icon)),
    shiny::tags$p(class = "fw-semibold mb-1", title),
    if (!is.null(body)) shiny::tags$p(class = "small mb-0", body)
  )
}


# ── Badges ────────────────────────────────────────────────────────────────────

#' Variant for each badge role, written once
#'
#' Before this, Prepare rendered a column's type as `tags$code` while Analyze
#' rendered the same fact as a coloured Bootstrap badge at two different font
#' sizes. One fact, one look: every badge in the app comes from
#' `edark_badge()` and takes its colour from here.
#'
#' Keys are roles, not colours, so a caller never names a Bootstrap variant.
#'
#' No entry names a text colour: `text-bg-*` computes one from the background,
#' and the `text-dark` these three carried was written for flatly's bright
#' orange warning, which `bs_theme(warning =)` in `edark()` has since muted.
#'
#' @keywords internal
#' @noRd
.EDARK_BADGE_VARIANT <- c(
  # Column types (detect_column_types() values)
  numeric   = "primary",
  factor    = "success",
  datetime  = "warning",
  character = "secondary",

  # Study type (Analyze > Setup info pane)
  study_exposure_outcome     = "primary",
  study_risk_factor          = "success",
  study_descriptive_exposure = "warning",
  study_descriptive          = "secondary",

  # Other roles
  role      = "dark",       # a variable's assigned analysis role
  count     = "primary",    # a live count beside a label
  neutral   = "secondary",
  muted     = "light text-dark",
  changed   = "warning"
)


#' One badge
#'
#' The app's only badge builder. Callers pass a role from
#' `.EDARK_BADGE_VARIANT`; unknown roles fall back to `muted` rather than
#' erroring, so a new column type shows up as a plain badge instead of
#' breaking a table cell.
#'
#' @param text Character or tag. What the badge says.
#' @param role Character. A name of `.EDARK_BADGE_VARIANT`.
#' @param size One of "sm" (in-table, the default) or "md" (standalone).
#' @param class Character or NULL. Extra classes, e.g. `"w-100 d-block"`.
#' @param ... Passed to `htmltools::tags$span()` (`title`, `style`, ...).
#'
#' @return A `htmltools::tags$span`.
#' @keywords internal
#' @noRd
edark_badge <- function(text, role = "neutral", size = c("sm", "md"),
                        class = NULL, ...) {
  size    <- match.arg(size)
  variant <- unname(.EDARK_BADGE_VARIANT[role])
  if (length(variant) != 1L || is.na(variant)) variant <- .EDARK_BADGE_VARIANT[["muted"]]

  htmltools::tags$span(
    class = paste(
      c("badge", paste0("text-bg-", variant), "edark-badge",
        paste0("edark-badge-", size), class),
      collapse = " "
    ),
    ...,
    text
  )
}


#' A column type, as a badge
#'
#' Used by every table that shows a type - Prepare > Columns, Prepare > Data
#' Preview, Analyze > Setup, Analyze > Covariates - so the same type reads the
#' same everywhere.
#'
#' @param type Character. A `detect_column_types()` value.
#' @param transformed Logical. TRUE marks a column the Prepare pipeline has
#'   transformed (cast to a new type, or rescaled in place). The type badge is
#'   untouched and a separate "T" badge (`edark_transformed_badge()`) follows
#'   it, so the mark reads as a fact rather than as part of the type.
#'
#' @return A `htmltools::tags$span`, or a tag list of two when transformed.
#' @keywords internal
#' @noRd
edark_type_badge <- function(type, transformed = FALSE) {
  if (is.null(type) || length(type) != 1L || is.na(type)) {
    return(edark_badge("-", role = "muted"))
  }
  badge <- edark_badge(
    type,
    role = if (type %in% names(.EDARK_BADGE_VARIANT)) type else "muted"
  )
  if (!isTRUE(transformed)) return(badge)
  htmltools::tagList(badge, edark_transformed_badge())
}


#' The "transformed" mark, as its own small badge
#'
#' One letter in the Prepare tint (`.edark-badge-transformed` in edark.css),
#' with the meaning in words on its title. Used beside a type badge and in the
#' Data Preview column headers, so "the pipeline touched this" is one mark
#' everywhere.
#'
#' @return A `htmltools::tags$span`.
#' @keywords internal
#' @noRd
edark_transformed_badge <- function() {
  htmltools::tags$span(
    class = "badge edark-badge edark-badge-sm edark-badge-transformed ms-1",
    title = "Transformed by the Prepare pipeline",
    "T"
  )
}


# ── Status messages ───────────────────────────────────────────────────────────

#' The severity scale, named once
#'
#' Before Stage 2 the app used four vocabularies for one scale: `alert-*`
#' blocks, `badge bg-*`, `card(class = "border-warning")` and transient
#' toasts (§BUILD_UI-redesign 2.4). Stage 4 gives these a dedicated messages
#' area; this is the tag they render as.
#'
#' @keywords internal
#' @noRd
.EDARK_MESSAGE_LEVELS <- list(
  ok      = list(class = "alert-success",   icon = "circle-check"),
  info    = list(class = "alert-info",      icon = "circle-info"),
  warn    = list(class = "alert-warning",   icon = "triangle-exclamation"),
  error   = list(class = "alert-danger",    icon = "circle-exclamation"),
  pending = list(class = "alert-secondary", icon = "clock"),
  stale   = list(class = "alert-warning",   icon = "rotate"),
  locked  = list(class = "alert-secondary", icon = "lock")
)


#' One warning, error, blocker or stale notice
#'
#' @param level One of the names of `.EDARK_MESSAGE_LEVELS`.
#' @param text Character or tag. The message itself, one sentence.
#' @param detail Character or tag. Optional smaller second line.
#'
#' @return A `shiny::div`.
#' @keywords internal
#' @noRd
edark_message <- function(level, text, detail = NULL) {
  spec <- .EDARK_MESSAGE_LEVELS[[level]]
  if (is.null(spec)) {
    stop("Unknown message level: ", level,
         ". Add it to .EDARK_MESSAGE_LEVELS in R/ui_helpers.R.", call. = FALSE)
  }
  shiny::div(
    class = paste("edark-message alert py-2 px-3 mb-2", spec$class),
    shiny::div(
      class = "d-flex align-items-baseline gap-2",
      shiny::icon(spec$icon),
      shiny::div(
        shiny::div(text),
        if (!is.null(detail)) shiny::div(class = "small mt-1 opacity-75", detail)
      )
    )
  )
}


# ── Info pane rows ────────────────────────────────────────────────────────────

#' One label / value line in an info pane
#'
#' The info pane is neutral and factual: what the current settings produce
#' (D2). It never holds an input or an action.
#'
#' @param label Character. What the number is.
#' @param value Character or tag. The number.
#'
#' @return A `shiny::div`.
#' @keywords internal
#' @noRd
edark_info_row <- function(label, value) {
  shiny::div(
    class = "edark-info-row d-flex justify-content-between align-items-baseline gap-2",
    shiny::tags$span(class = "text-muted small", label),
    shiny::tags$span(class = "small fw-semibold text-end", value)
  )
}


# ── Model header ──────────────────────────────────────────────────────────────

#' The card that names the fitted model, above a Model sub-tab's output
#'
#' Replaces four near-duplicates that carried different fields
#' (§BUILD_UI-redesign 2.5). Each caller passes the fields that apply to it;
#' one line per field.
#'
#' @param title Character. The model type label.
#' @param fields Named list. Label -> value, one `small` line each. NULL
#'   entries are dropped, so a caller may pass a field it might not have.
#' @param notes Character vector or list of tags. Footnote lines, rendered
#'   muted beneath the fields.
#'
#' @return A `bslib::card`.
#' @keywords internal
#' @noRd
edark_model_header <- function(title, fields = list(), notes = NULL) {
  fields <- fields[!vapply(fields, is.null, logical(1))]

  bslib::card(
    bslib::card_body(
      class = "py-2",
      shiny::div(class = "fw-semibold", title),
      lapply(names(fields), function(nm) {
        shiny::div(class = "small mt-1",
                   shiny::span(class = "text-muted", paste0(nm, ": ")),
                   fields[[nm]])
      }),
      lapply(notes, function(x) shiny::div(class = "small text-muted mt-1", x))
    )
  )
}


# ── Plot aesthetics ───────────────────────────────────────────────────────────

#' The app's only copy of the plot aesthetics controls
#'
#' Aesthetics are one app-level setting, not a per-mode and not a per-report
#' one (D9, F4). Before Stage 2 there were four copies of these controls -
#' Describe, Trend, Full Report and Custom Report - and the two Report copies
#' used `custom_*` ids applied at generation time, so an exported report did
#' not match the plot the user had been looking at (D7).
#'
#' The controls apply live, so they are kept away from the staged mode panels
#' (§BUILD_UI-redesign 2.6). They now live on their own Appearance page, a
#' sibling pill of Explore Data and Report.
#'
#' Ids are bare; the owning module writes them into `shared_state`.
#'
#' Returned as named groups so one definition of the inputs serves both
#' layouts: stacked under section labels for a 340 px pane
#' (`edark_aesthetics_controls()`), or a card each on the full-width
#' Appearance page. The ids must not be written twice - two copies of
#' `ns("ggplot_theme")` in one document is a duplicate input.
#'
#' @param ns The owning module's namespace function.
#'
#' @return A named list of tag lists. Names are the group headings.
#' @keywords internal
#' @noRd
edark_aesthetics_groups <- function(ns) {
  list(
    "Theme" = shinyWidgets::pickerInput(
      ns("ggplot_theme"),
      label    = NULL,
      choices  = c(
        "Minimal"         = "minimal",
        "Publication"     = "publication",
        "Cowplot"         = "cowplot",
        "Economist"       = "economist",
        "FiveThirtyEight" = "fivethirtyeight",
        "Tufte"           = "tufte",
        "Modern"          = "modern"
      ),
      selected = "minimal"
    ),

    # Choices come from .EDARK_PALETTES (R/plot_palettes.R): grouped, each
    # with a strip of its colours and, if qualitative, how many it has.
    "Colour palette" = local({
      pal <- .palette_picker_choices()
      shinyWidgets::pickerInput(
        ns("color_palette"),
        label      = NULL,
        choices    = pal$choices,
        choicesOpt = list(content = pal$content),
        selected   = "Set2"
      )
    }),

    "Legend" = shiny::tagList(
      shiny::checkboxInput(ns("show_legend"), "Show legend", value = TRUE),
      shinyWidgets::radioGroupButtons(
        ns("legend_position"),
        label    = NULL,
        choices  = c("right", "left", "top", "bottom"),
        selected = "top",
        size     = "sm",
        width    = "100%"
      )
    ),

    "Labels" = shiny::checkboxInput(ns("show_data_labels"),
                                    "Show data labels", value = FALSE)
  )
}


#' The aesthetics controls stacked for a narrow pane
#'
#' @inheritParams edark_aesthetics_groups
#'
#' @return A `shiny::tagList`.
#' @keywords internal
#' @noRd
edark_aesthetics_controls <- function(ns) {
  groups <- edark_aesthetics_groups(ns)
  shiny::tagList(
    lapply(seq_along(groups), function(i) {
      shiny::tagList(
        edark_section_label(names(groups)[i], first = i == 1L),
        groups[[i]]
      )
    })
  )
}


# ── App identity ─────────────────────────────

#' The version and last-updated date shown in the navbar and on the splash
#'
#' One definition each, so the navbar and `edark_splash()` can never disagree -
#' they used to carry the version as two separate literals. `EDARK_VERSION`
#' must match `Version:` in DESCRIPTION.
#'
#' `EDARK_LAST_UPDATE` is the date of the most recent commit and is maintained
#' by hand: bump it in the same commit as the work it describes (see CLAUDE.md,
#' Coding philosophy). It is deliberately not read from git, because an
#' installed package is not a repository.
#'
#' @keywords internal
#' @noRd
EDARK_VERSION <- "1.0"

#' @rdname EDARK_VERSION
#' @keywords internal
#' @noRd
EDARK_LAST_UPDATE <- "2026-10-07"


#' The loading splash, shown until the app is ready to use
#'
#' Sits in the `page_navbar()` header, so it is in the page HTML from the first
#' byte and covers the whole startup gap - measured at 2.4 s warm and 4.1 s cold
#' from page request to `shiny:idle` (§N1.16). A centred card over a plain
#' backdrop, so the app is never seen half-built behind it.
#'
#' It holds for at least 2 s even when the app is ready sooner, and the final
#' fill is stretched across whatever hold is left so the bar completes as the
#' card leaves.
#'
#' The bar is divided into three equal segments, each completed by a real
#' browser event rather than a timer: `shiny:connected`, the first
#' `shiny:value`, then `shiny:idle`. Startup progress cannot come from the
#' server - messages sent while the server function runs are queued until it
#' returns, so they would all arrive after the work they describe. A slow creep
#' inside each segment keeps the bar moving without ever completing a segment
#' the browser has not reported.
#'
#' Not `waiter`: that is for busy-spinners over outputs. This needs a full
#' bleed gradient, the app's own staged-bar idiom and precise hide timing.
#'
#' @return A `shiny::tagList` - the overlay and its controlling script.
#' @keywords internal
#' @noRd
edark_splash <- function() {
  shiny::tagList(
    shiny::div(
      id = "edark-splash", class = "edark-splash",

      # A card, not the whole viewport: the overlay behind it is the plain body
      # colour, so the app is never seen assembling itself.
      shiny::div(
        class = "edark-splash-card",

        shiny::div(
          class = "edark-splash-inner",
          shiny::div(class = "edark-splash-title", "EDARK"),
          shiny::div(class = "edark-splash-subtitle",
                     "Exploratory Data Analysis GUI"),
          shiny::div(
            class = "edark-splash-progress",
            shiny::div(
              class = "edark-splash-track",
              shiny::div(id = "edark-splash-bar", class = "edark-splash-bar"),
              # Equal thirds, so the ticks are fixed rather than computed.
              shiny::div(class = "edark-splash-tick", style = "left: 33.333%;"),
              shiny::div(class = "edark-splash-tick", style = "left: 66.667%;")
            ),
            shiny::div(
              class = "edark-splash-stops",
              # One word each: the card is narrow, and two-word labels wrap.
              shiny::tags$span(class = "edark-splash-stop", "Connecting"),
              shiny::tags$span(class = "edark-splash-stop", "Building"),
              shiny::tags$span(class = "edark-splash-stop", "Preparing")
            )
          )
        ),

        shiny::div(
          class = "edark-splash-footer",
          shiny::div(class = "edark-splash-version", paste0("v", EDARK_VERSION)),
          shiny::div(
            class = "edark-splash-meta",
            shiny::div("creator: RK"),
            shiny::div(paste0("last update: ", EDARK_LAST_UPDATE))
          )
        )
      )
    ),

    shiny::tags$script(shiny::HTML(paste(
      "(function() {",
      "  var el = document.getElementById('edark-splash');",
      "  if (!el) return;",
      "  var bar   = document.getElementById('edark-splash-bar');",
      "  var stops = el.querySelectorAll('.edark-splash-stop');",
      "  var EDGES = [0, 1/3, 2/3, 1];",
      "  var MIN_MS = 2000, MAX_MS = 15000;",
      "  var shownAt = Date.now(), stage = 0, creep = 0, finished = false;",
      "",
      "  function paint(f) { bar.style.width = (f * 100).toFixed(1) + '%'; }",
      "",
      "  function markStops() {",
      "    for (var i = 0; i < stops.length; i++) {",
      "      stops[i].classList.toggle('is-done',   i <  stage);",
      "      stops[i].classList.toggle('is-active', i === stage);",
      "    }",
      "  }",
      "",
      "  var timer = setInterval(function() {",
      "    if (finished) return;",
      "    var from = EDGES[stage], to = EDGES[stage + 1];",
      "    if (to === undefined) return;",
      "    creep += (to - from) * 0.02;",
      "    paint(Math.min(from + creep, from + (to - from) * 0.9));",
      "  }, 120);",
      "",
      "  function advance() {",
      "    if (finished || stage >= 3) return;",
      "    stage += 1; creep = 0;",
      "    paint(EDGES[stage]); markStops();",
      "  }",
      "",
      "  function finish() {",
      "    if (finished) return;",
      "    finished = true;",
      "    clearInterval(timer);",
      "    stage = 3; markStops();",
      "    // Ready early: stretch the last fill across the remaining hold so",
      "    // the bar completes as the card leaves, rather than sitting full.",
      "    var hold = Math.max(0, MIN_MS - (Date.now() - shownAt));",
      "    bar.style.transitionDuration = Math.max(250, hold) + 'ms';",
      "    paint(1);",
      "    setTimeout(function() { el.classList.add('is-hidden'); }, hold + 200);",
      "  }",
      "",
      "  markStops();",
      "  $(document).one('shiny:connected', advance);",
      "  $(document).one('shiny:value',     advance);",
      "  $(document).one('shiny:idle',      finish);",
      "  setTimeout(finish, MAX_MS);",
      "})();",
      sep = "\n"
    )))
  )
}


# ── The page contract ─────────────────────────────────────────────────────────

#' The two pane widths, written once
#'
#' Before Stage 4 the app used six sidebar widths - 400, 380, 405, 390, 360 and
#' 340, three of them inside Step 3 alone - and two pages flipped the sidebar to
#' the right mid-workflow (§BUILD_UI-redesign 2.2). D6 settles it: config is
#' always left at 340 px, info always right at 300 px.
#'
#' @keywords internal
#' @noRd
EDARK_CONFIG_WIDTH <- 340

#' @rdname EDARK_CONFIG_WIDTH
#' @keywords internal
#' @noRd
EDARK_INFO_WIDTH <- 300

#' Viewport-relative height cap for a result-pane table
#'
#' Passed to `reactable::reactable(height=)`, which writes it as an inline
#' style and so beats bslib's own `.bslib-card .card-body` rule. Non-reactable
#' content uses the `.edark-scroll-table` class instead - keep that rule's
#' `max-height` in `inst/www/edark.css` in step with this value by hand, since
#' CSS cannot read it.
#'
#' @keywords internal
#' @noRd
EDARK_RESULT_HEIGHT <- "calc(100vh - 320px)"


#' Lay a page out as config / messages / result / info
#'
#' Every page has the same four places (D2 + D3 + D6):
#'
#' \preformatted{
#' +--------------+-------------------------------------+-------------+
#' | CONFIG       | MESSAGES (only when there are any)  | INFO        |
#' | left, 340 px +-------------------------------------+ right,      |
#' |              | RESULT                              | 300 px      |
#' | [Primary]    |  the one artefact this page makes   |             |
#' +--------------+-------------------------------------+-------------+
#' }
#'
#' The rules each pane follows:
#' \itemize{
#'   \item \strong{Config} holds inputs and exactly one primary action, at the
#'     bottom, in every mode of the page (F3). No read-only facts.
#'   \item \strong{Result} holds the one artefact, with actions *on* it in an
#'     `edark_action_toolbar()` directly above (F2).
#'   \item \strong{Info} is neutral, factual and live: what the current
#'     settings produce. Never an input, never an action, never a warning.
#'   \item \strong{Messages} is the only place a warning, error, blocker or
#'     stale notice appears. Empty means it takes no space.
#' }
#'
#' @param config Tag list for the left sidebar.
#' @param result Tag list for the centre. Required: every page has one. The
#'   D11 exception that let this be `NULL` and dropped the centre column is
#'   gone - Report › Custom was its only user, and it now puts its item list
#'   in the centre like every other page. A page with no obvious artefact has
#'   one it has not identified yet, not grounds for two panes.
#' @param info Tag list for the right pane. Every page owes it real content
#'   (F7); a page with nothing factual to say is missing information the user
#'   wants, not a candidate for dropping the pane.
#' @param messages Tag or `shiny::uiOutput()` for the messages slot.
#' @param info_open Passed to `bslib::sidebar(open=)`. Set `"closed"` on a page
#'   whose centre is a wide table, so 1280 px still works (§M9.4 NF-05).
#' @param info_title Optional heading for the info pane.
#'
#' @return A `bslib::layout_sidebar`.
#' @keywords internal
#' @noRd
edark_page <- function(config, result, info = NULL, messages = NULL,
                       info_open = TRUE, info_title = NULL) {

  info_body <- if (is.null(info)) NULL else shiny::tagList(
    if (!is.null(info_title)) edark_section_label(info_title, first = TRUE),
    info
  )

  if (missing(result) || is.null(result)) {
    stop("edark_page(result=) is required: every page shows its artefact in ",
         "the centre. See the note on the `result` argument.", call. = FALSE)
  }

  centre <- if (is.null(info)) {
    shiny::tagList(messages, result)
  } else {
    # No border and no rounding of its own: the outer layout's frame is the
    # only one, so config | centre | info read as three columns of one box
    # rather than a box inside a box.
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        position = "right",
        width    = EDARK_INFO_WIDTH,
        open     = info_open,
        class    = "edark-info-pane",
        info_body
      ),
      fillable      = FALSE,
      border        = FALSE,
      border_radius = FALSE,
      shiny::tagList(messages, result)
    )
  }

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      position = "left",
      width    = EDARK_CONFIG_WIDTH,
      class    = "edark-config-pane",
      config
    ),
    fillable = FALSE,
    # With an info pane, the inner layout brings the centre's padding and the
    # info pane sits flush to the frame; padding here as well was the second,
    # inset box. Without one, the centre needs this padding itself.
    padding  = if (is.null(info)) NULL else 0,
    centre
  )
}


#' The messages slot for a page
#'
#' One per page. Renders whatever `edark_message()` items the module's server
#' writes into it, and takes no space when there are none. Every warning,
#' error, blocker and stale notice on the page goes here and nowhere else
#' (D3) - not into the info pane, and not into a toast.
#'
#' @param ns The module's namespace function.
#' @param id Character. Defaults to `"messages"`, so a module normally has one
#'   and does not have to name it.
#'
#' @return A `shiny::uiOutput`.
#' @keywords internal
#' @noRd
edark_messages_ui <- function(ns, id = "messages") {
  shiny::uiOutput(ns(id), class = "edark-messages")
}


#' Fill a page's messages slot
#'
#' @param output The module's `output` object.
#' @param items A reactive returning a list of `edark_message()` tags, or
#'   `NULL` / an empty list when the page has nothing to say.
#' @param id Character. Matches `edark_messages_ui()`.
#'
#' @return Invisible NULL, called for its side effect.
#' @keywords internal
#' @noRd
edark_messages_server <- function(output, items, id = "messages") {
  output[[id]] <- shiny::renderUI({
    msgs <- if (is.function(items)) items() else items
    msgs <- Filter(Negate(is.null), as.list(msgs))
    if (length(msgs) == 0) return(NULL)
    shiny::tagList(msgs)
  })

  # The slot takes no space when empty, which `.edark-messages:empty` in
  # edark.css does with `display: none`. Shiny suspends a hidden output, so an
  # empty slot would never recompute and could never stop being empty - the
  # rule that keeps it out of the way also kept every message in the app from
  # ever appearing. Opting this one output out of suspension breaks the loop.
  shiny::outputOptions(output, id, suspendWhenHidden = FALSE)
  invisible(NULL)
}
