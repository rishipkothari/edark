#' Appearance Module
#'
#' The app's single set of plot aesthetics controls, and the only writer of
#' the five aesthetic fields in `shared_state`.
#'
#' Before UI Stage 2 these controls existed four times - Describe, Trend, Full
#' Report and Custom Report - and the two Report copies used separate
#' `custom_*` inputs that were applied at generation time, so a generated
#' report did not match the plot on screen (D7, D9, F4 in
#' `PRD/BUILD_UI-redesign.md`).
#'
#' Appearance is not a mode of the Explore plot, it is one app-level setting
#' that every plot and every report reads, so it is a page of its own - a pill
#' beside Explore Data and Report - rather than a fourth panel in the Explore
#' config pane. Sharing that pane with Describe / Correlate / Trend put a live
#' control row next to three staged ones and spent 340 px on settings that are
#' changed rarely and then left alone.
#'
#' The page has no config pane: its whole content *is* configuration, so the
#' controls sit in the main area as one plain list, each group under an
#' `edark_section_label()` (`edark_aesthetics_groups()`). There are five
#' single-value settings in total - a card apiece framed them as four separate
#' artefacts and spent most of the page on chrome. It is the D6 exception the
#' page contract allows for a page that produces nothing.
#'
#' `appearance_controls_ui()` is the narrow stacked form, kept for any pane
#' that needs it. Only one of the two may be in the document at a time - both
#' emit the same input ids.
#'
#' @param id Character. The module namespace ID.
#' @param shared_state A Shiny `reactiveValues` object.
#'
#' @name module_appearance
NULL


#' @rdname module_appearance
#' @export
appearance_controls_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$p(
      class = "text-muted small mb-3",
      "Applies to the plot on screen and to every report generated from it."
    ),
    edark_aesthetics_controls(ns)
  )
}


#' @rdname module_appearance
#' @export
appearance_page_ui <- function(id) {
  ns <- shiny::NS(id)

  # The same stacked list a narrow pane gets, capped and centred: a picker
  # stretched to 1900 px is harder to read than one at a comfortable measure.
  shiny::div(
    class = "edark-settings-page mx-auto",
    shiny::tags$p(
      class = "text-muted mb-4",
      "These settings apply live to the plot in Explore Data and to every ",
      "report generated from it. They are the app's only plot-appearance ",
      "controls."
    ),
    edark_aesthetics_controls(ns)
  )
}


#' @rdname module_appearance
#' @export
appearance_controls_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {

    # Each observer is guarded with !identical() (§N1.2) so a re-render of the
    # control cannot spuriously invalidate everything that reads the plot.
    shiny::observeEvent(input$ggplot_theme, {
      val <- input$ggplot_theme
      if (!is.null(val) && !identical(shared_state$ggplot_theme, val))
        shared_state$ggplot_theme <- val
    })

    shiny::observeEvent(input$color_palette, {
      val <- input$color_palette
      if (!is.null(val) && !identical(shared_state$color_palette, val))
        shared_state$color_palette <- val
    })

    # A plot needed more colours than the palette has (§E7.1): adopt the
    # palette that fits for the rest of the session, and show it in the picker.
    shiny::observeEvent(shared_state$palette_switch, {
      to <- shared_state$palette_switch$to
      if (!is.null(to) && !identical(shared_state$color_palette, to)) {
        shared_state$color_palette <- to
        shinyWidgets::updatePickerInput(session, "color_palette", selected = to)
      }
    })

    shiny::observeEvent(input$show_data_labels, {
      val <- isTRUE(input$show_data_labels)
      if (!identical(shared_state$show_data_labels, val))
        shared_state$show_data_labels <- val
    })

    shiny::observeEvent(input$show_legend, {
      val <- isTRUE(input$show_legend)
      if (!identical(shared_state$show_legend, val))
        shared_state$show_legend <- val
    })

    shiny::observeEvent(input$legend_position, {
      val <- input$legend_position
      if (!is.null(val) && !identical(shared_state$legend_position, val))
        shared_state$legend_position <- val
    })
  })
}


#' The aesthetics in force, as a plot-spec fragment
#'
#' Read wherever a plot or a report needs the current appearance: the Explore
#' plot, Full Report generation, and the snapshot written into a Custom Report
#' item at the moment it is added.
#'
#' @param shared_state A Shiny `reactiveValues` object.
#'
#' @return A named list of the five aesthetic fields.
#' @keywords internal
#' @noRd
edark_current_aesthetics <- function(shared_state) {
  list(
    ggplot_theme     = shared_state$ggplot_theme     %||% "minimal",
    color_palette    = shared_state$color_palette    %||% "Set2",
    show_data_labels = isTRUE(shared_state$show_data_labels),
    show_legend      = isTRUE(shared_state$show_legend),
    legend_position  = shared_state$legend_position  %||% "top"
  )
}
