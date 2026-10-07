#' Explore Output Module
#'
#' Main panel for the Explore stage. Observes `shared_state$plot_specification`
#' and renders the plot. The variable summary and the custom-report state go to
#' the page's info pane, and the "dataset has changed" notice to its messages
#' slot (D2, D3 - the page contract, BUILD_UI-redesign Stage 4).
#'
#' Aesthetic changes (palette, labels, legend) re-render the plot without
#' rebuilding the spec — the spec is already stored in shared_state.
#'
#' @param id Character. The module namespace ID.
#' @param shared_state A Shiny `reactiveValues` object.
#'
#' @name module_explore_output
NULL


#' @rdname module_explore_output
#' @export
explore_output_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    # Actions on the produced plot live with the plot, not in a pane (F2 / D10)
    edark_action_toolbar(
      edark_button(ns, "save_plot_btn", "Save Plot", icon = "download",
                   size = "toolbar", type = "download"),
      edark_button(ns, "copy_plot_btn", "Copy to Clipboard", icon = "copy",
                   size = "toolbar"),
      edark_button(ns, "add_to_custom_btn", "Add to Custom Report", icon = "plus",
                   size = "toolbar"),
      shiny::uiOutput(ns("view_report_btn_ui"))
    ),

    # JS for copy-to-clipboard: reads the rendered plot <img> and writes to clipboard
    shiny::tags$script(shiny::HTML(paste0(
      "$(document).on('click', '#", ns("copy_plot_btn"), "', function() {",
      "  var imgEl = document.querySelector('#", ns("main_plot"), " img');",
      "  if (!imgEl) { alert('No plot to copy. Run a plot first.'); return; }",
      "  fetch(imgEl.src)",
      "    .then(function(r) { return r.blob(); })",
      "    .then(function(blob) {",
      "      return navigator.clipboard.write([new ClipboardItem({'image/png': blob})]);",
      "    })",
      "    .then(function() {",
      "      var btn = document.querySelector('#", ns("copy_plot_btn"), "');",
      "      var orig = btn.innerHTML;",
      "      btn.innerHTML = '<i class=\"fa fa-check\"></i> Copied!';",
      "      setTimeout(function() { btn.innerHTML = orig; }, 1800);",
      "    })",
      "    .catch(function(e) { alert('Clipboard write failed: ' + e.message); });",
      "});"
    ))),

    # Plot area - the one artefact this page produces. Before any plot exists
    # this was an empty 500 px card beside four enabled buttons that answered a
    # click with a JS alert, a thrown download error or a toast
    # (§BUILD_UI-redesign 2.6). Now the card is hidden and the page says what
    # to do instead.
    shiny::uiOutput(ns("plot_empty")),
    shiny::div(
      id = ns("plot_card_wrap"),
      bslib::card(
        bslib::card_body(
          waiter::waiter_on_busy(waiter::spin_flower()),
          shiny::plotOutput(ns("main_plot"), height = "500px", width = "100%")
        )
      )
    )
  )
}


#' @rdname module_explore_output
#' @export
explore_output_info_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("info_panel"))
}


#' @rdname module_explore_output
#' @export
explore_output_messages_ui <- function(id) {
  edark_messages_ui(shiny::NS(id))
}


#' @rdname module_explore_output
#' @export
explore_output_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Render the plot ───────────────────────────────────────────────────────
    # Rerenders whenever the spec OR any aesthetic setting changes.
    current_plot <- shiny::reactive({
      spec    <- shared_state$plot_specification
      dataset <- shared_state$dataset_working
      shiny::req(!is.null(spec), !is.null(dataset))

      # Read aesthetic values directly — creates reactive dependencies so that
      # palette/legend changes re-render without requiring a button re-click.
      # Do NOT use isolate() here: it can return a stale value when this reactive
      # re-runs due to an aesthetic change.
      ggplot_theme        <- shared_state$ggplot_theme
      color_palette       <- shared_state$color_palette
      show_data_labels    <- shared_state$show_data_labels
      show_legend         <- shared_state$show_legend
      legend_position     <- shared_state$legend_position
      trend_zero_baseline <- shared_state$trend_zero_baseline

      spec_with_aesthetics <- modifyList(spec, list(
        ggplot_theme        = ggplot_theme,
        color_palette       = color_palette,
        show_data_labels    = show_data_labels,
        show_legend         = show_legend,
        legend_position     = legend_position,
        trend_zero_baseline = trend_zero_baseline
      ))

      # Palette too small for this plot: render_plot() already draws it with
      # the palette that fits; ask Appearance (the palette's only writer) to
      # make that palette the session's, and say so in the messages (§E7.1).
      sw <- .palette_switch_needed(spec_with_aesthetics, dataset)
      # Stamped so it is never identical() to the last one (reactiveValues
      # ignores an identical write): re-picking the small palette for the same
      # plot must switch again. Once switched the palette fits, so no loop.
      if (!is.null(sw)) {
        sw$spec <- spec
        sw$at   <- Sys.time()
        shared_state$palette_switch <- sw
      }

      gg <- render_plot(spec_with_aesthetics, dataset)

      # Store the raw ggplot for report export
      shared_state$active_plot <- gg

      # Clear the stale flag now that we've rendered
      shared_state$explore_needs_refresh <- FALSE

      gg
    })

    output$main_plot <- shiny::renderPlot({
      gg <- current_plot()
      shiny::req(!is.null(gg))

      spec_now <- shiny::isolate(shared_state$plot_specification)

      # Suppress legend for plot types where axes/facet strips make it redundant.
      # This overrides the user's show_legend toggle for these types only.
      is_violin        <- !is.null(spec_now) && identical(spec_now$plot_type, "violin_jitter")
      is_scatter_strat <- !is.null(spec_now) &&
        identical(spec_now$plot_type, "scatter_loess") &&
        !is.null(spec_now$stratify_by)

      if (is_violin || is_scatter_strat) {
        gg <- gg + ggplot2::theme(legend.position = "none")
      }

      gg
    }, res = 96)

    # The card is hidden until a plot exists, and Shiny suspends outputs inside
    # a hidden element. Keep this one live so the first plot renders the moment
    # the card is revealed rather than one flush later.
    shiny::outputOptions(output, "main_plot", suspendWhenHidden = FALSE)


    # -- Empty state, and the toolbar gated on a plot existing ----------------
    # Disabled-with-a-reason is the app's only precondition affordance
    # (Stage 1): a button must not sit enabled and answer a click with an
    # alert or a toast.
    # Keyed on the spec, not on active_plot. active_plot is written *by* the
    # render, and a hidden plotOutput is suspended, so gating the card on
    # active_plot deadlocks: no card -> no render -> no active_plot -> no card.
    has_plot <- shiny::reactive(!is.null(shared_state$plot_specification))

    shiny::observe({
      shinyjs::toggle(id = "plot_card_wrap", condition = has_plot())
      for (b in c("save_plot_btn", "copy_plot_btn", "add_to_custom_btn")) {
        shinyjs::toggleState(b, condition = has_plot())
      }
    })

    output$plot_empty <- shiny::renderUI({
      if (has_plot()) return(NULL)
      edark_empty_state(
        "No plot yet",
        shiny::tagList(
          "Choose a mode on the left - ", shiny::tags$strong("Describe"), ", ",
          shiny::tags$strong("Correlate"), " or ", shiny::tags$strong("Trend"),
          " - pick your variables, then click that mode's button."
        ),
        icon = "chart-area"
      )
    })

    # -- Info pane: what the plot on screen is made of ------------------------
    # The variable summary, transposed to label/value rows. It used to be a
    # reactable in a card below the plot; the page contract puts facts about
    # the result in the info pane (D2), and a wide stats table does not fit
    # 300 px, so the same numbers are shown one per line instead.
    output$info_panel <- shiny::renderUI({
      spec    <- shared_state$plot_specification
      dataset <- shared_state$dataset_working
      types   <- shared_state$column_types
      items   <- shared_state$custom_report_items

      custom_block <- shiny::tagList(
        edark_section_label("Custom report"),
        edark_info_row("Items so far", length(items)),
        edark_info_row(
          "This plot",
          if (is.null(spec)) shiny::tags$em(class = "text-muted", "-")
          else if (.plot_already_added(spec, items))
            shiny::tags$span(class = "text-success", "already added")
          else shiny::tags$span(class = "text-muted", "not added")
        )
      )

      if (is.null(spec) || is.null(dataset)) {
        return(shiny::tagList(
          edark_section_label("Variable summary", first = TRUE),
          shiny::tags$p(class = "small text-muted",
                        "Run a plot to see its summary here."),
          custom_block
        ))
      }

      # For trend plots the meaningful variable is column_b (the trend
      # variable), not column_a (the datetime timestamp).
      is_trend <- !is.null(spec$plot_type) &&
                  spec$plot_type %in% c("trend_numeric", "trend_factor")
      primary  <- if (is_trend && !is.null(spec$column_b)) spec$column_b else spec$column_a
      col_type <- if (primary %in% names(types)) types[[primary]] else "datetime"

      summary_df <- tryCatch(build_variable_summary(dataset, primary, col_type),
                             error = function(e) NULL)

      shiny::tagList(
        edark_section_label("Variable summary", first = TRUE),
        if (is.null(summary_df) || nrow(summary_df) == 0) {
          shiny::tags$p(class = "small text-muted", "No summary for this plot.")
        } else {
          # One row per statistic, so it reads at 300 px.
          lapply(names(summary_df), function(nm) {
            val <- summary_df[[nm]][1]
            edark_info_row(gsub("_", " ", nm),
                           if (is.na(val)) "-" else as.character(val))
          })
        },
        custom_block
      )
    })


    # -- Messages: stale data, and anything else this page must say -----------
    edark_messages_server(output, shiny::reactive({
      stale <- if (isTRUE(shared_state$explore_needs_refresh)) edark_message(
        "stale", "The dataset has changed since this plot was drawn.",
        detail = "Re-run the plot to update it."
      )

      # Shown while the switched palette is in force on the plot it was made for
      sw <- shared_state$palette_switch
      palette <- if (!is.null(sw) &&
                     identical(sw$spec, shared_state$plot_specification) &&
                     identical(shared_state$color_palette, sw$to)) edark_message(
        "warn",
        sprintf("%s has %d colours, but %s has %d levels - switched to %s.",
                .palette_label(sw$from), .EDARK_PALETTES[[sw$from]]$max,
                sw$column, sw$n, .palette_label(sw$to)),
        detail = "It stays the palette for the rest of the session; change it in Appearance."
      )

      list(stale, palette)
    }))


    # ── Save Plot download ────────────────────────────────────────────────────
    output$save_plot_btn <- shiny::downloadHandler(
      filename = function() {
        spec <- shiny::isolate(shared_state$plot_specification)
        stem <- if (!is.null(spec$column_a)) spec$column_a else "plot"
        if (!is.null(spec$column_b)) stem <- paste0(stem, "_x_", spec$column_b)
        paste0("edark_", stem, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
      },
      content = function(file) {
        gg <- shiny::isolate(shared_state$active_plot)
        if (is.null(gg)) stop("No plot to save. Run a plot first.")
        ggplot2::ggsave(file, plot = gg, width = 16, height = 9,
                        units = "in", dpi = 150)
      }
    )

    # ── Custom report buttons ─────────────────────────────────────────────────

    # "View Report" button — rendered dynamically so its badge stays live
    output$view_report_btn_ui <- shiny::renderUI({
      n     <- length(shared_state$custom_report_items)
      label <- if (n > 0)
        shiny::tagList("View Report", edark_badge(n, role = "count", class = "ms-1"))
      else "View Report"
      edark_button(ns, "view_report_btn", label, icon = "file-export",
                   size = "toolbar")
    })

    # Add current plot to the custom report
    shiny::observeEvent(input$add_to_custom_btn, {
      spec <- shiny::isolate(shared_state$plot_specification)
      gg   <- shiny::isolate(shared_state$active_plot)

      if (is.null(spec) || is.null(gg)) {
        shiny::showNotification("No plot to add. Run a plot first.", type = "warning")
        return()
      }

      # Save thumbnail PNG (active_plot is always the patchwork, safe for ggsave)
      thumb_path <- tempfile(pattern = "edark_thumb_", fileext = ".png")
      ggplot2::ggsave(thumb_path, plot = gg, width = 4, height = 3,
                      units = "in", dpi = 96)

      # Build human-readable title from spec
      title <- spec$column_a
      if (!is.null(spec$column_b))    title <- paste0(title, " \u00d7 ", spec$column_b)
      if (!is.null(spec$stratify_by) && nzchar(spec$stratify_by))
        title <- paste0(title, " \u00b7 by ", spec$stratify_by)

      # Freeze the appearance in force into the item's own spec. The spec in
      # shared_state may predate aesthetic changes made after the plot was
      # drawn, so without this the exported item can differ from the thumbnail
      # beside it and from what was on screen when it was added (D7).
      spec <- utils::modifyList(spec, edark_current_aesthetics(shared_state))

      new_item <- list(
        id         = paste0("item_", as.numeric(Sys.time()), "_", sample.int(1e6, 1)),
        plot_spec  = spec,
        thumb_path = thumb_path,
        title      = title,
        added_at   = Sys.time()
      )

      shared_state$custom_report_items <- c(
        shiny::isolate(shared_state$custom_report_items),
        list(new_item)
      )

      n <- length(shared_state$custom_report_items)
      shiny::showNotification(
        paste0("\u2713 Added (", n, " item", if (n != 1) "s" else "", " in custom report)"),
        type = "message", duration = 3
      )
    })

    # Navigate to Report tab → Custom Report pill
    shiny::observeEvent(input$view_report_btn, {
      shared_state$requested_report_subtab <- "custom_report"
      if (!identical(shared_state$requested_tab, "report"))
        shared_state$requested_tab <- "report"
    })
  })
}


# Is the plot currently on screen already in the custom report?
#
# Compares what identifies a plot to the reader - the variables, the plot type
# and the stratification - and deliberately ignores appearance, so restyling a
# plot does not make it look like a different one (§BUILD_UI-redesign 1.3g).
.plot_already_added <- function(spec, items) {
  if (is.null(spec) || length(items) == 0) return(FALSE)
  key <- function(s) paste(s$plot_type %||% "", s$column_a %||% "",
                           s$column_b %||% "", s$stratify_by %||% "", sep = "\r")
  this <- key(spec)
  any(vapply(items, function(it) identical(key(it$plot_spec), this), logical(1)))
}
