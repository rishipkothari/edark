#' Report Module
#'
#' UI and server for the Report stage. Lets the user configure and download a
#' report from the current working dataset. Provides two top-level pill tabs:
#' \itemize{
#'   \item \strong{Full Report} — auto-generated report (Describe Variables or
#'     Correlation mode).
#'   \item \strong{Custom Report} — user-curated collection of plots added from
#'     the Explore tab.
#' }
#' Output formats: PowerPoint (\code{.pptx}), Word (\code{.docx}),
#' or HTML (\code{.html}).
#'
#' @param id Character. Module namespace ID.
#' @param shared_state A Shiny \code{reactiveValues} object (shared across modules).
#'
#' @name module_report
NULL


# Helper: encode a thumbnail PNG as a base64 data URI for inline <img> display.
.thumb_src <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return("")
  paste0("data:image/png;base64,", base64enc::base64encode(path))
}


# How a plot_type string reads in a list row or an info row. Keep in step with
# the switch() in render_plot.R: an unmapped type still renders, de-underscored,
# rather than blank, so a new plot type is legible before it is named here.
.PLOT_TYPE_LABELS <- c(
  bar_count         = "Bar",
  histogram_density = "Histogram",
  bar_grouped       = "Grouped bar",
  violin_jitter     = "Violin",
  scatter_loess     = "Scatter",
  trend_mean        = "Trend (mean)",
  trend_numeric     = "Trend (numeric)",
  trend_factor      = "Trend (proportion)"
)

.plot_type_label <- function(plot_type) {
  if (is.null(plot_type) || !nzchar(plot_type)) return("Plot")
  lbl <- unname(.PLOT_TYPE_LABELS[plot_type])
  if (is.na(lbl)) gsub("_", " ", plot_type) else lbl
}


# Helper: the ids of a list of custom-report items, in order.
.item_ids <- function(items) {
  if (length(items) == 0) return(character(0))
  vapply(items, `[[`, character(1), "id")
}


# Helper: one row of the Custom Report item list.
#
# The description is itself an actionButton, so clicking anywhere along the row
# selects it with no client-side JS. The delete button is a *sibling* of that
# button, not nested inside it: a button within a button is invalid HTML and
# browsers drop the inner one.
.custom_item_row <- function(ns, item, i, selected = FALSE) {
  shiny::div(
    class = paste("edark-report-item d-flex align-items-stretch mb-1",
                  if (isTRUE(selected)) "selected" else ""),
    shiny::actionButton(
      ns(paste0("sel_", item$id)),
      label = shiny::div(
        class = "d-flex align-items-baseline gap-2 w-100",
        shiny::tags$span(class = "edark-report-item-index text-muted small", i),
        shiny::tags$span(class = "flex-grow-1 text-truncate", item$title),
        edark_badge(.plot_type_label(item$plot_spec$plot_type),
                    role = "neutral", class = "fw-normal")
      ),
      class = paste("edark-report-item-select btn flex-grow-1",
                    "d-flex align-items-center text-start")
    ),
    edark_button(ns, paste0("remove_", item$id), NULL, icon = "trash",
                 variant = "danger", size = "toolbar",
                 class = "edark-report-item-delete",
                 title = "Remove this item")
  )
}


# Helper: the blocking progress modal both pills show while a report builds
# (§M3.5). Its bar and detail line are driven by the edark_report_progress
# handler registered in report_ui().
.report_progress_modal <- function(title) {
  shiny::modalDialog(
    title = shiny::tagList(
      shiny::tags$span(
        class = "spinner-border spinner-border-sm me-2",
        role  = "status",
        shiny::tags$span(class = "visually-hidden", "Loading...")
      ),
      title
    ),
    shiny::div(
      class = "progress mb-2",
      style = "height: 6px;",
      shiny::div(
        id              = "edark_progress_bar",
        class           = "progress-bar progress-bar-striped progress-bar-animated",
        role            = "progressbar",
        style           = "width: 0%;",
        `aria-valuenow` = "0",
        `aria-valuemin` = "0",
        `aria-valuemax` = "100"
      )
    ),
    shiny::tags$p(
      id    = "edark_progress_detail",
      class = "text-muted mb-0 small",
      "Starting..."
    ),
    footer    = NULL,
    easyClose = FALSE
  )
}


#' @rdname module_report
#' @export
report_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    # JS handler — updates the detail text inside the blocking progress modal
    shiny::tags$script(shiny::HTML(
      "Shiny.addCustomMessageHandler('edark_report_progress', function(msg) {",
      "  var bar = document.getElementById('edark_progress_bar');",
      "  if (bar) {",
      "    bar.style.width = (msg.frac * 100) + '%';",
      "    bar.setAttribute('aria-valuenow', Math.round(msg.frac * 100));",
      "  }",
      "  var txt = document.getElementById('edark_progress_detail');",
      "  if (txt) txt.textContent = msg.detail;",
      "});"
    )),

  bslib::navset_underline(
    id = ns("report_mode_tabs"),

    # ── Full Report pill ──────────────────────────────────────────────────────
    bslib::nav_panel(
      value = "full_report",
      title = "Full Report",

      edark_page(
        # Config: settings first, then output format, then the one primary
        # action last - in every mode of every page (F3). Generate used to sit
        # at the *top* here and at the bottom in Custom (§1.3k).
        config = shiny::tagList(
          edark_section_label("Report Type", first = TRUE),
          shinyWidgets::radioGroupButtons(
            ns("report_type"),
            label    = NULL,
            choices  = c("Describe Variables" = "all_vars",
                         "Correlation"        = "primary_vs_others"),
            selected = "all_vars",
            size     = "sm",
            width    = "100%"
          ),

          # ── Primary variable (Correlation only) ────────────────────────────
          shiny::conditionalPanel(
            condition = paste0("input['", ns("report_type"), "'] == 'primary_vs_others'"),
            edark_section_label("Primary Variable"),
            shiny::uiOutput(ns("primary_var_picker")),
            shinyWidgets::radioGroupButtons(
              ns("primary_role"),
              label    = "Role:",
              choices  = c("Exposure (X)" = "exposure",
                           "Outcome (Y)"  = "outcome"),
              selected = "exposure",
              size     = "sm",
              width    = "100%"
            )
          ),

          edark_section_label("Variables"),
          shiny::uiOutput(ns("var_selection_summary")),
          edark_button(ns, "open_var_modal", "Select Variables\u2026",
                       icon = "sliders", variant = "secondary", outline = TRUE,
                       class = "mt-1"),

          edark_section_label("Options"),
          shiny::uiOutput(ns("stratify_picker")),

          edark_section_label("Report Contents"),
          shiny::checkboxInput(ns("include_dataset_summary"),
                               "Dataset Summary", value = TRUE),
          shiny::checkboxInput(ns("include_tableone"),
                               "Table One", value = FALSE),
          shiny::checkboxInput(ns("include_collinearity"),
                               "Collinearity", value = FALSE),

          edark_section_label("Output Format"),
          shinyWidgets::radioGroupButtons(
            ns("output_format"),
            label    = NULL,
            choices  = c("In App"     = "inapp",
                         "PowerPoint" = "pptx",
                         "Word"       = "docx",
                         "HTML"       = "html"),
            selected = "pptx",
            size     = "sm",
            width    = "100%"
          ),

          # One primary action either way (F3): In App builds the HTML report
          # into the centre pane, every other format downloads a file.
          shiny::div(
            class = "mt-3",
            shiny::conditionalPanel(
              condition = paste0("input['", ns("output_format"), "'] == 'inapp'"),
              edark_button(ns, "preview_btn", "Generate Preview", icon = "eye")
            ),
            shiny::conditionalPanel(
              condition = paste0("input['", ns("output_format"), "'] != 'inapp'"),
              edark_button(ns, "download_btn", "Generate & Download",
                           icon = "download", type = "download")
            )
          )
        ),

        # Result, in two tabs. Sections: what the current settings resolve to,
        # in order - read-only, and not a restatement of the controls (F5),
        # since which variables survive depends on eligibility, the primary
        # variable and the stratify variable. Preview: the In App report
        # (§E11.4). Generating a preview switches to its tab; closing it
        # switches back.
        result   = bslib::navset_card_tab(
          id = ns("full_result_tabs"),
          bslib::nav_panel(
            value = "sections",
            title = "Sections",
            shiny::uiOutput(ns("report_sections_panel"))
          ),
          bslib::nav_panel(
            value = "preview",
            title = "Preview",
            shiny::uiOutput(ns("full_preview_panel"))
          )
        ),
        messages = edark_messages_ui(ns, "full_messages"),
        info     = shiny::uiOutput(ns("full_info"))
      )
    ),

    # ── Custom Report pill ────────────────────────────────────────────────────
    bslib::nav_panel(
      value = "custom_report",
      title = "Custom Report",

      # Three panes like every other page. The item list is the artefact this
      # page produces, so it belongs in the centre with its actions in a
      # toolbar above it (F2); the config pane keeps the format and the one
      # primary action; the info pane describes the selected item and the file.
      # This page used to pass `result = NULL` under a D11 exception, with the
      # list in the config pane - that exception is gone.
      edark_page(
        config = shiny::tagList(
          edark_section_label("Output Format", first = TRUE),
          shinyWidgets::radioGroupButtons(
            ns("custom_output_format"),
            label    = NULL,
            choices  = c("In App"     = "inapp",
                         "PowerPoint" = "pptx",
                         "Word"       = "docx",
                         "HTML"       = "html"),
            selected = "pptx",
            size     = "sm",
            width    = "100%"
          ),

          # As in Full Report: In App previews, every other format downloads.
          shiny::div(
            class = "mt-3",
            shiny::conditionalPanel(
              condition = paste0("input['", ns("custom_output_format"), "'] == 'inapp'"),
              edark_button(ns, "custom_preview_btn", "Generate Preview", icon = "eye")
            ),
            shiny::conditionalPanel(
              condition = paste0("input['", ns("custom_output_format"), "'] != 'inapp'"),
              edark_button(ns, "custom_download_btn", "Generate & Download",
                           icon = "download", type = "download")
            )
          )
        ),
        # Two artefacts, so two tabs: the item list the user curates, and the
        # in-app preview of the report built from it (§E12). Generating a
        # preview switches to its tab; closing it switches back.
        result   = bslib::navset_card_tab(
          id = ns("custom_result_tabs"),
          bslib::nav_panel(
            value = "items",
            title = "Items",
            shiny::uiOutput(ns("custom_items_panel"))
          ),
          bslib::nav_panel(
            value = "preview",
            title = "Preview",
            shiny::uiOutput(ns("custom_preview_panel"))
          )
        ),
        messages = edark_messages_ui(ns, "custom_messages"),
        info     = shiny::uiOutput(ns("custom_info"))
      )
    )
  )   # closes navset_underline
  )   # closes tagList
}



#' @rdname module_report
#' @export
report_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Navigate to a report sub-tab when requested externally (e.g. "View Report")
    shiny::observeEvent(shared_state$requested_report_subtab, {
      shiny::req(!is.null(shared_state$requested_report_subtab))
      bslib::nav_select("report_mode_tabs", shared_state$requested_report_subtab, session = session)
      shared_state$requested_report_subtab <- NULL
    })

    # Track which variables the user has selected (NULL = all eligible)
    selected_vars <- shiny::reactiveVal(NULL)

    # Helper: eligible columns (numeric + factor only, no datetime/character)
    eligible_vars <- shiny::reactive({
      types <- shared_state$column_types
      names(types)[types %in% c("numeric", "factor")]
    })

    # Initialise selected_vars when the working dataset is first available
    shiny::observe({
      req(shared_state$dataset_working)
      if (is.null(selected_vars()))
        selected_vars(eligible_vars())
    })

    # When the dataset changes (after Apply), reset selection to new eligible set
    shiny::observeEvent(shared_state$dataset_working, {
      selected_vars(eligible_vars())
    }, ignoreInit = TRUE)

    # ── Dynamic pickers ───────────────────────────────────────────────────────

    output$primary_var_picker <- shiny::renderUI({
      types    <- shared_state$column_types
      eligible <- names(types)[types %in% c("numeric", "factor")]
      shinyWidgets::pickerInput(
        ns("primary_variable"),
        # label   = "Primary variable:",
        choices = eligible,
        options = shinyWidgets::pickerOptions(liveSearch = TRUE, container = "body")
      )
    })

    output$stratify_picker <- shiny::renderUI({
      types       <- shared_state$column_types
      factor_cols <- names(types)[types == "factor"]
      # In correlation mode, exclude the primary variable
      if (isTRUE(input$report_type == "primary_vs_others"))
        factor_cols <- setdiff(factor_cols, input$primary_variable)
      shinyWidgets::pickerInput(
        ns("stratify_variable"),
        label    = "Stratify by:",
        choices  = c("None" = "", factor_cols),
        selected = "",
        options  = shinyWidgets::pickerOptions(container = "body")
      )
    })

    # ── Variable selection summary ────────────────────────────────────────────

    output$var_selection_summary <- shiny::renderUI({
      elig  <- eligible_vars()
      sel   <- selected_vars()
      if (is.null(sel)) sel <- elig
      # In correlation mode, exclude primary and stratify variables from the count
      if (input$report_type == "primary_vs_others" && !is.null(input$primary_variable)) {
        sel  <- setdiff(sel, input$primary_variable)
        elig <- setdiff(elig, input$primary_variable)
        sv <- input$stratify_variable
        if (!is.null(sv) && nzchar(sv)) {
          sel  <- setdiff(sel, sv)
          elig <- setdiff(elig, sv)
        }
      }
      n_sel   <- length(intersect(sel, elig))
      n_total <- length(elig)
      shiny::tags$p(
        class = "text-muted small mb-1",
        paste0(n_sel, " of ", n_total, " variables selected")
      )
    })

    # ── Variable selection modal ──────────────────────────────────────────────

    shiny::observeEvent(input$open_var_modal, {
      elig      <- eligible_vars()
      currently <- selected_vars()
      if (is.null(currently)) currently <- elig

      # In correlation mode, exclude primary and stratify variables from the selector.
      if (input$report_type == "primary_vs_others" && !is.null(input$primary_variable)) {
        elig      <- setdiff(elig, input$primary_variable)
        currently <- setdiff(currently, input$primary_variable)
        sv <- input$stratify_variable
        if (!is.null(sv) && nzchar(sv)) {
          elig      <- setdiff(elig, sv)
          currently <- setdiff(currently, sv)
        }
      }

      # Actions at the *top*, above a list of unknown length, so they stay
      # reachable without scrolling however many variables the dataset has
      # (F6 / §1.3j). The list scrolls under them; the footer is empty.
      shiny::showModal(shiny::modalDialog(
        title = "Select Variables for Report",
        shiny::div(
          class = "d-flex align-items-center gap-2 mb-3 pb-2 border-bottom",
          edark_button(ns, "modal_select_all", "Select All",
                       variant = "secondary", size = "dialog", outline = TRUE),
          edark_button(ns, "modal_deselect_all", "Clear",
                       variant = "secondary", size = "dialog", outline = TRUE),
          shiny::tags$span(class = "flex-grow-1"),
          shiny::modalButton("Cancel"),
          edark_button(ns, "modal_done", "Done", size = "dialog")
        ),
        shiny::div(
          style = "max-height: 45vh; overflow-y: auto;",
          shiny::checkboxGroupInput(
            ns("modal_vars"),
            label    = NULL,
            choices  = elig,
            selected = currently
          )
        ),
        footer    = NULL,
        easyClose = FALSE,
        size      = "m"
      ))
    })

    shiny::observeEvent(input$modal_select_all, {
      elig <- eligible_vars()
      if (input$report_type == "primary_vs_others" && !is.null(input$primary_variable)) {
        elig <- setdiff(elig, input$primary_variable)
        sv <- input$stratify_variable
        if (!is.null(sv) && nzchar(sv)) elig <- setdiff(elig, sv)
      }
      shiny::updateCheckboxGroupInput(session, "modal_vars", selected = elig)
    })

    shiny::observeEvent(input$modal_deselect_all, {
      shiny::updateCheckboxGroupInput(session, "modal_vars", selected = character(0))
    })

    shiny::observeEvent(input$modal_done, {
      selected_vars(input$modal_vars)
      shiny::removeModal()
    })

    # -- Full report: what the current settings resolve to ---------------------
    # Which variables become sections is derived, not stated: eligibility,
    # the primary variable and the stratify variable all remove candidates.
    full_sections <- shiny::reactive({
      elig <- eligible_vars()
      sel  <- selected_vars()
      if (is.null(sel)) sel <- elig
      sv <- input$stratify_variable

      v <- intersect(sel, elig)
      if (identical(input$report_type, "primary_vs_others") &&
          !is.null(input$primary_variable)) {
        v <- setdiff(v, input$primary_variable)
      }
      if (!is.null(sv) && nzchar(sv)) v <- setdiff(v, sv)
      v
    })


    # Info pane: the scalar facts about the file that will be produced.
    output$full_info <- shiny::renderUI({
      ds   <- shared_state$dataset_working
      elig <- eligible_vars()
      secs <- full_sections()

      type_label <- switch(input$report_type,
        all_vars          = "Describe Variables",
        primary_vs_others = "Correlation",
        "-"
      )
      format_label <- switch(input$output_format %||% "pptx",
        inapp = "In app (HTML preview)",
        pptx = "PowerPoint (.pptx)",
        docx = "Word (.docx)",
        html = "HTML (.html)",
        "-"
      )
      sv <- input$stratify_variable

      shiny::tagList(
        edark_section_label("Will contain", first = TRUE),
        edark_info_row("Report type",   type_label),
        edark_info_row("Output format", format_label),
        edark_info_row("Sections",      length(secs)),
        edark_info_row("Variables",
                       sprintf("%d of %d eligible", length(secs), length(elig))),
        if (identical(input$report_type, "primary_vs_others")) {
          edark_info_row(
            "Primary variable",
            paste0(input$primary_variable %||% "-", " - ",
                   if ((input$primary_role %||% "exposure") == "exposure")
                     "Exposure (X)" else "Outcome (Y)")
          )
        },
        edark_info_row("Stratify by",
                       if (is.null(sv) || !nzchar(sv)) "None" else sv),
        edark_info_row("Dataset summary",
                       if (isTRUE(input$include_dataset_summary)) "Included" else "No"),
        edark_info_row("Table One",
                       if (isTRUE(input$include_tableone)) "Included" else "No"),
        edark_info_row("Collinearity",
                       if (isTRUE(input$include_collinearity)) "Included" else "No"),

        edark_section_label("Source data"),
        edark_info_row("Rows",    if (!is.null(ds)) format(nrow(ds), big.mark = ",") else "-"),
        edark_info_row("Columns", if (!is.null(ds)) ncol(ds) else "-")
      )
    })


    # Result, Sections tab: the ordered section list itself.
    output$report_sections_panel <- shiny::renderUI({
      secs <- full_sections()

      if (length(secs) == 0) {
        return(edark_empty_state(
          "No sections to generate",
          "Pick at least one variable under Variables, in the pane on the left.",
          icon = "file-circle-question"
        ))
      }

      # One item per section, so the list scrolls itself rather than the page.
      # No card of its own: the Sections card tab is already the frame.
      shiny::div(
        class = "edark-scroll-table",
        shiny::tags$ol(
          class = "mb-0",
          lapply(secs, function(v) shiny::tags$li(v))
        )
      )
    })


    # Messages for the Full Report page.
    edark_messages_server(output, shiny::reactive({
      msgs <- list()
      if (length(full_sections()) == 0) {
        msgs <- c(msgs, list(edark_message(
          "warn", "This report would have no sections.",
          detail = "Select at least one variable before generating."
        )))
      }
      if (isTRUE(shared_state$explore_needs_refresh)) {
        msgs <- c(msgs, list(edark_message(
          "stale", "The working dataset has changed since the last plot was drawn.",
          detail = "The report is generated from the current working dataset."
        )))
      }
      if (.preview_is_stale(full_preview(), full_report_args())) {
        msgs <- c(msgs, list(.preview_stale_message()))
      }
      msgs
    }), id = "full_messages")

    # ── In-app preview (both pills) ───────────────────────────────────────────
    # In App renders the HTML report into a per-session temp folder served under
    # its own resource path, and the centre pane shows it in an iframe (§N5.8).
    # Nothing is downloaded until the user clicks Save HTML, which copies the
    # file already built. Each pill keeps its own reactiveVal holding
    # list(path, url, args, at), or NULL when there is no preview.
    preview_prefix <- paste0("edark-preview-", session$token)
    preview_dir    <- file.path(tempdir(), preview_prefix)
    preview_count  <- 0L

    session$onSessionEnded(function() {
      if (preview_prefix %in% names(shiny::resourcePaths()))
        shiny::removeResourcePath(preview_prefix)
      unlink(preview_dir, recursive = TRUE)
    })

    .preview_discard <- function(rv) {
      pv <- shiny::isolate(rv())
      if (!is.null(pv)) unlink(pv$path)
      rv(NULL)
    }

    # Build a preview with run(args, path), which returns TRUE on success. The
    # previous preview is only discarded once the new one has been built.
    .preview_build <- function(rv, stem, args, run) {
      dir.create(preview_dir, showWarnings = FALSE, recursive = TRUE)
      if (!preview_prefix %in% names(shiny::resourcePaths()))
        shiny::addResourcePath(preview_prefix, preview_dir)

      # A new file name each time, so the iframe never shows a cached copy.
      preview_count <<- preview_count + 1L
      fname <- sprintf("%s_%03d.html", stem, preview_count)
      path  <- file.path(preview_dir, fname)

      if (!run(args, path)) return(FALSE)
      .preview_discard(rv)
      rv(list(
        path = path,
        url  = paste0(preview_prefix, "/", fname),
        args = args,
        at   = Sys.time()
      ))
      TRUE
    }

    # The preview is stale when what it was built from no longer matches what
    # would be built now. `args` holds the dataset itself, and identical()
    # short-circuits on the same object, so this is cheap.
    .preview_is_stale <- function(pv, args_now) {
      !is.null(pv) && !identical(pv$args, args_now)
    }

    .preview_stale_message <- function() {
      edark_message(
        "stale", "The report contents or data have changed since this preview was generated.",
        detail = "Generate Preview again to update it. Save HTML saves the preview as shown."
      )
    }

    .preview_empty_state <- function() {
      edark_empty_state(
        "No preview yet",
        shiny::tagList(
          "Choose ", shiny::tags$strong("In App"), " under Output Format and click ",
          shiny::tags$strong("Generate Preview"), " to see the report here."
        ),
        icon = "eye"
      )
    }

    .preview_view <- function(pv, save_id, close_id) {
      shiny::tagList(
        edark_action_toolbar(
          shiny::tags$span(
            class = "text-muted small me-auto",
            paste0("Preview generated ", format(pv$at, "%H:%M:%S"))
          ),
          edark_button(ns, save_id, "Save HTML", icon = "download",
                       type = "download", variant = "secondary",
                       size = "toolbar"),
          edark_button(ns, close_id, "Close Preview", icon = "xmark",
                       variant = "secondary", size = "toolbar")
        ),
        # An iframe, not includeHTML(): the report carries its own Bootstrap
        # theme and scripts, which would restyle the app if inlined (§N5.8).
        shiny::tags$iframe(
          src   = pv$url,
          class = "edark-report-preview",
          title = "Report preview"
        )
      )
    }

    .preview_save_handler <- function(rv, file_stem) {
      shiny::downloadHandler(
        filename = function() {
          paste0(file_stem, format(shiny::isolate(rv())$at, "%Y%m%d_%H%M%S"), ".html")
        },
        content = function(file) {
          pv <- shiny::isolate(rv())
          if (is.null(pv) || !file.exists(pv$path)) stop("No preview to save.")
          file.copy(pv$path, file, overwrite = TRUE)
        }
      )
    }


    # ── Full report: generation ───────────────────────────────────────────────
    # Everything generate_report() needs apart from the format, the output path
    # and the progress callback. The download and the in-app preview both call
    # it, and the preview keeps the list it was built from so the stale check
    # is a plain identical() against the current settings.
    full_report_args <- shiny::reactive({
      vars <- selected_vars()
      if (is.null(vars) || length(vars) == 0) vars <- eligible_vars()
      sv <- input$stratify_variable

      aes_now <- edark_current_aesthetics(shared_state)

      list(
        dataset                 = shared_state$dataset_working,
        column_types            = shared_state$column_types,
        report_type             = input$report_type,
        variables               = vars,
        primary_variable        = if (identical(input$report_type, "primary_vs_others"))
                                     input$primary_variable else NULL,
        primary_role            = input$primary_role %||% "exposure",
        stratify_variable       = if (is.null(sv) || !nzchar(sv)) NULL else sv,
        include_dataset_summary = isTRUE(input$include_dataset_summary),
        include_tableone        = isTRUE(input$include_tableone),
        include_collinearity    = isTRUE(input$include_collinearity),
        # The aesthetics on screen, not a second set owned by Report, so the
        # generated document matches the plot the user has been looking at
        # (D7 / D9).
        ggplot_theme            = aes_now$ggplot_theme,
        color_palette           = aes_now$color_palette,
        show_data_labels        = aes_now$show_data_labels,
        show_legend             = aes_now$show_legend,
        legend_position         = aes_now$legend_position
      )
    })

    # Build the full report into `path` behind the progress modal. Returns
    # TRUE on success; on failure shows the error and returns FALSE.
    .run_full_report <- function(args, format, path) {
      n_sections <- if (identical(args$report_type, "primary_vs_others")) {
        length(setdiff(args$variables, args$primary_variable))
      } else {
        length(args$variables)
      }

      shiny::showModal(.report_progress_modal(
        paste0("Generating Report (", n_sections, " section",
               if (n_sections != 1) "s" else "", ")")
      ))
      on.exit(shiny::removeModal(), add = TRUE)

      tryCatch({
        do.call(generate_report, c(args, list(
          format      = format,
          output_path = path,
          progress_fn = function(frac, detail) {
            session$sendCustomMessage("edark_report_progress",
                                      list(frac = frac, detail = detail))
          }
        )))
        TRUE
      }, error = function(e) {
        shiny::showNotification(
          paste("Report generation failed:", conditionMessage(e)),
          type     = "error",
          duration = 10
        )
        FALSE
      })
    }

    output$download_btn <- shiny::downloadHandler(
      filename = function() {
        ext <- switch(input$output_format,
                      pptx = ".pptx", docx = ".docx", ".html")
        paste0("edark_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ext)
      },
      content = function(file) {
        fmt <- if (input$output_format %in% c("pptx", "docx")) input$output_format
               else "html"
        if (!.run_full_report(full_report_args(), fmt, file))
          stop("Report generation failed.")
      }
    )


    # ── Full report: in-app preview (Preview tab) ─────────────────────────────

    full_preview <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$preview_btn, {
      args <- full_report_args()
      if (length(args$variables) == 0) return()
      built <- .preview_build(full_preview, "full", args,
                              function(a, path) .run_full_report(a, "html", path))
      if (built) bslib::nav_select("full_result_tabs", "preview", session = session)
    })

    shiny::observeEvent(input$preview_close, {
      .preview_discard(full_preview)
      bslib::nav_select("full_result_tabs", "sections", session = session)
    })

    output$full_preview_panel <- shiny::renderUI({
      pv <- full_preview()
      if (is.null(pv)) return(.preview_empty_state())
      .preview_view(pv, "preview_save", "preview_close")
    })

    output$preview_save <- .preview_save_handler(full_preview, "edark_report_")


    # ── Custom Report: selection ──────────────────────────────────────────────
    # Which row the user has clicked. The toolbar above the list acts on it and
    # the info pane describes it, so one selection replaces the per-row control
    # stacks the list used to carry.
    selected_item_id <- shiny::reactiveVal(NULL)

    selected_index <- shiny::reactive({
      items <- shared_state$custom_report_items
      id    <- selected_item_id()
      if (is.null(id) || length(items) == 0) return(NA_integer_)
      idx <- which(.item_ids(items) == id)
      if (length(idx) == 1) idx else NA_integer_
    })

    selected_item <- shiny::reactive({
      idx <- selected_index()
      if (is.na(idx)) NULL else shared_state$custom_report_items[[idx]]
    })

    # Removing a row leaves the selection on its neighbour rather than emptying
    # the info pane.
    .remove_item <- function(iid) {
      curr <- shared_state$custom_report_items
      idx  <- which(.item_ids(curr) == iid)
      if (length(idx) != 1) return(invisible(NULL))

      remaining <- curr[-idx]
      shared_state$custom_report_items <- remaining

      if (identical(shiny::isolate(selected_item_id()), iid)) {
        selected_item_id(
          if (length(remaining) == 0) NULL
          else .item_ids(remaining)[min(idx, length(remaining))]
        )
      }
      invisible(NULL)
    }

    .move_selected <- function(delta) {
      curr   <- shared_state$custom_report_items
      idx    <- shiny::isolate(selected_index())
      if (is.na(idx)) return(invisible(NULL))
      target <- idx + delta
      if (target < 1 || target > length(curr)) return(invisible(NULL))

      curr[c(idx, target)] <- curr[c(target, idx)]
      shared_state$custom_report_items <- curr
      invisible(NULL)
    }

    # ── Custom Report: the item list (result pane) ────────────────────────────

    output$custom_items_panel <- shiny::renderUI({
      items <- shared_state$custom_report_items
      n     <- length(items)

      if (n == 0) {
        return(edark_empty_state(
          "No items in this report yet",
          shiny::tagList(
            "Run a plot in ", shiny::tags$strong("Explore"), " and click ",
            shiny::tags$strong("Add to Custom Report"), " to queue it here."
          ),
          icon = "layer-group"
        ))
      }

      idx     <- selected_index()
      has_sel <- !is.na(idx)

      shiny::tagList(
        # Actions *on* the list, directly above it (F2). They act on the
        # selected row, so the toolbar stays one row wide however many items
        # there are - the per-row up/down/delete stack it replaces was three
        # buttons tall and left little room for the description.
        edark_action_toolbar(
          shiny::tags$span(
            class = "text-muted small me-auto",
            if (has_sel) paste0("Item ", idx, " of ", n, " selected")
            else "Select an item to reorder or remove it"
          ),
          # `disabled` is a formal of shiny::actionButton() taking a logical,
          # not a raw HTML attribute - `disabled = NA` is accepted and silently
          # ignored, which leaves the button live with nothing selected.
          edark_button(ns, "item_up", "Move Up", icon = "angle-up",
                       variant = "secondary", size = "toolbar",
                       disabled = !(has_sel && idx > 1)),
          edark_button(ns, "item_down", "Move Down", icon = "angle-down",
                       variant = "secondary", size = "toolbar",
                       disabled = !(has_sel && idx < n)),
          edark_button(ns, "item_remove", "Remove", icon = "trash",
                       variant = "danger", size = "toolbar",
                       disabled = !has_sel),
          edark_button(ns, "items_clear", "Clear All", icon = "xmark",
                       variant = "danger", size = "toolbar")
        ),

        # No card of its own: the Items card tab is already the frame.
        shiny::div(
          class = "edark-scroll-table",
          lapply(seq_along(items), function(i) {
            .custom_item_row(ns, items[[i]], i,
                             selected = identical(i, idx))
          })
        )
      )
    })

    # Dynamic observer registration for the per-row select and delete buttons.
    # Uses the same lazy-registration + local() closure pattern as
    # module_row_filter.R to avoid double-registration and R closure capture
    # issues. Reordering is no longer per row, so there is nothing to register
    # for it.
    registered_item_ids <- shiny::reactiveVal(character(0))

    shiny::observe({
      items   <- shared_state$custom_report_items
      ids     <- .item_ids(items)
      new_ids <- setdiff(ids, registered_item_ids())
      if (!length(new_ids)) return()

      for (item_id in new_ids) {
        local({
          iid <- item_id

          shiny::observeEvent(input[[paste0("sel_", iid)]], {
            selected_item_id(iid)
          }, ignoreInit = TRUE)

          shiny::observeEvent(input[[paste0("remove_", iid)]], {
            .remove_item(iid)
          }, ignoreInit = TRUE)
        })
      }

      registered_item_ids(c(registered_item_ids(), new_ids))
    })

    # ── Custom Report: toolbar actions ────────────────────────────────────────

    shiny::observeEvent(input$item_up,   .move_selected(-1L))
    shiny::observeEvent(input$item_down, .move_selected(1L))

    shiny::observeEvent(input$item_remove, {
      iid <- shiny::isolate(selected_item_id())
      if (!is.null(iid)) .remove_item(iid)
    })

    # Clear All discards every item with no undo, so it confirms first. The
    # per-row and toolbar Remove buttons take one item and do not.
    shiny::observeEvent(input$items_clear, {
      n <- length(shared_state$custom_report_items)
      if (n == 0) return()

      shiny::showModal(shiny::modalDialog(
        title = "Clear all report items?",
        shiny::tags$p(paste0(
          "This removes all ", n, " item", if (n != 1) "s" else "",
          " from the custom report. It cannot be undone."
        )),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          edark_button(ns, "items_clear_confirm", "Clear All",
                       icon = "trash", variant = "danger", size = "dialog")
        ),
        easyClose = TRUE,
        size      = "m"
      ))
    })

    shiny::observeEvent(input$items_clear_confirm, {
      shared_state$custom_report_items <- list()
      selected_item_id(NULL)
      shiny::removeModal()
    })

    # -- Custom Report: info pane ---------------------------------------------
    # The selected row's preview and provenance, then the scalar facts about
    # the file. Factual only (D2): the list and its actions are in the centre,
    # the output format and Generate in the config pane.
    output$custom_info <- shiny::renderUI({
      items <- shared_state$custom_report_items
      n     <- length(items)
      item  <- selected_item()
      idx   <- selected_index()
      ds    <- shared_state$dataset_working

      format_label <- switch(input$custom_output_format %||% "pptx",
        inapp = "In app (HTML preview)",
        pptx = "PowerPoint (.pptx)",
        docx = "Word (.docx)",
        html = "HTML (.html)",
        "-"
      )

      selected_block <- if (is.null(item)) {
        shiny::tags$p(
          class = "small text-muted",
          if (n == 0) "Nothing queued yet."
          else "Click an item in the list to preview it here."
        )
      } else {
        spec <- item$plot_spec
        shiny::tagList(
          # The thumbnail the list used to carry at 80x60 px, where it was too
          # small to recognise a plot by. One at a time, it gets the full pane.
          shiny::tags$img(
            src   = .thumb_src(item$thumb_path),
            class = "edark-report-thumb mb-2",
            alt   = paste("Preview of", item$title)
          ),
          edark_info_row("Position",  paste0(idx, " of ", n)),
          edark_info_row("Plot type", .plot_type_label(spec$plot_type)),
          # column_a / column_b are the axes: build_bivariate_plot_spec() has
          # already swapped them for the primary variable's role, so naming
          # them by axis is honest where "Primary" would be wrong half the time.
          if (is.null(spec$column_b)) {
            edark_info_row("Variable", spec$column_a)
          } else {
            shiny::tagList(
              edark_info_row("X axis", spec$column_a),
              edark_info_row("Y axis", spec$column_b)
            )
          },
          edark_info_row(
            "Stratify by",
            if (is.null(spec$stratify_by) || !nzchar(spec$stratify_by)) "None"
            else spec$stratify_by
          ),

          # The appearance frozen into this item when it was added (D7). It can
          # differ from the Appearance panel's current settings and from the
          # other items', and it is not visible anywhere else in the app.
          edark_section_label("Appearance when added"),
          edark_info_row("Theme",   spec$ggplot_theme  %||% "-"),
          edark_info_row("Palette", spec$color_palette %||% "-"),
          edark_info_row(
            "Legend",
            if (isTRUE(spec$show_legend)) (spec$legend_position %||% "Shown")
            else "Hidden"
          ),
          edark_info_row("Data labels",
                         if (isTRUE(spec$show_data_labels)) "Shown" else "Hidden"),
          edark_info_row("Added", format(item$added_at, "%H:%M:%S"))
        )
      }

      kinds <- if (n == 0) NULL else table(vapply(
        items, function(it) .plot_type_label(it$plot_spec$plot_type), character(1)
      ))

      shiny::tagList(
        edark_section_label("Selected item", first = TRUE),
        selected_block,

        edark_section_label("Will contain"),
        edark_info_row("Items",         n),
        edark_info_row("Output format", format_label),
        if (!is.null(kinds))
          lapply(names(kinds), function(k) edark_info_row(k, as.integer(kinds[[k]]))),

        edark_section_label("Source data"),
        edark_info_row("Rows",    if (!is.null(ds)) format(nrow(ds), big.mark = ",") else "-"),
        edark_info_row("Columns", if (!is.null(ds)) ncol(ds) else "-"),
        shiny::tags$p(
          class = "small text-muted mt-2 mb-0",
          "Each item is re-drawn from the current working dataset when the
           report is generated, so the file reflects the data as it is now."
        )
      )
    })


    # Messages for the Custom Report page, above the item list.
    edark_messages_server(output, shiny::reactive({
      msgs <- list()
      if (length(shared_state$custom_report_items) == 0) {
        msgs <- c(msgs, list(edark_message(
          "info", "Your custom report is empty.",
          detail = shiny::tagList(
            "Go to ", shiny::tags$strong("Explore"), ", run a plot, then click ",
            shiny::tags$strong("Add to Custom Report"), "."
          )
        )))
      }
      if (isTRUE(shared_state$explore_needs_refresh)) {
        msgs <- c(msgs, list(edark_message(
          "stale", "The working dataset has changed since these items were added.",
          detail = "Their thumbnails still show the earlier data; the generated report will not."
        )))
      }
      if (.preview_is_stale(custom_preview(), custom_report_args())) {
        msgs <- c(msgs, list(.preview_stale_message()))
      }
      msgs
    }), id = "custom_messages")

    # ── Custom Report: generation ─────────────────────────────────────────────
    # What generate_custom_report() is built from. The items are re-drawn from
    # the current working dataset, so the dataset is part of it (§E12).
    custom_report_args <- shiny::reactive({
      list(
        items        = shared_state$custom_report_items,
        dataset      = shared_state$dataset_working,
        column_types = shared_state$column_types
      )
    })

    # Build the custom report into `path` behind the progress modal. Returns
    # TRUE on success; on failure shows the error and returns FALSE.
    .run_custom_report <- function(args, format, path) {
      n_items <- length(args$items)
      if (n_items == 0) {
        shiny::showNotification(
          "Custom report is empty. Add plots from the Explore tab first.",
          type = "warning", duration = 6
        )
        return(FALSE)
      }

      shiny::showModal(.report_progress_modal(
        paste0("Generating Custom Report (", n_items, " item",
               if (n_items != 1) "s" else "", ")")
      ))
      on.exit(shiny::removeModal(), add = TRUE)

      tryCatch({
        generate_custom_report(
          items        = args$items,
          dataset      = args$dataset,
          column_types = args$column_types,
          format       = format,
          output_path  = path,
          # No report-level aesthetics: each item carries the appearance it
          # had on screen when it was added, so the document reproduces what
          # the user saw rather than restyling every item at once (D7).
          progress_fn  = function(frac, detail) {
            session$sendCustomMessage("edark_report_progress",
                                      list(frac = frac, detail = detail))
          }
        )
        TRUE
      }, error = function(e) {
        shiny::showNotification(
          paste("Custom report generation failed:", conditionMessage(e)),
          type     = "error",
          duration = 10
        )
        FALSE
      })
    }

    output$custom_download_btn <- shiny::downloadHandler(
      filename = function() {
        ext <- switch(input$custom_output_format,
                      pptx = ".pptx", docx = ".docx", ".html")
        paste0("edark_custom_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ext)
      },
      content = function(file) {
        fmt <- if (input$custom_output_format %in% c("pptx", "docx"))
                 input$custom_output_format else "html"
        if (!.run_custom_report(shiny::isolate(custom_report_args()), fmt, file))
          stop("Custom report generation failed.")
      }
    )


    # ── Custom Report: in-app preview (Preview tab) ───────────────────────────

    custom_preview <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$custom_preview_btn, {
      built <- .preview_build(custom_preview, "custom", custom_report_args(),
                              function(a, path) .run_custom_report(a, "html", path))
      if (built) bslib::nav_select("custom_result_tabs", "preview", session = session)
    })

    shiny::observeEvent(input$custom_preview_close, {
      .preview_discard(custom_preview)
      bslib::nav_select("custom_result_tabs", "items", session = session)
    })

    output$custom_preview_panel <- shiny::renderUI({
      pv <- custom_preview()
      if (is.null(pv)) return(.preview_empty_state())
      .preview_view(pv, "custom_preview_save", "custom_preview_close")
    })

    output$custom_preview_save <- .preview_save_handler(custom_preview,
                                                        "edark_custom_report_")
  })
}


# Null-coalescing helper (avoids taking a dependency on rlang)
`%||%` <- function(x, y) if (is.null(x)) y else x
