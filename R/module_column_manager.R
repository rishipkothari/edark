#' Column Manager Module
#'
#' Compact table showing all columns with an Include checkbox per column.
#' Transform staging and configuration is handled entirely in the Transforms tab.
#'
#' @param id Character. The module namespace ID.
#' @param shared_state A Shiny `reactiveValues` object.
#'
#' @name module_column_manager
NULL


#' @rdname module_column_manager
#' @export
column_manager_ui <- function(id) {
  ns <- shiny::NS(id)

  # Plain content, not a card: Prepare's pages already sit in a card tab
  # (level 4, inst/www/edark.css section 3), so a card here was a box in a box.
  shiny::tagList(
    shiny::div(
      class = "mb-2 small",
      shiny::actionLink(ns("select_all"),   "Select all"),
      shiny::actionLink(ns("deselect_all"), "Clear", class = "ms-3")
    ),
    # Scroll the rows, not the page, so "Select all" and the header stay put
    # however many columns the dataset has (§BUILD_UI-redesign 2.6).
    shiny::div(
      class = "edark-scroll-table",
      shiny::uiOutput(ns("column_table"))
    )
  )
}


#' @rdname module_column_manager
#' @export
column_manager_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Render compact column table ──────────────────────────────────────────
    output$column_table <- shiny::renderUI({
      dataset    <- shared_state$dataset_original
      col_names  <- names(dataset)
      orig_types <- shared_state$original_column_types  # never changes
      included   <- shared_state$included_columns

      current_types <- shared_state$column_types
      # Applied, not staged: the badge sits beside Curr. type, which is applied.
      # Covers transforms that keep the type (log, winsorize, standardize ...).
      applied_tx    <- names(shared_state$last_applied_specs$column_transform_specs)

      header <- shiny::tags$thead(
        shiny::tags$tr(
          shiny::tags$th(class = "ps-2", "Include"),
          shiny::tags$th("Column name"),
          shiny::tags$th("Unique"),
          shiny::tags$th("Orig. type"),
          shiny::tags$th("Curr. type")
        )
      )

      rows <- lapply(col_names, function(col) {
        orig_type    <- orig_types[[col]]
        curr_type    <- if (col %in% names(current_types)) current_types[[col]] else orig_type
        n_unique     <- length(unique(na.omit(dataset[[col]])))
        is_included  <- col %in% included
        type_changed <- !identical(orig_type, curr_type)

        shiny::tags$tr(
          shiny::tags$td(
            class = "edark-checkbox-cell ps-2 py-0",
            shiny::checkboxInput(ns(paste0("include_", col)), label = NULL, value = is_included)
          ),
          shiny::tags$td(class = "py-1 align-middle small fw-semibold", col),
          shiny::tags$td(
            class = "py-1 align-middle text-muted small",
            format(n_unique, big.mark = ",")
          ),
          shiny::tags$td(
            class = "py-1 align-middle",
            edark_type_badge(orig_type)
          ),
          shiny::tags$td(
            class = "py-1 align-middle",
            edark_type_badge(curr_type, transformed = type_changed || col %in% applied_tx)
          )
        )
      })

      # Autofit: as wide as its content, left-aligned, not stretched to the pane.
      shiny::tags$table(
        class = "table table-sm table-hover align-middle mb-0 edark-autofit-table",
        header,
        shiny::tags$tbody(rows)
      )
    })


    # ── Observe include checkboxes ────────────────────────────────────────────
    # Register once for all columns (column list never changes within a session).
    # Guard with !identical() so that UI re-renders during revert (which reset
    # checkboxes to already-current values) do not spuriously set has_pending_changes.
    shiny::observe({
      col_names <- names(shiny::isolate(shared_state$dataset_original))
      lapply(col_names, function(col) {
        shiny::observeEvent(input[[paste0("include_", col)]], {
          included     <- shared_state$included_columns
          new_included <- if (isTRUE(input[[paste0("include_", col)]])) {
            union(included, col)
          } else {
            setdiff(included, col)
          }
          if (!identical(new_included, included)) {
            shared_state$included_columns    <- new_included
            shared_state$has_pending_changes <- TRUE
          }
        }, ignoreInit = TRUE)
      })
    })

    # ── Revert trigger: sync checkboxes to last-applied state ─────────────────
    shiny::observeEvent(shared_state$revert_trigger, {
      specs     <- shared_state$last_applied_specs
      col_names <- names(shared_state$dataset_original)
      included  <- if (!is.null(specs$included_columns)) specs$included_columns else col_names
      lapply(col_names, function(col)
        shiny::updateCheckboxInput(session, paste0("include_", col), value = col %in% included)
      )
    }, ignoreInit = TRUE)


    # ── Select / Deselect all ────────────────────────────────────────────────
    shiny::observeEvent(input$select_all, {
      col_names <- names(shared_state$dataset_original)
      shared_state$included_columns    <- col_names
      shared_state$has_pending_changes <- TRUE
      lapply(col_names, function(col)
        shiny::updateCheckboxInput(session, paste0("include_", col), value = TRUE))
    })

    shiny::observeEvent(input$deselect_all, {
      shared_state$included_columns    <- character(0)
      shared_state$has_pending_changes <- TRUE
      col_names <- names(shared_state$dataset_original)
      lapply(col_names, function(col)
        shiny::updateCheckboxInput(session, paste0("include_", col), value = FALSE))
    })
  })
}
