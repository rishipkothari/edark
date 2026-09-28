#' Row Filter Module
#'
#' Allows the user to add row-filter criteria against any included column.
#' Numeric columns get lower / upper limit boxes (with the column's original
#' range for context); factor/character columns get a button group of levels
#' to retain. Multiple filters compose with AND logic.
#' All filters are staged — nothing is applied until "Apply & Proceed".
#'
#' @param id Character. The module namespace ID.
#' @param shared_state A Shiny `reactiveValues` object.
#'
#' @name module_row_filter
NULL


#' @rdname module_row_filter
#' @export
row_filter_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    # Add-filter controls - the button sits against the picker it acts on,
    # bottom-aligned with the picker's box rather than its label.
    shiny::div(
      class = "d-flex align-items-end gap-2 mb-3 edark-filter-add",
      shiny::div(style = "width: 280px;", shiny::uiOutput(ns("column_picker"))),
      edark_button(ns, "add_filter", "Add filter", icon = "plus",
                   outline = TRUE, size = "dialog")
    ),

    # The live row count is a fact about the result, so it lives in Prepare's
    # info pane (F2). It used to be a badge here, which scrolled away as
    # filters accumulated (§BUILD_UI-redesign 2.6).
    shiny::hr(),

    # Active filter cards - the stack grows one card per filter, so it scrolls
    # itself rather than the page.
    shiny::div(
      class = "edark-scroll-table",
      shiny::uiOutput(ns("active_filters"))
    )
  )
}


#' @rdname module_row_filter
#' @export
row_filter_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Track which columns already have input observers registered so we never
    # double-register when the specs reactive fires multiple times.
    registered_cols <- character(0)

    # ── Column picker ─────────────────────────────────────────────────────────
    output$column_picker <- shiny::renderUI({
      included <- shared_state$included_columns
      shinyWidgets::pickerInput(
        ns("filter_column"),
        label   = "Column to filter:",
        choices = included,
        options = shinyWidgets::pickerOptions(liveSearch = TRUE, container = "body")
      )
    })



    # ── Add filter ────────────────────────────────────────────────────────────
    shiny::observeEvent(input$add_filter, {
      col <- input$filter_column
      if (is.null(col) || col == "") return()
      if (!is.null(shared_state$row_filter_specs[[col]])) return()

      # Use dataset_working so that post-Apply transforms (e.g. numeric → factor)
      # are reflected in both the type and the data values used to build the spec.
      x        <- shared_state$dataset_working[[col]]
      col_type <- shared_state$column_types[[col]]

      if (col_type == "numeric") {
        spec <- list(
          type = "numeric",
          min  = min(x, na.rm = TRUE),
          max  = max(x, na.rm = TRUE),
          data_min = min(x, na.rm = TRUE),
          data_max = max(x, na.rm = TRUE)
        )
      } else {
        lvls <- as.character(sort(unique(x[!is.na(x)])))
        spec <- list(
          type             = "categorical",
          levels_all       = lvls,
          levels_selected  = lvls
        )
      }

      specs        <- shared_state$row_filter_specs
      specs[[col]] <- spec
      shared_state$row_filter_specs    <- specs
      shared_state$has_pending_changes <- TRUE
    })


    # ── Render filter cards ───────────────────────────────────────────────────
    # Re-render only when the set of filtered columns changes, or on a revert /
    # session load - not on every edit of a filter's values. Re-rendering on
    # each edit rebuilt every card, so tabbing from Lower to Upper lost focus
    # the moment Lower was sent. A reactiveVal only invalidates on a new value.
    filter_cols <- shiny::reactiveVal(character(0))
    shiny::observe(filter_cols(names(shared_state$row_filter_specs)))

    output$active_filters <- shiny::renderUI({
      cols <- filter_cols()
      shared_state$revert_trigger
      specs <- shiny::isolate(shared_state$row_filter_specs)
      orig  <- shiny::isolate(shared_state$dataset_original)
      if (length(cols) == 0) {
        return(shiny::tags$p(
          class = "text-muted small",
          "No filters added yet. Select a column above and click Add filter."
        ))
      }

      lapply(cols, function(col) {
        .render_filter_widget(ns, col, specs[[col]], orig[[col]])
      })
    })


    # ── Register input observers for filter widgets ───────────────────────────
    # Re-runs when row_filter_specs changes (new filter added), but only
    # registers observers for columns not yet tracked.
    shiny::observe({
      specs    <- shared_state$row_filter_specs
      new_cols <- setdiff(names(specs), registered_cols)
      if (length(new_cols) == 0) return()

      for (col in new_cols) {
        local({
          .col     <- col
          col_type <- specs[[.col]]$type

          if (col_type == "numeric") {
            # One observer per bound. An empty box (NA) leaves the bound as it
            # was; lower > upper is allowed here and flagged in the messages
            # area (.build_prepare_warnings()), not silently swapped.
            Map(function(prefix, field) {
              shiny::observeEvent(input[[paste0(prefix, .col)]], {
                val <- suppressWarnings(as.numeric(input[[paste0(prefix, .col)]]))
                s   <- shared_state$row_filter_specs
                if (length(val) != 1L || is.na(val) || is.null(s[[.col]])) return()
                if (!identical(s[[.col]][[field]], val)) {
                  s[[.col]][[field]]               <- val
                  shared_state$row_filter_specs    <- s
                  shared_state$has_pending_changes <- TRUE
                }
              }, ignoreNULL = TRUE, ignoreInit = TRUE)
            }, c("lo_", "hi_"), c("min", "max"))

          } else {
            shiny::observeEvent(input[[paste0("levels_", .col)]], {
              val <- input[[paste0("levels_", .col)]]
              s   <- shared_state$row_filter_specs
              if (!is.null(s[[.col]])) {
                s[[.col]]$levels_selected        <- val
                shared_state$row_filter_specs    <- s
                shared_state$has_pending_changes <- TRUE
              }
            }, ignoreNULL = TRUE, ignoreInit = TRUE)
          }

          # Remove button
          shiny::observeEvent(input[[paste0("remove_filter_", .col)]], {
            s         <- shared_state$row_filter_specs
            s[[.col]] <- NULL
            shared_state$row_filter_specs    <- s
            shared_state$has_pending_changes <- TRUE
          }, ignoreInit = TRUE, once = TRUE)
        })

        registered_cols <<- c(registered_cols, col)
      }
    })

    # When a column is excluded, remove its filter spec reactively so the UI
    # stays in sync and the warnings panel reflects the current state.
    shiny::observeEvent(shared_state$included_columns, {
      included <- shared_state$included_columns
      specs    <- shared_state$row_filter_specs
      stale    <- setdiff(names(specs), included)
      if (length(stale) > 0) {
        for (col in stale) specs[[col]] <- NULL
        shared_state$row_filter_specs    <- specs
        shared_state$has_pending_changes <- TRUE
      }
    }, ignoreInit = TRUE)

    # When row_filter_specs is cleared (Apply or Reset), purge the cache so
    # the same column can be re-registered with fresh observers next time.
    shiny::observeEvent(shared_state$row_filter_specs, {
      if (length(shared_state$row_filter_specs) == 0)
        registered_cols <<- character(0)
    }, ignoreInit = TRUE)

    # Revert trigger: row_filter_specs has already been restored by
    # .revert_to_last_applied(); clear the registration cache so the reverted
    # filter specs can register fresh observers when active_filters re-renders.
    shiny::observeEvent(shared_state$revert_trigger, {
      registered_cols <<- character(0)
    }, ignoreInit = TRUE)
  })
}


# ── Internal helpers ──────────────────────────────────────────────────────────

.fmt_filter_num <- function(x) {
  format(signif(x, 6), big.mark = ",", scientific = FALSE, trim = TRUE)
}

# `orig` is the column in dataset_original, shown as context for a numeric
# filter. The filter itself acts on the working (possibly transformed) values,
# so when the two ranges differ both are shown.
.render_filter_widget <- function(ns, col, spec, orig = NULL) {
  remove_btn <- shiny::actionLink(
    ns(paste0("remove_filter_", col)),
    label = shiny::icon("xmark"),
    class = "text-danger"
  )

  widget <- if (spec$type == "numeric") {
    range_txt <- function(lo, hi) paste(.fmt_filter_num(lo), "to", .fmt_filter_num(hi))
    context <- if (is.numeric(orig) && any(!is.na(orig))) {
      o_lo <- min(orig, na.rm = TRUE)
      o_hi <- max(orig, na.rm = TRUE)
      txt  <- paste0("Original range: ", range_txt(o_lo, o_hi))
      if (!isTRUE(all.equal(c(o_lo, o_hi), c(spec$data_min, spec$data_max))))
        txt <- paste0(txt, "; after transforms: ", range_txt(spec$data_min, spec$data_max))
      txt
    } else {
      paste0("Range: ", range_txt(spec$data_min, spec$data_max))
    }

    shiny::tagList(
      shiny::div(
        class = "d-flex flex-wrap align-items-end gap-3 mb-2 edark-filter-add",
        # updateOn = "blur": sent on blur or Enter, not per keystroke, so a
        # half-typed number never lands in the spec.
        shiny::numericInput(ns(paste0("lo_", col)), "Lower limit",
                            value = spec$min, width = "160px", updateOn = "blur"),
        shiny::numericInput(ns(paste0("hi_", col)), "Upper limit",
                            value = spec$max, width = "160px", updateOn = "blur")
      ),
      shiny::tags$p(
        class = "small text-muted mb-0",
        paste0(context, ". Rows from lower to upper (inclusive) are kept; ",
               "missing values are dropped.")
      )
    )
  } else {
    shiny::div(
      class = "edark-filter-levels",
      shinyWidgets::checkboxGroupButtons(
        ns(paste0("levels_", col)),
        label     = NULL,
        choices   = spec$levels_all,
        selected  = spec$levels_selected,
        direction = "horizontal"
      )
    )
  }

  bslib::card(
    class = "mb-2",
    bslib::card_header(
      class = "py-1 d-flex justify-content-between align-items-center",
      shiny::tags$strong(col),
      remove_btn
    ),
    bslib::card_body(class = "py-2", widget)
  )
}


# Apply row filter specs to a dataset (used for live preview in the badge).
.apply_row_filters_preview <- function(dataset, specs) {
  result <- dataset
  for (col in names(specs)) {
    if (!col %in% names(result)) next
    spec <- specs[[col]]
    if (spec$type == "numeric") {
      result <- result[
        !is.na(result[[col]]) &
          result[[col]] >= spec$min &
          result[[col]] <= spec$max, ,
        drop = FALSE
      ]
    } else {
      result <- result[
        !is.na(result[[col]]) &
          as.character(result[[col]]) %in% spec$levels_selected, ,
        drop = FALSE
      ]
    }
  }
  result
}
