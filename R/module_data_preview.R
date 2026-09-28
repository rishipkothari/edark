#' Data Preview Module
#'
#' One table at a time, chosen by two toggles: which dataset (Original or
#' Working) and which view (Data or Summary). Transformed columns are tinted
#' and their type label gains a "T" badge in the data views.
#'
#' Before UI Stage 5 these four tables were three levels of tabs deep - the
#' navbar, Prepare's card tabs, a `navset_card_tab` and a `navset_tab` inside
#' it (§BUILD_UI-redesign 2.3). Two toggles say the same thing in one level.
#'
#' @param id Character. The module namespace ID.
#' @param shared_state A Shiny `reactiveValues` object.
#'
#' @name module_data_preview
NULL


#' @rdname module_data_preview
#' @export
data_preview_ui <- function(id) {
  ns <- shiny::NS(id)

  # Both toggles pick what the one table below shows; neither is staged, so
  # they sit with the table rather than in Prepare's config pane, which is
  # shared by all four Prepare pages and holds only Apply / Reset.
  shiny::tagList(
    shiny::div(
      class = "d-flex flex-wrap align-items-center gap-3 mb-3",
      shinyWidgets::radioGroupButtons(
        ns("which_dataset"),
        label    = NULL,
        choices  = c("Original" = "original", "Working" = "working"),
        selected = "original",
        size     = "sm"
      ),
      shinyWidgets::radioGroupButtons(
        ns("which_view"),
        label    = NULL,
        choices  = c("Data" = "data", "Summary" = "summary"),
        selected = "data",
        size     = "sm"
      ),
      shiny::uiOutput(ns("preview_caption"), inline = TRUE)
    ),

    # One panel per combination. conditionalPanel only shows or hides, so the
    # reactables are never re-rendered by a toggle.
    shiny::conditionalPanel(
      condition = "input.which_dataset == 'original' && input.which_view == 'data'",
      ns = ns,
      shiny::div(class = "edark-scroll-table",
                 reactable::reactableOutput(ns("original_table")))
    ),
    shiny::conditionalPanel(
      condition = "input.which_dataset == 'original' && input.which_view == 'summary'",
      ns = ns,
      shiny::div(class = "edark-scroll-table",
                 reactable::reactableOutput(ns("original_summary")))
    ),
    shiny::conditionalPanel(
      condition = "input.which_dataset == 'working' && input.which_view == 'data'",
      ns = ns,
      shiny::div(class = "edark-scroll-table",
                 reactable::reactableOutput(ns("working_table")))
    ),
    shiny::conditionalPanel(
      condition = "input.which_dataset == 'working' && input.which_view == 'summary'",
      ns = ns,
      shiny::div(class = "edark-scroll-table",
                 reactable::reactableOutput(ns("working_summary")))
    )
  )
}



#' @rdname module_data_preview
#' @export
data_preview_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {

    output$preview_caption <- shiny::renderUI({
      shiny::tags$span(class = "small text-muted",
                       .dp_caption(input$which_dataset, input$which_view))
    })

    # ── Original: data table ──────────────────────────────────────────────────
    output$original_table <- reactable::renderReactable({
      df         <- shared_state$dataset_original
      orig_types <- shared_state$original_column_types
      shiny::req(!is.null(df))

      staged_transforms <- names(shared_state$column_transform_specs)

      col_defs <- .make_col_defs(df, orig_types,
                                  tinted_cols  = staged_transforms,
                                  numeric_orig = names(orig_types)[orig_types == "numeric"])

      reactable::reactable(
        df,
        columns             = col_defs,
        compact             = TRUE,
        bordered            = TRUE,
        striped             = FALSE,
        highlight           = TRUE,
        resizable           = TRUE,
        defaultPageSize     = 100,
        showPageSizeOptions = TRUE,
        pageSizeOptions     = c(100, 250, 500)
      )
    })


    # ── Original: summary table ───────────────────────────────────────────────
    output$original_summary <- reactable::renderReactable({
      df         <- shared_state$dataset_original
      orig_types <- shared_state$original_column_types
      shiny::req(!is.null(df))

      summary_df <- .build_dataset_summary(df, orig_types)
      shiny::req(!is.null(summary_df) && nrow(summary_df) > 0)

      .render_summary_reactable(summary_df)
    })


    # ── Working: data table ───────────────────────────────────────────────────
    output$working_table <- reactable::renderReactable({
      df            <- shared_state$dataset_working
      orig_types    <- shared_state$original_column_types
      current_types <- shared_state$column_types
      applied_specs <- shared_state$column_transform_specs
      shiny::req(!is.null(df))

      # Tint any column that has an applied transform spec (covers type-changing
      # transforms like auto/cutpoints AND numeric-preserving ones like log/
      # winsorize/round/standardize where curr_type == orig_type = "numeric")
      tinted <- names(df)[names(df) %in% names(applied_specs)]

      col_defs <- .make_col_defs(df, current_types,
                                  tinted_cols  = tinted,
                                  numeric_orig = names(orig_types)[orig_types == "numeric"])

      reactable::reactable(
        df,
        columns             = col_defs,
        compact             = TRUE,
        bordered            = TRUE,
        striped             = FALSE,
        highlight           = TRUE,
        resizable           = TRUE,
        defaultPageSize     = 100,
        showPageSizeOptions = TRUE,
        pageSizeOptions     = c(100, 250, 500)
      )
    })


    # ── Working: summary table ────────────────────────────────────────────────
    output$working_summary <- reactable::renderReactable({
      df            <- shared_state$dataset_working
      current_types <- shared_state$column_types
      shiny::req(!is.null(df))

      summary_df <- .build_dataset_summary(df, current_types)
      shiny::req(!is.null(summary_df) && nrow(summary_df) > 0)

      .render_summary_reactable(summary_df)
    })

  })
}


# ── Internal helpers ──────────────────────────────────────────────────────────

# Build a named list of reactable colDef objects for the raw data table.
.make_col_defs <- function(df, type_map, tinted_cols, numeric_orig) {
  col_names <- names(df)

  defs <- lapply(col_names, function(col) {
    col_type    <- if (!is.null(type_map[[col]])) type_map[[col]] else "unknown"
    is_tinted   <- col %in% tinted_cols
    is_num_orig <- col %in% numeric_orig

    header_fn <- function(value) {
      shiny::tags$div(
        shiny::tags$div(value, style = "font-weight: 600;"),
        shiny::tags$div(
          # The "T" badge marks a transformed column, the same badge Prepare >
          # Columns puts beside the type, so one mark means "the pipeline
          # touched this" in both places.
          col_type,
          if (is_tinted) edark_transformed_badge(),
          style = paste0(
            "font-size: 0.72em; color: ",
            if (is_tinted) "var(--edark-tint-fg)" else "var(--bs-secondary-color)",
            "; font-style: italic;"
          )
        )
      )
    }

    # Tokens, not literals: the amber this used to be was the app's warning
    # colour doing a second job, and it had no dark-mode value.
    cell_style <- if (is_tinted) list(background = "var(--edark-tint-bg)") else NULL

    reactable::colDef(
      header = header_fn,
      align  = if (is_num_orig) "right" else "left",
      style  = cell_style
    )
  })

  names(defs) <- col_names
  defs
}


# Render a .build_dataset_summary() data frame as a compact reactable.
.render_summary_reactable <- function(summary_df) {
  # NA numerics → em-dash for display
  num_cols <- c("Min", "Max", "Mean", "SD", "Median", "IQR", "Skewness", "Kurtosis")
  disp     <- summary_df
  for (col in num_cols) {
    disp[[col]] <- ifelse(is.na(disp[[col]]), "-", as.character(disp[[col]]))
  }
  disp$Top_values <- ifelse(is.na(disp$Top_values), "-", disp$Top_values)

  reactable::reactable(
    disp,
    compact         = TRUE,
    bordered        = TRUE,
    striped         = FALSE,
    highlight       = TRUE,
    resizable       = TRUE,
    defaultPageSize = 250,
    columns = list(
      Variable   = reactable::colDef(minWidth = 110, sticky = "left",
                     style = list(fontWeight = "600")),
      Type       = reactable::colDef(minWidth = 75,
                     cell = function(v) edark_type_badge(v)),
      N          = reactable::colDef(minWidth = 60,  align = "right"),
      N_missing  = reactable::colDef(name = "Missing", minWidth = 70, align = "right"),
      Pct_miss   = reactable::colDef(name = "% Miss",  minWidth = 65, align = "right"),
      N_unique   = reactable::colDef(name = "Unique",  minWidth = 65, align = "right"),
      Min        = reactable::colDef(minWidth = 70,  align = "right"),
      Max        = reactable::colDef(minWidth = 70,  align = "right"),
      Mean       = reactable::colDef(minWidth = 70,  align = "right"),
      SD         = reactable::colDef(minWidth = 70,  align = "right"),
      Median     = reactable::colDef(minWidth = 70,  align = "right"),
      IQR        = reactable::colDef(minWidth = 60,  align = "right"),
      Skewness   = reactable::colDef(minWidth = 80,  align = "right"),
      Kurtosis   = reactable::colDef(minWidth = 80,  align = "right"),
      Top_values = reactable::colDef(name = "Top values", minWidth = 160)
    )
  )
}


# The caption that used to sit above each of the four tables as its own
# paragraph. One line, driven by the two toggles.
.dp_caption <- function(which_dataset, which_view) {
  if (identical(which_dataset, "original")) {
    if (identical(which_view, "data")) {
      "Dataset as passed to edark(), after auto-casting only. No filters or transforms applied."
    } else {
      "EDA summary for numeric and factor columns (original dataset)."
    }
  } else {
    if (identical(which_view, "data")) {
      "Dataset after the last Apply. Tinted columns marked T have a transform applied."
    } else {
      "EDA summary for numeric and factor columns (working dataset after Apply)."
    }
  }
}
