#' Session Module
#'
#' The Session menu on the right of the navbar: *Save session...* downloads a
#' `.edark.rds` file, *Load session...* restores one (§M8). Also applies a
#' session passed to `edark(session = )` at launch.
#'
#' Loading writes the Prepare state itself (the same fields Apply and Cancel
#' write) and hands the Analyze part to Steps 1 and 4 as
#' `shared_state$session_restore`. It never writes analysis fields directly
#' (§M8.10); it only reads `analysis_spec` to save it.
#'
#' @param id Character. The module namespace ID.
#' @param shared_state A Shiny `reactiveValues` object.
#' @param dataset_input The data frame passed to `edark()`, before casting.
#' @param launch_session A session list to apply at startup, or `NULL`.
#'
#' @name module_session
#' @keywords internal
NULL


#' @rdname module_session
session_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_menu(
    title = "Session",
    value = "session_menu",
    align = "right",
    bslib::nav_item(
      shiny::actionLink(ns("save"), shiny::tagList(shiny::icon("download"), " Save session\u2026"),
                        class = "dropdown-item")
    ),
    bslib::nav_item(
      shiny::actionLink(ns("load"), shiny::tagList(shiny::icon("upload"), " Load session\u2026"),
                        class = "dropdown-item")
    )
  )
}


#' @rdname module_session
session_server <- function(id, shared_state, dataset_input, launch_session = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # A session read and checked, waiting on the confirm dialog
    pending <- shiny::reactiveVal(NULL)

    # ── Save ─────────────────────────────────────────────────────────────────
    shiny::observeEvent(input$save, {
      shiny::showModal(shiny::modalDialog(
        title = "Save Session",
        shiny::p("Saves your applied data preparation, the analysis setup (roles, model",
                 "purpose, validation settings and covariates) and the custom report list.",
                 "Results are not saved - re-run them after loading."),
        shiny::checkboxInput(ns("include_data"), "Include dataset", value = FALSE),
        shiny::tags$p(class = "small text-muted mb-0",
                      "Includes patient-level data. Only share where your data governance allows."),
        footer = shiny::tagList(
          shiny::modalButton("Close"),
          shiny::downloadButton(ns("download"), "Save", class = "btn-primary",
                                icon = shiny::icon("download"))
        ),
        easyClose = TRUE
      ))
    }, ignoreInit = TRUE)

    output$download <- shiny::downloadHandler(
      filename = function() session_file_name(),
      content  = function(file) {
        s <- build_session(
          dataset_input       = dataset_input,
          column_types        = shiny::isolate(shared_state$original_column_types),
          prepare             = shiny::isolate(shared_state$last_applied_specs),
          analysis_spec       = shiny::isolate(shared_state$analysis_spec),
          custom_report_items = shiny::isolate(shared_state$custom_report_items),
          include_data        = isTRUE(shiny::isolate(input$include_data))
        )
        saveRDS(s, file)
      }
    )

    # ── Load ─────────────────────────────────────────────────────────────────
    shiny::observeEvent(input$load, {
      shiny::showModal(shiny::modalDialog(
        title = "Load Session",
        shiny::fileInput(ns("file"), label = NULL, accept = ".rds",
                         buttonLabel = "Choose file\u2026",
                         placeholder = "edark_session_....edark.rds"),
        shiny::tags$p(class = "small text-muted mb-0",
                      "The session must come from a dataset with the same columns and column",
                      "types. Any data saved inside the file is ignored - the app keeps its dataset."),
        footer = shiny::modalButton("Cancel"),
        easyClose = TRUE
      ))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$file, {
      f <- input$file
      if (is.null(f) || !nrow(f)) return()
      # Any error refuses the load; a session error's message is written for
      # the user, anything else is shown as is.
      checked <- tryCatch(.check(read_session(f$datapath[1])),
                          error = function(e) e)
      if (inherits(checked, "condition")) {
        .refuse(conditionMessage(checked))
        return()
      }

      shiny::removeModal()
      if (!.app_has_work()) {
        .apply(checked)
        return()
      }
      pending(checked)
      n_items <- length(shiny::isolate(shared_state$custom_report_items))
      shiny::showModal(shiny::modalDialog(
        title = "Load Session?",
        shiny::p("This replaces your current data preparation and analysis setup.",
                 "Analysis results are cleared."),
        if (n_items > 0L) {
          edark_message("warn", sprintf(
            "Your %d custom report item%s will be discarded and replaced by the session's.",
            n_items, if (n_items == 1L) "" else "s"))
        },
        footer = shiny::tagList(
          edark_button(ns, "cancel_load", "Cancel", variant = "secondary", size = "dialog"),
          edark_button(ns, "confirm_load", "Load", variant = "primary", size = "dialog")
        ),
        easyClose = FALSE
      ))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$confirm_load, {
      shiny::removeModal()
      p <- pending()
      pending(NULL)
      if (!is.null(p)) .apply(p)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$cancel_load, {
      shiny::removeModal()
      pending(NULL)
    }, ignoreInit = TRUE)

    # ── Launch: edark(session = ) ────────────────────────────────────────────
    # edark() has already checked the session against the dataset, so a
    # failure here is unexpected; it is shown rather than stopping the app.
    if (!is.null(launch_session)) {
      startup <- shiny::observe({
        startup$destroy()
        checked <- tryCatch(.check(launch_session), error = function(e) e)
        if (inherits(checked, "condition")) .refuse(conditionMessage(checked))
        else .apply(checked)
      })
    }

    # ── Helpers ──────────────────────────────────────────────────────────────

    # Match the session to this dataset and rebuild the working dataset from
    # it. Returns list(session, working) or signals an edark_session_error.
    .check <- function(s) {
      reasons <- session_dataset_mismatch(
        s, dataset_input, shiny::isolate(shared_state$original_column_types))
      if (!is.null(reasons)) {
        .session_error(paste0(.SESSION_MSG_MISMATCH, " ", paste(reasons, collapse = ". "), "."))
      }
      list(session = s,
           working = session_prepare_dataset(s, shiny::isolate(shared_state$dataset_original)))
    }

    .refuse <- function(msg) {
      shiny::showModal(shiny::modalDialog(
        title = "Session Not Loaded",
        shiny::p(msg),
        footer = shiny::modalButton("Close"),
        easyClose = TRUE
      ))
    }

    # Anything a load would replace: applied or staged Prepare changes, a
    # frozen analysis, or queued custom report items.
    .app_has_work <- function() {
      orig <- shiny::isolate(shared_state$dataset_original)
      la   <- shiny::isolate(shared_state$last_applied_specs)
      prepared <- !setequal(la$included_columns, names(orig)) ||
                  length(la$column_type_overrides)  > 0L ||
                  length(la$column_transform_specs) > 0L ||
                  length(la$row_filter_specs)       > 0L
      prepared ||
        isTRUE(shiny::isolate(shared_state$has_pending_changes)) ||
        !is.null(shiny::isolate(shared_state$analysis_data)) ||
        length(shiny::isolate(shared_state$custom_report_items)) > 0L
    }

    # The load sequence (§M8.7), from a checked session.
    .apply <- function(checked) {
      s  <- checked$session
      df <- checked$working
      p  <- s$prepare

      # Prepare: the staged fields, then the same commit Apply does. No filter
      # pruning - the specs were applied together when saved. revert_trigger
      # makes every Prepare tab resync its inputs to them.
      prep <- list(
        included_columns       = p$included_columns,
        column_type_overrides  = p$column_type_overrides  %||% list(),
        column_transform_specs = p$column_transform_specs %||% list(),
        row_filter_specs       = p$row_filter_specs       %||% list()
      )
      shared_state$included_columns       <- prep$included_columns
      shared_state$column_type_overrides  <- prep$column_type_overrides
      shared_state$column_transform_specs <- prep$column_transform_specs
      shared_state$row_filter_specs       <- prep$row_filter_specs
      .commit_working_dataset(shared_state, df)   # also snapshots last_applied_specs
      shared_state$revert_trigger         <- shiny::isolate(shared_state$revert_trigger) + 1L

      # Custom report: the session's list replaces the current one
      old <- shiny::isolate(shared_state$custom_report_items)
      # as.character(): unlist() of no items is NULL, and file.exists(NULL) errors
      old_paths <- as.character(unlist(lapply(old, `[[`, "thumb_path")))
      unlink(old_paths[nzchar(old_paths) & file.exists(old_paths)])
      shared_state$custom_report_items <- .session_unpack_items(s$custom_report_items %||% list())

      # Analyze: handed to Steps 1 and 4. With no analysis in the session, a
      # frozen analysis is cleared so it cannot describe the old preparation.
      a <- s$analysis
      frozen <- !is.null(shiny::isolate(shared_state$analysis_data))
      if (!is.null(a) || frozen) {
        shared_state$session_restore <- list(
          token      = Sys.time(),
          step1_done = FALSE,
          roles      = a$roles,
          purpose_specification = a$purpose_specification,
          validation_settings   = a$validation_settings,
          covariates = a$final_model_covariates
        )
      }

      # Navigate once Steps 1 and 4 have taken their payload (same flush)
      target <- if (length(a$final_model_covariates)) "step4" else if (!is.null(a)) "step1"
      root <- session$rootScope()
      session$onFlushed(function() {
        if (is.null(target)) {
          bslib::nav_select("main_navbar", "prepare", session = root)
        } else {
          bslib::nav_select("main_navbar", "analyze", session = root)
          bslib::nav_select("analysis_main-analysis_steps", target, session = root)
        }
      }, once = TRUE)

      shiny::showNotification(
        sprintf("Session loaded (saved %s).", format(s$saved_at, "%Y-%m-%d %H:%M")),
        type = "message", duration = 5
      )
    }
  })
}
