#' Launch the EDARK exploratory data analysis GUI
#'
#' The primary entry point for the package. Validates the input dataset,
#' auto-casts column types, initialises the session-scoped shared state, and
#' launches the Shiny application.
#'
#' @param dataset A `data.frame` or `tibble`. Defaults to the built-in
#'   `liver_tx` synthetic liver transplant dataset so that `edark()` with no
#'   arguments launches immediately for testing and demonstration.
#' @param max_factor_levels Integer. Character columns with no more than this
#'   many unique non-NA values are auto-converted to `factor` at launch.
#'   Also used as the high-cardinality guard threshold in the Explore stage.
#'   Default `20`.
#' @param session Path to a saved session (`.edark.rds`, from the Session
#'   menu) to restore at launch (§M8.8). The session must match the dataset's
#'   columns and column types exactly. If `dataset` is omitted, the data saved
#'   in the session file is used.
#'
#' @return Launches a Shiny app (does not return a value).
#'
#' @export
#' @examples
#' \dontrun{
#' edark()                  # launches with built-in liver_tx demo data
#' edark(mtcars)
#' edark(palmerpenguins::penguins, max_factor_levels = 10)
#' edark(my_data, session = "edark_session_2026-09-28_101500.edark.rds")
#' }
edark <- function(dataset = liver_tx, max_factor_levels = 20, session = NULL) {

  # ── Session file (§M8.8) ───────────────────────────────────────────────────
  # Read before validation: with no dataset argument, the session's own data
  # is the dataset. `session` is renamed here because server() below has a
  # `session` argument of its own.
  launch_session <- NULL
  if (!is.null(session)) {
    launch_session <- tryCatch(read_session(session), edark_session_error = function(e) {
      stop(conditionMessage(e), call. = FALSE)
    })
    if (missing(dataset)) {
      if (is.null(launch_session$data)) {
        stop("This session has no data. Call edark(your_data, session = ...).", call. = FALSE)
      }
      dataset <- launch_session$data
    }
  }

  # ── Validate ───────────────────────────────────────────────────────────────
  validate_input(dataset, max_factor_levels)

  # ── Static assets ──────────────────────────────────────────────────────────
  # Serve inst/www at /edark so the stylesheet can be linked in the UI header
  # below. One CSS file, no build step (PRD/BUILD_UI-redesign.md D5).
  shiny::addResourcePath("edark", system.file("www", package = "edark"))

  # ── Pre-process (runs once, before the reactive graph starts) ──────────────
  dataset_cast  <- cast_column_types(dataset, max_factor_levels)
  column_types  <- detect_column_types(dataset_cast)

  # Refuse a mismatched session here, in the console, rather than in the app
  if (!is.null(launch_session)) {
    reasons <- session_dataset_mismatch(launch_session, dataset, column_types)
    if (!is.null(reasons)) {
      stop(.SESSION_MSG_MISMATCH, "\n", paste0("  ", reasons, collapse = "\n"), call. = FALSE)
    }
    tryCatch(session_prepare_dataset(launch_session, dataset_cast),
             edark_session_error = function(e) stop(conditionMessage(e), call. = FALSE))
    launch_session$data <- NULL   # not needed past this point
  }

  # ── UI ─────────────────────────────────────────────────────────────────────
  ui <- bslib::page_navbar(
    id    = "main_navbar",
    title = shiny::tags$span(
      shiny::tags$strong("EDARK"),
      shiny::tags$span(paste0(" v", EDARK_VERSION),
                       class = "text-muted small ms-1")
    ),
    theme = bslib::bs_theme(
      version    = 5,
      bootswatch = "flatly",
      primary    = "#2c7be5",
      # flatly's own warning is #f39c12, a bright orange that read as an alarm
      # wherever it landed - a badge, a dialog button, a full-width alert. One
      # muted ochre replaces it everywhere; dark mode lifts it in edark.css.
      warning    = "#b7791f",
      # The native OS font stack, not font_google(): a Google font needs
      # internet on first launch, which locked-down hospital machines do not
      # have (§BUILD_UI-redesign Stage 3).
      base_font  = bslib::font_collection(
        "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"
      )
    ),
    header = shiny::tagList(
      # Required for shinyjs::disabled() / toggleState() to take effect
      shinyjs::useShinyjs(),
      edark_splash(),
      # The one stylesheet (inst/www/edark.css, served at /edark)
      shiny::tags$head(
        shiny::tags$link(rel = "stylesheet", type = "text/css",
                         href = "edark/edark.css")
      )
    ),

    # ── Tab 1: Prepare ───────────────────────────────────────────────────────
    bslib::nav_panel(
      value = "prepare",
      title = "1 \u00b7 Prepare",
      edark_page(
        config   = prepare_confirm_ui("prepare_confirm"),
        messages = prepare_confirm_messages_ui("prepare_confirm"),
        info     = prepare_confirm_info_ui("prepare_confirm"),
        result   = bslib::navset_card_tab(
          id = "prepare_tabs",
          bslib::nav_panel(
            value = "columns",
            title = "Columns",
            column_manager_ui("column_manager")
          ),
          bslib::nav_panel(
            value = "transforms",
            title = "Transforms",
            transform_variables_ui("transform_variables")
          ),
          bslib::nav_panel(
            value = "filters",
            title = "Row Filters",
            row_filter_ui("row_filter")
          ),
          bslib::nav_panel(
            value = "preview",
            title = "Data Preview",
            data_preview_ui("data_preview")
          )
        )
      )
    ),

    # ── Tab 2: Explore ───────────────────────────────────────────────────────
    bslib::nav_panel(
      value = "explore",
      title = "2 \u00b7 Explore",
      bslib::navset_pill(
        id = "explore_tabs",
        bslib::nav_panel(
          value = "plot",
          title = "Explore Data",
          edark_page(
            config = shiny::tagList(
              # Describe / Correlate / Trend are modes: each stages its own
              # pickers and they all feed the one plot panel (level 3a, D8).
              # Appearance used to sit in this row; it is not a mode but one
              # app-level live setting, so it is now its own pill beside
              # Explore Data and Report.
              #
              # The panel is identified so each mode server can reclaim the
              # shared_state fields it owns the moment its panel is shown -
              # without that, the three panels overwrite each other's
              # primary / secondary / stratify picks.
              bslib::navset_pill(
                id = "explore_mode",
                bslib::nav_panel("Describe",  value = "describe",
                                 describe_controls_ui("describe_controls")),
                bslib::nav_panel("Correlate", value = "correlate",
                                 relationship_controls_ui("relationship_controls")),
                bslib::nav_panel("Trend",     value = "trend",
                                 trend_controls_ui("trend_controls"))
              )
            ),
            result   = explore_output_ui("explore_output"),
            messages = explore_output_messages_ui("explore_output"),
            info     = explore_output_info_ui("explore_output")
          )
        ),
        bslib::nav_panel(
          value = "report",
          title = "Report",
          report_ui("report")
        ),
        # A settings page, not a mode of the plot: no config pane, no result,
        # the controls themselves fill the main area. Likely to broaden into
        # "Settings" later, which is why it is a page rather than a panel.
        bslib::nav_panel(
          value = "appearance",
          title = "Appearance",
          appearance_page_ui("appearance_controls")
        )
      )
    ),

    # ── Tab 3: Analyze ───────────────────────────────────────────────────────
    bslib::nav_panel(
      value = "analyze",
      title = "3 \u00b7 Analyze",
      analysis_main_ui("analysis_main")
    ),

    # ── Tab 4: Export ────────────────────────────────────────────────────────
    # Materials from every stage in one zip (PRD/BUILD_Export.md), and the R
    # script that repeats the work (PRD §A7.9). Its own top-level step because
    # it works without any Analyze work.
    bslib::nav_panel(
      value = "export",
      title = "4 \u00b7 Export",
      export_page_ui("export", is_demo = isTRUE(identical(dataset, liver_tx)))
    ),

    bslib::nav_spacer(),
    session_ui("session"),
    bslib::nav_item(
      shiny::actionButton("debug_btn", label = shiny::icon("bug"))
    ),
    # Bootstrap 5.3 colour modes, not a preset swap. The old toggle rebuilt the
    # whole theme (flatly <-> darkly) on every click, which re-sends the
    # stylesheet and re-lays out the page; this flips one attribute and the
    # variables in edark.css follow (§BUILD_UI-redesign Stage 3).
    bslib::nav_item(
      bslib::input_dark_mode(id = "dark_mode")
    )
  )


  # ── Server ─────────────────────────────────────────────────────────────────
  server <- function(input, output, session) {

    # Session-scoped shared state — the single source of truth for everything.
    shared_state <- shiny::reactiveValues(

      # Dataset
      dataset_original        = dataset_cast,
      dataset_working         = dataset_cast,
      column_types            = column_types,
      original_column_types   = column_types,  # set once at launch, never overwritten

      # Prepare stage: staged (unapplied)
      included_columns        = names(dataset_cast),
      column_type_overrides   = list(),
      row_filter_specs        = list(),
      column_transform_specs  = list(),
      has_pending_changes     = FALSE,

      # Explore stage — Analyze tab
      primary_variable        = NULL,
      primary_variable_role   = "exposure",
      secondary_variable      = NULL,
      stratify_variable       = NULL,

      # Explore stage — Trend tab
      trend_timestamp_variable = NULL,
      trend_variable           = NULL,
      trend_summary_stat       = "mean_sd",
      trend_resolution         = "Month",
      trend_stratify_variable  = NULL,
      trend_zero_baseline      = TRUE,
      trend_impute_zero        = TRUE,

      # Plot state
      plot_specification      = NULL,
      active_plot             = NULL,
      variable_summary        = NULL,
      explore_needs_refresh   = FALSE,

      # Aesthetics
      ggplot_theme            = "minimal",
      color_palette           = "Set2",
      show_data_labels        = FALSE,
      show_legend             = TRUE,
      legend_position         = "top",

      # Plot options (captured on plot button click, not reactive)
      bar_display             = "count",

      # Custom report
      custom_report_items     = list(),   # list of item objects added from Explore tab
      requested_tab           = NULL,     # cross-tab navigation signal
      requested_report_subtab = NULL,     # navigate to report pill (full_report / custom_report)

      # Prepare stage revert support
      last_applied_specs = list(
        included_columns       = names(dataset_cast),
        column_type_overrides  = list(),
        column_transform_specs = list(),
        row_filter_specs       = list()
      ),
      revert_trigger = 0L,              # incremented by .revert_to_last_applied(); modules observe

      # Analysis module fields — initialized as NULL; written to only by the
      # analysis modules (see PRD §3.3). Never read or modified by Prepare/Explore.
      analysis_data   = NULL,
      analysis_spec   = NULL,
      analysis_result = NULL,

      # Session load (§M8.7): the Analyze part of a loaded session, waiting for
      # Steps 1 and 4 to take it. NULL when idle. Written by the session module.
      session_restore = NULL
    )

    # ── Prepare tab navigation guard ──────────────────────────────────────────
    # Auto-applies pending changes when the user switches prepare sub-tabs.
    # Invalid transforms block navigation (pipeline would mangle the column).
    # If custom report items exist, shows a modal before applying.
    last_prepare_tab <- shiny::reactiveVal("columns")

    # Shared helper: run the pipeline and commit to shared_state.
    .do_nav_apply <- function() {
      df <- .run_prepare_apply(shared_state, on_error = function() {
        bslib::nav_select("prepare_tabs", last_prepare_tab())
      })
      if (is.null(df)) return()
      shiny::showNotification("Changes applied.", type = "message", duration = 2)
    }

    shiny::observeEvent(input$prepare_tabs, {
      if (isTRUE(shared_state$has_pending_changes)) {
        invalid <- .find_invalid_transforms(shared_state)
        if (length(invalid) > 0) {
          bslib::nav_select("prepare_tabs", last_prepare_tab())
          shiny::showNotification(
            paste0("Fix transforms before switching tabs: ",
                   paste(invalid, collapse = ", ")),
            type = "error", duration = 6
          )
          return()
        }
        # Guard: warn if custom report items exist (same guard as Apply button).
        n_items <- length(shiny::isolate(shared_state$custom_report_items))
        if (n_items > 0) {
          .custom_items_modal(n_items, "cancel_nav_apply_btn",
                              "confirm_nav_apply_btn", "Apply Changes",
                              "clear_nav_apply_btn")
          return()  # do NOT update last_prepare_tab - stays on old tab
        }
        .do_nav_apply()
      }
      last_prepare_tab(input$prepare_tabs)
    }, ignoreInit = TRUE)

    # Confirm: apply the pending changes and advance last_prepare_tab.
    shiny::observeEvent(input$confirm_nav_apply_btn, {
      shiny::removeModal()
      .do_nav_apply()
      last_prepare_tab(shiny::isolate(input$prepare_tabs))
    }, ignoreInit = TRUE)

    # Cancel: revert staged changes back to last-applied specs and return to
    # the previous tab (the tab navigation has already occurred in the DOM,
    # so we explicitly navigate back).
    shiny::observeEvent(input$cancel_nav_apply_btn, {
      shiny::removeModal()
      .revert_to_last_applied(shared_state)
      bslib::nav_select("prepare_tabs", last_prepare_tab())
    }, ignoreInit = TRUE)

    # Discard the queued items, then apply and let the navigation stand. Chosen
    # once, this stops the dialog firing on every later tab switch.
    shiny::observeEvent(input$clear_nav_apply_btn, {
      shiny::removeModal()
      .clear_custom_report_items(shared_state)
      .do_nav_apply()
      last_prepare_tab(shiny::isolate(input$prepare_tabs))
    }, ignoreInit = TRUE)

    # Light / dark mode needs no server code: bslib::input_dark_mode() sets
    # data-bs-theme on <html> in the browser, and every colour in edark.css is
    # a Bootstrap variable, so both modes follow from one stylesheet.

    # ── Debug button ──────────────────────────────────────────────────────────
     shiny::observeEvent(input$debug_btn, {
      if (!identical(input$main_navbar, "analyze")) return(invisible(NULL))
    
      step <- input[["analysis_main-analysis_steps"]]
      if (is.null(step)) step <- "step1"
      # Step 5 (Model) has sub-tabs: key on "step5_<subtab>"
      if (step == "step5") step <- paste0("step5_", input[["analysis_main-model_tabs"]] %||% "summary")
    
      .dbg <- function(label, x) {
        cat(sprintf("  [%s]\n", label), file = stderr())
        if (is.null(x)) cat("    <NULL>\n", file = stderr()) else str(x, max.level = 3, give.attr = FALSE, file = stderr())
      }
    
      spec   <- shiny::isolate(shared_state$analysis_spec)
      result <- shiny::isolate(shared_state$analysis_result)
      adata  <- shiny::isolate(shared_state$analysis_data)
    
      cat(sprintf("\n\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550 DEBUG \u00b7 %s \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\n", toupper(step)), file = stderr())
    
      if (step == "step1") {
        if (!is.null(adata)) {
          cat(sprintf("  [analysis_data] %d rows \u00d7 %d cols\n", nrow(adata), ncol(adata)), file = stderr())
          cat(sprintf("  cols: %s\n", paste(names(adata), collapse = ", ")), file = stderr())
        } else {
          cat("  [analysis_data] <NULL>\n", file = stderr())
        }
        .dbg("analysis_spec$variable_roles",                    spec$variable_roles)
        .dbg("analysis_spec$specification_metadata$study_type", spec$specification_metadata$study_type)
        .dbg("analysis_spec$variable_roles$reference_levels",   spec$variable_roles$reference_levels)
    
      } else if (step == "step2") {
        .dbg("analysis_result$result_tables$table1_overall",     result$result_tables$table1_overall)
        .dbg("analysis_result$result_tables$table1_by_exposure", result$result_tables$table1_by_exposure)
        .dbg("analysis_result$result_tables$table1_by_outcome",  result$result_tables$table1_by_outcome)
    
      } else if (step == "step3") {
        .dbg("analysis_result$variable_investigation",        result$variable_investigation)
        .dbg("analysis_spec$variable_selection_specification", spec$variable_selection_specification)
    
      } else if (step == "step4") {
        .dbg("analysis_spec$variable_roles$final_model_covariates", spec$variable_roles$final_model_covariates)
        .dbg("analysis_spec$variable_roles$reference_levels",       spec$variable_roles$reference_levels)
    
      } else if (step %in% c("step5_summary", "step5_create")) {
        .dbg("analysis_spec$model_design",             spec$model_design)
        .dbg("analysis_result$fitted_models$primary",  result$fitted_models$primary_model)
        .dbg("analysis_result$run_status",             result$run_status)
    
      } else if (step == "step5_diagnostics") {
        .dbg("analysis_result$result_plots$diagnostic_plots", result$result_plots$diagnostic_plots)
        .dbg("analysis_result$inference_summary",             result$inference_summary)
    
      } else if (step == "step5_performance") {
        .dbg("analysis_spec$purpose_specification", spec$purpose_specification)
        .dbg("analysis_spec$validation_settings",   spec$validation_settings)
        .dbg("analysis_result$performance",         result$performance)

      } else if (step == "step5_results") {
        .dbg("analysis_result$result_tables",    result$result_tables)
        .dbg("analysis_result$inference_summary", result$inference_summary)
      }
    
      cat("\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\n\n", file = stderr())
    }, ignoreInit = TRUE)

    # ── Cross-tab navigation (requested by modules via shared_state$requested_tab) ─
    shiny::observeEvent(shared_state$requested_tab, {
      shiny::req(!is.null(shared_state$requested_tab))
      tab <- shared_state$requested_tab
      if (tab == "report") {
        bslib::nav_select("main_navbar", "explore")
        bslib::nav_select("explore_tabs", "report")
      } else {
        bslib::nav_select("main_navbar", tab)
      }
      shared_state$requested_tab <- NULL
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # ── Session cleanup: remove thumbnail temp files on exit ──────────────────
    session$onSessionEnded(function() {
      paths <- vapply(shiny::isolate(shared_state$custom_report_items),
                      `[[`, character(1), "thumb_path")
      unlink(paths[file.exists(paths)])
    })

    # Wire all modules — each is a sibling, none calls another's server.
    column_manager_server("column_manager",         shared_state)
    transform_variables_server("transform_variables", shared_state)
    row_filter_server("row_filter",                 shared_state)
    data_preview_server("data_preview",             shared_state)
    prepare_confirm_server("prepare_confirm",       shared_state)
    explore_mode <- shiny::reactive(input$explore_mode)
    describe_controls_server("describe_controls",         shared_state, explore_mode)
    relationship_controls_server("relationship_controls", shared_state, explore_mode)
    trend_controls_server("trend_controls",               shared_state)
    appearance_controls_server("appearance_controls",     shared_state)
    explore_output_server("explore_output",   shared_state)
    report_server("report",                   shared_state)
    analysis_main_server("analysis_main",     shared_state)
    export_server("export",                   shared_state, dataset_input = dataset)
    # Last: a launch session is applied in the first flush, after every
    # module above has registered its observers.
    session_server("session", shared_state, dataset_input = dataset,
                   launch_session = launch_session)
  }

  shiny::shinyApp(ui = ui, server = server)
}
