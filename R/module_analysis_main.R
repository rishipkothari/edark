#' Analysis Module — Main Orchestrator
#'
#' UI and server for the Analysis stage (Tab 4 — \code{4 · Analyze}).
#' Renders a 5-step \code{navset_pill}; Step 5 (Model)
#' nests its own \code{navset_underline} — Summary, Create, Diagnostics,
#' Performance, Results — and delegates each step to its own
#' sub-module. Also registers the JS custom message handler used by the
#' blocking progress modal shared across all analysis run buttons.
#'
#' @param id Character. Module namespace ID.
#' @param shared_state A Shiny \code{reactiveValues} object (shared across
#'   all modules).
#'
#' @name module_analysis_main
NULL


# Labels of the nav items whose titles are rendered from the server, so a
# locked one can carry its own reason (see .nav_title()). They must read
# exactly as the nav shows them - the lock reasons name destinations this way.
.ANALYSIS_NAV_STEPS <- c(step5 = "5 \u00b7 Model")
.ANALYSIS_NAV_MODEL <- c(diagnostics = "Diagnostics", performance = "Performance",
                         results     = "Results")


#' @rdname module_analysis_main
#' @export
analysis_main_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    # JS handler — updates the progress bar and detail text inside the
    # blocking analysis modal (mirrors the pattern in module_report.R).
    shiny::tags$script(shiny::HTML(
      "Shiny.addCustomMessageHandler('edark_analysis_progress', function(msg) {",
      "  var bar = document.getElementById('edark_analysis_progress_bar');",
      "  if (bar) {",
      "    bar.style.width = (msg.frac * 100) + '%';",
      "    bar.setAttribute('aria-valuenow', Math.round(msg.frac * 100));",
      "  }",
      "  var txt = document.getElementById('edark_analysis_progress_detail');",
      "  if (txt) txt.textContent = msg.detail;",
      "});"
    )),

    bslib::navset_pill(
      id = ns("analysis_steps"),

      bslib::nav_panel(
        value = "step1",
        title = "1 \u00b7 Setup",
        analysis_setup_ui(ns("setup"))
      ),
      bslib::nav_panel(
        value = "step2",
        title = "2 \u00b7 Table 1",
        analysis_table1_ui(ns("table1"))
      ),
      bslib::nav_panel(
        value = "step3",
        title = "3 \u00b7 Variables",
        analysis_varinvestigation_ui(ns("varinvestigation"))
      ),
      bslib::nav_panel(
        value = "step4",
        title = "4 \u00b7 Covariates",
        analysis_covariate_confirm_ui(ns("covariate_confirm"))
      ),
      bslib::nav_panel(
        value = "step5",
        title = shiny::uiOutput(ns("title_step5"), inline = TRUE),
        bslib::navset_underline(
          id = ns("model_tabs"),
          bslib::nav_panel(
            value = "summary", title = "Summary",
            analysis_modelspec_summary_ui(ns("modelspec"))
          ),
          bslib::nav_panel(
            value = "create", title = "Create",
            analysis_modelspec_create_ui(ns("modelspec"))
          ),
          bslib::nav_panel(
            value = "diagnostics",
            title = shiny::uiOutput(ns("title_diagnostics"), inline = TRUE),
            analysis_diagnostics_ui(ns("diagnostics"))
          ),
          bslib::nav_panel(
            value = "performance",
            title = shiny::uiOutput(ns("title_performance"), inline = TRUE),
            analysis_performance_ui(ns("performance"))
          ),
          bslib::nav_panel(
            value = "results",
            title = shiny::uiOutput(ns("title_results"), inline = TRUE),
            analysis_results_ui(ns("results"))
          )
        )
      )
    )
  )
}


#' @rdname module_analysis_main
#' @export
analysis_main_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Wire all step sub-modules — each is a sibling, none calls another's server.
    analysis_setup_server("setup",                       shared_state)
    analysis_table1_server("table1",                     shared_state)
    analysis_varinvestigation_server("varinvestigation", shared_state)
    analysis_covariate_confirm_server("covariate_confirm", shared_state)
    analysis_modelspec_server("modelspec",               shared_state)
    analysis_diagnostics_server("diagnostics",           shared_state)
    analysis_performance_server("performance",           shared_state)
    analysis_results_server("results",                   shared_state)

    # ── Step gating ──────────────────────────────────────────────────────────
    # Steps 1–4 are always reachable (each shows its own guidance when Step 1
    # is incomplete). Table 1 and variable investigation are optional, so
    # Step 5 opens as soon as Step 1 has a frozen dataset and an outcome;
    # Model › Create's Run Model is gated only by its preflight. The Model
    # sub-tabs after Create need a fitted model. Export is not an Analyze
    # step: it is the top-level 4 · Export page (module_export.R).
    gate <- shiny::reactive({
      spec   <- shared_state$analysis_spec
      ready  <- !is.null(shared_state$analysis_data) && !is.null(spec) &&
                !is.null(spec$variable_roles$outcome_variable)
      fitted <- !is.null(shared_state$analysis_result$fitted_models$primary_model)
      list(
        steps = c(step1 = TRUE, step2 = TRUE, step3 = TRUE, step4 = TRUE,
                  step5 = ready),
        model = c(summary = TRUE, create = TRUE, diagnostics = fitted,
                  performance = fitted, results = fitted)
      )
    })

    # Which lock reason each gated destination shows. The strings live in
    # R/ui_helpers.R, so this popover, the in-panel placeholders and the
    # disabled run buttons cannot disagree.
    lock_key <- c(step5       = "analysis_outcome",
                  diagnostics = "fit_model",
                  performance = "fit_model",
                  results     = "fit_model")

    # A step is done when its output exists and is current. Stage 3 styles the
    # class; until then it is inert. Step 4 rewrites its covariate list on
    # every change, so done there means at least one covariate is confirmed.
    done <- shiny::reactive({
      spec <- shared_state$analysis_spec
      res  <- shared_state$analysis_result
      vi   <- res$variable_investigation
      c(step1 = !is.null(shared_state$analysis_data) &&
                !is.null(spec$variable_roles$outcome_variable),
        step2 = !is.null(res$result_tables$table1_overall),
        step3 = !is.null(vi$univariable) || !is.null(vi$stepwise) ||
                !is.null(vi$lasso),
        step4 = length(spec$variable_roles$final_model_covariates) > 0L,
        step5 = !is.null(res$fitted_models$primary_model) &&
                !analysis_fit_is_stale(spec, res))
    })

    # Nav titles come from the server so a locked item can explain itself: the
    # label gains a lock glyph and a popover holding the reason. The app CSS
    # keeps the click alive (Bootstrap's .nav-link.disabled would swallow it)
    # while Bootstrap still refuses to switch tabs, so the click only opens
    # the popover. Steps 1-4 and Model > Summary / Create are never locked, so
    # their titles stay static in the UI.
    .nav_title <- function(key, label, unlocked) {
      out_id <- paste0("title_", key)
      force(label)   # the renderUI below is lazy; capture this loop value now
      output[[out_id]] <- shiny::renderUI({
        if (isTRUE(unlocked()[[key]])) return(shiny::span(label))
        bslib::popover(
          shiny::span(shiny::icon("lock"), " ", label),
          edark_lock_reason(lock_key[[key]]),
          title = "Locked"
        )
      })
      # The pill header sits inside the Analyze panel, so it counts as hidden
      # until the stage is opened; render anyway so the state is right on
      # arrival rather than one flush later.
      shiny::outputOptions(output, out_id, suspendWhenHidden = FALSE)
    }

    steps_unlocked <- shiny::reactive(gate()$steps)
    model_unlocked <- shiny::reactive(gate()$model)
    for (k in names(.ANALYSIS_NAV_STEPS)) {
      .nav_title(k, .ANALYSIS_NAV_STEPS[[k]], steps_unlocked)
    }
    for (k in names(.ANALYSIS_NAV_MODEL)) {
      .nav_title(k, .ANALYSIS_NAV_MODEL[[k]], model_unlocked)
    }

    # The last selection that was allowed, per navset. Restoring the click
    # (see inst/www/edark.css) means Bootstrap does switch to a locked tab, so
    # we send the user back where they were - which also covers the case of a
    # step becoming locked while it is open.
    last_ok <- shiny::reactiveValues(analysis_steps = "step1", model_tabs = "summary")

    # Disable locked tab links, mark finished steps, and keep the selection on
    # a tab the user is allowed to be on.
    .apply_gate <- function(navset_id, unlocked, finished = logical(0)) {
      tabs_sel <- paste0("#", ns(navset_id))
      for (tab in names(unlocked)) {
        link <- sprintf("%s a[data-value='%s']", tabs_sel, tab)
        shinyjs::toggleClass(selector = link, class = "disabled",
                             condition = !unlocked[[tab]])
        shinyjs::toggleClass(selector = link, class = "edark-step-done",
                             condition = isTRUE(unname(finished[tab])))
      }

      # Read, do not isolate: this observer must also fire when the user
      # clicks a locked tab, not only when the gate changes.
      current <- input[[navset_id]]
      if (!is.null(current) && current %in% names(unlocked)) {
        if (unlocked[[current]]) {
          last_ok[[navset_id]] <- current
        } else {
          bslib::nav_select(navset_id, selected = last_ok[[navset_id]])
        }
      }
    }

    shiny::observe({
      g <- gate()
      .apply_gate("analysis_steps", g$steps, done())
      .apply_gate("model_tabs",     g$model)
    })
  })
}
