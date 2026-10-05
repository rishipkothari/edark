#' Analysis Step 3 — Variable Investigation Module
#'
#' UI and server for Step 3 of the Analysis workflow: univariable outcome
#' regression screen, collinearity assessment, stepwise selection, and LASSO.
#' Full implementation per PRD §5.3 Step 3, §6.5, §7.6–7.8, §8.5, §9.
#'
#' @param id Character. Module namespace ID.
#' @param shared_state A Shiny \code{reactiveValues} object.
#'
#' @importFrom magrittr %>%
#'
#' @name module_analysis_varinvestigation
NULL


# ── Shared helper: Tier 1 validation banner ───────────────────────────────────
# Returns tagList with red alert if Tier 1 errors; NULL if clean.
.tier1_banner_ui <- function(spec, data) {
  if (is.null(spec) || is.null(data)) {
    return(shiny::div(
      class = "alert alert-warning mb-2",
      shiny::icon("triangle-exclamation"),
      " Complete Step 1 (dataset freeze and role assignment) before running analysis."
    ))
  }
  val <- validate_analysis(spec, data, tier = "tier1")
  errors <- if (val$validity_flag == "invalid") {
    vapply(Filter(function(m) m$level == "error", val$display_messages),
           `[[`, character(1), "message")
  } else character(0)
  if (!.has_candidates(spec)) {
    errors <- c(errors,
      "No candidate covariates selected. Assign covariates in Step 1 to run variable investigation.")
  }
  if (length(errors) == 0L) return(NULL)
  shiny::div(
    class = "alert alert-danger mb-2",
    shiny::tags$ul(
      class = "mb-0 ps-3",
      lapply(errors, shiny::tags$li)
    )
  )
}


# TRUE when Step 1 assigned at least one candidate covariate
.has_candidates <- function(spec) {
  length(spec$variable_roles$univariable_test_pool) > 0L
}


# ── Shared helper: excluded-variables alert ───────────────────────────────────
# Amber alert listing candidates a selection method left out, with reasons.
# `excluded` is a data.frame(variable, reason); NULL / zero rows → NULL.
.excluded_vars_alert <- function(excluded, heading) {
  if (is.null(excluded) || nrow(excluded) == 0L) return(NULL)
  shiny::div(
    class = "alert alert-warning mb-2",
    shiny::tags$strong(shiny::icon("triangle-exclamation"), " ", heading),
    shiny::tags$ul(
      class = "mb-0 ps-3",
      lapply(seq_len(nrow(excluded)), function(i) {
        shiny::tags$li(shiny::tags$code(excluded$variable[i]),
                       " - ", excluded$reason[i])
      })
    )
  )
}


# ── Shared helper: blocking modal ─────────────────────────────────────────────
# `stops` (optional character vector) splits the bar into one equal segment
# per named step: a tick between segments and each name underneath. The caller
# moves the bar through the segments with the usual progress messages.
.analysis_progress_modal <- function(title_text, detail_text = "Running\u2026", cancel_id = NULL,
                                     stops = NULL) {
  n_stops <- length(stops)
  ticks <- if (n_stops > 1L) lapply(seq_len(n_stops - 1L), function(k) {
    shiny::tags$span(class = "edark-progress-tick",
                     style = sprintf("left: %.4f%%;", 100 * k / n_stops))
  })
  stop_labels <- if (n_stops > 1L) shiny::div(
    class = "edark-progress-stops mb-2",
    lapply(stops, function(s) shiny::tags$span(class = "text-truncate", title = s, s))
  )

  shiny::modalDialog(
    title = shiny::tagList(
      shiny::tags$span(
        class = "spinner-border spinner-border-sm me-2",
        role  = "status",
        shiny::tags$span(class = "visually-hidden", "Loading...")
      ),
      title_text
    ),
    shiny::div(
      class = if (n_stops > 1L) "progress edark-progress-staged mb-1" else "progress mb-2",
      style = "height: 6px;",
      shiny::div(
        id              = "edark_analysis_progress_bar",
        class           = "progress-bar progress-bar-striped progress-bar-animated",
        role            = "progressbar",
        style           = "width: 5%;",
        `aria-valuenow` = "5",
        `aria-valuemin` = "0",
        `aria-valuemax` = "100"
      ),
      ticks
    ),
    stop_labels,
    shiny::tags$p(
      id    = "edark_analysis_progress_detail",
      class = "text-muted mb-0 small",
      detail_text
    ),
    # A Cancel button only works when the caller runs its computation in
    # steps (see module_analysis_performance.R) — a single long call blocks
    # Shiny from seeing the click.
    footer    = if (!is.null(cancel_id)) {
      edark_button(NULL, cancel_id, "Cancel", variant = "secondary",
                   size = "toolbar")
    },
    easyClose = FALSE
  )
}


#' @rdname module_analysis_varinvestigation
#' @export
analysis_varinvestigation_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::navset_underline(
    id = ns("vi_pills"),

    # ── Pill 1: Univariable Screen ──────────────────────────────────────────
    bslib::nav_panel(
      value = "univariable",
      title = "Univariable Screen",

      edark_page(
        config = shiny::tagList(
          edark_section_label("P-value Threshold", first = TRUE),
          shiny::numericInput(
            ns("univ_threshold"), label = NULL,
            value = 0.2, min = 0.01, max = 1, step = 0.05, width = "120px"
          ),
          shiny::tags$p(
            class = "text-muted small mb-2",
            "Variables below this threshold are flagged as candidates."
          ),
          shiny::tags$hr(class = "my-2"),
          edark_run_button(ns, "btn_run_univariable", "Run Screen")
        ),
        result   = shiny::uiOutput(ns("univ_results_ui")),
        messages = shiny::uiOutput(ns("univ_banner")),
        info     = shiny::uiOutput(ns("univ_summary"))
      )
    ),

    # ── Pill 2: Collinearity ────────────────────────────────────────────────
    bslib::nav_panel(
      value = "collinearity",
      title = "Collinearity",

      edark_page(
        config = shiny::tagList(
          edark_section_label("Threshold", first = TRUE),
          shiny::tags$p(class = "small text-muted mb-0",
                        "Pairs correlated above 0.7 are flagged. The cut-off is fixed.")
        ),
        result   = shiny::uiOutput(ns("collin_results_ui")),
        messages = shiny::uiOutput(ns("collin_banner")),
        info     = shiny::uiOutput(ns("collin_summary"))
      )
    ),

    # ── Pill 3: Stepwise / LASSO ────────────────────────────────────────────
    bslib::nav_panel(
      value = "stepwise_lasso",
      title = "Stepwise / LASSO",

      edark_page(
        config = shiny::tagList(
          edark_section_label("Method", first = TRUE),
          shinyWidgets::radioGroupButtons(
            ns("sl_method"),
            label    = NULL,
            choices  = c("Stepwise", "LASSO"),
            selected = "Stepwise",
            size     = "sm",
            width    = "100%"
          ),

          shiny::uiOutput(ns("sl_config_ui")),

          shiny::tags$hr(class = "my-2"),
          edark_run_button(ns, "btn_run_sl", "Run Selection")
        ),
        result   = shiny::uiOutput(ns("sl_results_ui")),
        messages = shiny::uiOutput(ns("sl_banner")),
        info     = shiny::uiOutput(ns("sl_summary"))
      )
    )
  )
}


#' @rdname module_analysis_varinvestigation
#' @export
analysis_varinvestigation_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # ── Tier 1 validity (reactive, shared across pills) ───────────────────────
    # NULL when the step may run; otherwise the lock-reason key saying why not.
    tier1_block <- shiny::reactive({
      spec  <- shared_state$analysis_spec
      adata <- shared_state$analysis_data
      if (is.null(spec) || is.null(adata)) return("analysis_start")
      if (!.has_candidates(spec)) return("analysis_candidates")
      val <- validate_analysis(spec, adata, tier = "tier1")
      if (val$validity_flag == "invalid") return("analysis_tier1")
      NULL
    })
    tier1_valid <- shiny::reactive(is.null(tier1_block()))

    # Both run buttons share the one precondition, so they share its reason.
    tier1_reason <- shiny::reactive(edark_lock_reason(tier1_block()))
    edark_run_gate(output, "btn_run_univariable", tier1_valid, tier1_reason)
    edark_run_gate(output, "btn_run_sl",          tier1_valid, tier1_reason)

    # ── Collinearity: auto-compute on pill entry ──────────────────────────────
    collin_computed <- shiny::reactiveVal(NULL)

    .run_collinearity <- function() {
      spec  <- shiny::isolate(shared_state$analysis_spec)
      adata <- shiny::isolate(shared_state$analysis_data)
      if (is.null(spec) || is.null(adata)) return()

      candidates <- spec$variable_roles$univariable_test_pool
      if (is.null(candidates) || length(candidates) == 0L) return()

      result <- compute_collinearity(analysis_model_data(spec, adata), candidates)
      collin_computed(result)

      # Store in analysis_result
      res <- shiny::isolate(shared_state$analysis_result)
      if (is.null(res)) res <- list()
      if (is.null(res$result_plots)) res$result_plots <- list()
      if (is.null(res$result_plots$collinearity_plots)) {
        res$result_plots$collinearity_plots <- list()
      }
      res$result_plots$collinearity_plots$flagged_pairs_table <- result$flagged_pairs
      # The matrices too, so Export can draw the heatmaps without recomputing
      # (PRD/BUILD_Export.md X13). `[<-` keeps a NULL matrix as a NULL entry.
      res$result_plots$collinearity_plots["cor_matrix"]     <- list(result$cor_matrix)
      res$result_plots$collinearity_plots["cramers_v_matrix"] <- list(result$cramers_v_mat)
      shared_state$analysis_result <- res
    }

    shiny::observeEvent(input$vi_pills, {
      if (identical(input$vi_pills, "collinearity")) .run_collinearity()
    }, ignoreInit = TRUE)

    # A new train/test split changes the rows: recompute what is on screen
    split_key <- shiny::reactiveVal(NULL)
    shiny::observe(split_key(analysis_split(shared_state$analysis_spec)))
    shiny::observeEvent(split_key(), {
      collin_computed(NULL)
      if (identical(input$vi_pills, "collinearity")) .run_collinearity()
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    # ── UNIVARIABLE SCREEN ────────────────────────────────────────────────────

    output$univ_banner <- shiny::renderUI({
      .tier1_banner_ui(shared_state$analysis_spec, shared_state$analysis_data)
    })

    # Current p-value threshold. Falls back to the spec value while the numeric
    # input is blank, mid-edit, or out of range, so downstream renders never
    # see NA.
    univ_threshold <- shiny::reactive({
      thr <- input$univ_threshold
      if (is.null(thr) || is.na(thr) || thr <= 0 || thr > 1) {
        spec <- shiny::isolate(shared_state$analysis_spec)
        thr  <- spec$variable_selection_specification$univariable_p_threshold
      }
      if (is.null(thr)) 0.2 else thr
    })

    # Threshold change: persist to spec and re-flag the stored screen result
    # (no refit - p-values are unchanged, only the cut-off moves).
    shiny::observeEvent(univ_threshold(), {
      thr <- univ_threshold()

      spec <- shiny::isolate(shared_state$analysis_spec)
      if (!is.null(spec) &&
          !identical(spec$variable_selection_specification$univariable_p_threshold, thr)) {
        spec$variable_selection_specification$univariable_p_threshold <- thr
        shared_state$analysis_spec <- spec
      }

      res <- shiny::isolate(shared_state$analysis_result)
      tbl <- res$variable_investigation$univariable
      if (!is.null(tbl)) {
        new_suggested <- !is.na(tbl$p.value) & tbl$p.value < thr
        if (!identical(tbl$suggested, new_suggested)) {
          tbl$suggested <- new_suggested
          res$variable_investigation$univariable <- tbl
          res$result_tables$univariable_screen   <- tbl
          shared_state$analysis_result <- res
        }
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$btn_run_univariable, {
      spec  <- shiny::isolate(shared_state$analysis_spec)
      adata <- shiny::isolate(shared_state$analysis_data)
      if (is.null(spec) || is.null(adata)) return()

      thr <- univ_threshold()
      spec$variable_selection_specification$univariable_p_threshold <- thr

      shiny::showModal(.analysis_progress_modal("Running Univariable Screen\u2026"))
      on.exit(shiny::removeModal(), add = TRUE)

      screen_result <- run_univariable_screen(analysis_model_data(spec, adata), spec)

      # Store results
      res <- shiny::isolate(shared_state$analysis_result)
      if (is.null(res)) res <- list()
      if (is.null(res$variable_investigation)) res$variable_investigation <- list()
      if (is.null(res$result_tables)) res$result_tables <- list()

      res$variable_investigation$univariable          <- screen_result
      res$variable_investigation$univariable_excluded <- attr(screen_result, "excluded_variables")
      res$result_tables$univariable_screen             <- screen_result
      shared_state$analysis_result <- res

      # Persist threshold
      cur_spec <- shiny::isolate(shared_state$analysis_spec)
      if (!is.null(cur_spec)) {
        cur_spec$variable_selection_specification$univariable_p_threshold <- thr
        shared_state$analysis_spec <- cur_spec
      }
    }, ignoreInit = TRUE)

    output$univ_summary <- shiny::renderUI({
      res <- shared_state$analysis_result
      tbl <- res$variable_investigation$univariable
      if (is.null(tbl)) return(NULL)

      thr <- univ_threshold()

      # Count variables, not terms: a factor is suggested if any level passes
      n_tested  <- length(unique(tbl$variable))
      n_suggest <- length(unique(
        tbl$variable[!is.na(tbl$p.value) & tbl$p.value < thr]
      ))

      shiny::tagList(
        shiny::tags$hr(class = "my-2"),
        shiny::div(
          class = "d-flex justify-content-between mb-1",
          shiny::tags$span(class = "text-muted small", "Variables tested"),
          shiny::tags$span(class = "small fw-semibold", n_tested)
        ),
        shiny::div(
          class = "d-flex justify-content-between mb-1",
          shiny::tags$span(class = "text-muted small",
                           paste0("Variables suggested (p < ",
                                  format(thr, nsmall = 2L), ")")),
          shiny::tags$span(class = "small fw-semibold text-success", n_suggest)
        )
      )
    })

    output$univ_results_ui <- shiny::renderUI({
      res <- shared_state$analysis_result
      tbl <- res$variable_investigation$univariable
      if (is.null(tbl)) {
        return(shiny::div(
          class = "text-center text-muted mt-5",
          shiny::icon("magnifying-glass-chart", style = "font-size:2rem; opacity:0.3;"),
          shiny::tags$p(class = "mt-2", "Click \u201cRun Screen\u201d to run univariable regressions.")
        ))
      }
      shiny::tagList(
        .excluded_vars_alert(res$variable_investigation$univariable_excluded,
                             "Not tested:"),
        reactable::reactableOutput(ns("univ_table"))
      )
    })

    output$univ_table <- reactable::renderReactable({
      res <- shared_state$analysis_result
      tbl <- res$variable_investigation$univariable
      shiny::req(!is.null(tbl))

      thr       <- univ_threshold()
      sig       <- !is.na(tbl$p.value) & tbl$p.value < thr
      is_or     <- identical(tbl$effect_measure[1], "odds_ratio")
      est_label <- if (is_or) "Odds Ratio" else "Coefficient (\u03b2)"

      display <- data.frame(
        variable        = tbl$variable,
        term            = tbl$term,
        reference_level = if ("reference_level" %in% names(tbl)) tbl$reference_level else NA_character_,
        estimate        = tbl$estimate,
        conf.low        = if ("conf.low"  %in% names(tbl)) tbl$conf.low  else NA_real_,
        conf.high       = if ("conf.high" %in% names(tbl)) tbl$conf.high else NA_real_,
        p.value         = tbl$p.value,
        stringsAsFactors = FALSE
      )

      num_col <- function(name) {
        reactable::colDef(name = name, format = reactable::colFormat(digits = 3L),
                          na = "-", align = "right")
      }

      reactable::reactable(
        display,
        columns = list(
          variable        = reactable::colDef(name = "Variable", minWidth = 140),
          term            = reactable::colDef(name = "Term", minWidth = 140),
          reference_level = reactable::colDef(name = "Reference", na = "-"),
          estimate        = num_col(est_label),
          conf.low        = num_col("95% CI Low"),
          conf.high       = num_col("95% CI High"),
          p.value         = reactable::colDef(
            name  = "P-value",
            align = "right",
            cell  = function(value) edark_format_p(value)
          )
        ),
        # rowStyle index is the original data row, so highlighting survives sorting
        rowStyle   = function(index) {
          if (sig[index]) list(background = "rgba(25, 135, 84, 0.18)")
        },
        pagination = FALSE,
        height     = EDARK_RESULT_HEIGHT,
        highlight  = TRUE,
        compact    = TRUE
      )
    })

    # ── COLLINEARITY ──────────────────────────────────────────────────────────

    output$collin_banner <- shiny::renderUI({
      .tier1_banner_ui(shared_state$analysis_spec, shared_state$analysis_data)
    })

    output$collin_summary <- shiny::renderUI({
      coll <- collin_computed()
      if (is.null(coll)) return(NULL)

      n_flagged <- nrow(coll$flagged_pairs)

      shiny::tagList(
        shiny::tags$hr(class = "my-2"),
        shiny::div(
          class = "d-flex justify-content-between mb-1",
          shiny::tags$span(class = "text-muted small", "Numeric vars"),
          shiny::tags$span(class = "small fw-semibold", length(coll$num_vars))
        ),
        shiny::div(
          class = "d-flex justify-content-between mb-1",
          shiny::tags$span(class = "text-muted small", "Factor vars"),
          shiny::tags$span(class = "small fw-semibold", length(coll$fac_vars))
        ),
        shiny::div(
          class = "d-flex justify-content-between mb-1",
          shiny::tags$span(class = "text-muted small", "Flagged pairs (> 0.7)"),
          shiny::tags$span(
            class = if (n_flagged > 0) "small fw-semibold text-warning" else "small fw-semibold",
            n_flagged
          )
        )
      )
    })

    output$collin_results_ui <- shiny::renderUI({
      coll <- collin_computed()
      spec  <- shared_state$analysis_spec
      adata <- shared_state$analysis_data

      if (is.null(spec) || is.null(adata)) {
        return(shiny::div(
          class = "text-center text-muted mt-5",
          shiny::tags$p("Assign candidates in Step 1 first.")
        ))
      }

      if (!.has_candidates(spec)) {
        return(shiny::div(
          class = "text-center text-muted mt-5",
          shiny::tags$p("No candidate covariates selected. Assign covariates in Step 1.")
        ))
      }

      if (is.null(coll)) {
        return(shiny::div(
          class = "text-center text-muted mt-5",
          shiny::tags$span(
            class = "spinner-border spinner-border-sm me-2", role = "status"
          ),
          " Computing collinearity\u2026"
        ))
      }

      tabs <- list()

      if (!is.null(coll$cor_matrix) && ncol(coll$cor_matrix) >= 2L) {
        tabs[[length(tabs) + 1L]] <- bslib::nav_panel(
          "Correlation (Numeric)", shiny::plotOutput(ns("collin_heatmap"))
        )
      }
      if (!is.null(coll$cramers_v_mat) && ncol(coll$cramers_v_mat) >= 2L) {
        tabs[[length(tabs) + 1L]] <- bslib::nav_panel(
          "Cram\u00e9r's V (Factor)", shiny::plotOutput(ns("collin_cramers"))
        )
      }
      tabs[[length(tabs) + 1L]] <- bslib::nav_panel(
        "Flagged Pairs", reactable::reactableOutput(ns("collin_flagged"))
      )

      if (length(tabs) == 1L) {
        # Only flagged pairs (no matrices to show)
        tabs[[1L]]$children
      } else {
        do.call(bslib::navset_card_tab, tabs)
      }
    })

    # Heatmaps: the same builders the Explore report uses (service_analysis_plots.R)
    output$collin_heatmap <- shiny::renderPlot({
      coll <- collin_computed()
      shiny::req(!is.null(coll), !is.null(coll$cor_matrix))
      .plot_correlation_heatmap(coll$cor_matrix,
                                "Pearson Correlation Matrix (Numeric Candidates)")
    })

    output$collin_cramers <- shiny::renderPlot({
      coll <- collin_computed()
      shiny::req(!is.null(coll), !is.null(coll$cramers_v_mat))
      .plot_cramers_heatmap(coll$cramers_v_mat,
                            "Cram\u00e9r's V Matrix (Factor Candidates)")
    })

    # Flagged pairs table
    output$collin_flagged <- reactable::renderReactable({
      coll <- collin_computed()
      shiny::req(!is.null(coll))

      fp <- coll$flagged_pairs
      if (nrow(fp) == 0L) fp <- data.frame(
        var1 = "No pairs exceed threshold",
        var2 = "",
        type = "",
        value = NA_real_,
        stringsAsFactors = FALSE
      )

      reactable::reactable(
        fp,
        height = EDARK_RESULT_HEIGHT,
        columns = list(
          var1  = reactable::colDef(name = "Variable 1"),
          var2  = reactable::colDef(name = "Variable 2"),
          type  = reactable::colDef(name = "Metric"),
          value = reactable::colDef(
            name   = "Value",
            format = reactable::colFormat(digits = 3L)
          )
        ),
        pagination = FALSE,
        highlight  = TRUE,
        compact    = TRUE
      )
    })

    # ── STEPWISE / LASSO ─────────────────────────────────────────────────────

    output$sl_banner <- shiny::renderUI({
      .tier1_banner_ui(shared_state$analysis_spec, shared_state$analysis_data)
    })

    output$sl_config_ui <- shiny::renderUI({
      method <- input$sl_method
      if (is.null(method)) method <- "Stepwise"

      if (method == "Stepwise") {
        shiny::tagList(
          edark_section_label("Direction"),
          shiny::selectInput(
            ns("sw_direction"), label = NULL,
            choices  = c("Backward" = "backward", "Forward" = "forward"),
            selected = "backward"
          ),
          edark_section_label("Criterion"),
          shiny::selectInput(
            ns("sw_criterion"), label = NULL,
            choices  = c("BIC", "AIC"),
            selected = "BIC"
          )
        )
      } else {
        # Start from the stored settings (defaults on a fresh spec)
        spec <- shiny::isolate(shared_state$analysis_spec)
        vsel <- spec$variable_selection_specification
        shiny::tagList(
          edark_section_label("Lambda Selection"),
          shiny::selectInput(
            ns("lasso_lambda"), label = NULL,
            choices  = c(
              "lambda.1se (more regularized)" = "lambda.1se",
              "lambda.min (least error)"      = "lambda.min"
            ),
            selected = vsel$lasso_lambda %||% "lambda.1se"
          ),
          edark_section_label("Random Seed"),
          shiny::numericInput(
            ns("lasso_seed"), label = NULL,
            value = lasso_seed(spec), min = 1, max = .Machine$integer.max, step = 1
          ),
          shiny::tags$p(class = "small text-muted mt-n2",
                        "Fixes the random cross-validation folds so a run can be repeated.")
        )
      }
    })

    shiny::observeEvent(input$sw_direction, {
      new_criterion <- if (isTRUE(input$sw_direction == "forward")) "AIC" else "BIC"
      shiny::updateSelectInput(session, "sw_criterion", selected = new_criterion)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    shiny::observeEvent(input$btn_run_sl, {
      spec   <- shiny::isolate(shared_state$analysis_spec)
      adata  <- shiny::isolate(shared_state$analysis_data)
      method <- input$sl_method
      if (is.null(spec) || is.null(adata) || is.null(method)) return()

      # Update spec with current parameters
      if (method == "Stepwise") {
        spec$variable_selection_specification$stepwise_direction <- input$sw_direction
        spec$variable_selection_specification$stepwise_criterion <- input$sw_criterion
        modal_title <- sprintf("Running Stepwise (%s, %s)\u2026",
                               input$sw_direction, input$sw_criterion)
      } else {
        spec$variable_selection_specification$lasso_lambda <- input$lasso_lambda
        # An empty or invalid entry falls back to the stored / default seed
        seed_in <- suppressWarnings(as.integer(input$lasso_seed))
        if (length(seed_in) == 1L && !is.na(seed_in) && seed_in >= 1L) {
          spec$variable_selection_specification$lasso_seed <- seed_in
        }
        spec$variable_selection_specification$lasso_seed <- lasso_seed(spec)
        shiny::updateNumericInput(session, "lasso_seed",
                                  value = spec$variable_selection_specification$lasso_seed)
        modal_title <- sprintf("Running LASSO (%s, seed %d)\u2026", input$lasso_lambda,
                               spec$variable_selection_specification$lasso_seed)
      }

      shiny::showModal(.analysis_progress_modal(modal_title))
      on.exit(shiny::removeModal(), add = TRUE)

      sl_result <- if (method == "Stepwise") {
        run_stepwise(analysis_model_data(spec, adata), spec)
      } else {
        run_lasso(analysis_model_data(spec, adata), spec)
      }

      # Store in analysis_result — each method gets its own slot
      res <- shiny::isolate(shared_state$analysis_result)
      if (is.null(res)) res <- list()
      if (is.null(res$variable_investigation)) res$variable_investigation <- list()

      if (method == "Stepwise") {
        res$variable_investigation$stepwise <- sl_result
      } else {
        res$variable_investigation$lasso <- sl_result
      }
      shared_state$analysis_result <- res

      # Persist updated spec parameters
      cur_spec <- shiny::isolate(shared_state$analysis_spec)
      if (!is.null(cur_spec)) {
        cur_spec$variable_selection_specification <- spec$variable_selection_specification
        shared_state$analysis_spec <- cur_spec
      }
    }, ignoreInit = TRUE)

    output$sl_summary <- shiny::renderUI({
      method <- input$sl_method
      if (is.null(method)) return(NULL)
      res <- shared_state$analysis_result

      result <- if (method == "Stepwise") {
        res$variable_investigation$stepwise
      } else {
        res$variable_investigation$lasso
      }
      if (is.null(result)) return(NULL)

      n_sel <- length(result$selected_variables)
      shiny::tagList(
        shiny::tags$hr(class = "my-2"),
        if (!is.null(result$n_used)) {
          shiny::div(
            class = "d-flex justify-content-between mb-1",
            shiny::tags$span(class = "text-muted small", "Complete rows used"),
            shiny::tags$span(class = "small fw-semibold",
                             paste(result$n_used, "of", result$n_total))
          )
        },
        shiny::div(
          class = "d-flex justify-content-between mb-1",
          shiny::tags$span(class = "text-muted small", "Variables suggested"),
          shiny::tags$span(class = "small fw-semibold text-success", n_sel)
        )
      )
    })

    output$sl_results_ui <- shiny::renderUI({
      method <- input$sl_method
      if (is.null(method)) method <- "Stepwise"
      res <- shared_state$analysis_result

      result <- if (method == "Stepwise") {
        res$variable_investigation$stepwise
      } else {
        res$variable_investigation$lasso
      }

      if (is.null(result)) {
        label <- if (method == "Stepwise") "Stepwise" else "LASSO"
        return(shiny::div(
          class = "text-center text-muted mt-5",
          shiny::icon("list-check", style = "font-size:2rem; opacity:0.3;"),
          shiny::tags$p(class = "mt-2",
                        paste0("Click \u201cRun\u201d to run ", label, " selection."))
        ))
      }

      sel_vars <- result$selected_variables
      err      <- result$error

      shiny::tagList(
        if (!is.null(err)) {
          shiny::div(class = "alert alert-danger mb-3",
                     shiny::icon("circle-xmark"), " ", err)
        },
        .excluded_vars_alert(result$excluded_variables,
                             "Excluded from selection:"),
        if (length(result$held_variables) > 0L) {
          shiny::tags$p(
            class = "text-muted small mb-2",
            shiny::icon("thumbtack"),
            sprintf(" The exposure (%s) was held in every model, so covariates are",
                    paste(result$held_variables, collapse = ", ")),
            "chosen for what they add alongside it. It is not part of the selection."
          )
        },
        bslib::card(
          bslib::card_header("Suggested Variables"),
          bslib::card_body(
            if (length(sel_vars) == 0L) {
              shiny::tags$p(class = "text-muted", "No variables selected.")
            } else {
              shiny::tags$ul(
                class = "mb-0",
                lapply(sel_vars, function(v) shiny::tags$li(v))
              )
            }
          )
        ),
        if (method == "LASSO" && !is.null(result$coef_data) &&
            nrow(result$coef_data) > 0L) {
          bslib::card(
            class = "mt-2",
            bslib::card_header("Non-zero Coefficients"),
            bslib::card_body(
              reactable::reactable(
                result$coef_data %>%
                  dplyr::mutate(estimate = round(.data$estimate, 4L)) %>%
                  dplyr::select(Term = term, Estimate = estimate),
                compact    = TRUE,
                pagination = FALSE,
                highlight  = TRUE,
                height     = EDARK_RESULT_HEIGHT
              )
            )
          )
        },
        if (method == "Stepwise" && !is.null(result$step_trace)) {
          crit_label <- if (isTRUE(result$criterion == "BIC")) "BIC" else "AIC"
          direction_label <- paste0(
            toupper(substring(result$direction, 1L, 1L)),
            substring(result$direction, 2L)
          )
          trace_df <- result$step_trace
          if ("AIC" %in% names(trace_df))
            names(trace_df)[names(trace_df) == "AIC"] <- crit_label
          num_cols <- intersect(c(crit_label, "Df", "Sum of Sq", "RSS",
                                  "Deviance", "Resid. Df", "Resid. Dev"), names(trace_df))
          trace_df[num_cols] <- lapply(trace_df[num_cols], function(x) round(x, 2L))

          bslib::card(
            class = "mt-2",
            bslib::card_header("Selection Steps"),
            bslib::card_body(
              shiny::tags$p(
                class = "text-muted small mb-2",
                paste0(
                  direction_label, " stepwise selection (", crit_label, "). ",
                  "Each row shows a variable added or removed and the resulting ",
                  crit_label, " score. Lower ", crit_label,
                  " is better; a step is accepted when it reduces the score."
                )
              ),
              reactable::reactable(
                trace_df,
                compact    = TRUE,
                pagination = FALSE,
                highlight  = TRUE,
                height     = EDARK_RESULT_HEIGHT
              )
            )
          )
        }
      )
    })

  })
}
