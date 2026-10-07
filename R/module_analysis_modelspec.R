#' Analysis Step 5 — Model › Summary and Create
#'
#' UI and server for the first two sub-tabs of Step 5 (Model). The
#' orchestrator (\code{module_analysis_main.R}) places the two UI fragments —
#' \code{analysis_modelspec_summary_ui()} and
#' \code{analysis_modelspec_create_ui()} — in the Model sub-tabs, both under
#' the same namespace so one server drives them. Diagnostics, Performance and
#' Results are the other sub-tabs, each its own module.
#' \itemize{
#'   \item \strong{Summary} — read-only account of every step that led to the
#'     model, ending with all preflight checks (passes included). Built by
#'     \code{build_analysis_summary()}.
#'   \item \strong{Create} — sidebar with the model type (chosen from the
#'     outcome type and cluster roles; other types shown disabled with the
#'     reason), the optimizer (mixed models, under Advanced), a live compact
#'     preflight (errors and warnings), and Run Model; main panel with the
#'     formula, the "Modelling:" line, the training-set line when Step 1
#'     set a train/test split, and the results.
#' }
#'
#' The model type is written to the spec automatically — there is only ever
#' one valid choice. Preflight re-runs on every spec change. Clicking the
#' disabled Run Model button pulses the preflight box. Changing the optimizer
#' after a fit asks, then clears the model and everything downstream
#' (\code{reset_analysis_pipeline(from_step = 5)}).
#'
#' With a train/test split (Step 1, prediction purpose) the model is fitted
#' to the training rows only; the test rows are used in Model › Performance.
#'
#' @param id Character. Module namespace ID.
#' @param shared_state A Shiny \code{reactiveValues} object.
#'
#' @importFrom magrittr %>%
#'
#' @name module_analysis_modelspec
NULL


.ms_js <- function(ns) {
  shiny::tagList(
    # Bootstrap gives disabled buttons pointer-events: none, so a click on the
    # disabled Run Model lands on its wrapper — pulse the preflight box then.
    shiny::tags$script(shiny::HTML(paste0("
(function() {
  var NS = '", ns(""), "';
  $(document).on('click', '#' + NS + 'run_wrap', function() {
    var btn = document.getElementById(NS + 'btn_run');
    var box = document.getElementById(NS + 'preflight_box');
    if (!btn || !btn.disabled || !box) return;
    box.classList.remove('edark-pulse');
    void box.offsetWidth;
    box.classList.add('edark-pulse');
  });
})();
")))
  )
}


#' @rdname module_analysis_modelspec
#' @export
analysis_modelspec_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    .ms_js(ns),
    # Summary is an audit view: it has no settings of its own, which is why it
    # had no sidebar at all before Stage 4. D6 keeps the page shape anyway, so
    # the config pane says where each part of what is shown is actually set -
    # useful on a page whose whole job is "is this the model I meant?".
    edark_page(
      config = shiny::tagList(
        edark_section_label("Nothing to set here", first = TRUE),
        shiny::tags$p(
          class = "small text-muted",
          "This page reports the model that Steps 1 to 4 have specified. To
           change any of it, go back to the step that owns it:"
        ),
        shiny::tags$ul(
          class = "small text-muted ps-3 mb-0",
          shiny::tags$li(shiny::tags$strong("Step 1 \u00b7 Setup"),
                         " - outcome, exposure, clusters, study type, purpose, split"),
          shiny::tags$li(shiny::tags$strong("Step 3 \u00b7 Variable Investigation"),
                         " - which covariates are suggested"),
          shiny::tags$li(shiny::tags$strong("Step 4 \u00b7 Covariate Confirmation"),
                         " - the final covariates and their reference levels"),
          shiny::tags$li(shiny::tags$strong("Model \u203a Create"),
                         " - model type, optimizer, and fitting")
        )
      ),
      result = shiny::uiOutput(ns("summary_ui")),
      info   = shiny::uiOutput(ns("summary_info_ui"))
    )
  )
}


#' @rdname module_analysis_modelspec
#' @export
analysis_modelspec_create_ui <- function(id) {
  ns <- shiny::NS(id)

  edark_page(
    config = shiny::tagList(
      edark_section_label("Model", first = TRUE),
      shiny::uiOutput(ns("model_select_ui")),
      shiny::uiOutput(ns("advanced_ui")),
      edark_section_label("Preflight"),
      shiny::div(id = ns("preflight_box"), class = "p-1",
                 shiny::uiOutput(ns("preflight_ui"))),
      shiny::div(
        id = ns("run_wrap"), class = "mt-3",
        edark_run_button(ns, "btn_run", "Run Model")
      )
    ),
    # The R script for the analysis is on 4 · Export › R Code (PRD §A7.9)
    result = shiny::uiOutput(ns("results_ui")),
    info = shiny::uiOutput(ns("model_header_ui"))
  )
}


#' @rdname module_analysis_modelspec
#' @export
analysis_modelspec_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # ── Model options → spec (automatic: exactly one valid type) ─────────────
    opts <- shiny::reactive({
      spec  <- shared_state$analysis_spec
      adata <- shared_state$analysis_data
      if (is.null(spec) || is.null(adata)) return(NULL)
      analysis_model_options(spec, adata)
    })

    shiny::observe({
      o <- opts()
      if (is.null(o)) return()
      rec  <- attr(o, "recommended")
      spec <- shiny::isolate(shared_state$analysis_spec)
      if (is.null(spec$model_design)) spec$model_design <- .default_model_design()
      if (!identical(spec$model_design$model_type, rec)) {
        spec$model_design["model_type"] <- list(rec)
        shared_state$analysis_spec <- spec
      }
    })

    # reactiveVals only invalidate on a real change, so the select and the
    # optimizer input are not rebuilt on every unrelated spec write.
    opts_view <- shiny::reactiveVal(NULL)
    shiny::observe(opts_view(opts()))
    is_mixed <- shiny::reactiveVal(FALSE)
    shiny::observe({
      mt <- shared_state$analysis_spec$model_design$model_type
      is_mixed(isTRUE(mt %in% c("linear_mixed", "logistic_mixed")))
    })

    # ── Preflight (live) ─────────────────────────────────────────────────────
    validation <- shiny::reactive({
      spec  <- shared_state$analysis_spec
      adata <- shared_state$analysis_data
      if (is.null(spec) || is.null(adata)) return(NULL)
      validate_analysis(spec, adata, tier = "full", verbose = TRUE)
    })

    # NULL when the model may be fitted; otherwise the lock-reason key.
    run_block <- shiny::reactive({
      v    <- validation()
      spec <- shared_state$analysis_spec
      if (is.null(v)) return("analysis_start")
      if (is.null(spec$model_design$model_type)) return("model_type_none")
      if (v$validity_flag == "invalid") return("model_preflight")
      NULL
    })
    can_run <- shiny::reactive(is.null(run_block()))

    edark_run_gate(output, "btn_run", can_run,
                   shiny::reactive(edark_lock_reason(run_block())))

    # ── Sidebar: model select ────────────────────────────────────────────────
    output$model_select_ui <- shiny::renderUI({
      o <- opts_view()
      if (is.null(o)) {
        return(shiny::tags$p(class = "small text-muted", edark_lock_reason("analysis_start")))
      }
      rec <- attr(o, "recommended")
      labels <- ifelse(o$available, o$label, paste0(o$label, " - ", o$reason))
      sel <- shiny::selectInput(
        ns("model_type"), label = NULL,
        choices  = stats::setNames(o$model_type, labels),
        selected = if (is.null(rec)) character(0) else rec,
        selectize = FALSE, width = "100%"
      )
      for (mt in o$model_type[!o$available]) sel <- .disable_choice(sel, mt)
      shiny::tagList(
        sel,
        shiny::tags$p(
          class = "small text-muted mb-1",
          if (is.null(rec)) {
            "No model fits this outcome. Models need a numeric outcome or a two-level factor."
          } else {
            "Chosen from the outcome type and whether cluster variables are assigned in Step 1."
          }
        )
      )
    })

    # ── Sidebar: advanced (mixed only) ───────────────────────────────────────
    output$advanced_ui <- shiny::renderUI({
      if (!is_mixed()) return(NULL)
      current <- shiny::isolate(shared_state$analysis_spec$model_design$optimizer) %||% "bobyqa"
      bslib::accordion(
        open = FALSE, class = "mb-2",
        bslib::accordion_panel(
          "Advanced",
          shiny::selectInput(ns("optimizer"), "Optimizer",
                             choices = .ANALYSIS_OPTIMIZERS, selected = current,
                             selectize = FALSE, width = "100%"),
          shiny::tags$p(
            class = "small text-muted mb-0",
            "The search method used to estimate a mixed model. bobyqa almost always works.",
            "If you see a convergence warning, re-run with another optimizer - if the",
            "estimates agree, the warning can usually be ignored."
          )
        )
      )
    })

    # Any Step 5 setting change after a fit clears the model and everything
    # built on it (diagnostics, results) — ask first; Cancel puts the
    # dropdown back.
    .write_optimizer <- function(value) {
      spec <- shiny::isolate(shared_state$analysis_spec)
      spec$model_design$optimizer <- value
      shared_state$analysis_spec <- spec
    }

    shiny::observeEvent(input$optimizer, {
      spec <- shiny::isolate(shared_state$analysis_spec)
      if (is.null(spec) || !input$optimizer %in% .ANALYSIS_OPTIMIZERS) return()
      if (identical(spec$model_design$optimizer, input$optimizer)) return()

      res <- shiny::isolate(shared_state$analysis_result)
      if (is.null(res$fitted_models$primary_model)) {
        .write_optimizer(input$optimizer)
        return()
      }
      shiny::showModal(shiny::modalDialog(
        title = "Clear Model Results?",
        shiny::p("A model has already been fitted with the current optimizer.",
                 "Changing it will clear the fitted model, diagnostics and results.",
                 "Table 1, variable investigation and your covariates are kept."),
        shiny::tags$small(class = "text-muted",
                          "Cancel keeps the current optimizer and results."),
        footer = shiny::tagList(
          edark_button(ns, "cancel_optimizer", "Cancel", variant = "secondary", size = "dialog"),
          edark_button(ns, "confirm_optimizer", "Clear & Continue", variant = "warning", size = "dialog")
        ),
        easyClose = FALSE
      ))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$confirm_optimizer, {
      shiny::removeModal()
      reset_analysis_pipeline(shared_state, from_step = 5L)
      .write_optimizer(input$optimizer)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$cancel_optimizer, {
      shiny::removeModal()
      current <- shiny::isolate(shared_state$analysis_spec$model_design$optimizer) %||% "bobyqa"
      shiny::updateSelectInput(session, "optimizer", selected = current)
    }, ignoreInit = TRUE)

    # ── Sidebar: compact preflight ───────────────────────────────────────────
    output$preflight_ui <- shiny::renderUI({
      v <- validation()
      if (is.null(v)) return(NULL)
      shown <- Filter(function(m) m$level %in% c("error", "warning"), v$messages)
      if (length(shown) == 0L) {
        return(shiny::div(class = "small text-success",
                          shiny::icon("circle-check"), " All preflight checks passed."))
      }
      ord <- order(match(vapply(shown, `[[`, character(1), "level"), c("error", "warning")))
      shiny::tagList(
        lapply(shown[ord], function(m) .ms_check_item(m$level, m$message)),
        shiny::tags$p(class = "small text-muted mt-1 mb-0",
                      "Every check, including passes, is listed in the Summary tab.")
      )
    })


    # ── Run ──────────────────────────────────────────────────────────────────
    shiny::observeEvent(input$btn_run, {
      spec  <- shiny::isolate(shared_state$analysis_spec)
      adata <- shiny::isolate(shared_state$analysis_data)
      v     <- validate_analysis(spec, adata, tier = "full")
      if (is.null(spec$model_design$model_type) ||
          v$validity_flag == "invalid") return()   # the button is disabled

      label <- .ANALYSIS_MODEL_LABELS[[spec$model_design$model_type]]
      shiny::showModal(.analysis_progress_modal(paste0("Fitting ", label, "\u2026")))
      on.exit(shiny::removeModal(), add = TRUE)
      session$sendCustomMessage("edark_analysis_progress",
                                list(frac = 0.3, detail = "Fitting the model\u2026"))

      fit <- fit_analysis_model(spec, adata)

      session$sendCustomMessage("edark_analysis_progress",
                                list(frac = 0.9, detail = "Storing results\u2026"))

      # A new fit replaces the old one and everything built on it
      reset_analysis_pipeline(shared_state, from_step = 4L)
      res <- shiny::isolate(shared_state$analysis_result)
      if (is.null(res)) res <- list()
      res$specification_snapshot <- spec
      res$run_status <- list(
        status           = fit$status,
        fitted_at        = Sys.time(),
        error            = fit$error,
        n_used           = fit$n_used,
        n_total          = fit$n_total,
        formula          = fit$formula,
        outcome_event    = fit$outcome_event,
        reference_levels = fit$reference_levels,
        run_messages     = fit$messages,
        preflight        = Filter(function(m) m$level == "warning", v$messages)
      )
      if (is.null(res$fitted_models)) res$fitted_models <- list()
      res$fitted_models["primary_model"] <- list(fit$model)
      res$inference_summary <- list(
        coefficients       = fit$coefficients,
        fit_statistics     = fit$fit_statistics,
        predicted_values   = fit$predicted_values,
        influence_measures = NULL
      )
      shared_state$analysis_result <- res

      if (identical(fit$status, "success")) {
        shiny::showNotification(sprintf("%s fitted on %d rows.", label, fit$n_used),
                                type = "message", duration = 4)
      } else {
        shiny::showNotification(fit$error, type = "error", duration = 8)
      }
    }, ignoreInit = TRUE)

    # ── Main: model header ───────────────────────────────────────────────────
    output$model_header_ui <- shiny::renderUI({
      spec  <- shared_state$analysis_spec
      adata <- shared_state$analysis_data
      if (is.null(spec) || is.null(adata) || is.null(spec$variable_roles$outcome_variable)) {
        return(.ms_placeholder(edark_lock_reason("analysis_outcome")))
      }
      mt <- spec$model_design$model_type
      ev <- analysis_outcome_event(spec, adata)
      fmla <- if (!is.null(mt)) paste(deparse(build_analysis_formula(spec), width.cutoff = 500L), collapse = " ")
      sr   <- analysis_split_rows(spec, adata)

      edark_model_header(
        title  = if (is.null(mt)) "No model available" else .ANALYSIS_MODEL_LABELS[[mt]],
        fields = list(
          Modelling = if (!is.null(ev)) shiny::tagList(
            shiny::tags$strong(sprintf("%s = %s", ev$variable, ev$event)),
            sprintf(" (vs %s)", ev$reference)
          ),
          `Fitted on` = if (!is.null(sr))
            sprintf("training set, %s = %s (%d rows); %d test rows held out for Performance",
                    sr$variable, sr$training_level, sum(sr$training), sum(sr$test)),
          Formula = if (!is.null(fmla)) shiny::tags$code(fmla)
        )
      )
    })

    # ── Main: results ────────────────────────────────────────────────────────
    output$results_ui <- shiny::renderUI({
      res  <- shared_state$analysis_result
      spec <- shared_state$analysis_spec
      rs   <- res$run_status
      if (is.null(rs$status)) {
        return(.ms_placeholder("Run the model to see results."))
      }
      if (identical(rs$status, "failed")) {
        return(shiny::tagList(
          shiny::div(class = "alert alert-danger mt-3",
                     shiny::icon("circle-xmark"), " ", rs$error),
          .ms_notes_card(rs)
        ))
      }

      coefs  <- res$inference_summary$coefficients
      fstats <- res$inference_summary$fit_statistics
      snap   <- res$specification_snapshot
      mt     <- snap$model_design$model_type
      logit  <- mt %in% c("logistic", "logistic_mixed")
      exposure <- snap$variable_roles$exposure_variable

      shiny::tagList(
        if (analysis_fit_is_stale(spec, res)) {
          shiny::div(class = "alert alert-warning mt-3 mb-0 py-2",
                     shiny::icon("triangle-exclamation"),
                     " The specification has changed since this model was fitted. Run the model again.")
        },
        .ms_primary_card(coefs, exposure, rs, mt, logit),
        .ms_coef_card(coefs, rs$reference_levels, mt, logit),
        bslib::layout_columns(
          col_widths = c(6, 6), class = "mt-3",
          .ms_stats_card(fstats),
          .ms_notes_card(rs)
        )
      )
    })

    # ── Summary tab ──────────────────────────────────────────────────────────
    output$summary_ui <- shiny::renderUI({
      spec  <- shared_state$analysis_spec
      adata <- shared_state$analysis_data
      res   <- shared_state$analysis_result
      if (is.null(spec) || is.null(adata)) {
        return(.ms_placeholder(edark_lock_reason("analysis_start")))
      }
      sections <- build_analysis_summary(spec, res, adata, validation())
      shiny::tagList(lapply(sections, .ms_section_card))
    })

    # Info pane: the scalar facts about what Steps 1-4 have specified, so the
    # audit view in the centre does not have to restate them (D2).
    output$summary_info_ui <- shiny::renderUI({
      spec  <- shared_state$analysis_spec
      adata <- shared_state$analysis_data
      res   <- shared_state$analysis_result
      if (is.null(spec) || is.null(adata)) {
        return(shiny::tags$p(class = "small text-muted",
                             edark_lock_reason("analysis_start")))
      }

      roles <- spec$variable_roles
      mt    <- spec$model_design$model_type
      fitted <- !is.null(res$fitted_models$primary_model)

      shiny::tagList(
        edark_section_label("This model", first = TRUE),
        edark_info_row("Model type",
                       if (is.null(mt)) "-" else .ANALYSIS_MODEL_LABELS[[mt]]),
        edark_info_row("Outcome",  roles$outcome_variable  %||% "-"),
        edark_info_row("Exposure", roles$exposure_variable %||% "-"),
        edark_info_row("Covariates", length(spec$final_covariates %||% character(0))),
        edark_info_row("Clusters", length(roles$cluster_variables %||% character(0))),

        edark_section_label("Sample"),
        edark_info_row("Rows in analysis set", format(nrow(adata), big.mark = ",")),
        edark_info_row("Complete cases",
                       tryCatch({
                         vars <- c(roles$outcome_variable, roles$exposure_variable,
                                   spec$final_covariates,
                                   if (isTRUE(grepl("_mixed$", mt))) roles$cluster_variables)
                         cc <- compute_complete_cases(analysis_model_data(spec, adata), vars)
                         format(nrow(cc$data), big.mark = ",")
                       }, error = function(e) "-")),

        edark_section_label("Status"),
        edark_info_row("Fitted",
                       if (fitted) shiny::tags$span(class = "text-success", "yes")
                       else shiny::tags$span(class = "text-muted", "not yet"))
      )
    })
  })
}


# ── Rendering helpers ─────────────────────────────────────────────────────────

.ms_level_icon <- function(level) {
  cfg <- switch(level %||% "",
    error   = list("circle-xmark",         "text-danger"),
    warning = list("triangle-exclamation", "text-warning"),
    note    = list("circle-info",          "text-info"),
    pass    = list("circle-check",         "text-success"),
    NULL)
  if (is.null(cfg)) return(NULL)
  shiny::span(class = cfg[[2]], shiny::icon(cfg[[1]]))
}

.ms_check_item <- function(level, msg) {
  shiny::div(class = "d-flex gap-2 small mb-1", .ms_level_icon(level), shiny::span(msg))
}

.ms_placeholder <- function(text) {
  shiny::div(class = "text-center text-muted mt-5",
             shiny::icon("chart-line", style = "font-size:2rem; opacity:0.3;"),
             shiny::tags$p(class = "mt-2 fst-italic", text))
}

.ms_section_card <- function(section) {
  bslib::card(
    class = "mb-3",
    bslib::card_header(section$title),
    bslib::card_body(
      class = "py-2",
      lapply(section$rows, function(r) {
        value <- if (identical(r$label, "Formula")) shiny::tags$code(r$value) else r$value
        shiny::div(
          class = "edark-summary-row small",
          shiny::div(class = "edark-summary-label", r$label),
          shiny::div(
            class = "edark-summary-value",
            shiny::div(class = "d-flex gap-2", .ms_level_icon(r$level), shiny::span(value)),
            if (length(r$items) > 0L) {
              shiny::tags$ul(class = "mb-0 mt-1 ps-3", lapply(r$items, shiny::tags$li))
            }
          )
        )
      })
    )
  )
}

.ms_fmt_p <- function(p) {
  if (is.null(p)) return("-")
  edark_format_p(p)
}

.ms_fmt_est <- function(x) edark_format_est(x)

.ms_term_label <- function(row, refs) {
  if (is.na(row$level)) return(row$variable)
  ref <- refs[[row$variable]]
  paste0(row$variable, ": ", row$level, if (!is.null(ref)) paste0(" vs ", ref))
}

.ms_primary_card <- function(coefs, exposure, rs, mt, logit) {
  measure <- if (logit) "OR" else "\u03b2"
  n_excl  <- rs$n_total - rs$n_used
  rows <- if (!is.null(coefs) && !is.null(exposure)) coefs[coefs$variable %in% exposure, , drop = FALSE]

  body <- if (is.null(exposure)) {
    shiny::tags$p(class = "small text-muted mb-0",
                  "No exposure assigned (risk-factor study) - all estimates are in the table below.")
  } else if (is.null(rows) || nrow(rows) == 0L) {
    shiny::tags$p(class = "small text-warning mb-0", "The exposure has no estimate in this model.")
  } else {
    shiny::tagList(lapply(seq_len(nrow(rows)), function(i) {
      r <- rows[i, ]
      what <- if (is.na(r$level)) {
        sprintf("%s (per 1-unit increase)", r$variable)
      } else {
        .ms_term_label(r, rs$reference_levels)
      }
      shiny::div(
        class = "mb-1",
        shiny::div(class = "small text-muted", what),
        shiny::div(
          shiny::tags$strong(sprintf("%s %s", measure, .ms_fmt_est(r$effect))),
          sprintf("  (95%% CI %s \u2013 %s)", .ms_fmt_est(r$effect.low), .ms_fmt_est(r$effect.high)),
          shiny::span(class = "ms-2", sprintf("p %s", {
            p <- .ms_fmt_p(r$p.value); if (startsWith(p, "<")) p else paste("=", p)
          }))
        )
      )
    }))
  }

  bslib::card(
    class = "mt-3",
    bslib::card_header(
      class = "d-flex justify-content-between",
      shiny::span("Primary result"),
      shiny::span(class = "small text-muted",
                  sprintf("N analysed %d%s", rs$n_used,
                          if (n_excl > 0) sprintf(" (%d excluded - missing data)", n_excl) else ""))
    ),
    bslib::card_body(body)
  )
}

.ms_coef_card <- function(coefs, refs, mt, logit) {
  if (is.null(coefs)) return(NULL)
  tbl <- coefs[coefs$variable != "(Intercept)", , drop = FALSE]
  if (nrow(tbl) == 0L) return(NULL)
  measure <- if (logit) "Odds ratio" else "Estimate (\u03b2)"
  df <- data.frame(
    Term     = vapply(seq_len(nrow(tbl)), function(i) .ms_term_label(tbl[i, ], refs), character(1)),
    Estimate = vapply(tbl$effect, .ms_fmt_est, character(1)),
    CI       = sprintf("%s \u2013 %s", vapply(tbl$effect.low, .ms_fmt_est, character(1)),
                       vapply(tbl$effect.high, .ms_fmt_est, character(1))),
    p        = vapply(tbl$p.value, .ms_fmt_p, character(1)),
    stringsAsFactors = FALSE
  )
  bslib::card(
    class = "mt-3",
    bslib::card_header("Coefficients"),
    bslib::card_body(
      reactable::reactable(
        df, compact = TRUE, pagination = FALSE, highlight = TRUE,
        height = EDARK_RESULT_HEIGHT,
        columns = list(
          Term     = reactable::colDef(minWidth = 220),
          Estimate = reactable::colDef(name = measure, align = "right"),
          CI       = reactable::colDef(name = "95% CI", align = "right"),
          p        = reactable::colDef(name = "p-value", align = "right")
        )
      ),
      shiny::tags$p(class = "small text-muted mt-2 mb-0", edark_inference_note(mt))
    )
  )
}

.ms_fmt_stat <- function(value, format) {
  switch(format,
    integer = format(round(value), big.mark = ","),
    percent = sprintf("%.1f%%", value * 100),
    pvalue  = .ms_fmt_p(value),
    if (abs(value) >= 100) sprintf("%.1f", value) else sprintf("%.3f", value))
}

.ms_stats_card <- function(fstats) {
  bslib::card(
    bslib::card_header("Fit statistics"),
    bslib::card_body(
      class = "py-2",
      if (is.null(fstats) || nrow(fstats) == 0L) {
        shiny::tags$p(class = "small text-muted mb-0", "Not available.")
      } else {
        lapply(seq_len(nrow(fstats)), function(i) {
          shiny::div(class = "d-flex justify-content-between small mb-1",
                     shiny::span(class = "text-muted", fstats$label[i]),
                     shiny::span(class = "fw-semibold",
                                 .ms_fmt_stat(fstats$value[i], fstats$format[i])))
        })
      }
    )
  )
}

.ms_notes_card <- function(rs) {
  rm   <- rs$run_messages
  pf   <- rs$preflight
  items <- c(
    lapply(pf, function(m) list(level = "warning", message = m$message)),
    if (!is.null(rm) && nrow(rm) > 0L) {
      rm <- rm[rm$level != "error", , drop = FALSE]
      lapply(seq_len(nrow(rm)), function(i) list(level = rm$level[i], message = rm$message[i]))
    }
  )
  bslib::card(
    bslib::card_header("Fitting notes"),
    bslib::card_body(
      class = "py-2",
      if (length(items) == 0L) {
        shiny::tags$p(class = "small text-muted mb-0", "None.")
      } else {
        lapply(items, function(m) .ms_check_item(m$level, m$message))
      }
    )
  )
}
