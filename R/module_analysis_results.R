#' Analysis Step 5 — Model › Results Module
#'
#' UI and server for the Results sub-tab of Step 5 (Model). The sidebar lists the
#' outputs that can be generated — results table (with an optional
#' unadjusted column), fit statistics, forest plot, methods paragraph — and
#' one Generate Outputs button. Ticked outputs are created and stored in
#' \code{analysis_result}; unticked ones are not created, so the Export page cannot
#' export them. The Summary tab (key numbers, no prose) is always shown and
#' reads what already exists.
#'
#' Stored: \code{result_tables$main_results} (\code{build_results_table()}),
#' \code{result_tables$fit_statistics}, \code{result_plots$coefficient_plot},
#' \code{methods_paragraph}, \code{fitted_models$univariable_models} and
#' \code{results_generation} (what was generated, when). Every value comes
#' from \code{stats_inference.R} via the services — no statistics here.
#'
#' @param id Character. Module namespace ID.
#' @param shared_state A Shiny \code{reactiveValues} object.
#'
#' @importFrom magrittr %>%
#'
#' @name module_analysis_results
NULL


# The outputs a user can choose to generate. Add new outputs here (and a
# branch in the Generate handler and a tab in output$tabs_ui).
.RESULTS_OUTPUTS <- list(
  list(id = "results_table", label = "Results table",
       description = "Estimate (95% CI) and p for every model variable"),
  list(id = "fit_statistics", label = "Fit statistics",
       description = "Model fit, plus the AUC if Performance computed it"),
  list(id = "forest_plot", label = "Forest plot",
       description = "Adjusted estimates for every model term"),
  list(id = "methods", label = "Methods paragraph",
       description = "Text for the statistical methods section, with software versions")
)


#' @rdname module_analysis_results
#' @export
analysis_results_ui <- function(id) {
  ns <- shiny::NS(id)
  .choice <- function(o) {
    shiny::checkboxInput(
      ns(paste0("out_", o$id)), value = TRUE, width = "100%",
      label = shiny::span(o$label, shiny::tags$br(),
                          shiny::span(class = "small text-muted", o$description)))
  }

  edark_page(
    config = shiny::tagList(
      edark_section_label("Outputs", first = TRUE),
      .choice(.RESULTS_OUTPUTS[[1]]),
      shiny::conditionalPanel(
        condition = "input.out_results_table", ns = ns,
        shiny::div(class = "ms-4 mt-n2",
                   shiny::checkboxInput(ns("include_unadjusted"), "Include unadjusted estimates",
                                        value = TRUE, width = "100%"))
      ),
      lapply(.RESULTS_OUTPUTS[-1], .choice),
      shiny::tags$hr(class = "my-2"),
      edark_run_button(ns, "btn_generate", "Generate Outputs"),
      shiny::tags$p(class = "small text-muted mt-2 mb-0",
                    "Only the ticked outputs are created, and only created outputs can be",
                    "exported from 4 \u00b7 Export. The Summary tab is always shown.")
    ),
    result = shiny::tagList(
      shiny::tags$script(shiny::HTML("
        function edarkCopyText(id, btn) {
          var el = document.getElementById(id);
          if (!el || !navigator.clipboard) return;
          navigator.clipboard.writeText(el.innerText).then(function() {
            var old = btn.innerHTML; btn.innerHTML = 'Copied';
            setTimeout(function() { btn.innerHTML = old; }, 1500);
          });
        }")),
      shiny::uiOutput(ns("tabs_ui"))
    ),
    info = shiny::uiOutput(ns("header_ui"))
  )
}


#' @rdname module_analysis_results
#' @export
analysis_results_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    has_model <- shiny::reactive({
      !is.null(shared_state$analysis_result$fitted_models$primary_model)
    })
    .selected_outputs <- function() {
      ids <- vapply(.RESULTS_OUTPUTS, `[[`, character(1), "id")
      ids[vapply(ids, function(i) isTRUE(input[[paste0("out_", i)]]), logical(1))]
    }

    # A fitted model, and at least one output ticked.
    res_block <- shiny::reactive({
      if (!has_model()) return("fit_model")
      if (length(.selected_outputs()) == 0L) return("pick_output")
      NULL
    })
    edark_run_gate(output, "btn_generate",
                   enabled = shiny::reactive(is.null(res_block())),
                   reason  = shiny::reactive(edark_lock_reason(res_block())))

    # ── Generate ─────────────────────────────────────────────────────────────
    shiny::observeEvent(input$btn_generate, {
      res <- shiny::isolate(shared_state$analysis_result)
      if (!is.null(shiny::isolate(res_block()))) return()   # the button is disabled
      selected <- .selected_outputs()
      want_unadj <- "results_table" %in% selected && isTRUE(input$include_unadjusted)

      shiny::showModal(.analysis_progress_modal("Generating Outputs\u2026"))
      on.exit(shiny::removeModal(), add = TRUE)
      progress <- function(frac, detail) {
        session$sendCustomMessage("edark_analysis_progress", list(frac = frac, detail = detail))
      }

      errors <- character(0)
      .try <- function(label, expr) {
        tryCatch(expr, error = function(e) {
          errors <<- c(errors, sprintf("%s: %s", label, conditionMessage(e)))
          NULL
        })
      }

      un  <- if (want_unadj) .try("Unadjusted models", fit_unadjusted_models(res, progress_fn = progress))
      progress(0.9, "Building outputs\u2026")
      tbl <- if (any(c("results_table", "forest_plot") %in% selected)) {
        .try("Results table", build_results_table(res, un))
      }

      if (is.null(res$result_tables)) res$result_tables <- list()
      if (is.null(res$result_plots))  res$result_plots  <- list()
      res$result_tables["main_results"]    <- list(if ("results_table" %in% selected) tbl)
      res$result_tables["fit_statistics"]  <- list(
        if ("fit_statistics" %in% selected) .try("Fit statistics", build_fit_statistics_table(res)))
      res$result_plots["coefficient_plot"] <- list(
        if ("forest_plot" %in% selected && !is.null(tbl)) .try("Forest plot", build_forest_plot(tbl)))
      res["methods_paragraph"] <- list(
        if ("methods" %in% selected) .try("Methods paragraph", build_methods_paragraph(res, !is.null(un))))
      res$fitted_models["univariable_models"] <- list(un$models)
      res$results_generation <- list(
        generated_at       = Sys.time(),
        outputs            = selected,
        include_unadjusted = !is.null(un),
        unadjusted_status  = un$status
      )
      shared_state$analysis_result <- res

      if (length(errors) > 0L) {
        shiny::showNotification(paste(errors, collapse = "\n"), type = "error", duration = 8)
      } else {
        shiny::showNotification(sprintf("Generated %d output%s.", length(selected),
                                        if (length(selected) == 1L) "" else "s"),
                                type = "message", duration = 4)
      }
    }, ignoreInit = TRUE)

    # ── Main: header ─────────────────────────────────────────────────────────
    output$header_ui <- shiny::renderUI({
      res <- shared_state$analysis_result
      if (!has_model()) return(.ms_placeholder(edark_lock_reason("fit_model")))
      snap <- res$specification_snapshot
      mt   <- snap$model_design$model_type
      gen  <- res$results_generation
      edark_model_header(
        title  = .ANALYSIS_MODEL_LABELS[[mt]],
        fields = list(
          Formula = shiny::tags$code(
            paste(deparse(res$run_status$formula, width.cutoff = 500L), collapse = " "))
        ),
        notes  = if (is.null(gen)) "Outputs not generated yet."
                 else sprintf("Outputs generated %s", format(gen$generated_at, "%H:%M:%S"))
      )
    })

    # ── Main: tabs ───────────────────────────────────────────────────────────
    output$tabs_ui <- shiny::renderUI({
      res <- shared_state$analysis_result
      if (!has_model()) return(NULL)
      gen <- res$results_generation
      tabs <- list(bslib::nav_panel("Summary", .rs_summary(res)))
      if (!is.null(res$result_tables$main_results)) {
        tabs <- c(tabs, list(bslib::nav_panel("Results table",
          shiny::div(class = "pt-2 edark-scroll-table", gt::gt_output(ns("results_gt"))))))
      }
      if (!is.null(res$result_tables$fit_statistics)) {
        tabs <- c(tabs, list(bslib::nav_panel("Fit statistics",
          shiny::div(class = "pt-2", style = "max-width: 640px;", gt::gt_output(ns("fit_gt"))))))
      }
      if (!is.null(res$result_plots$coefficient_plot)) {
        n <- attr(res$result_plots$coefficient_plot, "n_rows") %||% 10L
        # The plot's own height scales with the number of terms, so a large
        # model would run past the viewport; the wrapper scrolls it instead.
        tabs <- c(tabs, list(bslib::nav_panel("Forest plot",
          shiny::div(
            class = "edark-scroll-table",
            shiny::plotOutput(ns("forest"), height = sprintf("%dpx", 34L * n + 90L))
          ))))
      }
      if (!is.null(res$methods_paragraph)) {
        tabs <- c(tabs, list(bslib::nav_panel("Methods", .rs_methods(res$methods_paragraph, ns("methods_text")))))
      }
      do.call(bslib::navset_card_tab, c(list(id = ns("result_tabs")), tabs))
    })

    output$results_gt <- gt::render_gt({
      tbl <- shared_state$analysis_result$result_tables$main_results
      shiny::req(!is.null(tbl))
      results_table_gt(tbl)
    })

    output$fit_gt <- gt::render_gt({
      fs <- shared_state$analysis_result$result_tables$fit_statistics
      shiny::req(!is.null(fs))
      gt::gt(fs) %>%
        gt::cols_label(Statistic = "", Value = "") %>%
        gt::cols_align("right", columns = "Value") %>%
        gt::tab_options(table.font.size = gt::px(14), data_row.padding = gt::px(4),
                        column_labels.hidden = TRUE)
    })

    output$forest <- shiny::renderPlot({
      p <- shared_state$analysis_result$result_plots$coefficient_plot
      shiny::req(!is.null(p))
      p
    }, res = 96)
  })
}


# ── Rendering helpers ─────────────────────────────────────────────────────────

.rs_kv <- function(label, value, level = NULL) {
  shiny::div(class = "d-flex justify-content-between gap-3 small mb-1",
             shiny::span(class = "text-muted", label),
             shiny::span(class = "fw-semibold text-end", if (!is.null(level)) .ms_level_icon(level), " ", value))
}

.rs_card <- function(title, ...) {
  bslib::card(class = "mb-3", bslib::card_header(title), bslib::card_body(class = "py-2", ...))
}

# Key numbers only — no prose.
.rs_summary <- function(res) {
  snap  <- res$specification_snapshot
  vr    <- snap$variable_roles
  mt    <- snap$model_design$model_type
  rs    <- res$run_status
  logit <- mt %in% c("logistic", "logistic_mixed")
  mixed <- mt %in% c("linear_mixed", "logistic_mixed")
  coefs <- res$inference_summary$coefficients
  fs    <- res$inference_summary$fit_statistics
  gen   <- res$results_generation
  exposure <- vr$exposure_variable
  measure  <- if (logit) "OR" else "\u03b2"
  .fs <- function(key) {
    if (is.null(fs)) return(NULL)
    i <- match(key, fs$key)
    if (is.na(i)) NULL else .fmt_fit_stat(fs$value[i], fs$format[i])
  }
  .p <- function(p) { x <- edark_format_p(p); if (startsWith(x, "<")) paste("p", x) else paste("p =", x) }

  # Exposure estimates: adjusted (Step 5), and unadjusted if generated
  un_tbl <- res$result_tables$main_results
  exp_rows <- if (!is.null(exposure) && !is.null(coefs)) coefs[coefs$variable %in% exposure, , drop = FALSE]
  exposure_card <- .rs_card(
    if (is.null(exposure)) "Exposure" else sprintf("Exposure: %s", exposure),
    if (is.null(exposure)) {
      .rs_kv("Exposure", "None (risk-factor study)")
    } else if (is.null(exp_rows) || nrow(exp_rows) == 0L) {
      .rs_kv("Estimate", "-")
    } else {
      lapply(seq_len(nrow(exp_rows)), function(i) {
        r   <- exp_rows[i, ]
        lab <- if (is.na(r$level)) "Per 1-unit increase" else sprintf("%s vs %s", r$level, rs$reference_levels[[exposure]])
        un  <- if (isTRUE(attr(un_tbl, "include_unadjusted"))) {
          u <- un_tbl[un_tbl$variable == exposure & (if (is.na(r$level)) is.na(un_tbl$level) else un_tbl$level %in% r$level), ]
          if (nrow(u)) sprintf("%s, %s", edark_format_ci(u$unadj_est, u$unadj_low, u$unadj_high), .p(u$unadj_p))
        }
        shiny::tagList(
          .rs_kv(sprintf("%s - adjusted %s", lab, measure),
                 sprintf("%s, %s", edark_format_ci(r$effect, r$effect.low, r$effect.high), .p(r$p.value))),
          if (!is.null(un)) .rs_kv(sprintf("%s - unadjusted %s", lab, measure), un)
        )
      })
    }
  )

  ev <- rs$outcome_event
  model_card <- .rs_card("Model",
    .rs_kv("Model", .ANALYSIS_MODEL_LABELS[[mt]]),
    .rs_kv("Outcome", if (logit) sprintf("%s = %s (vs %s)", ev$variable, ev$event, ev$reference) else vr$outcome_variable),
    .rs_kv("Covariates", if (length(vr$final_model_covariates)) length(vr$final_model_covariates) else "None"),
    if (length(vr$final_model_covariates)) shiny::div(class = "small text-muted mb-1",
                                                     paste(vr$final_model_covariates, collapse = ", ")),
    if (mixed) .rs_kv("Random intercepts", paste(vr$cluster_variables, collapse = ", "))
  )

  n_excl <- rs$n_total - rs$n_used
  sp <- analysis_split(snap)
  sample_card <- .rs_card("Sample",
    if (!is.null(sp)) .rs_kv("Fitted on", sprintf("Training set (%s = %s)", sp$variable, sp$training_level)),
    .rs_kv("Observations used", format(rs$n_used, big.mark = ",")),
    .rs_kv("Excluded (missing data)", sprintf("%s (%.1f%%)", format(n_excl, big.mark = ","), 100 * n_excl / rs$n_total)),
    if (logit) .rs_kv("Events", sprintf("%s (%s)", .fs("n_events") %||% "-", .fs("event_rate") %||% "-")),
    if (mixed) lapply(vr$cluster_variables, function(cl) {
      .rs_kv(sprintf("Clusters (%s)", cl), .fs(paste0("n_groups_", cl)) %||% "-")
    })
  )

  ps  <- res$performance$sets
  .auc <- function(s) {
    if (is.null(s$auc)) return(NULL)
    if (is.null(s$auc_low)) edark_format_est(s$auc) else edark_format_ci(s$auc, s$auc_low, s$auc_high)
  }
  .slope <- function(s) if (!is.null(s$cal_slope)) edark_format_est(s$cal_slope)
  fit_rows <- list(
    if (mt == "linear") list("R\u00b2 / adjusted R\u00b2", paste(.fs("r2"), "/", .fs("adj_r2"))),
    if (mt == "logistic") list("Pseudo R\u00b2 (Nagelkerke)", .fs("r2_nagelkerke")),
    if (mixed) list("Marginal / conditional R\u00b2", paste(.fs("r2_marginal") %||% "-", "/", .fs("r2_conditional") %||% "-")),
    if (mixed) list("ICC (adjusted)", .fs("icc") %||% "-"),
    if (!is.null(.auc(ps$apparent))) list("AUC (apparent)", .auc(ps$apparent)),
    if (!is.null(.auc(ps$test))) list("AUC (test set)", .auc(ps$test)),
    if (!is.null(.auc(ps$cv))) list("AUC (cross-validated)", .auc(ps$cv)),
    if (!is.null(.auc(ps$bootstrap))) list("AUC (bootstrap-corrected)", .auc(ps$bootstrap)),
    if (!is.null(.slope(ps$test))) list("Calibration slope (test set)", .slope(ps$test)),
    if (!is.null(.slope(ps$cv))) list("Calibration slope (cross-validated)", .slope(ps$cv)),
    if (!is.null(.slope(ps$bootstrap))) list("Calibration slope (bootstrap-corrected)", .slope(ps$bootstrap)),
    list("AIC", .fs("aic")),
    list("BIC", .fs("bic"))
  )
  fit_card <- .rs_card("Fit", lapply(Filter(Negate(is.null), fit_rows), function(r) .rs_kv(r[[1]], r[[2]] %||% "-")))

  rm <- rs$run_messages
  n_fit_warn <- if (is.null(rm)) 0L else sum(rm$level == "warning")
  n_pf_warn  <- length(rs$preflight)
  dg <- res$diagnostics
  n_dg_warn  <- if (is.null(dg)) NA else sum(dg$messages$level == "warning")
  checks_card <- .rs_card("Checks",
    .rs_kv("Preflight warnings at fit", n_pf_warn, if (n_pf_warn > 0) "warning"),
    .rs_kv("Fitting warnings", n_fit_warn, if (n_fit_warn > 0) "warning"),
    .rs_kv("Diagnostics", if (is.na(n_dg_warn)) "Not run" else sprintf("Run - %d warning%s", n_dg_warn, if (n_dg_warn == 1L) "" else "s"),
           if (!is.na(n_dg_warn) && n_dg_warn > 0) "warning"),
    .rs_kv("Performance", if (is.null(res$performance)) "Not run" else {
      paste(vapply(res$performance$sets, `[[`, character(1), "label"), collapse = " + ")
    }),
    if (!is.null(gen$unadjusted_status)) {
      bad <- sum(gen$unadjusted_status$status != "ok")
      .rs_kv("Unadjusted models with problems", bad, if (bad > 0) "warning")
    }
  )

  labels <- stats::setNames(vapply(.RESULTS_OUTPUTS, `[[`, character(1), "label"),
                            vapply(.RESULTS_OUTPUTS, `[[`, character(1), "id"))
  outputs_card <- .rs_card("Outputs",
    if (is.null(gen)) {
      shiny::tags$p(class = "small text-muted mb-0", "Not generated yet. Choose outputs in the sidebar and click Generate Outputs.")
    } else {
      shiny::tagList(
        .rs_kv("Generated", format(gen$generated_at, "%Y-%m-%d %H:%M:%S")),
        lapply(names(labels), function(id) {
          .rs_kv(labels[[id]], if (id %in% gen$outputs) {
            if (id == "results_table") {
              if (isTRUE(gen$include_unadjusted)) "Yes (with unadjusted)" else "Yes (adjusted only)"
            } else "Yes"
          } else "No")
        })
      )
    }
  )

  shiny::div(
    class = "pt-2",
    bslib::layout_columns(
      col_widths = c(6, 6),
      shiny::tagList(exposure_card, model_card, sample_card),
      shiny::tagList(fit_card, checks_card, outputs_card)
    )
  )
}

.rs_methods <- function(text, id) {
  paras <- strsplit(text, "\n\n", fixed = TRUE)[[1]]
  shiny::div(
    class = "pt-2", style = "max-width: 900px;",
    shiny::div(class = "d-flex justify-content-end mb-2",
               shiny::tags$button(type = "button", class = "btn btn-outline-secondary btn-sm",
                                  onclick = sprintf("edarkCopyText('%s', this)", id),
                                  shiny::icon("copy"), " Copy")),
    shiny::div(id = id, class = "border rounded p-3 bg-body-tertiary", style = "user-select: text;",
               lapply(paras, function(p) shiny::tags$p(class = "mb-2", p)))
  )
}
