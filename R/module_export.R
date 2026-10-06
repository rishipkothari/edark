#' Export Module - the top-level 4 · Export page
#'
#' One zip of materials from every stage (PRD/BUILD_Export.md): the working
#' dataset, the files that reproduce it, Table 1, variable selection, the
#' model, diagnostics, performance and a compiled report.
#'
#' Page contract (D6): the config pane holds the build action, what is
#' selected, Select all / Clear and Download Last Build; the centre is a
#' checklist of what can be exported, in plain-language sections (one per zip
#' folder), where ticking an item is selecting it (X12) and an item's options
#' - data format, report format, the session's input dataset - sit on its
#' row; the info pane counts what the zip will hold. Items whose output has
#' not been created, or is stale, are listed but cannot be ticked (X9); a
#' section with nothing available yet collapses to one line saying where it
#' is made.
#'
#' The checklist is plain HTML driven by \code{inst/www/edark_export.js}:
#' tri-state section boxes and the selection are handled in the browser and
#' reported as \code{input$selection}. It is re-rendered only when the
#' registry changes, and renders the server's copy of the selection, so a
#' re-render (e.g. Diagnostics just ran) keeps it (§N8).
#'
#' Build & Download runs the build in ticks with a Cancel button (the
#' Performance pattern, §N6.9a), then starts the download itself.
#'
#' @param id Character. Module namespace ID.
#' @param shared_state A Shiny \code{reactiveValues} object.
#' @param dataset_input The data frame passed to \code{edark()}, before casting
#'   - what a session file describes and may carry.
#'
#' @name module_export
NULL


# Seconds of file writing per reactive tick: long enough to keep overhead low,
# short enough that Cancel responds promptly.
.EXPORT_TICK_SECS <- 0.3


#' @rdname module_export
#' @export
export_ui <- function(id) {
  ns <- shiny::NS(id)

  edark_page(
    config = shiny::tagList(
      # The checklist's behaviour and the download trigger (§N8)
      shiny::tags$script(src = "edark/edark_export.js"),

      # What goes in the zip is chosen in the centre; this pane holds the
      # actions and what is selected (BUILD_Export.md §5)
      edark_run_button(ns, "btn_build", "Build & Download", icon = "file-zipper"),
      shiny::uiOutput(ns("selection_summary"), class = "small mt-2"),
      shiny::div(
        class = "small mt-1",
        shiny::tags$a(href = "#", `data-export-select` = "all", "Select all"),
        " \u00b7 ",
        shiny::tags$a(href = "#", `data-export-select` = "none", "Clear")
      ),

      shiny::tags$hr(class = "my-3"),
      # Build & Download clicks this for the user; it stays to download the
      # last build again. A download button is an <a>: shinyjs::disabled()
      # neither greys it nor stops the click, and Shiny strips Bootstrap's
      # .disabled as soon as the handler is ready. Our own class (edark.css
      # section 10) holds it off until a build exists; edark_export.js removes
      # it (§N8.4).
      edark_button(ns, "download", "Download Last Build Again", icon = "download",
                   variant = "secondary", outline = TRUE, type = "download",
                   class = "edark-export-no-build",
                   `aria-disabled` = "true", tabindex = "-1"),

      shiny::tags$p(class = "small text-muted mt-3 mb-0",
                    "The working dataset, and a session file that includes your original data,",
                    "hold patient-level data. Only share them where your data governance allows.")
    ),
    messages = edark_messages_ui(ns),
    result = shiny::uiOutput(ns("tree")),
    info = shiny::uiOutput(ns("info"))
  )
}


#' @rdname module_export
#' @export
export_server <- function(id, shared_state, dataset_input) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── State and registry ───────────────────────────────────────────────────
    # The working dataset's digest only matters once Analyze has frozen one;
    # cached here so the registry does not re-hash on every tick.
    working_sig <- shiny::reactive({
      if (is.null(shared_state$analysis_spec)) return(NULL)
      wd <- shared_state$dataset_working
      if (is.null(wd)) NULL else digest::digest(wd, algo = "sha256")
    })

    st <- shiny::reactive({
      export_state(
        dataset_input         = dataset_input,
        dataset_original      = shared_state$dataset_original,
        dataset_working       = shared_state$dataset_working,
        original_column_types = shared_state$original_column_types,
        last_applied_specs    = shared_state$last_applied_specs,
        custom_report_items   = shared_state$custom_report_items,
        analysis_data         = shared_state$analysis_data,
        analysis_spec         = shared_state$analysis_spec,
        analysis_result       = shared_state$analysis_result,
        has_pending           = shared_state$has_pending_changes,
        working_sig           = working_sig()
      )
    })

    items <- shiny::reactive({
      export_items(st(), input$data_format %||% "rds", input$report_format %||% "docx")
    })

    # ── Selection ────────────────────────────────────────────────────────────
    # The server's copy of the ticked ids. An id is ticked by default the first
    # time it becomes available (if its default is TRUE); after that the user's
    # choice stands. Ids that are not available keep their tick here, so an
    # output that goes stale and is re-run comes back ticked.
    sel  <- shiny::reactiveVal(character(0))
    seen <- shiny::reactiveVal(character(0))
    sel  <- shiny::reactiveVal(character(0))
    seen <- shiny::reactiveVal(character(0))

    # Called by the observer below and again by the checklist before it
    # renders, so the first render already carries the defaults - otherwise
    # the browser would report the empty list back and wipe them.
    .take_defaults <- function(it) {
      avail <- it$id[it$status == "available"]
      new   <- setdiff(avail, shiny::isolate(seen()))
      if (length(new) == 0L) return(invisible())
      seen(union(shiny::isolate(seen()), new))
      add <- intersect(new, it$id[it$default])
      if (length(add)) sel(union(shiny::isolate(sel()), add))
      invisible()
    }
    shiny::observe(.take_defaults(items()))

    shiny::observeEvent(input$selection, {
      it      <- shiny::isolate(items())
      avail   <- it$id[it$status == "available"]
      kept    <- setdiff(shiny::isolate(sel()), avail)
      new_sel <- union(kept, intersect(input$selection, avail))
      if (!setequal(new_sel, shiny::isolate(sel()))) sel(new_sel)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    selected <- shiny::reactive({
      it <- items()
      it[it$status == "available" & it$id %in% sel(), , drop = FALSE]
    })

    # ── Checklist ────────────────────────────────────────────────────────────
    # Re-rendered only when what it shows changes (ids, statuses), never on a
    # tick: ticks live in the browser until the next render (§N8). Not on a
    # format change either - the format selects sit in the list, and the
    # browser updates the file type on its row (§N8.3).
    tree_key <- shiny::reactiveVal(NULL)
    shiny::observe({
      key <- items()[, c("id", "status", "reason")]
      if (!identical(key, shiny::isolate(tree_key()))) tree_key(key)
    })

    output$tree <- shiny::renderUI({
      shiny::req(tree_key())
      it <- shiny::isolate(items())
      shiny::isolate(.take_defaults(it))
      opts <- shiny::isolate(list(
        data_format          = input$data_format %||% "rds",
        report_format        = input$report_format %||% "docx",
        session_include_data = isTRUE(input$session_include_data)
      ))
      .export_tree(it, shiny::isolate(sel()), ns, opts)
    })

    output$selection_summary <- shiny::renderUI({
      it    <- items()
      n_sel <- nrow(selected())
      n_na  <- sum(it$status %in% c("not_run", "stale"))
      shiny::tagList(
        shiny::div(class = "fw-semibold",
                   if (n_sel == 0L) "Nothing selected"
                   else sprintf("%d item%s selected", n_sel, if (n_sel == 1L) "" else "s")),
        if (n_na > 0L) shiny::div(class = "text-muted",
          sprintf("%d more become%s available as you work through Analyze.",
                  n_na, if (n_na == 1L) "s" else ""))
      )
    })

    # ── Build gate ───────────────────────────────────────────────────────────
    edark_run_gate(output, "btn_build",
                   enabled = shiny::reactive(nrow(selected()) > 0L),
                   reason  = shiny::reactive(edark_lock_reason("pick_export")))

    # ── Build ────────────────────────────────────────────────────────────────
    # The job runs in ticks; `runner` holds it between ticks (not reactive).
    runner <- new.env(parent = emptyenv())
    runner$job    <- NULL
    runner$cancel <- FALSE
    running    <- shiny::reactiveVal(FALSE)
    last_build <- shiny::reactiveVal(NULL)

    .progress <- function(frac, detail) {
      session$sendCustomMessage("edark_analysis_progress", list(frac = frac, detail = detail))
    }

    shiny::observeEvent(input$btn_build, {
      todo <- shiny::isolate(selected())
      if (nrow(todo) == 0L) return()   # the button is disabled
      opts <- list(data_format          = input$data_format %||% "rds",
                   report_format        = input$report_format %||% "docx",
                   session_include_data = isTRUE(input$session_include_data))
      shiny::showModal(.analysis_progress_modal("Building Export\u2026", cancel_id = ns("cancel_build")))
      job <- tryCatch(export_job(shiny::isolate(items()), todo$id, shiny::isolate(st()), opts),
                      error = function(e) e)
      if (inherits(job, "error")) {
        shiny::removeModal()
        shiny::showNotification(paste("Export failed:", conditionMessage(job)), type = "error", duration = 8)
        return()
      }
      runner$job    <- job
      runner$cancel <- FALSE
      .progress(.export_job_progress(job), .export_job_detail(job))
      running(TRUE)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$cancel_build, { runner$cancel <- TRUE }, ignoreInit = TRUE)

    shiny::observe({
      if (!running()) return()
      shiny::invalidateLater(10)
      shiny::isolate({
        job <- runner$job
        if (isTRUE(runner$cancel)) {
          running(FALSE)
          unlink(job$dir, recursive = TRUE)
          runner$job <- NULL
          shiny::removeModal()
          shiny::showNotification("Export cancelled - nothing was saved.", type = "warning", duration = 5)
          return()
        }
        t0 <- Sys.time()
        repeat {
          if (job$done || as.numeric(difftime(Sys.time(), t0, units = "secs")) > .EXPORT_TICK_SECS) break
          job <- .export_job_step(job)
        }
        runner$job <- job
        .progress(.export_job_progress(job), .export_job_detail(job))
        if (!job$done) return()

        running(FALSE)
        runner$job <- NULL
        out <- tryCatch(.export_job_finish(job), error = function(e) e)
        shiny::removeModal()
        if (inherits(out, "error")) {
          unlink(job$dir, recursive = TRUE)
          shiny::showNotification(paste("Export failed:", conditionMessage(out)), type = "error", duration = 8)
          return()
        }
        # One build kept at a time
        old <- last_build()
        if (!is.null(old)) unlink(old$dir, recursive = TRUE)
        out$time <- Sys.time()
        out$size <- file.size(out$zip)
        last_build(out)
        session$sendCustomMessage("edark_export_download", list(id = ns("download")))
      })
    })

    output$download <- shiny::downloadHandler(
      filename = function() basename(last_build()$zip),
      content  = function(file) file.copy(last_build()$zip, file, overwrite = TRUE),
      contentType = "application/zip"
    )
    # The page may be hidden when the build ends; the link still needs its href
    shiny::outputOptions(output, "download", suspendWhenHidden = FALSE)

    session$onSessionEnded(function() {
      b <- shiny::isolate(last_build())
      if (!is.null(b)) unlink(b$dir, recursive = TRUE)
    })

    # ── Messages (D3) ────────────────────────────────────────────────────────
    edark_messages_server(output, shiny::reactive({
      s <- st()
      b <- last_build()
      c(
        if (isTRUE(s$prepare_changed)) list(edark_message("stale",
          "Prepare has changed since Analyze \u203a Setup froze the dataset.",
          "Every Analyze output is stale and cannot be exported. Restart the analysis in Analyze \u203a Setup to export it.")),
        if (isTRUE(s$has_pending)) list(edark_message("pending",
          "Prepare has unapplied changes.",
          "The export uses the last applied state. Apply or reset them in Prepare.")),
        if (!is.null(b) && nrow(b$failed) > 0L) list(edark_message("warn",
          sprintf("%d file%s could not be written and %s left out of the last build.",
                  nrow(b$failed), if (nrow(b$failed) == 1L) "" else "s",
                  if (nrow(b$failed) == 1L) "was" else "were"),
          shiny::tags$ul(class = "mb-0 ps-3", lapply(seq_len(nrow(b$failed)), function(i) {
            shiny::tags$li(shiny::tags$code(b$failed$id[i]), " - ", b$failed$message[i])
          }))))
      )
    }))

    # ── Info pane ────────────────────────────────────────────────────────────
    output$info <- shiny::renderUI({
      it  <- items()
      sl  <- selected()
      s   <- st()
      b   <- last_build()
      wd  <- s$dataset_working
      res <- s$analysis_result
      mt  <- res$specification_snapshot$model_design$model_type
      gs  <- analysis_output_status(s$analysis_spec, res, s$prepare_changed)

      folder_rows <- lapply(names(.EXPORT_SECTIONS), function(key) {
        f  <- .export_section_folder(key)
        av <- it[it$folder == f & it$status == "available", , drop = FALSE]
        if (nrow(av) == 0L) return(NULL)
        k <- sum(sl$folder == f)
        edark_info_row(.EXPORT_SECTIONS[[key]]$title,
                       if (k == 0L) shiny::span(class = "text-muted fw-normal", "none")
                       else sprintf("%d of %d", k, nrow(av)))
      })

      analyze_state <- if (is.null(s$analysis_spec)) "Not started"
        else if (isTRUE(s$prepare_changed)) "Stale - Prepare changed"
        else if (identical(gs$model$status, "available")) .ANALYSIS_MODEL_LABELS[[mt]]
        else if (identical(gs$model$status, "stale")) "Model stale - refit"
        else "No model fitted"

      shiny::tagList(
        edark_section_label("The zip will hold", first = TRUE),
        folder_rows,
        edark_info_row("Files", nrow(sl) + 1L),
        shiny::tags$p(class = "small text-muted mt-1 mb-0", "Plus a README file describing the contents."),

        edark_section_label("Source"),
        edark_info_row("Working dataset",
                       sprintf("%s \u00d7 %s", format(nrow(wd), big.mark = ","), ncol(wd))),
        edark_info_row("Analyze", analyze_state),
        if (any(sl$kind == "report"))
          edark_info_row("Report format", .EXPORT_REPORT_FORMATS[[input$report_format %||% "docx"]]),
        if (any(sl$kind == "data"))
          edark_info_row("Data format", .EXPORT_DATA_FORMATS[[input$data_format %||% "rds"]]),
        if (any(sl$kind == "session"))
          edark_info_row("Session file", if (isTRUE(input$session_include_data)) "With input dataset" else "Without data"),

        if (!is.null(b)) shiny::tagList(
          edark_section_label("Last build"),
          edark_info_row("Built", format(b$time, "%H:%M:%S")),
          edark_info_row("Files", b$n_written + 1L),
          edark_info_row("Size", .export_fmt_bytes(b$size)),
          if (nrow(b$failed) > 0L) edark_info_row("Left out", nrow(b$failed))
        )
      )
    })
  })
}


# ── Checklist rendering ───────────────────────────────────────────────────────
# The zip's folders shown as plain-language sections, files by what they are
# rather than their file names (BUILD_Export.md §5). The zip keeps its folder
# and file names; only the page speaks in plain words.

.export_fmt_bytes <- function(x) {
  if (is.null(x) || is.na(x)) return("-")
  if (x < 1024^2) sprintf("%.0f KB", x / 1024) else sprintf("%.1f MB", x / 1024^2)
}

# One section per zip folder ("report" is the zip's root), in page order
.EXPORT_SECTIONS <- list(
  report             = list(title = "Report",
                            blurb = "One document that brings together everything below."),
  data               = list(title = "Your data",
                            blurb = "The dataset as you prepared it in Prepare."),
  reproduce          = list(title = "Pick up where you left off",
                            blurb = "Reopen this work in EDARK later, or see how the data were prepared."),
  table1             = list(title = "Table 1",
                            blurb = "Characteristics of the study population."),
  variable_selection = list(title = "Choosing variables",
                            blurb = "How the candidate variables were screened and selected."),
  model              = list(title = "Model results",
                            blurb = "Estimates, model fit and a methods paragraph for your write-up."),
  diagnostics        = list(title = "Model checks",
                            blurb = "Whether the model's assumptions hold."),
  performance        = list(title = "Model performance",
                            blurb = "How well the model separates outcomes, and how well it is calibrated.")
)

.export_section_folder <- function(key) if (identical(key, "report")) "" else key

# What an item is, in plain words. Keyed on the file name without extension;
# performance figures on their plot key.
.EXPORT_ITEM_LABELS <- c(
  analysis_report            = "Analysis report",
  working_dataset            = "Working dataset",
  session                    = "EDARK session file",
  prepare_steps              = "Data preparation steps",
  analysis_script            = "R script that reproduces the analysis",
  table1_overall             = "Table 1 - whole cohort",
  table1_by_exposure         = "Table 1 - by exposure",
  table1_by_outcome          = "Table 1 - by outcome",
  univariable_screen         = "Univariable screen",
  stepwise_selection         = "Stepwise selection",
  lasso_selection            = "LASSO selection",
  collinearity_flagged_pairs = "Strongly related variable pairs",
  correlation_heatmap        = "Correlation heat map",
  cramers_v_heatmap          = "Cramer's V heat map",
  results_table              = "Results table",
  fit_statistics             = "Model fit statistics",
  forest_plot                = "Forest plot",
  methods                    = "Methods paragraph",
  analysis_result            = "Complete results, for R users",
  diagnostic_summary         = "Summary of checks",
  vif                        = "Variance inflation (VIF)",
  influential_rows           = "Most influential rows",
  random_effects             = "Random effects",
  residuals_vs_fitted        = "Residuals vs fitted values",
  qq_plot                    = "Normal Q-Q plot",
  scale_location             = "Scale-location plot",
  binned_residuals           = "Binned residuals",
  linearity_plot             = "Linearity check",
  influence_plot             = "Cook's distance",
  leverage_plot              = "Leverage plot",
  random_effects_qq          = "Random effects Q-Q plot",
  cluster_size_plot          = "Cluster sizes",
  performance_summary        = "Summary of performance",
  bootstrap_optimism         = "Bootstrap optimism",
  roc_curve                  = "ROC curve",
  calibration_plot           = "Calibration plot",
  calibration_curve          = "Calibration curve",
  predicted_probs            = "Predicted probabilities"
)

.export_item_label <- function(item) {
  if (item$kind == "notes") return("Notes on how this was done")
  name <- if (item$kind == "perf_fig") item$key else sub("\\.[^.]*$", "", item$file)
  if (item$kind == "session") name <- "session"
  lab <- .EXPORT_ITEM_LABELS[name]
  lab <- if (is.na(lab)) .export_cap(gsub("_", " ", name)) else unname(lab)
  if (item$kind == "perf_fig") lab <- paste0(lab, " (", .PERF_SET_SHORT[[item$set]] %||% item$set, ")")
  lab
}

# The kind of file, for rows with no format to choose
.export_item_type <- function(item) {
  if (item$kind == "result_rds") return("R data file")
  if (item$kind == "session") return("")
  switch(tools::file_ext(item$file),
         docx = "Word", png = "Image", txt = "Text", R = "R script", "")
}

.EXPORT_STATUS_BADGE <- list(
  stale       = list(text = "out of date", role = "changed"),
  coming_soon = list(text = "coming soon", role = "neutral")
)

# An item's own options, on its row: the data and report formats, and whether
# the session file carries the input dataset. Real Shiny inputs (bound by id),
# rendered with the server's current value so a re-render keeps them;
# edark_export.js disables them while their item is unticked.
.export_leaf_option <- function(item, ns, opts) {
  .select <- function(id, choices, selected, what) {
    shiny::tags$select(
      id = ns(id), class = "edark-export-opt form-select form-select-sm",
      `aria-label` = paste("Format of the", what),
      lapply(names(choices), function(v) {
        shiny::tags$option(value = v, selected = if (identical(v, selected)) NA, choices[[v]])
      })
    )
  }
  switch(item$kind,
    data   = .select("data_format", .EXPORT_DATA_FORMATS, opts$data_format, "dataset"),
    report = .select("report_format", .EXPORT_REPORT_FORMATS, opts$report_format, "report"),
    session = shiny::tags$label(
      class = "edark-export-opt-check",
      title = "Patient-level data. Only share where your data governance allows.",
      shiny::tags$input(type = "checkbox", id = ns("session_include_data"),
                        class = "edark-export-opt form-check-input",
                        checked = if (isTRUE(opts$session_include_data)) NA),
      "include my original data"
    ),
    NULL
  )
}

.export_leaf <- function(item, checked, ns, opts) {
  ok  <- identical(item$status, "available")
  bd  <- .EXPORT_STATUS_BADGE[[item$status]]
  opt <- if (ok) .export_leaf_option(item, ns, opts)
  shiny::tags$li(
    class = paste("edark-export-leaf", if (!ok) "is-unavailable"),
    shiny::div(
      class = "edark-export-row",
      title = if (!ok) item$reason,
      shiny::tags$label(
        class = "edark-export-pick",
        shiny::tags$input(type = "checkbox", class = "edark-export-box form-check-input",
                          `data-id` = item$id,
                          checked = if (ok && checked) NA,
                          disabled = if (!ok) NA),
        shiny::span(class = "edark-export-label", .export_item_label(item))
      ),
      if (!is.null(opt)) opt
      else if (ok) shiny::span(class = "edark-export-type", .export_item_type(item)),
      if (!is.null(bd)) edark_badge(bd$text, role = bd$role),
      if (!ok && nzchar(item$reason) && !identical(item$status, "coming_soon"))
        shiny::span(class = "edark-export-reason", item$reason)
    )
  )
}

# One section: a heading (with a box that ticks the whole section when it
# holds more than one item), a line on what it is, then its items. A section
# with nothing available yet is one line saying where it is made.
.export_section <- function(key, rows, sel, ns, opts) {
  sec    <- .EXPORT_SECTIONS[[key]]
  folder <- .export_section_folder(key)

  if (!any(rows$status == "available")) {
    why <- rows$reason[rows$status != "coming_soon" & nzchar(rows$reason)]
    why <- if (length(why)) names(sort(table(why), decreasing = TRUE))[1L] else ""
    return(shiny::tags$section(
      class = "edark-export-section is-unavailable", `data-folder` = folder,
      shiny::div(class = "edark-export-section-head",
                 shiny::span(class = "edark-export-section-title", sec$title),
                 shiny::div(class = "edark-export-section-blurb",
                            paste0("Not available yet", if (nzchar(why)) paste0(" - ", why))))
    ))
  }

  # Long sections read better split into tables and figures
  groups <- if (all(c("tables", "figures") %in% rows$sub)) {
    list(Tables  = rows[rows$sub == "tables", , drop = FALSE],
         Figures = rows[rows$sub == "figures", , drop = FALSE],
         Other   = rows[rows$sub == "", , drop = FALSE])
  } else {
    list(Other = rows)
  }
  .leaves <- function(r) lapply(seq_len(nrow(r)), function(i) .export_leaf(r[i, ], r$id[i] %in% sel, ns, opts))

  shiny::tags$section(
    class = "edark-export-section", `data-folder` = folder,
    shiny::div(
      class = "edark-export-section-head",
      if (nrow(rows) > 1L) {
        shiny::tags$label(
          class = "edark-export-pick",
          shiny::tags$input(type = "checkbox", class = "edark-export-folder-box form-check-input",
                            `aria-label` = paste("Select everything in", sec$title)),
          shiny::span(class = "edark-export-section-title", sec$title))
      } else {
        shiny::span(class = "edark-export-section-title", sec$title)
      },
      shiny::div(class = "edark-export-section-blurb", sec$blurb)
    ),
    lapply(names(groups), function(g) {
      r <- groups[[g]]
      if (nrow(r) == 0L) return(NULL)
      shiny::tagList(
        if (length(groups) > 1L) shiny::div(class = "edark-export-group", g),
        shiny::tags$ul(class = "edark-export-list", .leaves(r))
      )
    })
  )
}

# The whole checklist, rendered from the registry; the browser keeps it live.
.export_tree <- function(it, sel, ns, opts) {
  shiny::div(
    class = "edark-export-tree",
    `data-input` = ns("selection"),
    lapply(names(.EXPORT_SECTIONS), function(key) {
      rows <- it[it$folder == .export_section_folder(key), , drop = FALSE]
      if (nrow(rows) == 0L) return(NULL)
      .export_section(key, rows, sel, ns, opts)
    })
  )
}
