#' Export Module - the top-level 4 · Export page
#'
#' One zip of materials from every stage (PRD/BUILD_Export.md): the working
#' dataset, the files that reproduce it, Table 1, variable selection, the
#' model, diagnostics, performance and a compiled report.
#'
#' Page contract (D6): the config pane holds only Build & Download; the centre
#' is the zip itself as a folder tree, where ticking a file is selecting it
#' (X12) and the file's options - data format, report format, the session's
#' input dataset - sit on its row; the info pane counts what the zip will
#' hold. Items whose
#' output has not been created, or is stale, are listed but cannot be ticked
#' (X9).
#'
#' The tree is plain HTML driven by \code{inst/www/edark_export.js}: folders
#' are native \code{<details>}, tri-state folder boxes and the selection are
#' handled in the browser and reported as \code{input$selection} and
#' \code{input$open}. The tree is re-rendered only when the registry changes,
#' and renders the server's copy of the selection and the open folders, so a
#' re-render (e.g. Diagnostics just ran) keeps both (§N8).
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
      # The tree's behaviour and the download trigger (§N8)
      shiny::tags$script(src = "edark/edark_export.js"),

      # Everything that is chosen - files, formats, the session's data - is
      # chosen in the tree, so this pane is only the action (BUILD_Export.md §5)
      edark_run_button(ns, "btn_build", "Build & Download", icon = "file-zipper"),
      shiny::tags$p(class = "small text-muted mt-2 mb-0",
                    "Tick files in the tree; formats are set on their rows."),
      shiny::tags$p(class = "small text-muted mt-2 mb-0",
                    "The working dataset, and a session file that includes the input dataset,",
                    "hold patient-level data. Only share where your data governance allows.")
    ),
    messages = edark_messages_ui(ns),
    result = shiny::tagList(
      edark_action_toolbar(
        shiny::uiOutput(ns("tree_header"), class = "me-auto small text-muted"),
        # The one action on the produced zip. Build & Download clicks it for
        # the user; it stays here to download the last build again.
        # A download button is an <a>: shinyjs::disabled() neither greys it
        # nor stops the click, and Shiny strips Bootstrap's .disabled as soon
        # as the handler is ready. Our own class (edark.css section 10) holds
        # it off until a build exists; edark_export.js removes it (§N8.4).
        edark_button(ns, "download", "Download Last Build", icon = "download",
                     size = "toolbar", type = "download", class = "edark-export-no-build",
                     `aria-disabled` = "true", tabindex = "-1")
      ),
      shiny::uiOutput(ns("tree"))
    ),
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
    open_dirs <- shiny::reactiveVal(character(0))

    # Called by the observer below and again by the tree before it renders,
    # so the first render already carries the defaults - otherwise the browser
    # would report the empty tree back and wipe them.
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

    shiny::observeEvent(input$open, {
      if (!setequal(input$open %||% character(0), shiny::isolate(open_dirs()))) open_dirs(input$open %||% character(0))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    selected <- shiny::reactive({
      it <- items()
      it[it$status == "available" & it$id %in% sel(), , drop = FALSE]
    })

    # ── Tree ─────────────────────────────────────────────────────────────────
    # Re-rendered only when what it shows changes (ids, statuses), never on a
    # tick: ticks live in the browser until the next render (§N8). Not on a
    # format change either - the format selects sit in the tree, and the
    # browser renames the file on its row (§N8.3).
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
      .export_tree(it, shiny::isolate(sel()), shiny::isolate(open_dirs()), ns, opts)
    })

    output$tree_header <- shiny::renderUI({
      it <- items()
      n_sel <- nrow(selected())
      n_av  <- sum(it$status == "available")
      shiny::span(sprintf("%d of %d available files selected", n_sel, n_av),
                  if (sum(it$status != "available") > 0L)
                    sprintf(" \u00b7 %d not available yet", sum(it$status %in% c("not_run", "stale"))))
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

      folder_rows <- lapply(c("", names(.EXPORT_FOLDERS)), function(f) {
        av <- it[it$folder == f & it$status == "available", , drop = FALSE]
        if (nrow(av) == 0L) return(NULL)
        k <- sum(sl$folder == f)
        edark_info_row(if (nzchar(f)) paste0(f, "/") else "Report",
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
        shiny::tags$p(class = "small text-muted mt-1 mb-0", "Including README.txt, always added."),

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


# ── Tree rendering ────────────────────────────────────────────────────────────

.export_fmt_bytes <- function(x) {
  if (is.null(x) || is.na(x)) return("-")
  if (x < 1024^2) sprintf("%.0f KB", x / 1024) else sprintf("%.1f MB", x / 1024^2)
}

.EXPORT_STATUS_BADGE <- list(
  stale       = list(text = "stale",       role = "changed"),
  not_run     = list(text = "not run",     role = "muted"),
  coming_soon = list(text = "coming soon", role = "neutral")
)

# A file's own options, on its row: the data and report formats, and whether
# the session file carries the input dataset. Real Shiny inputs (bound by id),
# rendered with the server's current value so a re-render keeps them;
# edark_export.js disables them while their file is unticked and renames the
# file when its format changes.
.export_leaf_option <- function(item, ns, opts) {
  .select <- function(id, choices, selected, stem) {
    shiny::tags$select(
      id = ns(id), class = "edark-export-opt form-select form-select-sm",
      `data-stem` = stem, `aria-label` = paste("Format of", stem),
      lapply(names(choices), function(v) {
        shiny::tags$option(value = v, selected = if (identical(v, selected)) NA, choices[[v]])
      })
    )
  }
  switch(item$kind,
    data   = .select("data_format", .EXPORT_DATA_FORMATS, opts$data_format, "working_dataset"),
    report = .select("report_format", .EXPORT_REPORT_FORMATS, opts$report_format, "analysis_report"),
    session = shiny::tags$label(
      class = "edark-export-opt-check",
      title = "Patient-level data. Only share where your data governance allows.",
      shiny::tags$input(type = "checkbox", id = ns("session_include_data"),
                        class = "edark-export-opt form-check-input",
                        checked = if (isTRUE(opts$session_include_data)) NA),
      "include input dataset"
    ),
    NULL
  )
}

.export_leaf <- function(item, checked, ns, opts) {
  ok <- identical(item$status, "available")
  bd <- .EXPORT_STATUS_BADGE[[item$status]]
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
        shiny::span(class = "edark-export-name", item$file)
      ),
      if (ok) .export_leaf_option(item, ns, opts),
      if (!is.null(bd)) edark_badge(bd$text, role = bd$role),
      if (!ok && nzchar(item$reason) && !identical(item$status, "coming_soon"))
        shiny::span(class = "edark-export-reason", item$reason)
    )
  )
}

.export_folder <- function(path, label, leaves, children, open_set, note = NULL) {
  shiny::tags$li(
    class = "edark-export-folder",
    shiny::tags$details(
      `data-folder` = path,
      open = if (path %in% open_set) NA,
      shiny::tags$summary(
        class = "edark-export-row",
        shiny::tags$input(type = "checkbox", class = "edark-export-folder-box form-check-input",
                          `aria-label` = paste("Select all in", label)),
        shiny::span(class = "edark-export-name", paste0(basename(path), "/")),
        if (!is.null(note)) shiny::span(class = "edark-export-folder-note", note),
        shiny::span(class = "edark-export-count")
      ),
      shiny::tags$ul(class = "edark-export-list", children, leaves)
    )
  )
}

# The zip as a tree: root files, then one folder per stage with tables/ and
# figures/ inside. Rendered from the registry; the browser keeps it live.
.export_tree <- function(it, sel, open_set, ns, opts) {
  .leaves <- function(rows) {
    lapply(seq_len(nrow(rows)), function(i) .export_leaf(rows[i, ], rows$id[i] %in% sel, ns, opts))
  }

  root_items <- it[it$folder == "", , drop = FALSE]
  folders <- lapply(names(.EXPORT_FOLDERS), function(f) {
    rows <- it[it$folder == f, , drop = FALSE]
    if (nrow(rows) == 0L) return(NULL)
    subs <- lapply(c("tables", "figures"), function(sb) {
      r <- rows[rows$sub == sb, , drop = FALSE]
      if (nrow(r) == 0L) return(NULL)
      .export_folder(paste(f, sb, sep = "/"), sb, .leaves(r), NULL, open_set)
    })
    .export_folder(f, .EXPORT_FOLDERS[[f]], .leaves(rows[rows$sub == "", , drop = FALSE]),
                   Filter(Negate(is.null), subs), open_set, note = .EXPORT_FOLDERS[[f]])
  })

  shiny::div(
    class = "edark-export-tree",
    `data-input`      = ns("selection"),
    `data-open-input` = ns("open"),
    shiny::div(
      class = "edark-export-tree-links small mb-2",
      shiny::tags$a(href = "#", `data-export-select` = "all", "Select all"),
      " · ",
      shiny::tags$a(href = "#", `data-export-select` = "none", "Clear"),
      " · ",
      shiny::tags$a(href = "#", `data-export-expand` = "all", "Expand all"),
      " · ",
      shiny::tags$a(href = "#", `data-export-expand` = "none", "Collapse all")
    ),
    shiny::div(class = "edark-export-root",
               shiny::span(class = "edark-export-name", "edark_export_<date>_<time>.zip")),
    shiny::tags$ul(
      class = "edark-export-list edark-export-top",
      shiny::tags$li(class = "edark-export-leaf is-fixed",
                     shiny::div(class = "edark-export-row",
                                shiny::tags$label(class = "edark-export-pick",
                                  shiny::tags$input(type = "checkbox", class = "form-check-input",
                                                    checked = NA, disabled = NA),
                                  shiny::span(class = "edark-export-name", "README.txt")),
                                shiny::span(class = "edark-export-reason", "always included"))),
      .leaves(root_items),
      Filter(Negate(is.null), folders)
    )
  )
}
