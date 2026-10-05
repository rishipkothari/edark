#' Export Report Service
#'
#' The compiled analysis report of the Export page (PRD/BUILD_Export.md §6):
#' one document holding everything that is current - data preparation, the
#' analysis summary, Table 1, results, performance and, as appendices,
#' variable selection and diagnostics. A section appears only when its source
#' is available (not run and stale outputs are left out, X9); for a prediction
#' model Performance comes before Results.
#'
#' The content is built once as a list of blocks (\code{.export_report_blocks()})
#' and rendered twice: Word on the bundled template with the Explore report's
#' \code{.docx_*} helpers, and HTML as one self-contained file built with
#' \code{htmltools} (figures inlined as base64, no pandoc needed).
#'
#' @name service_export_report
NULL


# ── Blocks ───────────────────────────────────────────────────────────────────
# list(type = "h1" | "h2" | "para" | "sections" | "table" | "figure", ...)

.rb_h1       <- function(text) list(type = "h1", text = text)
.rb_h2       <- function(text) list(type = "h2", text = text)
.rb_para     <- function(text) list(type = "para", text = text)
.rb_sections <- function(sections) list(type = "sections", sections = Filter(function(s) length(s$rows) > 0L, sections))
.rb_table    <- function(ft, caption) list(type = "table", ft = ft, caption = caption)
.rb_figure   <- function(plot, caption, size = c(.EXPORT_FIG_W, .EXPORT_FIG_H)) {
  list(type = "figure", plot = plot, caption = caption, size = size)
}


.export_report_meta <- function(st) {
  wd <- st$dataset_working
  mt <- st$analysis_result$specification_snapshot$model_design$model_type
  gs <- analysis_output_status(st$analysis_spec, st$analysis_result, st$prepare_changed)
  facts <- c(Dataset = sprintf("%s rows \u00d7 %s columns", format(nrow(wd), big.mark = ","), ncol(wd)))
  if (!is.null(mt) && identical(gs$model$status, "available")) {
    facts[["Model"]] <- .ANALYSIS_MODEL_LABELS[[mt]]
    vr <- st$analysis_result$specification_snapshot$variable_roles
    facts[["Outcome"]] <- vr$outcome_variable
    if (!is.null(vr$exposure_variable)) facts[["Exposure"]] <- vr$exposure_variable
  }
  facts[["EDARK version"]] <- EDARK_VERSION
  facts[["Generated"]] <- format(Sys.time(), "%d %B %Y %H:%M")
  list(
    title         = "Analysis Report",
    subtitle      = if ("Model" %in% names(facts)) facts[["Model"]] else "Data preparation",
    group_heading = "Sections",
    facts         = facts
  )
}


.export_report_blocks <- function(st) {
  spec <- st$analysis_spec
  res  <- st$analysis_result
  gs   <- analysis_output_status(spec, res, st$prepare_changed)
  ok   <- function(g) identical(gs[[g]]$status, "available")
  b    <- list()
  .add <- function(...) b <<- c(b, list(...))

  # 1. Data preparation - always: it describes the working dataset
  .add(.rb_h1("Data preparation"), .rb_sections(.export_prepare_sections(st)))
  if (isTRUE(st$prepare_changed)) {
    .add(.rb_para(paste("Prepare changed after the analysis dataset was frozen, so the analysis",
                        "outputs are out of date and are not included. Restart the analysis in",
                        "Analyze \u203a Setup to include them.")))
  }

  # 2. Analysis summary + methods
  if (ok("model")) {
    secs <- Filter(function(s) !identical(s$id, "prepare"), .export_notes_model(st))
    .add(.rb_h1("Analysis summary"), .rb_sections(secs))
    if (ok("results") && !is.null(res$methods_paragraph)) {
      .add(.rb_h2("Statistical methods"))
      for (p in strsplit(res$methods_paragraph, "\n\\s*\n")[[1]]) .add(.rb_para(trimws(p)))
    }
  }

  # 3. Table 1
  if (ok("table1")) {
    .add(.rb_h1("Table 1"))
    t1 <- c(Overall = "table1_overall", `By exposure` = "table1_by_exposure", `By outcome` = "table1_by_outcome")
    for (nm in names(t1)) {
      tbl <- res$result_tables[[t1[[nm]]]]
      if (!is.null(tbl)) .add(.rb_table(.export_table1_ft(tbl), paste("Table 1 -", tolower(nm))))
    }
  }

  # 4 / 5. Results and Performance - Performance first for a prediction model
  results <- list()
  if (ok("results")) {
    rt <- res$result_tables
    results <- c(list(.rb_h1("Results")),
      if (!is.null(rt$main_results)) list(.rb_table(results_table_flextable(rt$main_results), "Model results")),
      if (!is.null(rt$fit_statistics)) list(.rb_table(.export_ft(rt$fit_statistics), "Fit statistics")),
      if (!is.null(res$result_plots$coefficient_plot)) {
        p <- res$result_plots$coefficient_plot
        list(.rb_figure(p, "Forest plot", .export_plot_size(p, "forest_plot", st)))
      })
  }
  perf <- list()
  if (ok("performance")) {
    pf <- res$performance
    perf <- c(list(.rb_h1("Performance"),
                   .rb_table(.export_perf_summary_ft(pf), "Performance by set of rows")),
      if (is.data.frame(pf$sets$bootstrap$optimism) && nrow(pf$sets$bootstrap$optimism) > 0L)
        list(.rb_table(.export_perf_optimism_ft(pf$sets$bootstrap$optimism), "Bootstrap optimism correction")),
      unlist(lapply(names(res$result_plots$performance_plots), function(s) {
        pl <- Filter(Negate(is.null), res$result_plots$performance_plots[[s]])
        lapply(names(pl), function(k) .rb_figure(pl[[k]], paste0(pf$sets[[s]]$label, " - ", gsub("_", " ", k))))
      }), recursive = FALSE),
      list(.rb_sections(.export_notes_performance(st))))
  }
  prediction <- identical(spec$purpose_specification$model_purpose, "prediction")
  b <- c(b, if (prediction) c(perf, results) else c(results, perf))

  # Appendix A - variable selection
  if (ok("univariable") || ok("selection") || ok("collinearity")) {
    vi <- res$variable_investigation
    cp <- res$result_plots$collinearity_plots
    .add(.rb_h1("Appendix A - Variable selection"))
    if (ok("univariable")) .add(.rb_table(.export_univariable_ft(vi$univariable), "Univariable screen"))
    if (ok("selection")) {
      if (!is.null(vi$stepwise)) .add(.rb_table(.export_selection_ft(vi$stepwise, "stepwise"), "Stepwise selection"))
      if (!is.null(vi$lasso))    .add(.rb_table(.export_selection_ft(vi$lasso, "lasso"), "LASSO coefficients"))
    }
    if (ok("collinearity")) {
      if (!is.null(cp$cor_matrix)) {
        p <- .plot_correlation_heatmap(cp$cor_matrix)
        .add(.rb_figure(p, "Pearson correlation", .export_plot_size(p, "cor", st)))
      }
      if (!is.null(cp$cramers_v_matrix)) {
        p <- .plot_cramers_heatmap(cp$cramers_v_matrix)
        .add(.rb_figure(p, "Cram\u00e9r's V", .export_plot_size(p, "cramers", st)))
      }
    }
    .add(.rb_sections(.export_notes_variable_selection(st)))
  }

  # Appendix B - diagnostics
  if (ok("diagnostics")) {
    dg <- res$diagnostics
    .add(.rb_h1("Appendix B - Diagnostics"),
         .rb_table(.export_diag_summary_ft(dg), "Diagnostic summary"))
    if (is.data.frame(dg$vif))                  .add(.rb_table(.export_diag_table_ft(dg, "vif"), "Variance inflation factors"))
    if (!is.null(dg$influence$top))             .add(.rb_table(.export_diag_table_ft(dg, "influence"), "Most influential rows"))
    if (!is.null(dg$random_effects$components)) .add(.rb_table(.export_diag_table_ft(dg, "random_effects"), "Random effects"))
    pl <- Filter(Negate(is.null), res$result_plots$diagnostic_plots %||% list())
    for (k in names(pl)) .add(.rb_figure(pl[[k]], .export_cap(gsub("_", " ", k)), .export_plot_size(pl[[k]], k, st)))
    .add(.rb_sections(list(.export_messages_section(dg$messages))))
  }

  b
}


# ── Word ─────────────────────────────────────────────────────────────────────

#' Write the compiled report as Word
#' @param st From \code{export_state()}.
#' @param path Output \code{.docx} path.
#' @keywords internal
export_report_docx <- function(st, path) {
  meta <- .export_report_meta(st)
  doc  <- .export_docx()
  doc  <- .docx_add_title_page(doc, meta)
  doc  <- .docx_end_section(doc, "portrait", meta = NULL)
  doc  <- .docx_add_toc_page(doc)

  for (bl in .export_report_blocks(st)) {
    doc <- switch(bl$type,
      h1 = {
        # Each top-level section starts a page
        doc <- officer::body_add_break(doc)
        officer::body_add_par(doc, bl$text, style = "heading 1")
      },
      h2       = officer::body_add_par(doc, bl$text, style = "heading 2"),
      para     = officer::body_add_par(doc, bl$text, style = "Normal"),
      sections = .export_add_sections(doc, bl$sections, style = "heading 2"),
      table    = {
        doc <- .docx_add_caption(doc, bl$caption)
        doc <- flextable::body_add_flextable(doc, .docx_fit_ft(bl$ft))
        officer::body_add_par(doc, "", style = "Normal")
      },
      figure   = {
        png <- tempfile(fileext = ".png")
        on.exit(unlink(png), add = TRUE)
        ggplot2::ggsave(png, plot = bl$plot, width = bl$size[1], height = bl$size[2],
                        units = "in", dpi = 200, bg = "white")
        # Fit the page: full text width, at most 8.5 in tall
        w <- .DOCX_PORTRAIT_WIDTH
        h <- w * bl$size[2] / bl$size[1]
        if (h > 8.5) { w <- w * 8.5 / h; h <- 8.5 }
        doc <- .docx_add_caption(doc, bl$caption)
        officer::body_add_img(doc, src = png, width = w, height = h)
      },
      doc)
  }

  doc <- officer::body_set_default_section(doc, .docx_section_props("portrait", meta))
  print(doc, target = path)
  invisible(path)
}


# ── HTML ─────────────────────────────────────────────────────────────────────

.EXPORT_REPORT_CSS <- "
body { font-family: system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif; color: #212529;
       margin: 0; background: #f8f9fa; }
.page { max-width: 980px; margin: 0 auto; padding: 2rem 2.5rem 4rem; background: #fff; }
h1.title { font-size: 2rem; margin: 0 0 .25rem; }
.subtitle { color: #6c757d; font-size: 1.15rem; margin-bottom: 1.25rem; }
.facts { border-collapse: collapse; margin-bottom: 1.5rem; }
.facts td { padding: .15rem 1.25rem .15rem 0; font-size: .9rem; }
.facts td:first-child { color: #6c757d; }
nav.toc { border: 1px solid #dee2e6; border-radius: .375rem; padding: .75rem 1rem; margin-bottom: 2rem; }
nav.toc ol { margin: .25rem 0 0; padding-left: 1.25rem; }
nav.toc a { color: #2c7be5; text-decoration: none; }
h1.section { font-size: 1.5rem; border-bottom: 2px solid #dee2e6; padding-bottom: .3rem; margin-top: 2.5rem; }
h2 { font-size: 1.15rem; margin-top: 1.5rem; }
.caption { font-weight: 600; font-size: .95rem; margin: 1.25rem 0 .4rem; }
.table-wrap { overflow-x: auto; margin-bottom: 1rem; }
figure { margin: 0 0 1.5rem; }
figure img { max-width: 100%; height: auto; border: 1px solid #e9ecef; }
table.kv { border-collapse: collapse; width: 100%; font-size: .9rem; margin-bottom: 1rem; }
table.kv td { border-top: 1px solid #e9ecef; padding: .3rem .5rem; vertical-align: top; }
table.kv td:first-child { color: #6c757d; width: 32%; }
table.kv ul { margin: .2rem 0 0; padding-left: 1.1rem; }
.lvl-warning { color: #8a6516; font-weight: 600; }
.lvl-error { color: #b02a37; font-weight: 600; }
"

.export_ft_html <- function(ft) {
  html <- tryCatch(flextable::to_html(ft, type = "table"),
                   error = function(e) as.character(flextable::htmltools_value(ft)))
  htmltools::div(class = "table-wrap", htmltools::HTML(html))
}

.export_sections_html <- function(sections) {
  lapply(sections, function(sec) htmltools::tagList(
    htmltools::tags$h2(sec$title),
    htmltools::tags$table(class = "kv", lapply(sec$rows, function(r) {
      lvl <- if (!is.null(r$level) && r$level %in% c("warning", "error")) r$level
      htmltools::tags$tr(
        htmltools::tags$td(r$label),
        htmltools::tags$td(
          htmltools::span(class = if (!is.null(lvl)) paste0("lvl-", lvl),
                          if (!is.null(lvl)) paste0(.export_cap(lvl), ": "),
                          paste(r$value, collapse = " ")),
          if (length(r$items)) htmltools::tags$ul(lapply(r$items, htmltools::tags$li))
        )
      )
    }))
  ))
}

#' Write the compiled report as one self-contained HTML file
#' @inheritParams export_report_docx
#' @keywords internal
export_report_html <- function(st, path) {
  meta   <- .export_report_meta(st)
  blocks <- .export_report_blocks(st)
  n_h1   <- 0L
  toc    <- list()
  body   <- list()

  for (bl in blocks) {
    body[[length(body) + 1L]] <- switch(bl$type,
      h1 = {
        n_h1 <- n_h1 + 1L
        anchor <- paste0("s", n_h1)
        toc[[length(toc) + 1L]] <- htmltools::tags$li(htmltools::tags$a(href = paste0("#", anchor), bl$text))
        htmltools::tags$h1(class = "section", id = anchor, bl$text)
      },
      h2       = htmltools::tags$h2(bl$text),
      para     = htmltools::tags$p(bl$text),
      sections = .export_sections_html(bl$sections),
      table    = htmltools::tagList(htmltools::div(class = "caption", bl$caption), .export_ft_html(bl$ft)),
      figure   = {
        png <- tempfile(fileext = ".png")
        ggplot2::ggsave(png, plot = bl$plot, width = bl$size[1], height = bl$size[2],
                        units = "in", dpi = 150, bg = "white")
        uri <- base64enc::dataURI(file = png, mime = "image/png")
        unlink(png)
        htmltools::tags$figure(htmltools::div(class = "caption", bl$caption),
                               htmltools::tags$img(src = uri, alt = bl$caption))
      },
      NULL)
  }

  page <- htmltools::tags$html(
    lang = "en",
    htmltools::tags$head(
      htmltools::tags$meta(charset = "utf-8"),
      htmltools::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      htmltools::tags$title(meta$title),
      htmltools::tags$style(htmltools::HTML(.EXPORT_REPORT_CSS))
    ),
    htmltools::tags$body(htmltools::div(
      class = "page",
      htmltools::tags$h1(class = "title", meta$title),
      htmltools::div(class = "subtitle", meta$subtitle),
      htmltools::tags$table(class = "facts", lapply(names(meta$facts), function(nm) {
        htmltools::tags$tr(htmltools::tags$td(nm), htmltools::tags$td(meta$facts[[nm]]))
      })),
      htmltools::tags$nav(class = "toc", htmltools::tags$strong("Contents"), htmltools::tags$ol(toc)),
      body
    ))
  )
  writeLines(c("<!DOCTYPE html>", as.character(page)), path, useBytes = TRUE)
  invisible(path)
}
