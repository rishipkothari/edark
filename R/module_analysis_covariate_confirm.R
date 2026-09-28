#' Analysis Step 4 — Covariate Confirmation Module
#'
#' UI and server for Step 4 of the Analysis workflow: final covariate
#' selection and reference levels. Full implementation per PRD §5.3 Step 4,
#' §6.6, §9.7–9.8. All candidates start unchecked; "Add" only adds checks and
#' "Replace" swaps the selection for a method's list; role variables (outcome,
#' exposure, clusters) are locked rows at the top; reference levels are chosen
#' from the levels that survive listwise deletion for the current selection.
#'
#' There is no Confirm step: every change is written to \code{analysis_spec}
#' as it happens. The first change after a model has been fitted asks before
#' clearing the model (\code{reset_analysis_pipeline(shared_state, 4)});
#' Cancel undoes the click in the table.
#'
#' The table is rendered once per structural change (roles, Step 3 results).
#' Checkbox clicks do not re-render it — the server pushes a patch message
#' (checked state, row cost, reference-level options) that JS applies in
#' place, so search text and scroll position survive. reactable remounts rows
#' when a search filter is cleared, so JS keeps the last patch and re-applies
#' it whenever rows are added to the DOM.
#'
#' Cells therefore carry no state: checkboxes have no \code{checked}, the
#' row-cost span and the reference-level \code{<select>} are rendered empty.
#' reactable turns cell tags into React elements, so (a) valueless attributes
#' such as \code{checked = NA} serialise to \code{null} and crash its renderer
#' (blank table), and (b) JS must only fill elements React left empty — editing
#' children React created breaks its next reconciliation.
#'
#' @param id Character. Module namespace ID.
#' @param shared_state A Shiny \code{reactiveValues} object.
#'
#' @importFrom magrittr %>%
#'
#' @name module_analysis_covariate_confirm
NULL


# Role rows locked at the top of the table, in display order (cluster
# variables follow these).
.CC_LOCKED_ROLES <- c(
  outcome_variable  = "Outcome",
  exposure_variable = "Exposure"
)

# Grouping variables: random-effect factors, so a reference level is meaningless.
.CC_GROUPING_ROLES <- "Cluster"

.CC_METHODS <- c(univariable = "Univariable", stepwise = "Stepwise", lasso = "LASSO")


.cc_js <- function(ns) {
  shiny::tags$script(shiny::HTML(paste0("
(function() {
  var NS = '", ns(""), "';
  var lastPatch = null;

  function send(ev) {
    Shiny.setInputValue(NS + 'cc_event', ev, { priority: 'event' });
  }

  $(document).on('change', '.edark-cc-include', function() {
    send({ type: 'toggle', var: this.getAttribute('data-var'), value: this.checked });
  });

  $(document).on('change', '.edark-cc-reflevel', function() {
    send({ type: 'ref', var: this.getAttribute('data-var'), value: $(this).val() });
  });

  $(document).on('click', '.edark-cc-action', function(e) {
    e.preventDefault();
    e.stopPropagation();
    if (this.disabled) return;
    send({ type: this.getAttribute('data-action'),
            method: this.getAttribute('data-method') || '' });
  });

  // Apply a patch to whatever rows are currently in the DOM. Every write is
  // guarded so a second pass changes nothing (keeps the MutationObserver quiet).
  function applyPatch(p) {
    if (!p) return;
    p.rows.forEach(function(r) {
      var sel = '[data-var=\"' + CSS.escape(r.var) + '\"]';

      var cb = document.querySelector('.edark-cc-include' + sel);
      if (cb && cb.checked !== r.checked) cb.checked = r.checked;

      var cost = document.querySelector('.edark-cc-cost' + sel);
      if (cost) {
        if (cost.textContent !== r.cost) cost.textContent = r.cost;
        if (cost.className !== r.cost_class) cost.className = r.cost_class;
      }

      var dd = document.querySelector('.edark-cc-reflevel' + sel);
      if (dd) {
        var cur = Array.from(dd.options).map(function(o) { return o.value; });
        if (cur.join('\\u0001') !== r.levels.join('\\u0001')) {
          dd.innerHTML = '';
          r.levels.forEach(function(l) {
            var o = document.createElement('option');
            o.value = l; o.textContent = l;
            dd.appendChild(o);
          });
        }
        if (dd.value !== r.ref) dd.value = r.ref;
      }

      var note = document.querySelector('.edark-cc-refnote' + sel);
      if (note) {
        var disp = r.ref_note ? 'inline' : 'none';
        if (note.style.display !== disp) note.style.display = disp;
        if ((note.getAttribute('title') || '') !== (r.ref_note || '')) {
          note.setAttribute('title', r.ref_note || '');
        }
      }
    });
  }

  Shiny.addCustomMessageHandler(NS + 'cc_patch', function(p) {
    lastPatch = p;
    applyPatch(p);
  });

  $(function() {
    var wrap = document.getElementById(NS + 'cc_table_wrap');
    if (!wrap) return;
    var pending = false;
    new MutationObserver(function() {
      if (pending) return;
      pending = true;
      requestAnimationFrame(function() { pending = false; applyPatch(lastPatch); });
    }).observe(wrap, { childList: true, subtree: true });
  });
})();
")))
}


#' @rdname module_analysis_covariate_confirm
#' @export
analysis_covariate_confirm_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    .cc_js(ns),
    # This page's configuration is the table: one row per candidate, with its
    # own checkbox and reference-level control. That is the shape the contract
    # asks for (table-shaped config stays in the centre), so the config pane
    # carries what the table cannot - how to read it - rather than a duplicate
    # set of controls. It is the thinnest config pane in the app; if global
    # table actions are ever added, they belong here.
    edark_page(
      config = shiny::tagList(
        edark_section_label("How to use this table", first = TRUE),
        shiny::tags$ul(
          class = "small text-muted ps-3",
          shiny::tags$li("Tick a variable to keep it in the model."),
          shiny::tags$li("Set each factor's reference level - every effect is reported against it."),
          shiny::tags$li(
            "The method columns carry Step 3's suggestions. ",
            shiny::tags$strong("Add"), " unions them into your selection; ",
            shiny::tags$strong("Replace"), " overwrites it."
          )
        ),
        shiny::tags$p(
          class = "small text-muted mb-0",
          "Sample size and the checks on the right update as you tick."
        )
      ),
      # main_message stays *outside* cc_table_wrap: .cc_js() hangs a
      # MutationObserver on that div to re-patch the embedded inputs, and
      # anything else re-rendering inside it would wake the observer for no
      # reason (§N1.4).
      result = shiny::tagList(
        shiny::uiOutput(ns("main_message")),
        shiny::div(
          id = ns("cc_table_wrap"),
          reactable::reactableOutput(ns("cc_table"))
        )
      ),
      messages  = edark_messages_ui(ns),
      info      = shiny::uiOutput(ns("sample_ui"))
    )
  )
}


#' @rdname module_analysis_covariate_confirm
#' @export
analysis_covariate_confirm_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # ── Local staged state ───────────────────────────────────────────────────
    # covariates: checked candidates. refs: the user's preferred reference
    # level per variable (the effective level may differ if it drops out).
    # key: the roles_key() this selection belongs to — the commit observer
    # only writes a selection made for the current roles.
    staged <- shiny::reactiveVal(list(key = NULL, covariates = character(0), refs = list()))
    pending_replace <- shiny::reactiveVal(NULL)
    pending_change  <- shiny::reactiveVal(NULL)

    .set_staged <- function(covariates = NULL, refs = NULL, key = NULL) {
      cur <- shiny::isolate(staged())
      new <- cur
      if (!is.null(covariates)) new$covariates <- covariates
      if (!is.null(refs))       new$refs       <- refs
      if (!is.null(key))        new$key        <- key
      if (!identical(cur, new)) staged(new)
    }

    # ── Roles (structure of the table) ───────────────────────────────────────
    roles <- shiny::reactive({
      spec  <- shared_state$analysis_spec
      adata <- shared_state$analysis_data
      if (is.null(spec) || is.null(adata)) return(NULL)
      vr <- spec$variable_roles
      if (is.null(vr$outcome_variable)) return(NULL)

      locked <- list()
      for (field in names(.CC_LOCKED_ROLES)) {
        v <- vr[[field]]
        if (!is.null(v) && nzchar(v) && v %in% names(adata)) {
          locked[[length(locked) + 1L]] <- list(var = v, role = .CC_LOCKED_ROLES[[field]])
        }
      }
      clusters <- intersect(vr$cluster_variables, names(adata))
      for (v in clusters) {
        locked[[length(locked) + 1L]] <- list(var = v, role = "Cluster")
      }
      locked_vars <- vapply(locked, `[[`, character(1), "var")
      exposure    <- vr$exposure_variable

      list(
        outcome    = vr$outcome_variable,
        exposure   = if (!is.null(exposure) && nzchar(exposure)) exposure else NULL,
        locked     = locked,
        clusters   = clusters,
        candidates = setdiff(intersect(vr$candidate_covariates, names(adata)), locked_vars)
      )
    })

    # Step 1 bumps roles_version on every role write; freeze sets created_at.
    # Either one means the table's structure changed → start over.
    roles_key <- shiny::reactive({
      spec <- shared_state$analysis_spec
      list(spec$specification_metadata$created_at,
           spec$specification_metadata$roles_version)
    })

    shiny::observe({
      key <- roles_key()
      if (identical(key, shiny::isolate(staged())$key)) return()
      spec <- shiny::isolate(shared_state$analysis_spec)
      refs <- if (!is.null(spec)) spec$variable_roles$reference_levels else list()

      # Session load (§M8.7): Step 1 has just applied the session's roles, so
      # start from the session's covariates instead of none - but only if the
      # roles in the spec are the ones the covariates were saved with.
      covs <- character(0)
      sr   <- shiny::isolate(shared_state$session_restore)
      if (isTRUE(sr$step1_done) && !is.null(spec)) {
        vr <- spec$variable_roles
        .same <- function(a, b) setequal(as.character(a), as.character(b))
        if (.same(vr$outcome_variable,     sr$roles$outcome_variable) &&
            .same(vr$exposure_variable,    sr$roles$exposure_variable) &&
            .same(vr$candidate_covariates, sr$roles$candidate_covariates) &&
            .same(vr$cluster_variables,    sr$roles$cluster_variables)) {
          covs <- intersect(as.character(sr$covariates), vr$candidate_covariates)
        }
        shared_state$session_restore <- NULL
      }

      .set_staged(covariates = covs, refs = if (is.null(refs)) list() else refs,
                  key = key)
    })

    # The rows covariates are chosen on: the training set when Step 1 set a
    # train/test split. A reactiveVal, so it only changes with the split.
    split_spec <- shiny::reactiveVal(NULL)
    shiny::observe(split_spec(list(
      purpose_specification = shared_state$analysis_spec$purpose_specification)))
    model_data <- shiny::reactive({
      adata <- shared_state$analysis_data
      if (is.null(adata)) return(NULL)
      analysis_model_data(split_spec(), adata)
    })

    # ── Live sample summary (recomputed on every click) ──────────────────────
    sample_info <- shiny::reactive({
      r     <- roles()
      adata <- model_data()
      if (is.null(r) || is.null(adata)) return(NULL)
      compute_covariate_sample(
        adata,
        outcome      = r$outcome,
        exposure     = r$exposure,
        covariates   = intersect(staged()$covariates, r$candidates),
        candidates   = r$candidates,
        cluster_vars = r$clusters
      )
    })

    # Effective reference level per factor variable: the preferred level if it
    # survives listwise deletion, else the first surviving level.
    effective_refs <- shiny::reactive({
      si <- sample_info()
      if (is.null(si)) return(list())
      prefs <- staged()$refs
      out <- list()
      for (v in names(si$levels)) {
        lv   <- si$levels[[v]]
        if (length(lv) == 0L) next
        pref <- prefs[[v]]
        eff  <- if (!is.null(pref) && pref %in% lv) pref else lv[1]
        out[[v]] <- list(
          ref  = eff,
          note = if (!is.null(pref) && !pref %in% lv) {
            sprintf("'%s' is not present in the complete rows; using '%s'.", pref, eff)
          } else ""
        )
      }
      out
    })

    # Variables whose reference level goes into the spec on Confirm
    ref_targets <- shiny::reactive({
      r <- roles()
      if (is.null(r)) return(character(0))
      locked <- Filter(function(l) !l$role %in% .CC_GROUPING_ROLES, r$locked)
      c(vapply(locked, `[[`, character(1), "var"),
        intersect(staged()$covariates, r$candidates))
    })

    # ── Step 3 method results, summarised per variable ───────────────────────
    method_info <- shiny::reactive({
      res  <- shared_state$analysis_result$variable_investigation
      spec <- shared_state$analysis_spec
      vsel <- spec$variable_selection_specification

      univ <- res$univariable
      univ_info <- if (!is.null(univ)) {
        excl <- attr(univ, "excluded_variables")
        if (is.null(excl)) excl <- res$univariable_excluded
        sugg <- unique(univ$variable[univ$suggested])
        pmin <- vapply(split(univ$p.value, univ$variable), function(p) {
          if (all(is.na(p))) NA_real_ else min(p, na.rm = TRUE)
        }, numeric(1))
        list(run = TRUE, suggested = sugg, p = pmin,
             modelled = unique(univ$variable), excluded = excl,
             params = sprintf("P-value threshold: %s", vsel$univariable_p_threshold),
             label  = "Univariable")
      } else list(run = FALSE)

      .sl_info <- function(x, params, label) {
        if (is.null(x)) return(list(run = FALSE))
        if (!is.null(x$error)) {
          return(list(run = FALSE, error = x$error, label = label))
        }
        excl <- x$excluded_variables
        list(run = TRUE, suggested = x$selected_variables, excluded = excl,
             modelled = NULL, params = params, label = label)
      }

      sw <- res$stepwise
      la <- res$lasso
      list(
        univariable = univ_info,
        stepwise = .sl_info(sw, if (!is.null(sw)) sprintf(
          "Direction: %s\nCriterion: %s", sw$direction, sw$criterion),
          sprintf("Stepwise (%s, %s)", sw$direction, sw$criterion)),
        lasso = .sl_info(la, if (!is.null(la)) sprintf(
          "Lambda: %s (%s)\nSeed: %s", la$lambda_type,
          if (is.numeric(la$lambda_selected)) signif(la$lambda_selected, 3) else "?",
          la$seed %||% "?"),
          sprintf("LASSO (%s)", la$lambda_type))
      )
    })

    # ── Commit: write the live selection into the spec ───────────────────────
    # Runs on every change to the selection or the levels that survive it.
    # Errors (e.g. a factor collapsing to one level) are written anyway —
    # Step 5's preflight blocks the model; the Checks panel here explains why.
    shiny::observe({
      st   <- staged()
      r    <- roles()
      eff  <- effective_refs()
      tgts <- ref_targets()
      if (is.null(r) || !identical(st$key, roles_key())) return()

      spec <- shiny::isolate(shared_state$analysis_spec)
      if (is.null(spec)) return()

      covs <- intersect(r$candidates, st$covariates)
      covs <- if (length(covs) > 0L) covs else NULL
      refs <- spec$variable_roles$reference_levels
      if (is.null(refs)) refs <- list()
      for (v in intersect(tgts, names(eff))) refs[[v]] <- eff[[v]]$ref

      new <- spec
      new$variable_roles$final_model_covariates                <- covs
      new$variable_roles$reference_levels                      <- refs
      new$variable_selection_specification$selected_variables  <- covs
      if (!identical(new, spec)) shared_state$analysis_spec <- new
    })

    # ── Table ────────────────────────────────────────────────────────────────
    # roles() and method_info() recompute on every analysis_spec write
    # (including this module's commits). A reactiveVal only invalidates when its value
    # actually changes, so routing them through one keeps the table from
    # re-rendering (and losing search/scroll) on unrelated spec writes.
    table_struct <- shiny::reactiveVal(NULL)
    shiny::observe({
      table_struct(list(roles = roles(), methods = method_info(), rows = split_spec()))
    })

    output$main_message <- shiny::renderUI({
      spec  <- shared_state$analysis_spec
      adata <- shared_state$analysis_data
      msg <- if (is.null(spec) || is.null(adata)) {
        edark_lock_reason("analysis_start")
      } else if (is.null(spec$variable_roles$outcome_variable)) {
        "Assign an outcome in Step 1 to select covariates."
      }
      if (!is.null(msg)) {
        return(shiny::div(
          class = "text-center text-muted mt-5",
          shiny::icon("list-check", style = "font-size:2rem; opacity:0.3;"),
          shiny::tags$p(class = "mt-2 fst-italic", msg)
        ))
      }
      r <- roles()
      shiny::tagList(
        shiny::tags$p(
          class = "text-muted small mb-2",
          "Check the covariates to include in the model - selections are saved as",
          "you go. Role variables from Step 1 are locked at the top. Reference levels",
          "list only the levels present in the rows that remain after missing data",
          "are removed."
        ),
        if (length(r$candidates) == 0L) {
          shiny::div(
            class = "alert alert-secondary py-2 small mb-2",
            shiny::icon("circle-info"),
            " No candidate covariates were assigned in Step 1. The model will",
            " use the outcome and exposure alone."
          )
        }
      )
    })

    output$cc_table <- reactable::renderReactable({
      ts <- table_struct()
      r  <- ts$roles
      mi <- ts$methods
      if (is.null(r)) return(NULL)

      adata <- shiny::isolate(model_data())
      ctypes <- shiny::isolate(shared_state$column_types)
      n     <- nrow(adata)

      locked_vars  <- vapply(r$locked, `[[`, character(1), "var")
      locked_roles <- vapply(r$locked, `[[`, character(1), "role")
      vars  <- c(locked_vars, r$candidates)
      roles_col <- c(locked_roles, rep("", length(r$candidates)))

      n_miss <- vapply(vars, function(v) sum(is.na(adata[[v]])), integer(1))

      df <- data.frame(
        include  = vars,
        Variable = vars,
        Type     = vapply(vars, function(v) {
          if (!is.null(ctypes) && v %in% names(ctypes)) ctypes[[v]] else class(adata[[v]])[1]
        }, character(1)),
        missing  = n_miss,
        cost     = vars,
        univariable = vars,
        stepwise = vars,
        lasso    = vars,
        ref      = vars,
        stringsAsFactors = FALSE,
        row.names = NULL
      )

      # ── Method column: header with Add/Replace + tooltip, coloured cells ──
      .method_state <- function(key, v) {
        info <- mi[[key]]
        if (!isTRUE(info$run)) return(list(state = "not_run"))
        excl <- info$excluded
        if (!is.null(excl) && nrow(excl) > 0L && v %in% excl$variable) {
          return(list(state = "na", reason = excl$reason[excl$variable == v][1]))
        }
        if (!is.null(info$modelled) && !v %in% info$modelled) {
          return(list(state = "na", reason = "not in the candidate pool when this method was run"))
        }
        list(state = if (v %in% info$suggested) "yes" else "no",
             p = if (!is.null(info$p) && v %in% names(info$p)) info$p[[v]] else NULL)
      }

      .fmt_p <- function(p) {
        if (is.null(p) || is.na(p)) return("")
        fp <- edark_format_p(p)
        if (startsWith(fp, "<")) paste0(" p", gsub(" ", "", fp)) else paste0(" p=", fp)
      }

      .method_col <- function(key) {
        info  <- mi[[key]]
        label <- .CC_METHODS[[key]]
        ran   <- isTRUE(info$run)
        tip   <- if (ran) {
          paste0("Parameters:\n", info$params)
        } else if (!is.null(info$error)) {
          paste0("Failed: ", info$error)
        } else {
          "Not run. Return to Step 3 to run this method."
        }
        .btn <- function(action, text) {
          b <- htmltools::tags$button(
            text,
            class         = "edark-cc-action btn btn-outline-secondary btn-sm py-0 px-1",
            style         = "font-size:0.7rem;",
            `data-action` = action,
            `data-method` = key
          )
          if (!ran) b <- htmltools::tagAppendAttributes(b, disabled = "disabled")
          b
        }

        reactable::colDef(
          header = htmltools::tags$div(
            class = "d-flex flex-column align-items-center gap-1",
            htmltools::tags$span(
              style = "font-size:0.75rem;",
              label, " ",
              htmltools::tags$span(title = tip, style = "cursor:help;",
                                   shiny::icon("circle-question", class = "text-muted"))
            ),
            htmltools::tags$div(class = "d-flex gap-1",
                                .btn("add", "Add"), .btn("replace", "Replace"))
          ),
          minWidth = 110,
          sortable = FALSE,
          cell = function(value, index) {
            if (index <= length(locked_vars)) return("")
            s <- .method_state(key, value)
            switch(s$state,
              yes = htmltools::tags$span(class = "text-success fw-semibold",
                                         paste0("\u2713", .fmt_p(s$p))),
              no  = htmltools::tags$span(class = "text-danger", "-"),
              na  = htmltools::tags$span(class = "text-muted",
                                         title = if (is.na(s$reason)) "" else s$reason,
                                         style = "cursor:help;", "n/a"),
              htmltools::tags$span(class = "text-muted", "-")
            )
          },
          style = function(value, index) {
            if (index <= length(locked_vars)) return(NULL)
            bg <- switch(.method_state(key, value)$state,
                         yes = "#d1e7dd", no = "#f8d7da", "#e9ecef")
            list(background = bg)
          }
        )
      }

      reactable::reactable(
        df,
        height     = EDARK_RESULT_HEIGHT,
        searchable = TRUE,
        pagination = FALSE,
        highlight  = TRUE,
        compact    = TRUE,
        defaultColDef = reactable::colDef(align = "center", vAlign = "center",
                                          minWidth = 60),
        rowStyle = function(index) {
          if (index <= length(locked_vars)) list(background = "#f8f9fa")
        },
        columns = list(
          include = reactable::colDef(
            header = htmltools::tags$div(
              class = "d-flex flex-column align-items-center",
              htmltools::tags$span(style = "font-size:0.75rem;", "Include"),
              htmltools::tags$div(
                class = "d-flex gap-2",
                htmltools::tags$button("All", `data-action` = "all",
                  class = "edark-cc-action btn btn-link btn-sm p-0 text-muted",
                  style = "font-size:0.7rem; line-height:1;"),
                htmltools::tags$button("Clear", `data-action` = "clear",
                  class = "edark-cc-action btn btn-link btn-sm p-0 text-muted",
                  style = "font-size:0.7rem; line-height:1;")
              )
            ),
            minWidth = 70,
            sortable = FALSE,
            searchable = FALSE,
            cell = function(value, index) {
              # Checked state comes from the patch (see module docs)
              cb <- htmltools::tags$input(
                type       = "checkbox",
                class      = "edark-cc-include form-check-input",
                `data-var` = value
              )
              if (index <= length(locked_vars)) {
                cb <- htmltools::tagAppendAttributes(
                  cb, disabled = "disabled", title = "Set by role in Step 1")
              }
              cb
            }
          ),
          Variable = reactable::colDef(
            align = "left", minWidth = 170,
            cell = function(value, index) {
              if (index > length(locked_vars)) return(value)
              htmltools::tagList(
                value, " ",
                edark_badge(roles_col[index], role = "role")
              )
            }
          ),
          Type = reactable::colDef(
            minWidth = 80, cell = function(value, index) edark_type_badge(value)
          ),
          missing = reactable::colDef(
            name = "Missing", minWidth = 80,
            cell = function(value, index) {
              if (value == 0L) return(htmltools::tags$span(class = "text-muted", "0"))
              sprintf("%d (%d%%)", value, round(value / n * 100))
            }
          ),
          cost = reactable::colDef(
            header = htmltools::tags$span(
              title = paste(
                "Rows dropped because of this variable's missing values.",
                "Checked: rows it is costing now. Unchecked: rows you would",
                "lose by adding it. Cluster: rows lost to the mixed model.",
                sep = "\n"),
              style = "cursor:help; font-size:0.75rem;",
              "Row cost ", shiny::icon("circle-question", class = "text-muted")
            ),
            minWidth = 80,
            sortable = FALSE,
            cell = function(value, index) {
              htmltools::tags$span(class = "edark-cc-cost", `data-var` = value)
            }
          ),
          univariable = .method_col("univariable"),
          stepwise    = .method_col("stepwise"),
          lasso       = .method_col("lasso"),
          ref = reactable::colDef(
            name = "Ref. level", minWidth = 150, align = "left", sortable = FALSE,
            cell = function(value, index) {
              role <- roles_col[index]
              if (role %in% .CC_GROUPING_ROLES) {
                return(htmltools::tags$span(class = "text-muted small",
                                            "grouping - no reference"))
              }
              if (!is.factor(adata[[value]])) {
                return(htmltools::tags$span("-", class = "text-muted"))
              }
              # Options and selection come from the patch (see module docs)
              htmltools::tags$div(
                class = "d-flex align-items-center gap-1",
                htmltools::tags$select(
                  class      = "edark-cc-reflevel form-select form-select-sm",
                  `data-var` = value,
                  style      = "font-size:0.8rem;"
                ),
                htmltools::tags$span(
                  class      = "edark-cc-refnote text-warning",
                  `data-var` = value,
                  style      = "cursor:help; display:none;",
                  shiny::icon("triangle-exclamation")
                )
              )
            }
          )
        )
      )
    })

    # ── Patch: push live state into the rendered table ───────────────────────
    patch <- shiny::reactive({
      r  <- roles()
      si <- sample_info()
      if (is.null(r) || is.null(si)) return(NULL)
      st  <- staged()
      eff <- effective_refs()
      n   <- si$n_total

      locked_vars <- vapply(r$locked, `[[`, character(1), "var")
      oe_vars     <- c(r$outcome, r$exposure)

      rows <- lapply(c(locked_vars, r$candidates), function(v) {
        ct <- .cc_cost_text(v, si, n, v %in% st$covariates, v %in% oe_vars)
        e  <- eff[[v]]
        list(
          var        = v,
          checked    = v %in% locked_vars || v %in% st$covariates,
          cost       = ct$text,
          cost_class = ct$class,
          levels     = as.list(if (is.null(si$levels[[v]])) character(0) else si$levels[[v]]),
          ref        = if (!is.null(e)) e$ref else "",
          ref_note   = if (!is.null(e)) e$note else ""
        )
      })
      list(rows = rows)
    })

    .push_patch <- function(p) {
      if (!is.null(p)) session$sendCustomMessage(ns("cc_patch"), p)
    }
    shiny::observe(.push_patch(patch()))

    # ── Selection changes ────────────────────────────────────────────────────
    # Every edit goes through here. Once a model has been fitted, the first
    # edit asks before clearing it; Cancel re-sends the current patch, which
    # puts back whatever the click already changed in the browser.
    .propose <- function(covariates = NULL, refs = NULL, on_apply = NULL) {
      st  <- shiny::isolate(staged())
      new <- st
      if (!is.null(covariates)) new$covariates <- covariates
      if (!is.null(refs))       new$refs       <- refs
      if (identical(new, st)) {
        .push_patch(shiny::isolate(patch()))
        return(invisible(FALSE))
      }

      res <- shiny::isolate(shared_state$analysis_result)
      if (is.null(res$fitted_models$primary_model)) {
        staged(new)
        if (!is.null(on_apply)) on_apply()
        return(invisible(TRUE))
      }

      pending_change(list(staged = new, on_apply = on_apply))
      shiny::showModal(shiny::modalDialog(
        title = "Clear Model Results?",
        shiny::p("A model has already been fitted with the current covariates.",
                 "Changing them will clear the fitted model, diagnostics and results.",
                 "Table 1 and variable investigation are kept."),
        shiny::tags$small(class = "text-muted",
                          "Cancel undoes your change and keeps everything as it was."),
        footer = shiny::tagList(
          edark_button(ns, "cancel_change", "Cancel", variant = "secondary", size = "dialog"),
          edark_button(ns, "confirm_change", "Clear & Continue", variant = "warning", size = "dialog")
        ),
        easyClose = FALSE
      ))
      invisible(FALSE)
    }

    shiny::observeEvent(input$confirm_change, {
      shiny::removeModal()
      pc <- pending_change()
      pending_change(NULL)
      if (is.null(pc)) return()
      reset_analysis_pipeline(shared_state, from_step = 4L)
      staged(pc$staged)
      if (!is.null(pc$on_apply)) pc$on_apply()
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$cancel_change, {
      shiny::removeModal()
      pending_change(NULL)
      .push_patch(shiny::isolate(patch()))
    }, ignoreInit = TRUE)

    # ── Events from the table ────────────────────────────────────────────────
    shiny::observeEvent(input$cc_event, {
      ev <- input$cc_event
      r  <- roles()
      if (is.null(r)) return()
      st <- staged()

      switch(ev$type,
        toggle = {
          if (!ev$var %in% r$candidates) return()
          covs <- if (isTRUE(ev$value)) union(st$covariates, ev$var)
                  else setdiff(st$covariates, ev$var)
          .propose(covariates = intersect(r$candidates, covs))
        },
        ref = {
          refs <- st$refs
          refs[[ev$var]] <- ev$value
          .propose(refs = refs)
        },
        all   = .propose(covariates = r$candidates),
        clear = .propose(covariates = character(0)),
        add = {
          info <- method_info()[[ev$method]]
          if (!isTRUE(info$run)) return()
          added <- setdiff(intersect(info$suggested, r$candidates), st$covariates)
          if (length(added) == 0L) {
            shiny::showNotification(sprintf("%s: nothing new to add.", info$label),
                                    type = "message", duration = 4)
            return()
          }
          .propose(
            covariates = intersect(r$candidates, c(st$covariates, added)),
            on_apply   = function() shiny::showNotification(
              sprintf("%s: added %d variable%s.", info$label, length(added),
                      if (length(added) == 1L) "" else "s"),
              type = "message", duration = 4)
          )
        },
        replace = {
          info <- method_info()[[ev$method]]
          if (!isTRUE(info$run)) return()
          new_set  <- intersect(r$candidates, info$suggested)
          removed  <- setdiff(st$covariates, new_set)
          added    <- setdiff(new_set, st$covariates)
          pending_replace(list(method = ev$method, covariates = new_set))

          body <- if (length(new_set) == 0L) {
            shiny::div(class = "alert alert-warning py-2",
              shiny::icon("triangle-exclamation"),
              sprintf(" %s selected no variables. Replacing will uncheck all covariates.",
                      info$label))
          } else {
            shiny::tagList(
              shiny::p(sprintf("Your selection will be replaced by the %d variable%s selected by %s.",
                               length(new_set), if (length(new_set) == 1L) "" else "s",
                               info$label)),
              .cc_var_list("Will be unchecked", removed),
              .cc_var_list("Will be checked", added)
            )
          }
          shiny::showModal(shiny::modalDialog(
            title = sprintf("Replace selection with %s?", info$label),
            body,
            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
              edark_button(ns, "confirm_replace", "Replace", variant = "warning", size = "dialog")
            ),
            easyClose = TRUE
          ))
        }
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$confirm_replace, {
      shiny::removeModal()
      pr <- pending_replace()
      pending_replace(NULL)
      if (!is.null(pr)) .propose(covariates = pr$covariates)
    }, ignoreInit = TRUE)

    # ── Sidebar ──────────────────────────────────────────────────────────────
    output$sample_ui <- shiny::renderUI({
      si <- sample_info()
      r  <- roles()
      if (is.null(si) || is.null(r)) return(NULL)
      n <- si$n_total

      .row <- function(label, val, cls = "") {
        shiny::div(
          class = "d-flex justify-content-between mb-1",
          shiny::tags$span(class = "text-muted small", label),
          shiny::tags$span(class = paste("small fw-semibold", cls), val)
        )
      }
      .pct <- function(k) sprintf("%d (%d%%)", k, if (n > 0) round(k / n * 100) else 0L)

      dropped <- n - si$n_fixed
      base_lbl <- if (is.null(r$exposure)) "Outcome complete" else "Outcome + exposure complete"
      n_cov <- length(intersect(staged()$covariates, r$candidates))

      shiny::tagList(
        edark_section_label("Model"),
        .row("Covariates selected", n_cov),
        .row("Parameters", si$n_params),
        edark_section_label("Sample"),
        .row("Total rows", n),
        .row(base_lbl, .pct(si$n_base)),
        .row("With selected covariates", .pct(si$n_fixed)),
        .row("Rows dropped", .pct(dropped),
             if (n > 0 && dropped / n > 0.2) "text-warning" else ""),
        if (!is.na(si$n_mixed)) .row("If mixed model", .pct(si$n_mixed)),
        if (!is.null(si$outcome_counts) && length(si$outcome_counts) > 0L) {
          .row(sprintf("Outcome (%s)", r$outcome),
               paste(sprintf("%s: %d", names(si$outcome_counts), si$outcome_counts),
                     collapse = " \u00b7 "))
        },
        if (!is.na(si$epv)) {
          .row("Events per parameter", sprintf("%.1f", si$epv),
               if (si$epv < 10) "text-warning" else "")
        }
      )
    })

    # -- Messages: the checks, in the page's one messages slot (D3) -----------
    # These used to sit under the sample summary in the right sidebar, mixing
    # neutral facts with warnings in one stack (§BUILD_UI-redesign 2.2). The
    # info pane keeps the counts; the problems come here.
    edark_messages_server(output, shiny::reactive({
      si <- sample_info()
      if (is.null(si)) return(NULL)
      iss <- si$issues
      if (nrow(iss) == 0L) return(NULL)

      ord <- order(match(iss$level, c("error", "warning", "note")))
      lvl <- c(error = "error", warning = "warn", note = "info")

      msgs <- lapply(ord, function(i) {
        edark_message(unname(lvl[[iss$level[i]]]), iss$message[i])
      })
      if (any(iss$level == "error")) {
        msgs <- c(msgs, list(edark_message(
          "error", "Resolve the errors above before running the model."
        )))
      }
      msgs
    }))
  })
}


# Row-cost cell text and class. `oe` = outcome/exposure rows, which are part
# of the baseline and have no cost of their own.
.cc_cost_text <- function(v, si, n, checked, oe) {
  if (oe || is.null(si) || !v %in% names(si$row_cost)) {
    return(list(text = "-", class = "edark-cc-cost text-muted"))
  }
  k <- si$row_cost[[v]]
  if (k == 0L) return(list(text = "0", class = "edark-cc-cost text-muted"))
  frac <- if (n > 0) k / n else 0
  tone <- if (frac >= 0.2) "text-danger fw-semibold"
          else if (frac >= 0.05) "text-warning-emphasis"
          else ""
  list(text  = paste0("\u2212", k),
       class = paste("edark-cc-cost", tone, if (!checked) "fst-italic"))
}

.cc_var_list <- function(heading, vars) {
  if (length(vars) == 0L) return(NULL)
  shiny::tagList(
    shiny::tags$p(class = "small fw-semibold mb-1", heading),
    shiny::tags$p(class = "small", paste(vars, collapse = ", "))
  )
}
