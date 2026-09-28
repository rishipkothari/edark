#' Analysis Step 1 — Setup Module
#'
#' UI and server for Step 1 of the Analysis workflow: dataset freeze,
#' variable role assignment, and study type derivation.
#' Full implementation per PRD §5.3 Step 1, §6.3, §1.4, §3.4, §3.5.
#'
#' @param id Character. Module namespace ID.
#' @param shared_state A Shiny \code{reactiveValues} object.
#'
#' @importFrom magrittr %>%
#'
#' @name module_analysis_setup
NULL


# Build the JS block for role table interactivity.
# Injects the module namespace so Shiny.setInputValue targets the right input.
.setup_role_js <- function(ns) {
  shiny::tags$script(shiny::HTML(paste0("
(function() {
  // One role per variable. Outcome and exposure are single-select (radios);
  // candidate and cluster are multi-select (checkboxes).
  var NS = '", ns(""), "';

  function varSel(varName) { return '[data-var=\"' + CSS.escape(varName) + '\"]'; }

  // Uncheck every role input for this variable except `keep`
  function clearOtherRoles(varName, keep) {
    document.querySelectorAll(
      '.edark-role-radio' + varSel(varName) + ', .edark-role-checkbox' + varSel(varName)
    ).forEach(function(el) { if (el !== keep) el.checked = false; });
  }

  // Radio role changed
  $(document).on('change', '.edark-role-radio', function() {
    if (!this.checked) return;
    var varName = $(this).data('var');
    clearOtherRoles(varName, this);
    Shiny.setInputValue(NS + 'role_change',
      { var: varName, role: $(this).data('role'), value: true },
      { priority: 'event' }
    );
  });

  // Checkbox role (candidate / cluster) changed
  $(document).on('change', '.edark-role-checkbox', function() {
    var varName = $(this).data('var');
    if (this.checked) clearOtherRoles(varName, this);
    Shiny.setInputValue(NS + 'role_change',
      { var: varName, role: $(this).data('role'), value: this.checked },
      { priority: 'event' }
    );
  });

  // Reference level dropdown changed
  $(document).on('change', '.edark-role-reflevel', function() {
    var varName = $(this).data('var');
    Shiny.setInputValue(NS + 'role_change',
      { var: varName, role: 'reference_level', value: $(this).val() },
      { priority: 'event' }
    );
  });

  // Column Clear button clicked
  $(document).on('click', '.edark-clear-role', function(e) {
    e.preventDefault();
    var role = $(this).data('role');
    document.querySelectorAll(
      '.edark-role-radio[data-role=\"' + role + '\"], .edark-role-checkbox[data-role=\"' + role + '\"]'
    ).forEach(function(el) { el.checked = false; });
    Shiny.setInputValue(NS + 'role_change',
      { var: '__clear__', role: role, value: false },
      { priority: 'event' }
    );
  });

  // Server-pushed role state: make every input match it
  var lastRoles = null;
  function applyRoles(rows) {
    if (!rows) return;
    rows.forEach(function(r) {
      var sel = varSel(r.var);
      document.querySelectorAll(
        '.edark-role-radio' + sel + ', .edark-role-checkbox' + sel
      ).forEach(function(el) {
        var want = el.getAttribute('data-role') === r.role;
        if (el.checked !== want) el.checked = want;
      });
      document.querySelectorAll('.edark-role-reflevel' + sel).forEach(function(el) {
        if (r.ref && el.value !== r.ref) el.value = r.ref;
      });
    });
  }
  Shiny.addCustomMessageHandler(NS + 'sync_roles', function(rows) {
    lastRoles = rows;
    applyRoles(rows);
  });
  $(function() {
    var wrap = document.getElementById(NS + 'main_content');
    if (!wrap) return;
    var pending = false;
    new MutationObserver(function() {
      if (pending) return;
      pending = true;
      requestAnimationFrame(function() { pending = false; applyRoles(lastRoles); });
    }).observe(wrap, { childList: true, subtree: true });
  });

  // Select All candidates button clicked
  $(document).on('click', '.edark-select-all-candidates', function(e) {
    e.preventDefault();
    document.querySelectorAll('.edark-role-checkbox[data-role=\"candidate\"]').forEach(function(el) {
      var sel = varSel(el.getAttribute('data-var'));
      var hasOtherRole = Array.from(document.querySelectorAll(
        '.edark-role-radio' + sel + ', .edark-role-checkbox' + sel
      )).some(function(other) { return other !== el && other.checked; });
      if (!hasOtherRole) el.checked = true;
    });
    Shiny.setInputValue(NS + 'role_change',
      { var: '__select_all_candidates__', role: 'candidate', value: true },
      { priority: 'event' }
    );
  });
})();
")))
}


#' @rdname module_analysis_setup
#' @export
analysis_setup_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    .setup_role_js(ns),
    # The sidebar used to be on the right and to mix config (actions, study
    # type, purpose) with info (incoming snapshot, role summary, selected
    # snapshot) in one stack (§BUILD_UI-redesign 2.2). Split per the page
    # contract, with the primary action last in the config pane (F3).
    edark_page(
      config = shiny::tagList(
        shiny::uiOutput(ns("study_type_ui")),
        shiny::uiOutput(ns("purpose_ui")),
        shiny::div(class = "mt-3", shiny::uiOutput(ns("action_buttons_ui")))
      ),
      result   = shiny::uiOutput(ns("main_content")),
      messages = edark_messages_ui(ns),
      info     = shiny::tagList(
        shiny::uiOutput(ns("incoming_snapshot_ui")),
        shiny::uiOutput(ns("role_summary_ui")),
        shiny::uiOutput(ns("selected_snapshot_ui"))
      )
    )
  )
}


#' @rdname module_analysis_setup
#' @export
analysis_setup_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # ── Local state ──────────────────────────────────────────────────────────
    # Each variable holds at most one role. Radio roles are single-select
    # (one variable each); multi roles can be held by many variables.
    RADIO_ROLES <- c("outcome", "exposure")
    MULTI_ROLES <- c("candidate", "cluster")
    ALL_ROLES   <- c(RADIO_ROLES, MULTI_ROLES)

    # Which column types may hold a role. Model terms must be numeric or
    # factor: a timestamp enters as seconds since 1970 and free text as one
    # dummy per string. Derived variables (year, era, duration) are the way in.
    # A cluster only labels groups, so anything but a timestamp will do.
    .role_eligible <- function(v, role) {
      ctypes <- shiny::isolate(shared_state$column_types)
      type   <- if (!is.null(ctypes) && v %in% names(ctypes)) ctypes[[v]] else "unknown"
      if (role == "cluster") return(type != "datetime")
      type %in% c("numeric", "factor")
    }

    # Named list keyed by variable name; each entry is a role assignment list.
    roles_state    <- shiny::reactiveVal(NULL)
    # Incremented on freeze / unfreeze to force re-render of the role table.
    frozen_trigger <- shiny::reactiveVal(0L)
    # Holds a pending role-change event while waiting for user confirmation.
    pending_change <- shiny::reactiveVal(NULL)
    # Incremented to rebuild the model purpose inputs from the spec (after a
    # role write, or to undo a cancelled change).
    purpose_trigger <- shiny::reactiveVal(0L)
    # Holds a proposed purpose_specification while waiting for confirmation.
    pending_purpose <- shiny::reactiveVal(NULL)

    .empty_role <- function() {
      list(outcome = FALSE, exposure = FALSE, candidate = FALSE,
           cluster = FALSE, reference_level = NULL)
    }

    # ── Dataset freeze ───────────────────────────────────────────────────────
    .do_freeze <- function() {
      wd <- shiny::isolate(shared_state$dataset_working)
      if (is.null(wd) || nrow(wd) == 0) return()

      sig <- digest::digest(wd, algo = "sha256")

      # One-time read of the Prepare stage, so the Step 5 summary (and later
      # the R script / export) can describe how this dataset was produced.
      orig <- shiny::isolate(shared_state$dataset_original)
      prepare_snapshot <- c(
        shiny::isolate(shared_state$last_applied_specs),
        list(
          original_columns = names(orig),
          original_dims    = if (!is.null(orig)) c(rows = nrow(orig), cols = ncol(orig)),
          working_dims     = c(rows = nrow(wd), cols = ncol(wd))
        )
      )

      wd[[".edark_row_id"]] <- seq_len(nrow(wd))

      reset_analysis_pipeline(shared_state, from_step = 1L)
      shared_state$analysis_data   <- wd
      shared_state$analysis_result <- NULL

      shared_state$analysis_spec <- list(
        specification_metadata = list(
          study_type        = "descriptive",
          created_at        = Sys.time(),
          dataset_signature = sig,
          roles_version     = 0L,  # bumped on every Step 1 role write
          prepare_snapshot  = prepare_snapshot
        ),
        variable_roles = list(
          outcome_variable       = NULL,
          exposure_variable      = NULL,
          candidate_covariates   = NULL,
          table1_variables       = NULL,
          univariable_test_pool  = NULL,
          final_model_covariates = NULL,
          cluster_variables      = NULL,
          reference_levels       = list()
        ),
        table1_specification = list(
          stratify_by_exposure                 = TRUE,
          stratify_by_outcome                  = FALSE,
          include_pvalues_exposure             = FALSE,
          include_pvalues_outcome              = TRUE,
          include_smd_exposure                 = TRUE,
          include_smd_outcome                  = FALSE
        ),
        variable_selection_specification = .default_variable_selection_specification(),
        purpose_specification = .default_purpose_specification(),
        validation_settings   = .default_validation_settings(),
        model_design = .default_model_design(),
        analysis_options = list(
          missing_data_handling = "complete_case",
          interaction_terms     = list()
        )
      )

      ctypes <- shiny::isolate(shared_state$column_types)
      vars   <- setdiff(names(wd), ".edark_row_id")

      init_roles <- stats::setNames(
        lapply(vars, function(v) {
          role <- .empty_role()
          if (!is.null(ctypes) && v %in% names(ctypes) && ctypes[[v]] == "factor") {
            col <- wd[[v]]
            if (is.factor(col) && length(levels(col)) > 0) {
              role$reference_level <- levels(col)[1]
            }
          }
          role
        }),
        vars
      )

      roles_state(init_roles)
      frozen_trigger(shiny::isolate(frozen_trigger()) + 1L)
    }

    shiny::observeEvent(input$btn_start_analysis, {
      .do_freeze()
    }, ignoreInit = TRUE)

    # ── Dataset signature mismatch detection ─────────────────────────────────
    working_sig <- shiny::reactive({
      wd <- shared_state$dataset_working
      if (is.null(wd)) return(NULL)
      digest::digest(wd, algo = "sha256")
    })

    sig_mismatch <- shiny::reactive({
      spec <- shared_state$analysis_spec
      if (is.null(spec)) return(FALSE)
      frozen_sig <- spec$specification_metadata$dataset_signature
      ws <- working_sig()
      if (is.null(frozen_sig) || is.null(ws)) return(FALSE)
      !identical(ws, frozen_sig)
    })

    # ── Restart Analysis ─────────────────────────────────────────────────────
    shiny::observeEvent(input$btn_restart_analysis, {
      shiny::showModal(shiny::modalDialog(
        title = "Restart Analysis?",
        shiny::p("This will clear all analysis results and re-freeze the current working dataset."),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          edark_button(ns, "confirm_restart", "Restart", variant = "danger", size = "dialog")
        ),
        easyClose = TRUE
      ))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$confirm_restart, {
      shiny::removeModal()
      .do_freeze()
    }, ignoreInit = TRUE)

    # ── Session load (§M8.7) ─────────────────────────────────────────────────
    # The session module has already confirmed with the user and rebuilt
    # Prepare, so no dialog here. With roles: freeze, then apply them through
    # the normal path (.sync_spec + table push), then purpose and validation
    # settings. Without: the session has no analysis, so unfreeze. Step 4
    # takes the covariates on the roles_key change this causes.
    shiny::observeEvent(shared_state$session_restore, {
      sr <- shared_state$session_restore
      if (is.null(sr) || isTRUE(sr$step1_done)) return()

      if (is.null(sr$roles)) {
        shared_state$analysis_data   <- NULL
        shared_state$analysis_spec   <- NULL
        shared_state$analysis_result <- NULL
        roles_state(NULL)
        frozen_trigger(shiny::isolate(frozen_trigger()) + 1L)
        shared_state$session_restore <- NULL
        return()
      }

      .do_freeze()
      rs <- shiny::isolate(roles_state())
      if (is.null(rs)) {                  # nothing to freeze (no rows)
        shared_state$session_restore <- NULL
        return()
      }
      r     <- sr$roles
      adata <- shiny::isolate(shared_state$analysis_data)
      .set_role <- function(rs, vars, role) {
        for (v in intersect(vars, names(rs))) {
          if (.role_eligible(v, role)) rs[[v]][[role]] <- TRUE
        }
        rs
      }
      rs <- .set_role(rs, r$outcome_variable,     "outcome")
      rs <- .set_role(rs, r$exposure_variable,    "exposure")
      rs <- .set_role(rs, r$candidate_covariates, "candidate")
      rs <- .set_role(rs, r$cluster_variables,    "cluster")
      for (v in intersect(names(r$reference_levels), names(rs))) {
        lvl <- r$reference_levels[[v]]
        if (is.factor(adata[[v]]) && isTRUE(lvl %in% levels(adata[[v]]))) {
          rs[[v]]$reference_level <- lvl
        }
      }
      roles_state(rs)
      .sync_spec(rs)

      spec <- shiny::isolate(shared_state$analysis_spec)
      if (!is.null(sr$purpose_specification)) {
        spec$purpose_specification <- sr$purpose_specification
      }
      if (!is.null(sr$validation_settings)) {
        spec$validation_settings <- sr$validation_settings
      }
      shared_state$analysis_spec <- spec
      purpose_trigger(shiny::isolate(purpose_trigger()) + 1L)
      .push_roles_to_table()

      sr$step1_done <- TRUE
      shared_state$session_restore <- if (length(sr$covariates)) sr else NULL
    }, ignoreNULL = TRUE)   # no ignoreInit: a launch session arrives in the first flush

    # ── Role change dispatcher ────────────────────────────────────────────────
    shiny::observeEvent(input$role_change, {
      ev     <- input$role_change
      result <- shiny::isolate(shared_state$analysis_result)
      covs   <- shiny::isolate(shared_state$analysis_spec)$variable_roles$final_model_covariates

      # Anything downstream worth protecting: results, or a Step 4 selection
      has_downstream <- !is.null(result) || length(covs) > 0L

      if (has_downstream) {
        # Downstream results exist — ask for confirmation before applying
        pending_change(ev)
        shiny::showModal(shiny::modalDialog(
          title = "Clear Analysis Results?",
          shiny::p("Changing role assignments will clear all downstream work: Table 1, variable investigation, covariate selection, and any model results."),
          shiny::tags$small(
            class = "text-muted",
            "Cancel undoes your change and keeps everything as it was."
          ),
          footer = shiny::tagList(
            edark_button(ns, "cancel_role_change", "Cancel", variant = "secondary", size = "dialog"),
            edark_button(ns, "confirm_role_change", "Clear & Continue", variant = "warning", size = "dialog")
          ),
          easyClose = FALSE
        ))
      } else {
        .apply_role_change(ev)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$confirm_role_change, {
      shiny::removeModal()
      ev <- pending_change()
      pending_change(NULL)
      reset_analysis_pipeline(shared_state, from_step = 1L)
      if (!is.null(ev)) .apply_role_change(ev)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$cancel_role_change, {
      shiny::removeModal()
      pending_change(NULL)
      .push_roles_to_table()   # the click already changed the DOM; put it back
    }, ignoreInit = TRUE)

    # Send roles_state to the browser so the table's inputs match it. JS keeps
    # the last payload and re-applies it when reactable remounts rows (e.g.
    # after a search filter is cleared, which renders inputs from scratch).
    .push_roles_to_table <- function() {
      rs <- shiny::isolate(roles_state())
      if (is.null(rs)) return()
      payload <- lapply(names(rs), function(v) {
        r    <- rs[[v]]
        role <- ALL_ROLES[vapply(ALL_ROLES, function(k) isTRUE(r[[k]]), logical(1))]
        list(var  = v,
             role = if (length(role) > 0) role[1] else "",
             ref  = if (is.null(r$reference_level)) "" else r$reference_level)
      })
      session$sendCustomMessage(ns("sync_roles"), payload)
    }

    # ── Apply a role change event to roles_state and sync spec ───────────────
    .apply_role_change <- function(ev) {
      var   <- ev$var
      role  <- ev$role
      value <- ev$value

      current <- roles_state()
      if (is.null(current)) return()

      .has_role <- function(v) any(vapply(ALL_ROLES, function(r) isTRUE(current[[v]][[r]]), logical(1)))

      if (var == "__clear__") {
        if (role %in% ALL_ROLES) {
          for (v in names(current)) current[[v]][[role]] <- FALSE
        }
      } else if (var == "__select_all_candidates__") {
        for (v in names(current)) {
          if (!.has_role(v) && .role_eligible(v, "candidate")) current[[v]][["candidate"]] <- TRUE
        }
      } else {
        if (!var %in% names(current)) return()
        if (role %in% ALL_ROLES && isTRUE(value) && !.role_eligible(var, role)) return()

        if (role %in% ALL_ROLES) {
          if (isTRUE(value)) {
            # A radio role is held by one variable only
            if (role %in% RADIO_ROLES) {
              for (v in names(current)) current[[v]][[role]] <- FALSE
            }
            # A variable holds one role only
            for (r in ALL_ROLES) current[[var]][[r]] <- FALSE
          }
          current[[var]][[role]] <- isTRUE(value)
        } else if (role == "reference_level") {
          current[[var]][["reference_level"]] <- value
        }
      }

      roles_state(current)
      .sync_spec(current)
      .push_roles_to_table()
    }

    # ── Sync roles_state → analysis_spec ─────────────────────────────────────
    .sync_spec <- function(rs) {
      spec <- shiny::isolate(shared_state$analysis_spec)
      if (is.null(spec)) return()

      outcome_vars  <- names(which(vapply(rs, function(r) isTRUE(r$outcome),   logical(1))))
      exposure_vars <- names(which(vapply(rs, function(r) isTRUE(r$exposure),  logical(1))))
      candidates    <- names(which(vapply(rs, function(r) isTRUE(r$candidate), logical(1))))
      cluster_vars  <- names(which(vapply(rs, function(r) isTRUE(r$cluster),   logical(1))))

      outcome_var  <- if (length(outcome_vars)  > 0) outcome_vars[1]  else NULL
      exposure_var <- if (length(exposure_vars) > 0) exposure_vars[1] else NULL

      study_type <- if (!is.null(exposure_var) && !is.null(outcome_var)) {
        "exposure_outcome"
      } else if (is.null(exposure_var) && !is.null(outcome_var)) {
        "risk_factor"
      } else if (!is.null(exposure_var) && is.null(outcome_var)) {
        "descriptive_exposure"
      } else {
        "descriptive"
      }

      ctypes <- shiny::isolate(shared_state$column_types)
      exp_is_factor <- !is.null(exposure_var) && !is.null(ctypes) &&
                       exposure_var %in% names(ctypes) && ctypes[[exposure_var]] == "factor"
      out_is_factor <- !is.null(outcome_var) && !is.null(ctypes) &&
                       outcome_var %in% names(ctypes) && ctypes[[outcome_var]] == "factor"
      t1_strat_exp <- study_type %in% c("exposure_outcome", "descriptive_exposure") && exp_is_factor
      t1_strat_out <- study_type == "risk_factor" && out_is_factor

      candidates   <- setdiff(candidates, c(outcome_var, exposure_var))
      cluster_vars <- setdiff(cluster_vars, c(outcome_var, exposure_var, candidates))

      t1_vars <- unique(c(exposure_var, outcome_var, candidates))
      t1_vars <- t1_vars[!vapply(t1_vars, is.null, logical(1))]
      t1_vars <- t1_vars[nzchar(t1_vars)]

      ref_levels <- Filter(Negate(is.null), lapply(rs, `[[`, "reference_level"))

      # Step 4 writes final covariates and reference levels into the same
      # variable_roles list, so compare against Step 1's own last write rather
      # than the live spec — otherwise any Step 1 event would look like a change.
      step1_roles <- list(
        outcome_var, exposure_var, candidates, cluster_vars, ref_levels
      )
      if (identical(step1_roles, spec$specification_metadata$step1_roles)) return()

      prev_version <- spec$specification_metadata$roles_version
      spec$specification_metadata$step1_roles    <- step1_roles
      spec$specification_metadata$roles_version  <-
        (if (is.null(prev_version)) 0L else prev_version) + 1L
      spec$specification_metadata$study_type     <- study_type
      spec$variable_roles$outcome_variable        <- outcome_var
      spec$variable_roles$exposure_variable       <- exposure_var
      spec$variable_roles$candidate_covariates    <- if (length(candidates) > 0) candidates else NULL
      spec$variable_roles$table1_variables        <- if (length(t1_vars) > 0) t1_vars else NULL
      spec$variable_roles$univariable_test_pool   <- if (length(candidates) > 0) candidates else NULL
      # Covariates start unselected; Step 4 writes them as the user checks
      spec$variable_roles$final_model_covariates  <- NULL
      spec$variable_roles$cluster_variables       <- if (length(cluster_vars) > 0) cluster_vars else NULL
      spec$variable_roles$reference_levels        <- ref_levels
      spec$table1_specification$stratify_by_exposure <- t1_strat_exp
      spec$table1_specification$stratify_by_outcome  <- t1_strat_out

      # A variable cannot be both a model role and the train/test variable
      ps <- spec$purpose_specification
      if (!is.null(ps$split_variable) &&
          ps$split_variable %in% c(outcome_var, exposure_var, candidates, cluster_vars)) {
        ps["split_variable"] <- list(NULL)   # keep the fields, as NULL
        ps["training_level"] <- list(NULL)
        spec$purpose_specification <- ps
      }

      shared_state$analysis_spec <- spec
      purpose_trigger(shiny::isolate(purpose_trigger()) + 1L)   # refresh split choices
    }

    # ── Main content (pre-freeze vs post-freeze) ──────────────────────────────
    output$main_content <- shiny::renderUI({
      frozen_trigger()
      adata <- shared_state$analysis_data

      if (is.null(adata)) {
        wd   <- shared_state$dataset_working
        n_r  <- if (!is.null(wd)) nrow(wd)  else 0L
        n_c  <- if (!is.null(wd)) ncol(wd)  else 0L
        n_cc <- if (!is.null(wd)) sum(stats::complete.cases(wd)) else 0L
        pct  <- if (n_r > 0) round(n_cc / n_r * 100L) else 0L

        bslib::card(
          bslib::card_header("Ready to Start Analysis"),
          bslib::card_body(
            shiny::tags$p(
              class = "text-muted mb-3",
              sprintf("%d rows \u00b7 %d columns \u00b7 %d complete cases (%d%% complete)",
                      n_r, n_c, n_cc, pct)
            ),
            shiny::tags$p(
              class = "text-muted small mb-0",
              "Freezing the dataset creates a snapshot for analysis. Changes in Prepare",
              "after this point won't affect the analysis unless you restart."
            )
          )
        )
      } else {
        reactable::reactableOutput(ns("role_table"))
      }
    })

    # -- Messages: every notice on this page, in one place (D3) ---------------
    edark_messages_server(output, shiny::reactive({
      if (!sig_mismatch()) return(NULL)
      list(edark_message(
        "stale",
        "Your working dataset has changed since this analysis was started.",
        detail = "Use \u201cRestart Analysis\u201d in the pane on the left to use the updated data."
      ))
    }))

    # ── Role assignment table ────────────────────────────────────────────────
    output$role_table <- reactable::renderReactable({
      frozen_trigger()
      adata  <- shiny::isolate(shared_state$analysis_data)
      if (is.null(adata)) return(NULL)
      ctypes <- shiny::isolate(shared_state$column_types)
      vars   <- setdiff(names(adata), ".edark_row_id")

      .dash <- function() htmltools::tags$span("-", class = "text-muted")

      .radio_cell <- function(role_key) {
        function(value, index) {
          if (!.role_eligible(vars[index], role_key)) return(.dash())
          htmltools::tags$input(
            type        = "radio",
            class       = "edark-role-radio form-check-input",
            name        = paste0("role_", role_key),
            `data-var`  = vars[index],
            `data-role` = role_key
          )
        }
      }

      .clear_header <- function(label, role_key) {
        htmltools::tags$div(
          class = "d-flex flex-column align-items-center",
          htmltools::tags$span(style = "font-size:0.75rem;", label),
          htmltools::tags$button(
            "Clear",
            class       = "edark-clear-role btn btn-link btn-sm p-0 text-muted",
            style       = "font-size:0.7rem; line-height:1;",
            `data-role` = role_key
          )
        )
      }

      df <- data.frame(
        ID         = seq_along(vars),
        Variable   = vars,
        Type       = vapply(vars, function(v) {
          if (!is.null(ctypes) && v %in% names(ctypes)) ctypes[[v]] else "unknown"
        }, character(1)),
        ref_level  = vapply(vars, function(v) {
          if (!is.null(ctypes) && v %in% names(ctypes) && ctypes[[v]] == "factor") {
            col <- adata[[v]]
            if (is.factor(col) && length(levels(col)) > 0) levels(col)[1] else ""
          } else ""
        }, character(1)),
        exposure   = FALSE,
        outcome    = FALSE,
        candidate  = FALSE,
        cluster    = FALSE,
        stringsAsFactors = FALSE,
        row.names  = NULL
      )

      reactable::reactable(
        df,
        height        = EDARK_RESULT_HEIGHT,
        searchable    = TRUE,
        pagination    = FALSE,
        highlight     = TRUE,
        compact       = TRUE,
        defaultColDef = reactable::colDef(
          align  = "center",
          vAlign = "center",
          minWidth = 60
        ),
        columns = list(
          ID = reactable::colDef(
            name     = "ID",
            minWidth = 45,
            sortable = TRUE
          ),
          Variable = reactable::colDef(
            name     = "Variable",
            align    = "left",
            minWidth = 140,
            sticky   = "left"
          ),
          Type = reactable::colDef(
            name     = "Type",
            minWidth = 80,
            cell     = function(value, index) edark_type_badge(value)
          ),
          ref_level = reactable::colDef(
            name     = "Ref. Level",
            minWidth = 130,
            align    = "left",
            cell     = function(value, index) {
              v <- vars[index]
              if (!is.null(ctypes) && v %in% names(ctypes) && ctypes[[v]] == "factor") {
                col  <- adata[[v]]
                lvls <- if (is.factor(col)) levels(col) else character(0)
                if (length(lvls) == 0) {
                  return(htmltools::tags$span("-", class = "text-muted"))
                }
                htmltools::tags$select(
                  class       = "edark-role-reflevel form-select form-select-sm",
                  `data-var`  = v,
                  style       = "font-size:0.8rem;",
                  lapply(lvls, function(lev) htmltools::tags$option(value = lev, lev))
                )
              } else {
                htmltools::tags$span("-", class = "text-muted")
              }
            }
          ),
          exposure = reactable::colDef(
            header   = .clear_header("Exposure", "exposure"),
            minWidth = 80,
            cell     = .radio_cell("exposure")
          ),
          outcome = reactable::colDef(
            header   = .clear_header("Outcome", "outcome"),
            minWidth = 80,
            cell     = .radio_cell("outcome")
          ),
          candidate = reactable::colDef(
            header = htmltools::tags$div(
              class = "d-flex flex-column align-items-center",
              htmltools::tags$span(style = "font-size:0.75rem;", "Covariate"),
              htmltools::tags$div(
                class = "d-flex gap-2",
                htmltools::tags$button(
                  "All",
                  class = "edark-select-all-candidates btn btn-link btn-sm p-0 text-muted",
                  style = "font-size:0.7rem; line-height:1;"
                ),
                htmltools::tags$button(
                  "Clear",
                  class       = "edark-clear-role btn btn-link btn-sm p-0 text-muted",
                  style       = "font-size:0.7rem; line-height:1;",
                  `data-role` = "candidate"
                )
              )
            ),
            minWidth = 90,
            cell = function(value, index) {
              if (!.role_eligible(vars[index], "candidate")) return(.dash())
              htmltools::tags$input(
                type        = "checkbox",
                class       = "edark-role-checkbox form-check-input",
                `data-var`  = vars[index],
                `data-role` = "candidate"
              )
            }
          ),
          cluster = reactable::colDef(
            header = htmltools::tags$div(
              class = "d-flex flex-column align-items-center",
              htmltools::tags$span(
                style = "font-size:0.75rem; cursor:help;",
                title = paste(
                  "Grouping variables (e.g. patient ID, centre). Each becomes a",
                  "random intercept in a mixed model. Check more than one for",
                  "nested or crossed groupings."),
                "Cluster"
              ),
              htmltools::tags$button(
                "Clear",
                class       = "edark-clear-role btn btn-link btn-sm p-0 text-muted",
                style       = "font-size:0.7rem; line-height:1;",
                `data-role` = "cluster"
              )
            ),
            minWidth = 70,
            cell = function(value, index) {
              v <- vars[index]
              if (!.role_eligible(v, "cluster")) return(.dash())
              htmltools::tags$input(
                type        = "checkbox",
                class       = "edark-role-checkbox form-check-input",
                `data-var`  = v,
                `data-role` = "cluster"
              )
            }
          )
        )
      )
    })

    # ── Sidebar outputs ──────────────────────────────────────────────────────
    output$action_buttons_ui <- shiny::renderUI({
      adata    <- shared_state$analysis_data
      mismatch <- sig_mismatch()

      if (is.null(adata)) {
        edark_button(ns, "btn_start_analysis", "Start Analysis", icon = "play")
      } else {
        shiny::tagList(
          # Warning-filled while the frozen dataset no longer matches what
          # Prepare holds; otherwise a de-emphasised secondary.
          edark_button(
            ns, "btn_restart_analysis", "Restart Analysis", icon = "rotate",
            variant = if (mismatch) "warning" else "secondary",
            outline = !mismatch
          )
        )
      }
    })

    output$study_type_ui <- shiny::renderUI({
      spec <- shared_state$analysis_spec
      if (is.null(spec)) return(NULL)

      st  <- spec$specification_metadata$study_type
      st  <- if (is.null(st)) "descriptive" else st
      cfg <- switch(st,
        exposure_outcome     = list(label = "Exposure-Outcome Study",
                                    role = "study_exposure_outcome"),
        risk_factor          = list(label = "Risk Factor / Association",
                                    role = "study_risk_factor"),
        descriptive_exposure = list(label = "Descriptive (Exposure)",
                                    role = "study_descriptive_exposure"),
        list(label = "Descriptive Cohort", role = "study_descriptive")
      )

      exp_var <- spec$variable_roles$exposure_variable
      out_var <- spec$variable_roles$outcome_variable
      nudge   <- if (!is.null(exp_var) && is.null(out_var)) {
        shiny::tags$p(
          class = "text-muted small mt-1 mb-0",
          shiny::icon("circle-info"),
          " Exposure assigned but no outcome - descriptive summaries only."
        )
      } else NULL

      shiny::tagList(
        edark_section_label("Study Type"),
        edark_badge(cfg$label, role = cfg$role, size = "md",
                    class = "w-100 d-block",
                    style = "white-space:normal;"),
        nudge
      )
    })

    # ── Model purpose (prediction: optional train/test split) ────────────────
    # The purpose is a study design decision, so it lives in Step 1: a split
    # decides which rows Steps 3–5 may learn from, and must be set before them.

    # Factor columns with two or more levels and no role
    .split_candidates <- function(spec, data) {
      vr    <- spec$variable_roles
      roled <- c(vr$outcome_variable, vr$exposure_variable,
                 vr$candidate_covariates, vr$cluster_variables)
      vars  <- setdiff(names(data), c(".edark_row_id", roled))
      vars[vapply(vars, function(v) {
        is.factor(data[[v]]) && nlevels(droplevels(data[[v]])) >= 2L
      }, logical(1))]
    }

    # Default training level: one named like "train", else the first level
    .guess_training_level <- function(lv) {
      hit <- lv[grepl("^train", lv, ignore.case = TRUE)]
      if (length(hit) > 0L) hit[1L] else lv[1L]
    }

    output$purpose_ui <- shiny::renderUI({
      purpose_trigger()
      frozen_trigger()
      spec  <- shiny::isolate(shared_state$analysis_spec)
      adata <- shiny::isolate(shared_state$analysis_data)
      if (is.null(spec) || is.null(adata)) return(NULL)
      ps <- spec$purpose_specification %||% .default_purpose_specification()

      cands <- .split_candidates(spec, adata)
      sv    <- ps$split_variable
      shiny::tagList(
        edark_section_label("Model Purpose"),
        shiny::radioButtons(
          ns("model_purpose"), label = NULL, inline = TRUE,
          choices  = c(Association = "association", Prediction = "prediction"),
          selected = ps$model_purpose %||% "association"
        ),
        shiny::tags$p(
          class = "text-muted small mb-1",
          "Association: estimate how the exposure or risk factors relate to the outcome.",
          "Prediction: build a model to predict the outcome for new patients -",
          "judged by its performance (Step 5 \u203a Performance)."
        ),
        shiny::conditionalPanel(
          condition = "input.model_purpose == 'prediction'", ns = ns,
          edark_section_label("Validation"),
          shiny::radioButtons(
            ns("validation_method"), label = NULL, width = "100%",
            choiceNames = list(
              shiny::span("Bootstrap", shiny::span(class = "small text-muted",
                "- optimism correction")),
              shiny::span("Cross-validation", shiny::span(class = "small text-muted",
                "- k-fold")),
              shiny::span("Held-out test set", shiny::span(class = "small text-muted",
                "- e.g. other centres"))
            ),
            choiceValues = c("bootstrap", "cv", "split"),
            selected = ps$validation_method %||% "bootstrap"
          ),
          shiny::tags$p(
            class = "text-muted small mb-1",
            "Bootstrap and cross-validation build the model from all rows and refit it in each",
            "resample with the covariates confirmed in Step 4 (settings: Step 5 \u203a Performance).",
            "A held-out test set is data kept apart on purpose, marked by a variable."
          ),
          shiny::conditionalPanel(
            condition = "input.validation_method == 'split'", ns = ns,
            shiny::selectInput(
              ns("split_variable"), "Variable",
              choices  = c(stats::setNames("", if (length(cands)) "Choose a factor\u2026" else "No eligible factor"),
                           cands),
              selected = if (!is.null(sv) && sv %in% cands) sv else "",
              selectize = FALSE, width = "100%"
            ),
            shiny::uiOutput(ns("training_level_ui")),
            shiny::tags$p(
              class = "text-muted small mb-0",
              "Variable investigation, covariate selection and the model use the training",
              "rows only. Every other level is the held-out test set, used in Step 5 \u203a Performance.",
              "Rows missing this variable are in neither set."
            )
          )
        )
      )
    })

    output$training_level_ui <- shiny::renderUI({
      purpose_trigger()
      v     <- input$split_variable
      adata <- shiny::isolate(shared_state$analysis_data)
      if (is.null(v) || !nzchar(v) || is.null(adata) || !v %in% names(adata)) return(NULL)
      lv  <- levels(droplevels(adata[[v]]))
      cur <- shiny::isolate(shared_state$analysis_spec)$purpose_specification
      sel <- if (identical(cur$split_variable, v) && isTRUE(cur$training_level %in% lv)) {
        cur$training_level
      } else .guess_training_level(lv)
      shiny::selectInput(ns("training_level"), "Training level", choices = lv,
                         selected = sel, selectize = FALSE, width = "100%")
    })

    # The purpose_specification the inputs describe. Derived from state, not
    # trusted blindly: training_level keeps its old value while its select is
    # re-rendered for a new variable (§N1.3), so it must be a level of the
    # chosen variable.
    .purpose_from_inputs <- function() {
      adata <- shiny::isolate(shared_state$analysis_data)
      v <- input$split_variable
      if (is.null(v) || !nzchar(v) || !v %in% names(adata) || !is.factor(adata[[v]])) v <- NULL
      lvl <- NULL
      if (!is.null(v)) {
        lv  <- levels(droplevels(adata[[v]]))
        lvl <- if (isTRUE(input$training_level %in% lv)) input$training_level else .guess_training_level(lv)
      }
      vm <- input$validation_method
      list(
        model_purpose     = if (identical(input$model_purpose, "prediction")) "prediction" else "association",
        validation_method = if (isTRUE(vm %in% c("bootstrap", "cv", "split"))) vm else "bootstrap",
        split_variable    = v,
        training_level    = lvl
      )
    }

    .write_purpose <- function(ps) {
      spec <- shiny::isolate(shared_state$analysis_spec)
      if (is.null(spec) || identical(spec$purpose_specification, ps)) return()
      spec$purpose_specification <- ps
      shared_state$analysis_spec <- spec
    }

    shiny::observeEvent(
      list(input$model_purpose, input$validation_method, input$split_variable, input$training_level), {
      spec <- shiny::isolate(shared_state$analysis_spec)
      if (is.null(spec) || is.null(input$model_purpose)) return()
      proposed <- .purpose_from_inputs()
      if (identical(proposed, spec$purpose_specification)) return()

      # Only a change of training rows invalidates anything: purpose alone
      # changes no computation.
      new_spec <- spec
      new_spec$purpose_specification <- proposed
      rows_change <- !identical(analysis_split(spec), analysis_split(new_spec))
      res <- shiny::isolate(shared_state$analysis_result)
      learned <- !is.null(res$variable_investigation) ||
                 !is.null(res$fitted_models$primary_model)

      if (rows_change && learned) {
        pending_purpose(proposed)
        shiny::showModal(shiny::modalDialog(
          title = "Clear Analysis Results?",
          shiny::p("Adding, changing or removing the held-out test set changes the rows the model learns from.",
                   "Variable investigation, the fitted model, diagnostics, performance and",
                   "results will be cleared. Table 1 and your covariate selection are kept."),
          shiny::tags$small(class = "text-muted",
                            "Cancel undoes your change and keeps everything as it was."),
          footer = shiny::tagList(
            edark_button(ns, "cancel_purpose", "Cancel", variant = "secondary", size = "dialog"),
            edark_button(ns, "confirm_purpose", "Clear & Continue", variant = "warning", size = "dialog")
          ),
          easyClose = FALSE
        ))
        return()
      }
      .write_purpose(proposed)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$confirm_purpose, {
      shiny::removeModal()
      ps <- pending_purpose()
      pending_purpose(NULL)
      if (is.null(ps)) return()
      reset_analysis_pipeline(shared_state, from_step = 3L)
      .write_purpose(ps)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$cancel_purpose, {
      shiny::removeModal()
      pending_purpose(NULL)
      purpose_trigger(shiny::isolate(purpose_trigger()) + 1L)   # inputs back to the spec
    }, ignoreInit = TRUE)

    output$role_summary_ui <- shiny::renderUI({
      rs <- roles_state()
      if (is.null(rs)) return(NULL)

      outcome    <- names(which(vapply(rs, function(r) isTRUE(r$outcome),    logical(1))))
      exposure   <- names(which(vapply(rs, function(r) isTRUE(r$exposure),   logical(1))))
      candidates <- names(which(vapply(rs, function(r) isTRUE(r$candidate),  logical(1))))
      clusters   <- names(which(vapply(rs, function(r) isTRUE(r$cluster),    logical(1))))

      .row <- function(label, display) {
        shiny::div(
          class = "d-flex justify-content-between mb-1",
          shiny::tags$span(class = "text-muted small", label),
          shiny::tags$span(class = "small fw-semibold", display)
        )
      }

      shiny::tagList(
        edark_section_label("Role Summary"),
        .row("Outcome",    if (length(outcome) == 0)
                             shiny::span("-", class = "text-muted fw-normal")
                           else outcome[1]),
        .row("Exposure",   if (length(exposure) == 0)
                             shiny::span("-", class = "text-muted fw-normal")
                           else exposure[1]),
        .row("Candidates", if (length(candidates) == 0)
                             shiny::span("-", class = "text-muted fw-normal")
                           else sprintf("%d variable%s", length(candidates),
                                        if (length(candidates) != 1L) "s" else "")),
        .row("Clusters",   if (length(clusters) == 0)
                             shiny::span("-", class = "text-muted fw-normal")
                           else paste(clusters, collapse = ", "))
      )
    })

    output$incoming_snapshot_ui <- shiny::renderUI({
      wd <- shared_state$dataset_working
      if (is.null(wd)) return(NULL)

      n_rows <- nrow(wd)
      n_cols <- ncol(wd)
      n_cc   <- sum(stats::complete.cases(wd))

      .row <- function(label, val) {
        shiny::div(
          class = "d-flex justify-content-between mb-1",
          shiny::tags$span(class = "text-muted small", label),
          shiny::tags$span(class = "small fw-semibold", val)
        )
      }

      shiny::tagList(
        edark_section_label("Incoming Dataset Snapshot"),
        .row("Rows",           n_rows),
        .row("Variables",      n_cols),
        .row("Complete cases", sprintf("%d (%d%%)", n_cc,
                                       round(n_cc / n_rows * 100L)))
      )
    })

    output$selected_snapshot_ui <- shiny::renderUI({
      adata <- shared_state$analysis_data
      spec  <- shared_state$analysis_spec
      if (is.null(adata) || is.null(spec)) return(NULL)

      vr <- spec$variable_roles
      selected_vars <- unique(Filter(Negate(is.null), c(
        vr$outcome_variable,
        vr$exposure_variable,
        vr$candidate_covariates,
        vr$cluster_variables
      )))
      selected_vars <- intersect(selected_vars, names(adata))

      n_rows <- nrow(adata)

      if (length(selected_vars) == 0) {
        return(shiny::tagList(
          shiny::tags$hr(class = "my-2"),
          edark_section_label("Selected Dataset Snapshot"),
          shiny::tags$small(class = "text-muted", "No variables assigned yet.")
        ))
      }

      n_cc <- sum(stats::complete.cases(adata[, selected_vars, drop = FALSE]))

      .row <- function(label, val) {
        shiny::div(
          class = "d-flex justify-content-between mb-1",
          shiny::tags$span(class = "text-muted small", label),
          shiny::tags$span(class = "small fw-semibold", val)
        )
      }

      shiny::tagList(
        shiny::tags$hr(class = "my-2"),
        edark_section_label("Selected Dataset Snapshot"),
        .row("Rows",           n_rows),
        .row("Variables",      length(selected_vars)),
        .row("Complete cases", sprintf("%d (%d%%)", n_cc,
                                       round(n_cc / n_rows * 100L)))
      )
    })

  })
}
