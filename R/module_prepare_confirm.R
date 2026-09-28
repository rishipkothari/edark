#' Prepare Confirm Module
#'
#' Displays dataset dimensions (original / current / pending), grouped warnings,
#' and a pending-changes status badge. When the user clicks "Apply Changes",
#' runs the full prepare pipeline atomically.
#'
#' The pipeline order (from PRD §4.5):
#'   1. Start from `dataset_original`
#'   2. Apply column type overrides
#'   3. Select included columns
#'   4. Apply column transformations (numeric → ordered factor)
#'   5. Apply row filters
#'
#' @param id Character. The module namespace ID.
#' @param shared_state A Shiny `reactiveValues` object.
#'
#' @name module_prepare_confirm
NULL


#' @rdname module_prepare_confirm
#' @export
prepare_confirm_ui <- function(id) {
  ns <- shiny::NS(id)

  # Config pane: the two actions and nothing else. Dimensions and the pending
  # list are facts, so they moved to the info pane (§1.3d), and the warnings
  # moved to the messages slot. That is also what stops a long warning list
  # pushing Apply below the fold (§BUILD_UI-redesign 2.6) - without moving
  # Apply anywhere.
  # Apply starts disabled and is enabled only while something is staged, so
  # the button answers "is there anything to apply?" before it is clicked
  # rather than after (Stage 1's disabled-with-a-reason rule). The reason sits
  # in the info pane, which already says "Pending: no changes".
  shiny::tagList(
    shinyjs::disabled(
      edark_button(ns, "apply_btn", "Apply Changes", icon = "circle-check")
    ),
    edark_button(ns, "reset_btn", "Reset to Original", icon = "rotate-left",
                 variant = "secondary", outline = TRUE, class = "mt-2")
  )
}


#' @rdname module_prepare_confirm
#' @export
prepare_confirm_info_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("info_panel"))
}


#' @rdname module_prepare_confirm
#' @export
prepare_confirm_messages_ui <- function(id) {
  edark_messages_ui(shiny::NS(id))
}


#' @rdname module_prepare_confirm
#' @export
prepare_confirm_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Apply is live only while something is staged ───────────────────
    # Every stage of Prepare sets has_pending_changes; Apply, Reset and the
    # tab-switch auto-apply clear it. Gating the button on that one field is
    # what greys it out again after an Apply and brings it back on the next
    # change.
    shiny::observe({
      shinyjs::toggleState("apply_btn",
                           condition = isTRUE(shared_state$has_pending_changes))
    })

    # ── Pending dataset preview ───────────────────────────────────────────────
    # apply_prepare_pipeline() isolates all reads, so we touch the relevant
    # fields here to create reactive dependencies before calling it.
    preview_dataset <- shiny::reactive({
      shared_state$included_columns       # dependency: col include/exclude
      shared_state$row_filter_specs       # dependency: row filters
      shared_state$column_transform_specs # dependency: transforms
      # Warnings are suppressed here, not ignored: this is a speculative render of
      # a staged state, and anything worth telling the user about it is already in
      # the messages pane below. The real Apply runs unsuppressed.
      tryCatch(suppressWarnings(apply_prepare_pipeline(shared_state)),
               error = function(e) NULL)
    })


    # -- Info pane: what the staged settings produce --------------------------
    # Dimensions now -> after apply, then what is actually staged, itemised
    # rather than counted: a bare "3 pending change(s)" never said *which*
    # three (§BUILD_UI-redesign 1.3d, D2).
    output$info_panel <- shiny::renderUI({
      orig       <- shared_state$dataset_original
      curr       <- shared_state$dataset_working
      pending    <- preview_dataset()
      is_pending <- isTRUE(shared_state$has_pending_changes)

      dims <- function(d) {
        if (is.null(d)) return(shiny::tags$em(class = "text-muted", "-"))
        paste0(format(nrow(d), big.mark = ","), " \u00d7 ", ncol(d))
      }

      groups <- .describe_pending_changes(shared_state)

      shiny::tagList(
        edark_section_label("Dimensions", first = TRUE),
        edark_info_row("Original", dims(orig)),
        edark_info_row("Current",  dims(curr)),
        edark_info_row(
          if (is_pending) "After apply" else "Pending",
          if (is_pending) shiny::tags$span(class = "text-primary", dims(pending))
          else shiny::tags$em(class = "text-muted", "no changes")
        ),

        # How many rows survive listwise deletion - the number that decides
        # how much of the dataset a model actually sees. It moves with column
        # exclusion and row filters, so it belongs beside the dimensions
        # rather than only in Analyze.
        edark_section_label("Complete cases"),
        edark_info_row("Original", .complete_case_label(orig)),
        edark_info_row("Current",  .complete_case_label(curr)),
        edark_info_row(
          if (is_pending) "After apply" else "Pending",
          if (is_pending)
            shiny::tags$span(class = "text-primary", .complete_case_label(pending))
          else shiny::tags$em(class = "text-muted", "no changes")
        ),

        if (length(groups) > 0) {
          shiny::tagList(lapply(names(groups), function(kind) {
            shiny::tagList(
              edark_section_label(kind),
              shiny::tags$ul(
                class = "small ps-3 mb-0",
                lapply(groups[[kind]], shiny::tags$li)
              ),
              # The row count the filters leave. This was a badge in the Row
              # Filters panel, where it scrolled away as filters accumulated
              # (§BUILD_UI-redesign 2.6); it is a fact, so it belongs here (F2).
              if (identical(kind, "Row Filters") && !is.null(pending)) {
                edark_info_row(
                  "Rows retained",
                  sprintf("%s of %s", format(nrow(pending), big.mark = ","),
                          format(nrow(curr), big.mark = ","))
                )
              }
            )
          }))
        }
      )
    })


    # -- Messages: every warning on this page, in one place --------------------
    # Read each field at this level so Shiny registers them as dependencies -
    # do not rely on reads buried inside the helper call.
    edark_messages_server(output, shiny::reactive({
      specs        <- shared_state$column_transform_specs
      filters      <- shared_state$row_filter_specs
      included     <- shared_state$included_columns
      last_applied <- shared_state$last_applied_specs
      has_pending  <- shared_state$has_pending_changes

      # No pending changes -> nothing can be in conflict
      if (!isTRUE(has_pending)) return(NULL)

      warn_groups <- .build_prepare_warnings(specs, filters, included,
                                             last_applied,
                                             shared_state$dataset_original)
      if (length(warn_groups) == 0) return(NULL)

      lapply(warn_groups, function(grp) {
        edark_message(
          "warn", grp$title,
          detail = shiny::tags$ul(class = "mb-0 ps-3",
                                  lapply(grp$items, shiny::tags$li))
        )
      })
    }))


    # ── Shared apply helper ───────────────────────────────────────────────────
    do_apply <- function() {
      df <- .run_prepare_apply(shared_state)
      if (is.null(df)) return()
      shiny::showNotification(
        paste0("Applied! ",
               format(nrow(df), big.mark = ","), " rows \u00d7 ", ncol(df), " columns."),
        type = "message", duration = 4
      )
    }

    # Shared reset helper — increments revert_trigger so module UIs sync.
    do_reset <- function() {
      orig <- shared_state$dataset_original
      shared_state$included_columns       <- names(orig)
      shared_state$column_type_overrides  <- list()
      shared_state$column_transform_specs <- list()
      shared_state$row_filter_specs       <- list()
      shared_state$dataset_working        <- orig
      shared_state$column_types           <- shared_state$original_column_types
      shared_state$has_pending_changes    <- FALSE
      .snapshot_last_applied_specs(shared_state)
      shared_state$revert_trigger <- shiny::isolate(shared_state$revert_trigger) + 1L
      shiny::showNotification("Reset to original dataset.", type = "message", duration = 3)
    }

    # Show a confirmation modal when custom report items exist.
    # Three ways out: proceed, roll the staged changes back, or discard the
    # queued items - the last one so a user who does not want the report no
    # longer meets this dialog on every Apply and every tab switch.
    .custom_items_guard <- function(action_label, confirm_btn_id, cancel_btn_id,
                                    clear_btn_id) {
      n_items <- length(shiny::isolate(shared_state$custom_report_items))
      if (n_items == 0) return(FALSE)  # no guard needed
      .custom_items_modal(n_items, ns(cancel_btn_id), ns(confirm_btn_id),
                          action_label, ns(clear_btn_id))
      TRUE  # guard was triggered
    }

    # ── Apply button ──────────────────────────────────────────────────────────
    shiny::observeEvent(input$apply_btn, {
      # Validate transforms first
      invalid <- .find_invalid_transforms(shared_state)
      if (length(invalid) > 0) {
        # Navigate to Transforms tab so user sees what needs fixing
        bslib::nav_select("prepare_tabs", "transforms")
        shiny::showNotification(
          paste0("Fix transforms before applying: ",
                 paste(invalid, collapse = ", ")),
          type = "error", duration = 6
        )
        return()
      }

      # Warn if custom report items exist
      if (.custom_items_guard("Apply Changes", "confirm_apply_btn",
                              "cancel_apply_btn", "clear_apply_btn")) return()

      do_apply()
    })

    shiny::observeEvent(input$confirm_apply_btn, {
      shiny::removeModal()
      do_apply()
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$cancel_apply_btn, {
      shiny::removeModal()
      .revert_to_last_applied(shared_state)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$clear_apply_btn, {
      shiny::removeModal()
      .clear_custom_report_items(shared_state)
      do_apply()
    }, ignoreInit = TRUE)


    # ── Reset button ──────────────────────────────────────────────────────────
    shiny::observeEvent(input$reset_btn, {
      # Warn if custom report items exist
      if (.custom_items_guard("Reset to Original", "confirm_reset_btn",
                              "cancel_reset_btn", "clear_reset_btn")) return()
      do_reset()
    })

    shiny::observeEvent(input$confirm_reset_btn, {
      shiny::removeModal()
      do_reset()
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$cancel_reset_btn, {
      shiny::removeModal()
      .revert_to_last_applied(shared_state)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$clear_reset_btn, {
      shiny::removeModal()
      .clear_custom_report_items(shared_state)
      do_reset()
    }, ignoreInit = TRUE)
  })
}


# ── Pipeline ──────────────────────────────────────────────────────────────────

#' Apply all staged prepare-stage changes to produce the working dataset
#'
#' Runs the full prepare pipeline in order:
#' type overrides → column selection → transformations → row filters.
#' Called by `prepare_confirm_server` on Apply and used for the dimension
#' preview. Also called by `edark_report()` for the programmatic API.
#'
#' @param shared_state A Shiny `reactiveValues` object (or a plain named list
#'   with the same fields, for programmatic use).
#'
#' @return A `data.frame` — the result of applying all staged specs.
#'
#' @keywords internal
apply_prepare_pipeline <- function(shared_state) {
  dataset    <- shiny::isolate(shared_state$dataset_original)
  overrides  <- shiny::isolate(shared_state$column_type_overrides)
  included   <- shiny::isolate(shared_state$included_columns)
  transforms <- shiny::isolate(shared_state$column_transform_specs)
  filters    <- shiny::isolate(shared_state$row_filter_specs)

  # Step 1: apply column type overrides
  dataset <- .apply_column_type_overrides(dataset, overrides)

  # Step 2: select included columns only
  dataset <- dataset[, intersect(included, names(dataset)), drop = FALSE]

  # Step 3: apply column transformations (numeric → ordered factor)
  dataset <- .apply_column_transforms(dataset, transforms)

  # Step 4: apply row filters
  dataset <- .apply_row_filters(dataset, filters)

  dataset
}


# ── Step implementations ──────────────────────────────────────────────────────

.apply_column_type_overrides <- function(dataset, overrides) {
  if (length(overrides) == 0) return(dataset)
  for (col in names(overrides)) {
    if (!col %in% names(dataset)) next
    target <- overrides[[col]]
    dataset[[col]] <- switch(target,
      numeric  = suppressWarnings(as.numeric(dataset[[col]])),
      factor   = as.factor(dataset[[col]]),
      datetime = suppressWarnings(as.POSIXct(as.character(dataset[[col]]), tz = "UTC")),
      character = as.character(dataset[[col]]),
      dataset[[col]]  # unknown type — leave unchanged
    )
  }
  dataset
}


# Delegates to the canonical implementation in module_column_transform.R
# (that file is sourced by load_all / NAMESPACE, so .apply_column_transforms
#  is defined once there — this duplicate has been removed)


.apply_row_filters <- function(dataset, filters) {
  if (length(filters) == 0) return(dataset)
  for (col in names(filters)) {
    if (!col %in% names(dataset)) next
    spec <- filters[[col]]

    # A spec is built against the column as it was when the filter was added, so a
    # transform staged or removed afterwards can leave the two out of step - a
    # categorical spec holding band labels over a column that is numeric again, say.
    # Matching those labels keeps no rows at all, which surfaced as an empty dataset
    # with nothing to explain it. .prune_conflicting_filter_specs() should have
    # dropped such a filter before we get here; skipping it loudly is the backstop
    # for any route that does not.
    if (identical(spec$type, "numeric") != is.numeric(dataset[[col]])) {
      warning("Row filter on '", col, "' no longer matches that column's type ",
              "and was skipped. Re-add the filter after applying your transforms.")
      next
    }

    if (spec$type == "numeric") {
      keep <- !is.na(dataset[[col]]) &
              dataset[[col]] >= spec$min &
              dataset[[col]] <= spec$max
    } else {
      keep <- !is.na(dataset[[col]]) &
              as.character(dataset[[col]]) %in% spec$levels_selected
    }
    dataset <- dataset[keep, , drop = FALSE]
  }
  dataset
}


# The Apply pipeline, shared by the Apply button and the tab-switch auto-apply
# in edark.R: prune filters the staged changes invalidate, run the pipeline,
# commit. Returns the working dataset, or NULL after showing the error (and
# calling on_error, e.g. to put the tab back).
.run_prepare_apply <- function(shared_state, on_error = NULL) {
  # Remove filter specs that would be invalidated by staged transforms or
  # by column exclusion, before running the pipeline.
  .prune_conflicting_filter_specs(shared_state)
  df <- tryCatch(
    apply_prepare_pipeline(shared_state),
    error = function(e) {
      if (is.function(on_error)) on_error()
      shiny::showNotification(
        paste("Error during Apply:", conditionMessage(e)),
        type = "error", duration = 8
      )
      NULL
    }
  )
  if (is.null(df)) return(NULL)
  .commit_working_dataset(shared_state, df)
  df
}


# Commit a working dataset built from the staged specs: set it, re-detect
# types, clear pending, flag Explore, and snapshot the specs as last applied.
# Used by Apply and by session load (module_session.R), which sets the staged
# specs itself first.
.commit_working_dataset <- function(shared_state, df) {
  shared_state$dataset_working       <- df
  shared_state$column_types          <- detect_column_types(df)
  shared_state$has_pending_changes   <- FALSE
  shared_state$explore_needs_refresh <- TRUE
  .snapshot_last_applied_specs(shared_state)
}


# Snapshot the current staged specs into shared_state$last_applied_specs.
# Called after every successful Apply or Reset so Cancel can revert to this point.
.snapshot_last_applied_specs <- function(shared_state) {
  shared_state$last_applied_specs <- list(
    included_columns       = shiny::isolate(shared_state$included_columns),
    column_type_overrides  = shiny::isolate(shared_state$column_type_overrides),
    column_transform_specs = shiny::isolate(shared_state$column_transform_specs),
    row_filter_specs       = shiny::isolate(shared_state$row_filter_specs)
  )
}


# Restore staged specs to last_applied_specs and signal all modules to sync their UIs.
# Increments shared_state$revert_trigger so modules can observe the revert event.
.revert_to_last_applied <- function(shared_state) {
  specs <- shiny::isolate(shared_state$last_applied_specs)
  if (is.null(specs)) return()
  shared_state$included_columns       <- specs$included_columns
  shared_state$column_type_overrides  <- specs$column_type_overrides
  shared_state$column_transform_specs <- specs$column_transform_specs
  shared_state$row_filter_specs       <- specs$row_filter_specs
  shared_state$has_pending_changes    <- FALSE
  shared_state$revert_trigger         <- shiny::isolate(shared_state$revert_trigger) + 1L
}


# Return names of staged transforms that fail validation.
.find_invalid_transforms <- function(shared_state) {
  names(.find_invalid_transforms_in(
    shiny::isolate(shared_state$column_transform_specs),
    shiny::isolate(shared_state$dataset_original)
  ))
}


# Return named list of col → reason string for the supplied transform specs.
# Accepts plain values so it can be called from both reactive and non-reactive contexts.
.find_invalid_transforms_in <- function(specs, dataset) {
  result  <- list()

  for (col in names(specs)) {
    spec <- specs[[col]]
    x    <- dataset[[col]]

    reason <- if (identical(spec$method, "cutpoints")) {
      breaks <- spec$breakpoints
      if (is.null(breaks) || length(breaks) == 0) {
        "no cutpoints specified"
      } else {
        x_min <- min(x, na.rm = TRUE)
        x_max <- max(x, na.rm = TRUE)
        if (length(breaks[breaks > x_min & breaks < x_max]) == 0)
          "cutpoints do not discriminate (all outside data range)"
        else
          NULL
      }
    } else if (identical(spec$method, "log")) {
      if (any(!is.na(x) & x <= 0)) "contains non-positive values (log undefined)" else NULL
    } else if (identical(spec$method, "winsorize")) {
      # The two boxes clamp themselves (.winsor_lower / .winsor_upper), so this
      # only catches a spec that reached the state some other way.
      lo <- if (!is.null(spec$lower_pct)) spec$lower_pct else EDARK_WINSOR_MIN
      hi <- if (!is.null(spec$upper_pct)) spec$upper_pct else EDARK_WINSOR_MAX - 1
      if (!.transform_spec_is_valid(spec, x))
        sprintf("percentiles must satisfy %g <= lower < upper <= %g (now %g and %g)",
                EDARK_WINSOR_MIN, EDARK_WINSOR_MAX, lo, hi)
      else
        NULL
    } else {
      NULL
    }

    if (!is.null(reason)) result[[col]] <- reason
  }
  result
}


# Build grouped warning list for the warnings panel.
# Accepts pre-read values (already extracted from shared_state in the renderUI)
# so all reactive dependencies are registered at the caller level.
# Returns a list of groups, each with $title (character) and $items (list of character).
.build_prepare_warnings <- function(specs, filters, included, last_applied, dataset) {
  groups <- list()

  # Group 1: columns whose transform has CHANGED since the last Apply and that
  # also have an active row filter. A transform that was already applied and whose
  # filter was added afterward is not a conflict - only flag changes to the spec.
  #
  # "Changed" includes a transform that has been REMOVED, which is why the column
  # set below is the union of the staged and last-applied names rather than the
  # staged names alone: a removed transform is absent from `specs`, so iterating
  # `specs` skipped the very case that leaves a banded filter pointing at a column
  # that is numeric again - zero rows on Apply, with no warning first. For the same
  # reason the gate is on `filters` alone; removing the only transform empties
  # `specs` but must still warn.
  if (length(filters) > 0) {
    last_tx <- if (!is.null(last_applied$column_transform_specs))
                 last_applied$column_transform_specs else list()
    pending_tx_cols <- Filter(function(col) {
      !identical(specs[[col]], last_tx[[col]])
    }, union(names(specs), names(last_tx)))
    conflict_cols <- intersect(pending_tx_cols, names(filters))
    if (length(conflict_cols) > 0) {
      groups <- c(groups, list(list(
        title = "Transform changed or removed on column(s) with an active row filter - filter will be removed on Apply:",
        items = as.list(conflict_cols)
      )))
    }
  }

  # Group 2: columns with row filters that are now excluded
  # (filter will be dropped on Apply since column is not in working dataset)
  if (length(filters) > 0) {
    excluded_filtered <- setdiff(names(filters), included)
    if (length(excluded_filtered) > 0) {
      groups <- c(groups, list(list(
        title = "Excluded column(s) have active row filters - filters will be removed on Apply:",
        items = as.list(excluded_filtered)
      )))
    }
  }

  # Group 2b: numeric filters whose lower limit is above the upper - they keep
  # no rows. The boxes accept it (swapping would move a number under the user),
  # so this is where it is caught.
  inverted <- names(Filter(function(f) identical(f$type, "numeric") &&
                             isTRUE(f$min > f$max), filters))
  if (length(inverted) > 0) {
    groups <- c(groups, list(list(
      title = "Row filter lower limit is above the upper limit - no rows will pass:",
      items = as.list(inverted)
    )))
  }

  # Group 3: transform validation issues (only for pending transforms)
  if (length(specs) > 0) {
    last_tx <- if (!is.null(last_applied$column_transform_specs))
                 last_applied$column_transform_specs else list()
    pending_specs <- specs[vapply(names(specs), function(col) {
      !identical(specs[[col]], last_tx[[col]])
    }, logical(1))]
    invalid_detail <- .find_invalid_transforms_in(pending_specs, dataset)
    if (length(invalid_detail) > 0) {
      items <- lapply(names(invalid_detail), function(col) {
        paste0(col, ": ", invalid_detail[[col]])
      })
      groups <- c(groups, list(list(
        title = "These variable transforms need attention:",
        items = items
      )))
    }
  }

  groups
}


# Remove row filter specs that would be invalidated before running the pipeline.
# Called at the top of do_apply() and .do_nav_apply().
#   - Pending transforms: only transforms that differ from last_applied_specs
#     (i.e., changed since the last Apply), including transforms that have been
#     REMOVED - hence the union of staged and last-applied names. A filter added
#     on an already-applied transform's output is valid and must not be removed.
#   - Excluded columns: column won't be in the working dataset.
.prune_conflicting_filter_specs <- function(shared_state) {
  specs        <- shiny::isolate(shared_state$column_transform_specs)
  filters      <- shiny::isolate(shared_state$row_filter_specs)
  included     <- shiny::isolate(shared_state$included_columns)
  last_applied <- shiny::isolate(shared_state$last_applied_specs)

  if (length(filters) == 0) return(invisible(NULL))

  last_tx <- if (!is.null(last_applied$column_transform_specs))
               last_applied$column_transform_specs else list()
  pending_tx_cols <- Filter(function(col) {
    !identical(specs[[col]], last_tx[[col]])
  }, union(names(specs), names(last_tx)))

  to_remove <- union(
    intersect(pending_tx_cols, names(filters)),  # pending-transform conflicts
    setdiff(names(filters), included)            # excluded column filters
  )

  if (length(to_remove) > 0) {
    for (col in to_remove) filters[[col]] <- NULL
    shared_state$row_filter_specs <- filters
  }

  invisible(NULL)
}


# Confirmation modal shown before Prepare changes are applied or reset while
# the custom report has items. Items store only a plot spec and are re-drawn
# from the working dataset when the custom report is generated, so proceeding
# keeps them and they reflect the new data.
#
# Three ways out, and a caller wires all three: confirm, revert, and discard
# the items (clear_id). Discard is what ends the dialog for the session - with
# only the first two, it fires again on the next Apply and on every later
# Prepare tab switch. Shared by the Apply / Reset buttons here and the
# tab-switch auto-apply in edark.R, which pass fully namespaced button ids.
.custom_items_modal <- function(n_items, cancel_id, confirm_id, confirm_label,
                                clear_id = NULL) {
  shiny::showModal(shiny::modalDialog(
    title = "Custom Report Will Use the Changed Data",
    shiny::tagList(
      shiny::p(paste0(
        "You have ", n_items, " item(s) in your custom report. They will be kept ",
        "and re-drawn from the changed dataset when you generate the report. ",
        "Their thumbnails still show the current data. Would you like to proceed?"
      )),
      # Without this the dialog reappears on every Apply and on every tab
      # switch for the rest of the session, with no way out of it from Prepare.
      if (!is.null(clear_id))
        shiny::p(class = "small text-muted mb-0",
                 "Discarding the items answers this question for good: it will ",
                 "not be asked again unless you queue new ones from Explore.")
    ),
    footer = shiny::tagList(
      edark_button(NULL, cancel_id, "Go Back & Revert Changes",
                   variant = "secondary", size = "dialog", outline = TRUE),
      if (!is.null(clear_id))
        edark_button(NULL, clear_id, "Discard Items & Continue",
                     icon = "trash", variant = "danger", size = "dialog",
                     outline = TRUE),
      edark_button(NULL, confirm_id, confirm_label, variant = "primary",
                   size = "dialog")
    ),
    easyClose = FALSE
  ))
}


# Drop every queued custom-report item, with a notification saying so.
#
# The Report module keeps its selected row in a local reactiveVal and resolves
# it by id against this list, so an emptied list is enough - it falls back to
# no selection on its own.
.clear_custom_report_items <- function(shared_state) {
  n <- length(shiny::isolate(shared_state$custom_report_items))
  shared_state$custom_report_items <- list()
  shiny::showNotification(
    sprintf("Discarded %d custom report item%s.", n, if (n == 1L) "" else "s"),
    type = "message", duration = 4
  )
}


# Rows with no NA in any column, as "n (pct%)".
#
# A muted dash for a missing or column-less dataset; a bare count when every
# row has been filtered out, where a percentage would be 0/0.
.complete_case_label <- function(d) {
  if (is.null(d) || ncol(d) == 0) {
    return(shiny::tags$em(class = "text-muted", "-"))
  }
  n <- tryCatch(sum(stats::complete.cases(d)), error = function(e) NA_integer_)
  if (is.na(n)) return(shiny::tags$em(class = "text-muted", "-"))
  if (nrow(d) == 0) return(format(n, big.mark = ","))
  sprintf("%s (%.1f%%)", format(n, big.mark = ","), 100 * n / nrow(d))
}


# Describe what is staged, one line per change, grouped by kind.
#
# The info pane says what the current settings produce (D2), so a count is not
# enough: "3 pending change(s)" never said which three
# (§BUILD_UI-redesign 1.3d). Returns a named list, one element per non-empty
# kind, each a character vector. Names are the headings shown in the pane.
.describe_pending_changes <- function(shared_state) {
  all_cols  <- names(shared_state$dataset_original)
  included  <- shared_state$included_columns
  transf    <- shared_state$column_transform_specs
  filters   <- shared_state$row_filter_specs

  groups <- list()

  excluded <- setdiff(all_cols, included)
  if (length(excluded) > 0) {
    groups[["Columns"]] <- sprintf(
      "%d excluded: %s", length(excluded), paste(excluded, collapse = ", ")
    )
  }

  if (length(transf) > 0) {
    groups[["Transforms"]] <- vapply(
      names(transf),
      function(col) .describe_one_transform(col, transf[[col]]),
      character(1), USE.NAMES = FALSE
    )
  }

  if (length(filters) > 0) {
    groups[["Row Filters"]] <- vapply(
      names(filters),
      function(col) .describe_one_filter(col, filters[[col]]),
      character(1), USE.NAMES = FALSE
    )
  }

  groups
}


# One transform, in the words the Transforms tab uses.
.describe_one_transform <- function(col, spec) {
  method <- spec$method %||% "none"
  switch(
    method,
    cutpoints = {
      n_bins <- length(spec$breakpoints %||% numeric(0)) + 1L
      sprintf("%s -> %d bands", col, n_bins)
    },
    log       = sprintf("%s(%s)", spec$log_base %||% "ln", col),
    winsorize = sprintf("%s winsorized", col),
    round     = sprintf("%s rounded", col),
    sprintf("%s: %s", col, method)
  )
}


# One row filter, with the range or the levels it keeps.
.describe_one_filter <- function(col, spec) {
  if (identical(spec$type, "numeric")) {
    at_min <- isTRUE(all.equal(spec$min, spec$data_min))
    at_max <- isTRUE(all.equal(spec$max, spec$data_max))
    fmt <- function(x) format(x, big.mark = ",", trim = TRUE)
    if (at_min && at_max)  sprintf("%s: full range", col)
    else if (at_min)       sprintf("%s <= %s", col, fmt(spec$max))
    else if (at_max)       sprintf("%s >= %s", col, fmt(spec$min))
    else                   sprintf("%s %s to %s", col, fmt(spec$min), fmt(spec$max))
  } else {
    kept <- spec$levels_selected %||% character(0)
    all_lv <- spec$levels_all %||% character(0)
    if (length(kept) == length(all_lv)) {
      sprintf("%s: all levels", col)
    } else {
      sprintf("%s in {%s}", col, paste(kept, collapse = ", "))
    }
  }
}
