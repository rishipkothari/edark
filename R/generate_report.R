# generate_report.R
# Core report generation logic — no Shiny dependency.
# Called by module_report.R (downloadHandler) and edark_report() programmatic API.


# ---------------------------------------------------------------------------
# Shared utilities
# ---------------------------------------------------------------------------

# Generate a safe HTML anchor ID from arbitrary text.
# Used by both section builders and the HTML assembler so anchors are consistent.
.make_html_anchor <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9_-]", "-", x)
  x <- gsub("-+", "-", x)
  x <- gsub("^-+|-+$", "", x)
  paste0("sec-", x)
}


# ---------------------------------------------------------------------------
# Dataset-level summary
# ---------------------------------------------------------------------------

#' Build a wide EDA summary table across all numeric and factor columns
#'
#' Returns a data.frame with one row per analyzed column. Numeric stats are
#' NA-filled for factor columns; Top_values is NA-filled for numeric columns.
#' Datetime and character columns are excluded.
#'
#' @param dataset A data.frame.
#' @param column_types Named character vector from detect_column_types().
#' @param variables Optional character vector restricting the summary to these
#'   columns (in the order given). NULL summarises every eligible column.
#' @return A data.frame suitable for flextable().
#' @noRd
.build_dataset_summary <- function(dataset, column_types, variables = NULL) {
  analyzed_cols <- names(column_types)[column_types %in% c("numeric", "factor")]
  analyzed_cols <- analyzed_cols[analyzed_cols %in% names(dataset)]
  # Report callers pass the selected variables so the summary matches what the
  # report actually contains, not the whole working dataset.
  if (!is.null(variables))
    analyzed_cols <- variables[variables %in% analyzed_cols]

  rows <- lapply(analyzed_cols, function(col) {
    x        <- dataset[[col]]
    col_type <- column_types[[col]]
    n_total  <- length(x)
    n_miss   <- sum(is.na(x))
    pct_miss <- sprintf("%.1f%%", 100 * n_miss / n_total)
    n_unique <- length(unique(x[!is.na(x)]))

    if (col_type == "numeric") {
      vals <- x[!is.na(x)]
      data.frame(
        Variable   = col,
        Type       = "numeric",
        N          = n_total,
        N_missing  = n_miss,
        Pct_miss   = pct_miss,
        N_unique   = n_unique,
        Min        = if (length(vals)) round(min(vals), 2)    else NA_real_,
        Max        = if (length(vals)) round(max(vals), 2)    else NA_real_,
        Mean       = if (length(vals)) round(mean(vals), 2)   else NA_real_,
        SD         = if (length(vals)) round(sd(vals), 2)     else NA_real_,
        Median     = if (length(vals)) round(median(vals), 2) else NA_real_,
        IQR        = if (length(vals)) round(IQR(vals), 2)    else NA_real_,
        Skewness   = tryCatch(round(e1071::skewness(vals), 2), error = function(e) NA_real_),
        Kurtosis   = tryCatch(round(e1071::kurtosis(vals), 2), error = function(e) NA_real_),
        Top_values = NA_character_,
        stringsAsFactors = FALSE
      )
    } else {
      vals <- x[!is.na(x)]
      tbl  <- sort(table(as.character(vals)), decreasing = TRUE)
      top5 <- if (length(tbl) == 0) NA_character_ else
        paste(head(names(tbl), 5), collapse = " | ")
      data.frame(
        Variable   = col,
        Type       = "factor",
        N          = n_total,
        N_missing  = n_miss,
        Pct_miss   = pct_miss,
        N_unique   = n_unique,
        Min        = NA_real_,
        Max        = NA_real_,
        Mean       = NA_real_,
        SD         = NA_real_,
        Median     = NA_real_,
        IQR        = NA_real_,
        Skewness   = NA_real_,
        Kurtosis   = NA_real_,
        Top_values = top5,
        stringsAsFactors = FALSE
      )
    }
  })

  do.call(rbind, rows)
}


#' Style the dataset summary data.frame as a flextable
#' @noRd
.style_dataset_summary_ft <- function(df) {
  # Replace NA numerics with em-dash for display
  num_cols <- c("Min", "Max", "Mean", "SD", "Median", "IQR", "Skewness", "Kurtosis")
  df_disp  <- df
  for (col in num_cols) {
    df_disp[[col]] <- ifelse(is.na(df_disp[[col]]), "-", as.character(df_disp[[col]]))
  }
  df_disp$Top_values <- ifelse(is.na(df_disp$Top_values), "-", df_disp$Top_values)

  right_cols <- c("N", "N_missing", "Pct_miss", "N_unique",
                  "Min", "Max", "Mean", "SD", "Median", "IQR", "Skewness", "Kurtosis")

  ft <- flextable::flextable(df_disp)
  ft <- flextable::set_header_labels(ft,
    Variable   = "Variable",
    Type       = "Type",
    N          = "N",
    N_missing  = "N Missing",
    Pct_miss   = "% Missing",
    N_unique   = "Unique",
    Min        = "Min",
    Max        = "Max",
    Mean       = "Mean",
    SD         = "SD",
    Median     = "Median",
    IQR        = "IQR",
    Skewness   = "Skewness",
    Kurtosis   = "Kurtosis",
    Top_values = "Top Values (categorical)"
  )
  ft <- flextable::font(ft, fontname = "Arial", part = "all")
  ft <- flextable::fontsize(ft, size = 7, part = "body")
  ft <- flextable::fontsize(ft, size = 8, part = "header")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::bg(ft,
    i   = seq(1, nrow(df_disp), by = 2),
    bg  = "#F7F7F7",
    part = "body"
  )
  ft <- flextable::align(ft, part = "header", align = "center")
  ft <- flextable::align(ft, j = c("Variable", "Type", "Top_values"),
                         align = "left",  part = "body")
  ft <- flextable::align(ft, j = right_cols, align = "right", part = "body")
  ft <- flextable::border_remove(ft)
  ft <- flextable::hline_top(ft,  part = "header",
                              border = officer::fp_border(width = 1.2))
  ft <- flextable::hline(ft,      part = "header",
                          border = officer::fp_border(width = 0.6))
  ft <- flextable::hline_bottom(ft, part = "body",
                                 border = officer::fp_border(width = 1.2))
  ft <- flextable::width(ft, j = "Variable",   width = 1.5)
  ft <- flextable::width(ft, j = "Type",       width = 0.6)
  ft <- flextable::width(ft, j = "N",          width = 0.4)
  ft <- flextable::width(ft, j = "N_missing",  width = 0.6)
  ft <- flextable::width(ft, j = "Pct_miss",   width = 0.6)
  ft <- flextable::width(ft, j = "N_unique",   width = 0.5)
  ft <- flextable::width(ft, j = "Min",        width = 0.55)
  ft <- flextable::width(ft, j = "Max",        width = 0.55)
  ft <- flextable::width(ft, j = "Mean",       width = 0.55)
  ft <- flextable::width(ft, j = "SD",         width = 0.55)
  ft <- flextable::width(ft, j = "Median",     width = 0.55)
  ft <- flextable::width(ft, j = "IQR",        width = 0.45)
  ft <- flextable::width(ft, j = "Skewness",   width = 0.6)
  ft <- flextable::width(ft, j = "Kurtosis",   width = 0.6)
  ft <- flextable::width(ft, j = "Top_values", width = 2.0)
  ft <- flextable::set_table_properties(ft, layout = "fixed", align = "center")
  ft
}


# ---------------------------------------------------------------------------
# Table One
# ---------------------------------------------------------------------------

#' Build a Table One data frame (one row per variable; factor vars multi-row).
#'
#' For numeric variables: "mean (SD)" in the Overall and per-stratum columns
#' with a Kruskal-Wallis p-value (omitted when not stratified).
#' For factor variables: N (%) per level with a chi-square / Fisher's p-value.
#'
#' @param dataset A data.frame.
#' @param column_types Named character vector from detect_column_types().
#' @param variables Character vector of variable names to include.
#' @param stratify_by Column name to stratify by, or NULL.
#' @return A data.frame suitable for .style_tableone_ft().
#' @noRd
.build_tableone_df <- function(dataset, column_types, variables, stratify_by) {
  has_strat  <- !is.null(stratify_by) && nzchar(stratify_by) &&
                stratify_by %in% names(dataset)
  strata     <- if (has_strat) levels(factor(dataset[[stratify_by]])) else character(0)
  n_overall  <- nrow(dataset)
  n_strata   <- if (has_strat) vapply(strata, function(s)
    sum(!is.na(dataset[[stratify_by]]) & dataset[[stratify_by]] == s, na.rm = TRUE),
    integer(1)) else integer(0)

  # Build dynamic column names. These reach the reader as the table's column
  # headers, so every as.data.frame() below passes check.names = FALSE:
  # R's default sanitising turned "Overall (n=500)" into "Overall..n.500."
  # and "p-value" into "p.value", and flextable printed exactly that.
  overall_col <- paste0("Overall (n=", n_overall, ")")
  strat_cols  <- if (has_strat)
    paste0(strata, " (n=", n_strata, ")") else character(0)
  p_col       <- "p-value"

  # Helper: format mean (SD)
  fmt_mean_sd <- function(x) {
    vals <- x[!is.na(x)]
    if (length(vals) == 0) return(NA_character_)
    paste0(round(mean(vals), 1), " (", round(sd(vals), 1), ")")
  }

  all_cols <- c("Variable", overall_col, strat_cols, if (has_strat) p_col)

  rows <- lapply(variables, function(var) {
    if (!var %in% names(column_types)) return(NULL)
    vtype <- column_types[[var]]
    x     <- dataset[[var]]

    if (vtype == "numeric") {
      # One row: "var, mean (SD)"
      row <- setNames(
        as.list(rep(NA_character_, length(all_cols))),
        all_cols
      )
      row[["Variable"]]   <- paste0(var, ", mean (SD)")
      row[[overall_col]]  <- fmt_mean_sd(x)
      if (has_strat) {
        for (i in seq_along(strata)) {
          idx          <- !is.na(dataset[[stratify_by]]) & dataset[[stratify_by]] == strata[i]
          row[[strat_cols[i]]] <- fmt_mean_sd(x[idx])
        }
        # Kruskal-Wallis p (stats_inference.R)
        row[[p_col]] <- tryCatch({
          gt <- edark_group_test(x, factor(dataset[[stratify_by]], levels = strata))
          if (is.na(gt$p.value)) NA_character_ else edark_format_p(gt$p.value)
        }, error = function(e) NA_character_)
      }
      list(as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE))

    } else if (vtype == "factor") {
      lvls <- if (is.factor(x)) levels(x) else
        sort(unique(as.character(x[!is.na(x)])))
      n_valid_overall <- sum(!is.na(x))

      # Chi-square / Fisher's exact p (stats_inference.R)
      p_val <- if (has_strat) {
        tryCatch({
          gt <- edark_group_test(factor(x, levels = lvls),
                                 factor(dataset[[stratify_by]], levels = strata))
          if (is.na(gt$p.value)) NA_character_ else edark_format_p(gt$p.value)
        }, error = function(e) NA_character_)
      } else NA_character_

      # Header row (variable name + N overall + N per stratum + p)
      hdr <- setNames(as.list(rep(NA_character_, length(all_cols))), all_cols)
      hdr[["Variable"]]  <- var
      hdr[[overall_col]] <- paste0("n=", n_valid_overall)
      if (has_strat) {
        for (i in seq_along(strata)) {
          idx <- !is.na(dataset[[stratify_by]]) & dataset[[stratify_by]] == strata[i]
          hdr[[strat_cols[i]]] <- paste0("n=", sum(!is.na(x[idx])))
        }
        hdr[[p_col]] <- p_val
      }

      # Level rows (indented with two spaces)
      level_rows <- lapply(lvls, function(lv) {
        row <- setNames(as.list(rep(NA_character_, length(all_cols))), all_cols)
        row[["Variable"]] <- paste0("  ", lv)
        n_lv <- sum(!is.na(x) & as.character(x) == lv)
        pct  <- if (n_valid_overall > 0) round(100 * n_lv / n_valid_overall, 1) else 0
        row[[overall_col]] <- paste0(n_lv, " (", pct, "%)")
        if (has_strat) {
          for (i in seq_along(strata)) {
            idx      <- !is.na(dataset[[stratify_by]]) & dataset[[stratify_by]] == strata[i]
            x_s      <- x[idx]
            n_valid_s <- sum(!is.na(x_s))
            n_lv_s   <- sum(!is.na(x_s) & as.character(x_s) == lv)
            pct_s    <- if (n_valid_s > 0) round(100 * n_lv_s / n_valid_s, 1) else 0
            row[[strat_cols[i]]] <- paste0(n_lv_s, " (", pct_s, "%)")
          }
          row[[p_col]] <- NA_character_
        }
        as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
      })

      c(list(as.data.frame(hdr, stringsAsFactors = FALSE, check.names = FALSE)), level_rows)
    } else {
      NULL
    }
  })

  rows <- unlist(rows, recursive = FALSE)
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}


#' Style a Table One data frame as a flextable
#' @noRd
.style_tableone_ft <- function(df, has_strat = FALSE) {
  if (nrow(df) == 0 || ncol(df) == 0) return(flextable::flextable(data.frame()))

  # Bold header rows (non-indented Variable rows that are not numeric stats)
  is_header_row <- !grepl("^  ", df[["Variable"]]) & !grepl(", mean \\(SD\\)$", df[["Variable"]])
  # Indent level rows
  df_disp <- df
  df_disp[["Variable"]] <- trimws(df_disp[["Variable"]])  # display trim (indentation via padding)

  ft <- flextable::flextable(df_disp)
  ft <- flextable::font(ft, fontname = "Arial", part = "all")
  ft <- flextable::fontsize(ft, size = 8,  part = "body")
  ft <- flextable::fontsize(ft, size = 9,  part = "header")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::bold(ft, i = which(is_header_row), part = "body")
  ft <- flextable::padding(ft,
    i    = which(grepl("^  ", df[["Variable"]])),
    j    = "Variable",
    padding.left = 16,
    part = "body"
  )
  ft <- flextable::bg(ft, i = seq(1, nrow(df_disp), by = 2), bg = "#F7F7F7", part = "body")
  ft <- flextable::border_remove(ft)
  ft <- flextable::hline_top(ft,    part = "header", border = officer::fp_border(width = 1.2))
  ft <- flextable::hline(ft,        part = "header", border = officer::fp_border(width = 0.6))
  ft <- flextable::hline_bottom(ft, part = "body",   border = officer::fp_border(width = 1.2))
  ft <- flextable::align(ft, part = "header", align = "center")
  ft <- flextable::align(ft, j = "Variable", align = "left",   part = "body")
  ft <- flextable::align(ft, j = setdiff(names(df_disp), "Variable"),
                         align = "center", part = "body")
  ft <- flextable::width(ft, j = "Variable", width = 2.0)
  ft <- flextable::set_table_properties(ft, layout = "autofit", align = "center")
  ft
}


#' Collinearity investigation for the report
#'
#' The same three pieces as Analyze › Variable Investigation › Collinearity,
#' from the same \code{compute_collinearity()}: a Pearson heatmap of the
#' numeric variables, a Cramer's V heatmap of the factors, and the pairs above
#' 0.7. A heatmap needs at least two variables of its type. Numeric x factor
#' pairs are not measured, there or here.
#'
#' @return \code{NULL} when fewer than two numeric or factor variables, else
#'   \code{list(cor_plot, cramers_plot, flagged_ft)}; either plot may be
#'   \code{NULL}.
#' @noRd
.build_collinearity_report <- function(dataset, column_types, variables) {
  vars <- variables[variables %in% names(column_types) &
                      column_types[variables] %in% c("numeric", "factor")]
  if (length(vars) < 2L) return(NULL)
  coll <- tryCatch(compute_collinearity(dataset, vars), error = function(e) NULL)
  if (is.null(coll)) return(NULL)

  cor_plot <- if (!is.null(coll$cor_matrix) && ncol(coll$cor_matrix) >= 2L)
    .plot_correlation_heatmap(coll$cor_matrix, "Pearson Correlation (Numeric Variables)")
  cramers_plot <- if (!is.null(coll$cramers_v_mat) && ncol(coll$cramers_v_mat) >= 2L)
    .plot_cramers_heatmap(coll$cramers_v_mat, "Cram\u00e9r's V (Factor Variables)")
  if (is.null(cor_plot) && is.null(cramers_plot)) return(NULL)

  fp <- coll$flagged_pairs
  fp_df <- if (nrow(fp) > 0L) {
    data.frame(`Variable 1` = fp$var1, `Variable 2` = fp$var2,
               Measure = fp$type, Value = sprintf("%.2f", fp$value),
               check.names = FALSE, stringsAsFactors = FALSE)
  } else {
    data.frame(Result = "No pair of variables above 0.7.", stringsAsFactors = FALSE)
  }

  list(cor_plot     = cor_plot,
       cramers_plot = cramers_plot,
       flagged_ft   = .style_section_ft(fp_df))
}


# ---------------------------------------------------------------------------
# Per-section table helpers
# ---------------------------------------------------------------------------

#' Univariate numeric: key-value table with Overall + per-stratum columns
#' @noRd
.build_univariate_numeric_table <- function(dataset, variable, stratify_by) {
  compute_stats <- function(x) {
    vals <- x[!is.na(x)]
    n    <- length(x)
    list(
      N        = n,
      Missing  = sum(is.na(x)),
      Mean     = if (length(vals)) round(mean(vals),   2) else NA_real_,
      Median   = if (length(vals)) round(median(vals), 2) else NA_real_,
      SD       = if (length(vals)) round(sd(vals),     2) else NA_real_,
      IQR      = if (length(vals)) round(IQR(vals),    2) else NA_real_,
      Min      = if (length(vals)) round(min(vals),    2) else NA_real_,
      Max      = if (length(vals)) round(max(vals),    2) else NA_real_,
      Skewness = tryCatch(round(e1071::skewness(vals), 2), error = function(e) NA_real_),
      Kurtosis = tryCatch(round(e1071::kurtosis(vals), 2), error = function(e) NA_real_)
    )
  }

  x         <- dataset[[variable]]
  overall   <- compute_stats(x)
  stat_names <- names(overall)

  df <- data.frame(Statistic = stat_names,
                   Overall   = unlist(overall),
                   stringsAsFactors = FALSE, row.names = NULL)

  if (!is.null(stratify_by) && stratify_by %in% names(dataset)) {
    strata <- levels(factor(dataset[[stratify_by]]))
    for (s in strata) {
      idx      <- !is.na(dataset[[stratify_by]]) & as.character(dataset[[stratify_by]]) == s
      s_stats  <- compute_stats(x[idx])
      df[[s]]  <- unlist(s_stats)
    }
  }

  df
}


#' Univariate factor: level counts with Overall + per-stratum columns
#' @noRd
.build_univariate_factor_table <- function(dataset, variable, stratify_by) {
  x      <- dataset[[variable]]
  levels_all <- if (is.factor(x)) levels(x) else sort(unique(as.character(x[!is.na(x)])))

  count_pct <- function(vec, lvls) {
    n_total <- length(vec)
    n_miss  <- sum(is.na(vec))
    tbl     <- table(factor(vec, levels = lvls))
    pcts    <- round(100 * as.integer(tbl) / (n_total - n_miss), 1)
    list(n_total = n_total, n_miss = n_miss, counts = as.integer(tbl), pcts = pcts)
  }

  overall <- count_pct(x, levels_all)

  # Header rows: N total, N missing
  header_rows <- data.frame(
    Level         = c("N", "N Missing"),
    `N (Overall)` = c(overall$n_total, overall$n_miss),
    `% (Overall)` = c(NA_real_, NA_real_),
    stringsAsFactors = FALSE, check.names = FALSE
  )

  # Level rows
  level_rows <- data.frame(
    Level         = levels_all,
    `N (Overall)` = overall$counts,
    `% (Overall)` = overall$pcts,
    stringsAsFactors = FALSE, check.names = FALSE
  )

  df <- rbind(header_rows, level_rows)

  if (!is.null(stratify_by) && stratify_by %in% names(dataset)) {
    strata <- levels(factor(dataset[[stratify_by]]))
    for (s in strata) {
      idx    <- !is.na(dataset[[stratify_by]]) & as.character(dataset[[stratify_by]]) == s
      s_res  <- count_pct(x[idx], levels_all)
      n_col  <- paste0("N (", s, ")")
      pct_col <- paste0("% (", s, ")")
      df[[n_col]]   <- c(s_res$n_total, s_res$n_miss, s_res$counts)
      df[[pct_col]] <- c(NA_real_,       NA_real_,       s_res$pcts)
    }
  }

  df
}


#' Bivariate numeric x numeric: correlation stats table
#' @noRd
.build_bivariate_num_num_table <- function(dataset, col_a, col_b, stratify_by) {
  run_cor <- function(x, y) {
    ct <- edark_cor_test(x, y)
    if (is.na(ct$r)) return(list(r = NA, r2 = NA, p = NA, ci_lo = NA, ci_hi = NA))
    list(
      r     = round(ct$r, 3),
      r2    = round(ct$r^2, 3),
      p     = edark_format_p(ct$p.value),
      ci_lo = round(ct$conf.low, 3),
      ci_hi = round(ct$conf.high, 3)
    )
  }

  x       <- dataset[[col_a]]
  y       <- dataset[[col_b]]
  overall <- run_cor(x, y)

  df <- data.frame(
    Statistic = c("r", "R\u00b2", "p-value", "95% CI (low)", "95% CI (high)"),
    Overall   = c(overall$r, overall$r2, overall$p, overall$ci_lo, overall$ci_hi),
    stringsAsFactors = FALSE
  )

  if (!is.null(stratify_by) && stratify_by %in% names(dataset)) {
    strata <- levels(factor(dataset[[stratify_by]]))
    for (s in strata) {
      idx     <- !is.na(dataset[[stratify_by]]) & as.character(dataset[[stratify_by]]) == s
      s_res   <- run_cor(x[idx], y[idx])
      df[[s]] <- c(s_res$r, s_res$r2, s_res$p, s_res$ci_lo, s_res$ci_hi)
    }
  }

  df
}


#' Bivariate numeric x factor: distribution by factor level + Kruskal-Wallis p
#' @noRd
.build_bivariate_num_fac_table <- function(dataset, numeric_col, factor_col, stratify_by) {
  num_vec <- dataset[[numeric_col]]
  fac_vec <- dataset[[factor_col]]
  lvls    <- if (is.factor(fac_vec)) levels(fac_vec) else
    sort(unique(as.character(fac_vec[!is.na(fac_vec)])))

  compute_by_level <- function(num, fac, prefix = "") {
    rows <- lapply(lvls, function(lv) {
      idx  <- !is.na(fac) & as.character(fac) == lv
      vals <- num[idx & !is.na(num)]
      data.frame(
        Level  = lv,
        N      = sum(idx),
        Mean   = if (length(vals)) round(mean(vals),   2) else NA_real_,
        Median = if (length(vals)) round(median(vals), 2) else NA_real_,
        SD     = if (length(vals)) round(sd(vals),     2) else NA_real_,
        IQR    = if (length(vals)) round(IQR(vals),    2) else NA_real_,
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, rows)
  }

  df <- compute_by_level(num_vec, fac_vec)

  # Kruskal-Wallis p appended as footer row
  mw_p <- tryCatch({
    gt <- edark_group_test(num_vec, factor(fac_vec, levels = lvls))
    if (is.na(gt$p.value)) NA_character_ else edark_format_p(gt$p.value)
  }, error = function(e) NA_character_)

  footer <- data.frame(
    Level  = if (!is.na(mw_p)) paste0("Kruskal-Wallis p ", if (startsWith(mw_p, "<")) mw_p else paste("=", mw_p))
             else "Kruskal-Wallis p = NA",
    N      = NA_real_, Mean = NA_real_, Median = NA_real_, SD = NA_real_, IQR = NA_real_,
    stringsAsFactors = FALSE
  )

  rbind(df, footer)
}


#' Bivariate factor x factor: cross-tab N (%) + chi-square (or Fisher's) footer
#' Uses Fisher's exact test (simulated p-value) when any expected cell count < 5.
#' @noRd
.build_bivariate_fac_fac_table <- function(dataset, col_a, col_b, stratify_by) {
  make_crosstab <- function(a_vec, b_vec, a_lvls, b_lvls) {
    tbl   <- table(factor(a_vec, levels = a_lvls), factor(b_vec, levels = b_lvls))
    n_row <- rowSums(tbl)
    rows  <- lapply(seq_along(a_lvls), function(i) {
      cells <- vapply(seq_along(b_lvls), function(j) {
        n   <- tbl[i, j]
        pct <- if (n_row[i] > 0) round(100 * n / n_row[i], 1) else 0
        paste0(n, " (", pct, "%)")
      }, character(1))
      as.data.frame(t(c(Level = a_lvls[i], cells)), stringsAsFactors = FALSE)
    })
    out <- do.call(rbind, rows)
    names(out) <- c(col_a, b_lvls)
    out
  }

  a_vec  <- dataset[[col_a]]
  b_vec  <- dataset[[col_b]]
  a_lvls <- if (is.factor(a_vec)) levels(a_vec) else sort(unique(as.character(a_vec[!is.na(a_vec)])))
  b_lvls <- if (is.factor(b_vec)) levels(b_vec) else sort(unique(as.character(b_vec[!is.na(b_vec)])))

  df <- make_crosstab(a_vec, b_vec, a_lvls, b_lvls)

  # Chi-square, or Fisher's exact when any expected count < 5 (stats_inference.R)
  footer_label <- tryCatch({
    gt <- edark_group_test(factor(a_vec, levels = a_lvls), factor(b_vec, levels = b_lvls))
    if (is.na(gt$p.value)) "Test p = NA" else {
      p <- edark_format_p(gt$p.value)
      paste0(gt$method, ": p ", if (startsWith(p, "<")) p else paste("=", p))
    }
  }, error = function(e) "Test p = NA")

  footer            <- df[1, , drop = FALSE]
  footer[1, ]       <- NA
  footer[[col_a]]   <- footer_label
  rbind(df, footer)
}


#' Style a section summary data.frame as a compact flextable
#' @noRd
.style_section_ft <- function(df) {
  ft <- flextable::flextable(df)
  ft <- flextable::font(ft, fontname = "Arial", part = "all")
  ft <- flextable::fontsize(ft, size = 8,  part = "body")
  ft <- flextable::fontsize(ft, size = 9,  part = "header")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::bg(ft,
    i    = seq(1, nrow(df), by = 2),
    bg   = "#F7F7F7",
    part = "body"
  )
  ft <- flextable::border_remove(ft)
  ft <- flextable::hline_top(ft,    part = "header", border = officer::fp_border(width = 1.2))
  ft <- flextable::hline(ft,        part = "header", border = officer::fp_border(width = 0.6))
  ft <- flextable::hline_bottom(ft, part = "body",   border = officer::fp_border(width = 1.2))
  ft <- flextable::set_table_properties(ft, layout = "autofit", align = "center")
  ft
}


# ---------------------------------------------------------------------------
# Section builders
# ---------------------------------------------------------------------------

.build_all_vars_sections <- function(dataset, column_types, variables, stratify_variable,
                                      progress_fn = NULL, plot_aesthetics = list()) {
  # Only numeric and factor — datetime excluded
  variables <- variables[variables %in% names(column_types) &
                           column_types[variables] %in% c("numeric", "factor")]
  # Describing a variable against itself as stratifier is nonsensical — skip it
  if (!is.null(stratify_variable) && nzchar(stratify_variable))
    variables <- variables[variables != stratify_variable]
  n <- length(variables)

  sections <- vector("list", n)
  for (i in seq_along(variables)) {
    if (!is.null(progress_fn)) progress_fn(i / n, paste0("Building plots: variable ", i, " of ", n, ": ", variables[[i]]))

    var      <- variables[[i]]
    var_type <- column_types[[var]]

    stratify_by <- if (!is.null(stratify_variable) && nzchar(stratify_variable) &&
                        stratify_variable %in% names(dataset))
      stratify_variable else NULL

    spec <- modifyList(list(
      plot_type        = route_plot_type(var_type, NULL),
      column_a         = var,
      column_b         = NULL,
      primary_role     = "exposure",
      stratify_by      = stratify_by,
      color_palette    = "Set2",
      show_data_labels = FALSE,
      show_legend      = TRUE,
      legend_position  = "top",
      trend_resolution = "Month"
    ), plot_aesthetics)

    plot_obj <- render_plot(spec, dataset, split_panels = TRUE)

    summary_df <- if (var_type == "numeric") {
      .build_univariate_numeric_table(dataset, var, stratify_by)
    } else {
      .build_univariate_factor_table(dataset, var, stratify_by)
    }
    summary_ft <- .style_section_ft(summary_df)

    sections[[i]] <- list(
      title      = var,
      anchor     = .make_html_anchor(var),
      plot_obj   = plot_obj,
      summary_ft = summary_ft
    )
  }
  Filter(Negate(is.null), sections)
}


.build_primary_vs_others_sections <- function(dataset, column_types,
                                               secondary_vars,
                                               primary_variable,
                                               primary_role,
                                               stratify_variable,
                                               progress_fn = NULL,
                                               plot_aesthetics = list()) {
  # Only numeric and factor — datetime excluded from both primary and secondary
  valid_types <- c("numeric", "factor")
  if (!primary_variable %in% names(column_types) ||
      !column_types[[primary_variable]] %in% valid_types)
    stop("primary_variable must be numeric or factor.")

  secondary_vars <- secondary_vars[
    secondary_vars %in% names(column_types) &
    column_types[secondary_vars] %in% valid_types
  ]

  # Skip any secondary variable that is also the stratify variable — plotting
  # a variable against itself as both a comparison and a stratifier is nonsensical.
  if (!is.null(stratify_variable) && nzchar(stratify_variable)) {
    secondary_vars <- secondary_vars[secondary_vars != stratify_variable]
  }

  n <- length(secondary_vars)
  sections <- vector("list", n)

  for (i in seq_along(secondary_vars)) {
    if (!is.null(progress_fn)) progress_fn(i / n, paste0("Building plots: variable ", i, " of ", n, ": ", secondary_vars[[i]]))

    sec_var <- secondary_vars[[i]]

    # Axis assignment from role
    if (primary_role == "exposure") {
      col_a      <- primary_variable
      col_b      <- sec_var
      col_a_type <- column_types[[primary_variable]]
      col_b_type <- column_types[[sec_var]]
    } else {
      col_a      <- sec_var
      col_b      <- primary_variable
      col_a_type <- column_types[[sec_var]]
      col_b_type <- column_types[[primary_variable]]
    }

    # violin_jitter normalization: factor must be col_a (X)
    if (col_a_type == "numeric" && col_b_type == "factor") {
      tmp        <- col_a;      col_a      <- col_b;      col_b      <- tmp
      tmp        <- col_a_type; col_a_type <- col_b_type; col_b_type <- tmp
    }

    stratify_by <- if (!is.null(stratify_variable) && nzchar(stratify_variable) &&
                        stratify_variable %in% names(dataset))
      stratify_variable else NULL

    spec <- modifyList(list(
      plot_type        = route_plot_type(col_a_type, col_b_type),
      column_a         = col_a,
      column_b         = col_b,
      primary_role     = primary_role,
      stratify_by      = stratify_by,
      color_palette    = "Set2",
      show_data_labels = FALSE,
      show_legend      = TRUE,
      legend_position  = "top",
      trend_resolution = "Month"
    ), plot_aesthetics)

    plot_obj <- render_plot(spec, dataset, split_panels = TRUE)

    # Select appropriate bivariate table helper
    summary_df <- if (col_a_type == "numeric" && col_b_type == "numeric") {
      .build_bivariate_num_num_table(dataset, col_a, col_b, stratify_by)
    } else if (col_a_type == "factor" && col_b_type == "numeric") {
      # col_a is factor (X), col_b is numeric (Y) after normalisation
      .build_bivariate_num_fac_table(dataset, col_b, col_a, stratify_by)
    } else if (col_a_type == "factor" && col_b_type == "factor") {
      .build_bivariate_fac_fac_table(dataset, col_a, col_b, stratify_by)
    } else {
      NULL
    }

    summary_ft <- if (!is.null(summary_df)) .style_section_ft(summary_df) else NULL

    title <- paste(primary_variable, "\u00d7", sec_var)
    if (!is.null(stratify_by))
      title <- paste(title, "\u00b7 stratified by", stratify_by)

    sections[[i]] <- list(
      title      = title,
      anchor     = .make_html_anchor(sec_var),   # keyed on secondary var for summary table links
      plot_obj   = plot_obj,
      summary_ft = summary_ft
    )
  }
  Filter(Negate(is.null), sections)
}


# ---------------------------------------------------------------------------
# Progress phases
# ---------------------------------------------------------------------------
# A report is built in two passes - build every section, then write the file -
# and both count through the sections. Each pass gets its own share of one bar,
# or the bar fills twice. HTML has no per-section write step (rmarkdown renders
# in one call), so building takes almost all of its bar.

# Map a pass's 0-1 progress onto [from, to] of the whole bar.
.progress_span <- function(progress_fn, from, to) {
  if (is.null(progress_fn)) return(NULL)
  function(frac, detail) progress_fn(from + frac * (to - from), detail)
}

# Where building ends and writing begins, per format.
.progress_split <- function(format) if (identical(format, "html")) 0.9 else 0.6


# ---------------------------------------------------------------------------
# Format assemblers
# ---------------------------------------------------------------------------

# Helper: add a title bar to a PPTX slide
.pptx_title <- function(prs, title) {
  officer::ph_with(
    prs,
    value    = title,
    location = officer::ph_location(left = 0.4, top = 0.2, width = 12.2, height = 0.55)
  )
}

# Helper: add a plot to a PPTX slide (full-width body area)
# Patchwork objects are rasterised to PNG because rvg::dml() only accepts plain ggplots.
.pptx_plot <- function(prs, plot_obj) {
  plot_loc <- officer::ph_location(left = 0.4, top = 0.85, width = 12.2, height = 6.0)
  if (inherits(plot_obj, "patchwork")) {
    tmp_png <- tempfile(fileext = ".png")
    on.exit(unlink(tmp_png), add = TRUE)
    ggplot2::ggsave(tmp_png, plot = plot_obj, width = 12.2, height = 6.0,
                    units = "in", dpi = 150)
    officer::ph_with(prs,
      value    = officer::external_img(src = tmp_png, width = 12.2, height = 6.0),
      location = plot_loc)
  } else {
    officer::ph_with(prs,
      value    = rvg::dml(ggobj = plot_obj),
      location = plot_loc)
  }
}

# Helper: add a flextable to a PPTX slide (full-width body area)
.pptx_table <- function(prs, ft) {
  officer::ph_with(prs,
    value    = ft,
    location = officer::ph_location(left = 0.4, top = 0.85, width = 12.2, height = 6.0))
}


.assemble_pptx <- function(sections, dataset_summary_df, output_path,
                             progress_fn = NULL,
                             include_dataset_summary = TRUE,
                             tableone_ft = NULL,
                             collinearity = NULL) {
  prs <- officer::read_pptx(
    system.file("templates/ppt_16x9_blank_template.pptx", package = "edark")
  )

  # ── Optional: Table One slide ────────────────────────────────────────────
  if (!is.null(tableone_ft)) {
    prs <- officer::add_slide(prs, layout = "Blank", master = "Office Theme")
    prs <- .pptx_title(prs, "Table 1")
    prs <- .pptx_table(prs, tableone_ft)
  }

  # ── Optional: Collinearity - one slide per heatmap, then the pairs ───────
  if (!is.null(collinearity)) {
    for (pl in Filter(Negate(is.null),
                      list(collinearity$cor_plot, collinearity$cramers_plot))) {
      prs <- officer::add_slide(prs, layout = "Blank", master = "Office Theme")
      prs <- .pptx_title(prs, "Collinearity")
      prs <- .pptx_plot(prs, pl)
    }
    prs <- officer::add_slide(prs, layout = "Blank", master = "Office Theme")
    prs <- .pptx_title(prs, "Collinearity: Pairs Above 0.7")
    prs <- .pptx_table(prs, collinearity$flagged_ft)
  }

  # ── Optional: Dataset summary slide ─────────────────────────────────────
  if (isTRUE(include_dataset_summary)) {
    prs <- officer::add_slide(prs, layout = "Blank", master = "Office Theme")
    prs <- .pptx_title(prs, "Dataset Summary")
    ds_ft <- .style_dataset_summary_ft(dataset_summary_df)
    prs   <- .pptx_table(prs, ds_ft)
  }

  # ── Per-section: plot slide + table slide ────────────────────────────────
  n <- length(sections)
  for (i in seq_along(sections)) {
    if (!is.null(progress_fn)) progress_fn(i / n, paste0("Writing slides: section ", i, " of ", n))
    sec <- sections[[i]]

    # Plot slide(s) — split_panels may give a list of two ggplots
    plots <- if (is.list(sec$plot_obj) && !inherits(sec$plot_obj, "ggplot")) sec$plot_obj else list(sec$plot_obj)
    for (pl in plots) {
      prs <- officer::add_slide(prs, layout = "Blank", master = "Office Theme")
      prs <- .pptx_title(prs, sec$title)
      prs <- .pptx_plot(prs, pl)
    }

    # Table slide (only if a summary flextable exists)
    if (!is.null(sec$summary_ft)) {
      prs <- officer::add_slide(prs, layout = "Blank", master = "Office Theme")
      prs <- .pptx_title(prs, sec$title)
      prs <- .pptx_table(prs, sec$summary_ft)
    }
  }

  print(prs, target = output_path)
  invisible(output_path)
}


# ---------------------------------------------------------------------------
# DOCX assembler
# ---------------------------------------------------------------------------
# The Word report is a real document, not a stack of captioned pages: a title
# page, a field-driven table of contents, Word heading styles so that TOC
# populates itself, a running header describing the document and a page number
# in the bottom-right footer. Everything is built on the bundled template in
# inst/templates/, so the heading / Title / Subtitle styles come from one place.

# Page geometry. Portrait is US Letter with 1" margins, giving 6.5" of text
# width - exactly the width the section plots are rendered at. The dataset
# summary is the one landscape page, with tighter margins because that table is
# the widest thing in the report.
.DOCX_PORTRAIT_WIDTH   <- 6.5
.DOCX_LANDSCAPE_MARGIN <- 0.6
.DOCX_LANDSCAPE_WIDTH  <- 11 - 2 * .DOCX_LANDSCAPE_MARGIN

# Heading 1 as the bundled template defines it (20pt, accent1 shade). Used for
# the one heading that must look like a heading without being one - the title
# of the contents page, which would otherwise list itself.
.DOCX_H1_COLOR <- "#0F4761"

# Font size for the per-variable summary tables. Deliberately large: these are
# read from a projector, so the tables are laid out to fit 18pt.
.DOCX_SECTION_FONT <- 18


#' Running header block for the body sections of the Word report
#' @noRd
.docx_header_block <- function(meta) {
  officer::block_list(
    officer::fpar(
      officer::ftext(
        paste0(meta$title, " \u00b7 ", meta$subtitle),
        officer::fp_text(font.size = 9, color = "#595959", font.family = "Arial")
      ),
      fp_p = officer::fp_par(
        text.align     = "left",
        padding.bottom = 3,
        border.bottom  = officer::fp_border(color = "#BFBFBF", width = 0.75)
      )
    )
  )
}

#' Running footer block: page number, bottom right
#'
#' PAGE only, no "of N": Word's NUMPAGES field counts pages within the current
#' section in a document with several sections, so "Page 4 of 5" appeared on
#' page 4 of 19.
#' @noRd
.docx_footer_block <- function() {
  small <- officer::fp_text(font.size = 9, color = "#595959", font.family = "Arial")
  officer::block_list(
    officer::fpar(
      officer::ftext("Page ", small),
      officer::run_word_field("PAGE", prop = small),
      fp_p = officer::fp_par(text.align = "right", padding.top = 3)
    )
  )
}

#' Section properties for one run of Word pages
#'
#' @param orient "portrait" or "landscape".
#' @param meta Document metadata list; when NULL the section gets no running
#'   header or footer. Only the title page uses that, and only because it is
#'   the first section - a later section with no header reference would inherit
#'   the previous one instead.
#' @noRd
.docx_section_props <- function(orient = "portrait", meta = NULL) {
  margin <- if (identical(orient, "landscape")) .DOCX_LANDSCAPE_MARGIN else 1
  officer::prop_section(
    page_size    = officer::page_size(width = 8.5, height = 11, orient = orient),
    page_margins = officer::page_mar(
      top = margin, bottom = margin, left = margin, right = margin,
      header = 0.5, footer = 0.5, gutter = 0
    ),
    type           = "nextPage",
    header_default = if (is.null(meta)) NULL else .docx_header_block(meta),
    footer_default = if (is.null(meta)) NULL else .docx_footer_block()
  )
}

# Close the current section, so everything added since the previous break sits
# on pages with that orientation and furniture.
.docx_end_section <- function(doc, orient = "portrait", meta = NULL) {
  officer::body_end_block_section(
    doc,
    officer::block_section(.docx_section_props(orient, meta))
  )
}


#' Fit a flextable to the text width of its page
#'
#' The column widths the styling helpers set were chosen for a 13.3" slide, so
#' they overflow both the portrait and the landscape page. Word's own autofit
#' is not the answer - handed a table wider than the page it shrinks every
#' column towards equal and wraps "numeric" to "numeri / c". Instead the widths
#' are computed here (from cell content when \code{autofit = TRUE}, otherwise
#' the ones already on the table) and scaled down - never up - to the page,
#' with a fixed layout so Word honours them.
#'
#' This is also what makes the 18pt section tables work: \code{autofit()}
#' measures at the size the text will actually print at, so each column is
#' given the room that font needs.
#'
#' @param avail_width Text width of the page, in inches.
#' @noRd
.docx_fit_ft <- function(ft, avail_width = .DOCX_PORTRAIT_WIDTH,
                         body_size = NULL, header_size = NULL,
                         padding_x = NULL, autofit = TRUE) {
  if (is.null(ft)) return(NULL)
  if (!is.null(body_size))   ft <- flextable::fontsize(ft, size = body_size,   part = "body")
  if (!is.null(header_size)) ft <- flextable::fontsize(ft, size = header_size, part = "header")
  ft <- flextable::padding(ft, padding.top = 2, padding.bottom = 2, part = "all")
  if (!is.null(padding_x))
    ft <- flextable::padding(ft, padding.left = padding_x,
                             padding.right = padding_x, part = "all")

  if (isTRUE(autofit)) ft <- tryCatch(flextable::autofit(ft), error = function(e) ft)
  widths <- dim(ft)$widths
  if (length(widths) && all(is.finite(widths)) && sum(widths) > avail_width) {
    # Measured widths are capped (the slack is in the widest columns);
    # hand-tuned widths are scaled, because their proportions are the design.
    widths <- if (isTRUE(autofit))
      .docx_shrink_widths(widths, avail_width)
    else
      widths * (avail_width / sum(widths))
    ft <- flextable::width(ft, width = widths)
  }

  flextable::set_table_properties(ft, layout = "fixed", align = "center")
}


#' Shrink a set of column widths onto a page
#'
#' Takes the overflow out of the widest columns first rather than scaling every
#' column by the same factor. A label column ("transplant_center") measures
#' much wider than the data columns beside it, and scaling proportionally
#' starved those data columns until "13 (18.6%)" broke over three lines while
#' the label column still had slack. Capping every column at one ceiling - the
#' largest ceiling whose total still fits - only touches the columns that can
#' afford it.
#'
#' @param widths Numeric column widths in inches.
#' @param avail Page text width in inches.
#' @noRd
.docx_shrink_widths <- function(widths, avail) {
  if (sum(widths) <= avail) return(widths)
  lo <- 0
  hi <- max(widths)
  for (i in seq_len(60)) {
    mid <- (lo + hi) / 2
    if (sum(pmin(widths, mid)) > avail) hi <- mid else lo <- mid
  }
  pmin(widths, lo)
}


# A non-heading label above a table or a continuation figure: it identifies the
# page without adding a second TOC entry for the same section.
.docx_add_caption <- function(doc, text) {
  officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(text, officer::fp_text(font.size = 11, bold = TRUE,
                                            font.family = "Arial")),
      fp_p = officer::fp_par(padding.bottom = 6, keep_with_next = TRUE)
    )
  )
}


#' Title-page metadata for the Word report
#'
#' Pure: derived entirely from the arguments, so \code{edark_report()} and the
#' Shiny download handler produce the same front matter.
#' @noRd
.docx_meta <- function(dataset, report_type = "all_vars", n_sections = 0,
                       primary_variable = NULL, primary_role = "exposure",
                       stratify_variable = NULL) {
  strat <- if (!is.null(stratify_variable) && nzchar(stratify_variable))
    stratify_variable else NULL
  plural <- function(n, one, many) paste0(n, " ", if (n == 1) one else many)

  subtitle <- switch(report_type,
    all_vars          = paste("Descriptive summary of",
                              plural(n_sections, "variable", "variables")),
    primary_vs_others = paste0(primary_variable, " (", primary_role, ") versus ",
                               plural(n_sections, "variable", "variables")),
    custom            = paste("Curated selection of",
                              plural(n_sections, "figure", "figures")),
    "Exploratory data analysis"
  )

  group_heading <- switch(report_type,
    all_vars          = "Variable Summaries",
    primary_vs_others = paste("Relationships with", primary_variable),
    custom            = "Selected Figures",
    "Sections"
  )

  facts <- c(
    Dataset  = paste0(format(nrow(dataset), big.mark = ","), " rows \u00d7 ",
                      ncol(dataset), " columns"),
    Sections = as.character(n_sections)
  )
  if (!is.null(strat)) facts[["Stratified by"]] <- strat
  facts[["Generated"]] <- format(Sys.Date(), "%d %B %Y")

  list(
    title         = "Exploratory Data Analysis Report",
    subtitle      = subtitle,
    group_heading = group_heading,
    facts         = facts
  )
}


#' Add the title page
#'
#' No heading style is used, so none of this reaches the table of contents.
#' @noRd
.docx_add_title_page <- function(doc, meta) {
  for (i in 1:3) doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- officer::body_add_par(doc, meta$title,    style = "Title")
  doc <- officer::body_add_par(doc, meta$subtitle, style = "Subtitle")
  doc <- officer::body_add_par(doc, "", style = "Normal")

  label_fmt <- officer::fp_text(font.size = 11, bold = TRUE, font.family = "Arial")
  value_fmt <- officer::fp_text(font.size = 11, font.family = "Arial")
  for (nm in names(meta$facts)) {
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(paste0(nm, ": "), label_fmt),
        officer::ftext(meta$facts[[nm]], value_fmt),
        fp_p = officer::fp_par(padding.bottom = 2)
      )
    )
  }
  doc
}


#' Add the table-of-contents page
#'
#' \code{body_add_toc()} writes a Word TOC field, so Word generates the entries
#' from the heading styles rather than us baking them in. Word offers to update
#' fields when the document opens; Ctrl+A then F9 refreshes it by hand.
#'
#' The page title is hand-formatted to match heading 1 rather than being one:
#' a real heading here puts "Table of Contents" in its own table of contents.
#' @noRd
.docx_add_toc_page <- function(doc) {
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext("Table of Contents",
                     officer::fp_text(font.size = 20, color = .DOCX_H1_COLOR)),
      fp_p = officer::fp_par(padding.bottom = 4, keep_with_next = TRUE)
    )
  )
  officer::body_add_toc(doc, level = 2)
}


.assemble_docx <- function(sections, dataset_summary_df, output_path,
                             progress_fn = NULL,
                             include_dataset_summary = TRUE,
                             tableone_ft = NULL,
                             collinearity = NULL,
                             meta = NULL) {
  if (is.null(meta))
    meta <- list(
      title         = "Exploratory Data Analysis Report",
      subtitle      = "Exploratory data analysis",
      group_heading = "Sections",
      facts         = c(Sections  = as.character(length(sections)),
                        Generated = format(Sys.Date(), "%d %B %Y"))
    )

  doc <- officer::read_docx(
    system.file("templates/word_docx_blank_template.docx", package = "edark")
  )
  # The template ships one empty paragraph; drop it so the title block starts
  # where the styles expect it to.
  doc <- tryCatch(
    officer::body_remove(officer::cursor_begin(doc)),
    error = function(e) doc
  )

  # -- Title page. Its own section, and the first one, so it inherits no
  #    running header or footer and shows no page number.
  doc <- .docx_add_title_page(doc, meta)
  doc <- .docx_end_section(doc, "portrait", meta = NULL)

  # -- Front matter: contents, then Table 1 and collinearity when requested.
  #    Portrait.
  doc <- .docx_add_toc_page(doc)
  if (!is.null(tableone_ft)) {
    doc <- officer::body_add_break(doc)
    doc <- officer::body_add_par(doc, "Table 1", style = "heading 1")
    doc <- flextable::body_add_flextable(doc, .docx_fit_ft(tableone_ft))
  }
  if (!is.null(collinearity)) {
    doc <- officer::body_add_break(doc)
    doc <- officer::body_add_par(doc, "Collinearity", style = "heading 1")
    plots <- Filter(Negate(is.null),
                    list(collinearity$cor_plot, collinearity$cramers_plot))
    for (k in seq_along(plots)) {
      if (k > 1) doc <- officer::body_add_break(doc)
      doc <- officer::body_add_gg(doc, value = plots[[k]], width = .DOCX_PORTRAIT_WIDTH,
                                  height = .DOCX_PORTRAIT_WIDTH, res = 150)
    }
    doc <- officer::body_add_break(doc)
    doc <- .docx_add_caption(doc, "Pairs above 0.7")
    doc <- flextable::body_add_flextable(doc, .docx_fit_ft(collinearity$flagged_ft))
  }

  # -- Dataset summary: the one landscape page, so it needs a section break on
  #    both sides of it.
  if (isTRUE(include_dataset_summary)) {
    doc <- .docx_end_section(doc, "portrait", meta)
    doc <- officer::body_add_par(doc, "Dataset Summary", style = "heading 1")
    # The hand-tuned column widths in .style_dataset_summary_ft() are kept
    # (autofit = FALSE) and only scaled to the landscape page: they encode
    # which of the fifteen columns deserve the room, which measuring the
    # content does not.
    # The hand-tuned widths in .style_dataset_summary_ft() are kept
    # (autofit = FALSE) and only scaled: they encode which of the fifteen
    # columns deserve the room, and measuring the content instead gives
    # "Top Values" and the variable names too little and wraps them onto a
    # second page. What the scaling costs is recovered from the cell padding
    # and a header at body size - fifteen columns at 8pt bold wrap "Skewness",
    # and a two-line header row reads worse than a small one.
    ds_ft <- .docx_fit_ft(.style_dataset_summary_ft(dataset_summary_df),
                          avail_width = .DOCX_LANDSCAPE_WIDTH,
                          header_size = 7,
                          padding_x   = 2,
                          autofit     = FALSE)
    doc <- flextable::body_add_flextable(doc, ds_ft)
    doc <- .docx_end_section(doc, "landscape", meta)
  } else {
    doc <- .docx_end_section(doc, "portrait", meta)
  }

  # -- Per-section content: one heading 2 per section (its TOC entry), its plot
  #    page(s), then its summary table page.
  doc <- officer::body_add_par(doc, meta$group_heading, style = "heading 1")

  n <- length(sections)
  for (i in seq_along(sections)) {
    if (!is.null(progress_fn)) progress_fn(i / n, paste0("Writing document: section ", i, " of ", n))
    sec <- sections[[i]]
    if (i > 1) doc <- officer::body_add_break(doc)

    doc <- officer::body_add_par(doc, sec$title, style = "heading 2")

    # Plot page(s) - split_panels may give a list of two ggplots
    plots <- if (is.list(sec$plot_obj) && !inherits(sec$plot_obj, "ggplot"))
      sec$plot_obj else list(sec$plot_obj)
    for (k in seq_along(plots)) {
      pl <- plots[[k]]
      if (k > 1) {
        doc <- officer::body_add_break(doc)
        doc <- .docx_add_caption(doc, paste0(sec$title, " (continued)"))
      }
      if (inherits(pl, "patchwork")) {
        tmp_png <- tempfile(fileext = ".png")
        ggplot2::ggsave(tmp_png, plot = pl, width = .DOCX_PORTRAIT_WIDTH,
                        height = 5, units = "in", dpi = 150)
        doc <- officer::body_add_img(doc, src = tmp_png,
                                     width = .DOCX_PORTRAIT_WIDTH, height = 5)
        unlink(tmp_png)
      } else {
        doc <- officer::body_add_gg(doc, value = pl, width = .DOCX_PORTRAIT_WIDTH,
                                    height = 5, res = 150)
      }
    }

    # Table page. 18pt body text, fitted to the full text width so the larger
    # font has somewhere to go.
    if (!is.null(sec$summary_ft)) {
      doc <- officer::body_add_break(doc)
      doc <- .docx_add_caption(doc, sec$title)
      doc <- flextable::body_add_flextable(
        doc,
        .docx_fit_ft(sec$summary_ft,
                     body_size   = .DOCX_SECTION_FONT,
                     header_size = .DOCX_SECTION_FONT)
      )
    }
  }

  # The trailing section carries the page setup for everything after the last
  # explicit break.
  doc <- officer::body_set_default_section(doc, .docx_section_props("portrait", meta))

  print(doc, target = output_path)
  invisible(output_path)
}


.assemble_html <- function(sections, dataset_summary_df, output_path,
                             report_type = "all_vars",
                             linked_var_anchors = NULL,
                             include_dataset_summary = TRUE,
                             tableone_ft = NULL,
                             collinearity = NULL) {
  template_path <- system.file("report_template.Rmd", package = "edark")

  rmarkdown::render(
    input             = template_path,
    output_file       = normalizePath(output_path, mustWork = FALSE),
    output_dir        = dirname(normalizePath(output_path, mustWork = FALSE)),
    intermediates_dir = tempdir(),
    params            = list(
      sections                = sections,
      dataset_summary_df      = dataset_summary_df,
      report_type             = report_type,
      linked_var_anchors      = linked_var_anchors,
      include_dataset_summary = include_dataset_summary,
      tableone_ft             = tableone_ft,
      collinearity            = collinearity
    ),
    quiet             = TRUE,
    envir             = new.env(parent = globalenv())
  )

  invisible(output_path)
}


# ---------------------------------------------------------------------------
# Main function
# ---------------------------------------------------------------------------

#' Generate an EDARK report
#'
#' Builds a report from a dataset and a list of plot sections. Called by
#' \code{module_report.R}'s \code{downloadHandler} and by \code{edark_report()}.
#'
#' @param dataset A \code{data.frame} — the working dataset.
#' @param column_types Named character vector of column types (from
#'   \code{detect_column_types()}).
#' @param report_type \code{"all_vars"} or \code{"primary_vs_others"}.
#' @param variables Character vector of variable names to include in the report.
#' @param primary_variable For \code{"primary_vs_others"}: the primary variable name.
#' @param primary_role \code{"exposure"} or \code{"outcome"}.
#' @param stratify_variable Optional column name to stratify all bivariate plots by.
#' @param format Output format: \code{"pptx"}, \code{"docx"}, or \code{"html"}.
#' @param output_path Absolute path to write the report to.
#' @param progress_fn Optional \code{function(fraction, detail)} called at each
#'   section milestone. Intended for use with \code{shiny::setProgress()}.
#' @param include_dataset_summary Logical. Open with a summary table of the
#'   report's variables. Default \code{TRUE}.
#' @param include_tableone Logical. Add a Table One of the report's variables.
#'   Default \code{FALSE}.
#' @param include_collinearity Logical. Add a collinearity investigation after
#'   Table One: Pearson and Cramer's V heatmaps plus pairs above 0.7, over the
#'   same variables as Table One. Default \code{FALSE}.
#' @param ggplot_theme,color_palette,show_data_labels,show_legend,legend_position
#'   Plot appearance applied to every plot in the report - the same settings as
#'   the Explore > Appearance panel.
#'
#' @return Invisibly returns \code{output_path}.
#'
#' @export
generate_report <- function(dataset,
                             column_types,
                             report_type,
                             variables,
                             primary_variable        = NULL,
                             primary_role            = "exposure",
                             stratify_variable       = NULL,
                             format,
                             output_path,
                             progress_fn             = NULL,
                             include_dataset_summary = TRUE,
                             include_tableone        = FALSE,
                             include_collinearity    = FALSE,
                             ggplot_theme            = "minimal",
                             color_palette           = "Set2",
                             show_data_labels        = FALSE,
                             show_legend             = TRUE,
                             legend_position         = "top") {
  stopifnot(report_type %in% c("all_vars", "primary_vs_others"))
  stopifnot(format %in% c("pptx", "docx", "html"))
  stopifnot(is.data.frame(dataset), length(variables) >= 1)

  strat <- if (!is.null(stratify_variable) && nzchar(stratify_variable))
    stratify_variable else NULL

  # Both tables describe the report, not the dataset. Table 1 carries the
  # variables that become sections, plus the primary variable in correlation
  # mode since it appears in every section; the dataset summary adds the
  # stratify variable on top of those.
  content_vars <- variables
  if (report_type == "primary_vs_others" && !is.null(primary_variable))
    content_vars <- union(primary_variable, content_vars)
  tableone_vars <- setdiff(content_vars, strat)
  summary_vars  <- unique(c(content_vars, strat))

  # Build dataset-level summary once (numeric + factor only)
  if (!is.null(progress_fn)) progress_fn(0, "Building dataset summary...")
  dataset_summary_df <- .build_dataset_summary(dataset, column_types, summary_vars)

  # Build Table One if requested
  tableone_ft <- if (isTRUE(include_tableone)) {
    to_vars <- tableone_vars[
      tableone_vars %in% names(column_types) &
        column_types[tableone_vars] %in% c("numeric", "factor")
    ]
    to_df <- tryCatch(
      .build_tableone_df(dataset, column_types, to_vars, strat),
      error = function(e) NULL
    )
    if (!is.null(to_df) && nrow(to_df) > 0)
      .style_tableone_ft(to_df, has_strat = !is.null(strat))
    else NULL
  } else NULL

  # Collinearity over the same variables as Table One: the stratify variable
  # is a column header there and not a candidate here either.
  collinearity <- if (isTRUE(include_collinearity)) {
    if (!is.null(progress_fn)) progress_fn(0, "Computing collinearity...")
    .build_collinearity_report(dataset, column_types, tableone_vars)
  } else NULL

  plot_aesthetics <- list(
    ggplot_theme     = ggplot_theme,
    color_palette    = color_palette,
    show_data_labels = show_data_labels,
    show_legend      = show_legend,
    legend_position  = legend_position
  )

  build_end      <- .progress_split(format)
  build_progress <- .progress_span(progress_fn, 0, build_end)
  write_progress <- .progress_span(progress_fn, build_end, 1)

  sections <- if (report_type == "all_vars") {
    .build_all_vars_sections(dataset, column_types, variables, stratify_variable,
                              progress_fn = build_progress, plot_aesthetics = plot_aesthetics)
  } else {
    if (is.null(primary_variable))
      stop("primary_variable must be specified for report_type = 'primary_vs_others'")
    secondary_vars <- setdiff(variables, primary_variable)
    if (length(secondary_vars) == 0)
      stop("No secondary variables to plot - ensure variables contains columns besides primary_variable.")
    .build_primary_vs_others_sections(
      dataset, column_types, secondary_vars,
      primary_variable, primary_role, stratify_variable,
      progress_fn = build_progress, plot_aesthetics = plot_aesthetics
    )
  }

  if (length(sections) == 0)
    stop("No sections could be built - check that selected variables exist in the dataset.")

  # Build linked_var_anchors for HTML: named vector mapping variable name → anchor ID.
  # For all_vars: every section variable links to its section.
  # For primary_vs_others: only secondary variables link to their sections.
  linked_var_anchors <- if (report_type == "all_vars") {
    setNames(
      sapply(sections, function(s) s$anchor),
      sapply(sections, function(s) s$title)
    )
  } else {
    sec_var_names <- setdiff(variables, primary_variable)
    sec_var_names <- sec_var_names[
      sec_var_names %in% names(column_types) &
      column_types[sec_var_names] %in% c("numeric", "factor")
    ]
    if (!is.null(stratify_variable) && nzchar(stratify_variable))
      sec_var_names <- sec_var_names[sec_var_names != stratify_variable]
    setNames(
      sapply(sections, function(s) s$anchor),
      sec_var_names[seq_along(sections)]
    )
  }

  if (!is.null(progress_fn)) progress_fn(build_end, "Assembling output...")

  switch(format,
    pptx = .assemble_pptx(sections, dataset_summary_df, output_path, write_progress,
                          include_dataset_summary = include_dataset_summary,
                          tableone_ft             = tableone_ft,
                          collinearity            = collinearity),
    docx = .assemble_docx(sections, dataset_summary_df, output_path, write_progress,
                          include_dataset_summary = include_dataset_summary,
                          tableone_ft             = tableone_ft,
                          collinearity            = collinearity,
                          meta                    = .docx_meta(
                            dataset           = dataset,
                            report_type       = report_type,
                            n_sections        = length(sections),
                            primary_variable  = primary_variable,
                            primary_role      = primary_role,
                            stratify_variable = stratify_variable
                          )),
    html = .assemble_html(sections, dataset_summary_df, output_path,
                          report_type             = report_type,
                          linked_var_anchors      = linked_var_anchors,
                          include_dataset_summary = include_dataset_summary,
                          tableone_ft             = tableone_ft,
                          collinearity            = collinearity)
  )

  invisible(output_path)
}


# ── Custom report helpers ─────────────────────────────────────────────────────

# Build a warning placeholder plot for items whose columns are no longer valid.
.custom_item_error_plot <- function(title, msg) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5,
                      label = paste0("Could not render: ", title, "\n", msg),
                      hjust = 0.5, vjust = 0.5, colour = "grey40", size = 4) +
    ggplot2::theme_void()
}

# Map each custom report item's plot spec to a section object, re-rendering
# using the current working dataset. Returns a list of section objects
# {title, anchor, plot_obj, summary_ft} — the same format used by the full report
# assemblers. Items that fail to render produce a warning placeholder rather
# than aborting the entire report.
.build_custom_report_sections <- function(items, dataset, column_types,
                                           progress_fn = NULL, plot_aesthetics = list()) {
  n        <- length(items)
  sections <- vector("list", n)

  trend_types <- c("trend_count", "trend_numeric", "trend_proportion", "trend_factor")

  for (i in seq_along(items)) {
    item  <- items[[i]]
    spec  <- item$plot_spec
    title <- item$title

    if (!is.null(progress_fn))
      progress_fn(i / n, paste0("Building plots: item ", i, " of ", n, ": ", title))

    # Apply report-level aesthetics over the frozen per-item spec
    if (length(plot_aesthetics) > 0) spec <- modifyList(spec, plot_aesthetics)

    # Re-render plot from spec + current dataset; trap errors per-item
    plot_obj <- tryCatch(
      render_plot(spec, dataset, split_panels = TRUE),
      error = function(e) .custom_item_error_plot(title, conditionMessage(e))
    )

    col_a      <- spec$column_a
    col_b      <- spec$column_b
    strat      <- if (!is.null(spec$stratify_by) && nzchar(spec$stratify_by))
                    spec$stratify_by else NULL
    col_a_type <- if (!is.null(col_a) && col_a %in% names(column_types))
                    column_types[[col_a]] else NULL
    col_b_type <- if (!is.null(col_b) && col_b %in% names(column_types))
                    column_types[[col_b]] else NULL

    is_trend <- !is.null(spec$plot_type) && spec$plot_type %in% trend_types

    summary_ft <- if (is_trend) {
      NULL  # trend plots: plot only, no table (consistent with existing report behaviour)
    } else if (!is.null(col_b) && !is.null(col_a_type) && !is.null(col_b_type)) {
      summary_df <- tryCatch({
        if (col_a_type == "numeric" && col_b_type == "numeric") {
          .build_bivariate_num_num_table(dataset, col_a, col_b, strat)
        } else if (col_a_type == "factor" && col_b_type == "numeric") {
          # spec normalises violin_jitter so factor is always col_a
          .build_bivariate_num_fac_table(dataset, col_b, col_a, strat)
        } else if (col_a_type == "factor" && col_b_type == "factor") {
          .build_bivariate_fac_fac_table(dataset, col_a, col_b, strat)
        } else NULL
      }, error = function(e) NULL)
      if (!is.null(summary_df)) .style_section_ft(summary_df) else NULL
    } else if (!is.null(col_a) && !is.null(col_a_type)) {
      summary_df <- tryCatch({
        if (col_a_type == "numeric") {
          .build_univariate_numeric_table(dataset, col_a, strat)
        } else if (col_a_type == "factor") {
          .build_univariate_factor_table(dataset, col_a, strat)
        } else NULL
      }, error = function(e) NULL)
      if (!is.null(summary_df)) .style_section_ft(summary_df) else NULL
    } else {
      NULL
    }

    sections[[i]] <- list(
      title      = title,
      anchor     = .make_html_anchor(paste0("custom-", i, "-", item$id)),
      plot_obj   = plot_obj,
      summary_ft = summary_ft
    )
  }

  sections
}


#' Generate a custom report from user-curated plot items
#'
#' Shiny-free entry point for custom report generation. Takes a list of items
#' collected via the EDARK Explore tab and assembles them into a report.
#'
#' @param items List of custom report item objects as stored in
#'   `shared_state$custom_report_items`. Each must have `id`, `plot_spec`,
#'   `thumb_path`, and `title` fields.
#' @param dataset The working data frame to use for re-rendering plots and
#'   building summary tables.
#' @param column_types Named character vector mapping column names to types
#'   (`"numeric"`, `"factor"`, `"datetime"`, `"character"`).
#' @param format Output format: `"pptx"`, `"docx"`, or `"html"`.
#' @param output_path File path for the generated report.
#' @param progress_fn Optional callback `function(fraction, detail)` for
#'   progress reporting (e.g. Shiny's `setProgress`).
#' @param ggplot_theme,color_palette,show_data_labels,show_legend,legend_position
#'   Optional appearance overrides applied to *every* item, for programmatic
#'   callers who want one consistent look. Leave them NULL - as the app does -
#'   to keep the appearance each item was captured with.
#'
#' @return `output_path`, invisibly.
#' @export
generate_custom_report <- function(items, dataset, column_types, format,
                                    output_path, progress_fn = NULL,
                                    ggplot_theme     = NULL,
                                    color_palette    = NULL,
                                    show_data_labels = NULL,
                                    show_legend      = NULL,
                                    legend_position  = NULL) {
  stopifnot(is.list(items), length(items) >= 1)
  stopifnot(format %in% c("pptx", "docx", "html"))
  stopifnot(is.data.frame(dataset))

  if (!is.null(progress_fn)) progress_fn(0, "Building dataset summary...")
  dataset_summary_df <- .build_dataset_summary(dataset, column_types)

  # Each item already carries the appearance it had when it was added, so an
  # aesthetic argument is an override for programmatic callers only. The app
  # passes none: a report must reproduce what the user saw, not restyle every
  # item to one look (D7 in PRD/BUILD_UI-redesign.md). Dropping the NULLs is
  # what makes "not supplied" mean "leave each item alone" - the defaults used
  # to be concrete values, so every item was silently restyled.
  plot_aesthetics <- Filter(Negate(is.null), list(
    ggplot_theme     = ggplot_theme,
    color_palette    = color_palette,
    show_data_labels = show_data_labels,
    show_legend      = show_legend,
    legend_position  = legend_position
  ))

  build_end      <- .progress_split(format)
  write_progress <- .progress_span(progress_fn, build_end, 1)
  sections <- .build_custom_report_sections(items, dataset, column_types,
                                             .progress_span(progress_fn, 0, build_end),
                                             plot_aesthetics = plot_aesthetics)

  if (length(sections) == 0)
    stop("No sections could be built from the custom report items.")

  if (!is.null(progress_fn)) progress_fn(build_end, "Assembling output...")

  switch(format,
    pptx = .assemble_pptx(sections, dataset_summary_df, output_path, write_progress),
    docx = .assemble_docx(sections, dataset_summary_df, output_path, write_progress,
                          meta = .docx_meta(dataset     = dataset,
                                            report_type = "custom",
                                            n_sections  = length(sections))),
    html = .assemble_html(sections, dataset_summary_df, output_path,
                          report_type = "custom", linked_var_anchors = NULL)
  )

  invisible(output_path)
}
