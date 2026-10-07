# Colour palettes for every Explore plot and report (§E7).
#
# All discrete colour in render_plot.R comes from .scale_palette(), so a plot
# never asks a palette for more colours than it has. A qualitative palette is
# never stretched - blending distinct colours gives muddy, look-alike ones -
# it hands over to the smallest palette that fits instead. In the app that
# hand-over is made permanent for the session and announced (§E7.1).


# Every palette the Appearance picker offers, in picker order. `max` is how
# many distinct colours a qualitative palette has; NA marks a sequential
# palette, which is interpolated to any number of levels.
.EDARK_PALETTES <- list(
  Set2         = list(label = "Set 2",      group = "Distinct - up to 10 levels", max = 8L),
  Set1         = list(label = "Set 1",      group = "Distinct - up to 10 levels", max = 9L),
  Dark2        = list(label = "Dark 2",     group = "Distinct - up to 10 levels", max = 8L),
  OkabeIto     = list(label = "Okabe-Ito (colour-blind safe)",
                      group = "Distinct - up to 10 levels", max = 8L),
  Tableau10    = list(label = "Tableau 10", group = "Distinct - up to 10 levels", max = 10L),
  Paired       = list(label = "Paired",     group = "Distinct - many levels", max = 12L),
  Set3         = list(label = "Set 3",      group = "Distinct - many levels", max = 12L),
  Tableau20    = list(label = "Tableau 20", group = "Distinct - many levels", max = 20L),
  Kelly        = list(label = "Kelly",      group = "Distinct - many levels", max = 20L),
  Alphabet     = list(label = "Alphabet",   group = "Distinct - many levels", max = 26L),
  Polychrome36 = list(label = "Polychrome", group = "Distinct - many levels", max = 36L),
  Viridis      = list(label = "Viridis",    group = "Shades - any number of levels", max = NA_integer_),
  Blues        = list(label = "Blues",      group = "Shades - any number of levels", max = NA_integer_),
  Greens       = list(label = "Greens",     group = "Shades - any number of levels", max = NA_integer_),
  Reds         = list(label = "Reds",       group = "Shades - any number of levels", max = NA_integer_),
  Purples      = list(label = "Purples",    group = "Shades - any number of levels", max = NA_integer_)
)

# The palettes an over-full one hands over to, smallest first.
.PALETTE_FALLBACKS <- c("Tableau10", "Tableau20", "Polychrome36")


# An unknown id (an old spec, or a typo through the edark_report() API) is
# drawn as the default rather than failing.
.palette_id <- function(palette) {
  if (!is.null(palette) && palette %in% names(.EDARK_PALETTES)) palette else "Set2"
}

.palette_label <- function(palette) .EDARK_PALETTES[[.palette_id(palette)]]$label

# The full colour set of a qualitative palette.
.palette_qualitative <- function(palette) {
  cols <- switch(palette,
    Set2         = grDevices::palette.colors(8,  "Set 2"),
    Set1         = grDevices::palette.colors(9,  "Set 1"),
    Dark2        = grDevices::palette.colors(8,  "Dark 2"),
    # Okabe-Ito's first colour is black, which reads as an outline, not a group
    OkabeIto     = grDevices::palette.colors(9,  "Okabe-Ito")[-1],
    Tableau10    = grDevices::palette.colors(10, "Tableau 10"),
    Paired       = grDevices::palette.colors(12, "Paired"),
    Set3         = grDevices::palette.colors(12, "Set 3"),
    # Tableau's classic 20, dark shades first so a few levels get strong colours
    Tableau20    = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD",
                     "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF",
                     "#AEC7E8", "#FFBB78", "#98DF8A", "#FF9896", "#C5B0D5",
                     "#C49C94", "#F7B6D2", "#C7C7C7", "#DBDB8D", "#9EDAE5"),
    # Kelly's 22 colours of maximum contrast, without white and black
    Kelly        = c("#F3C300", "#875692", "#F38400", "#A1CAF1", "#BE0032",
                     "#C2B280", "#848482", "#008856", "#E68FAC", "#0067A5",
                     "#F99379", "#604E97", "#F6A600", "#B3446C", "#DCD300",
                     "#882D17", "#8DB600", "#654522", "#E25822", "#2B3D26"),
    Alphabet     = grDevices::palette.colors(26, "Alphabet"),
    Polychrome36 = grDevices::palette.colors(36, "Polychrome 36")
  )
  unname(cols)
}

# n shades of a sequential palette. Up to 9 the Brewer set is used as is, so
# small plots look exactly as they did; beyond that it is interpolated.
.palette_sequential <- function(palette, n) {
  if (palette == "Viridis") return(grDevices::hcl.colors(n, "viridis"))
  if (n <= 9) return(scales::pal_brewer(palette = palette)(n))
  grDevices::colorRampPalette(scales::pal_brewer(palette = palette)(9))(n)
}

# Whether a palette can colour n levels with distinct colours of its own.
.palette_fits <- function(palette, n) {
  max <- .EDARK_PALETTES[[.palette_id(palette)]]$max
  is.na(max) || n <= max
}

# The smallest qualitative palette that fits n levels, or NULL past them all.
.palette_for_levels <- function(n) {
  for (id in .PALETTE_FALLBACKS) if (n <= .EDARK_PALETTES[[id]]$max) return(id)
  NULL
}


#' n colours from a palette, never fewer
#'
#' The one place plot colours come from. A qualitative palette with too few
#' colours is replaced by the smallest one that fits, and past every palette
#' by evenly spaced hues, so a plot never shows grey or missing levels.
#'
#' @param palette Character. An id from `.EDARK_PALETTES`.
#' @param n Integer. Number of levels to colour.
#' @return A character vector of n hex colours.
#' @keywords internal
#' @noRd
edark_palette_colours <- function(palette, n) {
  palette <- .palette_id(palette)
  if (is.na(.EDARK_PALETTES[[palette]]$max)) return(.palette_sequential(palette, n))
  if (!.palette_fits(palette, n)) {
    palette <- .palette_for_levels(n)
    if (is.null(palette)) return(grDevices::hcl.colors(n, "Dynamic"))
  }
  .palette_qualitative(palette)[seq_len(n)]
}

# A discrete colour / fill scale drawn from edark_palette_colours(). Replaces
# scale_*_brewer(), which leaves levels past the palette's size grey.
.scale_palette <- function(aesthetics, palette, ...) {
  ggplot2::discrete_scale(aesthetics,
                          palette = function(n) edark_palette_colours(palette, n), ...)
}


# The column a plot type colours by, or NULL if it maps no colour. Keep in
# step with the colour / fill mappings in render_plot.R.
.plot_colour_column <- function(spec) {
  switch(spec$plot_type %||% "",
    bar_count         = ,
    violin_jitter     = spec$column_a,
    bar_grouped       = ,
    trend_factor      = spec$column_b,
    histogram_density = ,
    scatter_loess     = ,
    trend_mean        = ,
    trend_numeric     = spec$stratify_by,
    NULL
  )
}

#' Does this plot need more colours than its palette has?
#'
#' @param spec A plot spec carrying `color_palette`.
#' @param dataset The data the plot is drawn from.
#' @return NULL when the palette fits, otherwise a list: `from` and `to`
#'   (palette ids), `n` (levels) and `column` (the column coloured by).
#' @keywords internal
#' @noRd
.palette_switch_needed <- function(spec, dataset) {
  col <- .plot_colour_column(spec)
  if (is.null(col) || !col %in% names(dataset)) return(NULL)
  n    <- length(unique(stats::na.omit(dataset[[col]])))
  from <- .palette_id(spec$color_palette)
  if (.palette_fits(from, n)) return(NULL)
  to <- .palette_for_levels(n)
  if (is.null(to)) return(NULL)
  list(from = from, to = to, n = n, column = col)
}


# The picker's choices, grouped, each shown with a strip of its colours.
.palette_picker_choices <- function() {
  ids    <- names(.EDARK_PALETTES)
  groups <- vapply(.EDARK_PALETTES, `[[`, "", "group")
  labels <- vapply(.EDARK_PALETTES, `[[`, "", "label")
  choices <- lapply(split(stats::setNames(ids, labels),
                          factor(groups, levels = unique(groups))), as.list)

  # choicesOpt is applied in option order, which is the grouped order above
  ordered <- unlist(choices, use.names = FALSE)
  content <- vapply(ordered, function(id) {
    info  <- .EDARK_PALETTES[[id]]
    shown <- if (is.na(info$max)) 9L else min(info$max, 12L)
    swatch <- paste0(sprintf(
      "<span style=\"display:inline-block;width:10px;height:12px;background:%s\"></span>",
      edark_palette_colours(id, shown)), collapse = "")
    count <- if (is.na(info$max)) "" else
      sprintf(" <span class=\"text-muted\">(%d)</span>", info$max)
    sprintf("<span style=\"margin-right:8px\">%s</span>%s%s", swatch, info$label, count)
  }, "")

  list(choices = choices, content = unname(content))
}
