#' @keywords internal
"_PACKAGE"

# Base-R functions used unqualified across the package
#' @importFrom stats IQR as.formula ave chisq.test density median na.omit
#'   ppoints qnorm quantile sd setNames
#' @importFrom utils head modifyList str
#' @importFrom rlang .data
NULL

# Column names used inside ggplot2 / dplyr verbs, and the built-in dataset
# (edark()'s default argument)
utils::globalVariables(c("n", "term", "estimate", "variable", "p.value", "liver_tx"))

# Packages in Imports that edark never calls itself but needs at run time:
# gtsummary::add_p() tidies its tests with broom, add_difference(test = "smd")
# needs smd, and inst/report_template.Rmd is knitted with knitr. Referenced
# here so R CMD check sees them used; never called.
.edark_runtime_deps <- function() {
  list(broom::tidy, smd::smd, knitr::knit)
}
