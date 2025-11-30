#' seasight: Tools to *see* seasonal adjustment more clearly
#'
#' `seasight` provides helper functions to run, compare and document
#' seasonal adjustment in a transparent and reproducible way. It builds
#' on the [seasonal](https://CRAN.R-project.org/package=seasonal) package,
#' the R interface to X-13ARIMA-SEATS.
#'
#' The main user-facing functions include:
#'
#' - [auto_seasonal_analysis()] for automatic model search & comparison
#' - [sa_report_html()] to generate HTML diagnostics reports
#' - [sa_compare()] and [sa_top_candidates_table()] for compact summaries
#'
#' @importFrom dplyr %>%
#' @importFrom stats arima setNames
#' @importFrom utils capture.output
#' @keywords internal
"_PACKAGE"
