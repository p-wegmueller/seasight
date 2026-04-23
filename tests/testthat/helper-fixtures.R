#' srr_stats_tests
#'
#' @srrstats {G5.0, G5.1, G5.2, G5.2a, G5.2b, G5.3, G5.5}
#'   Tests use base reference series and deterministic shared fixtures, and
#'   targeted tests cover expected errors, warnings, return structures, and
#'   generated files.

fixture_monthly_ts <- function(n = 24L, start = c(2020, 1), values = seq_len(n)) {
  stats::ts(values, start = start, frequency = 12)
}

fixture_quarterly_ts <- function(n = 40L, start = c(2015, 1), values = seq_len(n)) {
  stats::ts(values, start = start, frequency = 4)
}

fixture_monthly_regressor <- function(y, offset = 0) {
  stats::ts(
    seq_along(y) + offset,
    start = stats::start(y),
    frequency = stats::frequency(y)
  )
}

fixture_tempdir <- function(prefix) {
  file.path(
    tempdir(),
    paste0(prefix, "-", Sys.getpid(), "-", sample.int(.Machine$integer.max, 1L))
  )
}
