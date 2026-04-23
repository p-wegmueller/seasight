#' srr_stats_plotting
#'
#' @srrstats {TS5.0, TS5.1, TS5.2, TS5.3}
#'   The `auto_seasonal_analysis` class provides a default temporal plot with
#'   time on the horizontal axis and explicit axis labels.
#' @noRd
NULL

#' Plot an automatic seasonal-analysis result
#'
#' Provides a compact default plot for the main result class returned by
#' [auto_seasonal_analysis()]. The plot is intended for quick inspection; the
#' HTML report remains the richer review artifact.
#'
#' @param x An object returned by [auto_seasonal_analysis()].
#' @param series Which series to show. `"final"` plots the selected seasonally
#'   adjusted series when available and falls back to the original series for
#'   do-not-adjust cases. `"seasonal"` plots the selected seasonal component
#'   when available. `"original"` plots the original input series stored in the
#'   result.
#' @param xlab,ylab Axis labels.
#' @param ... Additional arguments passed to [stats::plot.ts()].
#'
#' @return Invisibly, `x`.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("seasonal", quietly = TRUE)) {
#'   res <- auto_seasonal_analysis(AirPassengers, max_specs = 3)
#'   plot(res)
#' }
#' }
#' @export
plot.auto_seasonal_analysis <- function(x,
                                        series = c("final", "seasonal", "original"),
                                        xlab = "time",
                                        ylab = NULL,
                                        ...) {
  series <- match.arg(series)
  if (!inherits(x, "auto_seasonal_analysis")) {
    stop("`x` must be an `auto_seasonal_analysis` object.", call. = FALSE)
  }

  y <- switch(
    series,
    original = x$y,
    final = if (is.null(x$best)) x$y else tryCatch(seasonal::final(x$best), error = function(e) x$y),
    seasonal = if (is.null(x$best)) {
      NULL
    } else {
      tryCatch(seasonal::series(x$best, "seats.seasonal"),
               error = function(e) tryCatch(seasonal::series(x$best, "x11.seasonal"),
                                            error = function(e) NULL))
    }
  )

  if (is.null(y)) {
    stop("The requested `series` is not available for this result.", call. = FALSE)
  }
  if (is.null(ylab)) {
    unit_label <- attr(y, "units", exact = TRUE)
    ylab <- if (!is.null(unit_label) && nzchar(unit_label)) {
      paste0(series, " (", unit_label, ")")
    } else {
      series
    }
  }

  stats::plot.ts(y, xlab = xlab, ylab = ylab, ...)
  invisible(x)
}
