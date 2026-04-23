test_that("plot.auto_seasonal_analysis handles do-not-adjust results", {
  res <- structure(
    list(
      best = NULL,
      y = fixture_monthly_ts(),
      table = tibble::tibble(),
      seasonality = list()
    ),
    class = "auto_seasonal_analysis"
  )

  pdf <- tempfile(fileext = ".pdf")
  grDevices::pdf(pdf)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_invisible(plot(res))
  expect_error(plot(res, series = "seasonal"), "not available")
})
