test_that("sa_report_html can render from a precomputed auto_seasonal_analysis object", {
  skip_on_cran()
  skip_if_not_installed("seasonal")
  
  res <- seasight::auto_seasonal_analysis(AirPassengers)
  out <- tempfile(fileext = ".html")
  
  expect_no_error(
    invisible(capture.output(seasight::sa_report_html(res, outfile = out)))
  )
  
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 0)
})
