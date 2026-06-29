test_that(".align2 preserves time-series metadata after alignment", {
  prev <- stats::ts(1:12, start = c(2020, 1), frequency = 12)
  new <- stats::ts(3:14, start = c(2020, 3), frequency = 12)
  
  aligned <- seasight:::.align2(prev, new)
  
  expect_true(isTRUE(aligned$ok))
  expect_s3_class(aligned$prev, "ts")
  expect_s3_class(aligned$new, "ts")
  expect_equal(stats::start(aligned$prev), c(2020, 3))
  expect_equal(stats::start(aligned$new), c(2020, 3))
  expect_equal(stats::frequency(aligned$prev), 12)
  expect_equal(stats::frequency(aligned$new), 12)
})

test_that("issue #49 monthly repro renders HTML with current_model comparison", {
  skip_on_cran()
  skip_if_not_installed("seasonal")
  
  y <- log(AirPassengers)
  current_model <- seasonal::seas(y)
  out <- tempfile(fileext = ".html")
  
  expect_no_error(
    invisible(capture.output(
      sa_report_html(
        y = y,
        current_model = current_model,
        outfile = out,
        max_specs = 3
      )
    ))
  )
  
  expect_true(file.exists(out))
  html <- paste(readLines(out, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(html, "Copy-paste model \\(current\\)")
  expect_match(html, "Comparison")
})

test_that("issue #49 quarterly repro renders HTML with current_model comparison", {
  skip_on_cran()
  skip_if_not_installed("seasonal")
  
  set.seed(1)
  y <- stats::ts(cumsum(stats::rnorm(160, 10, 2)), start = c(1985, 1), frequency = 4)
  current_model <- seasonal::seas(
    y,
    transform.function = "none",
    regression.aictest = NULL,
    outlier = NULL
  )
  out <- tempfile(fileext = ".html")
  
  expect_no_error(
    invisible(capture.output(
      sa_report_html(
        y = y,
        current_model = current_model,
        outfile = out,
        max_specs = 3
      )
    ))
  )
  
  expect_true(file.exists(out))
  html <- paste(readLines(out, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(html, "Copy-paste model \\(current\\)")
  expect_match(html, "Comparison")
})
