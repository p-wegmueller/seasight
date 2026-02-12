test_that("build_user_xreg pads moving-holiday regressors outside date span", {
  y <- ts(rnorm(120), start = c(2015, 1), frequency = 12)
  diwali <- as.Date(c("2018-11-07","2019-10-27","2020-11-14","2021-11-04","2022-10-24"))
  
  td <- build_user_xreg(y, holidays = list(list(name="diwali", dates=diwali, start=-7, end=0)))
  
  expect_true(is.list(td))
  expect_true("diwali" %in% names(td))
  expect_s3_class(td$diwali, "ts")
  expect_equal(length(td$diwali), length(y))
  expect_equal(stats::frequency(td$diwali), stats::frequency(y))
  expect_equal(stats::start(td$diwali), stats::start(y))
  expect_equal(stats::end(td$diwali), stats::end(y))
})
