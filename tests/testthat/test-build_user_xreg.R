test_that("build_user_xreg pads moving-holiday regressors outside date span", {
  y <- fixture_monthly_ts(n = 120L, start = c(2015, 1), values = seq_len(120))
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

test_that("build_user_xreg preserves named holiday candidates", {
  y <- fixture_monthly_ts(n = 96L, start = c(2017, 1), values = rep(1, 96))
  holidays <- list(
    diwali = as.Date(c("2018-11-07", "2019-10-27", "2020-11-14")),
    cny = list(
      dates = as.Date(c("2018-02-16", "2019-02-05", "2020-01-25")),
      start = -3,
      end = 3
    )
  )

  td <- build_user_xreg(y, holidays = holidays, td_usertype = "holiday")

  expect_identical(names(td), c("diwali", "cny"))
  expect_identical(attr(td, "td_usertype"), "holiday")
  expect_true(all(vapply(td, inherits, logical(1), "ts")))
  expect_true(all(vapply(td, length, integer(1)) == length(y)))
})
