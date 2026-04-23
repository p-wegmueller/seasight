test_that(".normalize_td_candidates keeps NULL as no-candidates sentinel", {
  skip_if_not(exists(".normalize_td_candidates", envir = asNamespace("seasight"), inherits = FALSE))
  
  y <- fixture_monthly_ts()
  out <- seasight:::.normalize_td_candidates(NULL, y = y)
  
  expect_null(out)
})

test_that(".normalize_td_candidates rejects invalid list inputs", {
  skip_if_not(exists(".normalize_td_candidates", envir = asNamespace("seasight"), inherits = FALSE))
  
  y <- fixture_monthly_ts()
  
  expect_error(
    seasight:::.normalize_td_candidates(list(), y = y),
    "`td_candidates` must not be an empty list."
  )
  
  expect_error(
    seasight:::.normalize_td_candidates(list(foo = NULL), y = y),
    "`td_candidates` entries must not be NULL."
  )

  expect_error(
    seasight:::.normalize_td_candidates(
      stats::setNames(list(y, y), c("dup", "dup")),
      y = y
    ),
    "`td_candidates` names must be unique."
  )
})

test_that(".normalize_td_candidates fills only missing names and preserves td_usertype", {
  skip_if_not(exists(".normalize_td_candidates", envir = asNamespace("seasight"), inherits = FALSE))
  
  y <- fixture_monthly_ts()
  td1 <- fixture_monthly_regressor(y, offset = 100)
  td2 <- fixture_monthly_regressor(y, offset = 200)
  
  out <- seasight:::.normalize_td_candidates(
    list(td_named = td1, td2),
    y = y,
    td_usertype = "holiday"
  )
  
  expect_identical(names(out), c("td_named", "td2"))
  expect_identical(attr(out, "td_usertype"), "holiday")
  expect_true(all(vapply(out, inherits, logical(1), what = "ts")))
})

test_that("sa_align_regressor aligns compatible regressors and rejects mismatches", {
  y <- fixture_monthly_ts()
  x <- fixture_monthly_ts(n = 48L, start = c(2019, 1), values = 101:148)
  q <- fixture_quarterly_ts(n = 8L, start = c(2020, 1), values = 1:8)

  out <- sa_align_regressor(y, x)

  expect_s3_class(out, "ts")
  expect_equal(NROW(out), length(y))
  expect_equal(stats::start(out), stats::start(y))
  expect_equal(stats::end(out), stats::end(y))
  expect_null(sa_align_regressor(y, q))
})

test_that("time-series inputs reject list columns explicitly", {
  y <- data.frame(
    date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 24),
    value = I(as.list(seq_len(24)))
  )

  expect_error(
    sa_align_regressor(y, y),
    "must not contain list columns"
  )

  expect_error(
    seasight:::.as_ts(y),
    "must not contain list columns"
  )
})

test_that("unit-bearing tabular regressors are accepted and retain unit metadata", {
  skip_if_not_installed("units")

  y <- data.frame(
    date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 24),
    value = seq_len(24)
  )
  x <- data.frame(
    date = y$date,
    value = units::set_units(seq_len(24), "m")
  )

  out <- sa_align_regressor(y, x)

  expect_s3_class(out, "ts")
  expect_equal(attr(out, "units"), "m")
})
