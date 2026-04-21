test_that(".normalize_td_candidates keeps NULL as no-candidates sentinel", {
  skip_if_not(exists(".normalize_td_candidates", envir = asNamespace("seasight"), inherits = FALSE))
  
  y <- ts(1:24, start = c(2020, 1), frequency = 12)
  out <- seasight:::.normalize_td_candidates(NULL, y = y)
  
  expect_null(out)
})

test_that(".normalize_td_candidates rejects invalid list inputs", {
  skip_if_not(exists(".normalize_td_candidates", envir = asNamespace("seasight"), inherits = FALSE))
  
  y <- ts(1:24, start = c(2020, 1), frequency = 12)
  
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
  
  y <- ts(1:24, start = c(2020, 1), frequency = 12)
  td1 <- ts(runif(24), start = start(y), frequency = frequency(y))
  td2 <- ts(runif(24), start = start(y), frequency = frequency(y))
  
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
  y <- ts(1:24, start = c(2020, 1), frequency = 12)
  x <- ts(101:148, start = c(2019, 1), frequency = 12)
  q <- ts(1:8, start = c(2020, 1), frequency = 4)

  out <- sa_align_regressor(y, x)

  expect_s3_class(out, "ts")
  expect_equal(NROW(out), length(y))
  expect_equal(stats::start(out), stats::start(y))
  expect_equal(stats::end(out), stats::end(y))
  expect_null(sa_align_regressor(y, q))
})
