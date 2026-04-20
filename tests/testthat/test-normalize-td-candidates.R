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
