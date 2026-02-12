test_that(".fit_spec pads xreg for SEATS (and sets forecast lead/back)", {
  skip_if_not(exists(".fit_spec", envir = asNamespace("seasight"), inherits = FALSE))
  
  y  <- ts(1:40, start = c(2015, 1), frequency = 4)
  xr <- ts(rnorm(40), start = start(y), frequency = frequency(y))
  
  captured <- NULL
  
  testthat::local_mocked_bindings(
    seas = function(...) {
      captured <<- list(...)
      structure(list(mock = TRUE), class = "seas")
    },
    .package = "seasonal"
  )
  
  res <- seasight:::.fit_spec(
    y = y,
    arima_model = "(0 1 1)(0 1 2)",
    transform_fun = "none",
    td_xreg = xr,
    td_usertype = "holiday",
    td_name = "diwali",
    engine = "seats",
    auto_outliers = FALSE,
    include_easter_mode = "off"
  )
  
  expect_true(inherits(res[[1]]$model, "seas"))
  expect_true(isTRUE(res[[1]]$with_td))
  expect_identical(res[[1]]$td_name, "diwali")
  expect_identical(res[[1]]$engine, "seats")
  
  lead_n <- 3L * frequency(y)
  
  expect_true(!is.null(captured$xreg))
  expect_equal(NROW(captured$xreg), length(y) + lead_n)
  
  xmat <- as.matrix(captured$xreg)
  expect_true(all(xmat[(nrow(xmat) - lead_n + 1L):nrow(xmat), , drop = FALSE] == 0))
  
  expect_equal(as.integer(captured$forecast.maxlead), lead_n)
  expect_equal(as.integer(captured$forecast.maxback), 0L)
})

test_that(".fit_spec does NOT pad xreg for X11", {
  skip_if_not(exists(".fit_spec", envir = asNamespace("seasight"), inherits = FALSE))
  
  y  <- ts(1:40, start = c(2015, 1), frequency = 4)
  xr <- ts(rnorm(40), start = start(y), frequency = frequency(y))
  
  captured <- NULL
  testthat::local_mocked_bindings(
    seas = function(...) {
      captured <<- list(...)
      structure(list(mock = TRUE), class = "seas")
    },
    .package = "seasonal"
  )
  
  res <- seasight:::.fit_spec(
    y = y,
    arima_model = "(0 1 1)(0 1 2)",
    transform_fun = "none",
    td_xreg = xr,
    td_usertype = "holiday",
    td_name = "diwali",
    engine = "x11",
    auto_outliers = FALSE,
    include_easter_mode = "off"
  )
  
  expect_true(inherits(res[[1]]$model, "seas"))
  expect_identical(res[[1]]$engine, "x11")
  expect_true(!is.null(captured$xreg))
  expect_equal(NROW(captured$xreg), length(y))
  expect_true(is.null(captured$forecast.maxlead))
  expect_true(is.null(captured$forecast.maxback))
})

test_that(".fit_spec does NOT pad when td_xreg is NULL", {
  skip_if_not(exists(".fit_spec", envir = asNamespace("seasight"), inherits = FALSE))
  
  y <- ts(1:40, start = c(2015, 1), frequency = 4)
  
  captured <- NULL
  testthat::local_mocked_bindings(
    seas = function(...) {
      captured <<- list(...)
      structure(list(mock = TRUE), class = "seas")
    },
    .package = "seasonal"
  )
  
  res <- seasight:::.fit_spec(
    y = y,
    arima_model = "(0 1 1)(0 1 2)",
    transform_fun = "none",
    td_xreg = NULL,
    engine = "seats",
    auto_outliers = FALSE,
    include_easter_mode = "off"
  )
  
  expect_true(inherits(res[[1]]$model, "seas"))
  expect_false(isTRUE(res[[1]]$with_td))
  expect_true(is.null(captured$xreg))
  expect_true(is.null(captured$forecast.maxlead))
  expect_true(is.null(captured$forecast.maxback))
})

test_that(".fit_spec padding preserves multi-column xreg", {
  skip_if_not(exists(".fit_spec", envir = asNamespace("seasight"), inherits = FALSE))
  
  y <- ts(1:40, start = c(2015, 1), frequency = 4)
  
  xr2 <- ts(cbind(a = rnorm(40), b = rnorm(40)),
            start = start(y), frequency = frequency(y))
  
  captured <- NULL
  testthat::local_mocked_bindings(
    seas = function(...) {
      captured <<- list(...)
      structure(list(mock = TRUE), class = "seas")
    },
    .package = "seasonal"
  )
  
  res <- seasight:::.fit_spec(
    y = y,
    arima_model = "(0 1 1)(0 1 2)",
    transform_fun = "none",
    td_xreg = xr2,
    td_usertype = "holiday",
    td_name = "multi",
    engine = "seats",
    auto_outliers = FALSE,
    include_easter_mode = "off"
  )
  
  lead_n <- 3L * frequency(y)
  
  xm <- as.matrix(captured$xreg)
  expect_equal(ncol(xm), 2L)
  expect_equal(colnames(xm), c("a", "b"))
  expect_equal(nrow(xm), length(y) + lead_n)
  expect_true(all(xm[(nrow(xm) - lead_n + 1L):nrow(xm), , drop = FALSE] == 0))
})
