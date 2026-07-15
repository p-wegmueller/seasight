test_that(".fit_spec pads xreg for SEATS (and sets forecast lead/back)", {
  skip_if_not(exists(".fit_spec", envir = asNamespace("seasight"), inherits = FALSE))
  
  y  <- fixture_quarterly_ts()
  xr <- stats::ts(seq_len(40), start = start(y), frequency = frequency(y))
  
  state <- new.env(parent = emptyenv())
  state$captured <- NULL
  
  testthat::local_mocked_bindings(
    seas = function(...) {
      state$captured <- list(...)
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
  
  expect_true(!is.null(state$captured$xreg))
  expect_equal(NROW(state$captured$xreg), length(y) + lead_n)
  
  xmat <- as.matrix(state$captured$xreg)
  expect_true(all(xmat[(nrow(xmat) - lead_n + 1L):nrow(xmat), , drop = FALSE] == 0))
  
  expect_equal(as.integer(state$captured$forecast.maxlead), lead_n)
  expect_equal(as.integer(state$captured$forecast.maxback), 0L)
})

test_that(".fit_spec does NOT pad xreg for X11", {
  skip_if_not(exists(".fit_spec", envir = asNamespace("seasight"), inherits = FALSE))
  
  y  <- fixture_quarterly_ts()
  xr <- stats::ts(seq_len(40), start = start(y), frequency = frequency(y))
  
  state <- new.env(parent = emptyenv())
  state$captured <- NULL
  testthat::local_mocked_bindings(
    seas = function(...) {
      state$captured <- list(...)
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
  expect_true(!is.null(state$captured$xreg))
  expect_equal(NROW(state$captured$xreg), length(y))
  expect_true(is.null(state$captured$forecast.maxlead))
  expect_true(is.null(state$captured$forecast.maxback))
})

test_that(".fit_spec does NOT pad when td_xreg is NULL", {
  skip_if_not(exists(".fit_spec", envir = asNamespace("seasight"), inherits = FALSE))
  
  y <- fixture_quarterly_ts()
  
  state <- new.env(parent = emptyenv())
  state$captured <- NULL
  testthat::local_mocked_bindings(
    seas = function(...) {
      state$captured <- list(...)
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
  expect_true(is.null(state$captured$xreg))
  expect_true(is.null(state$captured$forecast.maxlead))
  expect_true(is.null(state$captured$forecast.maxback))
})

test_that(".fit_spec padding preserves multi-column xreg", {
  skip_if_not(exists(".fit_spec", envir = asNamespace("seasight"), inherits = FALSE))
  
  y <- fixture_quarterly_ts()
  
  xr2 <- ts(cbind(a = seq_len(40), b = seq_len(40) + 100),
            start = start(y), frequency = frequency(y))
  
  state <- new.env(parent = emptyenv())
  state$captured <- NULL
  testthat::local_mocked_bindings(
    seas = function(...) {
      state$captured <- list(...)
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
  
  xm <- as.matrix(state$captured$xreg)
  expect_equal(ncol(xm), 2L)
  expect_equal(colnames(xm), c("a", "b"))
  expect_equal(nrow(xm), length(y) + lead_n)
  expect_true(all(xm[(nrow(xm) - lead_n + 1L):nrow(xm), , drop = FALSE] == 0))
})

test_that(".fit_spec returns the most recent fallback error when all fits fail", {
  skip_if_not(exists(".fit_spec", envir = asNamespace("seasight"), inherits = FALSE))
  
  y  <- fixture_quarterly_ts()
  xr <- stats::ts(seq_len(40), start = start(y), frequency = frequency(y))
  
  state <- new.env(parent = emptyenv())
  state$attempts <- character()
  
  testthat::local_mocked_bindings(
    seas = function(...) {
      args <- list(...)
      label <- if (is.null(args$regression.usertype)) {
        "fallback-drop-usertype"
      } else if (identical(unique(args$regression.usertype), "td")) {
        "fallback-td"
      } else {
        "initial"
      }
      state$attempts <- c(state$attempts, label)
      stop(paste("mock failure", label), call. = FALSE)
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
  
  expect_null(res[[1]]$model)
  expect_identical(
    state$attempts,
    c("initial", "fallback-drop-usertype", "fallback-td")
  )
  expect_match(res[[1]]$err, "mock failure fallback-td", fixed = TRUE)
})
