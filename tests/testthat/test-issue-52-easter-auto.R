fake_easter_seas <- function(easter_window = NA_integer_) {
  regression <- if (is.finite(easter_window)) {
    list(variables = sprintf("easter[%d]", as.integer(easter_window)))
  } else {
    NULL
  }
  structure(list(model = list(regression = regression)), class = "seas")
}

test_that("Easter modes pass distinct specifications to seasonal::seas", {
  y <- fixture_monthly_ts(n = 60L)
  calls <- list()

  testthat::local_mocked_bindings(
    seas = function(...) {
      args <- list(...)
      calls[[length(calls) + 1L]] <<- args
      requested <- args$regression.variables
      window <- if (length(requested)) {
        as.integer(sub(".*\\[([0-9]+)\\].*", "\\1", requested[[1]]))
      } else {
        NA_integer_
      }
      fake_easter_seas(window)
    },
    .package = "seasonal"
  )

  auto <- seasight:::.fit_spec(
    y, "(0 1 1)(0 1 1)", "none",
    td_xreg = fixture_monthly_regressor(y), td_usertype = "td",
    include_easter_mode = "auto", engine = "x11", auto_outliers = FALSE
  )
  always <- seasight:::.fit_spec(
    y, "(0 1 1)(0 1 1)", "none",
    include_easter_mode = "always", easter_len = 13L,
    engine = "x11", auto_outliers = FALSE
  )
  off <- seasight:::.fit_spec(
    y, "(0 1 1)(0 1 1)", "none",
    include_easter_mode = "off", engine = "x11", auto_outliers = FALSE
  )

  expect_identical(calls[[1]]$regression.aictest, "easter")
  expect_null(calls[[1]]$regression.variables)
  expect_identical(calls[[1]]$regression.usertype, "td")
  expect_true(auto[[1]]$with_td)
  expect_false(auto[[1]]$with_easter)

  expect_null(calls[[2]]$regression.aictest)
  expect_identical(calls[[2]]$regression.variables, "easter[13]")
  expect_true(always[[1]]$with_easter)
  expect_identical(always[[1]]$easter_window, 13L)

  expect_null(calls[[3]]$regression.aictest)
  expect_null(calls[[3]]$regression.variables)
  expect_false(off[[1]]$with_easter)
  expect_true(is.na(off[[1]]$easter_window))
})

test_that("auto Easter metadata follows the fitted model", {
  y <- fixture_monthly_ts(n = 60L)
  retained_window <- 8L

  testthat::local_mocked_bindings(
    seas = function(...) fake_easter_seas(retained_window),
    .package = "seasonal"
  )

  selected <- seasight:::.fit_spec(
    y, "(0 1 1)(0 1 1)", "none",
    include_easter_mode = "auto", engine = "x11", auto_outliers = FALSE
  )
  expect_true(selected[[1]]$with_easter)
  expect_identical(selected[[1]]$easter_window, 8L)

  retained_window <- NA_integer_
  rejected <- seasight:::.fit_spec(
    y, "(0 1 1)(0 1 1)", "none",
    include_easter_mode = "auto", engine = "x11", auto_outliers = FALSE
  )
  expect_false(rejected[[1]]$with_easter)
  expect_true(is.na(rejected[[1]]$easter_window))
})

test_that("Easter metadata handles fitted variables and coefficient fallback", {
  direct <- fake_easter_seas(15L)
  expect_identical(
    seasight:::.easter_metadata(direct),
    list(with_easter = TRUE, easter_window = 15L)
  )

  fallback <- structure(list(model = list()), class = "seas")
  testthat::local_mocked_bindings(
    coef = function(object, ...) stats::setNames(0.2, "Easter[1]"),
    .package = "stats"
  )
  expect_identical(
    seasight:::.easter_metadata(fallback),
    list(with_easter = TRUE, easter_window = 1L)
  )
  expect_identical(
    seasight:::.easter_metadata(NULL),
    list(with_easter = FALSE, easter_window = NA_integer_)
  )
})

test_that("candidate table stores actual auto-selected Easter window", {
  res <- auto_seasonal_analysis(
    AirPassengers,
    specs = "(0 1 1)(0 1 1)",
    use_fivebest = FALSE,
    max_specs = 1,
    auto_outliers = FALSE,
    include_easter = "auto",
    include_history_top_n = 0,
    transform_fun = "log",
    engine = "x11"
  )

  expected <- seasight:::.easter_metadata(res$best)
  expect_identical(res$table$with_easter[[1]], expected$with_easter)
  expect_identical(res$table$easter_window[[1]], expected$easter_window)
})

test_that("report shows actual Easter inclusion and selected window", {
  tbl <- tibble::tibble(
    model_label = c("best", "runner"),
    arima = c("(0 1 1)(0 1 1)", "(1 1 1)(0 1 1)"),
    with_td = c(FALSE, FALSE),
    with_easter = c(TRUE, FALSE),
    easter_window = c(8L, NA_integer_),
    score_100 = c(90, 80),
    AICc = c(100, 102),
    LB_p = c(0.30, 0.20),
    QS_p = c(0.30, 0.20)
  )
  res <- structure(
    list(
      table = tbl,
      seasonality = list(overall = tibble::tibble(call_overall = "ADJUST"))
    ),
    class = "auto_seasonal_analysis"
  )

  top_html <- paste(as.character(htmltools::renderTags(
    seasight:::.build_top_candidates_table(res, n = 2)
  )$html), collapse = "\n")
  rationale_html <- paste(as.character(htmltools::renderTags(
    seasight:::.build_selection_rationale(res)
  )$html), collapse = "\n")

  expect_match(top_html, ">Easter<", fixed = TRUE)
  expect_match(top_html, "easter[8]", fixed = TRUE)
  expect_match(top_html, ">none<", fixed = TRUE)
  expect_match(rationale_html, "with easter[8]", fixed = TRUE)
  expect_match(rationale_html, "without Easter", fixed = TRUE)
})
