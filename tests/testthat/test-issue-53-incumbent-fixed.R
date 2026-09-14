test_that("incumbent-fixed candidates retain transformation and fixed outliers", {
  incumbent <- seasonal::seas(
    AirPassengers,
    x11 = "",
    transform.function = "log",
    arima.model = "(0 1 1)(0 1 1)",
    regression.variables = c(
      "ao1951.May", "ls1958.Mar", "tc1954.Jan", "easter[8]"
    ),
    regression.aictest = NULL,
    outlier = NULL
  )
  incumbent_terms <- seasight:::.regression_metadata(incumbent)$effective_variables

  res <- auto_seasonal_analysis(
    AirPassengers,
    current_model = incumbent,
    comparison_mode = "incumbent_fixed",
    specs = c("(0 1 1)(0 1 1)", "(1 1 1)(0 1 1)"),
    use_fivebest = FALSE,
    max_specs = 2,
    include_history_top_n = 0,
    transform_fun = "none",
    include_easter = "off",
    auto_outliers = TRUE,
    engine = "x11"
  )

  expect_identical(res$comparison_mode, "incumbent_fixed")
  expect_identical(res$transform, "log")
  expect_true(res$regressor_comparison$unchanged)
  expect_setequal(res$regressor_comparison$candidate, incumbent_terms)
  expect_true(all(res$table$comparison_mode == "incumbent_fixed"))
  expect_length(unique(res$table$regression_variables), 1L)
  expect_match(res$table$outlier_variables[[1]], "ao1951.May", fixed = TRUE)
  expect_match(res$table$outlier_variables[[1]], "ls1958.Mar", fixed = TRUE)
  expect_match(res$table$outlier_variables[[1]], "tc1954.Jan", fixed = TRUE)

  candidate_terms <- seasight:::.regression_metadata(res$best)$effective_variables
  expect_setequal(candidate_terms, incumbent_terms)
  expect_null(res$best$list$regression.b)
  expect_null(res$best$list$regression.fix)
  expect_null(res$best$list$outlier)
  expect_null(res$best$list$regression.aictest)
})

test_that("incumbent-fixed candidates reuse stored user-regressor data", {
  set.seed(53)
  xreg <- stats::ts(
    cbind(
      policy = stats::rnorm(length(AirPassengers)),
      holiday = stats::rnorm(length(AirPassengers))
    ),
    start = stats::start(AirPassengers),
    frequency = stats::frequency(AirPassengers)
  )
  incumbent <- seasonal::seas(
    AirPassengers,
    xreg = xreg,
    regression.usertype = c("user", "holiday"),
    x11 = "",
    forecast.maxlead = 0,
    transform.function = "log",
    arima.model = "(0 1 1)(0 1 1)",
    regression.variables = c("ao1951.May", "easter[8]"),
    regression.aictest = NULL,
    outlier = NULL
  )
  fixed <- seasight:::.incumbent_fixed_spec(incumbent, AirPassengers)

  candidate <- seasight:::.fit_spec(
    AirPassengers,
    arima_model = "(1 1 1)(0 1 1)",
    transform_fun = "none",
    auto_outliers = TRUE,
    include_easter_mode = "off",
    engine = "x11",
    fixed_regression = fixed
  )[[1]]

  expect_s3_class(candidate$model, "seas")
  expect_identical(
    as.numeric(candidate$model$list$xreg),
    as.numeric(incumbent$list$xreg)
  )
  expect_identical(
    candidate$model$list$regression.usertype,
    c("user", "holiday")
  )
  expect_setequal(
    seasight:::.regression_metadata(candidate$model)$effective_variables,
    seasight:::.regression_metadata(incumbent)$effective_variables
  )
  expect_null(candidate$model$list$regression.b)

  broken <- incumbent
  broken$list$xreg <- NULL
  expect_error(
    seasight:::.incumbent_fixed_spec(broken, AirPassengers),
    "stored data could not be resolved",
    fixed = TRUE
  )
})

test_that("incumbent-fixed mode validates its comparison contract", {
  incumbent <- seasonal::seas(
    AirPassengers,
    x11 = "",
    transform.function = "log",
    arima.model = "(0 1 1)(0 1 1)",
    regression.aictest = NULL,
    outlier = NULL
  )

  expect_error(
    auto_seasonal_analysis(
      AirPassengers,
      comparison_mode = "incumbent_fixed",
      use_fivebest = FALSE,
      max_specs = 1
    ),
    "requires `current_model`",
    fixed = TRUE
  )
  expect_error(
    auto_seasonal_analysis(
      stats::window(AirPassengers, start = c(1950, 1)),
      current_model = incumbent,
      comparison_mode = "incumbent_fixed",
      use_fivebest = FALSE,
      max_specs = 1
    ),
    "same time series",
    fixed = TRUE
  )
  expect_error(
    auto_seasonal_analysis(
      AirPassengers,
      current_model = incumbent,
      comparison_mode = "incumbent_fixed",
      td_candidates = list(x = AirPassengers),
      use_fivebest = FALSE,
      max_specs = 1
    ),
    "keeps the incumbent regressor set unchanged",
    fixed = TRUE
  )
})

test_that("report rationale identifies the AICc comparison design", {
  tbl <- tibble::tibble(
    model_label = c("best", "runner"),
    arima = c("(1 1 1)(0 1 1)", "(0 1 1)(0 1 1)"),
    comparison_mode = "incumbent_fixed",
    with_td = FALSE,
    with_easter = FALSE,
    score_100 = c(90, 80),
    AICc = c(100, 103),
    LB_p = c(0.30, 0.20),
    QS_p = c(0.30, 0.20)
  )
  fixed_res <- structure(
    list(
      table = tbl,
      comparison_mode = "incumbent_fixed",
      regressor_comparison = list(
        summary = "Regressors unchanged: ao1951.May.",
        unchanged = TRUE
      ),
      seasonality = list(overall = tibble::tibble(call_overall = "ADJUST"))
    ),
    class = "auto_seasonal_analysis"
  )
  fixed_html <- paste(as.character(htmltools::renderTags(
    seasight:::.build_selection_rationale(fixed_res)
  )$html), collapse = "\n")
  expect_match(fixed_html, "incumbent-fixed comparison", fixed = TRUE)
  expect_match(fixed_html, "only ARIMA", fixed = TRUE)
  expect_match(fixed_html, "Regressors unchanged", fixed = TRUE)

  full_res <- fixed_res
  full_res$comparison_mode <- "full_search"
  full_res$table$comparison_mode <- "full_search"
  full_html <- paste(as.character(htmltools::renderTags(
    seasight:::.build_selection_rationale(full_res)
  )$html), collapse = "\n")
  expect_match(full_html, "full-search comparison", fixed = TRUE)
  expect_match(full_html, "different regressors or outliers", fixed = TRUE)
})
