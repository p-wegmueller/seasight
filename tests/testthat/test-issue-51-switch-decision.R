switch_result <- function(candidate_aicc = 100,
                          candidate_qs = 0.20,
                          candidate_lb = 0.20,
                          incumbent_aicc = 110,
                          incumbent_qs = 0.20,
                          incumbent_lb = 0.20,
                          comparable = TRUE,
                          dist_sa = 1,
                          corr_seas = 0.95) {
  structure(
    list(
      table = tibble::tibble(
        AICc = candidate_aicc,
        QS_p = candidate_qs,
        LB_p = candidate_lb,
        dist_sa_L1 = dist_sa,
        corr_seas = corr_seas
      ),
      baseline = list(
        diagnostics = tibble::tibble(
          AICc = incumbent_aicc,
          QS_p = incumbent_qs,
          LB_p = incumbent_lb
        ),
        aicc_comparable = comparable
      )
    ),
    class = "auto_seasonal_analysis"
  )
}

test_that("issue #51 requires a material incumbent-relative improvement", {
  expect_equal(sa_should_switch(switch_result()), "CHANGE_TO_NEW_MODEL")
  expect_equal(
    sa_should_switch(switch_result(incumbent_aicc = 101.99)),
    "KEEP_CURRENT_MODEL"
  )
  expect_equal(
    sa_should_switch(switch_result(incumbent_aicc = 95)),
    "KEEP_CURRENT_MODEL"
  )

  custom <- sa_should_switch(
    switch_result(incumbent_aicc = 101),
    thresholds = list(min_delta_aicc = 0.5)
  )
  expect_equal(custom, "CHANGE_TO_NEW_MODEL")
})

test_that("issue #51 blocks absolute and incumbent-relative diagnostic regressions", {
  expect_equal(
    sa_should_switch(switch_result(candidate_qs = 0.01)),
    "KEEP_CURRENT_MODEL"
  )
  expect_equal(
    sa_should_switch(switch_result(candidate_lb = 0.01)),
    "KEEP_CURRENT_MODEL"
  )
  expect_equal(
    sa_should_switch(switch_result(candidate_qs = 0.20, incumbent_qs = 0.80)),
    "KEEP_CURRENT_MODEL"
  )
  expect_equal(
    sa_should_switch(switch_result(candidate_lb = 0.20, incumbent_lb = 0.80)),
    "KEEP_CURRENT_MODEL"
  )
  expect_equal(
    sa_should_switch(switch_result(corr_seas = 0.5)),
    "KEEP_CURRENT_MODEL"
  )
})

test_that("issue #51 handles missing comparison evidence explicitly", {
  no_baseline <- structure(
    list(
      table = tibble::tibble(
        AICc = 100,
        QS_p = 0.20,
        LB_p = 0.20,
        dist_sa_L1 = 1,
        corr_seas = 0.95
      )
    ),
    class = "auto_seasonal_analysis"
  )

  expect_equal(sa_should_switch(no_baseline), "NO_BASELINE")
  expect_equal(
    sa_should_switch(switch_result(comparable = FALSE)),
    "REVIEW_REQUIRED"
  )
  expect_equal(
    sa_should_switch(switch_result(candidate_aicc = NA_real_)),
    "REVIEW_REQUIRED"
  )
  expect_equal(
    sa_should_switch(switch_result(incumbent_qs = NA_real_)),
    "REVIEW_REQUIRED"
  )
})

test_that("issue #51 exposes a structured decision reason", {
  assessment <- sa_should_switch(switch_result(), details = TRUE)

  expect_s3_class(assessment, "seasight_switch_assessment")
  expect_named(assessment, c("decision", "reason", "metrics"))
  expect_equal(assessment$decision, "CHANGE_TO_NEW_MODEL")
  expect_match(assessment$reason, "improves comparable AICc")
  expect_equal(assessment$metrics$delta_aicc, 10)
})

test_that("issue #51 exposes the incumbent-relative reason in report narrative", {
  res <- switch_result()
  res$baseline$current_sa <- stats::ts(1:4)
  res$seasonality <- list(overall = tibble::tibble(call_overall = "ADJUST"))
  res$table$model_label <- "candidate"
  res$table$arima <- "(0 1 1)(0 1 1)"
  res$table$score_100 <- 100
  res$table$with_td <- FALSE

  html <- as.character(seasight:::.build_selection_rationale(res))

  expect_match(html, "CHANGE_TO_NEW_MODEL")
  expect_match(html, "improves comparable AICc by 10.00")
})

test_that("issue #51 keeps an identical incumbent model", {
  skip_if_not_installed("seasonal")

  current_model <- seasonal::seas(
    AirPassengers,
    x11 = "",
    arima.model = "(0 1 1)(0 1 1)",
    regression.aictest = NULL,
    outlier = NULL,
    transform.function = "log"
  )
  res <- structure(
    list(
      best = current_model,
      transform = "log",
      table = tibble::tibble(
        AICc = seasight:::.aicc(current_model),
        QS_p = 0.20,
        LB_p = 0.20,
        dist_sa_L1 = 0,
        corr_seas = 1
      )
    ),
    class = "auto_seasonal_analysis"
  )

  assessment <- sa_should_switch(
    res,
    current_model = current_model,
    details = TRUE
  )

  expect_equal(assessment$decision, "KEEP_CURRENT_MODEL")
  expect_match(assessment$reason, "same seasonally adjusted series")
})

test_that("automatic analysis stores comparable incumbent diagnostics", {
  skip_on_cran()
  skip_if_not_installed("seasonal")

  current_model <- seasonal::seas(
    AirPassengers,
    x11 = "",
    arima.model = "(0 1 1)(0 1 1)",
    regression.aictest = NULL,
    outlier = NULL,
    transform.function = "log"
  )
  res <- auto_seasonal_analysis(
    AirPassengers,
    current_model = current_model,
    specs = "(0 1 1)(0 1 1)",
    use_fivebest = FALSE,
    max_specs = 1,
    auto_outliers = FALSE,
    include_easter = "off",
    include_history_top_n = 0,
    transform_fun = "log",
    engine = "x11"
  )

  expect_s3_class(res$baseline$diagnostics, "tbl_df")
  expect_true(res$baseline$aicc_comparable)
  expect_equal(sa_should_switch(res), "KEEP_CURRENT_MODEL")
})
