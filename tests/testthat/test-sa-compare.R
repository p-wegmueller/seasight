test_that("sa_compare returns a self-contained comparison object", {
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
        arima = "(0 1 1)(0 1 1)",
        engine = "x11",
        AICc = seasight:::.aicc(current_model),
        QS_p = 0.20,
        LB_p = 0.20,
        dist_sa_L1 = 0,
        corr_seas = 1
      )
    ),
    class = "auto_seasonal_analysis"
  )

  cmp <- sa_compare(res, current_model)

  expect_s3_class(cmp, "seasight_sa_compare")
  expect_equal(cmp$decision, "CHANGE_TO_NEW_MODEL")
  expect_named(cmp, c("decision", "summary", "series", "diagnostics", "table"))
  expect_s3_class(cmp$summary, "tbl_df")
  expect_true(all(c("metric", "current", "best", "difference") %in% names(cmp$summary)))
  expect_equal(cmp$table$model, c("current", "best"))
  expect_true(isTRUE(cmp$series$aligned_sa$ok))
})

test_that("sa_compare validates public inputs", {
  fake_res <- structure(
    list(best = NULL, table = tibble::tibble()),
    class = "auto_seasonal_analysis"
  )
  fake_model <- structure(list(), class = "seas")

  expect_error(sa_compare(list(), fake_model), "`res` must be an object")
  expect_error(sa_compare(fake_res, list()), "`current_model` must be")
  expect_error(sa_compare(fake_res, fake_model), "does not contain a fitted best model")
})

test_that("sa_should_switch validates result structure", {
  expect_error(sa_should_switch(list()), "`res` must be an object")
  expect_error(
    sa_should_switch(structure(list(table = tibble::tibble()), class = "auto_seasonal_analysis")),
    "`res\\$table` must contain"
  )
})
