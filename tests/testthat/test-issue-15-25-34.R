test_that("seasonal-only spec guard identifies non-seasonal ARIMA specs", {
  expect_false(seasight:::.is_seasonal_arima_spec("(1 1 1)(0 0 0)"))
  expect_true(seasight:::.is_seasonal_arima_spec("(1 1 1)(0 1 1)"))
  expect_true(seasight:::.is_seasonal_arima_spec("(0 1 1)(1 0 0)"))
})

test_that("seasonal-only policy returns explicit do-not-adjust when no seasonal specs remain", {
  res <- auto_seasonal_analysis(
    AirPassengers,
    specs = "(1 1 1)(0 0 0)",
    use_fivebest = FALSE
  )

  expect_s3_class(res, "auto_seasonal_analysis")
  expect_null(res$best)
  expect_equal(res$seasonality$overall$call_overall, "DO_NOT_ADJUST")
  expect_equal(res$table$model_label, "DO_NOT_ADJUST")

  out <- tempfile(fileext = ".html")
  report <- sa_report_html(res, outfile = out)
  expect_true(file.exists(out))
  expect_equal(report$report, out)
})

test_that("ranking tie-breaks deterministically by engine preference then AICc", {
  tbl <- tibble::tibble(
    model_label = c("x11_low_aicc", "seats_high_aicc", "unknown"),
    engine = c("x11", "seats", NA_character_),
    QS_p = c(0.20, 0.20, 0.20),
    AICc = c(10, 20, 5),
    rev_mae = c(NA_real_, NA_real_, NA_real_),
    dist_sa_L1 = c(NA_real_, NA_real_, NA_real_),
    dist_seas_RMS = c(NA_real_, NA_real_, NA_real_),
    vola_reduction_pct = c(NA_real_, NA_real_, NA_real_)
  )

  ranked_seats <- seasight:::.rank_candidates(
    tbl,
    w_qs = 0, w_stability = 0, w_aicc = 0, w_rev = 0,
    w_dist_sa = 0, w_dist_seas = 0, w_engine = 0,
    engine_pref = "seats"
  )
  ranked_x11 <- seasight:::.rank_candidates(
    tbl,
    w_qs = 0, w_stability = 0, w_aicc = 0, w_rev = 0,
    w_dist_sa = 0, w_dist_seas = 0, w_engine = 0,
    engine_pref = "x11"
  )

  expect_equal(ranked_seats$model_label[1], "seats_high_aicc")
  expect_equal(ranked_x11$model_label[1], "x11_low_aicc")
})

test_that("R source files stay ASCII-only", {
  r_files <- list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  has_non_ascii <- vapply(r_files, function(path) {
    any(grepl("[^ -~\r\n\t]", readLines(path, warn = FALSE), perl = TRUE))
  }, logical(1))

  expect_false(any(has_non_ascii), info = paste(names(has_non_ascii)[has_non_ascii], collapse = ", "))
})
