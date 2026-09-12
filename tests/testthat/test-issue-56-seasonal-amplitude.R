test_that("multiplicative amplitude is the seasonal-factor range in percent", {
  seasonal_factor <- stats::ts(c(0.9, 1.1, 0.95, 1.05), frequency = 4)
  original <- stats::ts(c(100, 120, 140, 160), frequency = 4)

  amplitude <- seasight:::.seasonal_amplitude(
    seasonal_factor, original, transform = "log"
  )
  scaled <- seasight:::.seasonal_amplitude(
    seasonal_factor, original * 1000, transform = "log"
  )

  expect_equal(amplitude$seasonal_amp_abs, 0.2)
  expect_equal(amplitude$seasonal_amp_pct, 20)
  expect_identical(amplitude$seasonal_amp_basis, "multiplicative_factor")
  expect_equal(scaled$seasonal_amp_pct, amplitude$seasonal_amp_pct)
})

test_that("additive amplitude uses a robust level and is scale invariant", {
  seasonal_component <- stats::ts(c(-10, 10, -5, 5), frequency = 4)
  original <- stats::ts(c(80, 100, 120, 100), frequency = 4)

  amplitude <- seasight:::.seasonal_amplitude(
    seasonal_component, original, transform = "none"
  )
  scaled <- seasight:::.seasonal_amplitude(
    seasonal_component * 10, original * 10, transform = "none"
  )

  expect_equal(amplitude$seasonal_amp_abs, 20)
  expect_equal(amplitude$seasonal_amp_pct, 20)
  expect_equal(amplitude$seasonal_amp_level, 100)
  expect_identical(amplitude$seasonal_amp_basis, "additive_median_abs_level")
  expect_equal(scaled$seasonal_amp_abs, 200)
  expect_equal(scaled$seasonal_amp_pct, amplitude$seasonal_amp_pct)
})

test_that("unavailable components and negligible additive levels return NA", {
  original <- stats::ts(c(10, 11, 12, 13), frequency = 4)
  missing <- seasight:::.seasonal_amplitude(NULL, original, transform = "log")
  all_missing <- seasight:::.seasonal_amplitude(
    stats::ts(rep(NA_real_, 4), frequency = 4),
    original,
    transform = "log"
  )
  negligible_level <- seasight:::.seasonal_amplitude(
    stats::ts(c(-1, 1, -0.5, 0.5), frequency = 4),
    stats::ts(c(0, 0, 0, 100), frequency = 4),
    transform = "none"
  )

  expect_true(all(is.na(missing)))
  expect_true(all(is.na(all_missing)))
  expect_equal(negligible_level$seasonal_amp_abs, 2)
  expect_true(is.na(negligible_level$seasonal_amp_pct))
  expect_equal(negligible_level$seasonal_amp_level, 0)
  expect_identical(
    negligible_level$seasonal_amp_basis,
    "additive_median_abs_level"
  )
})

test_that("model diagnostics use transformation metadata for amplitude", {
  skip_if_not_installed("seasonal")

  log_model <- seasonal::seas(
    AirPassengers,
    transform.function = "log",
    regression.aictest = NULL,
    outlier = NULL
  )
  level_model <- seasonal::seas(
    AirPassengers,
    transform.function = "none",
    x11 = "",
    regression.aictest = NULL,
    outlier = NULL
  )

  log_diagnostics <- seasight:::.diagnostics_finance(log_model, AirPassengers)
  level_diagnostics <- seasight:::.diagnostics_finance(level_model, AirPassengers)

  expect_identical(
    log_diagnostics$seasonal_amp_basis,
    "multiplicative_factor"
  )
  expect_equal(
    log_diagnostics$seasonal_amp_pct,
    100 * log_diagnostics$seasonal_amp_abs
  )
  expect_identical(
    level_diagnostics$seasonal_amp_basis,
    "additive_median_abs_level"
  )
  expect_equal(
    level_diagnostics$seasonal_amp_pct,
    100 * level_diagnostics$seasonal_amp_abs /
      stats::median(abs(AirPassengers))
  )
})

test_that("corrected multiplicative amplitudes do not trigger non-adjustment", {
  amplitude <- seasight:::.seasonal_amplitude(
    stats::ts(c(0.9, 1.1), frequency = 2),
    stats::ts(c(100, 110), frequency = 2),
    transform = "log"
  )
  row <- tibble::tibble(
    IDS = "no",
    M7 = 1.10,
    QSori_p_x11 = 0.20,
    QSori_p_seats = 0.20,
    SEATS_has_seasonal = TRUE,
    seasonal_amp_pct = amplitude$seasonal_amp_pct,
    vola_reduction_pct = 10
  )

  expect_gt(amplitude$seasonal_amp_pct, 1)
  expect_false(sa_is_do_not_adjust(row))
})

test_that("seasonal-amplitude report text states its units", {
  multiplicative <- tibble::tibble(
    seasonal_amp_abs = 0.2,
    seasonal_amp_pct = 20,
    seasonal_amp_basis = "multiplicative_factor"
  )
  additive <- tibble::tibble(
    seasonal_amp_abs = 20,
    seasonal_amp_pct = 20,
    seasonal_amp_basis = "additive_median_abs_level"
  )
  unavailable <- tibble::tibble(
    seasonal_amp_abs = NA_real_,
    seasonal_amp_pct = NA_real_,
    seasonal_amp_basis = NA_character_
  )

  expect_match(
    seasight:::.seasonal_amplitude_report_text(multiplicative),
    "seasonal-factor.*20.0 percentage points",
    ignore.case = TRUE
  )
  expect_match(
    seasight:::.seasonal_amplitude_report_text(additive),
    "20.0% of median absolute level",
    fixed = TRUE
  )
  expect_match(
    seasight:::.seasonal_amplitude_report_text(unavailable),
    "unavailable",
    fixed = TRUE
  )
})

test_that("HTML report uses the scale-specific amplitude label", {
  skip_if_not_installed("seasonal")
  best_model <- seasonal::seas(
    AirPassengers,
    transform.function = "log",
    arima.model = "(0 1 1)(0 1 1)",
    regression.aictest = NULL,
    outlier = NULL
  )
  result <- structure(
    list(
      best = best_model,
      y = AirPassengers,
      table = tibble::tibble(
        model_label = "best",
        arima = "(0 1 1)(0 1 1)",
        engine = "seats",
        SEATS_model_switch = FALSE,
        SEATS_operative_model = "(0 1 1)(0 1 1)",
        SEATS_has_seasonal = TRUE,
        with_td = FALSE,
        td_name = NA_character_,
        with_easter = FALSE,
        easter_window = NA_integer_,
        score_100 = 100,
        AICc = 100,
        M7 = 0.8,
        IDS = "yes",
        LB_p = 0.3,
        QSori_p_x11 = 0.01,
        QSori_p_seats = 0.02,
        QSori_p = 0.01,
        QS_p_x11 = 0.2,
        QS_p_seats = 0.3,
        QS_p = 0.2,
        td_p = NA_real_,
        vola_reduction_pct = 10,
        seasonal_amp_abs = 0.2,
        seasonal_amp_pct = 20,
        seasonal_amp_level = 1,
        seasonal_amp_basis = "multiplicative_factor",
        dist_sa_L1 = NA_real_,
        rev_mae = NA_real_
      ),
      frequency = 12,
      transform = "log",
      baseline = list(current_sa = NULL, diagnostics = NULL),
      seasonality = list(
        overall = tibble::tibble(call_overall = "ADJUST")
      )
    ),
    class = "auto_seasonal_analysis"
  )
  output <- tempfile(fileext = ".html")

  expect_no_error(
    invisible(capture.output(sa_report_html(y = result, outfile = output)))
  )
  html <- paste(readLines(output, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(
    html,
    "Seasonal-factor peak-to-trough range: 0.20 (20.0 percentage points).",
    fixed = TRUE
  )
  expect_match(html, "Seasonal P-T amp. %", fixed = TRUE)
})
