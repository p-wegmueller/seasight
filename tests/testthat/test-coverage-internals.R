test_that("diagnostic wrappers assemble robust output from helper results", {
  fake <- structure(list(), class = "seas")

  testthat::local_mocked_bindings(
    .m7_stat = function(m) 0.82,
    .ids_flag = function(m) "yes",
    .lb_p = function(m) 0.44,
    .qs_on_sa_both = function(m) {
      tibble::tibble(QS_p_x11 = 0.20, QS_p_seats = 0.30, QS_p = 0.20)
    },
    .qs_original = function(m) {
      tibble::tibble(
        QSori_p_x11 = 0.01,
        QSori_p_seats = 0.02,
        QSori_p = 0.01
      )
    },
    .package = "seasight"
  )

  out <- sa_tests_model(fake)

  expect_equal(out$M7, 0.82)
  expect_equal(out$IDS, "yes")
  expect_equal(out$QS_SA, out$QS_p)
  expect_equal(out$QSori, out$QSori_p)
  expect_equal(out$LB_p, 0.44)
})

test_that("existence and adjustment decisions cover main branches", {
  adjust_tbl <- tibble::tibble(
    QS_p = c(0.04, 0.08, 0.20),
    M7 = c(0.80, 0.85, 1.20),
    IDS = c("yes", "yes", "no"),
    QSori_p = c(0.03, 0.04, 0.20)
  )
  borderline_tbl <- tibble::tibble(
    QS_p = c(0.20, 0.20, 0.20, 0.20),
    M7 = c(1.00, 1.00, 1.20, 1.20),
    IDS = c("no", "yes", "no", "yes"),
    QSori_p = c(0.20, 0.20, 0.20, 0.20)
  )
  no_tbl <- tibble::tibble(
    QS_p = c(0.20, 0.20),
    M7 = c(1.10, 1.20),
    IDS = c("no", "no"),
    QSori_p_x11 = c(0.20, 0.30),
    QSori_p_seats = c(0.25, 0.35)
  )

  expect_equal(sa_existence_call(adjust_tbl)$call_overall, "ADJUST")
  expect_equal(sa_existence_call(borderline_tbl)$call_overall, "BORDERLINE")
  expect_equal(sa_existence_call(no_tbl)$call_overall, "DO_NOT_ADJUST")

  row <- tibble::tibble(
    IDS = "no",
    M7 = 1.10,
    QSori_p_x11 = 0.15,
    QSori_p_seats = 0.20,
    SEATS_has_seasonal = FALSE,
    seasonal_amp_pct = 0.5,
    vola_reduction_pct = 2
  )
  expect_true(sa_is_do_not_adjust(row))
})

test_that("comparison and UI decision helpers handle representative rows", {
  keep_res <- structure(
    list(
      table = tibble::tibble(
        QS_p = 0.20,
        LB_p = 0.20,
        dist_sa_L1 = 1,
        corr_seas = 0.95
      )
    ),
    class = "auto_seasonal_analysis"
  )
  fail_res <- structure(
    list(
      table = tibble::tibble(
        QS_p = 0.01,
        LB_p = 0.20,
        dist_sa_L1 = 1,
        corr_seas = 0.95
      )
    ),
    class = "auto_seasonal_analysis"
  )

  expect_equal(sa_should_switch(keep_res), "CHANGE_TO_NEW_MODEL")
  expect_equal(sa_should_switch(fail_res), "KEEP_CURRENT_MODEL")

  expect_equal(
    seasight:::.compose_decision("DO_NOT_ADJUST", NULL, FALSE)$decision,
    "DO_NOT_ADJUST"
  )
  expect_equal(
    seasight:::.compose_decision("ADJUST", NULL, TRUE)$decision,
    "KEEP_CURRENT_MODEL"
  )
  expect_equal(
    seasight:::.compose_decision(
      "ADJUST",
      tibble::tibble(QS_p = 0.20, LB_p = 0.01),
      TRUE
    )$decision,
    "KEEP_CURRENT_MODEL"
  )
  expect_equal(
    seasight:::.compose_decision(
      "ADJUST",
      tibble::tibble(QS_p = 0.20, LB_p = 0.20),
      FALSE
    )$decision,
    "ADJUST"
  )

  flags <- seasight:::.gate_flags_from_decision(list(decision = "ADJUST"))
  expect_true(flags$show_new_model_blocks)

  best <- tibble::tibble(
    arima = "(0 1 1)(0 1 1)",
    td_name = "wd",
    AICc = 10,
    LB_p = 0.4,
    engine = "seats"
  )
  summary <- seasight:::.summarize_for_summary_card(best, flags)
  expect_equal(summary$best, "(0 1 1)(0 1 1)")
  expect_equal(summary$td, "wd")
})

test_that("decision utility fallbacks are explicit about missing values", {
  expect_equal(
    seasight:::.coalesce_chr(c(NA, "", "x"), c("a", "b", NA)),
    c("a", "b", "x")
  )
  expect_equal(seasight:::.safepmin(c(NA, 2), c(NA, 1)), c(NA, 1))

  best <- seasight:::.extract_best_row(tibble::tibble(
    QS_p_x11 = 0.20,
    QS_p_seats = 0.05,
    LB_p = 0.50
  ))
  expect_equal(best$qs_sa_min, 0.05)

  best_unified <- seasight:::.extract_best_row(tibble::tibble(
    QS_p = 0.30,
    QS_p_x11 = 0.20,
    QS_p_seats = 0.05
  ))
  expect_equal(best_unified$qs_sa_min, 0.30)
  expect_null(seasight:::.extract_best_row(tibble::tibble()))

  expect_equal(seasight:::.qs_sa_from_row(tibble::tibble(QS_p = 0.40)), 0.40)
  expect_equal(
    seasight:::.qs_sa_from_row(tibble::tibble(QS_p_x11 = 0.40, QS_p_seats = 0.20)),
    0.20
  )
  expect_equal(seasight:::.lb_from_row(tibble::tibble(lb_p = 0.25)), 0.25)

  hidden <- seasight:::.summarize_for_summary_card(
    tibble::tibble(arima = "(0 1 1)(0 1 1)", lb_p = 0.10),
    list(show_any_model_sections = FALSE)
  )
  expect_equal(hidden$best, "n/a")
  expect_true(is.na(hidden$lb_p))
})

test_that("low-level helper utilities cover edge cases without X-13", {
  y <- fixture_monthly_ts(n = 24L)
  x <- fixture_monthly_regressor(y)
  x_short <- stats::window(x, end = c(2020, 12))

  expect_equal(seasight:::.safe_log_transform(y), "log")
  expect_equal(seasight:::.safe_log_transform(y - 100), "none")
  expect_true(seasight:::.is_seasonal_arima_spec("(0 1 1)(0 1 1)"))
  expect_false(seasight:::.is_seasonal_arima_spec("(1 0 0)(0 0 0)"))
  expect_true("(1 1 0)(0 1 1)" %in% seasight:::.default_specs(12))
  expect_equal(
    seasight:::.extract_arima_str('seasonal::seas(x, arima.model = "(1 1 0)(0 1 1)")'),
    "(1 1 0)(0 1 1)"
  )
  expect_equal(seasight:::.sanitize_arima(NULL), "n/a")

  aligned <- sa_align_regressor(y, x)
  expect_s3_class(aligned, "ts")
  expect_null(sa_align_regressor(y, fixture_quarterly_ts()))
  expect_true(seasight:::.td_is_compatible(y, aligned))

  filled <- seasight:::.align_xreg_to_y_fill0(x_short, y, fill = 0)
  expect_equal(length(filled), length(y))
  expect_equal(as.numeric(filled[length(y)]), 0)

  pair <- seasight:::.align_pair(y, y + 1)
  expect_equal(length(pair$a), length(y))
  expect_null(seasight:::.align_pair(y, fixture_quarterly_ts()))

  norm <- seasight:::.normalize_td_candidates(list(x), y = y, td_usertype = "td")
  expect_equal(names(norm), "td1")
  expect_equal(attr(norm, "td_usertype"), "td")
  expect_error(seasight:::.normalize_td_candidates(list(a = x, a = x), y = y))

  expect_equal(seasight:::.rescale_best_high(c(1, 2, NA)), c(100, 0, NA))
  expect_equal(seasight:::.std01(c(2, 4, NA)), c(0, 1, NA))
  expect_equal(seasight:::.w(list(a = "3"), "a"), 3)
  expect_equal(suppressWarnings(seasight:::.w(list(a = "x"), "a", default = 9)), 9)
  expect_named(seasight:::.sa_pc_stats(c(1, 2, 3, NA)), c("mean", "sd", "median", "min", "max"))
  expect_type(seasight:::.ts_pc_sd(y), "double")
})

test_that("coefficient, formatting, and seasonality helpers are deterministic", {
  fake <- structure(
    list(coefficients = c(a = 1, b = 0), est = list(se = c(a = 0.5, b = 2))),
    class = "lm"
  )

  pvals <- seasight:::.reg_pvals(fake)
  coef_tbl <- seasight:::.coef_table_df(fake)

  expect_equal(pvals$var, c("a", "b"))
  expect_equal(coef_tbl$term, c("a", "b"))
  expect_equal(seasight:::.sig_stars(0.04), "**")
  expect_equal(seasight:::.fmtP(0.0005), "<0.001")
  expect_equal(seasight:::.pct(0.123, digits = 1), "0.1%")

  row <- tibble::tibble(IDS = "yes", QSori_p = 0.01, M7 = 0.80)
  expect_true(seasight:::.flag_tests(row)$seasonal_ids)

  summary <- seasonality_summary(tibble::tibble(
    IDS = c("yes", "no"),
    QSori_p = c(0.01, 0.20),
    M7 = c(0.80, 1.20)
  ))
  expect_equal(summary$call_overall, "ADJUST")
})

test_that("seasonal-backed helpers run on a reference model", {
  skip_if_not_installed("seasonal")

  m <- seasonal::seas(AirPassengers, x11 = "")

  expect_type(seasight:::.m7_stat(m), "double")
  expect_type(seasight:::.ids_flag(m), "character")
  expect_type(seasight:::.lb_p(m), "double")
  expect_named(seasight:::.qs_on_sa_both(m), c("QS_p_x11", "QS_p_seats", "QS_p"))
  expect_named(
    seasight:::.qs_original(m),
    c("QSori_p_x11", "QSori_p_seats", "QSori_p")
  )
  expect_named(seasight:::.qs_x11_sma(m), "QS_p_x11_min_sma")
  expect_true(seasight:::.engine_used(m) %in% c("seats", "x11", "unknown"))
  expect_true(is.na(seasight:::.seats_has_seasonal(m)) ||
    is.logical(seasight:::.seats_has_seasonal(m)))
  expect_type(seasight:::.obs_n(m), "integer")
  expect_type(seasight:::.shapiro_stat(m), "double")
  expect_type(seasight:::.transform_label(m, fallback = "none"), "character")
  expect_named(seasight:::.diagnostics_finance(m, AirPassengers))
  expect_named(seasight:::.revision_mae(m), "rev_mae")
  expect_named(
    seasight:::.dist_vs_baseline(
      m,
      prev_sa = seasonal::final(m),
      prev_seasonal = seasonal::series(m, "x11.seasonal")
    )
  )

  call <- sa_copyable_call(m, x_expr = "AirPassengers", include_force = TRUE)
  expect_match(call, "seasonal::seas|seas")
  expect_match(call, "AirPassengers")
  expect_match(
    seasight:::.copyable_static(m, "AirPassengers", td_expr = NULL),
    "AirPassengers"
  )
})

test_that("outlier extraction and ARIMA display helpers use fallback paths", {
  fake <- structure(
    list(coefficients = c(AO2019.4 = 2, LS2020.1 = -1, ar1 = 0.5)),
    class = c("lm", "seas")
  )

  outliers <- extract_outliers(fake)
  expect_named(outliers, c("type", "period", "coef"))

  row <- tibble::tibble(ARIMA_disp = "", arima = "(2 1 0)(0 1 1)")
  expect_true(is.character(seasight:::.arima_display(row, "bad")))
  expect_equal(
    seasight:::.arima_display(row, 'arima.model = "(0 1 1)(0 1 1)"'),
    "(0 1 1)(0 1 1)"
  )
})

test_that("candidate ordering and alignment helpers expose expected choices", {
  candidates <- tibble::tibble(
    model_label = c("a", "b", "c"),
    rank = c(2, 1, 3),
    is_current = c(TRUE, FALSE, FALSE),
    arima = c("a", "b", "c"),
    with_td = c(FALSE, FALSE, TRUE)
  )

  top <- seasight:::.order_top_candidates(candidates, top_n = 1L)
  expect_true(any(top$is_best))
  expect_true(any(top$is_incumbent))
  expect_true(seasight:::.should_show_alternative(candidates))

  same_rank <- dplyr::mutate(candidates, spec_id = c("a", "b", "c"))
  same_rank$rank <- c(2, 1, 3)
  expect_false(seasight:::.should_show_alternative(
    same_rank,
    incumbent_spec_id = "b"
  ))

  aligned <- seasight:::.align2(fixture_monthly_ts(), fixture_monthly_ts() + 1)
  expect_true(aligned$ok)
  expect_equal(
    seasight:::.align2(fixture_monthly_ts(), fixture_quarterly_ts())$reason,
    "freq_mismatch"
  )
})

test_that("HTML compare and engine cards render from minimal objects", {
  fake_best <- structure(
    list(coefficients = c(a = 1), est = list(se = c(a = 0.5))),
    class = "lm"
  )

  testthat::local_mocked_bindings(
    .qs_overall_on_SA = function(m) 0.20,
    .lb_p = function(m) 0.30,
    .shapiro_stat = function(m) 0.90,
    .transform_label = function(m, fallback = NA_character_) "none",
    .aicc = function(m) 12,
    .obs_n = function(m) 24L,
    .arima_string = function(m) "(0 1 1)(0 1 1)",
    .package = "seasight"
  )

  html <- as.character(seasight:::.build_compare_html(NULL, fake_best))
  expect_match(html, "QS", fixed = TRUE)

  res <- structure(
    list(
      best = fake_best,
      table = tibble::tibble(
        engine = "x11",
        QS_p_x11 = 0.20,
        QS_p_seats = 0.05,
        QSori_p_x11 = 0.01,
        QSori_p_seats = 0.02,
        SEATS_model_switch = TRUE,
        SEATS_has_seasonal = FALSE,
        M7 = 0.8,
        IDS = "yes"
      ),
      seasonality = list(overall = tibble::tibble(call_overall = "ADJUST"))
    ),
    class = "auto_seasonal_analysis"
  )

  expect_s3_class(sa_existence_card(res), "shiny.tag")
  expect_s3_class(sa_engine_choice_card(res), "shiny.tag")
})

test_that("auto workflow can be exercised with mocked model fitting", {
  fake <- structure(list(), class = "seas")
  fit <- function(y, arima_model, transform_fun, td_xreg = NULL,
                  include_easter_mode = "auto", engine = "seats", ...) {
    list(list(
      model = fake,
      with_td = !is.null(td_xreg),
      with_easter = include_easter_mode != "off",
      engine = if (identical(engine, "auto")) "seats" else engine
    ))
  }

  testthat::local_mocked_bindings(
    .fivebest_specs = function(y) character(0),
    .fit_spec = fit,
    .arima_string = function(m) "(0 1 1)(0 1 1)",
    .engine_used = function(m) "seats",
    .m7_stat = function(m) 0.80,
    .ids_flag = function(m) "yes",
    .lb_p = function(m) 0.80,
    .seats_has_seasonal = function(m) TRUE,
    .qs_on_sa_both = function(m) {
      tibble::tibble(QS_p_x11 = 0.20, QS_p_seats = 0.30, QS_p = 0.20)
    },
    .qs_original = function(m) {
      tibble::tibble(
        QSori_p_x11 = 0.01,
        QSori_p_seats = 0.02,
        QSori_p = 0.01
      )
    },
    .qs_x11_sma = function(m) tibble::tibble(QS_p_x11_min_sma = 0.20),
    .diagnostics_finance = function(m, y) {
      tibble::tibble(
        vola_sd_pc_orig = 1,
        vola_sd_pc_sa = 0.5,
        vola_reduction_pct = 50,
        seasonal_amp_abs = 1,
        seasonal_amp_pct = 2
      )
    },
    .dist_vs_baseline = function(...) {
      tibble::tibble(
        dist_sa_L1 = NA_real_,
        dist_seas_RMS = NA_real_,
        corr_seas = NA_real_
      )
    },
    .reg_pvals = function(m) tibble::tibble(var = "td", p = 0.01),
    .revision_mae = function(m) tibble::tibble(rev_mae = 0.10),
    .package = "seasight"
  )

  y <- fixture_monthly_ts(n = 36L)
  td <- list(wd = fixture_monthly_regressor(y))
  res <- auto_seasonal_analysis(
    y,
    specs = "(0 1 1)(0 1 1)",
    use_fivebest = FALSE,
    td_candidates = td,
    max_specs = 1,
    include_history_top_n = 1
  )

  expect_s3_class(res, "auto_seasonal_analysis")
  expect_equal(nrow(res$table), 2)
  expect_true(any(res$table$with_td))
  expect_equal(res$seasonality$overall$call_overall, "ADJUST")
})

test_that("auto workflow returns explicit no-adjust result for empty grid", {
  res <- auto_seasonal_analysis(
    fixture_monthly_ts(),
    specs = "(1 0 0)(0 0 0)",
    use_fivebest = FALSE,
    seasonal_only = TRUE
  )

  expect_s3_class(res, "auto_seasonal_analysis")
  expect_null(res$best)
  expect_equal(res$table$model_label, "DO_NOT_ADJUST")
  expect_equal(res$seasonality$overall$call_overall, "DO_NOT_ADJUST")
})
