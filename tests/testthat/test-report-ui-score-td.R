test_that("small report UI helpers are fail-soft", {
  expect_null(seasight:::t_safe(NULL))
  expect_equal(seasight:::t_safe(1:3), 1:3)
  expect_equal(seasight:::t_safe(matrix(1:4, nrow = 2)), t(matrix(1:4, nrow = 2)))
  expect_equal(seasight:::P(NA_real_), "\u2014")
  expect_equal(as.character(seasight:::esc("<x>")), "&lt;x&gt;")
})

test_that("top candidates table displays score_100 and TD labels", {
  tbl <- tibble::tibble(
    model_label = c("best", "runner"),
    arima = c("(0 1 1)(0 1 1)", "(1 1 1)(0 1 1)"),
    with_td = c(TRUE, FALSE),
    td_name = c("wd", NA_character_),
    score_100 = c(96.5, 81.2),
    AICc = c(100, 102),
    LB_p = c(0.30, 0.20),
    QSori_p = c(0.01, 0.02),
    QS_p_x11 = c(0.30, 0.20),
    QS_p_seats = c(0.40, 0.25),
    QS_p = c(0.30, 0.20),
    td_p = c(0.03, NA_real_),
    vola_reduction_pct = c(12, 10),
    seasonal_amp_pct = c(3, 2),
    dist_sa_L1 = c(NA_real_, NA_real_),
    rev_mae = c(NA_real_, NA_real_)
  )
  res <- structure(list(table = tbl), class = "auto_seasonal_analysis")

  html <- paste(as.character(htmltools::renderTags(seasight:::.build_top_candidates_table(res, n = 2))$html), collapse = "\n")
  expect_match(html, "Score (0-100)", fixed = TRUE)
  expect_match(html, "96.5", fixed = TRUE)
  expect_match(html, "wd", fixed = TRUE)
  expect_match(html, "0-100; higher is better", fixed = TRUE)
})

test_that("selection rationale uses higher-is-better wording and gates first", {
  tbl <- tibble::tibble(
    model_label = c("best", "runner"),
    arima = c("(0 1 1)(0 1 1)", "(1 1 1)(0 1 1)"),
    with_td = c(TRUE, FALSE),
    td_name = c("wd", NA_character_),
    score_100 = c(90, 80),
    QS_p = c(0.01, 0.20),
    QS_p_x11 = c(0.01, 0.20),
    QS_p_seats = c(0.02, 0.25),
    LB_p = c(0.40, 0.30),
    vola_reduction_pct = c(5, 4),
    seasonal_amp_pct = c(1, 1),
    AICc = c(100, 101)
  )
  res <- structure(
    list(table = tbl, seasonality = list(overall = tibble::tibble(call_overall = "ADJUST"))),
    class = "auto_seasonal_analysis"
  )

  html <- paste(as.character(htmltools::renderTags(seasight:::.build_selection_rationale(res))$html), collapse = "\n")
  expect_match(html, "Decision gate", fixed = TRUE)
  expect_match(html, "highest overall score", fixed = TRUE)
  expect_match(html, "0-100; higher is better", fixed = TRUE)
  expect_match(html, "TD regressor wd", fixed = TRUE)
})
