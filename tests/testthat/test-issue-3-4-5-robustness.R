test_that(".safe_qs_pval and switch helper are fail-soft on broken seas objects", {
  m <- structure(list(), class = "seas")
  expect_true(is.na(seasight:::.safe_qs_pval(m)))
  expect_true(is.na(seasight:::.has_seats_model_switch_msg(m)))
})

test_that(".align2 handles partial overlap and no-overlap safely", {
  a <- stats::ts(1:12, start = c(2020, 1), frequency = 12)
  b <- stats::ts(1:12, start = c(2020, 6), frequency = 12)
  c <- stats::ts(1:12, start = c(2023, 1), frequency = 12)
  
  expect_silent(al <- seasight:::.align2(a, b))
  expect_true(isTRUE(al$ok))
  expect_equal(length(al$prev), length(al$new))
  expect_true(length(al$prev) > 0)
  
  expect_silent(al2 <- seasight:::.align2(a, c))
  expect_false(isTRUE(al2$ok))
  expect_identical(al2$reason, "no_overlap")
})

test_that("selection rationale uses higher-is-better score wording", {
  res <- list(
    table = tibble::tibble(
      model_label = c("best", "runner_up"),
      arima = c("(0 1 1)(0 1 1)", "(1 1 1)(0 1 1)"),
      with_td = c(TRUE, FALSE),
      score_100 = c(82.3, 77.1),
      AICc = c(100, 101),
      QS_p_x11 = c(0.2, 0.1),
      QS_p_seats = c(0.25, 0.12),
      QS_p = c(0.2, 0.1),
      LB_p = c(0.3, 0.2),
      vola_reduction_pct = c(10, 9),
      seasonal_amp_pct = c(2, 3)
    )
  )
  node <- seasight:::.build_selection_rationale(res, current_model = NULL)
  txt <- paste(as.character(node), collapse = " ")
  expect_match(txt, "highest overall score \\(0-100; higher is better\\)")
})

test_that("top-candidates table exposes score_100 and orders by it descending", {
  res <- list(
    table = tibble::tibble(
      model_label = c("a", "b", "c"),
      arima = c("(0 1 1)(0 1 1)", "(1 1 1)(0 1 1)", "(2 1 1)(0 1 1)"),
      with_td = c(FALSE, TRUE, FALSE),
      score_100 = c(65, 91, 74),
      AICc = c(100, 101, 102),
      LB_p = c(0.2, 0.3, 0.1),
      QSori_p = c(0.2, 0.2, 0.2),
      QS_p_x11 = c(0.2, 0.2, 0.2),
      QS_p_seats = c(0.2, 0.2, 0.2),
      QS_p = c(0.2, 0.2, 0.2),
      td_p = c(NA_real_, 0.04, NA_real_),
      vola_reduction_pct = c(10, 12, 9),
      seasonal_amp_pct = c(2, 2, 2),
      dist_sa_L1 = c(1, 2, 3),
      rev_mae = c(0.1, 0.2, 0.3)
    )
  )
  tag <- seasight:::sa_top_candidates_table(res, n = 3)
  html <- as.character(tag)
  expect_match(html, "Score \\(0-100\\)")
  expect_lt(regexpr(">b<", html), regexpr(">c<", html))
  expect_lt(regexpr(">c<", html), regexpr(">a<", html))
})
