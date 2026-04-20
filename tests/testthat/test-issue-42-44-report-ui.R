test_that("sa_top_candidates_table reports hidden lower-ranked candidates", {
  tbl <- tibble::tibble(
    model_label = c("best", "m2", "m3", "m4"),
    arima = c("(0 1 1)(0 1 1)", "(1 1 1)(0 1 1)", "(0 1 1)(1 1 0)", "(2 1 0)(0 1 1)"),
    with_td = c(TRUE, FALSE, TRUE, FALSE),
    score_100 = c(95, 90, 80, 70),
    AICc = c(100, 101, 102, 103),
    LB_p = c(0.4, 0.3, 0.2, 0.1),
    QSori_p = c(0.2, 0.2, 0.2, 0.2),
    QS_p_x11 = c(0.2, 0.2, 0.2, 0.2),
    QS_p_seats = c(0.2, 0.2, 0.2, 0.2),
    QS_p = c(0.2, 0.2, 0.2, 0.2),
    td_p = c(0.04, NA_real_, 0.20, NA_real_),
    vola_reduction_pct = c(10, 9, 8, 7),
    seasonal_amp_pct = c(5, 4, 3, 2),
    dist_sa_L1 = c(NA_real_, NA_real_, NA_real_, NA_real_),
    rev_mae = c(0.10, 0.11, 0.12, 0.13)
  )

  node <- sa_top_candidates_table(list(table = tbl), n = 2)
  html <- htmltools::renderTags(node)$html

  expect_match(html, "Showing top <b>2</b> of <b>4</b>", fixed = TRUE)
  expect_match(html, "lower-ranked candidates are hidden for readability", fixed = TRUE)
})

test_that("sa_top_candidates_table injects shared chip CSS classes", {
  tbl <- tibble::tibble(
    model_label = c("best", "m2"),
    arima = c("(0 1 1)(0 1 1)", "(1 1 1)(0 1 1)"),
    with_td = c(TRUE, FALSE),
    score_100 = c(95, 90)
  )

  node <- sa_top_candidates_table(list(table = tbl), n = 2)
  html <- htmltools::renderTags(node)$html

  expect_match(html, "\\.chip-best", perl = TRUE)
  expect_match(html, "\\.chip-prev", perl = TRUE)
})
