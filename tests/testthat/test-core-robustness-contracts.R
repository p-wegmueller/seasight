test_that("TD candidate normalization has a strict named-list contract", {
  y <- ts(seq_len(24), start = c(2020, 1), frequency = 12)
  x <- ts(rep(1, 24), start = c(2020, 1), frequency = 12)

  expect_null(seasight:::.normalize_td_candidates(NULL, y))
  expect_error(seasight:::.normalize_td_candidates(list(), y), "must not be an empty list")
  expect_error(seasight:::.normalize_td_candidates(list(NULL), y), "must not be NULL")

  out <- seasight:::.normalize_td_candidates(list(x, named = x), y, td_usertype = "holiday")
  expect_equal(names(out), c("td1", "named"))
  expect_equal(attr(out, "td_usertype"), "holiday")
  expect_equal(length(out), 2L)
  expect_s3_class(out[[1]], "ts")
  expect_equal(stats::start(out[[1]]), stats::start(y))
  expect_equal(stats::end(out[[1]]), stats::end(y))
})

test_that("sa_align_regressor aligns by frequency and target span", {
  y <- ts(seq_len(24), start = c(2020, 1), frequency = 12)
  x_long <- ts(seq_len(48), start = c(2019, 1), frequency = 12)
  x_bad_freq <- ts(seq_len(8), start = c(2020, 1), frequency = 4)

  aligned <- sa_align_regressor(y, x_long)
  expect_s3_class(aligned, "ts")
  expect_equal(NROW(aligned), length(y))
  expect_equal(stats::frequency(aligned), stats::frequency(y))
  expect_equal(stats::start(aligned), stats::start(y))
  expect_equal(stats::end(aligned), stats::end(y))

  expect_null(sa_align_regressor(y, x_bad_freq))
})

test_that(".align2 is fail-soft for partial and impossible overlap", {
  a <- ts(seq_len(24), start = c(2020, 1), frequency = 12)
  b <- ts(seq_len(12), start = c(2020, 7), frequency = 12)
  c <- ts(seq_len(12), start = c(2030, 1), frequency = 12)
  q <- ts(seq_len(8), start = c(2020, 1), frequency = 4)

  partial <- seasight:::.align2(a, b)
  expect_true(partial$ok)
  expect_equal(length(partial$prev), 12L)
  expect_equal(length(partial$new), 12L)

  no_overlap <- seasight:::.align2(a, c)
  expect_false(no_overlap$ok)
  expect_equal(no_overlap$reason, "no_overlap")

  freq_mismatch <- seasight:::.align2(a, q)
  expect_false(freq_mismatch$ok)
  expect_equal(freq_mismatch$reason, "freq_mismatch")
})

test_that("QS and summary-free diagnostic helpers are schema-stable", {
  m <- structure(list(), class = "seas")

  expect_silent(qs_sa <- seasight:::.qs_on_sa_both(m))
  expect_silent(qs_ori <- seasight:::.qs_original(m))
  expect_silent(qs_sma <- seasight:::.qs_x11_sma(m))
  expect_equal(names(qs_sa), c("QS_p_x11", "QS_p_seats", "QS_p"))
  expect_equal(names(qs_ori), c("QSori_p_x11", "QSori_p_seats", "QSori_p"))
  expect_equal(names(qs_sma), "QS_p_x11_min_sma")
  expect_equal(nrow(qs_sa), 1L)
  expect_equal(nrow(qs_ori), 1L)
  expect_equal(nrow(qs_sma), 1L)

  expect_silent(z <- seasight:::.has_seats_model_switch_msg(m))
  expect_true(is.na(z) || is.logical(z))
  expect_true(is.na(seasight:::.obs_n(m)))
  expect_true(is.na(seasight:::.shapiro_stat(m)))
})
