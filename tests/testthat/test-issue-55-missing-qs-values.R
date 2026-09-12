qs_card_result <- function(engine = "seats") {
  structure(
    list(
      best = NULL,
      table = tibble::tibble(
        engine = engine,
        QS_p_x11 = 0.2345,
        QS_p_seats = 0.3456,
        QSori_p_x11 = 0.0123,
        QSori_p_seats = 0.0234,
        SEATS_model_switch = FALSE,
        SEATS_has_seasonal = TRUE,
        M7 = 0.8,
        IDS = "yes"
      ),
      seasonality = list(overall = tibble::tibble(call_overall = "ADJUST"))
    ),
    class = "auto_seasonal_analysis"
  )
}

set_missing_qs <- function(res, kind) {
  if (identical(kind, "missing")) {
    res$table <- dplyr::select(
      res$table, -dplyr::any_of(c("QS_p_x11", "QSori_p_x11"))
    )
    return(res)
  }
  value <- switch(
    kind,
    null = NULL,
    zero_length = numeric(0),
    na = NA_real_,
    nan = NaN,
    infinite = Inf
  )
  res$table$QS_p_x11 <- list(value)
  res$table$QSori_p_x11 <- list(value)
  res
}

test_that("missing QS forms render explicitly in both cards and engines", {
  kinds <- c("missing", "null", "zero_length", "na", "nan", "infinite")

  for (engine in c("seats", "x11")) {
    for (kind in kinds) {
      res <- set_missing_qs(qs_card_result(engine), kind)
      existence_html <- paste(as.character(htmltools::renderTags(
        sa_existence_card(res)
      )$html), collapse = "\n")
      engine_html <- paste(as.character(htmltools::renderTags(
        sa_engine_choice_card(res)
      )$html), collapse = "\n")

      expect_match(existence_html, "X-11 p = —", fixed = TRUE)
      expect_match(engine_html, "X-11 p = —", fixed = TRUE)
      expect_no_match(
        paste(existence_html, engine_html),
        "p =[[:space:]]*(,|\\.|</li>)",
        perl = TRUE
      )
    }
  }
})

test_that("fully populated cards retain three-decimal p-value formatting", {
  for (engine in c("seats", "x11")) {
    res <- qs_card_result(engine)
    existence_html <- paste(as.character(htmltools::renderTags(
      sa_existence_card(res)
    )$html), collapse = "\n")
    engine_html <- paste(as.character(htmltools::renderTags(
      sa_engine_choice_card(res)
    )$html), collapse = "\n")

    expect_match(existence_html, "X-11 p = 0.012", fixed = TRUE)
    expect_match(existence_html, "SEATS p = 0.023", fixed = TRUE)
    expect_match(engine_html, "X-11 p = 0.234", fixed = TRUE)
    expect_match(engine_html, "SEATS p = 0.346", fixed = TRUE)
  }
})

test_that("report scalar accessors fail soft", {
  expect_identical(seasight:::.report_get(list(), "x", "fallback"), "fallback")
  expect_identical(seasight:::.report_get(list(x = numeric(0)), "x", 7), 7)

  broken <- new.env(parent = emptyenv())
  makeActiveBinding("x", function(value) stop("cannot extract"), broken)
  expect_identical(seasight:::.report_get(broken, "x", "fallback"), "fallback")

  missing_values <- list(NULL, numeric(0), NA_real_, NaN, Inf, -Inf, "not numeric")
  for (value in missing_values) {
    expect_identical(seasight:::.report_p(value), "—")
  }
})
