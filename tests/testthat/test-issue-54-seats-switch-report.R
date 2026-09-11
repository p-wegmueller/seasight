switch_result <- function(engine = "seats", flag = TRUE,
                          operative = "(0 1 1)(0 0 1)") {
  tbl <- tibble::tibble(
    model_label = "best",
    arima = "(0 1 1)(0 1 1)",
    engine = engine,
    SEATS_model_switch = flag,
    SEATS_operative_model = operative,
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
    seasonal_amp_pct = 3,
    dist_sa_L1 = NA_real_,
    rev_mae = NA_real_
  )
  structure(
    list(
      best = NULL,
      y = AirPassengers,
      table = tbl,
      frequency = 12,
      transform = "log",
      baseline = list(current_sa = NULL, diagnostics = NULL),
      seasonality = list(overall = tibble::tibble(call_overall = "ADJUST"))
    ),
    class = "auto_seasonal_analysis"
  )
}

test_that("SEATS switch warnings are engine-independent and tri-state", {
  for (engine in c("seats", "x11")) {
    for (flag in list(TRUE, FALSE, NA)) {
      res <- switch_result(engine = engine, flag = flag)
      engine_html <- paste(as.character(htmltools::renderTags(
        seasight:::.build_engine_choice_card(res)
      )$html), collapse = "\n")
      summary_tag <- seasight:::.seats_switch_warning_tag(
        res, location = "summary"
      )

      if (isTRUE(flag)) {
        expect_match(engine_html, "SEATS model-switch warning", fixed = TRUE)
        expect_match(engine_html, "Requested ARIMA", fixed = TRUE)
        expect_match(engine_html, "operative SEATS model", fixed = TRUE)
        expect_s3_class(summary_tag, "shiny.tag")
      } else {
        expect_no_match(engine_html, "SEATS model-switch warning", fixed = TRUE)
        expect_null(summary_tag)
      }
    }
  }
})

test_that("missing model details do not hide a switch warning", {
  res <- switch_result(flag = TRUE, operative = NA_character_)
  html <- paste(as.character(htmltools::renderTags(
    seasight:::.build_engine_choice_card(res)
  )$html), collapse = "\n")

  expect_match(html, "SEATS model-switch warning", fixed = TRUE)
  expect_match(
    html,
    "operative SEATS model:[[:space:]]*<code>n/a</code>",
    perl = TRUE
  )
})

test_that("top candidates expose stable switch and operative-model columns", {
  tbl <- dplyr::bind_rows(lapply(c("seats", "x11"), function(engine) {
    dplyr::bind_rows(lapply(seq_along(list(TRUE, FALSE, NA)), function(i) {
      flag <- list(TRUE, FALSE, NA)[[i]]
      switch_result(engine = engine, flag = flag)$table |>
        dplyr::mutate(
          model_label = paste(engine, i, sep = "-"),
          arima = paste0("(", i - 1L, " 1 1)(0 1 1)"),
          score_100 = 100 - i
        )
    }))
  }))
  res <- structure(list(table = tbl), class = "auto_seasonal_analysis")
  html <- paste(as.character(htmltools::renderTags(
    sa_top_candidates_table(res, n = 6)
  )$html), collapse = "\n")

  expect_match(html, "Requested ARIMA", fixed = TRUE)
  expect_match(html, "Operative SEATS model", fixed = TRUE)
  expect_match(html, "SEATS switch", fixed = TRUE)
  expect_equal(length(gregexpr("<td>yes</td>", html, fixed = TRUE)[[1]]), 2L)
  expect_equal(length(gregexpr("<td>no</td>", html, fixed = TRUE)[[1]]), 2L)
  expect_equal(length(gregexpr("<td>n/a</td>", html, fixed = TRUE)[[1]]), 2L)
})

test_that("operative SEATS model is extracted from X-13 output", {
  model <- seasonal::seas(
    AirPassengers,
    arima.model = "(0 1 1)(0 1 1)",
    regression.aictest = NULL,
    outlier = NULL
  )
  expected <- seasonal::udg(model, "seatsmdl", fail = FALSE)

  expect_identical(
    seasight:::.seats_model_used(model),
    unname(as.character(expected[[1]]))
  )
})

test_that("a flagged SEATS winner is warned in summary and engine cards", {
  res <- switch_result(engine = "seats", flag = TRUE)
  res$seasonality$overall$call_overall <- "DO_NOT_ADJUST"
  out <- tempfile(fileext = ".html")

  expect_no_error(
    invisible(capture.output(sa_report_html(y = res, outfile = out)))
  )
  html <- paste(readLines(out, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  warning_count <- lengths(regmatches(
    html, gregexpr("SEATS model-switch warning", html, fixed = TRUE)
  ))

  expect_equal(warning_count, 2L)
  expect_match(html, "The selected SEATS decomposition uses a substituted model", fixed = TRUE)
})
