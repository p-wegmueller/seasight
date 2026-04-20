# Late-loaded report/UI helpers.

P <- function(x) .fmtP(x)
esc <- htmltools::htmlEscape

t_safe <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.data.frame(x)) x <- as.matrix(x)
  if (is.null(dim(x))) return(x)
  tryCatch(t(x), error = function(e) x)
}

.report_get <- function(row, nm, default = NA) {
  if (!nm %in% names(row)) return(default)
  v <- row[[nm]]
  if (!length(v)) default else v[[1]]
}

.report_chr <- function(x, default = "") {
  x <- as.character(x %||% default)
  x[is.na(x)] <- default
  x
}

.report_num <- function(x, digits = 1) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.finite(x), sprintf(paste0("%.", digits, "f"), x), "-")
}

.report_p <- function(x) .fmtP(suppressWarnings(as.numeric(x)))

.report_norm <- function(x) gsub("\\s+", " ", trimws(ifelse(is.na(x), "", as.character(x))))

.report_td_label <- function(row) {
  with_td <- isTRUE(suppressWarnings(as.logical(.report_get(row, "with_td", FALSE))))
  if (!with_td) return("no")
  lab <- .report_chr(.report_get(row, "td_label", .report_get(row, "td_name", "")))
  if (nzchar(lab)) lab else "yes"
}

.report_arima <- function(row) {
  out <- tryCatch(.coalesce_arima_str(row), error = function(e) NA_character_)
  if (is.character(out) && length(out) == 1L && !is.na(out) && nzchar(out)) return(out)
  out <- .report_chr(.report_get(row, "arima", .report_get(row, "ARIMA_disp", "n/a")), "n/a")
  if (nzchar(out)) out else "n/a"
}

.report_score_100 <- function(tbl) {
  if ("score_100" %in% names(tbl)) return(suppressWarnings(as.numeric(tbl$score_100)))
  score <- if ("score_raw" %in% names(tbl)) tbl$score_raw else if ("score" %in% names(tbl)) tbl$score else NULL
  if (!is.null(score)) return(.rescale_best_high(score))
  rank <- if ("rank" %in% names(tbl)) suppressWarnings(as.numeric(tbl$rank)) else seq_len(nrow(tbl))
  .rescale_best_high(rank)
}

.build_top_candidates_table <- function(res, current_model = NULL, y = NULL, n = 5) {
  stopifnot(inherits(res, "auto_seasonal_analysis"), is.data.frame(res$table), nrow(res$table) >= 1)

  tbl <- tryCatch(.coalesce_arima_cols(res$table), error = function(e) tibble::as_tibble(res$table))
  tbl$score_100 <- .report_score_100(tbl)
  tbl$arima <- vapply(seq_len(nrow(tbl)), function(i) .report_arima(tbl[i, , drop = FALSE]), character(1))

  disp_cols <- c(
    "model_label", "arima", "with_td", "td_name", "td_label", "score_100",
    "AICc", "LB_p", "QSori_p", "QS_p_x11", "QS_p_seats", "QS_p",
    "td_p", "vola_reduction_pct", "seasonal_amp_pct", "dist_sa_L1", "rev_mae"
  )

  top <- tbl |>
    dplyr::select(dplyr::any_of(disp_cols)) |>
    dplyr::slice(seq_len(min(as.integer(n), nrow(tbl))))

  airline_arima <- "(0 1 1)(0 1 1)"
  if (!any(.report_norm(top$arima) == .report_norm(airline_arima), na.rm = TRUE)) {
    air <- tbl[.report_norm(tbl$arima) == .report_norm(airline_arima), , drop = FALSE]
    if (nrow(air)) {
      top <- dplyr::bind_rows(top, dplyr::select(air[1, , drop = FALSE], dplyr::any_of(disp_cols)))
    }
  }

  prev_arima <- NULL
  if (!is.null(current_model)) {
    prev_arima <- tryCatch(.arima_string(current_model), error = function(e) NULL)
    if (!is.null(prev_arima) && !any(.report_norm(top$arima) == .report_norm(prev_arima), na.rm = TRUE)) {
      qb <- tryCatch(.qs_on_sa_both(current_model), error = function(e) tibble::tibble(QS_p_x11 = NA_real_, QS_p_seats = NA_real_, QS_p = NA_real_))
      qo <- tryCatch(.qs_original(current_model), error = function(e) tibble::tibble(QSori_p = NA_real_))
      prev_row <- tibble::tibble(
        model_label = "current",
        arima = .report_norm(prev_arima),
        with_td = FALSE,
        td_name = NA_character_,
        td_label = NA_character_,
        score_100 = NA_real_,
        AICc = tryCatch(.aicc(current_model), error = function(e) NA_real_),
        LB_p = tryCatch(.lb_p(current_model), error = function(e) NA_real_)
      ) |>
        dplyr::bind_cols(qo, qb) |>
        dplyr::mutate(td_p = NA_real_, vola_reduction_pct = NA_real_, seasonal_amp_pct = NA_real_,
                      dist_sa_L1 = NA_real_, rev_mae = NA_real_) |>
        dplyr::select(dplyr::any_of(disp_cols))
      top <- dplyr::bind_rows(top, prev_row)
    }
  }

  n_top <- nrow(top)
  best_label <- if ("model_label" %in% names(tbl)) tbl$model_label[1] else NA_character_
  top$is_best <- if ("model_label" %in% names(top) && !is.na(best_label)) top$model_label == best_label else seq_len(n_top) == 1L
  has_prev_label <- if ("model_label" %in% names(top)) top$model_label == "current" else rep(FALSE, n_top)
  prev_match <- if (!is.null(prev_arima)) .report_norm(top$arima) == .report_norm(prev_arima) else rep(FALSE, n_top)
  top$is_prev <- has_prev_label | prev_match
  top$is_airline <- .report_norm(top$arima) == .report_norm(airline_arima)

  header <- htmltools::tags$tr(lapply(
    c("Label", "ARIMA", "Overall score", "TD regressor", "AICc", "LB p", "QSori p",
      "QS X-11 p", "QS SEATS p", "QS min", "TD p", "Volatility red. %",
      "Seasonal amp %", "L1 vs prev SA", "Rev. MAE"),
    htmltools::tags$th
  ))

  body_rows <- lapply(seq_len(n_top), function(i) {
    r <- top[i, , drop = FALSE]
    row_class <- if (isTRUE(r$is_best)) "row-best" else if (isTRUE(r$is_prev)) "row-prev" else if (isTRUE(r$is_airline)) "row-airline" else ""
    htmltools::tags$tr(
      class = row_class,
      htmltools::tags$td(esc(.report_chr(.report_get(r, "model_label", "")))),
      htmltools::tags$td(htmltools::tags$span(class = "model-spec", esc(.report_arima(r)))),
      htmltools::tags$td(.report_num(.report_get(r, "score_100"), 1)),
      htmltools::tags$td(esc(.report_td_label(r))),
      htmltools::tags$td(.report_num(.report_get(r, "AICc"), 2)),
      htmltools::tags$td(.report_p(.report_get(r, "LB_p"))),
      htmltools::tags$td(.report_p(.report_get(r, "QSori_p"))),
      htmltools::tags$td(.report_p(.report_get(r, "QS_p_x11"))),
      htmltools::tags$td(.report_p(.report_get(r, "QS_p_seats"))),
      htmltools::tags$td(.report_p(.report_get(r, "QS_p"))),
      htmltools::tags$td(.report_p(.report_get(r, "td_p"))),
      htmltools::tags$td(.report_num(.report_get(r, "vola_reduction_pct"), 1)),
      htmltools::tags$td(.report_num(.report_get(r, "seasonal_amp_pct"), 2)),
      htmltools::tags$td(.report_num(.report_get(r, "dist_sa_L1"), 2)),
      htmltools::tags$td(.report_num(.report_get(r, "rev_mae"), 3))
    )
  })

  styles <- htmltools::tags$style(htmltools::HTML(
    ".tbl-colored .row-airline td { background:#fff1f2; }
     .chip-airline{ background:#fee2e2 !important; color:#991b1b !important; border:1px solid #fecaca !important; }
     .tbl-colored th, .tbl-colored td { vertical-align: middle; }
     .tbl-colored th:nth-child(2), .tbl-colored td:nth-child(2){ min-width:180px; max-width:320px; }
     .tbl-colored .model-spec { white-space:normal; overflow-wrap:anywhere; }"
  ))

  legend <- htmltools::div(
    class = "legend",
    htmltools::span(class = "chip chip-best", "best"),
    htmltools::span(class = "chip chip-prev", "current"),
    htmltools::span(class = "chip chip-airline", "Airline model")
  )

  foot <- htmltools::tags$p(
    class = "tbl-note",
    htmltools::HTML(paste0(
      "Overall score is normalized to 0-100; higher is better. Showing ",
      min(as.integer(n), nrow(tbl)), " of ", nrow(tbl),
      " ranked candidates; current and airline reference rows may be added when relevant."
    ))
  )

  htmltools::tagList(styles, htmltools::tags$table(class = "tbl tbl-colored", htmltools::tags$thead(header), htmltools::tags$tbody(body_rows)), legend, foot)
}

.build_selection_rationale <- function(res, current_model = NULL, override_best_arima = NULL) {
  get1 <- function(row, nm, default = NA) .report_get(row, nm, default)
  N <- function(x, d = 2) .report_num(x, d)

  tbl <- tryCatch(.coalesce_arima_cols(res$table), error = function(e) tibble::as_tibble(res$table))
  br <- dplyr::slice(tbl, 1)
  sr <- if (nrow(tbl) >= 2) dplyr::slice(tbl, 2) else NULL

  arima_best <- if (!is.null(override_best_arima) && nzchar(override_best_arima)) override_best_arima else .report_arima(br)
  with_td <- isTRUE(suppressWarnings(as.logical(get1(br, "with_td", FALSE))))
  td_txt <- if (with_td) paste0("with TD regressor ", esc(.report_td_label(br))) else "without TD"

  score <- suppressWarnings(as.numeric(get1(br, "score_100", NA_real_)))
  if (!is.finite(score)) score <- .report_score_100(br)[1]
  runner_score <- if (!is.null(sr)) suppressWarnings(as.numeric(get1(sr, "score_100", NA_real_))) else NA_real_
  score_gap <- if (is.finite(score) && is.finite(runner_score)) score - runner_score else NA_real_

  existence <- tryCatch(.existence_call_ui(res)$call, error = function(e) NA_character_)
  dec <- tryCatch(.compose_decision(existence, br, has_current = !is.null(current_model)), error = function(e) NULL)
  switch_decision <- if (!is.null(current_model) && !is.null(dec) && identical(dec$decision, "ADJUST")) {
    tryCatch(sa_should_switch(res), error = function(e) NA_character_)
  } else {
    if (!is.null(dec)) dec$decision else NA_character_
  }
  gate_reason <- if (!is.null(dec) && !identical(dec$decision, "ADJUST")) dec$reason else NA_character_
  if (!is.null(current_model) && identical(switch_decision, "KEEP_CURRENT_MODEL") && is.na(gate_reason)) {
    gate_reason <- "The switch gate recommends keeping the current model."
  }

  qs_x11 <- get1(br, "QS_p_x11")
  qs_seats <- get1(br, "QS_p_seats")
  qs_min <- get1(br, "QS_p")
  lb_p <- get1(br, "LB_p")
  vola_red <- get1(br, "vola_reduction_pct")
  seas_amp <- get1(br, "seasonal_amp_pct")

  htmltools::tagList(
    if (!is.na(gate_reason) && nzchar(gate_reason)) htmltools::tags$p(
      htmltools::HTML(paste0("<b>Decision gate.</b> ", esc(switch_decision), ": ", esc(gate_reason)))
    ) else NULL,
    htmltools::tags$p(
      htmltools::HTML(paste0(
        "<b>Selection rationale.</b> The chosen specification is <code>ARIMA ",
        esc(arima_best), "</code> ", td_txt,
        ", selected because it achieved the <b>highest overall score</b> among candidates ",
        "(0-100; higher is better)",
        if (is.finite(score)) paste0("; score = ", N(score, 1)) else "",
        if (is.finite(score_gap)) paste0("; margin vs. next best = ", N(score_gap, 1)) else "",
        ". The score combines residual seasonality, residual autocorrelation, AICc, revision behavior, engine preference and distance to the incumbent where available."
      ))
    ),
    htmltools::tags$p(
      htmltools::HTML(paste0(
        "Diagnostics for the winner: QS(X-11) p = ", P(qs_x11),
        ", QS(SEATS) p = ", P(qs_seats),
        ", overall QS = ", P(qs_min), "; Ljung-Box p = ", P(lb_p),
        "; volatility reduction = ", N(vola_red, 1), "%; seasonal amplitude = ",
        N(seas_amp, 2), "%."
      ))
    ),
    if (!is.null(sr)) htmltools::tags$p(
      htmltools::HTML(paste0(
        "Runner-up: <code>ARIMA ", esc(.report_arima(sr)), "</code> ",
        if (isTRUE(suppressWarnings(as.logical(get1(sr, "with_td", FALSE))))) paste0("with TD regressor ", esc(.report_td_label(sr))) else "without TD",
        "."
      ))
    ) else NULL
  )
}
