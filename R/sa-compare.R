#' Should we switch to the new best model?
#'
#' @param res Result of [auto_seasonal_analysis()].
#' @param thresholds Named list with decision thresholds:
#'   - min_qs_p: minimum acceptable QS p-value on SA (overall) for the best model
#'   - max_dist_sa_mult: allow SA L1 distance up to this multiple of the cross-candidate median
#'   - min_corr_seas: minimum correlation of seasonal components (vs. incumbent)
#'   - min_lb_p: minimum acceptable Ljung-Box p-value on residuals
#' @return One of "CHANGE_TO_NEW_MODEL" or "KEEP_CURRENT_MODEL".
#' @export
sa_should_switch <- function(res,
                             thresholds = list(min_qs_p = 0.10,
                                               max_dist_sa_mult = 1.25,
                                               min_corr_seas = 0.90,
                                               min_lb_p = 0.05)) {
  stopifnot(inherits(res, "auto_seasonal_analysis"))
  br <- dplyr::slice(res$table, 1)
  
  # extract thresholds safely (provide defaults if unnamed / missing)
  tz <- function(nm, def) {
    v <- tryCatch(thresholds[[nm]], error = function(e) NA_real_)
    if (!is.finite(as.numeric(v))) def else as.numeric(v)
  }
  thr_qs     <- tz("min_qs_p",       0.10)
  thr_dist_m <- tz("max_dist_sa_mult", 1.25)
  thr_corr   <- tz("min_corr_seas",  0.90)
  thr_lb     <- tz("min_lb_p",       0.05)
  
  # QS on SA (overall): higher is better
  ok_qs <- is.finite(br$QS_p) && br$QS_p >= thr_qs
  
  # Ljung-Box: if missing (rare), do not block a switch
  ok_lb <- if (is.finite(br$LB_p)) br$LB_p >= thr_lb else TRUE
  
  # SA distance vs median: ignore criterion if no distances exist or current distance is NA
  meddist <- suppressWarnings(stats::median(res$table$dist_sa_L1[is.finite(res$table$dist_sa_L1)], na.rm = TRUE))
  if (!is.finite(meddist)) {
    ok_dist <- TRUE
  } else if (!is.finite(br$dist_sa_L1)) {
    ok_dist <- TRUE
  } else {
    ok_dist <- br$dist_sa_L1 <= (meddist * thr_dist_m)
  }
  
  # Seasonal correlation vs incumbent: OK if absent/NA (no incumbent or no overlap)
  ok_corr <- is.na(br$corr_seas) || br$corr_seas >= thr_corr
  
  if (ok_qs && ok_lb && ok_dist && ok_corr) "CHANGE_TO_NEW_MODEL" else "KEEP_CURRENT_MODEL"
}


#' Existence-of-seasonality narrative (HTML tag)
#'
#' Uses IDS, M7, QS on original, and presence/absence of a SEATS seasonal component
#' to provide a short textual assessment of seasonality for the best model.
#'
#' @param res Result of [auto_seasonal_analysis()].
#' @return An htmltools tag representing the card.
#' @export
sa_existence_card <- function(res) .build_existence_card(res)


.build_existence_card <- function(res) {
  stopifnot(inherits(res, "auto_seasonal_analysis"))
  br   <- dplyr::slice(res$table, 1)
  ui_call <- tryCatch(.existence_call_ui(res), error = function(e) list(call = "\u2014", note = NULL))
  call <- ui_call$call
  
  # M7 interpretation (X-11 rule of thumb)
  m7_txt <- if (is.na(br$M7)) "n/a" else if (br$M7 < 0.90) "clear seasonality"
  else if (br$M7 < 1.05) "weak seasonality" else "no seasonality"
  
  # IDS tolerant rendering
  ids_txt <- if (is.na(br$IDS) || br$IDS == "") "n/a" else as.character(br$IDS)
  
  # QS on original (existence-of-seasonality)
  qso_min <- suppressWarnings(pmin(br$QSori_p_x11, br$QSori_p_seats, na.rm = TRUE))
  if (!is.finite(qso_min)) qso_min <- NA_real_
  
  # SEATS seasonal component presence
  seats_has <- br$SEATS_has_seasonal
  seats_txt <- if (isTRUE(seats_has)) "present" else if (identical(seats_has, FALSE)) "absent" else "n/a"
  
  # One-sentence conclusion
  lead <- htmltools::HTML(
    paste0("<b>Existence of seasonality:</b> <span class='pill'>", call,
           "</span> \u2014 based on IDS, M7 and QS on the original series.")
  )
  
  bullets <- htmltools::tags$ul(
    htmltools::tags$li(htmltools::HTML(
      paste0("<b>IDS (ONS 'identifiable seasonality')</b>: ", ids_txt, ".")
    )),
    htmltools::tags$li(htmltools::HTML(
      paste0("<b>M7 (X-11)</b>: ", .num(br$M7, 3), " \u2192 ", m7_txt, ".")
    )),
    htmltools::tags$li(htmltools::HTML(
      paste0("<b>QS on original</b>: X-11 p = ", .fmtP(br$QSori_p_x11),
             ", SEATS p = ", .fmtP(br$QSori_p_seats),
             " \u2192 overall = ", .fmtP(qso_min), ".")
    )),
    htmltools::tags$li(htmltools::HTML(
      paste0("<b>SEATS seasonal component</b>: ", seats_txt, ".")
    ))
  )
  
  nuance <- switch(
    call,
    "DO_NOT_ADJUST" = "All tests point against material seasonality (or the signal is too weak to justify adjustment). We recommend not to seasonally adjust.",
    "BORDERLINE"    = "Evidence is mixed or weak. Consider business context and visual diagnostics before deciding to adjust.",
    "ADJUST"        = "At least two test families indicate seasonality. Proceed with seasonal adjustment.",
    # default
    "Evidence is mixed; consider visual diagnostics."
  )
  
  if (!is.null(ui_call$note) && nzchar(ui_call$note)) {
    nuance <- paste0(nuance, " ", ui_call$note)
  }
  
  htmltools::div(
    class = "card",
    htmltools::tags$h2("Existence of Seasonality"),
    htmltools::tags$p(lead),
    bullets,
    htmltools::tags$p(class = "sub", nuance)
  )
}


# --- Compare incumbent vs new best --------------------------------------------

# local AICc helper (safe across engines)
.aicc <- function(m) {
  aic <- suppressWarnings(tryCatch(stats::AIC(m), error = function(e) NA_real_))
  k   <- suppressWarnings(tryCatch(length(stats::coef(m)), error = function(e) NA_integer_))
  n   <- suppressWarnings(tryCatch(length(stats::na.omit(seasonal::original(m))), error = function(e) NA_integer_))
  if (!is.finite(aic) || !is.finite(k) || !is.finite(n) || n <= (k + 1)) return(aic)
  aic + (2 * k * (k + 1)) / (n - k - 1)
}

.build_compare_html <- function(prev, best) {
  df_prev <- if (!is.null(prev)) .coef_table_df(prev) else tibble::tibble()
  df_best <- .coef_table_df(best)
  
  get_terms <- function(df) if ("term" %in% names(df)) df$term else character(0)
  
  ord_hint <- c(
    "Constant",
    "AR-Nonseasonal-01","AR-Nonseasonal-02","AR-Nonseasonal-03",
    "MA-Nonseasonal-01","MA-Nonseasonal-02",
    "AR-Seasonal-12","MA-Seasonal-12"
  )
  
  terms <- unique(c(ord_hint, setdiff(union(get_terms(df_prev), get_terms(df_best)), ord_hint)))
  
  cell <- function(d) {
    if (!nrow(d)) return("")
    paste0(
      htmltools::htmlEscape(formatC(d$est[1], format = "f", digits = 2)),
      ifelse(d$stars[1] == "", "", paste0("<sup>", d$stars[1], "</sup>")),
      "<br><span style='color:#6b7280'>(",
      htmltools::htmlEscape(formatC(d$se[1], format = "f", digits = 2)),
      ")</span>"
    )
  }
  
  rows <- list(
    htmltools::tags$tr(
      htmltools::tags$th(""), htmltools::tags$th("Alt"), htmltools::tags$th("Neu")
    )
  )
  
  for (t in terms) {
    a <- if (nrow(df_prev)) dplyr::filter(df_prev, .data$term == t) else tibble::tibble()
    b <- dplyr::filter(df_best, .data$term == t)
    if (!nrow(a) && !nrow(b)) next
    rows[[length(rows) + 1]] <- htmltools::tags$tr(
      htmltools::tags$td(htmltools::htmlEscape(t)),
      htmltools::tags$td(htmltools::HTML(cell(a))),
      htmltools::tags$td(htmltools::HTML(cell(b)))
    )
  }
  
  # stats (using helpers)
  stat_row <- function(lbl, left, right) {
    htmltools::tags$tr(
      htmltools::tags$td(htmltools::tags$b(lbl)),
      htmltools::tags$td(htmltools::HTML(left)),
      htmltools::tags$td(htmltools::HTML(right))
    )
  }
  
  qs_prev   <- if (!is.null(prev)) .fmtP(.qs_overall_on_SA(prev)) else "\u2014"
  qs_best   <- .fmtP(.qs_overall_on_SA(best))
  lb_prev   <- if (!is.null(prev)) .fmtP(.lb_p(prev)) else "\u2014"
  lb_best   <- .fmtP(.lb_p(best))
  sh_prev   <- if (!is.null(prev)) .num(.shapiro_stat(prev), 2) else "\u2014"
  sh_best   <- .num(.shapiro_stat(best), 2)
  tf_prev   <- if (!is.null(prev)) as.character(.transform_label(prev)) else "\u2014"
  tf_best   <- as.character(.transform_label(best))
  aicc_prev <- if (!is.null(prev)) .num(tryCatch(.aicc(prev), error = function(e) NA_real_), 2) else "\u2014"
  aicc_best <- .num(tryCatch(.aicc(best), error = function(e) NA_real_), 2)
  n_prev    <- if (!is.null(prev)) as.character(.obs_n(prev)) else "\u2014"
  n_best    <- as.character(.obs_n(best))
  ar_prev   <- if (!is.null(prev)) htmltools::htmlEscape(.arima_string(prev)) else "\u2014"
  ar_best   <- htmltools::htmlEscape(.arima_string(best))
  
  rows <- c(rows, list(
    stat_row("QS (p val.)",        qs_prev,   qs_best),
    stat_row("Box-Ljung (p val.)", lb_prev,   lb_best),
    stat_row("Shapiro (p val.)",   sh_prev,   sh_best),
    stat_row("Transform",          tf_prev,   tf_best),
    stat_row("AICc",               aicc_prev, aicc_best),
    stat_row("Num. obs.",          n_prev,    n_best),
    stat_row("ARIMA",              ar_prev,   ar_best)
  ))
  
  htmltools::tags$table(class = "tbl", rows)
}


#' Engine choice rationale card (SEATS vs X-11)
#'
#' Explains—in plain language—why the selected engine was chosen for the best model.
#' @param res Object from [auto_seasonal_analysis()].
#' @return An htmltools tag (card).
#' @export
sa_engine_choice_card <- function(res) .build_engine_choice_card(res)

.build_engine_choice_card <- function(res) {
  stopifnot(inherits(res, "auto_seasonal_analysis"))
  br  <- dplyr::slice(res$table, 1)
  eng <- if ("engine" %in% names(br)) as.character(br$engine) else .engine_used(res$best)
  
  # small local formatters (don't rely on globals)
  fmtP <- function(p) ifelse(is.na(p), "\u2014", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
  
  # Residual seasonality (QS on SA) \u2014 higher p is better
  p_x11_sa   <- br$QS_p_x11
  p_seats_sa <- br$QS_p_seats
  
  # Existence (QS on original)
  p_x11_ori   <- dplyr::coalesce(br$QSori_p_x11, NA_real_)
  p_seats_ori <- dplyr::coalesce(br$QSori_p_seats, NA_real_)
  
  # SEATS flags
  has_switch   <- isTRUE(br$SEATS_model_switch)
  has_seas     <- br$SEATS_has_seasonal
  has_seas_txt <- if (isTRUE(has_seas)) "present" else if (identical(has_seas, FALSE)) "absent" else "n/a"
  
  lead <- htmltools::HTML(
    paste0("<b>Decomposition engine selected:</b> ",
           "<span class='pill'>", toupper(eng), "</span>")
  )
  
  if (identical(eng, "x11")) {
    # Why X-11 over SEATS?
    bullets <- htmltools::tags$ul(
      htmltools::tags$li(htmltools::HTML(
        paste0("<b>Residual seasonality (QS on SA):</b> X-11 p = ", fmtP(p_x11_sa),
               ", SEATS p = ", fmtP(p_seats_sa),
               ". X-11 yielded cleaner residuals (higher p).")
      )),
      htmltools::tags$li(htmltools::HTML(
        paste0("<b>QS on original (existence):</b> X-11 p = ", fmtP(p_x11_ori),
               ", SEATS p = ", fmtP(p_seats_ori), ".")
      )),
      if (isTRUE(has_switch)) htmltools::tags$li(
        htmltools::HTML("<b>SEATS model-switch warning:</b> detected; this can undermine SEATS diagnostics.")
      ),
      if (identical(has_seas, FALSE)) htmltools::tags$li(
        htmltools::HTML("<b>SEATS seasonal component:</b> absent; X-11 provides a stable seasonal factor.")
      )
    )
    nuance <- "Note: By default, we prefer SEATS. X-11 is selected if diagnostics (especially QA on SA) are clearly better or if SEATS warnings occur."
  } else {
    # eng == "seats" (or unknown defaults to SEATS in helpers)
    bullets <- htmltools::tags$ul(
      htmltools::tags$li(htmltools::HTML(
        paste0("<b>Residual seasonality (QS on SA):</b> SEATS p = ", fmtP(p_seats_sa),
               if (!is.na(p_x11_sa)) paste0(", X-11 p = ", fmtP(p_x11_sa)) else "",
               ". SEATS is equivalent or better.")
      )),
      htmltools::tags$li(htmltools::HTML(
        paste0("<b>QS on original (existence):</b> SEATS p = ", fmtP(p_seats_ori),
               if (!is.na(p_x11_ori)) paste0(", X-11 p = ", fmtP(p_x11_ori)) else "", ".")
      )),
      htmltools::tags$li(htmltools::HTML(
        paste0("<b>SEATS seasonal component:</b> ", has_seas_txt, ".")
      ))
    )
    nuance <- "SEATS is the default choice; switching to X-11 only occurs if there are clear advantages (residual QS) or stability-related warnings."
  }
  
  htmltools::div(
    class = "card",
    htmltools::tags$h2("Engine choice: SEATS vs X-11"),
    htmltools::tags$p(lead),
    bullets,
    htmltools::tags$p(class = "sub", nuance)
  )
}
