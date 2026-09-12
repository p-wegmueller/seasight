#' Should we switch to the new best model?
#'
#' Compares the first row of `res$table` with the incumbent model recorded by
#' [auto_seasonal_analysis()]. The candidate must improve comparable AICc by a
#' material amount, pass absolute residual diagnostics, avoid material
#' deterioration relative to the incumbent, and satisfy the existing distance
#' and seasonal-correlation safeguards. Identical adjusted series are kept.
#'
#' @param res Result of [auto_seasonal_analysis()].
#' @param thresholds Named list with decision thresholds:
#'   - min_qs_p: minimum acceptable QS p-value on SA (overall) for the best model
#'   - max_dist_sa_mult: allow SA L1 distance up to this multiple of the cross-candidate median
#'   - min_corr_seas: minimum correlation of seasonal components (vs. incumbent)
#'   - min_lb_p: minimum acceptable Ljung-Box p-value on residuals
#'   - min_delta_aicc: minimum incumbent-minus-candidate AICc improvement
#'   - max_qs_p_drop: maximum allowed decline in QS p-value vs. incumbent
#'   - max_lb_p_drop: maximum allowed decline in Ljung-Box p-value vs. incumbent
#'   - identical_sa_tolerance: relative tolerance used to identify equal adjusted series
#' @param current_model Optional fitted `seasonal::seas` incumbent. This is
#'   useful for older result objects that do not contain baseline diagnostics.
#' @param details Logical. If `FALSE` (default), return only the decision string.
#'   If `TRUE`, return a list containing `decision`, `reason`, and `metrics`.
#' @return One of `"CHANGE_TO_NEW_MODEL"`, `"KEEP_CURRENT_MODEL"`,
#'   `"REVIEW_REQUIRED"`, or `"NO_BASELINE"`; with `details = TRUE`, a
#'   structured list containing the decision and its supporting evidence.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("seasonal", quietly = TRUE)) {
#'   current_model <- seasonal::seas(AirPassengers)
#'   res <- auto_seasonal_analysis(
#'     AirPassengers,
#'     current_model = current_model,
#'     max_specs = 3
#'   )
#'   sa_should_switch(res)
#' }
#' }
#' @export
sa_should_switch <- function(res,
                             thresholds = list(min_qs_p = 0.10,
                                               max_dist_sa_mult = 1.25,
                                               min_corr_seas = 0.90,
                                               min_lb_p = 0.05,
                                               min_delta_aicc = 2,
                                               max_qs_p_drop = 0.05,
                                               max_lb_p_drop = 0.05,
                                               identical_sa_tolerance = 1e-8),
                             current_model = NULL,
                             details = FALSE) {
  if (!inherits(res, "auto_seasonal_analysis")) {
    stop("`res` must be an object returned by `auto_seasonal_analysis()`.", call. = FALSE)
  }
  if (!is.data.frame(res$table) || !nrow(res$table)) {
    stop("`res$table` must contain at least one candidate row.", call. = FALSE)
  }
  if (!is.null(current_model) && !inherits(current_model, "seas")) {
    stop("`current_model` must be a fitted `seasonal::seas` object.", call. = FALSE)
  }
  if (!is.logical(details) || length(details) != 1L || is.na(details)) {
    stop("`details` must be TRUE or FALSE.", call. = FALSE)
  }

  br <- dplyr::slice(res$table, 1)

  tz <- function(nm, def) {
    v <- tryCatch(thresholds[[nm]], error = function(e) NA_real_)
    v <- suppressWarnings(as.numeric(v))
    if (!length(v) || !is.finite(v[1])) def else v[1]
  }
  thr_qs     <- tz("min_qs_p",       0.10)
  thr_dist_m <- tz("max_dist_sa_mult", 1.25)
  thr_corr   <- tz("min_corr_seas",  0.90)
  thr_lb     <- tz("min_lb_p",       0.05)
  min_delta_aicc <- tz("min_delta_aicc", 2)
  max_qs_drop <- tz("max_qs_p_drop", 0.05)
  max_lb_drop <- tz("max_lb_p_drop", 0.05)
  identity_tol <- tz("identical_sa_tolerance", 1e-8)

  row_num <- function(row, nm) {
    if (is.null(row) || !nm %in% names(row)) return(NA_real_)
    x <- suppressWarnings(as.numeric(row[[nm]]))
    if (!length(x) || !is.finite(x[1])) NA_real_ else x[1]
  }

  comparison_y <- res$y %||% if (inherits(res$best, "seas")) {
    tryCatch(seasonal::original(res$best), error = function(e) NULL)
  } else {
    NULL
  }
  baseline <- if (!is.null(current_model)) {
    .build_switch_baseline(
      current_model = current_model,
      y = comparison_y,
      best_model = res$best,
      current_sa = tryCatch(seasonal::final(current_model), error = function(e) NULL),
      current_seasonal = tryCatch(
        seasonal::series(current_model, "seats.seasonal"),
        error = function(e) tryCatch(
          seasonal::series(current_model, "x11.seasonal"),
          error = function(e) NULL
        )
      ),
      candidate_transform = res$transform %||% NA_character_
    )
  } else {
    res$baseline
  }

  baseline_diag <- baseline$diagnostics %||% NULL
  baseline_row <- if (is.data.frame(baseline_diag) && nrow(baseline_diag)) {
    baseline_diag[1, , drop = FALSE]
  } else if (is.list(baseline_diag)) {
    baseline_diag
  } else {
    NULL
  }

  candidate_aicc <- row_num(br, "AICc")
  candidate_qs <- row_num(br, "QS_p")
  candidate_lb <- row_num(br, "LB_p")
  current_aicc <- row_num(baseline_row, "AICc")
  current_qs <- row_num(baseline_row, "QS_p")
  current_lb <- row_num(baseline_row, "LB_p")
  delta_aicc <- current_aicc - candidate_aicc
  metrics <- list(
    candidate_aicc = candidate_aicc,
    incumbent_aicc = current_aicc,
    delta_aicc = delta_aicc,
    candidate_qs_p = candidate_qs,
    incumbent_qs_p = current_qs,
    candidate_lb_p = candidate_lb,
    incumbent_lb_p = current_lb,
    dist_sa_L1 = row_num(br, "dist_sa_L1"),
    corr_seas = row_num(br, "corr_seas")
  )
  finish <- function(decision, reason) {
    out <- structure(
      list(decision = decision, reason = reason, metrics = metrics),
      class = "seasight_switch_assessment"
    )
    if (isTRUE(details)) out else out$decision
  }

  if (is.null(baseline_row)) {
    if (!is.null(baseline$current_sa)) {
      return(finish(
        "REVIEW_REQUIRED",
        "An incumbent series is available, but comparable incumbent diagnostics are missing."
      ))
    }
    return(finish("NO_BASELINE", "No incumbent model was supplied; a switch cannot be assessed."))
  }

  candidate_sa <- if (inherits(res$best, "seas")) {
    tryCatch(seasonal::final(res$best), error = function(e) NULL)
  } else {
    NULL
  }
  same_adjusted <- !is.null(baseline$current_sa) && !is.null(candidate_sa) &&
    .same_ts_values(baseline$current_sa, candidate_sa, tolerance = identity_tol)
  if (isTRUE(same_adjusted)) {
    return(finish(
      "KEEP_CURRENT_MODEL",
      "The candidate and incumbent produce the same seasonally adjusted series within tolerance."
    ))
  }

  if (!is.finite(candidate_qs) || !is.finite(candidate_lb)) {
    return(finish(
      "REVIEW_REQUIRED",
      "The candidate's QS or Ljung-Box diagnostic is unavailable."
    ))
  }
  if (candidate_qs < thr_qs) {
    return(finish(
      "KEEP_CURRENT_MODEL",
      sprintf("The candidate fails the residual QS threshold (%.3f < %.3f).", candidate_qs, thr_qs)
    ))
  }
  if (candidate_lb < thr_lb) {
    return(finish(
      "KEEP_CURRENT_MODEL",
      sprintf("The candidate fails the Ljung-Box threshold (%.3f < %.3f).", candidate_lb, thr_lb)
    ))
  }

  if (!isTRUE(baseline$aicc_comparable)) {
    return(finish(
      "REVIEW_REQUIRED",
      "Candidate and incumbent AICc values are not comparable because their input sample or transformation differs."
    ))
  }
  if (!is.finite(candidate_aicc) || !is.finite(current_aicc)) {
    return(finish(
      "REVIEW_REQUIRED",
      "Comparable AICc values are unavailable for the candidate or incumbent."
    ))
  }
  if (delta_aicc < min_delta_aicc) {
    return(finish(
      "KEEP_CURRENT_MODEL",
      sprintf(
        "The candidate's AICc improvement is not material (%.2f; required: %.2f).",
        delta_aicc,
        min_delta_aicc
      )
    ))
  }

  if (!is.finite(current_qs) || !is.finite(current_lb)) {
    return(finish(
      "REVIEW_REQUIRED",
      "The incumbent's QS or Ljung-Box diagnostic is unavailable."
    ))
  }
  if (candidate_qs < current_qs - max_qs_drop) {
    return(finish(
      "KEEP_CURRENT_MODEL",
      sprintf(
        "The candidate materially worsens the QS p-value (%.3f vs %.3f).",
        candidate_qs,
        current_qs
      )
    ))
  }
  if (candidate_lb < current_lb - max_lb_drop) {
    return(finish(
      "KEEP_CURRENT_MODEL",
      sprintf(
        "The candidate materially worsens the Ljung-Box p-value (%.3f vs %.3f).",
        candidate_lb,
        current_lb
      )
    ))
  }

  distances <- if ("dist_sa_L1" %in% names(res$table)) {
    suppressWarnings(as.numeric(res$table$dist_sa_L1))
  } else {
    numeric(0)
  }
  meddist <- suppressWarnings(stats::median(distances[is.finite(distances)], na.rm = TRUE))
  if (is.finite(metrics$dist_sa_L1) && is.finite(meddist) &&
      metrics$dist_sa_L1 > meddist * thr_dist_m) {
    return(finish(
      "KEEP_CURRENT_MODEL",
      "The candidate's adjusted series is unusually far from the incumbent."
    ))
  }
  if (is.finite(metrics$corr_seas) && metrics$corr_seas < thr_corr) {
    return(finish(
      "KEEP_CURRENT_MODEL",
      sprintf(
        "The seasonal-component correlation is below the safeguard (%.3f < %.3f).",
        metrics$corr_seas,
        thr_corr
      )
    ))
  }

  finish(
    "CHANGE_TO_NEW_MODEL",
    sprintf(
      "The candidate improves comparable AICc by %.2f and passes absolute and incumbent-relative diagnostics.",
      delta_aicc
    )
  )
}


#' Existence-of-seasonality narrative (HTML tag)
#'
#' Uses IDS, M7, QS on original, and presence/absence of a SEATS seasonal component
#' to provide a short textual assessment of seasonality for the best model.
#'
#' @param res Result of [auto_seasonal_analysis()].
#' @return An htmltools tag representing the card.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("seasonal", quietly = TRUE)) {
#'   res <- auto_seasonal_analysis(AirPassengers, max_specs = 3)
#'   sa_existence_card(res)
#' }
#' }
#' @export
sa_existence_card <- function(res) .build_existence_card(res)


.build_existence_card <- function(res) {
  stopifnot(inherits(res, "auto_seasonal_analysis"))
  br   <- dplyr::slice(res$table, 1)
  ui_call <- tryCatch(.existence_call_ui(res), error = function(e) list(call = "\u2014", note = NULL))
  call <- ui_call$call

  m7 <- .report_numeric(.report_get(br, "M7", NA_real_))
  ids <- .report_chr(.report_get(br, "IDS", "n/a"), "n/a")
  p_x11_ori <- .report_get(br, "QSori_p_x11", NA_real_)
  p_seats_ori <- .report_get(br, "QSori_p_seats", NA_real_)
  qso_values <- c(.report_numeric(p_x11_ori), .report_numeric(p_seats_ori))
  
  # M7 interpretation (X-11 rule of thumb)
  m7_txt <- if (!is.finite(m7)) "n/a" else if (m7 < 0.90) "clear seasonality"
  else if (m7 < 1.05) "weak seasonality" else "no seasonality"
  
  # IDS tolerant rendering
  ids_txt <- if (!nzchar(ids)) "n/a" else ids
  
  # QS on original (existence-of-seasonality)
  qso_min <- if (any(is.finite(qso_values))) min(qso_values, na.rm = TRUE) else NA_real_
  
  # SEATS seasonal component presence
  seats_has <- .seats_switch_state(.report_get(br, "SEATS_has_seasonal", NA))
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
      paste0("<b>M7 (X-11)</b>: ", .report_num(m7, 3), " \u2192 ", m7_txt, ".")
    )),
    htmltools::tags$li(htmltools::HTML(
      paste0("<b>QS on original</b>: X-11 p = ", .report_p(p_x11_ori),
             ", SEATS p = ", .report_p(p_seats_ori),
             " \u2192 overall = ", .report_p(qso_min), ".")
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
#' Explains, in plain language, why the selected engine was chosen for the best model.
#' @param res Object from [auto_seasonal_analysis()].
#' @return An htmltools tag (card).
#'
#' @examples
#' \donttest{
#' if (requireNamespace("seasonal", quietly = TRUE)) {
#'   res <- auto_seasonal_analysis(AirPassengers, max_specs = 3)
#'   sa_engine_choice_card(res)
#' }
#' }
#' @export
sa_engine_choice_card <- function(res) .build_engine_choice_card(res)

.seats_switch_details <- function(res) {
  br <- dplyr::slice(res$table, 1)
  flag <- if ("SEATS_model_switch" %in% names(br)) {
    .seats_switch_state(br$SEATS_model_switch)
  } else {
    NA
  }
  requested <- tryCatch(.coalesce_arima_str(br), error = function(e) NA_character_)
  operative <- if ("SEATS_operative_model" %in% names(br)) {
    as.character(br$SEATS_operative_model[[1]])
  } else {
    NA_character_
  }
  if (!length(operative) || is.na(operative[[1]]) || !nzchar(operative[[1]])) {
    operative <- .seats_model_used(res$best)
  }
  engine <- if ("engine" %in% names(br)) {
    as.character(br$engine[[1]])
  } else {
    tryCatch(.engine_used(res$best), error = function(e) "unknown")
  }
  clean <- function(x) {
    x <- as.character(x %||% NA_character_)
    if (!length(x)) return("n/a")
    x <- x[[1]]
    if (is.na(x) || !nzchar(trimws(x))) "n/a" else trimws(x)
  }
  list(
    flag = flag,
    engine = tolower(clean(engine)),
    requested = clean(requested),
    operative = clean(operative)
  )
}

.seats_switch_warning_tag <- function(res, location = c("engine", "summary")) {
  location <- match.arg(location)
  details <- .seats_switch_details(res)
  if (!isTRUE(details$flag)) return(NULL)

  context <- if (identical(details$engine, "seats")) {
    "The selected SEATS decomposition uses a substituted model."
  } else {
    "X-13 reported a model substitution for the SEATS alternative."
  }
  tag <- if (identical(location, "summary")) htmltools::tags$div else htmltools::tags$li
  tag(
    class = "seats-switch-warning",
    style = paste(
      "margin:10px 0; padding:10px 12px; border:1px solid #f59e0b;",
      "border-radius:8px; background:#fffbeb; color:#92400e;"
    ),
    htmltools::tags$strong("SEATS model-switch warning: "),
    context,
    " Requested ARIMA: ", htmltools::tags$code(details$requested),
    "; operative SEATS model: ", htmltools::tags$code(details$operative), "."
  )
}

.build_engine_choice_card <- function(res) {
  stopifnot(inherits(res, "auto_seasonal_analysis"))
  br  <- dplyr::slice(res$table, 1)
  eng <- .report_chr(
    .report_get(
      br, "engine",
      tryCatch(.engine_used(res$best), error = function(e) "unknown")
    ),
    "unknown"
  )
  
  # Residual seasonality (QS on SA) \u2014 higher p is better
  p_x11_sa   <- .report_get(br, "QS_p_x11", NA_real_)
  p_seats_sa <- .report_get(br, "QS_p_seats", NA_real_)
  
  # Existence (QS on original)
  p_x11_ori   <- .report_get(br, "QSori_p_x11", NA_real_)
  p_seats_ori <- .report_get(br, "QSori_p_seats", NA_real_)
  
  # SEATS flags
  has_seas     <- .seats_switch_state(.report_get(br, "SEATS_has_seasonal", NA))
  has_seas_txt <- if (isTRUE(has_seas)) "present" else if (identical(has_seas, FALSE)) "absent" else "n/a"
  switch_warning <- .seats_switch_warning_tag(res, location = "engine")
  
  lead <- htmltools::HTML(
    paste0("<b>Decomposition engine selected:</b> ",
           "<span class='pill'>", toupper(eng), "</span>")
  )
  
  if (identical(eng, "x11")) {
    # Why X-11 over SEATS?
    bullets <- htmltools::tags$ul(
      htmltools::tags$li(htmltools::HTML(
        paste0("<b>Residual seasonality (QS on SA):</b> X-11 p = ", .report_p(p_x11_sa),
               ", SEATS p = ", .report_p(p_seats_sa),
               ". X-11 yielded cleaner residuals (higher p).")
      )),
      htmltools::tags$li(htmltools::HTML(
        paste0("<b>QS on original (existence):</b> X-11 p = ", .report_p(p_x11_ori),
               ", SEATS p = ", .report_p(p_seats_ori), ".")
      )),
      switch_warning,
      if (identical(has_seas, FALSE)) htmltools::tags$li(
        htmltools::HTML("<b>SEATS seasonal component:</b> absent; X-11 provides a stable seasonal factor.")
      )
    )
    nuance <- "Note: By default, we prefer SEATS. X-11 is selected if diagnostics (especially QA on SA) are clearly better or if SEATS warnings occur."
  } else {
    # eng == "seats" (or unknown defaults to SEATS in helpers)
    bullets <- htmltools::tags$ul(
      htmltools::tags$li(htmltools::HTML(
        paste0("<b>Residual seasonality (QS on SA):</b> SEATS p = ", .report_p(p_seats_sa),
               ", X-11 p = ", .report_p(p_x11_sa),
               ". SEATS is equivalent or better.")
      )),
      htmltools::tags$li(htmltools::HTML(
        paste0("<b>QS on original (existence):</b> SEATS p = ", .report_p(p_seats_ori),
               ", X-11 p = ", .report_p(p_x11_ori), ".")
      )),
      htmltools::tags$li(htmltools::HTML(
        paste0("<b>SEATS seasonal component:</b> ", has_seas_txt, ".")
      )),
      switch_warning
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
