# =============================================================================
# sa-utils-decision.R unified decision + rendering helpers (internal)
# =============================================================================

# --- small utilities -----------------------------------------------------------

.coalesce_chr <- function(...) {
  xs <- list(...)
  n  <- max(vapply(xs, length, 1L))
  out <- rep(NA_character_, n)
  for (x in xs) {
    if (is.null(x)) next
    ok <- is.na(out) & !is.na(x) & x != ""
    out[ok] <- x[ok]
  }
  out
}

.safepmin <- function(a, b) {
  za <- suppressWarnings(as.numeric(a))
  zb <- suppressWarnings(as.numeric(b))
  z  <- suppressWarnings(pmin(za, zb, na.rm = TRUE))
  z[is.infinite(z)] <- NA_real_
  z
}

# Robust getters from a single-row tibble/list -------------------------------

.qs_sa_from_row <- function(row) {
  r <- if (is.data.frame(row)) as.list(row[1, , drop = FALSE]) else row
  # Prefer unified overall QS on SA if present
  q <- suppressWarnings(as.numeric(r$QS_p))
  if (is.finite(q)) return(q)
  # Fallback: precomputed min, or compute from split cols
  qmin <- suppressWarnings(as.numeric(r$qs_sa_min))
  if (is.finite(qmin)) return(qmin)
  qx <- suppressWarnings(as.numeric(r$QS_p_x11))
  qs <- suppressWarnings(as.numeric(r$QS_p_seats))
  .safepmin(qx, qs)
}

.lb_from_row <- function(row) {
  r <- if (is.data.frame(row)) as.list(row[1, , drop = FALSE]) else row
  lb <- suppressWarnings(as.numeric(r$LB_p))
  if (is.finite(lb)) return(lb)
  suppressWarnings(as.numeric(r$lb_p))
}

# --- UI consistency helpers ---------------------------------------------------

# Apply a "seasonal-only" guard before rendering UI elements.
#
# Rationale: SEATS seasonal component is only defined for SEATS models.
# If SEATS is selected but no seasonal component is available, showing
# "ADJUST" without explanation is contradictory. We downgrade the
# *display* call to BORDERLINE and surface a short note.
.existence_call_ui <- function(res) {
  stopifnot(inherits(res, "auto_seasonal_analysis"))
  call_raw <- tryCatch(as.character(res$seasonality$overall$call_overall[1]),
                       error = function(e) "\u2014")
  br <- tryCatch(dplyr::slice(res$table, 1), error = function(e) NULL)
  if (is.null(br) || !nrow(br)) {
    return(list(call = call_raw, note = NULL))
  }
  eng <- as.character(br$engine %||% NA_character_)
  seats_has <- if ("SEATS_has_seasonal" %in% names(br)) br$SEATS_has_seasonal else NA
  
  if (identical(eng, "seats") && identical(seats_has, FALSE) && identical(call_raw, "ADJUST")) {
    return(list(
      call = "BORDERLINE",
      note = "SEATS was selected but no SEATS seasonal component is available; treat the evidence as borderline and consider switching to X-11 or revising the specification."
    ))
  }
  
  list(call = call_raw, note = NULL)
}


# --- decision logic ------------------------------------------------------------

# Compose a high-level decision given the existence call and the best row.
# existence: "ADJUST" | "BORDERLINE" | "DO_NOT_ADJUST"
# best:      named list or 1-row df with diagnostics (QS/LB/etc.) or NULL
# has_current: whether there is an incumbent model to fall back to
.compose_decision <- function(existence, best, has_current,
                              alpha_lb = 0.05, alpha_qs = 0.10) {
  # If no material seasonality on the original series \u2192 do not adjust at all.
  if (identical(existence, "DO_NOT_ADJUST")) {
    return(list(
      decision = "DO_NOT_ADJUST",
      reason   = "No material seasonality on the original series."
    ))
  }
  # No admissible candidate
  if (is.null(best)) {
    return(list(
      decision = if (isTRUE(has_current)) "KEEP_CURRENT_MODEL" else "DO_NOT_ADJUST",
      reason   = "No admissible candidate could be estimated."
    ))
  }
  
  br <- if (is.data.frame(best)) best[1, , drop = FALSE] else best
  
  lb_p      <- .lb_from_row(br)
  qs_sa_min <- .qs_sa_from_row(br)
  
  # Residual autocorrelation blocks switching (but missing LB_p should not block)
  if (is.finite(lb_p) && lb_p < alpha_lb) {
    return(list(
      decision = if (isTRUE(has_current)) "KEEP_CURRENT_MODEL" else "DO_NOT_ADJUST",
      reason   = "Best candidate fails residual autocorrelation (Ljung-Box)."
    ))
  }
  # Residual seasonality on SA blocks switching (missing QS should not block)
  if (is.finite(qs_sa_min) && qs_sa_min < alpha_qs) {
    return(list(
      decision = if (isTRUE(has_current)) "KEEP_CURRENT_MODEL" else "DO_NOT_ADJUST",
      reason   = "Best candidate shows residual seasonality (QS on SA)."
    ))
  }
  
  list(decision = "ADJUST", reason = "Best candidate passes diagnostics.")
}

.gate_flags_from_decision <- function(dec) {
  list(
    show_new_model_blocks   = identical(dec$decision, "ADJUST"),
    show_current_only       = identical(dec$decision, "KEEP_CURRENT_MODEL"),
    show_any_model_sections = identical(dec$decision, "ADJUST") ||
      identical(dec$decision, "KEEP_CURRENT_MODEL")
  )
}

# --- ARIMA display helpers -----------------------------------------------------

.format_arima <- function(row) {
  r <- if (is.data.frame(row)) as.list(row[1, , drop = FALSE]) else row
  a <- r$arima; s <- r$spec; airline <- r$airline
  if (is.character(a) && nzchar(a)) return(a)
  if (is.character(s) && grepl("^\\(", s)) return(s)
  if (isTRUE(airline) || identical(airline, 1)) return("(0 1 1)(0 1 1)")
  "\u2014"
}

.coalesce_arima_cols <- function(df) {
  df <- tibble::as_tibble(df)
  n  <- nrow(df)
  arima_chr <- if ("arima" %in% names(df)) as.character(df$arima) else rep(NA_character_, n)
  disp_chr  <- if ("ARIMA_disp" %in% names(df)) as.character(df$ARIMA_disp) else rep(NA_character_, n)
  
  has_arima <- !is.na(arima_chr) & nzchar(arima_chr)
  has_disp  <- !is.na(disp_chr)  & nzchar(disp_chr)
  
  arima_out <- ifelse(has_arima, arima_chr, ifelse(has_disp, disp_chr, NA_character_))
  disp_out  <- ifelse(has_disp,  disp_chr,  arima_out)
  
  df$arima      <- arima_out
  df$ARIMA_disp <- disp_out
  df
}


# --- "best row" extraction & summary ------------------------------------------

.extract_best_row <- function(tbl) {
  if (is.null(tbl) || !nrow(tbl)) return(NULL)
  out <- tbl[1, , drop = FALSE]
  # ensure a qs_sa_min column (for legacy callers)
  if (!"qs_sa_min" %in% names(out)) {
    out$qs_sa_min <- .safepmin(out[["QS_p_x11"]], out[["QS_p_seats"]])
    # if unified QS_p exists, prefer it when finite
    if (is.finite(suppressWarnings(as.numeric(out$QS_p)))) {
      out$qs_sa_min <- suppressWarnings(as.numeric(out$QS_p))
    }
  }
  out
}

.summarize_for_summary_card <- function(best_row, flags) {
  if (is.null(best_row)) {
    return(list(best="n/a", td="n/a", aicc=NA_real_, lb_p=NA_real_, engine="n/a"))
  }
  br <- as.list(best_row[1, , drop = FALSE])
  list(
    best   = if (isTRUE(flags$show_any_model_sections)) .format_arima(br) else "n/a",
    td     = if (isTRUE(flags$show_any_model_sections)) as.character(br$td_name %||% br$td %||% NA_character_) else "n/a",
    aicc   = if (isTRUE(flags$show_any_model_sections)) suppressWarnings(as.numeric(br$AICc %||% br$aicc)) else NA_real_,
    lb_p   = if (isTRUE(flags$show_any_model_sections)) .lb_from_row(br) else NA_real_,
    engine = if (isTRUE(flags$show_any_model_sections)) as.character(br$engine %||% "n/a") else "n/a"
  )
}
