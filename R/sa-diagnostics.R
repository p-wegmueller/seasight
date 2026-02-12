# =============================================================================
# sa-diagnostics.R — Diagnostics, tests, and copy-paste helpers
# =============================================================================

# --- Internal: legacy alias (kept for compatibility) --------------------------
# Prefer using `sa_copyable_call()` below. This wrapper maps the old arguments.
#' @keywords internal
#' @noRd
.copyable_static <- function(m, x_expr, td_expr = NA,
                             force_denton = TRUE, drop_seats = TRUE) {
  # drop_seats is implicit in sa_copyable_call (we always normalize engine args)
  sa_copyable_call(
    m = m,
    x_expr = x_expr,
    xreg_expr = td_expr,               # NULL -> drop; NA -> keep; character -> set
    include_force = isTRUE(force_denton),
    engine = "auto"
  )
}

# --- Outliers ------------------------------------------------------------------

#' Extract X-13 outlier coefficients (AO/LS/TC) from a `seas` model
#'
#' @param m A fitted [seasonal::seas] model.
#' @return A tibble with columns `type`, `period`, `coef`. Empty when none found.
#' @export
extract_outliers <- function(m) {
  stopifnot(inherits(m, "seas"))
  nm <- names(stats::coef(m))
  # e.g. AO2016.4, LS2010.2, TC2008.12 (monthly/quarterly, etc.)
  pick <- nm[grepl("^(AO|LS|TC)\\d{4}\\.\\d{1,2}$", nm)]
  if (!length(pick)) {
    return(tibble::tibble(type = character(), period = character(), coef = numeric()))
  }
  tibble::tibble(
    type   = sub("^(AO|LS|TC).*", "\\1", pick),
    period = sub("^(AO|LS|TC)(\\d{4}\\.\\d+)$", "\\2", pick),
    coef   = as.numeric(stats::coef(m)[pick])
  )
}

# --- Seasonality tests: single model ------------------------------------------

#' Key seasonality diagnostics for a fitted model
#'
#' Returns M7, IDS, QS on SA (X-11 & SEATS, plus overall min), QS on original
#' (X-11 & SEATS, plus overall min), and Ljung–Box p-value on residuals.
#'
#' Columns:
#' * `M7`, `IDS`
#' * `QS_p_x11`, `QS_p_seats`, `QS_p`              (QS on SA)
#' * `QSori_p_x11`, `QSori_p_seats`, `QSori_p`     (QS on original)
#' * `LB_p`
#'
#' For backwards compatibility, the output also includes the legacy aliases
#' `QS_SA_x11`, `QS_SA_seats`, `QS_SA`, `QSori_x11`, `QSori_seats`, `QSori`.
#'
#' @param m A fitted [seasonal::seas] object.
#' @return A one-row tibble with the diagnostics above.
#' @export
sa_tests_model <- function(m) {
  stopifnot(inherits(m, "seas"))
  
  # small helper: safely get first element of a column if present
  get1 <- function(x, nm, default = NA_real_) {
    if (is.null(x)) return(default)
    if (!nm %in% names(x)) return(default)
    v <- x[[nm]]
    if (length(v) == 0) default else v[[1]]
  }
  
  # --- core stats -----------------------------------------------------------
  M7_val  <- suppressWarnings(tryCatch(.m7_stat(m),  error = function(e) NA_real_))
  IDS_val <- suppressWarnings(tryCatch(.ids_flag(m), error = function(e) NA_character_))
  LB_val  <- suppressWarnings(tryCatch(.lb_p(m),     error = function(e) NA_real_))
  
  # --- QS on SA (X-11 & SEATS) ---------------------------------------------
  tb_sa <- tryCatch(.qs_on_sa_both(m), error = function(e) NULL)
  
  # support both naming schemes: QS_SA_*  OR  QS_p_*
  QS_SA_x11   <- get1(tb_sa, "QS_SA_x11",   get1(tb_sa, "QS_p_x11"))
  QS_SA_seats <- get1(tb_sa, "QS_SA_seats", get1(tb_sa, "QS_p_seats"))
  QS_SA_min   <- get1(tb_sa, "QS_SA_min",   get1(tb_sa, "QS_p"))
  
  # --- QS on original -------------------------------------------------------
  tb_ori <- tryCatch(.qs_original(m), error = function(e) NULL)
  
  QSori_x11   <- get1(tb_ori, "QSori_x11",   get1(tb_ori, "QSori_p_x11"))
  QSori_seats <- get1(tb_ori, "QSori_seats", get1(tb_ori, "QSori_p_seats"))
  QSori_min   <- get1(tb_ori, "QSori_min",   get1(tb_ori, "QSori_p"))
  
  # --- main output: "new" column names -------------------------------------
  out <- tibble::tibble(
    M7   = M7_val,
    IDS  = IDS_val,
    QS_p_x11      = QS_SA_x11,
    QS_p_seats    = QS_SA_seats,
    QS_p          = QS_SA_min,
    QSori_p_x11   = QSori_x11,
    QSori_p_seats = QSori_seats,
    QSori_p       = QSori_min,
    LB_p          = LB_val
  )
  
  # --- legacy aliases to avoid breaking existing code/tests -----------------
  out <- dplyr::mutate(
    out,
    QS_SA_x11   = .data$QS_p_x11,
    QS_SA_seats = .data$QS_p_seats,
    QS_SA       = .data$QS_p,
    QSori_x11   = .data$QSori_p_x11,
    QSori_seats = .data$QSori_p_seats,
    QSori       = .data$QSori_p
  )
  
  out
}


# --- Aggregate existence-of-seasonality call ----------------------------------

#' Aggregate existence-of-seasonality decision (ADJUST / BORDERLINE / DO_NOT_ADJUST)
#'
#' Uses IDS, M7 (strong/weak), QS on SA, and QS on ORIGINAL (QSori) to form
#' a simple majority-style decision. Thresholds mirror those used elsewhere.
#'
#' @param tbl Candidate table (e.g., `res$table`).
#' @param majority Required share for a positive overall call (default 0.6).
#' @param thr List of thresholds: `qs` (0.10), `m7_strong` (0.90), `m7_weak` (1.05),
#'   `borderline_min_share` (0.4), `use_qsori` (TRUE).
#' @return A tibble with `call_overall` and shares per test family.
#' @export
sa_existence_call <- function(tbl,
                              majority = 0.6,
                              thr = list(qs = 0.10,
                                         m7_strong = 0.90,
                                         m7_weak   = 1.05,
                                         borderline_min_share = 0.4,
                                         use_qsori = TRUE)) {
  stopifnot(is.data.frame(tbl))
  
  # Shares per family
  qs_sa_sh <- mean(is.finite(tbl$QS_p) & (tbl$QS_p < thr$qs), na.rm = TRUE)
  m7_s_sh  <- mean(is.finite(tbl$M7)   & (tbl$M7   < thr$m7_strong), na.rm = TRUE)
  m7_w_sh  <- mean(is.finite(tbl$M7)   & (tbl$M7   < thr$m7_weak),   na.rm = TRUE)
  ids_sh   <- mean(tolower(as.character(tbl$IDS)) %in% "yes",        na.rm = TRUE)
  
  # QS on ORIGINAL (existence)
  if ("QSori_p" %in% names(tbl)) {
    qsori_p <- tbl$QSori_p
  } else {
    qx <- if ("QSori_p_x11"   %in% names(tbl)) tbl$QSori_p_x11   else NA_real_
    qs <- if ("QSori_p_seats" %in% names(tbl)) tbl$QSori_p_seats else NA_real_
    qsori_p <- suppressWarnings(pmin(qx, qs, na.rm = TRUE))
    qsori_p[!is.finite(qsori_p)] <- NA_real_
  }
  qsori_sh <- mean(is.finite(qsori_p) & (qsori_p < thr$qs), na.rm = TRUE)
  
  adjust <- (ids_sh >= majority) ||
    (isTRUE(thr$use_qsori) && qsori_sh >= majority) ||
    ((qs_sa_sh + m7_s_sh) >= majority)
  
  borderline <- (!adjust) && (
    max(c(ids_sh,
          if (isTRUE(thr$use_qsori)) qsori_sh else 0,
          m7_w_sh,
          qs_sa_sh), na.rm = TRUE) >= thr$borderline_min_share
  )
  
  call <- if (adjust) "ADJUST" else if (borderline) "BORDERLINE" else "DO_NOT_ADJUST"
  
  tibble::tibble(
    call_overall    = call,
    share_ids       = ids_sh,
    share_qs        = qs_sa_sh,
    share_m7_strong = m7_s_sh,
    share_m7_weak   = m7_w_sh,
    share_qsori     = qsori_sh
  )
}

# --- Copy-pasteable seas() call ------------------------------------------------

# local AICc (not exported here, but kept in sync with other files)
.aicc <- function(m) {
  aic <- suppressWarnings(tryCatch(stats::AIC(m), error = function(e) NA_real_))
  k   <- suppressWarnings(tryCatch(length(stats::coef(m)), error = function(e) NA_integer_))
  n   <- suppressWarnings(tryCatch(length(stats::na.omit(seasonal::original(m))), error = function(e) NA_integer_))
  if (!is.finite(aic) || !is.finite(k) || !is.finite(n) || n <= (k + 1)) return(aic)
  aic + (2 * k * (k + 1)) / (n - k - 1)
}

#' Build a copy-pasteable seas() call from a fitted model
#'
#' * SEATS (default engine) → omit `x11`/`seats` args
#' * X-11 → include `x11 = ""`
#'
#' @param m          Fitted `seas` model.
#' @param x_expr     Character code for `x=` (e.g. `"y"`).
#' @param xreg_expr  `NULL` to drop, `NA` to keep as-is, or character code to set.
#' @param include_force Logical; if TRUE adds `force.type = "denton"`.
#' @param engine     `"auto"` (use model's engine) or force `"seats"`/`"x11"`.
#' @return A single string containing the `seas(...)` call.
#' @export
sa_copyable_call <- function(m, x_expr, xreg_expr = NA,
                             include_force = FALSE,
                             engine = c("auto","seats","x11")) {
  stopifnot(inherits(m, "seas"), is.character(x_expr), length(x_expr) >= 1)
  x_expr <- paste(x_expr, collapse = "\n")
  
  engine <- match.arg(engine)
  
  cl <- seasonal::static(m)        # language: seas(...)
  stopifnot(is.call(cl))
  
  to_lang <- function(z) if (is.character(z)) str2lang(z) else z
  set_arg <- function(call, nm, val) {
    L <- as.list(call); nmv <- names(L)
    if (nm %in% nmv) L[[which(nmv == nm)[1]]] <- val else L <- c(L, setNames(list(val), nm))
    as.call(L)
  }
  drop_args <- function(call, nms) {
    L <- as.list(call); nmv <- names(L)
    keep <- !(nmv %in% nms)
    as.call(L[keep])
  }
  
  # x argument
  cl <- set_arg(cl, "x", to_lang(x_expr))
  
  # xreg handling
  if (is.null(xreg_expr)) {
    cl <- drop_args(cl, c("xreg", "regression.usertype"))
  } else if (!isTRUE(is.na(xreg_expr))) {
    cl <- set_arg(cl, "xreg", to_lang(xreg_expr))
  }
  
  # normalize engine: from model if "auto"
  eng_model <- .engine_used(m)  # "seats" or "x11" or "unknown"
  eng <- if (engine == "auto") eng_model else engine
  
  # remove any pre-existing engine args, then add only what we need
  cl <- drop_args(cl, c("x11", "seats", "seats.noadmiss"))
  if (eng == "x11") {
    cl <- set_arg(cl, "x11", "")
  } # SEATS: add nothing — it's default
  
  # optional force.type
  if (isTRUE(include_force)) cl <- set_arg(cl, "force.type", "denton")
  
  paste(deparse(cl, width.cutoff = 500L), collapse = "\n")
}

# --- Row-level “Do Not Adjust” check (exported wrapper) -----------------------

#' Row-level non-adjustment rule (wrapper)
#'
#' Delegates to the internal `.do_not_adjust()` to keep logic in one place.
#' @param row One-row tibble from `res$table`.
#' @return TRUE if the row satisfies the "do not adjust" rule, FALSE otherwise.
#' @export
sa_is_do_not_adjust <- function(row) {
  .do_not_adjust(row)
}

# --- Compare incumbent vs best (thin wrapper) ---------------------------------

#' Comparison wrapper (current vs best)
#'
#' Thin wrapper around `compare_to_current()` returning its result.
#' If `compare_to_current()` is not found, an informative error is raised.
#'
#' @param res Result from `auto_seasonal_analysis()`.
#' @param current_model A fitted `seas` object to compare against.
#' @return List with `decision`, `summary`, and aligned SA/seasonal series.
#' @export
sa_compare <- function(res, current_model) {
  stopifnot(inherits(res, "auto_seasonal_analysis"))
  cmp <- get0("compare_to_current", mode = "function", inherits = TRUE)
  if (is.null(cmp)) {
    stop("compare_to_current() not found in the search path. ",
         "Please provide/attach its implementation or remove this call.")
  }
  cmp(res, current_model)
}

# NOTE:
# The "Existence of Seasonality" HTML card (`sa_existence_card`) and the
# "Engine choice" card are defined in sa-compare.R to avoid duplicate exports.
# This file focuses on diagnostics and helpers only.
