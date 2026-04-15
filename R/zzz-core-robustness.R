# Core robustness overrides loaded late in the package build.

.safe_qs_matrix <- function(m) {
  out <- tryCatch(
    suppressWarnings(seasonal::qs(m)),
    error = function(e) NULL
  )
  if (is.null(out)) return(NULL)
  if (is.null(dim(out))) return(NULL)
  out
}

.safe_qs_pval <- function(m, row = "qsori", col = "p-val") {
  q <- .safe_qs_matrix(m)
  if (is.null(q)) return(NA_real_)

  rn <- rownames(q)
  cn <- colnames(q)
  if (is.null(rn) || is.null(cn) || !row %in% rn || !col %in% cn) {
    return(NA_real_)
  }

  val <- suppressWarnings(as.numeric(q[row, col]))
  if (length(val) != 1L || !is.finite(val)) NA_real_ else val
}

.qs_on_sa_both <- function(m) {
  sa <- tryCatch(seasonal::final(m), error = function(e) NULL)
  if (is.null(sa)) {
    return(tibble::tibble(QS_p_x11 = NA_real_, QS_p_seats = NA_real_, QS_p = NA_real_))
  }

  mx <- tryCatch(seasonal::seas(sa, x11 = ""), error = function(e) NULL)
  ms <- tryCatch(seasonal::seas(sa), error = function(e) NULL)

  p_x11 <- if (is.null(mx)) NA_real_ else .safe_qs_pval(mx)
  p_seats <- if (is.null(ms)) NA_real_ else .safe_qs_pval(ms)
  overall <- if (all(is.na(c(p_x11, p_seats)))) NA_real_ else suppressWarnings(min(p_x11, p_seats, na.rm = TRUE))

  tibble::tibble(QS_p_x11 = p_x11, QS_p_seats = p_seats, QS_p = overall)
}

.qs_original <- function(m) {
  x_orig <- tryCatch(seasonal::original(m), error = function(e) NULL)
  if (is.null(x_orig)) {
    return(tibble::tibble(QSori_p_x11 = NA_real_, QSori_p_seats = NA_real_, QSori_p = NA_real_))
  }

  mx <- tryCatch(seasonal::seas(x_orig, x11 = ""), error = function(e) NULL)
  ms <- tryCatch(seasonal::seas(x_orig), error = function(e) NULL)

  p_x11 <- if (is.null(mx)) NA_real_ else .safe_qs_pval(mx)
  p_seats <- if (is.null(ms)) NA_real_ else .safe_qs_pval(ms)
  overall <- if (all(is.na(c(p_x11, p_seats)))) NA_real_ else suppressWarnings(min(p_x11, p_seats, na.rm = TRUE))

  tibble::tibble(QSori_p_x11 = p_x11, QSori_p_seats = p_seats, QSori_p = overall)
}

.qs_x11_sma <- function(m, seasonalma_opts = c("stable", "s3x3", "s3x9", "s3x15")) {
  sa <- tryCatch(seasonal::final(m), error = function(e) NULL)
  if (is.null(sa)) return(tibble::tibble(QS_p_x11_min_sma = NA_real_))

  p <- vapply(seasonalma_opts, function(opt) {
    mx <- tryCatch(
      seasonal::seas(sa, x11 = "", x11.seasonalma = opt),
      error = function(e) NULL
    )
    if (is.null(mx)) NA_real_ else .safe_qs_pval(mx)
  }, numeric(1L))

  out <- if (all(is.na(p))) NA_real_ else suppressWarnings(min(p, na.rm = TRUE))
  tibble::tibble(QS_p_x11_min_sma = out)
}

.has_seats_model_switch_msg <- function(m) {
  udg <- tryCatch(seasonal::udg(m, fail = FALSE), error = function(e) NULL)
  if (is.null(udg) || !length(udg)) return(NA)

  txt <- paste(c(names(udg), as.character(udg)), collapse = " ")
  if (!nzchar(txt)) return(NA)

  hit <- grepl("Model used (in|for) SEATS is different|SEATS.*model.*different|model.*switch",
               txt, ignore.case = TRUE)
  isTRUE(hit)
}

.obs_n <- function(m) {
  n <- tryCatch(length(stats::na.omit(seasonal::original(m))), error = function(e) NA_integer_)
  if (!is.finite(n)) NA_integer_ else as.integer(n)
}

.shapiro_stat <- function(m) {
  e <- tryCatch(stats::residuals(m), error = function(e) NULL)
  if (is.null(e)) return(NA_real_)
  e <- stats::na.omit(as.numeric(e))
  n <- length(e)
  if (n < 3L) return(NA_real_)
  if (n > 5000L) {
    idx <- unique(as.integer(seq.int(1L, n, length.out = 5000L)))
    e <- e[idx]
  }
  tryCatch(stats::shapiro.test(e)$p.value, error = function(e) NA_real_)
}

#' Align an external regressor to a target series
#'
#' Converts `x` to a base `ts`, checks that it has the same frequency as `y`,
#' windows it to the span of `y`, and returns a `ts`/`mts` with the same start,
#' end and frequency as `y`. Returns `NULL` rather than throwing when alignment
#' is impossible.
#'
#' @param y Target series, a `ts` or object convertible with `tsbox::ts_ts()`.
#' @param x Regressor series, a `ts` or object convertible with `tsbox::ts_ts()`.
#' @return A `ts`/`mts` aligned to `y`, or `NULL` if conversion, frequency or
#'   span alignment fails.
#' @export
sa_align_regressor <- function(y, x) {
  y <- tryCatch(tsbox::ts_ts(y), error = function(e) NULL)
  xr <- tryCatch(tsbox::ts_ts(x), error = function(e) NULL)

  if (is.null(y) || !inherits(y, "ts")) return(NULL)
  if (is.null(xr) || !inherits(xr, "ts")) return(NULL)

  fy <- stats::frequency(y)
  fx <- stats::frequency(xr)
  if (!is.finite(fy) || !is.finite(fx) || fx != fy) return(NULL)

  xr <- tryCatch(
    stats::window(xr, start = stats::start(y), end = stats::end(y)),
    error = function(e) NULL
  )
  if (is.null(xr) || NROW(xr) != length(y)) return(NULL)

  xmat <- as.matrix(xr)
  if (is.null(colnames(xmat))) {
    colnames(xmat) <- paste0("xreg", seq_len(ncol(xmat)))
  }

  stats::ts(xmat, start = stats::start(y), frequency = fy)
}

.normalize_td_candidates <- function(td_candidates, y, td_usertype = "td") {
  if (is.null(td_candidates)) return(NULL)
  if (!is.list(td_candidates)) stop("`td_candidates` must be a list.", call. = FALSE)
  if (!length(td_candidates)) stop("`td_candidates` must not be an empty list.", call. = FALSE)

  nm <- names(td_candidates)
  if (is.null(nm) || length(nm) != length(td_candidates)) {
    nm <- rep("", length(td_candidates))
  }
  missing_nm <- !nzchar(nm)
  nm[missing_nm] <- paste0("td", which(missing_nm))
  names(td_candidates) <- nm

  out <- vector("list", length(td_candidates))
  names(out) <- names(td_candidates)

  for (i in seq_along(td_candidates)) {
    if (is.null(td_candidates[[i]])) {
      stop("`td_candidates` entries must not be NULL.", call. = FALSE)
    }
    xr <- sa_align_regressor(y, td_candidates[[i]])
    if (is.null(xr)) {
      stop("All `td_candidates` entries must align to `y` with the same frequency and span.", call. = FALSE)
    }
    out[[i]] <- xr
  }

  attr(out, "td_usertype") <- td_usertype
  out
}

.align2 <- function(a, b) {
  a <- .ts_or_null(a)
  b <- .ts_or_null(b)
  if (is.null(a) || is.null(b)) return(list(ok = FALSE, reason = "missing"))

  fa <- stats::frequency(a)
  fb <- stats::frequency(b)
  if (!is.finite(fa) || !is.finite(fb) || fa != fb) {
    return(list(ok = FALSE, reason = "freq_mismatch"))
  }

  ta <- stats::time(a)
  tb <- stats::time(b)
  start_time <- max(min(ta, na.rm = TRUE), min(tb, na.rm = TRUE))
  end_time <- min(max(ta, na.rm = TRUE), max(tb, na.rm = TRUE))
  if (!is.finite(start_time) || !is.finite(end_time) || start_time > end_time) {
    return(list(ok = FALSE, reason = "no_overlap"))
  }

  aw <- tryCatch(stats::window(a, start = start_time, end = end_time), error = function(e) NULL)
  bw <- tryCatch(stats::window(b, start = start_time, end = end_time), error = function(e) NULL)
  if (is.null(aw) || is.null(bw)) return(list(ok = FALSE, reason = "no_overlap"))

  z <- tryCatch(
    suppressWarnings(stats::ts.intersect(prev = aw, new = bw)),
    error = function(e) NULL
  )
  if (is.null(z) || NROW(z) == 0L) return(list(ok = FALSE, reason = "no_overlap"))

  Z <- as.matrix(z)
  ok <- stats::complete.cases(Z[, c("prev", "new"), drop = FALSE])
  if (!any(ok)) return(list(ok = FALSE, reason = "only_na_overlap"))

  tt <- stats::time(z)[ok]
  Z <- Z[ok, , drop = FALSE]
  list(
    prev = stats::ts(as.numeric(Z[, "prev"]), start = tt[1], frequency = fa),
    new = stats::ts(as.numeric(Z[, "new"]), start = tt[1], frequency = fa),
    ok = TRUE,
    reason = "ok"
  )
}

.align_pair <- function(x, y) {
  al <- .align2(x, y)
  if (!isTRUE(al$ok)) return(NULL)
  list(a = as.numeric(al$prev), b = as.numeric(al$new))
}
