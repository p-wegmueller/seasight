# --- Utilities -----------------------------------------------------------------

.m7_stat <- function(m) {
  # X-11 M7 statistic from an X-11 decomposition of the model's original series
  tryCatch(
    seasonal::udg(seasonal::seas(seasonal::original(m), x11 = ""), "f3.m07"),
    error = function(e) NA_real_
  )
}

.ids_flag <- function(m) {
  # ONS "identifiable seasonality" flag from X-11 on the original series
  out <- tryCatch(
    seasonal::udg(seasonal::seas(seasonal::original(m), x11 = ""), "f2.idseasonal"),
    error = function(e) NA_character_
  )
  as.character(out)
}

# Robust Ljung–Box p-value on model residuals (no summary() scraping)
.lb_p <- function(m, lag = NULL, adjust_df = TRUE) {
  e <- try(stats::residuals(m), silent = TRUE)
  if (inherits(e, "try-error")) return(NA_real_)
  e <- stats::na.omit(as.numeric(e))
  n <- length(e)
  if (n < 8) return(NA_real_)  # too short
  
  f <- try(stats::frequency(seasonal::original(m)), silent = TRUE)
  if (inherits(f, "try-error") || !is.finite(f)) f <- 12L
  
  if (is.null(lag)) {
    lag <- min(max(2L * f, 10L), 24L, max(1L, floor(n / 5)))
  }
  lag <- max(1L, min(as.integer(lag), n - 1L))
  
  k <- 0L
  if (isTRUE(adjust_df)) {
    nm <- names(stats::coef(m))
    if (length(nm)) {
      k <- sum(grepl("^(AR|MA|SAR|SMA|AR-Nonseasonal|MA-Nonseasonal|AR-Seasonal|MA-Seasonal)", nm))
    }
  }
  
  suppressWarnings(
    tryCatch(
      stats::Box.test(e, lag = lag, type = "Ljung-Box", fitdf = k)$p.value,
      error = function(...) NA_real_
    )
  )
}

# Residual seasonality (QS) on the SA series, via both engines
.qs_on_sa_both <- function(m) {
  sa <- try(seasonal::final(m), silent = TRUE)
  if (inherits(sa, "try-error") || is.null(sa)) {
    return(tibble::tibble(QS_p_x11 = NA_real_, QS_p_seats = NA_real_, QS_p = NA_real_))
  }
  
  p_x11 <- try({
    mx <- seasonal::seas(sa, x11 = "")
    as.numeric(seasonal::qs(mx)["qsori", "p-val"])
  }, silent = TRUE)
  if (inherits(p_x11, "try-error")) p_x11 <- NA_real_
  
  p_seats <- try({
    ms <- seasonal::seas(sa)  # default SEATS
    as.numeric(seasonal::qs(ms)["qsori", "p-val"])
  }, silent = TRUE)
  if (inherits(p_seats, "try-error")) p_seats <- NA_real_
  
  overall <- if (all(is.na(c(p_x11, p_seats)))) NA_real_ else suppressWarnings(min(p_x11, p_seats, na.rm = TRUE))
  
  tibble::tibble(
    QS_p_x11   = p_x11,
    QS_p_seats = p_seats,
    QS_p       = overall
  )
}

.ts_or_null <- function(x) tryCatch(tsbox::ts_ts(x), error = function(e) NULL)
.pc_or_null <- function(x) tryCatch(tsbox::ts_pc(x), error = function(e) NULL)

.as_ts <- function(y) {
  if (inherits(y, "ts")) return(y)
  if (inherits(y, c("data.frame","tbl_df","tsibble","xts","zoo"))) return(tsbox::ts_ts(y))
  stop("`y` must be a ts or convertible object.")
}

.detect_freq <- function(y) stats::frequency(y)

.safe_log_transform <- function(y) {
  if (all(is.finite(y)) && min(y, na.rm = TRUE) > 0) "log" else "none"
}

.has_seats_model_switch_msg <- function(m) {
  any(grepl("Model used for SEATS is different",
            capture.output(suppressMessages(summary(m))), fixed = TRUE))
}

# QS on the ORIGINAL series, tested with both engines
.qs_original <- function(m) {
  x_orig <- try(seasonal::original(m), silent = TRUE)
  if (inherits(x_orig, "try-error") || is.null(x_orig)) {
    return(tibble::tibble(QSori_p_x11 = NA_real_, QSori_p_seats = NA_real_, QSori_p = NA_real_))
  }
  
  mx11  <- try(seasonal::seas(x_orig, x11 = ""), silent = TRUE)
  mseas <- try(seasonal::seas(x_orig),            silent = TRUE)
  
  p_x11 <- if (!inherits(mx11, "try-error"))
    tryCatch(as.numeric(seasonal::qs(mx11)["qsori","p-val"]), error = function(e) NA_real_) else NA_real_
  p_seats <- if (!inherits(mseas, "try-error"))
    tryCatch(as.numeric(seasonal::qs(mseas)["qsori","p-val"]), error = function(e) NA_real_) else NA_real_
  
  overall <- if (all(is.na(c(p_x11, p_seats)))) NA_real_ else suppressWarnings(min(p_x11, p_seats, na.rm = TRUE))
  
  tibble::tibble(
    QSori_p_x11   = p_x11,
    QSori_p_seats = p_seats,
    QSori_p       = overall
  )
}

# QS sensitivity to X-11 seasonal moving average choice
.qs_x11_sma <- function(m, seasonalma_opts = c("stable","s3x3","s3x9","s3x15")) {
  p <- vapply(seasonalma_opts, function(opt) {
    tryCatch(
      {
        mx <- seasonal::seas(seasonal::final(m), x11 = "", x11.seasonalma = opt)
        as.numeric(seasonal::qs(mx)["qsori","p-val"])
      },
      error = function(e) NA_real_
    )
  }, numeric(1L))
  out <- if (all(is.na(p))) NA_real_ else suppressWarnings(min(p, na.rm = TRUE))
  tibble::tibble(QS_p_x11_min_sma = out)
}

.default_specs <- function(freq) {
  base <- c("(0 1 1)(0 1 1)","(2 1 0)(0 1 1)","(0 1 2)(0 1 1)","(0 1 1)(0 1 2)","(1 1 1)(0 1 1)")
  if (freq == 12) base <- c(base, "(1 1 0)(0 1 1)")
  unique(base)
}

.fivebest_specs <- function(y) {
  fb <- try(seasonal::fivebestmdl(seasonal::seas(y)), silent = TRUE)
  if (inherits(fb, "try-error") || is.null(fb)) return(character(0))
  fb <- tibble::as_tibble(fb)
  order_cols <- c("p","d","q","P","D","Q")
  if (all(order_cols %in% names(fb))) {
    return(unique(sprintf("(%d %d %d)(%d %d %d)", fb$p, fb$d, fb$q, fb$P, fb$D, fb$Q)))
  }
  guess <- intersect(names(fb), c("arima","model","spec","arima.model"))
  if (length(guess)) return(unique(fb[[guess[1]]]))
  character(0)
}

.is_arima_term <- function(nm) {
  grepl("^(AR|MA|SAR|SMA|AR-Seasonal|MA-Seasonal|AR-Nonseasonal|MA-Nonseasonal)", nm)
}

.extract_arima_str <- function(code_str) {
  m <- stringr::str_match(code_str, 'arima\\.model\\s*=\\s*"([^"]+)"')
  ifelse(is.na(m[,2]), NA_character_, m[,2])
}

.arima_display <- function(best_row, best_spec_code) {
  s <- suppressWarnings(tryCatch(.extract_arima_str(best_spec_code), error = function(e) NA_character_))
  if (is.character(s) && nzchar(s)) return(s)
  # fallback to table columns
  ar <- NA_character_
  if ("ARIMA_disp" %in% names(best_row)) ar <- best_row$ARIMA_disp[1]
  if (!is.character(ar) || !nzchar(ar))  ar <- best_row$arima %||% NA_character_
  if (is.character(ar) && nzchar(ar)) return(ar)
  "n/a"
}

# Coefficient p-values (z-test) using m$est$se where available
.reg_pvals <- function(m) {
  cf <- try(stats::coef(m), silent = TRUE)
  se <- try(m$est$se,        silent = TRUE)
  if (inherits(cf,"try-error")) return(tibble::tibble(var=character(), p=double()))
  est <- as.numeric(cf); nm <- names(cf)
  seval <- if (inherits(se, "try-error") || is.null(se)) rep(NA_real_, length(est)) else as.numeric(se)
  z <- est / seval
  p <- ifelse(is.finite(z), 2 * stats::pnorm(-abs(z)), NA_real_)
  tibble::tibble(var = nm, p = as.numeric(p))
}

# Fit one spec, returning 1–2 models (engine seats/x11/auto is respected)
.fit_spec <- function(y, arima_model, transform_fun,
                      auto_outliers = TRUE,
                      include_easter_mode = c("auto","always","off"),
                      easter_len = 15L,
                      td_xreg = NULL, td_usertype = "td",
                      outlier_types    = c("AO","LS","TC"),
                      outlier_method   = "AddOne",
                      outlier_critical = 4,
                      engine = c("seats","x11","auto")) {
  
  include_easter_mode <- match.arg(include_easter_mode)
  engine <- match.arg(engine)
  
  # regression variables (Easter handling)
  regvars <- character(0)
  if (include_easter_mode %in% c("auto","always")) {
    regvars <- sprintf("easter[%d]", as.integer(easter_len))  # X-13 will drop if not useful
  }
  
  call_args <- list(
    x = y,
    transform.function = transform_fun,
    regression.aictest = NULL,
    arima.model        = arima_model
  )
  
  if (length(regvars)) call_args$regression.variables <- regvars
  if (!is.null(td_xreg)) {
    call_args$xreg <- td_xreg
    call_args$regression.usertype <- td_usertype
  }
  if (isTRUE(auto_outliers)) {
    call_args$outlier          <- ""
    call_args$outlier.types    <- outlier_types
    call_args$outlier.method   <- outlier_method
    call_args$outlier.critical <- outlier_critical
  }
  
  if (engine == "x11") call_args$x11 <- "" else if (engine == "seats") call_args$seats <- ""
  
  out <- list()
  
  run1 <- try(do.call(seasonal::seas, call_args), silent = TRUE)
  if (!inherits(run1, "try-error")) {
    out <- c(out, list(list(model = run1,
                            with_td = !is.null(td_xreg),
                            with_easter = length(regvars) > 0)))
  }
  
  if (engine == "auto") {
    alt_args <- call_args
    if ("x11" %in% names(alt_args)) { alt_args$x11 <- NULL; alt_args$seats <- "" }
    else                            { alt_args$seats <- NULL; alt_args$x11 <- "" }
    run2 <- try(do.call(seasonal::seas, alt_args), silent = TRUE)
    if (!inherits(run2, "try-error")) {
      out <- c(out, list(list(model = run2,
                              with_td = !is.null(td_xreg),
                              with_easter = length(regvars) > 0)))
    }
  }
  
  out
}

.seats_has_seasonal <- function(m) {
  !inherits(try(seasonal::series(m, "seats.seasonal"), silent = TRUE), "try-error")
}

# Robust ARIMA string extraction (no summary() scraping)
.arima_string <- function(m) {
  # 1) Preferred: SEASONAL UDG "arimamdl" (6 numbers or already formatted)
  s <- tryCatch(seasonal::udg(m, "arimamdl", fail = FALSE), error = function(e) NULL)
  if (is.character(s) && length(s) == 1L) {
    if (grepl("\\(", s, perl = TRUE)) return(trimws(s))  # already "(p d q)(P D Q)"
    nums <- suppressWarnings(as.integer(strsplit(s, "\\s+")[[1]]))
    if (length(nums) >= 6L && all(is.finite(nums[1:6])))
      return(sprintf("(%d %d %d)(%d %d %d)", nums[1], nums[2], nums[3], nums[4], nums[5], nums[6]))
  }
  
  # 2) Legacy: "arima" (already formatted)
  s2 <- tryCatch(seasonal::udg(m, "arima", fail = FALSE), error = function(e) NULL)
  if (is.character(s2) && length(s2) == 1L &&
      grepl("\\(\\d+\\s+\\d+\\s+\\d+\\)\\(\\d+\\s+\\d+\\s+\\d+\\)", s2))
    return(trimws(s2))
  
  # 3) Fallback: "x13mdl" (6 numbers)
  s3 <- tryCatch(seasonal::udg(m, "x13mdl", fail = FALSE), error = function(e) NULL)
  if (is.character(s3) && length(s3) == 1L) {
    nums <- suppressWarnings(as.integer(strsplit(s3, "\\s+")[[1]]))
    if (length(nums) >= 6L && all(is.finite(nums[1:6])))
      return(sprintf("(%d %d %d)(%d %d %d)", nums[1], nums[2], nums[3], nums[4], nums[5], nums[6]))
  }
  
  NA_character_
}

.coalesce_arima_str <- function(row){
  pick <- function(nm) if (nm %in% names(row)) as.character(row[[nm]][1]) else NA_character_
  cand <- c(pick("ARIMA_disp"), pick("arima"), pick("spec_id"))
  cand <- cand[is.character(cand) & !is.na(cand) & nzchar(cand)]
  if (length(cand)) return(cand[1])
  tryCatch(.format_arima(row), error = function(e) "n/a")  # last resort
}

.sanitize_arima <- function(z) {
  if (is.null(z) || length(z) == 0 || is.na(z) || !nzchar(z)) "n/a" else z
}

# Finance-style diagnostics
.ts_pc_sd <- function(x) stats::sd(tsbox::ts_pc(x), na.rm = TRUE)

.diagnostics_finance <- function(m, orig_ts) {
  x_orig <- .ts_or_null(orig_ts)
  sa     <- tryCatch(seasonal::final(m), error = function(e) NULL)
  x_sa   <- .ts_or_null(sa)
  
  # Try SEATS seasonal first, fallback to X-11 seasonal on ORIGINAL
  seas_comp <- tryCatch(seasonal::series(m, "seats.seasonal"), error = function(e) NULL)
  if (is.null(seas_comp) && !is.null(x_orig)) {
    m_x11_orig <- tryCatch(seasonal::seas(x_orig, x11 = ""), error = function(e) NULL)
    seas_comp  <- tryCatch(seasonal::series(m_x11_orig, "x11.seasonal"), error = function(e) NULL)
  }
  
  vola_orig <- if (!is.null(x_orig)) { x <- .pc_or_null(x_orig); if (is.null(x)) NA_real_ else stats::sd(x, na.rm = TRUE) } else NA_real_
  vola_sa   <- if (!is.null(x_sa))   { x <- .pc_or_null(x_sa);   if (is.null(x)) NA_real_ else stats::sd(x, na.rm = TRUE) }   else NA_real_
  vola_red_pct <- if (is.finite(vola_orig) && vola_orig > 0 && is.finite(vola_sa)) (1 - vola_sa / vola_orig) * 100 else NA_real_
  
  if (!is.null(seas_comp)) {
    seas_ts <- .ts_or_null(seas_comp)
    if (!is.null(seas_ts) && !is.null(x_orig)) {
      seas_amp_abs <- diff(range(seas_ts, na.rm = TRUE))
      seas_amp_pct <- seas_amp_abs / mean(x_orig, na.rm = TRUE) * 100
    } else {
      seas_amp_abs <- 0; seas_amp_pct <- 0
    }
  } else {
    seas_amp_abs <- 0; seas_amp_pct <- 0
  }
  
  tibble::tibble(
    vola_sd_pc_orig    = as.numeric(vola_orig),
    vola_sd_pc_sa      = as.numeric(vola_sa),
    vola_reduction_pct = as.numeric(vola_red_pct),
    seasonal_amp_abs   = as.numeric(seas_amp_abs),
    seasonal_amp_pct   = as.numeric(seas_amp_pct)
  )
}

# Revisions (mean absolute change revisions)
.revision_mae <- function(m) {
  rev_ts <- try(suppressMessages(seasonal::series(m, "history.chngrevisions")), silent = TRUE)
  if (inherits(rev_ts, "try-error")) return(tibble::tibble(rev_mae = NA_real_))
  mx <- try(as.matrix(rev_ts), silent = TRUE)
  if (inherits(mx, "try-error")) return(tibble::tibble(rev_mae = NA_real_))
  tibble::tibble(rev_mae = mean(abs(mx), na.rm = TRUE))
}

# Baseline distances
.ts_align <- function(a, b) {
  ix <- tsbox::ts_span(tsbox::ts_c(a), start = max(stats::start(a), stats::start(b)),
                       end = min(stats::end(a), stats::end(b)))
  list(a = tsbox::ts_pick(a, ix), b = tsbox::ts_pick(b, ix))
}

.align_pair <- function(x, y) {
  if (is.null(x) || is.null(y)) return(NULL)
  if (!is.finite(stats::frequency(x)) || !is.finite(stats::frequency(y))) return(NULL)
  if (stats::frequency(x) != stats::frequency(y)) return(NULL)
  
  z <- try(suppressWarnings(stats::ts.intersect(a = x, b = y)), silent = TRUE)
  if (inherits(z, "try-error")) return(NULL)
  
  Z <- as.matrix(z)
  if (nrow(Z) == 0L) return(NULL)
  list(a = as.numeric(Z[, "a"]), b = as.numeric(Z[, "b"]))
}

.dist_vs_baseline <- function(m, prev_sa = NULL, prev_seasonal = NULL) {
  out <- tibble::tibble(
    dist_sa_L1    = NA_real_,
    dist_seas_RMS = NA_real_,
    corr_seas     = NA_real_
  )
  
  new_sa <- try(seasonal::final(m), silent = TRUE)
  if (!inherits(new_sa, "try-error") && !is.null(prev_sa)) {
    al <- .align_pair(new_sa, prev_sa)
    if (!is.null(al) && length(al$a) == length(al$b) && length(al$a) > 0) {
      out$dist_sa_L1 <- mean(abs(al$a - al$b), na.rm = TRUE)
    }
  }
  
  new_seas <- try(seasonal::series(m, "seasonal"), silent = TRUE)
  if (inherits(new_seas, "try-error") || is.null(new_seas)) {
    new_seas <- try(seasonal::series(m, "seats.seasonal"), silent = TRUE)
    if (inherits(new_seas, "try-error")) new_seas <- NULL
  }
  
  if (!is.null(new_seas) && !is.null(prev_seasonal)) {
    al <- .align_pair(new_seas, prev_seasonal)
    if (!is.null(al) && length(al$a) == length(al$b) && length(al$a) > 1) {
      out$dist_seas_RMS <- sqrt(mean((al$a - al$b)^2, na.rm = TRUE))
      out$corr_seas     <- suppressWarnings(stats::cor(al$a, al$b, use = "complete.obs"))
    }
  }
  
  out
}

.flag_tests <- function(row) {
  seasonal_ids   <- identical(row$IDS, "yes")
  seasonal_qsori <- is.finite(row$QSori_p) && row$QSori_p < 0.10
  seasonal_m7s   <- is.finite(row$M7)       && row$M7 < 0.90
  seasonal_m7w   <- is.finite(row$M7)       && row$M7 < 1.05
  tibble::tibble(seasonal_ids, seasonal_qsori, seasonal_m7s, seasonal_m7w)
}

seasonality_summary <- function(tbl, majority = 0.6) {
  flags <- purrr::pmap_dfr(tbl, ~ .flag_tests(tibble::tibble(...)))
  share_ids   <- mean(flags$seasonal_ids,   na.rm = TRUE)
  share_qsori <- mean(flags$seasonal_qsori, na.rm = TRUE)
  share_m7s   <- mean(flags$seasonal_m7s,   na.rm = TRUE)
  share_m7w   <- mean(flags$seasonal_m7w,   na.rm = TRUE)
  adjust <- (share_ids >= majority) || ((share_qsori + share_m7s) >= majority)
  borderline <- (!adjust) && (max(share_ids, share_qsori, share_m7w, na.rm = TRUE) >= 0.4)
  call <- if (adjust) "ADJUST" else if (borderline) "BORDERLINE" else "DO_NOT_ADJUST"
  tibble::tibble(call_overall = call, share_ids, share_qsori, share_m7_strong = share_m7s, share_m7_weak = share_m7w)
}

.do_not_adjust <- function(row) {
  no_ids <- is.character(row$IDS) && tolower(row$IDS) %in% c("no","none")
  m7_no  <- is.finite(row$M7) && row$M7 >= 1.05
  qsori  <- suppressWarnings(pmin(row$QSori_p_x11, row$QSori_p_seats, na.rm = TRUE))
  if (!is.finite(qsori)) qsori <- NA_real_
  qs_ok  <- is.na(qsori) || qsori >= 0.10
  
  no_seats <- identical(row$SEATS_has_seasonal, FALSE)
  weak_amp <- is.finite(row$seasonal_amp_pct)   && row$seasonal_amp_pct   < 1
  low_gain <- is.finite(row$vola_reduction_pct) && row$vola_reduction_pct < 5
  
  if (no_seats && qs_ok) return(TRUE)
  no_ids && m7_no && qs_ok && (weak_amp || low_gain)
}

# tiny formatters
.fmtP <- function(p) ifelse(is.na(p), "—", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
.num  <- function(x, d = 2) ifelse(is.na(x), "—", sprintf(paste0("%.", d, "f"), x))
.pct  <- function(x, digits = 1) ifelse(is.na(x), "NA", sprintf("%.*f%%", digits, x))
.num_safe <- function(x, d = 2) {
  y <- suppressWarnings(as.numeric(x))
  ifelse(is.finite(y), sprintf(paste0("%.", d, "f"), y), "—")
}

# --- INTERNAL: normalise TD candidates ----------------------------------------
#' Internal helper: `%||%`
#'
#' Null / empty / all-NA / empty-string coalescing operator.
#'
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x) ||
      length(x) == 0 ||
      all(is.na(x)) ||
      (is.character(x) && length(x) > 0 && all(x == ""))) {
    y
  } else {
    x
  }
}

# Ensure td_candidates is a named list of ts with same freq/length as y
.normalize_td_candidates <- function(td_candidates, y, td_usertype = "td") {
  if (is.null(td_candidates)) return(NULL)
  stopifnot(is.list(td_candidates))
  
  if (is.null(names(td_candidates)) || any(!nzchar(names(td_candidates)))) {
    names(td_candidates) <- paste0("td", seq_along(td_candidates))
  }
  
  yfreq <- stats::frequency(y); tspy <- stats::tsp(y)
  
  td_candidates <- lapply(td_candidates, function(x) {
    if (is.null(x)) return(NULL)
    xt <- try(tsbox::ts_ts(x), silent = TRUE)
    if (inherits(xt, "try-error")) stop("A td_candidates entry is not ts-boxable.")
    if (stats::frequency(xt) != yfreq) stop("All td_candidates must have same frequency as y.")
    stats::window(xt, start = tspy[1], end = tspy[2])
  })
  
  keep <- vapply(td_candidates, function(z) !is.null(z), logical(1))
  td_candidates <- td_candidates[keep]
  if (!length(td_candidates)) return(NULL)
  
  attr(td_candidates, "td_usertype") <- td_usertype
  td_candidates
}

.td_is_compatible <- function(y, xr) {
  if (is.null(xr) || !inherits(xr, "ts")) return(FALSE)
  if (!is.finite(stats::frequency(y)) || !is.finite(stats::frequency(xr))) return(FALSE)
  if (stats::frequency(y) != stats::frequency(xr)) return(FALSE)
  length(y) == length(xr) && all(stats::start(y) == stats::start(xr)) && all(stats::end(y) == stats::end(xr))
}

.flagP <- function(p, threshold = 0.05) {
  if (is.na(p)) return("—")
  if (p < threshold) {
    sprintf("<span style='color:#b91c1c;font-weight:600;'>❌ %s</span>", .fmtP(p))
  } else {
    sprintf("<span style='color:#065f46;font-weight:600;'>✅ %s</span>", .fmtP(p))
  }
}

# --- Coefficient & stats helpers ---------------------------------------------
.sig_stars <- function(p) {
  if (is.na(p)) "" else if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.1) "*" else ""
}

.coef_table_df <- function(m) {
  cf <- try(stats::coef(m), silent = TRUE)
  se <- try(m$est$se,        silent = TRUE)
  if (inherits(cf, "try-error")) return(tibble::tibble())
  nm <- names(cf); est <- as.numeric(cf)
  seval <- if (inherits(se, "try-error") || is.null(se)) rep(NA_real_, length(est)) else as.numeric(se)
  p <- ifelse(is.na(seval), NA_real_, 2 * stats::pnorm(-abs(est / seval)))
  tibble::tibble(term = nm, est = est, se = seval, p = p, stars = vapply(p, .sig_stars, character(1)))
}

.obs_n <- function(m) {
  n <- try(length(stats::na.omit(seasonal::original(m))), silent = TRUE)
  if (!inherits(n, "try-error")) return(n)
  s <- capture.output(suppressMessages(summary(m)))
  ln <- grep("Obs\\.:", s, value = TRUE); if (!length(ln)) return(NA_integer_)
  as.integer(gsub("[^0-9]", "", ln[1]))
}

.shapiro_stat <- function(m) {
  s <- capture.output(suppressMessages(summary(m)))
  ln <- grep("Shapiro \\(normality\\):", s, value = TRUE)
  if (!length(ln)) return(NA_real_)
  as.numeric(sub(".*Shapiro \\(normality\\):\\s*", "", ln[1]))
}

# Robust transform label from a seas() object
.transform_label <- function(m, fallback = NA_character_) {
  tf <- try({
    cl <- seasonal::static(m)
    L  <- as.list(cl)
    v  <- L[["transform.function"]]
    if (is.symbol(v)) as.character(v) else if (is.character(v)) v else NA_character_
  }, silent = TRUE)
  if (inherits(tf, "try-error") || is.null(tf)) tf <- NA_character_
  
  if (is.na(tf)) {
    tf <- tryCatch(as.character(seasonal::out(m, "transform.function")),
                   error = function(e) NA_character_)
  }
  
  tf <- tolower(tf)
  if (is.na(tf) || tf == "") return(if (!is.na(fallback)) tolower(fallback) else "—")
  if (tf == "auto" && !is.na(fallback)) return(tolower(fallback))
  if (tf %in% c("log", "none")) return(tf)
  if (grepl("iofile|\\.html$|[\\\\/]", tf)) return(if (!is.na(fallback)) tolower(fallback) else "—")
  tf
}

.qs_overall_on_SA <- function(m) { .qs_on_sa_both(m)$QS_p }

.sa_pc_stats <- function(x) {
  c(
    mean   = mean(x, na.rm = TRUE),
    sd     = stats::sd(x, na.rm = TRUE),
    median = stats::median(x, na.rm = TRUE),
    min    = min(x, na.rm = TRUE),
    max    = max(x, na.rm = TRUE)
  )
}

# returns "seats" or "x11" (prefers SEATS when both are available)
.engine_choice <- function(m) {
  has_seats <- !inherits(try(seasonal::series(m, "seats.seasonal"), silent = TRUE), "try-error")
  has_x11   <- !inherits(try(seasonal::series(m, "x11.seasonal"),   silent = TRUE), "try-error")
  if (has_seats && !has_x11) "seats"
  else if (has_x11 && !has_seats) "x11"
  else if (has_seats && has_x11) "seats"
  else "seats"
}

.engine_used <- function(m) {
  has_seats <- !inherits(try(seasonal::series(m, "seats.seasonal"), silent = TRUE), "try-error")
  has_x11   <- !inherits(try(seasonal::series(m, "x11.seasonal"),   silent = TRUE), "try-error")
  if (has_seats) return("seats")
  if (has_x11)   return("x11")
  
  cl  <- seasonal::static(m)
  nms <- names(as.list(cl))
  if ("seats" %in% nms && !("x11" %in% nms)) return("seats")
  if ("x11"   %in% nms && !("seats" %in% nms)) return("x11")
  "unknown"
}

# --- ordering helpers for "Top candidates" ------------------------------------

# GLOBAL helper (replace current implementation)

.order_top_candidates <- function(candidates, incumbent_spec_id = NULL, top_n = 5L) {
  stopifnot(is.data.frame(candidates))
  cand <- tibble::as_tibble(candidates)
  
  # rank: prefer 'rank' -> 'score_rank' -> row order
  rank_vec <- if ("rank" %in% names(cand) && any(is.finite(cand$rank))) {
    as.numeric(cand$rank)
  } else if ("score_rank" %in% names(cand) && any(is.finite(cand$score_rank))) {
    as.numeric(cand$score_rank)
  } else {
    seq_len(nrow(cand))
  }
  cand$rank <- rank_vec
  cand$is_best <- cand$rank == min(cand$rank, na.rm = TRUE)
  
  # incumbent flag
  cand$is_incumbent <- FALSE
  if (!is.null(incumbent_spec_id) && "spec_id" %in% names(cand)) {
    inc <- cand$spec_id == incumbent_spec_id
    inc[is.na(inc)] <- FALSE
    cand$is_incumbent <- inc
  } else if ("is_current" %in% names(cand)) {
    inc <- as.logical(cand$is_current)
    inc[is.na(inc)] <- FALSE
    cand$is_incumbent <- inc
  } else if (!is.null(incumbent_spec_id) && "model_label" %in% names(cand)) {
    inc <- cand$model_label == incumbent_spec_id
    inc[is.na(inc)] <- FALSE
    cand$is_incumbent <- inc
  }
  
  cand <- dplyr::arrange(cand, dplyr::desc(is_best), dplyr::desc(is_incumbent), rank)
  out  <- dplyr::slice_head(cand, n = top_n)
  
  # Ensure incumbent shows up at least once
  if (any(cand$is_incumbent, na.rm = TRUE) && !any(out$is_incumbent, na.rm = TRUE)) {
    inc_row <- cand[which(cand$is_incumbent)[1], , drop = FALSE]
    out <- dplyr::bind_rows(out, inc_row)
    if ("spec_id" %in% names(out)) {
      out <- dplyr::distinct(out, spec_id, .keep_all = TRUE)
    } else if (all(c("arima", "with_td") %in% names(out))) {
      out <- dplyr::distinct(out, arima, with_td, .keep_all = TRUE)
    } else if ("model_label" %in% names(out)) {
      out <- dplyr::distinct(out, model_label, .keep_all = TRUE)
    } else {
      out <- dplyr::distinct(out, dplyr::across(dplyr::everything()))
    }
  }
  
  out
}

.should_show_alternative <- function(candidates, incumbent_spec_id = NULL) {
  if (is.null(candidates) || !nrow(candidates)) return(TRUE)
  cand <- tibble::as_tibble(candidates)
  
  # rank again: 'rank' -> 'score_rank' -> row order
  r <- if ("rank" %in% names(cand) && any(is.finite(cand$rank))) {
    as.numeric(cand$rank)
  } else if ("score_rank" %in% names(cand) && any(is.finite(cand$score_rank))) {
    as.numeric(cand$score_rank)
  } else {
    seq_len(nrow(cand))
  }
  best_rank <- min(r, na.rm = TRUE)
  
  inc_rank <- NA_real_
  if (!is.null(incumbent_spec_id) && "spec_id" %in% names(cand)) {
    w <- which(cand$spec_id == incumbent_spec_id)[1]
    if (!is.na(w)) inc_rank <- r[w]
  } else if ("is_current" %in% names(cand)) {
    w <- which(isTRUE(cand$is_current))[1]
    if (!is.na(w)) inc_rank <- r[w]
  }
  
  if (!is.finite(inc_rank)) return(TRUE)
  inc_rank != best_rank
}

.align2 <- function(a, b) {
  a <- .ts_or_null(a); b <- .ts_or_null(b)
  if (is.null(a) || is.null(b)) return(list(ok = FALSE, reason = "missing"))
  
  fa <- stats::frequency(a); fb <- stats::frequency(b)
  if (!is.finite(fa) || !is.finite(fb) || fa != fb)
    return(list(ok = FALSE, reason = "freq_mismatch"))
  
  z <- suppressWarnings(stats::ts.intersect(prev = a, new = b))
  if (is.null(z) || nrow(z) == 0) return(list(ok = FALSE, reason = "no_overlap"))
  
  list(prev = z[, "prev"], new = z[, "new"], ok = TRUE, reason = "ok")
}

# ---- numeric rescalers / weights --------------------------------------------
.rescale_best_high <- function(x) {  # lower is better -> 0..100, higher is better
  x <- suppressWarnings(as.numeric(x))
  fin <- is.finite(x); if (!any(fin)) return(rep(NA_real_, length(x)))
  xr <- range(-x[fin], na.rm = TRUE)
  out <- rep(NA_real_, length(x))
  out[fin] <- if (diff(xr) == 0) 100 else ((-x[fin] - xr[1])/(xr[2] - xr[1])) * 100
  out
}

.std01 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  fin <- is.finite(x); if (!any(fin)) return(rep(NA_real_, length(x)))
  rng <- range(x[fin], na.rm = TRUE)
  out <- rep(NA_real_, length(x))
  out[fin] <- if (diff(rng) == 0) 0 else (x[fin] - rng[1])/(rng[2] - rng[1])
  out
}

.w <- function(lst, nm, default = 0) {
  v <- tryCatch(as.numeric(if (!is.null(lst)) lst[[nm]] else NA_real_), error = function(e) NA_real_)
  if (length(v) != 1L || !is.finite(v)) default else v
}
