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

# Robust Ljung-Box p-value on model residuals (no summary() scraping)
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
  # Robust: must never crash, even if summary()/qs() is broken for this model.
  tryCatch({
    out <- tryCatch(
      utils::capture.output(suppressMessages(summary(m))),
      error = function(e) character(0)
    )
    
    if (!length(out)) return(NA)
    
    # seasonal has used both phrasings depending on version/output
    any(
      grepl("Model used in SEATS is different", out, fixed = TRUE) |
        grepl("Model used for SEATS is different", out, fixed = TRUE)
    )
  }, error = function(e) {
    NA
  })
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

# Fit one spec, returning 1-2 models (engine seats/x11/auto is respected)
.fit_spec <- function(y, arima_model, transform_fun,
                      auto_outliers = TRUE,
                      include_easter_mode = c("auto","always","off"),
                      easter_len = 15L,
                      td_xreg = NULL,
                      td_usertype = "td",
                      td_name = NA_character_,
                      outlier_types    = c("AO","LS","TC"),
                      outlier_method   = "AddOne",
                      outlier_critical = 4,
                      engine = c("seats","x11","auto")) {
  
  include_easter_mode <- match.arg(include_easter_mode)
  engine <- match.arg(engine)
  
  # ---- y must be a clean ts -------------------------------------------------
  y <- tryCatch(tsbox::ts_ts(y), error = function(e) NULL)
  if (is.null(y) || !inherits(y, "ts")) {
    stop("`.fit_spec()` requires `y` to be convertible to a base `ts`.")
  }
  freq <- stats::frequency(y)
  
  # ---- regression.variables (Easter) ----------------------------------------
  regvars <- character(0)
  if (include_easter_mode %in% c("auto","always")) {
    regvars <- sprintf("easter[%d]", as.integer(easter_len))
  }
  
  call_args <- list(
    x = y,
    transform.function = transform_fun,
    regression.aictest = NULL,
    arima.model        = arima_model
  )
  
  # X-13: auto transform fails with nonpositive values
  if (identical(call_args$transform.function, "auto")) {
    yy <- as.numeric(y)
    if (any(!is.finite(yy)) || any(yy <= 0, na.rm = TRUE)) {
      call_args$transform.function <- "none"
    }
  }
  
  if (length(regvars)) call_args$regression.variables <- regvars
  
  # ---- user xreg: align to y and build mts ----------------------------------
  td_used <- FALSE
  if (!is.null(td_xreg)) {
    xr <- tryCatch(tsbox::ts_ts(td_xreg), error = function(e) NULL)
    
    if (is.null(xr) || !inherits(xr, "ts")) {
      warning("Skipping td_xreg: not ts-boxable / not a ts.")
    } else if (stats::frequency(xr) != freq) {
      warning(sprintf("Skipping td_xreg: frequency mismatch (xreg=%s, y=%s).",
                      stats::frequency(xr), freq))
    } else {
      xr <- tryCatch(stats::window(xr, start = stats::start(y), end = stats::end(y)),
                     error = function(e) NULL)
      
      # IMPORTANT: for multicol xreg, use NROW not length()
      if (is.null(xr) || NROW(xr) != length(y)) {
        warning("Skipping td_xreg: could not align to y (length/start/end mismatch).")
      } else {
        xmat <- as.matrix(xr)  # preserves multicol
        if (is.null(colnames(xmat))) {
          colnames(xmat) <- paste0("xreg", seq_len(ncol(xmat)))
        }
        xreg_y <- stats::ts(xmat, start = stats::start(y), frequency = freq)
        
        call_args$xreg <- xreg_y
        call_args$regression.usertype <- rep(td_usertype, ncol(xreg_y))
        td_used <- TRUE
      }
    }
  }
  
  # ---- outliers --------------------------------------------------------------
  if (isTRUE(auto_outliers)) {
    call_args$outlier          <- ""
    call_args$outlier.types    <- outlier_types
    call_args$outlier.method   <- outlier_method
    call_args$outlier.critical <- outlier_critical
  }
  
  # ---- helper: run seas safely ----------------------------------------------
  .last_err <- NULL
  .run_try <- function(args) {
    z <- try(do.call(seasonal::seas, args), silent = TRUE)
    if (inherits(z, "try-error")) {
      .last_err <<- as.character(z)
      return(NULL)
    }
    z
  }
  
  # ---- SEATS padding: extend xreg into forecast horizon ----------------------
  # X-13/SEATS often forecasts ~3 years; user xreg must exist for that horizon.
  # Default strategy:
  # - pulse/moving-holiday-like usertypes: pad with 0
  # - otherwise: repeat last full year as pragmatic default
  .extend_xreg_for_seats <- function(xreg_ts, lead_n, usertype) {
    if (is.null(xreg_ts) || lead_n <= 0) return(xreg_ts)
    
    x <- as.matrix(xreg_ts)
    if (NROW(x) < 1L) return(xreg_ts)
    
    ut <- tolower(usertype %||% "")
    is_pulse <- grepl("holiday|easter|diwali|ramadan|christmas|cny|lunar|pulse", ut)
    
    if (is_pulse) {
      pad <- matrix(0, nrow = lead_n, ncol = ncol(x))
    } else {
      k <- min(stats::frequency(xreg_ts), NROW(x))
      base <- x[(NROW(x) - k + 1L):NROW(x), , drop = FALSE]
      pad <- base[rep(seq_len(NROW(base)), length.out = lead_n), , drop = FALSE]
    }
    
    colnames(pad) <- colnames(x)
    x2 <- rbind(x, pad)
    stats::ts(x2, start = stats::start(xreg_ts), frequency = stats::frequency(xreg_ts))
  }
  
  # ---- build first attempt args (explicit engine choice) ---------------------
  make_args_for_engine <- function(engine_choice) {
    args <- call_args
    if (engine_choice == "x11") {
      args$x11 <- ""
    } else if (engine_choice == "seats") {
      args$seats <- ""
      args$seats.noadmiss <- "no"
    }
    args
  }
  
  out <- list()
  
  # Decide attempt order
  attempt_engines <- switch(
    engine,
    seats = "seats",
    x11   = "x11",
    auto  = c("seats", "x11")
  )
  
  for (eng_try in attempt_engines) {
    args1 <- make_args_for_engine(eng_try)
    
    # only pad when attempting SEATS (not X11)
    if (td_used && identical(eng_try, "seats")) {
      lead_n <- as.integer(3L * freq)
      args1$xreg <- .extend_xreg_for_seats(args1$xreg, lead_n = lead_n, usertype = td_usertype)
      args1$forecast.maxlead <- lead_n
      args1$forecast.maxback <- 0L
    }
    
    run1 <- .run_try(args1)
    
    # Fallback 1: drop usertype tokens (some X-13 builds are picky)
    if (is.null(run1) && td_used) {
      alt1 <- args1
      alt1$regression.usertype <- NULL
      run1 <- .run_try(alt1)
    }
    
    # Fallback 2: conservative token "td"
    if (is.null(run1) && td_used) {
      alt2 <- args1
      alt2$regression.usertype <- rep("td", ncol(args1$xreg %||% matrix(NA_real_, 0, 1)))
      run1 <- .run_try(alt2)
    }
    
    out <- c(out, list(list(
      model       = run1,
      with_td     = td_used,
      td_name     = td_name %||% NA_character_,
      with_easter = length(regvars) > 0,
      engine      = eng_try,
      err         = if (is.null(run1)) (.last_err %||% "unknown error") else NA_character_
    )))
  }
  
  out
}


.seats_has_seasonal <- function(m) {
  # IMPORTANT:
  # - For X-11 models, a SEATS seasonal component is not defined.
  # - Returning FALSE for non-SEATS engines leads to misleading UI
  #   ("SEATS seasonal component: absent") even when SEATS wasn't used.
  eng <- tryCatch(.engine_used(m), error = function(e) NA_character_)
  if (!identical(eng, "seats")) return(NA)
  !inherits(try(seasonal::series(m, "seats.seasonal"), silent = TRUE), "try-error")
}


#' Build user-supplied calendar regressors (Genhol-style)
#'
#' Helper to create *generic* calendar regressors without relying on any
#' country-specific datasets shipped with the package.
#'
#' Supports:
#' - Precomputed regressors (`td_candidates`): named list of ts-boxable series
#' - Moving-holiday regressors (`holidays`): built via [seasonal::genhol()]
#'
#' The output is directly consumable by [auto_seasonal_analysis()] via its
#' `td_candidates` argument.
#'
#' @param y A `ts` (or ts-boxable) target series used for alignment.
#' @param td_candidates Optional named list of precomputed regressors (ts-boxable).
#' @param holidays Optional list defining moving-holiday regressors. Each element may be:
#'   - a `Date` vector (holiday dates), or
#'   - a list with fields `dates` (Date), `start` (int), `end` (int),
#'     `center` (chr), `name` (chr)
#'   Windows are in days relative to the holiday date. Defaults: `start=-7`, `end=0`.
#' @param frequency Optional frequency to use for holiday regressors (defaults to `frequency(y)`).
#' @param td_usertype X-13 usertype label for *all* returned regressors (default "holiday").
#' @param default_center Default centering for holiday regressors.
#'
#' @return A named list of `ts` regressors aligned to `y`. Attribute `td_usertype`
#'   is attached for downstream use.
#' @export
build_user_xreg <- function(y,
                            td_candidates = NULL,
                            holidays = NULL,
                            frequency = NULL,
                            td_usertype = "holiday",
                            default_center = c("calendar","mean","none")) {
  y <- .as_ts(y)
  default_center <- match.arg(default_center)
  if (is.null(frequency)) frequency <- stats::frequency(y)
  
  out <- list()
  
  # 1) Precomputed regressors -------------------------------------------------
  if (!is.null(td_candidates)) {
    if (!is.list(td_candidates)) stop("`td_candidates` must be a list.")
    out <- c(out, td_candidates)
  }
  
  # 2) Moving-holiday regressors (Genhol-style) ------------------------------
  if (!is.null(holidays)) {
    if (!is.list(holidays)) holidays <- list(holidays)
    
    for (i in seq_along(holidays)) {
      h <- holidays[[i]]
      
      # Allow bare Date vector
      if (inherits(h, "Date")) h <- list(dates = h)
      if (!is.list(h)) {
        warning(sprintf("Skipping holidays[[%d]]: not a Date vector or list.", i))
        next
      }
      
      dates <- h$dates %||% h$date %||% NULL
      if (is.null(dates) || !inherits(dates, "Date")) {
        warning(sprintf("Skipping holidays[[%d]]: missing `dates` (Date vector).", i))
        next
      }
      
      start  <- as.integer(h$start %||% -7L)
      end    <- as.integer(h$end   %||%  0L)
      center <- as.character(h$center %||% default_center)
      name   <- as.character(h$name %||% paste0("hol", i))
      
      xr <- tryCatch(
        seasonal::genhol(
          x = dates,
          start = start,
          end = end,
          frequency = as.integer(frequency),
          center = center
        ),
        error = function(e) {
          warning(sprintf("Skipping holiday '%s': genhol() failed (%s).", name, e$message))
          NULL
        }
      )
      if (is.null(xr)) next
      
      # Align to y (frequency and window). Drop if incompatible.
      xr <- .align_xreg_to_y_fill0(xr, y, fill = 0)
      if (is.null(xr)) {
        warning(sprintf("Skipping holiday '%s': could not align to `y`.", name))
        next
      }
      
      out[[name]] <- xr
    }
  }
  
  # Final normalization (names, alignment checks) ----------------------------
  out <- if (length(out)) .normalize_td_candidates(out, y = y, td_usertype = td_usertype) else NULL
  out
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
# Align an xreg ts to y and fill missing periods with `fill` (default 0).
# Keeps column names and returns an mts with the same span as y.
# Used for user-supplied calendar regressors (e.g., genhol()) where pulses
# may lie outside the sample window.
.align_xreg_to_y_fill0 <- function(xreg, y, fill = 0) {
  y <- tryCatch(tsbox::ts_ts(y), error = function(e) NULL)
  xr <- tryCatch(tsbox::ts_ts(xreg), error = function(e) NULL)

  if (is.null(y) || !inherits(y, "ts")) return(NULL)
  if (is.null(xr) || !inherits(xr, "ts")) return(NULL)

  fy <- stats::frequency(y)
  if (stats::frequency(xr) != fy) return(NULL)

  # target timeline (robust against floating point quirks)
  ty <- round(stats::time(y), 8)
  tx <- round(stats::time(xr), 8)

  X  <- as.matrix(xr)
  k  <- ncol(X)
  out <- matrix(fill, nrow = length(y), ncol = k)
  colnames(out) <- colnames(X)

  pos <- match(tx, ty)
  ok  <- which(!is.na(pos))
  if (length(ok)) out[pos[ok], ] <- X[ok, , drop = FALSE]

  stats::ts(out, start = stats::start(y), frequency = fy)
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
.fmtP <- function(p) ifelse(is.na(p), "\u2014", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
.num  <- function(x, d = 2) ifelse(is.na(x), "\u2014", sprintf(paste0("%.", d, "f"), x))
.pct  <- function(x, digits = 1) ifelse(is.na(x), "NA", sprintf("%.*f%%", digits, x))
.num_safe <- function(x, d = 2) {
  y <- suppressWarnings(as.numeric(x))
  ifelse(is.finite(y), sprintf(paste0("%.", d, "f"), y), "\u2014")
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
  if (is.na(p)) return("\u2014")
  if (p < threshold) {
    sprintf("<span style='color:#b91c1c;font-weight:600;'>\u274C %s</span>", .fmtP(p))
  } else {
    sprintf("<span style='color:#065f46;font-weight:600;'>\u2705 %s</span>", .fmtP(p))
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
  if (is.na(tf) || tf == "") return(if (!is.na(fallback)) tolower(fallback) else "\u2014")
  if (tf == "auto" && !is.na(fallback)) return(tolower(fallback))
  if (tf %in% c("log", "none")) return(tf)
  if (grepl("iofile|\\.html$|[\\\\/]", tf)) return(if (!is.na(fallback)) tolower(fallback) else "\u2014")
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

#' Build user-supplied calendar regressors (Genhol-style)
#'
#' This helper lets users provide *generic* calendar regressors without relying
#' on any country-specific datasets shipped with the package.
#'
#' Supports:
#' - Precomputed regressors (`td_candidates`): named list of `ts`-boxable series
#' - Moving-holiday regressors (`holidays`): built via [seasonal::genhol()]
#'
#' The output is directly consumable by [auto_seasonal_analysis()] via its
#' `td_candidates` argument.
#'
#' @param y A `ts` (or ts-boxable) target series used for alignment.
#' @param td_candidates Optional named list of precomputed regressors (ts-boxable).
#' @param holidays Optional list defining moving-holiday regressors. Each element may be:
#'   - a `Date` vector (holiday dates), or
#'   - a list with fields `dates` (Date), `start` (int), `end` (int), `center` (chr), `name` (chr)
#'   Windows are in days relative to the holiday date. Defaults: `start=-7`, `end=0`.
#' @param frequency Optional frequency to use for holiday regressors (defaults to `frequency(y)`).
#' @param td_usertype X-13 usertype label for *all* returned regressors (default `"holiday"`).
#' @param default_center Default centering for holiday regressors (default `"calendar"`).
#'
#' @return A named list of `ts` regressors aligned to `y`. Attribute `td_usertype`
#'   is attached for downstream use.
#' @export
build_user_xreg <- function(y,
                            td_candidates = NULL,
                            holidays = NULL,
                            frequency = NULL,
                            td_usertype = "holiday",
                            default_center = c("calendar","mean","none")) {
  y <- .as_ts(y)
  default_center <- match.arg(default_center)
  if (is.null(frequency)) frequency <- stats::frequency(y)
  
  out <- list()
  
  # 1) Precomputed regressors -------------------------------------------------
  if (!is.null(td_candidates)) {
    if (!is.list(td_candidates)) stop("`td_candidates` must be a list.")
    out <- c(out, td_candidates)
  }
  
  # 2) Moving-holiday regressors (Genhol-style) ------------------------------
  if (!is.null(holidays)) {
    if (!is.list(holidays)) holidays <- list(holidays)
    
    for (i in seq_along(holidays)) {
      h <- holidays[[i]]
      
      # Allow bare Date vector
      if (inherits(h, "Date")) h <- list(dates = h)
      
      if (!is.list(h)) {
        warning(sprintf("Skipping holidays[[%d]]: not a Date vector or list.", i))
        next
      }
      
      dates <- h$dates %||% h$date %||% NULL
      if (is.null(dates) || !inherits(dates, "Date")) {
        warning(sprintf("Skipping holidays[[%d]]: missing `dates` (Date vector).", i))
        next
      }
      
      start  <- as.integer(h$start %||% -7L)
      end    <- as.integer(h$end   %||%  0L)
      center <- as.character(h$center %||% default_center)
      name   <- as.character(h$name %||% paste0("hol", i))
      
      xr <- tryCatch(
        seasonal::genhol(
          x = dates,
          start = start,
          end = end,
          frequency = as.integer(frequency),
          center = center
        ),
        error = function(e) {
          warning(sprintf("Skipping holiday '%s': genhol() failed (%s).", name, e$message))
          NULL
        }
      )
      if (is.null(xr)) next
      
      # Align to y (frequency and window). Drop if incompatible.
      xr <- .align_xreg_to_y_fill0(xr, y, fill = 0)
      if (is.null(xr)) {
        warning(sprintf("Skipping holiday '%s': could not align to `y`.", name))
        next
      }
      
      out[[name]] <- xr
    }
  }
  
  # Final normalization (names, alignment checks)
  out <- if (length(out)) .normalize_td_candidates(out, y = y, td_usertype = td_usertype) else NULL
  out
}
