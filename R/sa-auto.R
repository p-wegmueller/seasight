#' Automatic seasonal analysis (candidate grid + ranking)
#'
#' Runs a grid of X-13ARIMA-SEATS specifications, computes diagnostics
#' (QS tests, residual checks, revision metrics, distance to a baseline
#' model, etc.), and ranks candidates using a composite score. Returns
#' the best model together with a diagnostic table and seasonality call.
#'
#' @param y A time series (`ts`) or an object that can be converted to `ts`
#'   via `tsbox::ts_ts()`.
#' @param specs Optional character vector of ARIMA specifications in the
#'   form `"(p d q)(P D Q)"`. If `NULL`, default specs are chosen based
#'   on the series frequency.
#' @param use_fivebest Logical. If `TRUE`, a small set of data-driven
#'   "five best" specs (à la `seasonal::seas(x, x11 = "")`) is added to
#'   the candidate grid.
#' @param auto_outliers Logical. If `TRUE`, enables automatic outlier
#'   detection in the candidate models.
#' @param transform_fun Transformation applied to the input series before
#'   seasonal adjustment. `"auto"` selects a safe log transform if the
#'   series is strictly positive, `"log"` forces a log transform, and
#'   `"none"` keeps the series in levels.
#' @param td_candidates Optional named list of alternative trading-day
#'   regressors, e.g. `list(wd = wd.m, wd1 = wd1.m)`. Each element should
#'   be a time series aligned with `y`.
#' @param td_usertype Character string passed as `regression.usertype` when
#'   `xreg` is used (default `"td"`).
#' @param include_easter Controls inclusion of Easter regressors:
#'   `"auto"` (default) lets the procedure decide, `"always"` always
#'   includes Easter, `"off"` never includes Easter. A logical value is
#'   also accepted and mapped to `"auto"`/`"off"`.
#' @param easter_len Integer, length (in days) of the Easter effect when
#'   included.
#' @param include_history_top_n Integer, number of top-ranked models for
#'   which revision metrics over the history are computed before the
#'   final ranking step.
#' @param early_period_end Reserved for future use. Optional index or date
#'   marking the end of an early sample period used for stability checks.
#' @param current_model Optional incumbent `seasonal::seas` object used as
#'   a baseline for distance measures and comparison.
#' @param current_sa Optional baseline seasonally adjusted series. If not
#'   supplied, it is extracted from `current_model` when possible.
#' @param current_seasonal Optional baseline seasonal component. If not
#'   supplied, it is extracted from `current_model` (preferring SEATS,
#'   falling back to X-11 if needed).
#' @param w_qs Weight of the QS (seasonality) diagnostics in the composite
#'   ranking score.
#' @param w_stability Weight of residual / stability diagnostics (e.g.
#'   Ljung-Box) in the composite score.
#' @param w_aicc Weight of the information criterion (AICc) in the
#'   composite score.
#' @param w_rev Weight of the revision metric (historical revision MAE) in
#'   the composite score.
#' @param w_dist_sa Weight of the distance between seasonally adjusted
#'   series (candidate vs baseline) in the composite score.
#' @param w_dist_seas Weight of the distance between seasonal components
#'   (candidate vs baseline) in the composite score.
#' @param w_engine Weight / penalty term related to the decomposition
#'   engine (SEATS vs X-11) in the composite score.
#' @param engine Engine preference for candidate models. One of
#'   `"seats"`, `"x11"` or `"auto"` (tries both and lets the ranking
#'   decide, with a small penalty for deviations from the preferred
#'   engine).
#' @param outlier_types Character vector of outlier types to detect,
#'   typically a subset of `c("AO", "LS", "TC")`.
#' @param outlier_method Character scalar giving the outlier detection
#'   method (passed to `seasonal::seas()`, e.g. `"AddOne"`).
#' @param outlier_critical Numeric critical value for outlier detection
#'   (e.g. t- or z-threshold).
#' @param outlier_alpha Optional numeric significance level for outlier
#'   detection. If supplied, it is converted to a two-sided normal cutoff
#'   for `outlier_critical`.
#'
#' @return An object of class `"auto_seasonal_analysis"` with components
#'   such as `best` (best `seas` model), `table` (diagnostic and ranking
#'   table), `specs_tried`, `frequency`, `transform`, `baseline`, and
#'   `seasonality`.
#' @export
auto_seasonal_analysis <- function(y,
                                   specs = NULL,
                                   use_fivebest = TRUE,
                                   auto_outliers = TRUE,
                                   transform_fun = c("auto","log","none"),
                                   td_candidates = NULL,
                                   td_usertype = "td",
                                   include_easter = c("auto","always","off"),
                                   easter_len = 15L,
                                   # diagnostics
                                   include_history_top_n = 10,
                                   early_period_end = NULL, # reserved
                                   # baseline comparison
                                   current_model = NULL,
                                   current_sa = NULL,
                                   current_seasonal = NULL,
                                   # ranking weights
                                   w_qs = 1, w_stability = 1, w_aicc = 1,
                                   w_rev = 0.5, w_dist_sa = 1, w_dist_seas = 0.5,
                                   w_engine = 1,
                                   engine = c("seats","x11","auto"),
                                   outlier_types    = c("AO","LS","TC"),
                                   outlier_method   = "AddOne",
                                   outlier_critical = 4,
                                   outlier_alpha    = NULL) {
  
  # --- small internal helpers --------------------------------------------------
  .aicc <- function(m) {
    # AICc = AIC + [2k(k+1)] / (n - k - 1)
    aic <- suppressWarnings(tryCatch(stats::AIC(m), error = function(e) NA_real_))
    k   <- suppressWarnings(tryCatch(length(stats::coef(m)), error = function(e) NA_integer_))
    n   <- suppressWarnings(tryCatch(length(stats::na.omit(seasonal::original(m))), error = function(e) NA_integer_))
    if (!is.finite(aic) || !is.finite(k) || !is.finite(n) || n <= (k + 1)) return(aic)
    aic + (2 * k * (k + 1)) / (n - k - 1)
  }
  
  # prefer a project-level ranker; else use a robust fallback
  .ranker <- get0(".rank_candidates", mode = "function", inherits = TRUE)
  if (is.null(.ranker)) {
    .ranker <- function(tbl, w_qs, w_stability, w_aicc, w_rev, w_dist_sa, w_dist_seas, w_engine,
                        engine_pref = c("seats","x11","auto")) {
      engine_pref <- match.arg(engine_pref)
      # penalties / normalizations
      qs_pen  <- ifelse(!is.na(tbl$QS_p), (0.10 - pmin(tbl$QS_p, 0.10)) / 0.10, 0.5)     # <10% → penalty up to 1
      lb_pen  <- ifelse(!is.na(tbl$LB_p), (0.05 - pmin(tbl$LB_p, 0.05)) / 0.05, 0.5)     # <5% → penalty up to 1
      lb_pen  <- pmax(lb_pen, 0)                                                          # else 0
      aicc_n  <- .std01(tbl$AICc)                                                         # lower better
      rev_n   <- .std01(tbl$rev_mae)                                                      # lower better
      dsa_n   <- .std01(tbl$dist_sa_L1)                                                   # lower better
      dse_n   <- .std01(tbl$dist_seas_RMS)                                                # lower better
      e_pen   <- ifelse(is.na(tbl$engine), 0.5,
                        ifelse(engine_pref == "auto", ifelse(tbl$engine == "seats", 0, 0.25),
                               ifelse(tbl$engine == engine_pref, 0, 1)))
      score <- w_qs * qs_pen +
        w_stability * lb_pen +
        w_aicc * aicc_n +
        w_rev * rev_n +
        w_dist_sa * dsa_n +
        w_dist_seas * dse_n +
        w_engine * e_pen
      out <- dplyr::mutate(tbl, score = as.numeric(score))
      out <- dplyr::arrange(out, .data$score)
      out$rank <- rank(out$score, ties.method = "min")
      out
    }
  }
  
  # --- Inputs & normalization ---------------------------------------------------
  y <- .as_ts(y)
  freq <- .detect_freq(y)
  
  transform_fun <- match.arg(transform_fun)
  if (identical(transform_fun, "auto")) transform_fun <- .safe_log_transform(y)
  
  include_easter <- if (is.logical(include_easter)) {
    if (include_easter) "auto" else "off"
  } else {
    match.arg(include_easter)
  }
  
  engine <- match.arg(engine)
  
  # normalize & validate TD candidates (names, class, frequency)
  td_candidates <- .normalize_td_candidates(
    td_candidates,
    y = y,
    td_usertype = td_usertype
  )
  
  # if user supplies alpha, convert to two-sided z cutoff
  outlier_critical <- if (!is.null(outlier_alpha)) stats::qnorm(1 - outlier_alpha/2) else outlier_critical
  
  # candidate specs
  cand <- character(0)
  if (isTRUE(use_fivebest)) cand <- .fivebest_specs(y)
  if (is.null(specs)) specs <- .default_specs(freq)
  specs <- unique(c(cand, specs))
  if (!length(specs)) stop("No candidate ARIMA specs available.")
  
  # Baseline (current model) — try to pull SA/seasonal for distances
  if (!is.null(current_model)) {
    current_sa       <- tryCatch(seasonal::final(current_model), error = function(e) current_sa)
    # prefer SEATS seasonal; fall back to X-11 seasonal if needed
    current_seasonal <- tryCatch(seasonal::series(current_model, "seats.seasonal"),
                                 error = function(e) NA)
    if (inherits(current_seasonal, "try-error") || is.null(current_seasonal) || any(is.na(current_seasonal))) {
      current_seasonal <- tryCatch(seasonal::series(current_model, "x11.seasonal"), error = function(e) NULL)
    }
  }
  
  # --- Fit grid: for each spec, fit (i) no-TD, (ii) every TD candidate ---------
  fit_bundles <- purrr::map(specs, function(spec_str) {
    out <- list()
    
    # 1) No TD
    base <- .fit_spec(
      y, spec_str, transform_fun,
      auto_outliers       = auto_outliers,
      include_easter_mode = include_easter,
      easter_len          = easter_len,
      td_xreg             = NULL,                     # << no TD
      td_usertype         = td_usertype,
      outlier_types       = outlier_types,
      outlier_method      = outlier_method,
      outlier_critical    = outlier_critical,
      engine              = engine
    )
    if (length(base)) out <- c(out, lapply(base, function(z){ z$td_name <- NA_character_; z }))
    
    # 2) Each TD candidate
    if (!is.null(td_candidates) && length(td_candidates)) {
      td_usertype_use <- attr(td_candidates, "td_usertype")
      if (is.null(td_usertype_use)) td_usertype_use <- td_usertype
      
      for (nm in names(td_candidates)) {
        tdser <- td_candidates[[nm]]
        with_td <- .fit_spec(
          y, spec_str, transform_fun,
          auto_outliers       = auto_outliers,
          include_easter_mode = include_easter,
          easter_len          = easter_len,
          td_xreg             = tdser,               # << this TD
          td_usertype         = td_usertype_use,
          outlier_types       = outlier_types,
          outlier_method      = outlier_method,
          outlier_critical    = outlier_critical,
          engine              = engine
        )
        if (length(with_td)) out <- c(out, lapply(with_td, function(z){ z$td_name <- nm; z }))
      }
    }
    
    out
  })
  
  # If nothing estimated and engine == "seats", try a fallback cycle with engine="auto"
  any_ok <- any(vapply(
    fit_bundles,
    function(b) any(vapply(b, function(x) inherits(x$model, "seas"), logical(1))),
    logical(1)
  ))
  if (!any_ok && identical(engine, "seats")) {
    fit_bundles <- purrr::map(specs, function(spec_str) {
      out <- list()
      base <- .fit_spec(
        y, spec_str, transform_fun,
        auto_outliers       = auto_outliers,
        include_easter_mode = include_easter,
        easter_len          = easter_len,
        td_xreg             = NULL,
        td_usertype         = td_usertype,
        outlier_types       = outlier_types,
        outlier_method      = outlier_method,
        outlier_critical    = outlier_critical,
        engine              = "auto"
      )
      if (length(base)) out <- c(out, lapply(base, function(z){ z$td_name <- NA_character_; z }))
      
      if (!is.null(td_candidates) && length(td_candidates)) {
        td_usertype_use <- attr(td_candidates, "td_usertype")
        if (is.null(td_usertype_use)) td_usertype_use <- td_usertype
        
        for (nm in names(td_candidates)) {
          tdser <- td_candidates[[nm]]
          with_td <- .fit_spec(
            y, spec_str, transform_fun,
            auto_outliers       = auto_outliers,
            include_easter_mode = include_easter,
            easter_len          = easter_len,
            td_xreg             = tdser,
            td_usertype         = td_usertype_use,
            outlier_types       = outlier_types,
            outlier_method      = outlier_method,
            outlier_critical    = outlier_critical,
            engine              = "auto"
          )
          if (length(with_td)) out <- c(out, lapply(with_td, function(z){ z$td_name <- nm; z }))
        }
      }
      
      out
    })
  }
  
  # Collapse bundles to flat vectors (keep only successfully estimated models)
  fits <- list(); labels <- character(0)
  with_td_flag <- logical(0); with_easter_flag <- logical(0)
  td_name_vec <- character(0)
  
  for (i in seq_along(fit_bundles)) {
    for (j in seq_along(fit_bundles[[i]])) {
      b <- fit_bundles[[i]][[j]]
      if (!inherits(b$model, "seas")) next
      
      fits             <- c(fits, list(b$model))
      labels           <- c(labels, paste0("Spec_", length(fits)))
      with_td_flag     <- c(with_td_flag,     isTRUE(b$with_td))
      with_easter_flag <- c(with_easter_flag, isTRUE(b$with_easter))
      td_name_vec      <- c(td_name_vec,      b$td_name %||% NA_character_)
    }
  }
  if (!length(fits)) stop("All candidate specifications failed to estimate.")
  
  # --- Diagnostics table --------------------------------------------------------
  base_tbl <- purrr::imap_dfr(fits, function(m, i) {
    tibble::tibble(
      model_label = labels[[i]],
      arima       = .arima_string(m),
      with_td     = with_td_flag[[i]],
      td_name     = td_name_vec[[i]],
      with_easter = with_easter_flag[[i]],
      engine      = .engine_used(m),
      AICc        = tryCatch(.aicc(m), error = function(e) NA_real_),
      M7          = tryCatch(.m7_stat(m), error = function(e) NA_real_),
      IDS         = tryCatch(.ids_flag(m), error = function(e) NA_character_),
      LB_p        = tryCatch(.lb_p(m), error = function(e) NA_real_),
      SEATS_model_switch = .has_seats_model_switch_msg(m),
      SEATS_has_seasonal = .seats_has_seasonal(m)
    ) |>
      dplyr::bind_cols(.qs_on_sa_both(m)) |>
      dplyr::bind_cols(.qs_original(m))
  }) |>
    dplyr::bind_cols(purrr::map_dfr(fits, ~ .qs_x11_sma(.x)))
  
  # Finance-style KPIs, baseline distances
  fin_tbl   <- purrr::map_dfr(fits, ~ .diagnostics_finance(.x, y))
  base_dist <- purrr::map_dfr(fits, ~ .dist_vs_baseline(.x, current_sa, current_seasonal))
  
  # Regressor p-values & TD p
  reg_sig <- purrr::map(fits, .reg_pvals)
  td_p <- purrr::map_dbl(seq_along(fits), function(i) {
    df <- reg_sig[[i]]
    if (!NROW(df)) return(NA_real_)
    if (!with_td_flag[i]) return(NA_real_)
    td_rows <- df %>%
      dplyr::filter(grepl("xreg|wd|weekday|trading|td1coef|td", .data$var, ignore.case = TRUE) &
                      !.is_arima_term(.data$var))
    if (!NROW(td_rows)) NA_real_ else suppressWarnings(min(td_rows$p, na.rm = TRUE))
  })
  td_sig <- !is.na(td_p) & td_p < 0.05
  
  diag_tbl <- dplyr::bind_cols(base_tbl, fin_tbl, base_dist) %>%
    dplyr::mutate(td_p = td_p, td_sig = td_sig)
  
  # Ensure rev_mae exists (NA) for the first pass
  if (!"rev_mae" %in% names(diag_tbl)) diag_tbl$rev_mae <- NA_real_
  
  # First ranking
  ranked <- .ranker(diag_tbl, w_qs, w_stability, w_aicc, w_rev, w_dist_sa, w_dist_seas, w_engine,
                    engine_pref = if (engine == "auto") "seats" else engine)
  
  # Compute history revisions for top-N and re-rank
  k_hist <- min(include_history_top_n, nrow(ranked))
  top_idx <- match(ranked$model_label[seq_len(k_hist)], labels)
  
  rev_tbl <- if (k_hist > 0) {
    purrr::imap_dfr(fits[top_idx], function(m, i) {
      tibble::tibble(model_label = labels[top_idx[i]]) %>%
        dplyr::bind_cols(.revision_mae(m))
    })
  } else {
    tibble::tibble(model_label = character(0), rev_mae = numeric(0))
  }
  
  ranked <- ranked %>%
    dplyr::select(-dplyr::any_of("rev_mae")) %>%
    dplyr::left_join(rev_tbl, by = "model_label")
  if (!"rev_mae" %in% names(ranked)) ranked$rev_mae <- NA_real_
  
  # Final ranking (lower = better)
  ranked <- .ranker(ranked, w_qs, w_stability, w_aicc, w_rev, w_dist_sa, w_dist_seas, w_engine,
                    engine_pref = if (engine == "auto") "seats" else engine)
  
  ranked <- ranked %>%
    dplyr::mutate(
      score_raw  = .data$score,                         # lower = better
      score_100  = .rescale_best_high(.data$score_raw), # higher = better
      score_rank = rank(.data$score_raw, ties.method = "min"),
      rank       = score_rank
    )
  
  # Seasonality call (overall + best)
  overall <- seasonality_summary(ranked)
  best_flags <- .flag_tests(ranked[1, ]) %>%
    dplyr::mutate(call_best = if (seasonal_ids || seasonal_qsori || seasonal_m7s) "ADJUST" else "DO_NOT_ADJUST")
  seasonality <- list(overall = overall, best_model = best_flags)
  
  # Pick best
  best_lab <- ranked$model_label[1]
  best_fit <- fits[[match(best_lab, labels)]]
  
  # Weak seasonality flag
  weak_seasonality_flag <- with(ranked[1, ],
                                is.finite(seasonal_amp_pct) & seasonal_amp_pct < 1 &
                                  is.finite(vola_reduction_pct) & vola_reduction_pct < 5
  )
  
  # Specs tried (drop NAs)
  sp_tried <- unique(vapply(fits, .arima_string, character(1)))
  sp_tried <- sp_tried[is.finite(match(sp_tried, sp_tried)) & !is.na(sp_tried)]
  
  structure(
    list(
      best = best_fit,
      table = ranked,
      specs_tried = sp_tried,
      frequency = freq,
      transform = transform_fun,
      weak_seasonality = weak_seasonality_flag,
      baseline = list(current_sa = current_sa, current_seasonal = current_seasonal),
      seasonality = seasonality
    ),
    class = "auto_seasonal_analysis"
  )
}
