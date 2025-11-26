# --- Ranking -------------------------------------------------------------------
# Lower score = better. Adds a 'rank' column (min-rank) for downstream use.
.rank_candidates <- function(tbl,
                             w_qs, w_stability, w_aicc, w_rev, w_dist_sa, w_dist_seas,
                             w_engine = 0,
                             engine_pref = c("seats","x11","auto")) {
  engine_pref <- match.arg(engine_pref)
  
  # 0..1 scaler; keeps NA where NA in input
  scale01 <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    fin <- is.finite(x)
    if (!any(fin)) return(rep(NA_real_, length(x)))
    rng <- range(x[fin], na.rm = TRUE)
    out <- rep(NA_real_, length(x))
    if (diff(rng) == 0) {
      out[fin] <- 0
    } else {
      out[fin] <- (x[fin] - rng[1]) / (rng[2] - rng[1])
    }
    out
  }
  # NA-safe scaler: NA -> worst (=1)
  scale01_worstNA <- function(x) {
    s <- scale01(x)
    ifelse(is.na(s), 1, s)
  }
  
  # --- Components (all as penalties: larger = worse) ---------------------------
  # Residual seasonality on SA (QS overall): penalize only when QS < 0.10.
  # QS >= 0.10 -> 0 penalty; QS = 0 -> 1 penalty; NA -> 0.5 mid-penalty.
  s_qs <- {
    p <- suppressWarnings(as.numeric(tbl$QS_p))
    pen <- ifelse(is.finite(p), (0.10 - pmin(p, 0.10)) / 0.10, 0.5)
    pmax(pen, 0)
  }
  
  # Information criterion (lower is better) — scale 0..1, NA -> worst
  s_aicc <- scale01_worstNA(tbl$AICc)
  
  # Revision size (lower is better) — NA -> worst
  s_rev <- scale01_worstNA(tbl$rev_mae)
  
  # Distance to incumbent SA / seasonal (lower is better) — NA -> worst
  s_dist_sa   <- scale01_worstNA(tbl$dist_sa_L1)
  s_dist_seas <- scale01_worstNA(tbl$dist_seas_RMS)
  
  # Stability: % reduction in pct-change volatility; want large positive numbers.
  # Penalty = 1 - clamp(vola_reduction_pct/100, 0..1). NA -> 0.5 mid-penalty.
  s_stability <- {
    vr <- suppressWarnings(as.numeric(tbl$vola_reduction_pct))
    red <- ifelse(is.finite(vr), pmax(0, pmin(vr/100, 1)), NA_real_)
    pen <- 1 - red
    ifelse(is.na(pen), 0.5, pen)
  }
  
  # Engine preference penalty:
  # - If engine_pref == "auto": light bias toward SEATS (SEATS=0, X-11=0.25, else 0.5)
  # - Else: exact match = 0; mismatch = 1; unknown/NA = 0.5
  e_pen <- {
    eng <- tolower(as.character(tbl$engine))
    if (engine_pref == "auto") {
      ifelse(is.na(eng), 0.5,
             ifelse(eng == "seats", 0,
                    ifelse(eng == "x11", 0.25, 0.5)))
    } else {
      ifelse(is.na(eng), 0.5, ifelse(eng == engine_pref, 0, 1))
    }
  }
  
  # --- Weighted score (lower = better) -----------------------------------------
  score <- w_qs        * s_qs +
    w_aicc      * s_aicc +
    w_rev       * s_rev +
    w_dist_sa   * s_dist_sa +
    w_dist_seas * s_dist_seas +
    w_stability * s_stability +
    w_engine    * e_pen
  
  tbl$score <- as.numeric(score)
  tbl <- dplyr::arrange(tbl, .data$score)
  tbl$rank <- rank(tbl$score, ties.method = "min")
  tbl
}
