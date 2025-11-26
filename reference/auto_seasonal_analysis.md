# Automatic seasonal analysis (candidate grid + ranking)

Runs a grid of X-13ARIMA-SEATS specifications, computes diagnostics (QS
tests, residual checks, revision metrics, distance to a baseline model,
etc.), and ranks candidates using a composite score. Returns the best
model together with a diagnostic table and seasonality call.

## Usage

``` r
auto_seasonal_analysis(
  y,
  specs = NULL,
  use_fivebest = TRUE,
  auto_outliers = TRUE,
  transform_fun = c("auto", "log", "none"),
  td_candidates = NULL,
  td_usertype = "td",
  include_easter = c("auto", "always", "off"),
  easter_len = 15L,
  include_history_top_n = 10,
  early_period_end = NULL,
  current_model = NULL,
  current_sa = NULL,
  current_seasonal = NULL,
  w_qs = 1,
  w_stability = 1,
  w_aicc = 1,
  w_rev = 0.5,
  w_dist_sa = 1,
  w_dist_seas = 0.5,
  w_engine = 1,
  engine = c("seats", "x11", "auto"),
  outlier_types = c("AO", "LS", "TC"),
  outlier_method = "AddOne",
  outlier_critical = 4,
  outlier_alpha = NULL
)
```

## Arguments

- y:

  A time series (`ts`) or an object that can be converted to `ts` via
  [`tsbox::ts_ts()`](https://docs.ropensci.org/tsbox/reference/ts_ts.html).

- specs:

  Optional character vector of ARIMA specifications in the form
  `"(p d q)(P D Q)"`. If `NULL`, default specs are chosen based on the
  series frequency.

- use_fivebest:

  Logical. If `TRUE`, a small set of data-driven "five best" specs (à la
  `seasonal::seas(x, x11 = "")`) is added to the candidate grid.

- auto_outliers:

  Logical. If `TRUE`, enables automatic outlier detection in the
  candidate models.

- transform_fun:

  Transformation applied to the input series before seasonal adjustment.
  `"auto"` selects a safe log transform if the series is strictly
  positive, `"log"` forces a log transform, and `"none"` keeps the
  series in levels.

- td_candidates:

  Optional named list of alternative trading-day regressors, e.g.
  `list(wd = wd.m, wd1 = wd1.m)`. Each element should be a time series
  aligned with `y`.

- td_usertype:

  Character string passed as `regression.usertype` when `xreg` is used
  (default `"td"`).

- include_easter:

  Controls inclusion of Easter regressors: `"auto"` (default) lets the
  procedure decide, `"always"` always includes Easter, `"off"` never
  includes Easter. A logical value is also accepted and mapped to
  `"auto"`/`"off"`.

- easter_len:

  Integer, length (in days) of the Easter effect when included.

- include_history_top_n:

  Integer, number of top-ranked models for which revision metrics over
  the history are computed before the final ranking step.

- early_period_end:

  Reserved for future use. Optional index or date marking the end of an
  early sample period used for stability checks.

- current_model:

  Optional incumbent
  [`seasonal::seas`](https://rdrr.io/pkg/seasonal/man/seas.html) object
  used as a baseline for distance measures and comparison.

- current_sa:

  Optional baseline seasonally adjusted series. If not supplied, it is
  extracted from `current_model` when possible.

- current_seasonal:

  Optional baseline seasonal component. If not supplied, it is extracted
  from `current_model` (preferring SEATS, falling back to X-11 if
  needed).

- w_qs:

  Weight of the QS (seasonality) diagnostics in the composite ranking
  score.

- w_stability:

  Weight of residual / stability diagnostics (e.g. Ljung-Box) in the
  composite score.

- w_aicc:

  Weight of the information criterion (AICc) in the composite score.

- w_rev:

  Weight of the revision metric (historical revision MAE) in the
  composite score.

- w_dist_sa:

  Weight of the distance between seasonally adjusted series (candidate
  vs baseline) in the composite score.

- w_dist_seas:

  Weight of the distance between seasonal components (candidate vs
  baseline) in the composite score.

- w_engine:

  Weight / penalty term related to the decomposition engine (SEATS vs
  X-11) in the composite score.

- engine:

  Engine preference for candidate models. One of `"seats"`, `"x11"` or
  `"auto"` (tries both and lets the ranking decide, with a small penalty
  for deviations from the preferred engine).

- outlier_types:

  Character vector of outlier types to detect, typically a subset of
  `c("AO", "LS", "TC")`.

- outlier_method:

  Character scalar giving the outlier detection method (passed to
  [`seasonal::seas()`](https://rdrr.io/pkg/seasonal/man/seas.html), e.g.
  `"AddOne"`).

- outlier_critical:

  Numeric critical value for outlier detection (e.g. t- or z-threshold).

- outlier_alpha:

  Optional numeric significance level for outlier detection. If
  supplied, it is converted to a two-sided normal cutoff for
  `outlier_critical`.

## Value

An object of class `"auto_seasonal_analysis"` with components such as
`best` (best `seas` model), `table` (diagnostic and ranking table),
`specs_tried`, `frequency`, `transform`, `baseline`, and `seasonality`.
