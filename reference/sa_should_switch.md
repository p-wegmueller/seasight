# Should we switch to the new best model?

Should we switch to the new best model?

## Usage

``` r
sa_should_switch(
  res,
  thresholds = list(min_qs_p = 0.1, max_dist_sa_mult = 1.25, min_corr_seas = 0.9,
    min_lb_p = 0.05)
)
```

## Arguments

- res:

  Result of
  [`auto_seasonal_analysis()`](https://p-wegmueller.github.io/seasight/reference/auto_seasonal_analysis.md).

- thresholds:

  Named list with decision thresholds:

  - min_qs_p: minimum acceptable QS p-value on SA (overall) for the best
    model

  - max_dist_sa_mult: allow SA L1 distance up to this multiple of the
    cross-candidate median

  - min_corr_seas: minimum correlation of seasonal components (vs.
    incumbent)

  - min_lb_p: minimum acceptable Ljung–Box p-value on residuals

## Value

One of "CHANGE_TO_NEW_MODEL" or "KEEP_CURRENT_MODEL".
