# Comparison wrapper (current vs best)

Thin wrapper around `compare_to_current()` returning its result. If
`compare_to_current()` is not found, an informative error is raised.

## Usage

``` r
sa_compare(res, current_model)
```

## Arguments

- res:

  Result from
  [`auto_seasonal_analysis()`](https://p-wegmueller.github.io/seasight/reference/auto_seasonal_analysis.md).

- current_model:

  A fitted `seas` object to compare against.

## Value

List with `decision`, `summary`, and aligned SA/seasonal series.
