# Extract X-13 outlier coefficients (AO/LS/TC) from a `seas` model

Extract X-13 outlier coefficients (AO/LS/TC) from a `seas` model

## Usage

``` r
extract_outliers(m)
```

## Arguments

- m:

  A fitted [seasonal::seas](https://rdrr.io/pkg/seasonal/man/seas.html)
  model.

## Value

A tibble with columns `type`, `period`, `coef`. Empty when none found.
