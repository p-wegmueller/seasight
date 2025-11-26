# Top candidates table (HTML tag)

Build a colour-coded HTML table of the highest-ranked candidate models,
always placing the **best** model at the top and **including the current
model** (if supplied) even if it is not in the top `n`. The airline
reference model ARIMA (0 1 1)(0 1 1) is also appended if absent.

## Usage

``` r
sa_top_candidates_table(res, current_model = NULL, y = NULL, n = 5)
```

## Arguments

- res:

  Result of
  [`auto_seasonal_analysis()`](https://p-wegmueller.github.io/seasight/reference/auto_seasonal_analysis.md).

- current_model:

  Optional fitted
  [seasonal::seas](https://rdrr.io/pkg/seasonal/man/seas.html) model to
  mark as "current".

- y:

  Optional original series (needed to compute a few current-model
  diagnostics).

- n:

  Number of top rows to display (default 5). The airline/current rows
  may be appended even if not in the top `n`.

## Value

An `htmltools` tag (`<table>`) you can insert into reports.

## Details

Rows are lightly shaded: ✅ best (green), ⭐ current (blue), airline
(red).
