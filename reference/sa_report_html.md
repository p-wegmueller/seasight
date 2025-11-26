# Seasonal Adjustment: full HTML report

User-facing wrapper that runs the automatic seasonal analysis and writes
a human-readable HTML report with plots, tables, diagnostics, and
copy-pasteable [`seas()`](https://rdrr.io/pkg/seasonal/man/seas.html)
calls.

## Usage

``` r
sa_report_html(
  y,
  current_model = NULL,
  td_usertype = "td",
  td_candidates = NULL,
  use_fivebest = TRUE,
  title = "Seasonal Adjustment Report",
  outfile = "sa_report.html",
  png_width = 1400,
  png_height = 900,
  print_to_console = FALSE,
  print_which = c("new", "current", "both"),
  include_easter = c("auto", "always", "off"),
  easter_len = 15L,
  engine = c("seats", "x11", "auto"),
  w_engine = 1,
  outlier_types = c("AO", "LS", "TC"),
  outlier_method = "AddOne",
  outlier_critical = 4,
  outlier_alpha = NULL
)
```

## Arguments

- y:

  Time series to be analysed. Can be a `ts` object or anything that
  [`tsbox::ts_ts()`](https://docs.ropensci.org/tsbox/reference/ts_ts.html)
  can convert to `ts`.

- current_model:

  Optional incumbent
  [`seasonal::seas()`](https://rdrr.io/pkg/seasonal/man/seas.html) model
  to compare against the newly selected specification.

- td_usertype:

  Character string passed as `regression.usertype` when trading-day
  regressors are used (default `"td"`).

- td_candidates:

  Optional named list of trading-day candidate regressors (e.g.
  `list(wd = wd.m, wd1 = wd1.m)`), each aligned with `y`.

- use_fivebest:

  Logical. If `TRUE`, include the “five best” automatic specs in the
  candidate grid.

- title:

  Title of the report shown in the HTML page.

- outfile:

  Path to the HTML file to be written.

- png_width, png_height:

  Width and height (in pixels) of PNG plots embedded in the report.

- print_to_console:

  Logical. If `TRUE`, do not create an HTML file but print the
  copy-pasteable model code to the console and return it.

- print_which:

  Which code blocks to print when `print_to_console = TRUE`. One of
  `"new"`, `"current"` or `"both"`.

- include_easter:

  Controls inclusion of Easter regressors: `"auto"` (default) lets the
  selector decide, `"always"` always includes Easter, `"off"` never
  includes Easter. A logical value is also accepted and mapped to
  `"auto"`/`"off"`.

- easter_len:

  Integer, length (in days) of the Easter effect when included.

- engine:

  Preferred decomposition engine for candidate models. One of `"seats"`,
  `"x11"` or `"auto"`.

- w_engine:

  Numeric weight for the engine choice component in the composite
  ranking score.

- outlier_types:

  Character vector of outlier types to detect, typically a subset of
  `c("AO", "LS", "TC")`.

- outlier_method:

  Character scalar giving the outlier detection method passed to
  [`seasonal::seas()`](https://rdrr.io/pkg/seasonal/man/seas.html) (e.g.
  `"AddOne"`).

- outlier_critical:

  Numeric critical value for outlier detection (e.g. a t-/z-threshold).

- outlier_alpha:

  Optional numeric significance level; if supplied, it is converted to a
  two-sided normal cutoff for `outlier_critical`.

## Value

Invisibly, a list with elements `report` (path to the HTML file) and
`res` (the corresponding
[`auto_seasonal_analysis()`](https://p-wegmueller.github.io/seasight/reference/auto_seasonal_analysis.md)
result).

## Details

Notes:

- If the provided *current* specification equals the selected *best*
  model, the report omits the “Alternative model” comparison section.

- The “Top candidates” table starts with the best model and always
  includes the current model (flagged with ⭐) when one is supplied.

## See also

[`auto_seasonal_analysis()`](https://p-wegmueller.github.io/seasight/reference/auto_seasonal_analysis.md),
[`sa_existence_card()`](https://p-wegmueller.github.io/seasight/reference/sa_existence_card.md),
[`sa_top_candidates_table()`](https://p-wegmueller.github.io/seasight/reference/sa_top_candidates_table.md)

## Examples

``` r
# sa_report_html(y = AirPassengers, outfile = tempfile(fileext = ".html"))
```
