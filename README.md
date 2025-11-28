<!-- README.md is generated from README.Rmd. Please edit that file -->

# seasight <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/p-wegmueller/seasight/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/p-wegmueller/seasight/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/p-wegmueller/seasight/actions/workflows/pkgdown.yaml/badge.svg)](https://p-wegmueller.github.io/seasight/)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
<!-- badges: end -->


**seasight** provides tools to *see* seasonal adjustment more clearly.  
It helps applied economists and official statisticians to run, compare
and document seasonal adjustment in a transparent and reproducible way.

The package is built on top of the
[`seasonal`](https://cran.r-project.org/package=seasonal) interface to
X-13ARIMA-SEATS and is designed for production workflows in official
statistics and central banks.

## Key features

- 🔍 **Seasonality diagnostics & decision rules**  
  Helpers such as `sa_existence_card()`, `sa_existence_call()` and
  `sa_tests_model()` summarise QS, M7, IDS and residual diagnostics,
  while `sa_is_do_not_adjust()` and `sa_should_switch()` turn them into
  clear “ADJUST / DO_NOT_ADJUST / KEEP / CHANGE” signals.

- ⚙️ **Automatic model grid & ranking**  
  `auto_seasonal_analysis()` runs a grid of X-13ARIMA-SEATS specifications
  (ARIMA, trading-day, Easter, engine choice) and ranks them using a
  composite score based on QS, Ljung–Box, AICc, revision metrics and
  distance to a baseline model.

- 📊 **Comparison & top-candidates tables**  
  `sa_compare()` and `sa_top_candidates_table()` produce compact tables
  and HTML widgets that highlight the best model, the current
  specification and the airline model, together with key diagnostics and
  revision statistics.

- 🧾 **Reproducible HTML reporting**  
  `sa_report_html()` builds a self-contained HTML report with an
  executive summary, existence-of-seasonality card, decision logic, top
  candidates, plots, outlier tables and copy-pasteable `seas()` calls.

- 🏛 **Made for official statistics**  
  Focus on transparency, stability over time and documentation of
  revisions, with use cases in national accounts and short-term
  indicators. The tools are designed to fit into institutional review
  and approval processes.

> **Status:** `seasight` is under active development. The user-facing
> API may still change before the first CRAN release.

## Installation

You can install the development version of **seasight** from
[GitHub](https://github.com/) with:

```r
remotes::install_github("p-wegmueller/seasight")
```

Once the package is more mature and has been peer-reviewed, it is
planned to be released on CRAN.

## Getting started

A typical workflow is:
1.	start from a raw time series,
2.	let seasight run an automatic seasonal analysis and ranking,
3.	inspect the chosen model and diagnostics,
4.	generate an HTML report for documentation and review.

```r
library(seasight)
library(seasonal)

# Example series from base R
y <- AirPassengers

# 1) Run the automatic seasonal analysis
res <- auto_seasonal_analysis(y)

# 'res' is an auto_seasonal_analysis object:
# - res$best        : the selected seasonal::seas() model
# - res$table       : diagnostics + ranking of candidate specs
# - res$seasonality : overall and best-model seasonality calls

res$best
seasonal::final(res$best)[1:6]   # first observations of the SA series
res$seasonality$overall          # robust "ADJUST" / "DO_NOT_ADJUST" call

# 2) Generate a full HTML report (decision, diagnostics, top candidates)
out <- sa_report_html(
  y       = y,
  outfile = "sa_airpassengers.html"
)

# 'out$report' is the path to the HTML file.
# Open it in a browser to inspect plots, tables and the decision logic:
out$report
```

For more advanced use, you can:

  - pass an existing `seasonal::seas()` model as current_model to compare
a legacy and a new specification side by side,
  - use `sa_top_candidates_table(res)` to embed a colour-coded “Top
candidates” table (best / current / airline) in your own reports,
  - call helpers like `sa_tests_model()` or `extract_outliers()` when you
need specific diagnostics in a custom workflow.

## Advanced usage

### 1. Comparing against an existing “current” model

In production, you often have a **legacy / incumbent** specification that
you only want to change if the new model is clearly better. `seasight`
can take a `current_model` and provide a simple **KEEP / CHANGE**
recommendation.

```r
library(seasight)
library(seasonal)

y <- AirPassengers

# Existing "current" model (example only)
current_model <- seas(
  y,
  x11 = "",
  arima.model = "c(0, 1, 1, 0, 1, 1)",
  transform.function = "log"
)

# Run automatic analysis *including* the current model as baseline
res <- auto_seasonal_analysis(
  y             = y,
  current_model = current_model
)

# Seasonality decision (robust ADJUST / DO_NOT_ADJUST)
res$seasonality$overall

# Should we switch from current_model to the new best model?
sa_should_switch(res)
#> "KEEP_CURRENT_MODEL" or "CHANGE_TO_NEW_MODEL"

# Full HTML report with side-by-side comparison
sa_report_html(
  y             = y,
  current_model = current_model,
  outfile       = "sa_airpassengers_with_baseline.html"
)
```

The HTML report will show:

- an existence-of-seasonality card,
- decision pill (KEEP_CURRENT_MODEL / CHANGE_TO_NEW_MODEL),
- side-by-side plots (levels and growth),
- top-candidates table highlighting the best, current, and airline model.

### 2. Trying alternative trading-day (TD) regressors
You can pass several TD candidate regressors and let
`auto_seasonal_analysis()` decide which one (if any) to use, based on
significance, diagnostics and the composite score.

```r
library(seasight)
library(seasonal)

y <- AirPassengers

# Example: two hypothetical TD candidates (replace with your own)
# td_default <- your_default_td_ts
# td_alt     <- your_alternative_td_ts
#
# They should be ts / xts / data.frame etc. that tsbox can align with y.

td_candidates <- list(
  td_default = td_default,   # e.g. standard trading-day
  td_alt     = td_alt        # e.g. TD with country-specific holidays
)

res_td <- auto_seasonal_analysis(
  y             = y,
  td_candidates = td_candidates,
  td_usertype   = "td",      # passed to regression.usertype
  current_model = NULL
)
```

Look at the top candidates and see which TD variant was chosen
`head(res_td$table[, c("model_label", "with_td", "td_name", "AICc", "LB_p")])`

In the HTML report created via `sa_report_html()`, the Top candidates
table will include a TD column and TD p-values, so you can check whether
the chosen TD effect is statistically and substantively meaningful.

### 3. Extracting diagnostics and outliers programmatically
You can also use seasight’s helpers directly on a single model, e.g.
for integrating diagnostics into your own dashboards or QA pipelines.

```r
library(seasight)
library(seasonal)

y <- AirPassengers
m <- seas(y)

# Structured diagnostics (QS, M7, IDS, LB p-value, etc.)
diag_tbl <- sa_tests_model(m)
diag_tbl

# Simple existence-of-seasonality call
sa_existence_call(diag_tbl)
#> "ADJUST" / "DO_NOT_ADJUST" / "UNCERTAIN"

# Outliers in a tidy table
extract_outliers(m)
```

These building blocks are what `auto_seasonal_analysis()` and
`sa_report_html()` use internally. In production settings you can call
them directly to implement custom review rules or dashboards on top of
existing seasonal adjustment workflows.


## Typical use cases

- Quarterly National Accounts: real, nominal and deflator series of
  production, expenditure and income accounts
- Short-term indicators: industrial production, retail trade, labour
  market
- Internal method reports on (re-)design of seasonal adjustment strategy
- Comparing legacy and new SA specifications during transition periods

## Articles

On the package website you can find two introductory articles:

- [Getting started with seasight](articles/seasight-getting-started.html)  
  Basic workflow from a `seasonal::seas()` model to diagnostics and an HTML report.

- [Advanced usage: baselines, TD candidates & decision rules](articles/seasight-advanced.html)
  How to work with a current production model, custom trading-day regressors and
  seasight’s built-in decision rules.

## Contributing

Feedback from practitioners is highly welcome, especially from:

- statistical offices
- central banks and finance ministries
- researchers working with real-time data and revisions

Please feel free to open an issue or share ideas for additional
diagnostics and reports that would be useful in production.

## License

This package is released under the MIT License (see `LICENSE`).