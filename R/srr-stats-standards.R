#' srr_stats
#'
#' Formal rOpenSci Statistical Software Review standards mapping for the initial
#' bronze-target submission. The package is categorised as general statistical
#' software plus time-series software; it orchestrates diagnostics and reporting
#' around `seasonal`/X-13ARIMA-SEATS rather than implementing a new decomposition
#' estimator.
#'
#' @srrstatsVerbose TRUE
#'
#' @srrstats {G1.0, G1.1} Methodological references for X-11,
#'   X-13ARIMA-SEATS, SEATS, and institutional seasonal-adjustment practice are
#'   listed in the vignettes; the README states that `seasight` wraps existing
#'   `seasonal`/X-13 functionality and contributes diagnostics/reporting
#'   orchestration rather than a novel estimator.
#' @srrstats {G1.2} The README includes a lifecycle badge and a pre-submission
#'   stabilization statement for the frozen 0.1.x API.
#' @srrstats {G1.3} Statistical terms used by exported functions are documented
#'   in roxygen and explained in the vignettes, including QS tests, M7, IDS,
#'   Ljung-Box diagnostics, SEATS/X-11 engine choice, trading-day regressors,
#'   and do-not-adjust decisions.
#' @srrstats {G1.4} Exported functions are documented with roxygen2 and
#'   documentation is regenerated from roxygen comments.
#' @srrstats {G1.4a} Internal helpers use local comments and targeted tests;
#'   internal roxygen blocks are used where helper documentation is useful
#'   without creating user-facing Rd files.
#'
#' @srrstats {G2.0, G2.0a, G2.1, G2.1a} Public arguments document expected
#'   lengths and types; code checks scalar choices, list inputs, and series
#'   structures before fitting.
#' @srrstats {G2.2} The package expects univariate target series and converts
#'   acceptable inputs to a single `ts` representation before modelling.
#' @srrstats {G2.3, G2.3a, G2.3b} Enumerated character arguments are constrained
#'   with `match.arg()` or equivalent explicit choices; case-sensitive X-13
#'   tokens are documented as character inputs passed through to `seasonal`.
#' @srrstats {G2.4, G2.4a, G2.4b, G2.4c} Inputs are explicitly coerced where
#'   needed, including integer candidate limits, numeric diagnostics, and
#'   character labels or identifiers.
#' @srrstats {G2.6, G2.7, G2.8} Time-series preprocessing is centralised through
#'   `.as_ts()`/`tsbox::ts_ts()` so supported tabular and time-series classes are
#'   normalised to base `ts` before downstream fitting.
#' @srrstats {G2.9} Potentially lossy unsupported nested tabular structures are
#'   rejected with explicit errors rather than passed silently to X-13.
#' @srrstats {G2.10} Tests exercise data-frame/tibble outputs and avoid
#'   assumptions about partial column extraction behaviour.
#' @srrstats {G2.11, G2.12} Data-frame-like inputs with non-standard/list
#'   columns are explicitly rejected with informative errors and regression
#'   tests cover this behaviour.
#' @srrstats {G2.13, G2.15, G2.16} Numeric summaries and diagnostics use
#'   explicit finite/missing checks and guarded calculations before values are
#'   passed into scoring or report logic.
#'
#' @srrstats {G3.0} Tests and scoring use tolerance-aware expectations or
#'   integer/label comparisons; floating-point diagnostics are not used as exact
#'   equality conditions.
#'
#' @srrstats {G4.0} Report-writing functions document file arguments, create
#'   output directories, and tests cover generated report/log paths.
#'
#' @srrstats {G5.0, G5.1} Tests use base R reference time-series data where
#'   useful and shared test fixtures in `tests/testthat/helper-fixtures.R` for
#'   deterministic constructed series.
#' @srrstats {G5.2, G5.2a, G5.2b} Error and warning paths for input validation,
#'   list-column rejection, batch timeouts, and report edge cases are covered by
#'   targeted tests with expected messages.
#' @srrstats {G5.3} Tests check important return structures, generated files,
#'   and diagnostic table fields for expected non-missing behaviour where the
#'   function contract requires it.
#' @srrstats {G5.4, G5.4a, G5.4b, G5.4c} Correctness is assessed against
#'   `seasonal`/X-13 outputs and known base time-series examples; `seasight`
#'   does not reimplement the decomposition estimator.
#' @srrstats {G5.5} Synthetic fixtures are deterministic; tests avoid stochastic
#'   simulation where a fixed constructed series is sufficient.
#' @srrstats {G5.8, G5.8a, G5.8b, G5.8c, G5.8d} Edge-condition tests cover empty
#'   inputs, unsupported structures, incompatible frequencies, absent
#'   components, and all-constant or no-seasonality cases.
#'
#' @srrstats {TS1.0, TS1.1, TS1.2, TS1.3, TS1.4} Inputs are documented as `ts`
#'   or ts-boxable objects and normalised to base `ts` through `.as_ts()` before
#'   X-13 fitting, preserving start, end, and frequency attributes.
#' @srrstats {TS1.5, TS1.6} Alignment helpers check frequency and window
#'   compatibility so regressors are ordered and aligned to the target series.
#' @srrstats {TS1.7, TS4.1} Tabular time-series inputs with `units` columns are
#'   accepted by preprocessing and aligned regressor outputs retain a units
#'   attribute for downstream inspection.
#' @srrstats {TS1.8} Calendar and moving-holiday windows are documented in days,
#'   while seasonal-adjustment series are represented with explicit `ts`
#'   frequency attributes.
#'
#' @srrstats {TS2.0} Regularity is represented explicitly by `ts` frequency and
#'   tsbox conversion; incompatible regressor frequencies or spans are rejected
#'   or skipped with diagnostics.
#' @srrstats {TS2.2, TS2.3, TS2.4, TS2.4a, TS2.4b} Stationarity and transform
#'   assumptions are handled by X-13ARIMA-SEATS through `seasonal`; `seasight`
#'   documents and exposes transform/outlier/ARIMA choices and reports residual
#'   diagnostics for review.
#'
#' @srrstats {TS4.0, TS4.0b, TS4.2, TS4.3} Primary return values use documented
#'   list/tibble/html structures such as `auto_seasonal_analysis`, diagnostic
#'   tables, and report paths; temporal scale is retained in embedded `ts`
#'   series and X-13 model objects.
#' @srrstats {TS4.4, TS4.5, TS4.5b, TS4.5c} Transform choices and practical
#'   limitations of X-13/SEATS workflows are described in function
#'   documentation and vignettes; copyable model calls allow users to reproduce
#'   or adapt transformations in `seasonal`.
#'
#' @srrstats {TS5.1, TS5.2, TS5.3} Report visualisations are temporal plots with
#'   time on the horizontal axis via base/`seasonal` time-series plotting.
#' @srrstats {TS5.0} The `auto_seasonal_analysis` result class has a default
#'   `plot()` method for quick inspection.
#' @noRd
NULL

#' NA_standards
#'
#' Standards below are documented as non-applicable to this package's initial
#' scope: `seasight` is a diagnostics/reporting orchestration layer over
#' `seasonal`/X-13ARIMA-SEATS, not a new estimator, covariance engine,
#' forecasting package, or frequency-domain plotting package.
#'
#' @srrstatsNA {G1.5, G1.6} No independent performance claims are made in an
#'   associated publication; model comparisons are within `seasonal`/X-13.
#' @srrstatsNA {G2.4d, G2.4e, G2.5} No factor-valued statistical inputs are
#'   exposed.
#' @srrstatsNA {G2.14, G2.14a, G2.14b, G2.14c} Missing-value handling is
#'   delegated to `seasonal`/X-13 after preprocessing; `seasight` does not offer
#'   imputation modes because that would materially change the production
#'   series.
#' @srrstatsNA {G3.1, G3.1a} `seasight` does not compute covariance matrices or
#'   expose covariance-estimation algorithms.
#' @srrstatsNA {G5.6, G5.6a, G5.6b, G5.7, G5.9, G5.9a, G5.9b} The package does
#'   not estimate a novel statistical model with recoverable parameters or make
#'   asymptotic/performance claims requiring parameter-recovery, scaling, or
#'   stochastic-noise susceptibility tests.
#' @srrstatsNA {G5.10, G5.11, G5.11a, G5.12} There is no separate extended test
#'   suite or external data download requirement for the initial submission.
#'
#' @srrstatsNA {TS2.1, TS2.1a, TS2.1b, TS2.1c} The package does not impute,
#'   ignore, or repair missing observations because seasonal adjustment should
#'   be performed on the analyst-approved production series.
#' @srrstatsNA {TS2.5, TS2.6} The package does not compute or return
#'   auto-covariance matrices.
#' @srrstatsNA {TS3.0, TS3.1, TS3.2, TS3.3, TS3.3a, TS3.3b} `seasight` is not a
#'   forecasting package and does not expose forecast-error estimates; any
#'   internal X-13 forecast horizon is used only to support decomposition.
#' @srrstatsNA {TS4.0a} The main outputs are diagnostic/model objects rather
#'   than transformed series intended to mirror the input class.
#' @srrstatsNA {TS4.5a} Back-transformation routines are not exposed because
#'   final adjusted series are obtained from `seasonal` model objects.
#' @srrstatsNA {TS4.6, TS4.6a, TS4.6b, TS4.6c, TS4.7, TS4.7a, TS4.7b, TS4.7c}
#'   The package does not return user-facing forecast distributions or forecast
#'   values.
#' @srrstatsNA {TS5.4} The package does not provide frequency-domain plots.
#' @srrstatsNA {TS5.5} Missing-value line rendering is not exposed as a plotting
#'   option because report plots are generated from fitted `seasonal` objects.
#' @srrstatsNA {TS5.6, TS5.7, TS5.8} Forecast visualisation standards do not
#'   apply because forecasts are not returned or plotted as package outputs.
#' @noRd
NULL
