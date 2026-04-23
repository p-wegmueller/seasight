# rOpenSci statistical-software standards mapping (bronze target)

Last updated: 2026-04-23
Badge target: **Bronze** (first milestone)
Submission template `statsgrade`: `bronze`

This document maps current `seasight` capabilities to statistical software
review expectations and tracks remaining work for submission.

## Why bronze first

`seasight` is currently focused on a concrete workflow niche: transparent,
reviewable seasonal-adjustment diagnostics/reporting around X-13ARIMA-SEATS.
A bronze target is appropriate for initial peer review while preserving a clear
path toward broader standards coverage in later releases.

## Current evidence in package

- Multi-platform CI with R-CMD-check matrix.
- Unit tests for robustness and ranking behavior.
- User documentation via README + vignettes + pkgdown workflow.
- Grouped pkgdown reference index by workflow, diagnostics, reporting and
  utilities.
- Clear license and issue tracker links.
- Explicit contributor guidance in `CONTRIBUTING.md`, linked from the README.

## Planned standards progression

### Bronze scope (this cycle)

- [x] Define badge target and scope narrative.
- [x] Provide contributor/governance docs.
- [x] Provide citation metadata and changelog.
- [x] Clarify overlap/complementarity with alternatives.
- [x] Run full `pkgcheck` and resolve reported red flags in an R-enabled environment.
- [x] Add examples for exported functions flagged by `pkgcheck`.
- [x] Switch default branch to `main`.

## Editorial pre-check notes

- The pkgdown reference index is grouped in `_pkgdown.yml`.
- The README links to `CONTRIBUTING.md` and notes that normal local tests do
  not require tokens or external services.
- `srr::srr_report()` currently reports that this is not an `srr` package,
  because formal `@srrstats` roxygen tags have not yet been added. This
  document is therefore a submission-facing mapping rather than an automated
  `srr` compliance report.
- The package does not currently use `sandwich` or expose covariance
  calculations, so standards around configurable covariance estimators are not
  applicable to the current API.
- Time-series conversion now explicitly rejects data-frame list columns before
  calling `tsbox`, with a clear error message.
- Formal `@srrstats` / `@srrstatsNA` tags for the general and time-series
  standards are now tracked in `R/srr-stats-standards.R`.
- Repeated synthetic test series are centralised in
  `tests/testthat/helper-fixtures.R`, reducing hard-coded stochastic test data
  and improving the G5.0/G5.1 evidence trail.

### Toward silver/gold (future)

- [ ] Add richer comparative validation materials and benchmarking artifacts.
- [ ] Expand examples across additional official-statistics production contexts.
- [ ] Revisit `TS1.7`/`TS4.1` if future APIs intentionally support
  unit-bearing time-series inputs and unit-preserving outputs.
