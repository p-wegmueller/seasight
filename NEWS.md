# seasight 0.1.0

## Submission-readiness update

- Correct seasonal-amplitude scaling: log models now report the
  peak-to-trough seasonal-factor range in percentage points, level models use
  the additive range relative to the median absolute level, and unavailable
  components produce `NA` rather than zero.
- Render unavailable QS diagnostics explicitly in the existence and
  engine-choice cards instead of producing incomplete labels or errors.
- Surface SEATS model substitutions in the report summary and engine-choice
  card. The candidates table now records a stable switch indicator alongside
  the requested and operative model specifications.
- Add `comparison_mode = "incumbent_fixed"` for ARIMA comparisons that retain
  the incumbent transformation, fixed outliers, calendar terms and stored user
  regressors. Candidate metadata and reports identify the comparison design and
  regressor differences.
- Fix `include_easter = "auto"` so X-13 tests and selects the Easter effect
  instead of always imposing `easter[15]`. Candidate metadata and HTML reports
  now show the Easter window actually retained by the fitted model.
- Make `sa_should_switch()` compare the selected candidate directly with the
  incumbent. Identical adjustments are kept, a change requires a material
  comparable AICc improvement without diagnostic regression, and missing or
  non-comparable evidence is reported explicitly.
- Freeze core user-facing API for the `0.1.x` pre-submission stabilization cycle.
- Add contributor and governance documentation (`CONTRIBUTING.md`, `CODE_OF_CONDUCT.md`).
- Add package citation metadata via `inst/CITATION`.
- Add initial rOpenSci statistical-review standards mapping with a bronze-first target.
- Clarify positioning versus alternatives in README and getting-started vignette.

# seasight 0.0.1

- Initial development release.
