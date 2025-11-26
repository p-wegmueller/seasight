# Aggregate existence-of-seasonality decision (ADJUST / BORDERLINE / DO_NOT_ADJUST)

Uses IDS, M7 (strong/weak), QS on SA, and QS on ORIGINAL (QSori) to form
a simple majority-style decision. Thresholds mirror those used
elsewhere.

## Usage

``` r
sa_existence_call(
  tbl,
  majority = 0.6,
  thr = list(qs = 0.1, m7_strong = 0.9, m7_weak = 1.05, borderline_min_share = 0.4,
    use_qsori = TRUE)
)
```

## Arguments

- tbl:

  Candidate table (e.g., `res$table`).

- majority:

  Required share for a positive overall call (default 0.6).

- thr:

  List of thresholds: `qs` (0.10), `m7_strong` (0.90), `m7_weak` (1.05),
  `borderline_min_share` (0.4), `use_qsori` (TRUE).

## Value

A tibble with `call_overall` and shares per test family.
