# Key seasonality diagnostics for a fitted model

Returns M7, IDS, QS on SA (X-11 & SEATS, plus overall min), QS on
original (X-11 & SEATS, plus overall min), and Ljung–Box p-value on
residuals.

## Usage

``` r
sa_tests_model(m)
```

## Arguments

- m:

  A fitted [seasonal::seas](https://rdrr.io/pkg/seasonal/man/seas.html)
  object.

## Value

A one-row tibble with columns: `M7`, `IDS`, `QS_p_x11`, `QS_p_seats`,
`QS_p`, `QSori_p_x11`, `QSori_p_seats`, `QSori_p`, `LB_p`.
