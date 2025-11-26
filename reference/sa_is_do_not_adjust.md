# Row-level non-adjustment rule (wrapper)

Delegates to the internal `.do_not_adjust()` to keep logic in one place.

## Usage

``` r
sa_is_do_not_adjust(row)
```

## Arguments

- row:

  One-row tibble from `res$table`.

## Value

TRUE if the row satisfies the "do not adjust" rule, FALSE otherwise.
