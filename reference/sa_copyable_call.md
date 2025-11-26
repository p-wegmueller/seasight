# Build a copy-pasteable seas() call from a fitted model

- SEATS (default engine) → omit `x11`/`seats` args

- X-11 → include `x11 = ""`

## Usage

``` r
sa_copyable_call(
  m,
  x_expr,
  xreg_expr = NA,
  include_force = FALSE,
  engine = c("auto", "seats", "x11")
)
```

## Arguments

- m:

  Fitted `seas` model.

- x_expr:

  Character code for `x=` (e.g. `"y"`).

- xreg_expr:

  `NULL` to drop, `NA` to keep as-is, or character code to set.

- include_force:

  Logical; if TRUE adds `force.type = "denton"`.

- engine:

  `"auto"` (use model's engine) or force `"seats"`/`"x11"`.

## Value

A single string containing the `seas(...)` call.
