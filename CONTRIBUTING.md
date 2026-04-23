# Contributing to seasight

Thanks for your interest in improving `seasight`.

## Scope and workflow

- Open an issue first for substantial changes to discuss scope and impact.
- Keep pull requests focused, small, and easy to review.
- Preserve existing exported interfaces unless a change is explicitly agreed.
- Prefer deterministic examples and tests that run quickly on CI.

## Development setup

```r
devtools::load_all()
devtools::test()
```

No tokens, credentials or external playground services are required for the
standard local test suite. Tests and examples should use deterministic local
fixtures or temporary files created under `tempdir()`.

When roxygen comments or exports change:

```r
devtools::document()
```

Before opening a PR, run at least a targeted test pass and then a broader check:

```r
devtools::test()
devtools::check()
```

For rOpenSci pre-submission checks, `pkgcheck::pkgcheck()` may query GitHub
metadata. If needed, authenticate with the GitHub CLI or set a valid
`GITHUB_PAT` for that check only; package tests themselves should not depend on
that token.

## Style conventions

- Place package code in `R/` and tests in `tests/testthat/`.
- Use roxygen comments as the source of truth for documentation and namespace.
- Avoid unnecessary new dependencies.
- Keep report/UI changes separate from core diagnostics logic when practical.

## Review readiness

For peer-review readiness changes, consult:

- `README.md` for package positioning,
- `NEWS.md` for release notes,
- `inst/ropensci/bronze-standards-mapping.md` for statistical standards mapping.
