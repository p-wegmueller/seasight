# rOpenSci pre-submission checklist

Last updated: 2026-04-23

## Technical checks

- [x] Packaging guide, author guide and Statistical Software Peer Review Guide
  reviewed for package-preparation purposes.
- [x] `statsgrade` value for submission: `bronze`.
- [x] Package license is MIT, a CRAN/OSI accepted license.
- [x] README includes development-version installation instructions.
- [x] README states package need, scope and target audience.
- [x] README links to contribution guidelines and the Code of Conduct.
- [x] Normal tests/examples do not require credentials, external services or
  playground resources.
- [x] The package does not interact with external services in normal use, so no
  third-party Terms of Service are implicated.
- [x] Intended publication route: CRAN.
- [x] Intended publication route: not Bioconductor.

## Checks to re-run immediately before submission

- [ ] `autotest::autotest_package(test = TRUE)`
- [ ] `srr::srr_stats_pre_submit()`
- [ ] `pkgcheck::pkgcheck(goodpractice = FALSE)`
- [ ] `devtools::test()`
- [ ] `devtools::check()`

## Maintainer attestation

The maintainer should personally confirm in the submission form that they expect
to maintain the package for at least the required post-acceptance period and
agree to follow rOpenSci's Code of Conduct during review and maintenance.
