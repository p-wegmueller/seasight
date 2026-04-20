You are working in the `seasight` repository, an R package/codebase for seasonal adjustment analysis.

General working style
- Inspect before editing.
- For any non-trivial task, first state a short plan.
- Then execute the plan efficiently without asking for approval at every small step unless blocked or high-risk.
- Keep edits minimal, targeted, and easy to review.
- Do not modify unrelated files.
- Preserve existing style and package conventions unless the task explicitly requires modernization.
- State assumptions clearly.

Issue workflow
- Work through issues one by one.
- For each issue, first check whether it is already resolved in the current package state before making any edits.
- To do so, inspect the relevant code, documentation, tests, and recent package structure or behavior.
- If the issue appears already resolved:
  - do not implement a duplicate fix
  - explain clearly why it is already resolved
  - identify the evidence
  - add a missing regression test or documentation note only if genuinely useful
- If the issue is not yet resolved:
  - identify the smallest coherent fix
  - implement only what is needed for that issue
  - update tests and documentation where relevant
- After finishing one issue, summarize the outcome and continue to the next one.

R package rules
- Keep package code in `R/`.
- Keep tests in `tests/testthat/`.
- Use roxygen comments as the source of truth for documentation and namespace changes.
- Avoid unnecessary new dependencies.
- Preserve exported interfaces unless the issue explicitly requires a change.
- Keep examples and tests fast, deterministic, and portable.
- Avoid hidden dependence on the interactive workspace, machine-specific paths, or local state.
- When removing duplicate code or functions, always verify that implementations are identical. If differences exist, explain them and do not remove code without explicit resolution of those differences.

Validation
- Start with the smallest relevant validation step.
- Prefer targeted checks while iterating.
- Use broader package checks when needed.
- Report clearly what was run and what passed, failed, or remains uncertain.
- When validating in shell, prefer `Rscript` if available; otherwise fall back to `R -q -e`. Never claim validation was run unless it actually executed.

Typical validation loop
- `devtools::load_all()`
- `devtools::document()` when roxygen or exports change
- `devtools::test(filter = ...)` for targeted iteration
- broader tests or `devtools::check()` when appropriate