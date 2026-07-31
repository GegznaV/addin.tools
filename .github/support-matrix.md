# Support Matrix for addin.tools

## Current support policy

- Minimum R version: 4.1
- Test framework: testthat 3 edition
- Core runtime dependency: rstudioapi (>= 0.13)
- CI coverage: macOS, Windows, Ubuntu on GitHub Actions

## Compatibility policy

- `is_rmd_visual_mode()` remains as a deprecated wrapper for `is_visual_editor()`.
- Downstream packages should migrate to `is_visual_editor()` and remove the deprecated alias from new code.
- Package API changes should preserve behavior for `first`, `last`, and `all` selection modes unless a breaking change is explicitly announced in `NEWS.md`.
