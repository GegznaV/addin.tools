# addin.tools 0.0.9

* Initial CRAN submission.


# addin.tools 0.0.11

* Migrated runtime pipelines in package code from magrittr `%>%` to native R pipe `|>`.
* Removed stale `%>%` import annotations and cleaned dependency metadata to drop unused `magrittr` from `Imports`.
* Kept `%>%` references only in user-facing documentation contexts where the operator is discussed as inserted text.
* Validated migration safety with package-local tests and a clean maintainer `R CMD check` run.
* Fixed package documentation to use `"_PACKAGE"` instead of deprecated roxygen `@docType package`.
* Fixed `is_blank_line_needed_below()` argument matching for `"first row"` and `"last row"` call sites.
* Improved package-check readiness by excluding prompt/planning artifacts from package build inputs via `.Rbuildignore`.
* Updated `LICENSE` stub to valid DCF format for `MIT + file LICENSE` metadata.
* Added explicit global variable declarations used in tidy-eval pipelines to avoid R CMD check NSE notes.
* Expanded unit tests substantially for core helpers (selection/index/row/text/blank-line behavior).
* Added dedicated regression tests for `%>%` pipeline behavior, including `select_correct_range()` output and selection side effects, to support safe future migration from `%>%` to `|>`.
* Consolidated duplicate index helper definitions in `R/rs_get_index.R` into single implementations and removed superseded suffixed variants while preserving first/last/all selection behavior.
* Modernized package baselines in `DESCRIPTION` (`R (>= 4.1)`, `testthat (>= 3.0.0)`, `Config/testthat/edition: 3`) and moved `rstudioapi` to `Imports`.
* Confirmed GitHub Actions as the sole active CI path and removed Travis maintenance remnants.
* Refreshed README wording and install guidance, removed stale date badge, and updated logo source branch links in both `README.Rmd` and `README.md`.
* Added lightweight lint configuration via `.lintr` and set styler addin default transformer with `strict = FALSE` in `.Rprofile`.
* Renamed visual-editor helper to `is_visual_editor()` and documented `is_rmd_visual_mode` as a backward-compatible alias.
* Added a CRAN submission checklist and downstream deprecation migration guidance under `.github/`.
* Added GitHub Actions lint workflow to run `lintr` and a non-destructive `styler` check.
* Added a release prep helper script in `data-raw/release-prep.R`, a support matrix note under `.github/`, and a helper-family vignette for onboarding and discoverability.
* Added a maintainer-facing revdep smoke-check helper in `data-raw/revdep-smoke-check.R` and documented when to run it before release tagging.
* Expanded downstream deprecation guidance with an explicit timetable for `is_rmd_visual_mode()` and future compatibility wrappers.
* Expanded the helper-family vignette with concrete examples for common addin tasks and a build-time package setup chunk.
* Added a helper-family link to the README so discoverability paths surface the new vignette from the package homepage.


