# CRAN Submission Checklist for addin.tools

## Before submission

- [ ] Run package-local tests: `Rscript -e "testthat::test_local(reporter='summary')"`
- [ ] Run `R CMD check` on the package tarball
- [ ] Verify `NEWS.md` has a fresh release entry
- [ ] Verify `DESCRIPTION` metadata, version, and dependency floors
- [ ] Regenerate docs if `R/` or roxygen comments changed
- [ ] Confirm `README.Rmd` and `README.md` are synchronized
- [ ] Confirm there are no portable file-name warnings or unintended top-level files
- [ ] Confirm no warnings from deprecated helper aliases are used in package code

## Checks to run

```r
devtools::document()
testthat::test_local(reporter = "summary")
rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"))
source("data-raw/revdep-smoke-check.R")
addin_tools_revdep_smoke_check()
```

## Release packaging

- [ ] Create source tarball from the current tagged state
- [ ] Inspect `00check.log`
- [ ] Run the revdep smoke check before tagging if downstream packages are in scope
- [ ] Confirm examples and vignettes build cleanly
- [ ] Confirm GitHub Actions status is green

## Post-submission

- [ ] Update `NEWS.md` with any CRAN feedback fixes
- [ ] Tag the release in Git
- [ ] Sync pkgdown site if documentation changed
