# Release preparation helpers for addin.tools
#
# This file is not shipped in the package. It is intended to be sourced by
# maintainers during release preparation.

addin_tools_release_prep <- function(run_revdep = FALSE, run_pkgdown = TRUE) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("Package 'devtools' is required for release preparation.")
  }
  if (!requireNamespace("testthat", quietly = TRUE)) {
    stop("Package 'testthat' is required for release preparation.")
  }
  if (!requireNamespace("rcmdcheck", quietly = TRUE)) {
    stop("Package 'rcmdcheck' is required for release preparation.")
  }

  devtools::document()
  testthat::test_local(reporter = "summary")
  rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"))

  if (isTRUE(run_revdep)) {
    if (!requireNamespace("revdepcheck", quietly = TRUE)) {
      stop("Package 'revdepcheck' is required when run_revdep = TRUE.")
    }
    revdepcheck::revdep_check(num_workers = 1)
  }

  if (isTRUE(run_pkgdown)) {
    if (!requireNamespace("pkgdown", quietly = TRUE)) {
      stop("Package 'pkgdown' is required when run_pkgdown = TRUE.")
    }
    pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)
  }

  invisible(TRUE)
}
