# Revdep smoke-check helper for addin.tools
#
# This file is not shipped in the package. It is intended to be sourced by
# maintainers during release preparation.

addin_tools_revdep_smoke_check <- function(num_workers = 1) {
  if (!requireNamespace("revdepcheck", quietly = TRUE)) {
    stop("Package 'revdepcheck' is required for the revdep smoke check.")
  }

  revdepcheck::revdep_check(num_workers = num_workers)

  invisible(TRUE)
}