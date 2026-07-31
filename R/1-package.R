# ============================================================================
#' @name addin.tools
#'
#' @title Helper functions to create RStudio add-ins.
#'
#' @description
#' Package **`addin.tools`** contains various functions that help to construct
#' "RStudio" add-ins.
#'
#' `License:` MIT   \cr
#' `URL:` <https://gegznav.github.io/addin.tools> \cr
#' `Bug reports and suggestions:`
#'      <https://github.com/GegznaV/addin.tools/issues> \cr
#' `Author:` Vilmantas Gegzna
#'
#' @import rstudioapi
#' @importFrom dplyr "%>%"
#' @importFrom utils globalVariables

"_PACKAGE"

utils::globalVariables(c(
  "difference",
  "end.column",
  "end.row",
  "end_diff",
  "modify_group",
  "new_end.column",
  "new_start.column",
  "rng",
  "special",
  "start.column",
  "start.row",
  "start_diff"
))
