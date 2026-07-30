#' Check if Visual Editor (VE) is used
#'
#' Check if RStudio addins should treat the current selection of active document
#' as being in RStudio Visual Editor mode (RS VE) or not.
#'
#' The document [context][rstudioapi::getActiveDocumentContext()] from RS VE
#' is different than regular RStudio document context.
#'
#' @return Logical:
#'
#' - `TRUE` if an active document is in Visual Editor mode and
#'          current cursor position or selection is outside a code chunk.
#'
#' - `FALSE` if an active document is:
#'     a.  not in the Visual Editor mode;
#'     b.  in Visual Editor mode but current cursor position or
#'         selection is inside a code chunk.
#'
#' @export

is_visual_editor <- function() {
  length(rstudioapi::getActiveDocumentContext()$selection) == 0
}

#' @export
is_rmd_visual_mode <- is_visual_editor
