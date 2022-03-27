#' Check if Visual Markdown Editor (VME) is used
#'
#' Check if RStudio addins should treat the current selection of active document
#' as being in RStudio Visual Markdown Editor mode (RS VME) or not.
#'
#' The document [context][rstudioapi::getActiveDocumentContext()] from RS VME
#' is different than regular RStudio document context.
#'
#' @return Logical:
#'
#' - `TRUE` if an active document is in Visual Markdown Editor mode and
#'          current cursor position or selection is outside a code chunk.
#'
#' - `FALSE` if an active document is:
#'     a.  not in the Visual Markdown Editor mode;
#'     b.  in Visual Markdown Editor mode but current cursor position or
#'         selection is inside a code chunk.
#'
#' @export

is_rmd_visual_mode <- function() {
  length(rstudioapi::getActiveDocumentContext()$selection) == 0
}
