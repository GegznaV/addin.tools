#' Retrieve information about an active RStudio document.
#'
#' Wrapper for \code{\link[rstudioapi]{getActiveDocumentContext}()}.
#' @export
rs_get_context <- function() {
    rstudioapi::getActiveDocumentContext()
}
