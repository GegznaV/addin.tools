
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Check if a blank line is needed above.
#'
#' Check if a blank line should be added above the selection.
#' @return Logical value.
#'
#' @inheritParams rs_get_ind
#'
#' @export
is_blank_line_needed_above <- function(context = rs_get_context()) {
    row <- rs_get_ind_first_selected_row(context)

    # Contents of row above the selection:
    txt <- context$contents[row - 1]
    # Remove spaces and check if string is empty:
    cond <- stringi::stri_isempty(gsub("[[:space:]]", "", txt))
    # If not empty (result: FALSE) and not the first row (result: logical(0)),
    # then an empty row needs to be added:
    isTRUE(!cond)
}
