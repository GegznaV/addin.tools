# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' ' Check if a blank line is needed below.
#'
#' Checks if a blank line should be added below either all selected rows or
#' the first selected row.
#' @param where (string)
#'      If \code{"first"} - checks for a blank line below the first selected row.
#'      If \code{"last"} - checks for a blank line below the last selected row.
#' @rdname heplers
#'
#' @inheritParams rs_get_ind
#'
#' @export
is_blank_line_needed_below <- function(where = c("last row", "first row"),
                                       context = rs_get_context()) {
    where <- match.arg(where)
    row <- switch(where,
                  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  "first" = rs_get_ind_first_selected_row(context),
                  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  "last" = rs_get_ind_last_selected_row(context),
                  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  stop("Incorrect choice of `where`"))

    # Contents of row below the selection:
    txt <- context$contents[row + 1]

    # If the last line is selected, a blank line should be added:
    if (is.na(txt)) {
        return(TRUE)
    }
    # Remove spaces and check if string is empty:
    cond <- stringi::stri_isempty(gsub("[[:space:]]", "", txt))
    # If not empty (result: FALSE), then an empty row needs to be added:
    isTRUE(!cond)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


