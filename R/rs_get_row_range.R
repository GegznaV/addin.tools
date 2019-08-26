#' Get row range of the indicated line
#'
#'
#' `rs_get_row_range()` gets range of the indicated row from the beginning to the end of the line (excluding the new line symbol).\cr
#' `rs_get_row_range_w_newline()` gets range of the indicated row from the beggining of the line to the beginning to the next line.\cr
#' `rs_get_first_selected_row_range()` gets range of the first selected line.
#'
#' @param row (ineger) row index.
#' @param include_newline (logical) indicates if the new line symbol should be included in the range
#' @inheritParams rs_get_index
#'
#' @return An object of class "document_range" or a list of these objects.
#'
#' @export
rs_get_row_range <- function(row, context = rs_get_context()) {
    last_col <- nchar(context$contents[row]) + 1
    document_range(c(row, 1), c(row, last_col))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_get_row_range
#' @export
rs_get_row_ranges <- function(row, context = rs_get_context()) {
    last_col <- nchar(context$contents[row]) + 1
    purrr::map2(row, last_col, ~ document_range(c(..1, 1), c(..1, ..2)))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_get_row_range
#' @export
rs_get_row_range_w_newline <- function(row) {
    document_range(c(row, 1), c(row + 1, 1))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_get_row_range
#' @export
rs_get_first_selected_row_range <- function(include_newline = FALSE,
                                            context = rs_get_context()
) {

    row <- rs_get_index_first_selected_row(context = context)

    if (include_newline) {
        rs_get_row_range_w_newline(row)
    } else {
        rs_get_row_range(row, context = context)
    }
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
