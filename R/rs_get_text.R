#' Get indicated lines
#'
#' @param start Either \code{"document_position"} object of vector with the coordinates (row and column) of the begining of the selection.
#' @param end Either \code{"document_position"} object of vector with the coordinates (row and column) of the end of the selection.
#' @inheritParams rs_get_ind
#'
#' @return Character vector (extracted strings).
#' @export
#'
rs_get_text <- function(start, end = start, context = rs_get_context()) {
    start[start < 0] <- 0
    end[end < 0]     <- 0

    text <- context$contents[start[1]:end[1]]
    stringr::str_sub(text, start[2], end[2])
}

#' @rdname rs_get_text
#' @export
rs_get_selected_rows <- function(context = rs_get_context()) {
    ind <- rs_get_ind_selected_rows(context)
    structure(context$contents[ind], row_numbers = ind)
}
