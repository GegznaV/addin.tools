#' Get text
#'
#' @param start Either \code{"document_position"} object of vector with the coordinates (row and column) of the begining of the selection.
#' @param end Either \code{"document_position"} object of vector with the coordinates (row and column) of the end of the selection.
#' @inheritParams rs_get_ind

#'
#' @return Extracted string(s).
#' @export
#'
rs_get_text <- function(start, end = start, context = rs_get_context()) {
    start[start < 0] <- 0
    end[end < 0] <- 0

    text <- context$contents[start[1]:end[1]]
    stringr::str_sub(text, start[2], end[2])
}


