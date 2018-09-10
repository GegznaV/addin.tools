#' Get text in indicated lines
#'
#' Get text in all indicated consequtive lines.
#'
#' @param row (integer) Index of the first row of interest.
#' @param end_row (integer) Index of the last row of interest.
#' @inheritParams rs_get_ind
#'
#' @return Character vector with attribute `row_numbers` indicating which rows were returned.
#' @export
#'
rs_get_text <- function(row, end_row = row, context = rs_get_context()) {
    ind <- row:end_row
    structure(context$contents[ind], row_numbers = ind)
}

#' @rdname rs_get_text
#' @export
rs_get_selected_rows <- function(context = rs_get_context()) {
    ind <- rs_get_ind_selected_rows(context)
    structure(context$contents[ind], row_numbers = ind)
}

