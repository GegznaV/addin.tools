#' Get text in indicated lines.
#'
#' Get text in all indicated consequtive lines.
#'
#' @param row (integer) Index of the first row of interest.
#' @param end_row (integer) Index of the last row of interest.
#' @inheritParams rs_get_index
#'
#' @return Character vector with attribute `row_numbers` indicating which rows were returned.
#' @export
#'
rs_get_rows <- function(row, end_row = NULL, context = rs_get_context()) {
    if (is.null(end_row)) {
        ind <- row
    } else {
        ind <- row:end_row
    }
    structure(context$contents[ind], row_numbers = ind)
}

#' @rdname rs_get_rows
#' @export
rs_get_row_lengths <- function(row, end_row = NULL, context = rs_get_context()) {
    nchar(rs_get_rows(row = row, end_row = end_row, context = context))
}

#' @rdname rs_get_rows
#' @export
rs_get_selected_rows <- function(context = rs_get_context()) {
    ind <- rs_get_selected_row_indexes(context)
    structure(context$contents[ind], row_numbers = ind)
}

#' @rdname rs_get_rows
#' @export
rs_get_first_selected_row <- function(context = rs_get_context()) {
    ind <- rs_get_first_selected_row_index(context)
    structure(context$contents[ind], row_numbers = ind)
}

#' @rdname rs_get_rows
#' @export
rs_get_first_selected_row_length <- function(context = rs_get_context()) {
    nchar(rs_get_first_selected_row(context))
}

#' @rdname rs_get_rows
#' @export
rs_get_last_selected_row <- function(context = rs_get_context()) {
    ind <- rs_get_last_selected_row_index(context)
    structure(context$contents[ind], row_numbers = ind)
}

#' @rdname rs_get_rows
#' @export
rs_get_last_selected_row_length <- function(context = rs_get_context()) {
    nchar(rs_get_last_selected_row(context))
}
