#' Get length of selection
#'
#' Calculate number of characters in the first selection.
#'
#' @inheritParams rs_get_index
#'
#' @return Integer with number of characters in the first selection.
#' @export
rs_get_selection_length <- function(context = rs_get_context()) {
    nchar(rs_get_selection_text(context = context))
}
rs_get_row_lengths <- function(row, end_row = NULL, context = rs_get_context()) {
    nchar(rs_get_rows(context = context))
}

#' Get selection text
#'
#' Get the text in the first selection.
#'
#' @inheritParams rs_get_index
#'
#' @return String.
#' @export
rs_get_selection_text <- function(context = rs_get_context()) {
    context$selection[[1]]$text
}

#' Get range of selection
#'
#' Get the range of the first selection.
#'
#' @inheritParams rs_get_index
#'
#' @return "document_range" object.
#' @export
rs_get_selection_range <- function(context = rs_get_context()) {
    context$selection[[1]]$range
}
