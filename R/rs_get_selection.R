#' Length of selection
#'
#' Calculate number of characters in the first selection.
#'
#' @inheritParams rs_get_ind
#'
#' @return Integer with number of characters in the first selection.
#' @export
rs_get_selection_length <- function(context = rs_get_context()) {
    nchar(rs_get_selection_text(context))
}

#' Get selection text
#'
#' Get the text in the first selection.
#'
#' @inheritParams rs_get_ind
#'
#' @return String.
#' @export
rs_get_selection_text <- function(context = rs_get_context()) {
    context$selection[[1]]$text
}
