#' @name rs_get_index
#' @title  Get indices from the selection.
#' @description
#' Get index of the first/last row/column in the selection.
#'
#' @param context Object with context of active RStudio document
#'                (class \code{document_context}).
NULL

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_get_index
#' @export
rs_get_first_selected_col_index <- function(context = rs_get_context()) {
    context$selection[[1]]$range$start["column"]
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_get_index
#' @export
rs_get_last_selected_col_index <- function(context = rs_get_context()) {
    context$selection[[1]]$range$end["column"]
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_get_index
#' @export
rs_get_first_selected_row_index <- function(context = rs_get_context()) {
    context$selection[[1]]$range$start["row"]
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_get_index
#' @export
rs_get_last_selected_row_index <- function(context = rs_get_context()) {
    context$selection[[1]]$range$end["row"]
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_get_index
#' @export
rs_get_selected_row_indexes <- function(context = rs_get_context()) {
    first <- context$selection[[1]]$range$start["row"]
    last  <- context$selection[[1]]$range$end["row"]
    first:last
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_get_index
#' @export
rs_get_selection_start_index <- function(context = rs_get_context()) {
    context$selection[[1]]$range$start
}
#' @rdname rs_get_index
#' @export
rs_get_selection_end_index <- function(context = rs_get_context()) {
    context$selection[[1]]$range$end
}




