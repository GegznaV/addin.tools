#' @name rs_select_rows
#' @title Select rows
#'
#' @param first (integer) Index of the first row to select.
#' @param last (integer) Index of the last row to select.
#' @inheritParams rs_get_index
#'
#' @export
#'
rs_select_rows <- function(first, last, context = rs_get_context()) {
    sel_range <- document_range(c(first , 1), c(last, Inf))
    setSelectionRanges(sel_range, id = context$id)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Selects all rows (inclusice) from the first to the last selection
#' @rdname rs_select_rows
#' @export
rs_select_all_selected_rows <- function(context = rs_get_context()) {
    rs_select_rows(
        first = rs_get_index_first_selected_row(context = context),
        last  = rs_get_index_last_selected_row(context = context),
        context = context)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_select_rows
#' @export
rs_select_first_selected_row <- function(context = rs_get_context()) {
    row_ind <- rs_get_index_first_selected_row(context = context)
    rs_select_rows(first = row_ind, last = row_ind, context = context)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_select_rows
#' @export
rs_select_last_selected_row <- function(context = rs_get_context()) {
    row_ind <- rs_get_index_last_selected_row(context = context)
    rs_select_rows(first = row_ind, last = row_ind, context = context)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
