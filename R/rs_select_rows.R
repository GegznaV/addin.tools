#' Select rows
#'
#' @param first (integer) Index of the first row to select.
#' @param last (integer) Index of the last row to select.
#' @inheritParams rs_get_index
#'
#' @export
#'
rs_select_rows <- function(first, last, context = rs_get_context()) {
    sel_range <- rstudioapi::document_range(c(first , 1), c(last, Inf))
    rstudioapi::setSelectionRanges(sel_range, id = context$id)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rd rs_select_rows
#' @export
rs_select_all_selected_rows <- function(context = rs_get_context()) {
    rs_select_rows(first = rs_get_first_selected_row_index(context),
                   last  = rs_get_last_selected_row_index(context),
                   context = context)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rd rs_select_rows
#' @export
rs_select_first_selected_row <- function(context = rs_get_context()) {
    row_ind <- rs_get_first_selected_row_index(context)
    rs_select_rows(first = row_ind, last  = row_ind(context), context = context)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rd rs_select_rows
#' @export
rs_select_last_selected_row <- function(context = rs_get_context()) {
    row_ind <- rs_get_last_selected_row_index(context)
    rs_select_rows(first = row_ind, last  = row_ind(context), context = context)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~