#' @name rs_select_rows
#' @title Select rows
#'
#' @param first (integer) \cr
#'        Either index of the first row to select or a vector of indices.
#'        If \code{last} is not \code{NULL}, only the first value is used.
#' @param last (integer) \cr
#'        Index of the last row to select or \code{NULL}. If not \code{NULL},
#'        all lines from the \code{first[1]} to the \code{last[1]} are selected.
#' @inheritParams rs_get_index
#'
#' @export
#'
rs_select_rows <- function(first, last = NULL, context = rs_get_context()) {

    if (is.null(last)) {
        sel_range <- purrr::map(first, ~ document_range(c(..1, 1), c(..1, Inf)))

    } else {
        sel_range <- document_range(c(first[1], 1), c(last[1], Inf))
    }

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
