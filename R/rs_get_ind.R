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
rs_get_selection_start_index <- function(selection = c("last", "first"),
                                         context = rs_get_context()) {
    selection <- match.arg(selection)
    rs_get_selection_range(selection, context = context)$start
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_get_index
#' @export
rs_get_selection_end_index <- function(selection = c("last", "first"),
                                       context = rs_get_context()) {
    selection <- match.arg(selection)
    rs_get_selection_range(selection, context = context)$end
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_get_index
#' @export
rs_get_first_selected_col_index <- function(selection = c("first", "last"),
                                            context = rs_get_context()) {
    selection <- match.arg(selection)
    rs_get_selection_start_index(selection, context = context)["column"]
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_get_index
#' @export
rs_get_last_selected_col_index <- function(selection = c("last", "first"),
                                           context = rs_get_context()) {
    selection <- match.arg(selection)
    rs_get_selection_end_index(selection, context = context)["column"]
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_get_index
#' @export
rs_get_first_selected_row_index <- function(context = rs_get_context()) {
    rs_get_selection_start_index(context = context)["row"]
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_get_index
#' @export
rs_get_last_selected_row_index <- function(selection = c("last", "first"),
                                           context = rs_get_context()) {
    selection <- match.arg(selection)
    rs_get_selection_range(selection, context = context)$end["row"]
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_get_index
#' @export
rs_get_selected_row_indexes <- function(context = rs_get_context()) {
    first <- rs_get_selection_start_index("first", context = context)["row"]
    last  <- rs_get_selection_end_index("last",    context = context)["row"]
    first:last
}
