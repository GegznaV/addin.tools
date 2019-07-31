#' @name rs_get_index
#' @title  Get indices from the selection.
#' @description
#' Get index of the first/last row/column in the selection.
#'
#' @param context Object with context of active RStudio document
#'                (class \code{document_context}).
#' @param selection (string) \cr
#'              String, that indicates, which selection should be extracted.
NULL

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_get_index
#' @export
rs_get_selection_start_index <- function(selection = c("first", "last"),
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
rs_get_first_selected_row_index <- function(selection = c("first", "last"),
                                            context = rs_get_context()) {
    rs_get_selection_start_index(selection, context = context)["row"]
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
# Get indices of rows in each selections
# There may be several coinciding row indices, if there are several selections
# per row
rs_get_selected_row_indexes <- function(context = rs_get_context()) {

    ranges <- rs_get_selection_range("all", context = context)
    rows <-
        ranges %>%
        purrr::map(~ .[[1]]["row"]:.[[2]]["row"]) %>%
        purrr::reduce(c)

    # first <- min(rows)
    # last  <- max(rows)
    # rows <- first:last
    rows
}
