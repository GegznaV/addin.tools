# TODO:
# + 1) rename functions (regex):
#          from:    (rs_get_)(.*?)_(index).*?( |\()
#          to:      \1\3_\2\4

#' @name rs_get_index
#' @title  Get indices related to the selection(s).
#' @description
#' Get indices related to the selection(s).
#'
#' @param context (class \code{document_context}) \cr
#'  Object with context of active RStudio document.
#'
#' @param selection (string) \cr
#'  String, that indicates, which selection should be extracted.
NULL

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_get_index
#' @export
rs_get_index_selection_start <- function(selection = c("first", "last"),
                                         context = rs_get_context()) {
    selection <- match.arg(selection)
    rs_get_selection_range(selection, context = context)$start
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_get_index
#' @export
rs_get_index_selection_end <- function(selection = c("last", "first"),
                                       context = rs_get_context()) {
    selection <- match.arg(selection)
    rs_get_selection_range(selection, context = context)$end
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_get_index
#' @export
rs_get_index_first_selected_col <- function(selection = c("first", "last"),
                                            context = rs_get_context()) {
    selection <- match.arg(selection)
    rs_get_index_selection_start(selection, context = context)["column"]
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_get_index
#' @export
rs_get_index_last_selected_col <- function(selection = c("last", "first"),
                                           context = rs_get_context()) {
    selection <- match.arg(selection)
    rs_get_index_selection_end(selection, context = context)["column"]
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_get_index
#' @export
rs_get_index_first_selected_row <- function(selection = c("first", "last"),
                                            context = rs_get_context()) {
    rs_get_index_selection_start(selection, context = context)["row"]
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_get_index
#' @export
rs_get_index_last_selected_row <- function(selection = c("last", "first"),
                                           context = rs_get_context()) {

    selection <- match.arg(selection)
    rs_get_selection_range(selection, context = context)$end["row"]
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_get_index
#' @export
# Get indices of rows in each selections
# There may be several coinciding row indices, if there are several selections
# per row.
rs_get_index_selected_row <- function(context = rs_get_context()) {

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
