#' @name rs_get_position
#' @title  Get position(s) related to the selection(s).
#' @description
#' Get position(s) related to the selection(s).
#'
#' @param selection (string) \cr
#'        String that indicates, which selection should be extracted.
#'
#' @param context (class \code{document_context}) \cr
#'        Object with context of active RStudio document.
#'
#' @return A list of \code{docdument_position} objects.

NULL


#' @rdname rs_get_position
#' @export
rs_get_position_selection_start <- function(selection = c("all", "first", "last"),
                                            context = rs_get_context()) {
    selection <- match.arg(selection)
    purrr::map(rs_get_selection_range(selection, context = context), "start")
}

#' @rdname rs_get_position
#' @export
rs_get_position_selection_end <- function(selection = c("all", "first", "last"),
                                          context = rs_get_context()) {

    purrr::map(rs_get_selection_range(selection, context = context), "end")
}


# ============================================================================
# FIXME: remove these old functions (below), when possible.
# ============================================================================

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_get_position
#' @export
rs_get_index_selection_start <- function(selection = c("first", "last"),
                                         context = rs_get_context()) {
    selection <- match.arg(selection)
    rs_get_selection_range(selection, context = context)$start
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_get_position
#' @export
rs_get_index_selection_end <- function(selection = c("last", "first"),
                                       context = rs_get_context()) {
    selection <- match.arg(selection)
    rs_get_selection_range(selection, context = context)$end
}
