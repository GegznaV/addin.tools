#' @name rs_get_index
#' @title  Get indices related to the selection(s).
#' @description
#' Get indices related to the selection(s).
#'
#' @param selection (string) \cr
#'        String that indicates, which selection should be extracted.
#'
#' @param context (class `document_context`) \cr
#'        Object with context of active RStudio document.

NULL


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_get_index
#' @export
rs_get_index_first_selected_col <- function(selection = c("first", "last"),
                                            context = rs_get_context()) {
  selection <- match.arg(selection)
  rs_get_index_selection_start(selection, context = context)["column"]
}

# FIXME: use this new function.
rs_get_index_first_selected_col <- function(selection = c("all", "first", "last"),
                                            context = rs_get_context()) {
  purrr::map_dbl(rs_get_position_selection_start(selection, context = context), "column")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @rdname rs_get_index
#' @export
rs_get_index_last_selected_col <- function(selection = c("all", "last", "first"),
                                           context = rs_get_context()) {
  selection <- match.arg(selection)
  rs_get_index_selection_end(selection, context = context)["column"]
}

# FIXME: use this new function.
rs_get_index_last_selected_col <- function(selection = c("all", "last", "first"),
                                           context = rs_get_context()) {
  purrr::map_dbl(rs_get_position_selection_end(selection, context = context), "column")
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_get_index
#' @export
rs_get_index_first_selected_row <- function(selection = c("first", "last"),
                                            context = rs_get_context()) {
  rs_get_index_selection_start(selection, context = context)["row"]
}

# FIXME: use this new function.
rs_get_index_first_selected_row__2 <- function(selection = c("all", "first", "last"),
                                               context = rs_get_context()) {
  purrr::map_dbl(rs_get_position_selection_start(selection, context = context), "row")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_get_index
#' @export
rs_get_index_last_selected_row <- function(selection = c("last", "first"),
                                           context = rs_get_context()) {
  selection <- match.arg(selection)
  rs_get_selection_range(selection, context = context)$end["row"]
}

# FIXME: use this new function.
rs_get_index_last_selected_row__2 <- function(selection = c("all", "first", "last"),
                                              context = rs_get_context()) {
  purrr::map_dbl(rs_get_position_selection_end(selection, context = context), "row")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_get_index
#' @export
# Get indices of rows in each selections
# There may be several coinciding row indices, if there are several selections
# per row.
rs_get_index_selected_rows <- function(context = rs_get_context()) {
  ranges <- rs_get_selection_range("all", context = context)

  ranges %>%
    purrr::map(~ .[[1]]["row"]:.[[2]]["row"]) %>%
    purrr::reduce(c)
}
