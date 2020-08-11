#' Get selection text.
#'
#' Get the text in either the first selection or all selections.
#'
#' @inheritParams rs_get_index
#' @param as_list (logical)
#'        Flag, if result should be a list, if \code{selection} is either
#'        \code{"first"} or \code{"last"}.
#' @return A character vector.
#' @export
rs_get_selection_text <- function(selection = c("all", "first", "last"),
                                  as_list = FALSE,
                                  context = rs_get_context()) {
  selection <- match.arg(selection)
  str <- switch(
    selection,
    "all"   = purrr::map_chr(context$selection, "text"),
    "first" = context$selection[[1]]$text,
    "last"  = context$selection[[rs_get_n_selections(context = context)]]$text
  )
  if (isTRUE(as_list)) {
    str <- as.list(str)
  }
  str
}

#' Get length of selection.
#'
#' Calculate number of characters in each selection.
#'
#' @inheritParams rs_get_index
#'
#' @return An integer vector with number of characters in each selection.
#' @export
rs_get_selection_length <- function(selection = c("all", "first", "last"),
                                    context = rs_get_context()) {
  nchar(rs_get_selection_text(context = context, selection = selection))
}

#' Get lengths of selected rows.
#'
#' Calculate number of characters in each selected row.
#'
#' @inheritParams rs_get_index
#' @param row (numeric) \cr
#'        Index of the first row of interest of a vector of row indices.
#' @param end_row (numeric | \code{NULL}) \cr
#'        Index of the last row of interest or \code{NULL}.
#'
#' @return An integer vector with number of characters in each selection.
#' @export
rs_get_row_lengths <- function(row, end_row = NULL, context = rs_get_context()) {
  nchar(rs_get_text(row = row, end_row = end_row, context = context))
}

#' Get number of selections.
#'
#' @inheritParams rs_get_index
#'
#' @return Number of selections.
#' @export
rs_get_n_selections <- function(context = rs_get_context()) {
  length(context$selection)
}

#' Get range of selection.
#'
#' Get the range of the first/each selection.
#'
#' @inheritParams rs_get_index
#' @param as_list (locical) \cr
#'        Indicates if output sould be returned as a list.
#'
#' @return Either a "document_range" object, if \code{selection} is "first" or
#'         "last", and \code{as_list = TRUE}, or a list of those objects otherwise.
#' @export
rs_get_selection_range <- function(selection = c("all", "first", "last"),
                                   as_list = FALSE, # TODO: default to as_list = TRUE
                                   context = rs_get_context()) {
  selection <- match.arg(selection)

  range_obj <- switch(selection,
    "all" = purrr::map(context$selection, "range"), # returns a list of range objects
    "first" = context$selection[[1]]$range, # returns range object
    "last" = {
      n <- rs_get_n_selections(context = context)
      context$selection[[n]]$range
    }
  )

  if (isTRUE(as_list)) {
    range_obj <- switch(selection, "first" = , "last" = list(range_obj), range_obj)
  }

  range_obj
}
