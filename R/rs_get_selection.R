#' Get length of selection
#'
#' Calculate number of characters in the each selection.
#'
#' @inheritParams rs_get_index
#'
#' @return An integer vector with number of characters in each selection.
#' @export
rs_get_selection_length <- function(context = rs_get_context(), type = c("first", "all")) {
    nchar(rs_get_selection_text(context = context, type = type))
}

rs_get_row_lengths <- function(row, end_row = NULL, context = rs_get_context()) {
    nchar(rs_get_rows(row = row, end_row = end_row, context = context))
}

#' Get selection text
#'
#' Get the text in either the first selection or all selections
#'
#' @inheritParams rs_get_index
#'
#' @return A character vector.
#' @export

rs_get_selection_text <- function(context = rs_get_context(), type = c("first", "all")) {

    type <- match.arg(type)
    switch(type,
           "first" = context$selection[[1]]$text,
           "all"   = purrr::map_chr(context$selection, "text")
    )
 }

#' Get range of selection
#'
#' Get the range of the first/each selection.
#'
#' @inheritParams rs_get_index
#'
#' @return Either a "document_range" object or a list of those objects.
#' @export
rs_get_selection_range <- function(context = rs_get_context(), type = c("first", "all")) {

    type <- match.arg(type)
    switch(type,
           "first" = context$selection[[1]]$range,            # returns range object
           "all"   = purrr::map(context$selection, "range")   # returns a list of range objects
    )
}
