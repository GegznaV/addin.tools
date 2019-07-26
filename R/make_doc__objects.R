# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name create_objects
#' @title Create objects for rstudioapi.
#'
#' Create \code{"document_position"} and \code{"document_range"} objects.
#'
#' @param row,column,start.row,start.column,end.row,end.column (integer)\cr
#'  Single positive number that indicates certain position.
#'
#' @return An object of appropriate class.
#' @export
#'
#' @examples
#' make_doc_position(2, 5)
#'
#' make_doc_range(2, 5, 2, 6)

make_doc_position <- function(row, column) {
    checkmate::assertIntegerish(row,    len = 1, lower = 1)
    checkmate::assertIntegerish(column, len = 1, lower = 1)

    structure(c(row = row, column = column), class = "document_position")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname create_objects
#' @export

make_doc_range <- function(start.row, start.column, end.row, end.column) {
    checkmate::assertIntegerish(start.row,    len = 1, lower = 1)
    checkmate::assertIntegerish(start.column, len = 1, lower = 1)
    checkmate::assertIntegerish(end.row,      len = 1, lower = 1)
    checkmate::assertIntegerish(end.column,   len = 1, lower = 1)

    structure(
        list(start = make_doc_position(start.row, start.column),
             end   = make_doc_position(end.row,     end.column)),
        class = "document_range")
}
