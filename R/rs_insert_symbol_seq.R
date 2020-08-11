#' Insert a sequence of symbols.
#'
#' @param symbol (character) A sequence of symbols to be repeated
#' @param start_column (integer) Column position where the sequence begins.
#' @param end_column (integer) Column position where the sequence stops.
#' @inheritParams rs_get_index
#'
#' @export
rs_insert_symbol_seq <- function(symbol,
                                 start_column = 1,
                                 end_column = 80,
                                 context = rs_get_context()) {
  str <- repeat_symbol(as.character(symbol), end_column - start_column)
  insertText(text = str, id = context$id)
}
