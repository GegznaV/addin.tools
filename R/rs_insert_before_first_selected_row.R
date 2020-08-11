#' Insert text at the beginning of the first selected row.
#'
#' @param text (character) The text to insert.
#' @param ensure_blank_above (logical)
#'      If \code{TRUE}, inserts blank line above the selection if the line is not blank.
#'      If \code{FALSE}, no blank line is added.
#'
#' @inheritParams rs_get_index
#' @export
rs_insert_before_first_selected_row <- function(text = "",
                                                ensure_blank_above = FALSE,
                                                context = rs_get_context()) {
  row <-
    rs_get_index_first_selected_row(selection = "first", context = context)

  location <- document_range(
    start = c(row, 1),
    end   = c(row, 1)
  )

  text <-
    ensure_blank_line(text, context = context, above = ensure_blank_above)

  insertText(location = location, text = text, id = context$id)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
