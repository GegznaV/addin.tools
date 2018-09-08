#' Insert text at the beginning of the first selected row.
#'
#' @param text (character) The text to insert.
#' @param ensure_blank_above (logical)
#'      If \code{TRUE}, inserts blank line above the selection if the line is not blank.
#'      If \code{FALSE}, no blank line is added.
#'
#' @inheritParams rs_get_ind
#' @export
rs_insert_before_first_selected_row <-
    function(text = "",
             ensure_blank_above = FALSE,
             context = rs_get_context()) {

        row <- rs_get_ind_first_selected_row(context)

        location <- list(start = c(row, 1),
                         end   = c(row, 1))
        class(location) <- "document_range"

        text <- ensure_blank_line(text, context, above = ensure_blank_above)

        rstudioapi::insertText(
            location = location,
            text = text,
            id = context$id
        )
    }
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

