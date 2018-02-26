# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Insert text at the beginning of the row.
#'
#' @param row (integer) The index of the row in the document.
#' @param text (character) The text to insert.
#' @inheritParams rstudioapi::insertText
#' @export
rs_insert_at_row_start <- function(row,
                                   text = NULL,
                                   id = NULL) {
    row <- row[1]
    location <- list(start = c(row, 1),
                     end   = c(row, 1))
    class(location) <- "document_range"

    rstudioapi::insertText(location = location,
                           text = text,
                           id = id)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Insert text at the cursor position.
#'
#' @param text (character) The text to insert.
#' @param spaces (logical) If \code{TRUE}, ensures that text is
#'                         surrounded by spaces.
#'
#' @inheritParams rs_get_ind
#'
#' @inheritParams rstudioapi::insertText
#' @export
rs_insert_text <- function(text = NULL,
                           context = rs_get_context(),
                           spaces = FALSE) {

    start <- rs_get_ind_selection_start(context = context)
    end   <- rs_get_ind_selection_end(context = context)

    if (spaces) {
        spc_before <- check_space(postition = start - c(0, 1), context = context)
        spc_after  <- check_space(postition = end   + c(0, 1), context = context)

        text <- stringr::str_c(" "[!spc_before], text, " "[!spc_after])
    }

    rstudioapi::insertText(text = text,
                           id = context$id)
}

# Check if the symbol is space at the indicated position
check_space <- function(postition, context = rs_get_context()) {
    txt <- rs_get_text(postition, context = context)
    isTRUE(stringr::str_detect(txt, " "))
}
