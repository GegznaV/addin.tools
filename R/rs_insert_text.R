# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Insert text at the beginning of the row(s).
#'
#' @param rows (integer) \cr
#'       The index(es) of the row(s) in the document.
#'
#' @param text (character)  \cr
#'       The text to insert. This should either be length one (in which case,
#'       this text is applied to each specified row); otherwise, it should be
#'       the same length as the \code{rows} vector.
#'
#' @inheritParams rstudioapi::insertText
#' @export
rs_insert_at_row_start <- function(rows, text = NULL, id = rs_get_context()$id) {
    location <- purrr::map(rows, ~ document_range(start = c(., 1), end = c(., 1)))
    insertText(location = location, text = text, id = id)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Insert text at the cursor position.
#'
#' @param text (character) The text to insert.
#' @param spaces (logical) If \code{TRUE}, ensures that text is surrounded by spaces.
#'
#' @inheritParams rs_get_index
#'
#' @inheritParams rstudioapi::insertText
#' @export

# TODO: Test this function before using it.

rs_insert_text <- function(text = NULL, # single string
                           context = rs_get_context(),
                           spaces = FALSE,
                           keep_selected = TRUE) {

    old_range <- rs_get_selection_range(context = context)

    new_text <- if (spaces) {
        purrr::map_chr(old_range, ~ {
            # Check if space is not present before and after the selection
            spc_before <- check_space(postition = .$start - c(0, 1), context = context)
            spc_after  <- check_space(postition = .$end   + c(0, 1), context = context)

            stringr::str_c(" "[!spc_before], text, " "[!spc_after])
        })

    } else {
        text
    }

    insertText(text = new_text, location = old_range, id = context$id)

    if (keep_selected) {
        select_correct_range(text, new_text, old_range, id = context$id)
    }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check if the symbol is space at the indicated position
check_space <- function(postition, context = rs_get_context()) {
    txt <- get_text(postition, context = context)
    isTRUE(stringr::str_detect(txt, " "))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get text
#
# @param start Either \code{"document_position"} object or vector with the
# coordinates (row and column) of the begining of the selection.
#
# @param end Either \code{"document_position"} object or vector with the
# coordinates (row and column) of the end of the selection.
#
# @inheritParams rs_get_index
#
# @return Character vector (extracted strings).

get_text <- function(start, end = start, context = rs_get_context()) {
    start[start < 0] <- 0
    end[end < 0]     <- 0

    text <- context$contents[start[1]:end[1]]
    stringr::str_sub(text, start[2], end[2])
}

