# Main "replace" function
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Replace text in selection.
#'
#' @param pattern (character) A pattern of text to be replaced.
#' @param replacement (character) The replacement text.
#' @param keep_selected (logical) Flag indicating if the selection should be
#'                      kept after add-in was applied.
#' @param fixed (logical)
#'        If \code{TRUE}, the pattern is a fixed expression.
#'        If \code{FALSE}, the pattern is a regular expression.
#'
#' @inheritParams rs_get_index
#'
#' @seealso \code{\link[base]{gsub}}
#' @export
rs_replace_in_selection <- function(pattern, replacement, fixed = TRUE,
                                    keep_selected = TRUE, context = rs_get_context()) {

    current_range <- rs_get_selection_range(context = context)
    old_text      <- rs_get_selection_text(context = context)

    new_text <- gsub(pattern = pattern,
                     replacement = replacement,
                     x = old_text,
                     fixed = fixed)

    rstudioapi::modifyRange(location = current_range,
                            text = as.character(new_text),
                            id = context$id)

    if (keep_selected)
        select_correct_range(new_text, current_range)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rd rs_replace_in_selection
#' @export
rs_replace_selection <- function(replacement, keep_selected = TRUE,
                                 context = rs_get_context()) {

    current_range <- rs_get_selection_range(context = context)

    rstudioapi::modifyRange(location = current_range,
                            text = as.character(replacement),
                            id = context$id)

    if (keep_selected) {
        select_correct_range(replacement, current_range)
    }
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# If new text and current range is given, selects new text which replaced
# current celection.
select_correct_range <- function(new_text, current_range) {

        if (current_range$start[["row"]] == current_range$end[["row"]]) {
        # If selection spans only one row, the beggining of the selection is chosen.
        end_ind <- current_range$start[["column"]] + nchar(new_text)
    } else {
        # If selection spans several rows, the beggining of the last row is chosen.
        end_ind <- nchar(stringr::str_extract(new_text, "(?<!\n).*?$")) + 2
    }

    current_range$end[["column"]] <- end_ind
    rstudioapi::setSelectionRanges(current_range)

}
