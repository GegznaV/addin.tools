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
#' @inheritParams rs_get_ind
#'
#' @seealso \code{\link[base]{gsub}}
#' @export
rs_replace_in_selection <- function(pattern, replacement, fixed = TRUE,
                                    keep_selected = TRUE, context = rs_get_context()) {

    sel <- context$selection[[1]]
    old_text <- sel$text
    Encoding(old_text) <- "UTF-8"

    new_text <- gsub(pattern = pattern,
                     replacement = replacement,
                     x = old_text,
                     fixed = fixed)

    rstudioapi::modifyRange(location = sel$range,
                            text = as.character(new_text),
                            id = context$id)

    if (keep_selected) {
        new_range <- sel$range

        if (new_range$start[["row"]] == new_range$end[["row"]]) {
            # If selection spans only one row, the beggining of the selection is chosen.
            end_ind <- new_range$start[["column"]] + nchar(new_text)
        } else {
            # If selection spans several rows, the beggining of the last row is chosen.
            end_ind <- nchar(stringr::str_extract(new_text, "(?<!\n).*?$")) + 2
        }
        new_range$end[["column"]] <- end_ind
        rstudioapi::setSelectionRanges(new_range)
    }

}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

