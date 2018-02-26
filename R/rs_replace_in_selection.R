# Main "replace" function
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Replace text in selection.
#'
#' @param pattern (character) A pattern of text to be replaced.
#' @param replacement (character) The replacement text.
#' @param fixed (logical)
#'        If \code{TRUE}, the pattern is a fixed expression.
#'        If \code{FALSE}, the pattern is a regular expression.
#'
#' @inheritParams rs_get_ind
#'
#' @seealso \code{\link[base]{gsub}}
#' @export
rs_replace_in_selection <- function(pattern,
                                    replacement,
                                    fixed = TRUE,
                                    context = rs_get_context()) {

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

}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
