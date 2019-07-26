#' Add blank lines if needed.
#'
#' @param text (character) Text to modify.
#' @param above (logical) If \code{TRUE}, checks for the blank line above the
#' selection and if it is missing, adds blank line before the \code{text}.
#'
#' @param below_first_row (logical) If \code{TRUE}, checks for the blank line below
#' the first selected row and if it is missing, adds blank line after
#' the \code{text}.
#'
#' @param below_selection (logical) If \code{TRUE}, checks for the blank line
#' below the last selected row and if it is missing, adds blank line after
#' the \code{text}.
#'
#' @inheritParams rs_get_index
#'
#' @return String.
#' @export
#'
ensure_blank_line <- function(text,
                              context = rs_get_context(),
                              above = FALSE,
                              below_first_row = FALSE,
                              below_selection = FALSE) {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (above) {
        if (is_blank_line_needed_above(context = context)) {
            text <- paste0("\n", text)
        }
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Checks if blank line is needed below the first selected row
    if (below_first_row) {
        if (is_blank_line_needed_below("first row", context = context)) {
            text <- paste0(text, "\n")
        }
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Checks if blank line is needed below the last selected row
    if (below_selection) {
        if (is_blank_line_needed_below("last row", context = context)) {
            text <- paste0(text, "\n")
        }
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    text
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
