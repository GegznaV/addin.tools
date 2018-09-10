# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Enclose setected rows with text.
#'
#' Enclose selected rows with text: \itemize{
#'     \item \code{rs_enclose_all_with_lines()} - above and below all selected rows;
#'     \item \code{rs_enclose_first_row_with_lines()} - above and below the first selected row only.
#' }
#' @name rs_enclose_with_lines
#'
#' @param text_above (character) Text to be inserted above the selection.
#' @param text_below (character) Text to be inserted below the selection
#'                               (the first row).
#'
#' @param ensure_blank_above (logical)
#'      If \code{TRUE}, checks if there is a
#'      blank line above the selection. If line is not blank, a blank line
#'      is added. If \code{FALSE}, blank line is not added.
#'
#' @inheritParams rs_get_index
#'
#' @export
#'
#'
# REPLACEMENT:
# rs_enclose_all_with_lines -> rs_enclose_selected_rows_with
#
rs_enclose_selected_rows_with <- function(text_above = NA,
                                     text_below = NA,
                                     ensure_blank_above = FALSE,
                                     context = rs_get_context()) {

    sel <- context$selection[[1]]

    range_ <- range_first <- range_last <- sel$range

    ind_row_below <- range_$end[1] + 1

    range_first$start <- range_first$end <- c(range_$start[1], 1)
    range_last$start <-   range_last$end <- c(ind_row_below,   1)

    text_above <- ensure_blank_line(text = text_above,
                                    context = context,
                                    above = ensure_blank_above)

    # If the last line of document is selected, one line should be inserted
    add_last_line <-
        if (length(context$contents) < (ind_row_below)) "\n" else ""


    # To avoid error:
    #    firstly the text below is inserted,
    #    lastly, the text above is inserted.
    rstudioapi::insertText(location = range_last,
                           text = stringr::str_c(add_last_line, text_below, "\n"),
                           id = context$id)

    rstudioapi::insertText(location = range_first,
                           text = stringr::str_c(text_above, "\n"),
                           id = context$id)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_enclose_with_lines
#' @export
# rs_enclose_first_row_with_lines -> rs_enclose_first_row_with
rs_enclose_first_row_with <- function(text_above = NULL,
                                      text_below = NULL,
                                      ensure_blank_above = FALSE,
                                      context = rs_get_context()) {

    sel <- context$selection[[1]]

    range_ <- range_first <- range_second <- sel$range

    ind_row_below <- range_$start[1] + 1

    range_first$start  <- range_first$end  <- c(range_$start[1], 1)
    range_second$start <- range_second$end <- c(ind_row_below  , 1)

    # If the last line of document selected
    add_last_line <-
        if (length(context$contents) < (ind_row_below)) "\n" else ""

    # To avoid error:
    #    firstly the text below is inserted,
    #    lastly, the text above is inserted.
    if (!is.null(text_below)) {
        rstudioapi::insertText(location = range_second,
                               text = stringr::str_c(add_last_line, text_below, "\n"),
                               id = context$id)
    }

    if (!is.null(text_above)) {
        rstudioapi::insertText(location = range_first,
                               text = stringr::str_c(text_above, "\n"),
                               id = context$id)
    }

    # Lastly, add blank line above if needed.
    rs_insert_before_first_selected_row(ensure_blank_above = ensure_blank_above,
                                        context = context)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
