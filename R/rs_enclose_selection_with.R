# 'Enclose selected symbols' addins ---------------------------------------

#' Enclose selection of text with indicated symbols.
#'
#' Enclose selection of text with certain symbols which usually have special
#' meaning in R Markdown files.
#'
#' @details
#' If text is selected, the cursor is placed after the modified selection.
#' If no text is selected, the cursor is placed between the inserted symbols.
#'
#' @param symbol (character) A sequence of symbols to add on both sides of selection.
#' @param trim (logical) Flag if whitespace should be trimmed from both sides of the selection.
#' @param symbol_before (character) A sequence of symbols to before the selection
#'                      (overrides value of `symbol`).
#' @param symbol_after  (character) A sequence of symbols to add after the selection
#'                      (overrides value of `symbol`).
#'
#' @inheritParams rs_get_index
#'
#' @export
rs_enclose_selection_with <- function(symbol = "",
                                      symbol_before = symbol,
                                      symbol_after = symbol,
                                      trim = FALSE,
                                      context = rs_get_context()) {
  # For the first selection only
  old_text <- rstudioapi::selectionGet(id = context$id)$value
  Encoding(old_text) <- "UTF-8"

  if (trim) {
    new_text <- stringr::str_trim(old_text)
  } else {
    new_text <- old_text
  }

  new_text <- paste0(symbol_before, new_text, symbol_after)

  rstudioapi::selectionSet(new_text, context$id)

  # If no text is selected, cursor is placed between the symbols.
  if (stringi::stri_isempty(old_text)) {
    # "try" function is needed to avoid error in Rmd Visual mode
    try(silent = TRUE, {
      sel <- context$selection[[1]]
      rng <- sel$range
      rng[[1]]["column"] <- rng[[1]]["column"] + nchar(symbol_before)

      setCursorPosition(position = rng[[1]], id = context$id)
    })
  }
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Legacy version of the function
rs_enclose_selection_with__old <- function(symbol = "",
                                           symbol_before = symbol,
                                           symbol_after = symbol,
                                           trim = FALSE,
                                           context = rs_get_context()) {
  # For the first selection only
  sel <- context$selection[[1]]
  old_text <- sel$text
  Encoding(old_text) <- "UTF-8"

  if (trim) {
    new_text <- stringr::str_trim(old_text)
  } else {
    new_text <- old_text
  }

  new_text <- paste0(symbol_before, new_text, symbol_after)

  insertText(location = sel$range, text = new_text, id = context$id)

  # If no text is selected, cursor is placed between the symbols.
  if (stringi::stri_isempty(old_text)) {
    rng <- sel$range
    rng[[1]]["column"] <- rng[[1]]["column"] + nchar(symbol_before)

    setCursorPosition(position = rng[[1]], id = context$id)
  }
}
# ============================================================================
