# Main "replace" function
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Replace text in selection.
#'
#' @param pattern (character) \cr
#'        A pattern of text to be replaced.
#' @param replacement (character) \cr
#'        The replacement text.
#' @param keep_selected (logical) \cr
#'       Flag indicating if the selection should be kept after add-in is applied.
#' @param fixed (logical) \cr
#'        If \code{TRUE}, the pattern is a fixed expression.
#'        If \code{FALSE}, the pattern is a regular expression.
#'
#' @inheritParams rs_get_index
#'
#' @seealso \code{\link[base]{gsub}}
#' @export
rs_replace_in_selection <- function(pattern, replacement,
  fixed = TRUE,
  keep_selected = TRUE,
  selection = c("all", "first", "last"),
  context = rs_get_context()) {
  selection <- match.arg(selection)

  old_text  <- rs_get_selection_text(selection = selection, context = context)
  old_range <- rs_get_selection_range(
    selection = selection, context = context, as_list = TRUE
  )

  new_text <- gsub(
    pattern = pattern, replacement = replacement, x = old_text, fixed = fixed
  )

  modifyRange(location = old_range, text = new_text, id = context$id)

  if (keep_selected) {
    select_correct_range(old_text, new_text, old_range, id = context$id)
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname rs_replace_in_selection
#' @export
rs_replace_selection <- function(replacement, keep_selected = TRUE,
  selection = c("all", "first", "last"),
  context = rs_get_context()) {

  old_range <- rs_get_selection_range(selection = selection, context = context)

  modifyRange(
    location = old_range, text = as.character(replacement), id = context$id
  )

  if (keep_selected) {
    old_text <- rs_get_selection_text(selection = selection, context = context)
    select_correct_range(old_text, replacement, old_range, id = context$id)
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Select currently modiffied and inserted pieces of text.
# (correctly adjust the selection)

#' @rdname rs_replace_in_selection
#' @param old_text (character) Original text.
#' @param new_text (character) Text after correction.
#' @param old_range `document_range` object. See [rstudioapi::document_range].
#' @param id Document ID. See [getActiveDocumentContext()].
#' @details
#' `select_correct_range()` correctly adjust current selection.
#'
#' @export
select_correct_range <- function(old_text, new_text, old_range, id = NULL) {

  segment_size <- function(str, pattern) {
    nchar(stringr::str_extract(str, pattern))
  }

  last_line_segment_size <- function(str) {
    # "(?<=\n).*?$" text segment in the last line of a selection, i.e.,
    #               at the beggining of a line.
    segment_size(str, "(?<=\n).*?$")
  }

  single_line_segment_size <- function(str) {
    # "^[^\n]*$"    text selection that spans only one line.
    segment_size(str, "^[^\n]*$")
  }

  new_df <-
    tibble::tibble(old_range = old_range, old_text, new_text) %>%
    dplyr::mutate(
      rng = purrr::map(old_range, ~ tibble::as_tibble(t(unlist(.))))
    ) %>%
    tidyr::unnest_legacy(rng) %>%
    dplyr::mutate(
      difference = dplyr::coalesce( # Difference in number of characters per selection
        # If selection spans one line
        single_line_segment_size(new_text) - single_line_segment_size(old_text),
        # If selection spans several lines, take senment fron the last one
        last_line_segment_size(new_text) - last_line_segment_size(old_text),
      ),
      special =
        # Check if another selection exists in the same line before current selection
        start.row == dplyr::lag(end.row, default = 0) &
        # Check if this boundary (column position)
        # is the first start of selection in the line
        start.row != dplyr::lag(start.row, default = 0)
    ) %>%
    dplyr::group_by(end.row) %>%
    dplyr::mutate(end_diff = cumsum(difference)) %>%
    dplyr::group_by(start.row) %>%
    dplyr::mutate(
      # If the first selection of a line started in the previous line, indices
      # should be modiffied to indicate correct possitions.
      modify_group = dplyr::first(special),
      start_diff = cumsum(dplyr::lag(difference, default = 0))
    ) %>%
    dplyr::ungroup()

  new_range <-
    new_df %>%
    dplyr::mutate(
      start_diff = dplyr::if_else(
        modify_group,
        true = as.double(dplyr::lag(end_diff, default = 0)),
        false = start_diff
      ),

      # Calculate new new positions
      new_start.column = start.column + start_diff,
      new_end.column = end.column + end_diff,
      new_range = purrr::pmap(
        list(start.row, new_start.column, end.row, new_end.column),
        ~ document_range(c(..1, ..2), c(..3, ..4))
      )
    ) %>%
    dplyr::pull(new_range)

  setSelectionRanges(new_range, id = id)
}
