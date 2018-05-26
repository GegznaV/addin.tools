#' Repeat sequence of symbols.
#'
#' @param text (character) The symbol (or sequence of symbols) to be repeated
#'             until desired length is achieved.
#' @param length.out (integer) The total length (in characters) of the sequence.
#' @param times (integer) Times to repeat the sequence.
#'
#' @return Sting of defined length.
#' @export
#'
#' @importFrom magrittr "%>%"
#'
#' @examples
#'
#' repeat_symbol(".", 10)
#'
#' repeat_symbol("..+", 10)
repeat_symbol <- function(text, length.out) {
    rep_len(text, length.out) %>%
    paste0(collapse = "") %>%
    substr(1, length.out)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname repeat_symbol
#' @export
repeat_symbol_2 <- function(text = " ", times) {
    checkmate::assert_character(text)
    checkmate::assert_integerish(times, any.missing = TRUE, all.missing = TRUE)
    times <- if (is.na(times)) 0 else times
    paste0(rep(" ", times), collapse = "")
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Generate spaces of certain length
#'
#' @param n (integer) A vertor of integers indicating number of scoaaces to generate.
#'
#' @return A character vector of length \code{length(n)} with spaces.
#' @export
#'
#' @examples
#' make_spaces(c(2, NA))
make_spaces <- function(n) {purrr::map_chr(n, ~ repeat_symbol_2(" ", .))}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
