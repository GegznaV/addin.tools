# Enclose setected rows with text.

Enclose selected rows with text:

- `rs_enclose_all_with_lines()` - above and below all selected rows;

- `rs_enclose_first_row_with_lines()` - above and below the first
  selected row only.

## Usage

``` r
rs_enclose_selected_rows_with(
  text_above = NA,
  text_below = NA,
  ensure_blank_above = FALSE,
  context = rs_get_context()
)

rs_enclose_first_row_with(
  text_above = NULL,
  text_below = NULL,
  ensure_blank_above = FALSE,
  context = rs_get_context()
)
```

## Arguments

- text_above:

  (character) Text to be inserted above the selection.

- text_below:

  (character) Text to be inserted below the selection (the first row).

- ensure_blank_above:

  (logical) If `TRUE`, checks if there is a blank line above the
  selection. If the line is not blank, a blank line will be added. If
  `FALSE`, blank line is not added.

- context:

  (class `document_context`)  
  Object with context of active RStudio document.
