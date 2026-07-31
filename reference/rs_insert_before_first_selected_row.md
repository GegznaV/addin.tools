# Insert text at the beginning of the first selected row.

Insert text at the beginning of the first selected row.

## Usage

``` r
rs_insert_before_first_selected_row(
  text = "",
  ensure_blank_above = FALSE,
  context = rs_get_context()
)
```

## Arguments

- text:

  (character) The text to insert.

- ensure_blank_above:

  (logical) If `TRUE`, inserts blank line above the selection if the
  line is not blank. If `FALSE`, no blank line is added.

- context:

  (class `document_context`)  
  Object with context of active RStudio document.
