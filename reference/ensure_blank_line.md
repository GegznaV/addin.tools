# Add blank lines if needed.

Add blank lines if needed.

## Usage

``` r
ensure_blank_line(
  text,
  context = rs_get_context(),
  above = FALSE,
  below_first_row = FALSE,
  below_selection = FALSE
)
```

## Arguments

- text:

  (character) Text to modify.

- context:

  (class `document_context`)  
  Object with context of active RStudio document.

- above:

  (logical) If `TRUE`, checks for the blank line above the selection and
  if it is missing, adds blank line before the `text`.

- below_first_row:

  (logical) If `TRUE`, checks for the blank line below the first
  selected row and if it is missing, adds blank line after the `text`.

- below_selection:

  (logical) If `TRUE`, checks for the blank line below the last selected
  row and if it is missing, adds blank line after the `text`.

## Value

String.
