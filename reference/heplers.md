# ' Check if a blank line is needed below.

Checks if a blank line should be added below either all selected rows or
the first selected row.

## Usage

``` r
is_blank_line_needed_below(
  where = c("last row", "first row"),
  context = rs_get_context()
)
```

## Arguments

- where:

  (string) If `"first"` - checks for a blank line below the first
  selected row. If `"last"` - checks for a blank line below the last
  selected row.

- context:

  (class `document_context`)  
  Object with context of active RStudio document.
