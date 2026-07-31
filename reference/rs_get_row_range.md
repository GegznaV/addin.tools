# Get row range of the indicated line

`rs_get_row_range()` gets range of the indicated row from the beginning
to the end of the line (excluding the new line symbol).  
`rs_get_row_range_w_newline()` gets range of the indicated row from the
beggining of the line to the beginning to the next line.  
`rs_get_first_selected_row_range()` gets range of the first selected
line.

## Usage

``` r
rs_get_row_range(row, context = rs_get_context())

rs_get_row_ranges(row, context = rs_get_context())

rs_get_row_range_w_newline(row)

rs_get_first_selected_row_range(
  include_newline = FALSE,
  context = rs_get_context()
)
```

## Arguments

- row:

  (ineger) row index.

- context:

  (class `document_context`)  
  Object with context of active RStudio document.

- include_newline:

  (logical) indicates if the new line symbol should be included in the
  range

## Value

An object of class "document_range" or a list of these objects.
