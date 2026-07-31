# Get text in indicated lines.

Get text in all indicated consequtive lines.

## Usage

``` r
rs_get_text(row, end_row = NULL, context = rs_get_context())

rs_get_selected_rows(context = rs_get_context())

rs_get_first_selected_row(context = rs_get_context())

rs_get_first_selected_row_length(context = rs_get_context())

rs_get_last_selected_row(context = rs_get_context())

rs_get_last_selected_row_length(context = rs_get_context())
```

## Arguments

- row:

  (integer) Index of the first row of interest.

- end_row:

  (integer) Index of the last row of interest.

- context:

  (class `document_context`)  
  Object with context of active RStudio document.

## Value

Character vector with attribute `row_numbers` indicating which rows were
returned.
