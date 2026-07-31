# Get lengths of selected rows.

Calculate number of characters in each selected row.

## Usage

``` r
rs_get_row_lengths(row, end_row = NULL, context = rs_get_context())
```

## Arguments

- row:

  (numeric)  
  Index of the first row of interest of a vector of row indices.

- end_row:

  (numeric \| `NULL`)  
  Index of the last row of interest or `NULL`.

- context:

  (class `document_context`)  
  Object with context of active RStudio document.

## Value

An integer vector with number of characters in each selection.
