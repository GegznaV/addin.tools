# Select rows

Select rows

## Usage

``` r
rs_select_rows(first, last = NULL, context = rs_get_context())

rs_select_all_selected_rows(context = rs_get_context())

rs_select_first_selected_row(context = rs_get_context())

rs_select_last_selected_row(context = rs_get_context())

rs_deselect_range(context = rs_get_context())
```

## Arguments

- first:

  (integer)  
  Either index of the first row to select or a vector of indices. If
  `length(first) == 0` then current selection is deselected. If `last`
  is not `NULL`, only the first value is used.

- last:

  (integer)  
  Index of the last row to select or `NULL`. If not `NULL`, all lines
  from the `first[1]` to the `last[1]` are selected.

- context:

  (class `document_context`)  
  Object with context of active RStudio document.
