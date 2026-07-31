# Get indices related to the selection(s).

Get indices related to the selection(s).

## Usage

``` r
rs_get_index_first_selected_col(
  selection = c("all", "first", "last"),
  context = rs_get_context()
)

rs_get_index_last_selected_col(
  selection = c("all", "last", "first"),
  context = rs_get_context()
)

rs_get_index_first_selected_row(
  selection = c("first", "last", "all"),
  context = rs_get_context()
)

rs_get_index_last_selected_row(
  selection = c("last", "first", "all"),
  context = rs_get_context()
)

rs_get_index_selected_rows(context = rs_get_context())
```

## Arguments

- selection:

  (string)  
  String that indicates, which selection should be extracted.

- context:

  (class `document_context`)  
  Object with context of active RStudio document.
