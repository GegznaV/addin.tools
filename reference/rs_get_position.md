# Get position(s) related to the selection(s).

Get position(s) related to the selection(s).

## Usage

``` r
rs_get_position_selection_start(
  selection = c("all", "first", "last"),
  context = rs_get_context()
)

rs_get_position_selection_end(
  selection = c("all", "first", "last"),
  context = rs_get_context()
)

rs_get_index_selection_start(
  selection = c("first", "last"),
  context = rs_get_context()
)

rs_get_index_selection_end(
  selection = c("last", "first"),
  context = rs_get_context()
)
```

## Arguments

- selection:

  (string)  
  String that indicates, which selection should be extracted.

- context:

  (class `document_context`)  
  Object with context of active RStudio document.

## Value

A list of `docdument_position` objects.
