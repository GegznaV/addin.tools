# Get range of selection.

Get the range of the first/each selection.

## Usage

``` r
rs_get_selection_range(
  selection = c("all", "first", "last"),
  as_list = FALSE,
  context = rs_get_context()
)
```

## Arguments

- selection:

  (string)  
  String that indicates, which selection should be extracted.

- as_list:

  (locical)  
  Indicates if output sould be returned as a list.

- context:

  (class `document_context`)  
  Object with context of active RStudio document.

## Value

Either a "document_range" object, if `selection` is "first" or "last",
and `as_list = TRUE`, or a list of those objects otherwise.
