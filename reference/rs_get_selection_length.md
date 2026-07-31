# Get length of selection.

Calculate number of characters in each selection.

## Usage

``` r
rs_get_selection_length(
  selection = c("all", "first", "last"),
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

An integer vector with number of characters in each selection.
