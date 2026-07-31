# Get selection text.

Get the text in either the first selection or all selections.

## Usage

``` r
rs_get_selection_text(
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

  (logical) Flag, if result should be a list, if `selection` is either
  `"first"` or `"last"`.

- context:

  (class `document_context`)  
  Object with context of active RStudio document.

## Value

A character vector.
