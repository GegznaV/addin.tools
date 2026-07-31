# Insert text at the cursor position.

Insert text at the cursor position.

## Usage

``` r
rs_insert_text(text = NULL, context = rs_get_context(), spaces = FALSE)
```

## Arguments

- text:

  (character)  
  The text to insert.

- context:

  (class `document_context`)  
  Object with context of active RStudio document.

- spaces:

  (logical)  
  If `TRUE`, ensures that text is surrounded by spaces.
