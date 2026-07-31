# Insert text at the beginning of the row(s).

Insert text at the beginning of the row(s).

## Usage

``` r
rs_insert_at_row_start(rows, text = NULL, id = rs_get_context()$id)
```

## Arguments

- rows:

  (integer)  
  The index(es) of the row(s) in the document.

- text:

  (character)  
  The text to insert. This should either be length one (in which case,
  this text is applied to each specified row); otherwise, it should be
  the same length as the `rows` vector.

- id:

  The document id. When `NULL` or blank, the requested operation will
  apply to the currently open, or last focused, RStudio document.
