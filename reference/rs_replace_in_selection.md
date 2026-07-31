# Replace text in selection.

Replace text in selection.

## Usage

``` r
rs_replace_in_selection(
  pattern,
  replacement,
  fixed = TRUE,
  keep_selected = TRUE,
  selection = c("all", "first", "last"),
  context = rs_get_context()
)

rs_replace_selection(
  replacement,
  keep_selected = TRUE,
  selection = c("all", "first", "last"),
  context = rs_get_context()
)

select_correct_range(old_text, new_text, old_range, id = NULL)
```

## Arguments

- pattern:

  (character)  
  A pattern of text to be replaced.

- replacement:

  (character)  
  The replacement text.

- fixed:

  (logical)  
  If `TRUE`, the pattern is a fixed expression. If `FALSE`, the pattern
  is a regular expression.

- keep_selected:

  (logical)  
  Flag indicating if the selection should be kept after add-in is
  applied.

- selection:

  (string)  
  String that indicates, which selection should be extracted.

- context:

  (class `document_context`)  
  Object with context of active RStudio document.

- old_text:

  (character) Original text.

- new_text:

  (character) Text after correction.

- old_range:

  `document_range` object. See
  [rstudioapi::document_range](https://rstudio.github.io/rstudioapi/reference/document_range.html).

- id:

  Document ID. See
  [`getActiveDocumentContext()`](https://rstudio.github.io/rstudioapi/reference/rstudio-editors.html).

## Details

`select_correct_range()` correctly adjust current selection.

## See also

[`base::gsub()`](https://rdrr.io/r/base/grep.html)
