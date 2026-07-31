# Enclose selection of text with indicated symbols.

Enclose selection of text with certain symbols which usually have
special meaning in R Markdown files.

## Usage

``` r
rs_enclose_selection_with(
  symbol = "",
  symbol_before = symbol,
  symbol_after = symbol,
  trim = FALSE,
  context = rs_get_context()
)
```

## Arguments

- symbol:

  (character) A sequence of symbols to add on both sides of selection.

- symbol_before:

  (character) A sequence of symbols to before the selection (overrides
  value of `symbol`).

- symbol_after:

  (character) A sequence of symbols to add after the selection
  (overrides value of `symbol`).

- trim:

  (logical) Flag if whitespace should be trimmed from both sides of the
  selection.

- context:

  (class `document_context`)  
  Object with context of active RStudio document.

## Details

If text is selected, the cursor is placed after the modified selection.
If no text is selected, the cursor is placed between the inserted
symbols.
