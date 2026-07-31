# Insert a sequence of symbols.

Insert a sequence of symbols.

## Usage

``` r
rs_insert_symbol_seq(
  symbol,
  start_column = 1,
  end_column = 80,
  context = rs_get_context()
)
```

## Arguments

- symbol:

  (character) A sequence of symbols to be repeated

- start_column:

  (integer) Column position where the sequence begins.

- end_column:

  (integer) Column position where the sequence stops.

- context:

  (class `document_context`)  
  Object with context of active RStudio document.
