# Repeat sequence of symbols.

Repeat sequence of symbols.

## Usage

``` r
repeat_symbol(text, length.out)

repeat_symbol_2(text = " ", times)
```

## Arguments

- text:

  (character) The symbol (or sequence of symbols) to be repeated until
  desired length is achieved.

- length.out:

  (integer) The total length (in characters) of the sequence.

- times:

  (integer) Times to repeat the sequence.

## Value

Sting of defined length.

## Examples

``` r

repeat_symbol(".", 10)
#> [1] ".........."

repeat_symbol("..+", 10)
#> [1] "..+..+..+."
```
