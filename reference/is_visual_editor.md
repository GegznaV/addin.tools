# Check if Visual Editor (VE) is used

Check if RStudio addins should treat the current selection of active
document as being in RStudio Visual Editor mode (RS VE) or not.

`is_rmd_visual_mode()` was renamed to `is_visual_editor()`. Keep this
alias for compatibility with downstream addins.

## Usage

``` r
is_visual_editor()

is_rmd_visual_mode()
```

## Value

Logical:

- `TRUE` if an active document is in Visual Editor mode and current
  cursor position or selection is outside a code chunk.

- `FALSE` if an active document is: a. not in the Visual Editor mode; b.
  in Visual Editor mode but current cursor position or selection is
  inside a code chunk.

## Details

The document
[context](https://rstudio.github.io/rstudioapi/reference/rstudio-editors.html)
from RS VE is different than regular RStudio document context.
