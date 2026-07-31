# R package **addin.tools**

------------------------------------------------------------------------

Package `addin.tools` contains various functions that help to construct
*RStudio* addins. The functions are wrappers around package
`rstudioapi`. They are used as the core functions for packages
`addins.qmd`, `addins.rs` and other packages.

## Install package

Install development version from GitHub:

``` r

if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("GegznaV/addin.tools")
```

------------------------------------------------------------------------

More information at <https://gegznav.github.io/addin.tools/>

Package architecture and helper-family notes are documented in the
vignette [Package Architecture and Helper
Families](https://gegznav.github.io/addin.tools/articles/helper-families.md).
