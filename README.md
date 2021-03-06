
<!-- 

TO DO: 

1. Write function to check if there is a space before and after the selection
(for %>% and similar operators

-->

<!-- README.md is generated from README.Rmd. Please edit that file -->

[![MIT
licensed](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/addin.tools)](https://cran.r-project.org/package=addin.tools)
[![GitHub
version](https://img.shields.io/badge/GitHub-0.0.5-brightgreen.svg)](https://github.com/GegznaV/addin.tools)
[![Travis-CI Build
Status](https://travis-ci.org/GegznaV/addin.tools.svg?branch=master)](https://travis-ci.org/GegznaV/addin.tools)
[![Updated-on](https://img.shields.io/badge/Updated%20on-2019--07--28-yellowgreen.svg)](/commits/master)

<!-- [![Rdoc](http://www.rdocumentation.org/badges/version/addin.tools)](http://www.rdocumentation.org/packages/addin.tools) -->

<!--

-->

-----

<!-- <img src="https://gegznav.github.io/addin.tools/logo.png" align="right" width="15%" height="15%"/> -->

<img src="https://raw.githubusercontent.com/GegznaV/addin.tools/master/docs/logo.png" align="right" width="15%" height="15%"/>

# R package **addin.tools**

Package `addin.tools` contains various functions that help to construct
*RStudio* addins. The functions are wrappers arround package
`rstudioapi`. They are used as the core functions for packages
`addins.rmd`, `addins.rs` and other.

## Install package

<!-- Install released version from CRAN: -->

<!-- ```{r Install package from CRAN, eval=FALSE} -->

<!-- install.packages("addin.tools") -->

<!-- ``` -->

Install development version from GitHub:

``` r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("GegznaV/addin.tools")
```

-----

More information at <https://gegznav.github.io/addin.tools/>
