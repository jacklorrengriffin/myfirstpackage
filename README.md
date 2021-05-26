
# myfirstpackage

<!-- badges: start -->
[![R-CMD-check](https://github.com/jacklorrengriffin/myfirstpackage/workflows/R-CMD-check/badge.svg)](https://github.com/jacklorrengriffin/myfirstproject/actions)
[![codecov](https://codecov.io/gh/jacklorrengriffin/myfirstpackage/branch/master/graph/badge.svg?token=1HQ3DJP0VV)](https://codecov.io/gh/jacklorrengriffin/myfirstpackage)
<!-- badges: end -->

The goal of myfirstpackage is to ...

## Installation

You can install the package through github using

``` r
devtools::install_github("jacklorrengriffin/myfirstpackage")
```
To view the vignette:

```{r}
devtools::install_github("jacklorrengriffin/myfirstpackage", build_vignette = TRUE, build_opts = c())
library(Demo)
# Use this to view the vignette in the myfirstpackage HTML help
help(package = "myfirstpackage", help_type = "html")
# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "myfirstpackage")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(myfirstpackage)
## basic example code
```

