
<!-- README.md is generated from README.Rmd. Please edit that file -->

# predictp

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/predictp)](https://CRAN.R-project.org/package=predictp)
<!-- badges: end -->

The goal of predictp is to predict harbour porpoise detection
probability from an aerial camera survey data, using a model for the
probability of seeing a porpoise, given its depth, and a model for the
proportion of time porpoise spend at each depth.

## Installation

You can install the development version of predictp like so:

``` r
require(devtools)
install_github("david-borchers/predictp", build_vignettes = TRUE)
tools::buildVignettes(dir = ".", tangle=TRUE)
dir.create("inst/doc")
file.copy(dir("vignettes", full.names=TRUE), "inst/doc", overwrite=TRUE)
```

## Example

This is a basic example which shows you how to predict detection
probability using the depth detectoin probability model `vismodel`, the
depth distribution model `tagmodel`, the visibility covariate data frame
`eg_viscov` and the depth distribution covariate data `eg_tagcov`:

``` r
library(predictp)
data("eg_viscov")
data("eg_tagcov")
data("vismodel")
data("tagmodel")
p = psee(eg_viscov, eg_tagcov, vismodel, tagmodel)
```
