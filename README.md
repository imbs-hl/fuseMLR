---
title: "fuseMLR"
author: Cesaire J. K. Fouodo
output: 
  md_document:
    variant: gfm
    preserve_yaml: true
---

<!-- badges: start -->

[![R-CMD-check](https://github.com/imbs-hl/fuseMLR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/imbs-hl/fuseMLR/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
downloads](http://www.r-pkg.org/badges/version/fuseMLR)](http://cranlogs.r-pkg.org/badges/grand-total/fuseMLR)
[![Stack
Overflow](https://img.shields.io/badge/stackoverflow-questions-orange.svg)](https://stackoverflow.com/questions/tagged/fuseMLR)
<!-- badges: end -->

### fuseMLR

Cesaire J. K. Fouodo

### Introduction

Recent technological advances have enabled the simultaneous targeting of
multiple pathways to enhance therapies for complex diseases. This often
results in the collection of numerous data entities across various
layers of patient groups, posing a challenge for integrating all data
into a single analysis. Ideally, patient data will overlap across
layers, allowing for early or intermediate integrative techniques.
However, these techniques are challenging when patient data does not
overlap well. Additionally, the internal structure of each data entity
may necessitate specific statistical methods rather than applying the
same method across all layers. Late integration modeling addresses this
by analyzing each data entity separately to obtain layer-specific
results, which are then integrated using meta-analysis. Currently, no R
package offers this flexibility.

We introduce the fuseMLR package for late integration modeling in R.
This package allows users to define studies with multiple layers, data
entities, and layer-specific machine learning methods. FuseMLR is
user-friendly, enabling the training of different models across layers
and automatically conducting meta-analysis once layer-specific training
is completed. Additionally, fuseMLR allows for variable selection at the
layer level and makes predictions for new data entities.

### Installation

Install the development version from GitHub with

``` r
devtools::install_github("imbs-hl/fuseMLR")
```

### Usage example

The following example is based on simulated data available in fuseMLR.
Data have been simulated using the R package InterSIM, version 2.2.0.

- Let us examine our simulated data.

``` r
data("entities")
# This is a list containing two lists of data: training and test.
# Each sublist contains three entities.
str(object = entities,max.level = 2)
```

    ## List of 2
    ##  $ training:List of 3
    ##   ..$ geneexpr   :'data.frame':  50 obs. of  133 variables:
    ##   ..$ proteinexpr:'data.frame':  50 obs. of  162 variables:
    ##   ..$ methylation:'data.frame':  50 obs. of  369 variables:
    ##  $ testing :List of 3
    ##   ..$ geneexpr   :'data.frame':  23 obs. of  133 variables:
    ##   ..$ proteinexpr:'data.frame':  23 obs. of  162 variables:
    ##   ..$ methylation:'data.frame':  23 obs. of  369 variables:
