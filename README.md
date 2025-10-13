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
[![codecov](https://codecov.io/github/imbs-hl/fuseMLR/graph/badge.svg?token=SZU7NGK8G8)](https://app.codecov.io/github/imbs-hl/fuseMLR/)
[![Lifecycle:
Stable](https://img.shields.io/badge/lifecycle-Stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#Stable)
[![CRAN
Status](https://img.shields.io/badge/CRAN-fuseMLR-blue)](https://cran.r-project.org/package=fuseMLR)

<!-- badges: end -->

### fuseMLR

Cesaire J. K. Fouodo

### Introduction

Recent technological advances have enabled the simultaneous collection
of multi-omics data, i.e., different types or modalities of molecular
data across various organ tissues of patients. For integrative
predictive modeling, the analysis of such data is particularly
challenging. Ideally, data from the different modalities are measured in
the same individuals, allowing for early or intermediate integrative
techniques. However, they are often not applicable when patient data
only partially overlap, which requires either reducing the datasets or
imputing missing values. Additionally, the characteristics of each data
modality may necessitate specific statistical methods rather than
applying the same method across all modalities. Late integration
modeling approaches analyze each data modality separately to obtain
modality-specific predictions. These predictions are then integrated to
train aggregative models like Lasso, random forests, or compute the
weighted mean of modality-specific predictions.

We introduce the R package fuseMLR for late integration prediction
modeling. This comprehensive package enables users to define a training
process with multiple data modalities and modality-specific machine
learning methods. The package is user-friendly, facilitates variable
selection and training of different models across modalities, and
automatically performs aggregation once modality-specific training is
completed. We simulated multi-omics data to illustrate the usage of our
new package for conducting late-stage multi-omics integrative modeling.

`fuseMLR` is an object-oriented package based on `R6` version 2.5.1.
Refer to the vignette (section *Usage* below) for a quick overview of
how to use the package.

The following figure illustrates the general architecture of `fuseMLR`:

<img src="README_files/figure-gfm/structure.png" width="70%" height="100%" />

The general architecture of `fuseMLR` includes the collection classes
`Training`, `TrainLayer`, and `TrainMetaLayer`. `TrainLayer` and
`TrainMetaLayer` are stored within a `Training` instance, while
`TrainData`, `Lrner`, and `VarSel` (for variable selection) are stored
within a `TrainLayer` or `MetaTrainLayer` instance. An `Training` object
can be used to automatically conduct layer-specific variable selection
and train layer-specfic learner and the meta-learner. Analogously, a
`Testing` can be set up and predicted after the training.

### Installation

Install the release version (v0.0.1) from CRAN with

``` r
install.packages("fuseMLR")
```

Install the development version from GitHub with

``` r
devtools::install_github("imbs-hl/fuseMLR")
```

### Usage

Refer to our vignette to understand [how fuseMLR
works](https://cran.r-project.org/web/packages/fuseMLR/vignettes/fuseMLR.html).

© 2024 Institute of Medical Biometry and Statistics (IMBS). All rights
reserved.

### Reference

Fouodo, C. J. K., Bleskina, M. & Szymczak, S. fuseMLR: an R package for
integrative prediction modeling of multi-omics data. BMC Bioinformatics
26, 221 (2025). <https://doi.org/10.1186/s12859-025-06248-4>
