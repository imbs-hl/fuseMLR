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

`fuseMLR` object oriented based on the `R6` package version 2.5.1.

### Installation

Install the development version from GitHub with

``` r
devtools::install_github("imbs-hl/fuseMLR")
```

### Usage example

The following example is based on simulated data available in `fuseMLR`.
Data have been simulated using the R package `InterSIM`, version 2.2.0.

- Let us inspect our simulated data.

``` r
library(fuseMLR)
data("entities")
# This is a list containing two lists of data: training and test.
# Each sublist contains three entities.
str(object = entities, max.level = 2)
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

Variable selection, training and prediction are the main functionalities
of `fuseMLR`. As variable selection and training are performed for a
training study, predictions are made for a new study.

#### A) Preparation of a training study

We need to set up a study, its layers and the training data entities.

- Instantiate a training study: A study is the fundamental component of
  a `fuseMLR` object.

``` r
train_study <- TrainStudy$new(id = "train_study",
                              ind_col = "IDS",
                              target = "disease")
# See also train_study$summary()
print(train_study)
```

    ## TrainStudy      : train_study
    ## Status          : Not trained
    ## Number of layers: 0
    ## Layers trained  : 0

- Prepare new training layers: Training layers are components of a study
  and represent the second stage of the study.

``` r
tl_ge <- TrainLayer$new(id = "geneexpr", train_study = train_study)
tl_pr <- TrainLayer$new(id = "proteinexpr", train_study = train_study)
tl_me <- TrainLayer$new(id = "methylation", train_study = train_study)
# We also prepare the meta layer for the meta analysis.
tl_meta <- TrainMetaLayer$new(id = "meta_layer", train_study = train_study)
```

- Add training data (entities) to layers: Exclude the meta layer, as it
  is modifiable internally after the training phase.

``` r
train_data_ge <- TrainData$new(id = "geneexpr",
                               train_layer = tl_ge,
                               data_frame = entities$training$geneexpr)
train_data_pr <- TrainData$new(id = "proteinexpr",
                               train_layer = tl_pr,
                               data_frame = entities$training$proteinexpr)
train_data_me <- TrainData$new(id = "methylation",
                               train_layer = tl_me,
                               data_frame = entities$training$methylation)
# An overview of the gene expression training data
print(train_data_ge)
```

    ## TrainData : geneexpr
    ## Layer     : geneexpr
    ## ind. id.  : IDS
    ## target    : disease
    ## n         : 50
    ## Missing   : 0
    ## p         : 133

``` r
# An overview of the gene expression training layer
print(tl_ge)
```

    ## TrainLayer            : geneexpr
    ## Status                : Not trained
    ## Nb. of objects stored : 1
    ## -----------------------
    ##        key     class
    ## 1 geneexpr TrainData

``` r
# An overview of the training study
print(train_study)
```

    ## TrainStudy      : train_study
    ## Status          : Not trained
    ## Number of layers: 4
    ## Layers trained  : 0

- An upset plot of the training study: Visualize patient overlap across
  layers.

``` r
train_study$upset(order.by = "freq")
```

![](README_files/figure-gfm/upsetplot-1.png)<!-- -->

#### B) Variable selection

We need to set up variable selection methods to our training study. Note
that this can be the same method or different layer-specific methods.
For simplicity, we will set up the same method on all layers.

- Preparation parameters of the variable selection method.

``` r
same_param_varsel <- ParamVarSel$new(id = "ParamVarSel",
                                     param_list = list(num.trees = 1000, mtry = 3))
print(same_param_varsel)
```

    ## Class: ParamVarSel
    ## id   : ParamVarSel
    ## Parameter combination
    ## $num.trees
    ## [1] 1000
    ## 
    ## $mtry
    ## [1] 3

- Instantiate the variable selection method and assign training layers.

``` r
varsel_ge <- VarSel$new(id = "varsel_geneexpr",
                              package = "Boruta",
                              varsel_fct = "Boruta",
                              param = same_param_varsel,
                              train_layer = tl_ge)

varsel_pr <- VarSel$new(id = "varsel_geneexpr",
                                 package = "Boruta",
                                 varsel_fct = "Boruta",
                                 param = same_param_varsel,
                                 train_layer = tl_pr)

varsel_me <- VarSel$new(id = "varsel_geneexpr",
                                 package = "Boruta",
                                 varsel_fct = "Boruta",
                                 param = same_param_varsel,
                                 train_layer = tl_me)
```

- Perform variable selection on our training study.

``` r
var_sel_res <- train_study$varSelection()
print(var_sel_res)
```

    ##          Layer        variable
    ## 1     geneexpr           ACACA
    ## 2     geneexpr            BAP1
    ## 3     geneexpr            CDH3
    ## 4     geneexpr           CHEK2
    ## 5     geneexpr           EIF4E
    ## 6     geneexpr          MAP2K1
    ## 7     geneexpr          MAPK14
    ## 8     geneexpr            PCNA
    ## 9     geneexpr           YWHAE
    ## 10    geneexpr           YWHAZ
    ## 11 proteinexpr        Bap1.c.4
    ## 12 proteinexpr             Bid
    ## 13 proteinexpr       Cyclin_E2
    ## 14 proteinexpr      P.Cadherin
    ## 15 proteinexpr            Chk1
    ## 16 proteinexpr      Chk1_pS345
    ## 17 proteinexpr            EGFR
    ## 18 proteinexpr     EGFR_pY1173
    ## 19 proteinexpr     HER3_pY1289
    ## 20 proteinexpr           MIG.6
    ## 21 proteinexpr           ETS.1
    ## 22 proteinexpr MEK1_pS217_S221
    ## 23 proteinexpr        p38_MAPK
    ## 24 proteinexpr    c.Met_pY1235
    ## 25 proteinexpr           N.Ras
    ## 26 proteinexpr            PCNA
    ## 27 proteinexpr     PEA15_pS116
    ## 28 proteinexpr PKC.delta_pS664
    ## 29 proteinexpr           Rad50
    ## 30 proteinexpr     C.Raf_pS338
    ## 31 proteinexpr          p70S6K
    ## 32 proteinexpr    p70S6K_pT389
    ## 33 proteinexpr           Smad4
    ## 34 proteinexpr     STAT3_pY705
    ## 35 proteinexpr  14.3.3_epsilon
    ## 36 methylation      cg20139214
    ## 37 methylation      cg18457775
    ## 38 methylation      cg01306510
    ## 39 methylation      cg02412050
    ## 40 methylation      cg07566050
    ## 41 methylation      cg02630105
    ## 42 methylation      cg20849549
    ## 43 methylation      cg00547829
    ## 44 methylation      cg25539131
    ## 45 methylation      cg07064406

For each layer the variable selection results show which variable have
been selected.
