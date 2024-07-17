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
    ## 2     geneexpr            ASNS
    ## 3     geneexpr            BAP1
    ## 4     geneexpr           CHEK2
    ## 5     geneexpr           EIF4E
    ## 6     geneexpr          MAP2K1
    ## 7     geneexpr            PCNA
    ## 8     geneexpr           YWHAE
    ## 9     geneexpr           YWHAZ
    ## 10 proteinexpr        Bap1.c.4
    ## 11 proteinexpr             Bid
    ## 12 proteinexpr       Cyclin_E2
    ## 13 proteinexpr      P.Cadherin
    ## 14 proteinexpr            Chk1
    ## 15 proteinexpr      Chk1_pS345
    ## 16 proteinexpr            EGFR
    ## 17 proteinexpr     EGFR_pY1173
    ## 18 proteinexpr     HER3_pY1289
    ## 19 proteinexpr           MIG.6
    ## 20 proteinexpr           ETS.1
    ## 21 proteinexpr MEK1_pS217_S221
    ## 22 proteinexpr        p38_MAPK
    ## 23 proteinexpr    c.Met_pY1235
    ## 24 proteinexpr           N.Ras
    ## 25 proteinexpr            PCNA
    ## 26 proteinexpr     PEA15_pS116
    ## 27 proteinexpr PKC.delta_pS664
    ## 28 proteinexpr           Rad50
    ## 29 proteinexpr     C.Raf_pS338
    ## 30 proteinexpr          p70S6K
    ## 31 proteinexpr    p70S6K_pT389
    ## 32 proteinexpr           Smad4
    ## 33 proteinexpr     STAT3_pY705
    ## 34 proteinexpr  14.3.3_epsilon
    ## 35 methylation      cg20139214
    ## 36 methylation      cg18457775
    ## 37 methylation      cg24747396
    ## 38 methylation      cg01306510
    ## 39 methylation      cg02412050
    ## 40 methylation      cg25984124
    ## 41 methylation      cg07566050
    ## 42 methylation      cg02630105
    ## 43 methylation      cg20849549
    ## 44 methylation      cg25539131
    ## 45 methylation      cg07064406

For each layer, the variable selection results show the chosen
variables. In this example, we perform variable selection on the entire
study. However, users can opt to conduct variable selection on
individual layers if desired.

#### C) Training

We can now train our study using the subset of selected variables. Users
can choose to set up layer-specific learners, but for illustration, we
will use the same learner for all layers.

- Set up the same leaner parameters.

``` r
same_param <- ParamLrner$new(id = "ParamRanger",
                             param_list = list(probability = TRUE,
                                               mtry = 1),
                             hyperparam_list = list(num.trees = 1000))
```

- Set up learners for each layer. We will use a weighted sum for the
  meta-analysis.

``` r
lrner_ge <- Lrner$new(id = "ranger",
                      package = "ranger",
                      lrn_fct = "ranger",
                      param = same_param,
                      train_layer = tl_ge)
lrner_pr <- Lrner$new(id = "ranger",
                      package = "ranger",
                      lrn_fct = "ranger",
                      param = same_param,
                      train_layer = tl_pr)
lrner_me <- Lrner$new(id = "ranger",
                      package = "ranger",
                      lrn_fct = "ranger",
                      param = same_param,
                      train_layer = tl_me)
lrner_meta <- Lrner$new(id = "weighted",
                        lrn_fct = "weightedMeanLearner",
                        param = ParamLrner$new(id = "ParamWeighted",
                                               param_list = list(),
                                               hyperparam_list = list()),
                        train_layer = tl_meta)
```

- Train the study with the selected variables.

``` r
# Retrieve the target variable for resampling reasons. Resampling will be used by
# fuseMLR to generate meta data.
disease <- train_study$getTargetValues()$disease
trained_study <- train_study$train(resampling_method = "caret::createFolds",
                                   resampling_arg = list(y = disease,
                                                         k = 2),
                                   use_var_sel = TRUE)
# Let us now check the status of our study.
print(trained_study)
```

    ## TrainStudy      : train_study
    ## Status          : Trained
    ## Number of layers: 4
    ## Layers trained  : 4

``` r
# Let us check the status of a layer as well.
print(tl_ge)
```

    ## TrainLayer            : geneexpr
    ## Status                : Trained
    ## Nb. of objects stored : 4
    ## -----------------------
    ##               key     class
    ## 1        geneexpr TrainData
    ## 2 varsel_geneexpr    VarSel
    ## 3          ranger     Lrner
    ## 4        rangerMo     Model

- Retrieve the basic model of a specific layer.

``` r
model_ge <- tl_ge$getModel()
print(model_ge)
```

    ## Class           : Model
    ## 
    ## Learner info.   
    ## -----------------------
    ## Learner          : ranger
    ## TrainLayer       : geneexpr
    ## Package          : ranger
    ## Learn function   : ranger
    ## 
    ## Train data info.      
    ## -----------------------
    ## TrainData : geneexpr
    ## Layer     : geneexpr
    ## ind. id.  : IDS
    ## target    : disease
    ## n         : 23
    ## Missing   : 0
    ## p         : 10

#### C) Predicting

Now, we have created a training study, performed variable selection and
trained the study with the chosen variables. In this section, we create
and predict a new study.

- Create a new study.

``` r
new_study <- NewStudy$new(id = "new_study", ind_col = "IDS")
```

- Create new layers.

``` r
nl_ge <- NewLayer$new(id = "geneexpr", new_study = new_study)
nl_pr <- NewLayer$new(id = "proteinexpr", new_study = new_study)
nl_me <- NewLayer$new(id = "methylation", new_study = new_study)
```

- Instantiate and add new training data to new layers.

``` r
new_data_ge <- NewData$new(id = "geneexpr",
                                 new_layer = nl_ge,
                                 data_frame = entities$testing$geneexpr)
new_data_pr <- NewData$new(id = "proteinexpr",
                                    new_layer = nl_pr,
                                    data_frame = entities$testing$proteinexpr)
new_data_me <- NewData$new(id = "methylation",
                                    new_layer = nl_me,
                                    data_frame = entities$testing$methylation)
```

- Predict the new study.

``` r
new_predictions <- train_study$predict(new_study = new_study)
print(new_predictions)
```

    ## $predicted_study
    ## PredictStudy : new_study
    ## Nb. layers   : 4
    ## 
    ## $predicted_values
    ##          IDS  geneexpr proteinexpr methylation meta_layer
    ## 1   subject4 0.5819528   0.6423877   0.4556944  0.5548049
    ## 2   subject7 0.5957044   0.2418123   0.4356028  0.4174470
    ## 3   subject8 0.8260016   0.7800290   0.6811825  0.7563455
    ## 4  subject10 0.7634992   0.7719115   0.7171159  0.7489190
    ## 5  subject13 0.5961583   0.3214702   0.3154683  0.3991494
    ## 6  subject15 0.6096869   0.7909095   0.4298448  0.6028022
    ## 7  subject16 0.7128421   0.3172734   0.2370698  0.4023062
    ## 8  subject18 0.7296052   0.2315063   0.1759512  0.3556151
    ## 9  subject23 0.7137321   0.2179992   0.5798913  0.4979338
    ## 10 subject24 0.4262175   0.5892270   0.5801833  0.5384033
    ## 11 subject27 0.3516631   0.2446571   0.4184683  0.3409613
    ## 12 subject31 0.4353333   0.7683369   0.5736972  0.5984620
    ## 13 subject32 0.5478357   0.6644591   0.6187845  0.6133989
    ## 14 subject35 0.3448282   0.6636111   0.6363849  0.5606428
    ## 15 subject36 0.5197944   0.1920357   0.4225405  0.3738314
    ## 16 subject50 0.8559476   0.4433349   0.7268147  0.6696837
    ## 17 subject54 0.5804028   0.6493194   0.8653012  0.7102443
    ## 18 subject55 0.6496242   0.2483901   0.5191702  0.4666664
    ## 19 subject59 0.4640766   0.2734353   0.3363806  0.3525088
    ## 20 subject62 0.2497897   0.2903052   0.2857087  0.2767925
    ## 21 subject63 0.3049687   0.6436583   0.8194611  0.6110195
    ## 22 subject66 0.6930524   0.6277417   0.8553694  0.7320909
    ## 23 subject70 0.2264119   0.2864357   0.2686020  0.2622834

© 2024 Institute of Medical Biometry and Statistics (IMBS). All rights
reserved.
