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
[![codecov](https://codecov.io/github/imbs-hl/fuseMLR/graph/badge.svg?token=SZU7NGK8G8)](https://codecov.io/github/imbs-hl/fuseMLR)
[![Lifecycle:
Maturing](https://img.shields.io/badge/lifecycle-Maturing-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#Maturing)
[![CRAN
downloads](http://www.r-pkg.org/badges/version/fuseMLR)](http://cranlogs.r-pkg.org/badges/grand-total/fuseMLR)
[![Stack
Overflow](https://img.shields.io/badge/stackoverflow-questions-orange.svg)](https://stackoverflow.com/questions/tagged/fuseMLR)
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
Refer to our [cheat
sheet](https://github.com/imbs-hl/fuseMLR/blob/master/README_files/figure-gfm/fusemlrcheatsheet.pdf)
for a quick overview of classes and functionalities.

### Installation

Install the development version from GitHub with

``` r
devtools::install_github("imbs-hl/fuseMLR")
```

### Package overview

The following figure illustrates the general architecture of `fuseMLR`:

<img src="README_files/figure-gfm/structure.png" width="70%" height="100%" />

The general architecture of `fuseMLR` includes the collection classes
`Training`, `TrainLayer`, and `TrainMetaLayer`. `TrainLayer` and
`TrainMetaLayer` are stored within a `Training` instance, while
`TrainData`, `Lrnr`, and `VarSel` (for variable selection) are stored
within a `TrainLayer` or `MetaTrainLayer` instance. An `Training` object
can be used to automatically conduct layer-specific variable selection
and train layer-specfic and meta models.

### Usage example

The following example is based on simulated data available in `fuseMLR`.
Data have been simulated using the R package `InterSIM`, version 2.2.0.

``` r
library(fuseMLR)
library(UpSetR)
library(ranger)
library(DescTools)
```

#### A) Simulated data.

Two types of data were simulated: training and testing datasets. Each
consists of four `data.frame`s—gene expression, protein expression,
methylation data, and target variable observations. Individuals are
organized in rows, variables in columns, with an additional column for
individual IDs. In total, $70$ individuals with $50$ individuals pro
layer have been simulated for training, and $23$ ($20$ per layer) for
testing. Individuals do not necessarily overlapped. Effects have been
introduced for gene expression and methylation by shifting the means by
$0.5$ to create case-control study with $50$% prevalence. Individuals do
not necessarily overlap. Effects were introduced in gene expression and
methylation by shifting the means by 0.5 to create a case-control study.
For illustration, the number of variables was kept smaller than what is
typically expected in reality. The data simulation code is available
[here](https://github.com/imbs-hl/fuseMLR/blob/master/test_code/build_data.R).

``` r
data("entities")
# This is a list containing two lists of data: training and test.
# Each sublist contains three entities.
str(object = entities, max.level = 2L)
```

    ## List of 2
    ##  $ training:List of 4
    ##   ..$ geneexpr   :'data.frame':  50 obs. of  132 variables:
    ##   ..$ proteinexpr:'data.frame':  50 obs. of  161 variables:
    ##   ..$ methylation:'data.frame':  50 obs. of  368 variables:
    ##   ..$ target     :'data.frame':  70 obs. of  2 variables:
    ##  $ testing :List of 4
    ##   ..$ geneexpr   :'data.frame':  20 obs. of  132 variables:
    ##   ..$ proteinexpr:'data.frame':  20 obs. of  161 variables:
    ##   ..$ methylation:'data.frame':  20 obs. of  368 variables:
    ##   ..$ target     :'data.frame':  30 obs. of  2 variables:

Variable selection, training and prediction are the main functionalities
of `fuseMLR`. We can perform variable selection, train and fuse models
for training studies, and predict new studies.

#### B) Instantiate training resources

We need to set up training resources.

``` r
training <- Training$new(id = "training",
                         ind_col = "IDS",
                         target = "disease",
                         target_df = entities$training$target)
# See also training$summary()
print(training)
```

    ## Training        : training
    ## Status          : Not trained
    ## Number of layers: 0
    ## Layers trained  : 0
    ## n               : 70

- Prepare new training layers: Training layers are components of a study
  and represent the second component of a `fuseMLR` object.

``` r
tl_ge <- TrainLayer$new(id = "geneexpr", training = training)
tl_pr <- TrainLayer$new(id = "proteinexpr", training = training)
tl_me <- TrainLayer$new(id = "methylation", training = training)
# We also prepare the meta layer for the meta analysis.
tl_meta <- TrainMetaLayer$new(id = "meta_layer", training = training)
```

- Add training data (entities) to layers: Exclude the meta layer, as it
  is modified internally after the training phase.

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
# An overview of the training resources
print(training)
```

    ## Training        : training
    ## Status          : Not trained
    ## Number of layers: 4
    ## Layers trained  : 0
    ## n               : 70

- An upset plot of the training data: Visualize patient overlap across
  layers.

``` r
training$upset(order.by = "freq")
```

![](README_files/figure-gfm/upsetplot-1.png)<!-- -->

#### C) Variable selection

We need to set up variable selection methods to our training resources.
Note that this can be the same method or different layer-specific
methods. For simplicity, we will set up the same method on all layers.

- Instantiate the variable selection method and assign training layers.

``` r
varsel_ge <- VarSel$new(id = "varsel_geneexpr",
                        package = "Boruta",
                        varsel_fct = "Boruta",
                        varsel_param = list(num.trees = 1000L,
                                            mtry = 3L,
                                            probability = TRUE),
                        train_layer = tl_ge)

varsel_pr <- VarSel$new(id = "varsel_proteinexpr",
                        package = "Boruta",
                        varsel_fct = "Boruta",
                        varsel_param = list(num.trees = 1000L,
                                            mtry = 3L,
                                            probability = TRUE),
                        train_layer = tl_pr)
varsel_me <- VarSel$new(id = "varsel_methylation",
                        package = "Boruta",
                        varsel_fct = "Boruta",
                        varsel_param = list(num.trees = 1000L,
                                            mtry = 3L,
                                            probability = TRUE),
                        train_layer = tl_me)
```

- Perform variable selection on our training resources

``` r
set.seed(5467)
var_sel_res <- training$varSelection()
```

    ## Variable selection on layer geneexpr started.

    ## Variable selection on layer geneexpr done.

    ## Variable selection on layer proteinexpr started.

    ## Variable selection on layer proteinexpr done.

    ## Variable selection on layer methylation started.

    ## Variable selection on layer methylation done.

``` r
print(var_sel_res)
```

    ##          Layer   variable
    ## 1     geneexpr     ACVRL1
    ## 2     geneexpr     AKT1S1
    ## 3     geneexpr        BAX
    ## 4     geneexpr     BCL2L1
    ## 5     geneexpr     CDKN1A
    ## 6     geneexpr     CTNNB1
    ## 7     geneexpr       EEF2
    ## 8     geneexpr      EEF2K
    ## 9     geneexpr   EIF4EBP1
    ## 10    geneexpr     MAPK14
    ## 11    geneexpr      NFKB1
    ## 12    geneexpr      PREX1
    ## 13    geneexpr      PRKCD
    ## 14    geneexpr        PXN
    ## 15    geneexpr       SHC1
    ## 16    geneexpr        SRC
    ## 17    geneexpr      XRCC5
    ## 18    geneexpr       YAP1
    ## 19 proteinexpr Caveolin.1
    ## 20 proteinexpr     Rab.25
    ## 21 methylation cg09637363
    ## 22 methylation cg25060573
    ## 23 methylation cg23989635
    ## 24 methylation cg22679003
    ## 25 methylation cg01663570
    ## 26 methylation cg09186685
    ## 27 methylation cg20253551
    ## 28 methylation cg03386722
    ## 29 methylation cg00241355
    ## 30 methylation cg26187237
    ## 31 methylation cg24991452
    ## 32 methylation cg20042228
    ## 33 methylation cg23641145
    ## 34 methylation cg01228636
    ## 35 methylation cg17489897
    ## 36 methylation cg21573601
    ## 37 methylation cg00059930
    ## 38 methylation cg19427472
    ## 39 methylation cg16925486
    ## 40 methylation cg00239419
    ## 41 methylation cg23323671
    ## 42 methylation cg07160163
    ## 43 methylation cg12507125

For each layer, the variable selection results show the chosen
variables. In this example, we perform variable selection. Users can opt
to conduct variable selection on individual layers if desired.

#### D) Training

We can now train our models using the subset of selected variables.
Users can choose to set up layer-specific learners, but for
illustration, we will use the same learner for all layers.

- Set up learners for each layer. We will use a weighted sum,
  implemented internally by `fuseMLR`, for the meta-analysis.

``` r
lrner_ge <- Lrner$new(id = "ranger",
                      package = "ranger",
                      lrn_fct = "ranger",
                      param_train_list = list(probability = TRUE,
                                               mtry = 1L),
                      train_layer = tl_ge)
lrner_pr <- Lrner$new(id = "ranger",
                      package = "ranger",
                      lrn_fct = "ranger",
                      param_train_list = list(probability = TRUE,
                                               mtry = 1L),
                      train_layer = tl_pr)
lrner_me <- Lrner$new(id = "ranger",
                      package = "ranger",
                      lrn_fct = "ranger",
                      param_train_list = list(probability = TRUE,
                                               mtry = 1L),
                      train_layer = tl_me)
lrner_meta <- Lrner$new(id = "weighted",
                        lrn_fct = "weightedMeanLearner",
                        param_train_list = list(),
                        na_rm = FALSE,
                        train_layer = tl_meta)
```

- Train the models with the selected variables.

``` r
set.seed(5462)
# Retrieve the target variable for resampling reasons. Resampling will be used by
# fuseMLR to generate meta data.
disease <- training$getTargetValues()$disease
trained <- training$train(resampling_method = "caret::createFolds",
                          resampling_arg = list(y = disease,
                                                k = 10L),
                          use_var_sel = TRUE,
                          verbose = FALSE)
# Let us now check the status of our training resources.
print(trained)
```

    ## Training        : training
    ## Status          : Trained
    ## Number of layers: 4
    ## Layers trained  : 4
    ## n               : 70

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

``` r
## On the meta model
tmp_model <- tl_meta$getModel()
print(tmp_model$getBaseModel())
```

    ##    geneexpr proteinexpr methylation 
    ##   0.2326579   0.4241035   0.3432386 
    ## attr(,"class")
    ## [1] "weightedMeanLearner"

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
    ## n         : 50
    ## Missing   : 0
    ## p         : 19

#### E) Predicting

Now, we have created training resources, performed variable selection
and trained the models with the chosen variables. In this section, we
create testing resources and make predictions for new data.

- Create the testing object.

``` r
testing <- Testing$new(id = "testing", ind_col = "IDS")
```

- Create new layers.

``` r
nl_ge <- TestLayer$new(id = "geneexpr", testing = testing)
nl_pr <- TestLayer$new(id = "proteinexpr", testing = testing)
nl_me <- TestLayer$new(id = "methylation", testing = testing)
```

- Instantiate and add new training data to new layers.

``` r
new_data_ge <- TestData$new(id = "geneexpr",
                            new_layer = nl_ge,
                            data_frame = entities$testing$geneexpr)
new_data_pr <- TestData$new(id = "proteinexpr",
                            new_layer = nl_pr,
                            data_frame = entities$testing$proteinexpr)
new_data_me <- TestData$new(id = "methylation",
                            new_layer = nl_me,
                            data_frame = entities$testing$methylation)
```

- An upset plot of the training data: Visualize patient overlap across
  layers.

``` r
testing$upset(order.by = "freq")
```

![](README_files/figure-gfm/upsetplot_new-1.png)<!-- -->

- Predict the testing object.

``` r
predictions <- training$predict(testing = testing)
print(predictions)
```

    ## $predicting
    ## Predicting   : testing
    ## Nb. layers   : 4
    ## 
    ## $predicted_values
    ##          IDS  geneexpr proteinexpr methylation meta_layer
    ## 1  patient23 0.3955167   0.6035587  0.17541746  0.4082015
    ## 2  patient77 0.4194508   0.4910333  0.12029762  0.3471283
    ## 3  patient62 0.7698508   0.9696040          NA  0.8988413
    ## 4  patient43 0.3206127          NA          NA  0.3206127
    ## 5   patient8 0.7545587   0.8471222  0.83950000  0.8229704
    ## 6  patient74 0.5489651   0.6835690  0.53626667  0.6016925
    ## 7  patient29 0.3272770   0.4641143  0.28055873  0.3692746
    ## 8  patient17 0.4088706   0.3569238          NA  0.3753260
    ## 9  patient25 0.2823317   0.4480825  0.09887937  0.2896593
    ## 10 patient54 0.8051635          NA  0.84849444  0.8309891
    ## 11 patient60 0.7333056   0.8435198  0.80658413  0.8051999
    ## 12 patient44 0.3889190          NA          NA  0.3889190
    ## 13  patient1 0.8166127   0.9415270          NA  0.8972761
    ## 14 patient76 0.6997905          NA  0.62434603  0.6548250
    ## 15 patient16 0.7076770          NA  0.69410000  0.6995850
    ## 16 patient27 0.3591865          NA  0.22121032  0.2769517
    ## 17 patient58 0.5753381   0.7549960  0.76922857  0.7180823
    ## 18 patient52 0.4747833   0.1256032          NA  0.2493004
    ## 19 patient10 0.2418214          NA          NA  0.2418214
    ## 20 patient72 0.7309365   0.9540413  0.61715873  0.7865031
    ## 21 patient39        NA   0.0834881          NA  0.0834881
    ## 25 patient46        NA   0.2154611  0.53634524  0.3589953
    ## 26 patient97        NA   0.7014659  0.86992143  0.7768175
    ## 27 patient31        NA   0.2656349          NA  0.2656349
    ## 31 patient87        NA   0.2897254  0.25276032  0.2731906
    ## 33 patient59        NA   0.1475627  0.39242143  0.2570901
    ## 34  patient2        NA   0.5379889  0.81212460  0.6606121
    ## 53 patient85        NA          NA  0.15531746  0.1553175
    ## 60  patient3        NA          NA  0.58667143  0.5866714

- Prediction performances for layer-specific available patients, and all
  patients on the meta layer.

``` r
pred_values <- predictions$predicted_values
actual_pred <- merge(x = pred_values,
                     y = entities$testing$target,
                     by = "IDS",
                     all.y = TRUE)
x <- as.integer(actual_pred$disease == 2L)

# On all patients
perf_estimated <- sapply(X = actual_pred[ , 2L:5L], FUN = function (my_pred) {
  bs <- BrierScore(x = x[complete.cases(my_pred)],
                   pred = my_pred[complete.cases(my_pred)], na.rm = TRUE)
  return(bs)
})
print(perf_estimated)
```

    ##    geneexpr proteinexpr methylation  meta_layer 
    ##  0.11205017  0.16567829  0.07991965  0.12184525

- Prediction performances for overlapping individuals.

``` r
# On overlapping patients
perf_overlapping <- sapply(X = actual_pred[complete.cases(actual_pred),
                                           2L:5L],
                           FUN = function (my_pred) {
                             bs <- BrierScore(x = x[complete.cases(actual_pred)], pred = my_pred)
                             return(bs)
                           })
print(perf_overlapping)
```

    ##    geneexpr proteinexpr methylation  meta_layer 
    ##  0.12296962  0.13685581  0.06797495  0.09559852

Note that our example is based on simulated data for usage illustration;
only one run is not enough to appreciate the performances of our models.

© 2024 Institute of Medical Biometry and Statistics (IMBS). All rights
reserved.
