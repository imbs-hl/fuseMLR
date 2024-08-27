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

The general architecture of `fuseMLR` includes the storage classes
`Training`, `Layer`, and `MetaLayer`. `Layer` and `MetaLayer` are stored
within a `Training` instance, while `Data`, `Learner`, and `Varselect`
(for variable selection) are stored within a `Layer` instance. These
components can be used to automatically build and store the `Model` or
`MetaModel`.

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
    ## Number of layers: 1
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
    ## Number of layers: 5
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

- Preparation parameters of the variable selection method.

``` r
same_param_varsel <- ParamVarSel$new(id = "ParamVarSel",
                                     param_list = list(num.trees = 1000L,
                                                       mtry = 3L,
                                                       probability = TRUE))
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
    ## 
    ## $probability
    ## [1] TRUE

- Instantiate the variable selection method and assign training layers.

``` r
varsel_ge <- VarSel$new(id = "varsel_geneexpr",
                        package = "Boruta",
                        varsel_fct = "Boruta",
                        param = same_param_varsel,
                        train_layer = tl_ge)

varsel_pr <- VarSel$new(id = "varsel_proteinexpr",
                        package = "Boruta",
                        varsel_fct = "Boruta",
                        param = same_param_varsel,
                        train_layer = tl_pr)
varsel_me <- VarSel$new(id = "varsel_methylation",
                        package = "Boruta",
                        varsel_fct = "Boruta",
                        param = same_param_varsel,
                        train_layer = tl_me)
```

- Perform variable selection on our training resources

``` r
set.seed(5467)
var_sel_res <- training$varSelection()
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

- Set up the same leaner parameters.

``` r
same_param <- ParamLrner$new(id = "ParamRanger",
                             param_list = list(probability = TRUE,
                                               mtry = 1L),
                             hyperparam_list = list(num.trees = 1000L))
```

- Set up learners for each layer. We will use a weighted sum,
  implemented internally by `fuseMLR`, for the meta-analysis.

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
                                   use_var_sel = TRUE)
# Let us now check the status of our training resources.
print(trained)
```

    ## Training        : training
    ## Status          : Trained
    ## Number of layers: 5
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
    ##   0.2336235   0.4249688   0.3414078 
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
    ## 1  patient23 0.3899925  0.59490714  0.17449127 0.40350102
    ## 2  patient77 0.4222480  0.49866468  0.14058889 0.35856210
    ## 3  patient62 0.7736817  0.96993571          NA 0.90031823
    ## 4  patient43 0.3301254          NA          NA 0.33012540
    ## 5   patient8 0.7346532  0.85212024  0.83084960 0.81741522
    ## 6  patient74 0.5555329  0.69613294  0.54980476 0.61332790
    ## 7  patient29 0.3362905  0.46632857  0.27474444 0.37054031
    ## 8  patient17 0.3997508  0.34566627          NA 0.36485176
    ## 9  patient25 0.2778349  0.45710040  0.09602103 0.29194448
    ## 10 patient54 0.7863302          NA  0.84842857 0.82319925
    ## 11 patient60 0.7415452  0.84184524  0.79166865 0.80128213
    ## 12 patient44 0.3717952          NA          NA 0.37179524
    ## 13  patient1 0.8211250  0.93648373          NA 0.89556234
    ## 14 patient76 0.6840357          NA  0.60861508 0.63925695
    ## 15 patient16 0.6923929          NA  0.69758968 0.69547832
    ## 16 patient27 0.3699286          NA  0.20396786 0.27139431
    ## 17 patient58 0.5648032  0.75506071  0.76331349 0.71342965
    ## 18 patient52 0.4850317  0.13170119          NA 0.25703870
    ## 19 patient10 0.2517774          NA          NA 0.25177738
    ## 20 patient72 0.7087675  0.94573135  0.62339762 0.78032379
    ## 21 patient39        NA  0.08271032          NA 0.08271032
    ## 25 patient46        NA  0.22185635  0.51994405 0.35464937
    ## 26 patient97        NA  0.70887738  0.86840476 0.77994413
    ## 27 patient31        NA  0.28228095          NA 0.28228095
    ## 31 patient87        NA  0.29549603  0.26039048 0.27985710
    ## 33 patient59        NA  0.14183968  0.38772183 0.25137601
    ## 34  patient2        NA  0.55434325  0.79045437 0.65952675
    ## 53 patient85        NA          NA  0.15880913 0.15880913
    ## 60  patient3        NA          NA  0.57764881 0.57764881

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
    ##   0.1147740   0.1645159   0.0815040   0.1231755

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
    ##  0.12551583  0.13679344  0.06749516  0.09629256

© 2024 Institute of Medical Biometry and Statistics (IMBS). All rights
reserved.
