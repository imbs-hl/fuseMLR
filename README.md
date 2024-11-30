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
`TrainData`, `Lrner`, and `VarSel` (for variable selection) are stored
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
data("multi_omics")
# This is a list containing two lists of data: training and test.
# Each sublist contains three omics data.
str(object = multi_omics, max.level = 2L)
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
training <- createTraining(id = "training",
                           ind_col = "IDS",
                           target = "disease",
                           target_df = multi_omics$training$target,
                           verbose = TRUE)
print(training)
```

    ## Training        : training
    ## Problem type    : classification
    ## Status          : Not trained
    ## Number of layers: 0
    ## Layers trained  : 0

- Prepare training layers: Training layers contain `TrainData`, `Lrner`
  and `VarSel` objects. Therefore arguments to instantiate those object
  are provided when creating training layers. Users can choose to set up
  layer-specific learners, but for illustration, we will use the same
  learner for all layers. A `data.frame` of training data is required to
  instantiate a `TrainData` object. For `Lrner` and `VarSel` objects
  package and function names, and list of arguments are required.

``` r
# Create gene expression layer
createTrainLayer(training = training,
                 train_layer_id = "geneexpr",
                 train_data = multi_omics$training$geneexpr,
                 varsel_package = "Boruta",
                 varsel_fct = "Boruta",
                 varsel_param = list(num.trees = 1000L,
                                     mtry = 3L,
                                     probability = TRUE,
                                     na.action = "na.learn"),
                 lrner_package = "ranger",
                 lrn_fct = "ranger",
                 param_train_list = list(probability = TRUE,
                                         mtry = 1L,
                                     na.action = "na.learn"),
                 param_pred_list = list(),
                 na_action = "na.keep")
```

    ## Training        : training
    ## Problem type    : classification
    ## Status          : Not trained
    ## Number of layers: 1
    ## Layers trained  : 0
    ## p               : 131
    ## n               :  50

``` r
# Create gene protein abundance layer
createTrainLayer(training = training,
                 train_layer_id = "proteinexpr",
                 train_data = multi_omics$training$proteinexpr,
                 varsel_package = "Boruta",
                 varsel_fct = "Boruta",
                 varsel_param = list(num.trees = 1000L,
                                     mtry = 3L,
                                     probability = TRUE,
                                     na.action = "na.learn"),
                 lrner_package = "ranger",
                 lrn_fct = "ranger",
                 param_train_list = list(probability = TRUE,
                                         mtry = 1L,
                                     na.action = "na.learn"),
                 param_pred_list = list(),
                 na_action = "na.keep")
```

    ## Training        : training
    ## Problem type    : classification
    ## Status          : Not trained
    ## Number of layers: 2
    ## Layers trained  : 0
    ## p               : 131 | 160
    ## n               :  50 |  50

``` r
# Create methylation layer
createTrainLayer(training = training,
                 train_layer_id = "methylation",
                 train_data = multi_omics$training$proteinexpr,
                 varsel_package = "Boruta",
                 varsel_fct = "Boruta",
                 varsel_param = list(num.trees = 1000L,
                                     mtry = 3L,
                                     probability = TRUE,
                                     na.action = "na.learn"),
                 lrner_package = "ranger",
                 lrn_fct = "ranger",
                 param_train_list = list(probability = TRUE,
                                         mtry = 1L,
                                     na.action = "na.learn"),
                 param_pred_list = list(),
                 na_action = "na.keep")
```

    ## Training        : training
    ## Problem type    : classification
    ## Status          : Not trained
    ## Number of layers: 3
    ## Layers trained  : 0
    ## p               : 131 | 160 | 160
    ## n               :  50 |  50 |  50

- Also add a meta layer.

``` r
# Create meta layer
createTrainMetaLayer(training = training,
                     meta_layer_id = "meta_layer",
                     lrner_package = NULL,
                     lrn_fct = "weightedMeanLearner",
                     param_train_list = list(),
                     param_pred_list = list(),
                     na_action = "na.impute")
```

    ## Training        : training
    ## Problem type    : classification
    ## Status          : Not trained
    ## Number of layers: 4
    ## Layers trained  : 0
    ## p               : 131 | 160 | 160
    ## n               :  50 |  50 |  50

- An upset plot of the training data: Visualize patient overlap across
  layers.

``` r
upsetplot(object = training, order.by = "freq")
```

![](README_files/figure-gfm/upsetplot-1.png)<!-- -->

#### C) Variable selection

Perform variable selection on our training resources

``` r
# Variable selection
set.seed(5467)
var_sel_res <- varSelection(training = training)
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
    ## 21 methylation Caveolin.1

For each layer, the variable selection results show the chosen
variables. In this example, we perform variable selection. Users can opt
to conduct variable selection on individual layers if desired.

#### D) Training

We can now train our models using the subset of selected variables.

``` r
set.seed(5462)
training <- fusemlr(training = training,
                    use_var_sel = TRUE,
                    resampling_method = NULL,
                    resampling_arg = list(y = multi_omics$training$target$disease,
                                          k = 10L))
```

    ## Creating fold predictions.

    ## ================================================================================

    ## Training on layer geneexpr started.

    ## Training on layer geneexpr done.

    ## Training on layer proteinexpr started.

    ## Training on layer proteinexpr done.

    ## Training on layer methylation started.

    ## Training on layer methylation done.

``` r
print(training)
```

    ## Training        : training
    ## Problem type    : classification
    ## Status          : Trained
    ## Number of layers: 4
    ## Layers trained  : 4
    ## Var. sel. used  : Yes
    ## p               : 131 | 160 | 160 |  3
    ## n               :  50 |  50 |  50 | 64

``` r
# See also summary(training)
```

Use `extractModel` to retrieve the list of stored models and
`extractData` to retrieve training data.

``` r
models_list <- extractModel(training = training)
print(str(object = models_list, max.level = 1L))
```

    ## List of 4
    ##  $ geneexpr   :List of 14
    ##  $ proteinexpr:List of 14
    ##  $ methylation:List of 14
    ##  $ meta_layer : 'weightedMeanLearner' Named num [1:3] 0.501 0.29 0.209
    ##   ..- attr(*, "names")= chr [1:3] "geneexpr" "proteinexpr" "methylation"
    ## NULL

The list of four models (three random forests and one weighted
meta-model) trained on each layer is returned.

``` r
data_list <- extractData(object = training)
str(object = data_list, max.level = 1)
```

    ## List of 4
    ##  $ geneexpr   :'data.frame': 50 obs. of  133 variables:
    ##  $ proteinexpr:'data.frame': 50 obs. of  162 variables:
    ##  $ methylation:'data.frame': 50 obs. of  162 variables:
    ##  $ meta_layer :'data.frame': 64 obs. of  5 variables:

The list of the four training data (the three simulated training
modalities and the meta-data) is returned.

#### E) Predicting

In this section, we create a `testing` instance (from the *Testing*
class) and make predictions for new data. This is done analogously to
`training`. The only difference that only the testing data modalities
are required. Relevant functions are `createTesting()` and
`createTestLayer()`.

``` r
# Create testing for predcitions
testing <- createTesting(id = "testing",
                           ind_col = "IDS")

# Create gene expression layer
createTestLayer(testing = testing,
                test_layer_id = "geneexpr",
                 test_data = multi_omics$testing$geneexpr)
```

    ## Testing         : testing
    ## Number of layers: 1
    ## p               : 131
    ## n               :  20

``` r
# Create gene protein abundance layer
createTestLayer(testing = testing,
                 test_layer_id = "proteinexpr",
                 test_data = multi_omics$testing$proteinexpr)
```

    ## Testing         : testing
    ## Number of layers: 2
    ## p               : 131 | 160
    ## n               :  20 |  20

``` r
# Create methylation layer
createTestLayer(testing = testing,
                 test_layer_id = "methylation",
                 test_data = multi_omics$testing$proteinexpr)
```

    ## Testing         : testing
    ## Number of layers: 3
    ## p               : 131 | 160 | 160
    ## n               :  20 |  20 |  20

A look on testing data.

``` r
data_list <- extractData(object = testing)
str(object = data_list, max.level = 1)
```

    ## List of 3
    ##  $ geneexpr   :'data.frame': 20 obs. of  132 variables:
    ##  $ proteinexpr:'data.frame': 20 obs. of  161 variables:
    ##  $ methylation:'data.frame': 20 obs. of  161 variables:

An upset plot of the training data: Visualize patient overlap across
layers.

``` r
upsetplot(object = testing, order.by = "freq")
```

![](README_files/figure-gfm/upsetplot_new-1.png)<!-- -->

``` r
# See also extractData(testing = testing)
```

- Predict the testing object.

``` r
predictions <- predict(object = training, testing = testing)
print(predictions)
```

    ## $predicting
    ## Predicting   : testing
    ## Nb. layers   : 4
    ## 
    ## $predicted_values
    ##              IDS   geneexpr proteinexpr methylation meta_layer
    ## 1   participant1 0.80015714  0.93959762  0.92495794 0.86672812
    ## 2  participant10 0.27155317  0.27155317  0.27155317 0.27155317
    ## 3  participant16 0.68114127  0.68114127  0.68114127 0.68114127
    ## 4  participant17 0.38177857  0.30925873  0.36974444 0.35822212
    ## 5   participant2 0.61414286  0.56663333  0.66165238 0.61030376
    ## 6  participant23 0.42981587  0.63428413  0.92495794 0.59276022
    ## 7  participant25 0.27963730  0.40415000  0.48941032 0.35966166
    ## 8  participant27 0.36384444  0.36384444  0.36384444 0.36384444
    ## 9  participant29 0.37658968  0.45992778  0.24329444 0.37286840
    ## 10 participant31 0.21604841  0.27210000  0.15999683 0.22057776
    ## 11 participant39 0.06774087  0.08805476  0.04742698 0.06938237
    ## 12 participant43 0.30118651  0.30118651  0.30118651 0.30118651
    ## 13 participant44 0.40879048  0.40879048  0.40879048 0.40879048
    ## 14 participant46 0.35366349  0.21791667  0.48941032 0.34269423
    ## 15 participant52 0.50851190  0.12568889  0.04742698 0.30095513
    ## 16 participant54 0.78753413  0.78753413  0.78753413 0.78753413
    ## 17 participant58 0.53295873  0.77942222  0.59402937 0.61723869
    ## 18 participant59 0.18084444  0.13636111  0.22532778 0.17724989
    ## 19 participant60 0.73291905  0.83852619  0.65352381 0.74693873
    ## 20 participant62 0.78377778  0.96909206  0.95261508 0.87287306
    ## 21 participant72 0.69495794  0.94951984  0.95594841 0.82342858
    ## 22 participant74 0.54583016  0.68164921  0.13405079 0.49904934
    ## 23 participant76 0.68215079  0.68215079  0.68215079 0.68215079
    ## 24 participant77 0.47817778  0.45292381  0.57492778 0.49110054
    ## 25  participant8 0.73936111  0.87369683  0.51296032 0.73094799
    ## 26 participant87 0.17596865  0.30864365  0.04329365 0.18668969
    ## 27 participant97 0.54360079  0.74017381  0.34702778 0.55948523

- Prediction performances for layer-specific available patients, and all
  patients on the meta layer.

``` r
pred_values <- predictions$predicted_values
actual_pred <- merge(x = pred_values,
                     y = multi_omics$testing$target,
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
    ##   0.2728594   0.3511652   0.3056799   0.2936792

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
    ##   0.2728594   0.3511652   0.3056799   0.2936792

# E - Interface and wrapping

We distinguish common supervised learning arguments from method specific
arguments. The common arguments are a matrix `x` of independent
variables and `y` representing a response variable. These arguments are
handled by `fuseMLR`, so users do not need to make any adjustments
themselves. We also define standard argument names for predicting. The
arguments `object` and `data` (used by the generic `R` function
`predict` to pass model and the data for which prediction are needed)
are also stanard in `fuseMLR`. If the original learning or predicting
function do not use these names as arguments, either an interface or a
wrapping of the original function can be done to solve name
discepancies.

## Interface

The interface approach leverages the arguments in `createTrainLayer()`
to map the argument names of the original learning function. In the
example below, the gene expression layer is re-created using the `svm`
(Support Vector Machine) function from the `e1071` package as the
learner. A discrepancy arises in the argument names of the `predict.svm`
function, which uses `object` and `newdata`.

``` r
# Re-create the gene expression layer with support vector machine as learner.
createTrainLayer(training = training,
                 train_layer_id = "geneexpr",
                 train_data = multi_omics$training$geneexpr,
                 varsel_package = "Boruta",
                 varsel_fct = "Boruta",
                 varsel_param = list(num.trees = 1000L,
                                     mtry = 3L,
                                     probability = TRUE),
                 lrner_package = "e1071",
                 lrn_fct = "svm",
                 param_train_list = list(type = 'C-classification',
                                         kernel = 'radial',
                                         probability = TRUE),
                 param_pred_list = list(probability = TRUE),
                 na_action = "na.keep",
                 x = "x",
                 y = "y",
                 object = "object",
                 data = "newdata", # Name discrepancy resolved.
                 extract_pred_fct = function (pred) { 
                   pred <- attr(pred, "probabilities")
                   return(pred[ , 1L])
                 }
)
```

    ## Training        : training
    ## Problem type    : classification
    ## Status          : Trained
    ## Number of layers: 4
    ## Layers trained  : 4
    ## Var. sel. used  : Yes
    ## p               : 131 | 160 | 160 |  3
    ## n               :  50 |  50 |  50 | 64

``` r
# Variable selection
set.seed(5467)
var_sel_res <- varSelection(training = training)
```

    ## Variable selection on layer geneexpr started.

    ## Variable selection on layer geneexpr done.

    ## Variable selection on layer proteinexpr started.

    ## Variable selection on layer proteinexpr done.

    ## Variable selection on layer methylation started.

    ## Variable selection on layer methylation done.

``` r
set.seed(5462)
training <- fusemlr(training = training,
                    use_var_sel = TRUE)
```

    ## Creating fold predictions.

    ## ================================================================================

    ## Training on layer geneexpr started.

    ## Training on layer geneexpr done.

    ## Training on layer proteinexpr started.

    ## Training on layer proteinexpr done.

    ## Training on layer methylation started.

    ## Training on layer methylation done.

``` r
print(training)
```

    ## Training        : training
    ## Problem type    : classification
    ## Status          : Trained
    ## Number of layers: 4
    ## Layers trained  : 5
    ## Var. sel. used  : Yes
    ## p               : 131 | 160 | 160 |  3
    ## n               :  50 |  50 |  50 | 64

## Wrapping

In the wrapping approach we re-define the function `mylasso` to run a
Lasso regression from the `glmnet` package as the meta-leaner.

``` r
# We wrap the original functions
mylasso <- function (x, y,
                     nlambda = 25,
                     nfolds = 5) {
  # Perform cross-validation to find the optimal lambda
  cv_lasso <- cv.glmnet(x = as.matrix(x), y = y,
                        family = "binomial",
                        type.measure = "deviance",
                        nfolds = nfolds)
  best_lambda <- cv_lasso$lambda.min
  lasso_best <- glmnet(x = as.matrix(x), y = y,
                       family = "binomial",
                       alpha = 1,
                       lambda = best_lambda
  )
  lasso_model <- list(model = lasso_best)
  class(lasso_model) <- "mylasso"
  return(lasso_model)
}

predict.mylasso <- function(object, data) {
  glmnet_pred <- predict(object = object$model,
                         newx = as.matrix(data),
                         type = "response",
                         s = object$model$lambda)
  return(as.vector(glmnet_pred))
}

# Remove the current gene expression layer from training
removeLayer(training = training, layer_id = "meta_layer")
# Re-create the gene expression layer with support vector machine as learner.
createTrainMetaLayer(training = training,
                     meta_layer_id = "meta_layer",
                     lrner_package = NULL,
                     lrn_fct = "mylasso",
                     param_train_list = list(nlambda = 100L),
                     na_action = "na.impute")
set.seed(5462)
training <- fusemlr(training = training,
                    use_var_sel = TRUE)
print(training)
```

### Appendix

In addition to any pre-existing learner in R as a meta-learner, we have
implemented the following ones.

| Leaner              | Description                                                                                               |
|:--------------------|:----------------------------------------------------------------------------------------------------------|
| weightedMeanLearner | The weighted mean meta learner. It uses meta data to estimate the weights of the modality-specific models |
| bestLayerLearner    | The best layer-specific model is used as meta model.                                                      |

© 2024 Institute of Medical Biometry and Statistics (IMBS). All rights
reserved.
