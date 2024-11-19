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
training <- createTraining(id = "training",
                           ind_col = "IDS",
                           target = "disease",
                           target_df = entities$training$target,
                           verbose = FALSE)
print(training)
```

    ## Training        : training
    ## Status          : Not trained
    ## Number of layers: 0
    ## Layers trained  : 0
    ## n               : 70

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
                 train_data = entities$training$geneexpr,
                 varsel_package = "Boruta",
                 varsel_fct = "Boruta",
                 varsel_param = list(num.trees = 1000L,
                                     mtry = 3L,
                                     probability = TRUE),
                 lrner_package = "ranger",
                 lrn_fct = "ranger",
                 param_train_list = list(probability = TRUE,
                                         mtry = 1L),
                 param_pred_list = list(),
                 na_rm = TRUE)
```

    ## Training        : training
    ## Status          : Not trained
    ## Number of layers: 1
    ## Layers trained  : 0
    ## n               : 70

``` r
# Create gene protein abundance layer
createTrainLayer(training = training,
                 train_layer_id = "proteinexpr",
                 train_data = entities$training$proteinexpr,
                 varsel_package = "Boruta",
                 varsel_fct = "Boruta",
                 varsel_param = list(num.trees = 1000L,
                                     mtry = 3L,
                                     probability = TRUE),
                 lrner_package = "ranger",
                 lrn_fct = "ranger",
                 param_train_list = list(probability = TRUE,
                                         mtry = 1L),
                 param_pred_list = list(),
                 na_rm = TRUE)
```

    ## Training        : training
    ## Status          : Not trained
    ## Number of layers: 2
    ## Layers trained  : 0
    ## n               : 70

``` r
# Create methylation layer
createTrainLayer(training = training,
                 train_layer_id = "methylation",
                 train_data = entities$training$proteinexpr,
                 varsel_package = "Boruta",
                 varsel_fct = "Boruta",
                 varsel_param = list(num.trees = 1000L,
                                     mtry = 3L,
                                     probability = TRUE),
                 lrner_package = "ranger",
                 lrn_fct = "ranger",
                 param_train_list = list(probability = TRUE,
                                         mtry = 1L),
                 param_pred_list = list(),
                 na_rm = TRUE)
```

    ## Training        : training
    ## Status          : Not trained
    ## Number of layers: 3
    ## Layers trained  : 0
    ## n               : 70

- Also add a meta layer.

``` r
# Create meta layer
createTrainMetaLayer(training = training,
                     meta_layer_id = "meta_layer",
                     lrner_package = NULL,
                     lrn_fct = "weightedMeanLearner",
                     param_train_list = list(),
                     param_pred_list = list(),
                     na_rm = FALSE)
```

    ## Training        : training
    ## Status          : Not trained
    ## Number of layers: 4
    ## Layers trained  : 0
    ## n               : 70

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
var_sel_res <- varSelection(training = training,
                            verbose = FALSE)
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
                    resampling_arg = list(y = entities$training$target$disease,
                                          k = 10L),
                    verbose = FALSE)

print(training)
```

    ## Training        : training
    ## Status          : Trained
    ## Number of layers: 4
    ## Layers trained  : 4
    ## n               : 70

``` r
# See also summary(training)
```

Use `extractModel` to retrieve the list of stored models and
`extractData` to retrieve training data.

``` r
models_list <- extractModel(training = training)
data_list <- extractData(training = training)
```

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
                 test_data = entities$testing$geneexpr)
```

    ## Testing         : testing
    ## Number of layers: 1

``` r
# Create gene protein abundance layer
createTestLayer(testing = testing,
                 test_layer_id = "proteinexpr",
                 test_data = entities$testing$proteinexpr)
```

    ## Testing         : testing
    ## Number of layers: 2

``` r
# Create methylation layer
createTestLayer(testing = testing,
                 test_layer_id = "methylation",
                 test_data = entities$testing$proteinexpr)
```

    ## Testing         : testing
    ## Number of layers: 3

- An upset plot of the training data: Visualize patient overlap across
  layers.

``` r
upsetplot(object = testing, order.by = "freq")
```

![](README_files/figure-gfm/upsetplot_new-1.png)<!-- -->

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
    ##          IDS  geneexpr proteinexpr methylation meta_layer
    ## 1  patient23 0.4298159  0.63428413  0.92495794  0.7136764
    ## 2  patient77 0.4781778  0.45292381  0.57492778  0.5110424
    ## 3  patient62 0.7837778  0.96909206  0.95261508  0.9206364
    ## 4  patient43 0.3011865          NA          NA         NA
    ## 5   patient8 0.7393611  0.87369683  0.51296032  0.6885350
    ## 6  patient74 0.5458302  0.68164921  0.13405079  0.4157759
    ## 7  patient29 0.3765897  0.45992778  0.24329444  0.3481374
    ## 8  patient17 0.3817786  0.30925873  0.36974444  0.3514659
    ## 9  patient25 0.2796373  0.40415000  0.48941032  0.4130304
    ## 10 patient54 0.7875341          NA          NA         NA
    ## 11 patient60 0.7329190  0.83852619  0.65352381  0.7353710
    ## 12 patient44 0.4087905          NA          NA         NA
    ## 13  patient1 0.8001571  0.93959762  0.92495794  0.9021728
    ## 14 patient76 0.6821508          NA          NA         NA
    ## 15 patient16 0.6811413          NA          NA         NA
    ## 16 patient27 0.3638444          NA          NA         NA
    ## 17 patient58 0.5329587  0.77942222  0.59402937  0.6446556
    ## 18 patient52 0.5085119  0.12568889  0.04742698  0.1774818
    ## 19 patient10 0.2715532          NA          NA         NA
    ## 20 patient72 0.6949579  0.94951984  0.95594841  0.8954590
    ## 21 patient39        NA  0.08805476  0.04742698         NA
    ## 25 patient46        NA  0.21791667  0.48941032         NA
    ## 26 patient97        NA  0.74017381  0.34702778         NA
    ## 27 patient31        NA  0.27210000  0.15999683         NA
    ## 31 patient87        NA  0.30864365  0.04329365         NA
    ## 33 patient59        NA  0.13636111  0.22532778         NA
    ## 34  patient2        NA  0.56663333  0.66165238         NA

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
    ##   0.1246471   0.1589727   0.2346355   0.1445188

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
    ##  0.13701187  0.09945737  0.22350114  0.14451877

Note that our example is based on simulated data for usage illustration;
only one run is not enough to appreciate the performances of our models.

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
# Remove the current gene expression layer from training
removeLayer(training = training, layer_id = "geneexpr")
```

    ## Warning in training$removeLayer(id = layer_id): training was already trained.
    ## Do not forget to train it again to update its meta layer.

    ## Training        : training
    ## Status          : Trained
    ## Number of layers: 3
    ## Layers trained  : 3
    ## n               : 70

``` r
# Re-create the gene expression layer with support vector machine as learner.
createTrainLayer(training = training,
                 train_layer_id = "geneexpr",
                 train_data = entities$training$geneexpr,
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
                 na_rm = TRUE,
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
    ## Status          : Trained
    ## Number of layers: 4
    ## Layers trained  : 3
    ## n               : 70

``` r
# Variable selection
set.seed(5467)
var_sel_res <- varSelection(training = training,
                            verbose = FALSE)
set.seed(5462)
training <- fusemlr(training = training,
                    use_var_sel = TRUE,
                    verbose = FALSE)

print(training)
```

    ## Training        : training
    ## Status          : Trained
    ## Number of layers: 4
    ## Layers trained  : 4
    ## n               : 70

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
                     na_rm = TRUE)
set.seed(5462)
training <- fusemlr(training = training,
                    use_var_sel = TRUE,
                    verbose = FALSE)
print(training)
```

### Appendix

In addition to any pre-existing learner in R as a meta-learner, we have
implemented the following ones.

| Leaner              | Description                                                                                               |
|:--------------------|:----------------------------------------------------------------------------------------------------------|
| weightedMeanLearner | The weighted mean meta learner. It uses meta data to estimate the weights of the modality-specific models |
| bestSpecificLearner | The best layer-specific model is used as meta model.                                                      |

© 2024 Institute of Medical Biometry and Statistics (IMBS). All rights
reserved.
