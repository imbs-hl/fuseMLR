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

- Also add a meta layer: a meta layer contains a `Lrner` object.

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

- Perform variable selection on our training resources

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

- Train the models with the selected variables.

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

- Use `extractModel` to retrieve the list of stored models and
  `extractData` to retrieve training data.

``` r
models_list <- extractModel(training = training)
data_list <- extractData(training = training)
```

#### E) Predicting

Now, we have created training resources, performed variable selection
and trained the models with the chosen variables. In this section, we
create testing resources and make predictions for new data.

- Create the testing object.

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
    ##          IDS geneexpr proteinexpr methylation meta_layer
    ## 1  patient23 1.476367    1.568467    1.930067   1.676138
    ## 2  patient77 1.471433    1.444500    1.583533   1.503905
    ## 3  patient62 1.847333    1.991567    1.975600   1.943983
    ## 4  patient43 1.283000          NA          NA         NA
    ## 5   patient8 1.687400    1.907100    1.570900   1.718811
    ## 6  patient74 1.525300    1.690933    1.033100   1.398830
    ## 7  patient29 1.254267    1.421767    1.207500   1.293830
    ## 8  patient17 1.344233    1.365300    1.179300   1.290150
    ## 9  patient25 1.246167    1.413467    1.517533   1.403792
    ## 10 patient54 1.776800          NA          NA         NA
    ## 11 patient60 1.780100    1.846467    1.708000   1.775884
    ## 12 patient44 1.397133          NA          NA         NA
    ## 13  patient1 1.799467    1.970900    1.930067   1.906227
    ## 14 patient76 1.602833          NA          NA         NA
    ## 15 patient16 1.733467          NA          NA         NA
    ## 16 patient27 1.310033          NA          NA         NA
    ## 17 patient58 1.593933    1.803167    1.499233   1.629882
    ## 18 patient52 1.480200    1.102133    1.023733   1.182209
    ## 19 patient10 1.261400          NA          NA         NA
    ## 20 patient72 1.736700    1.976733    1.982100   1.909404
    ## 21 patient39       NA    1.068833    1.023733         NA
    ## 25 patient46       NA    1.171067    1.517533         NA
    ## 26 patient97       NA    1.708133    1.346467         NA
    ## 27 patient31       NA    1.264567    1.079367         NA
    ## 31 patient87       NA    1.291867    1.015900         NA
    ## 33 patient59       NA    1.087867    1.182267         NA
    ## 34  patient2       NA    1.610933    1.709833         NA

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
    ##    1.174433    1.127329    1.038924    1.230136

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
    ##    1.159882    1.322029    1.253180    1.230136

Note that our example is based on simulated data for usage illustration;
only one run is not enough to appreciate the performances of our models.

© 2024 Institute of Medical Biometry and Statistics (IMBS). All rights
reserved.
