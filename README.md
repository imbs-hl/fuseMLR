<!-- badges: start -->

[![R-CMD-check](https://github.com/imbs-hl/fuseMLR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/imbs-hl/fuseMLR/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/github/imbs-hl/fuseMLR/graph/badge.svg?token=SZU7NGK8G8)](https://codecov.io/github/imbs-hl/fuseMLR)
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

`fuseMLR` is an object-oriented package based on `R6` version 2.5.1.

### Installation

Install the development version from GitHub with

``` r
devtools::install_github("imbs-hl/fuseMLR")
```

### Usage example

The following example is based on simulated data available in `fuseMLR`.
Data have been simulated using the R package `InterSIM`, version 2.2.0.

``` r
library(fuseMLR)
library(UpSetR)
library(ranger)
```

- Let us inspect our simulated data.

``` r
data("entities")
# This is a list containing two lists of data: training and test.
# Each sublist contains three entities.
str(object = entities, max.level = 2L)
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
of `fuseMLR`. We can perform variable selection, train and fuse models
for training studies, and predict new studies.

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
                                     param_list = list(num.trees = 1000L,
                                                       mtry = 3L))
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
    ## 3     geneexpr           CHEK2
    ## 4     geneexpr           EIF4E
    ## 5     geneexpr          MAP2K1
    ## 6     geneexpr          MAPK14
    ## 7     geneexpr            PCNA
    ## 8     geneexpr           SMAD4
    ## 9     geneexpr          SQSTM1
    ## 10    geneexpr           YWHAE
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
    ## 38 methylation      cg24747396
    ## 39 methylation      cg01306510
    ## 40 methylation      cg02412050
    ## 41 methylation      cg07566050
    ## 42 methylation      cg02630105
    ## 43 methylation      cg20849549
    ## 44 methylation      cg00547829
    ## 45 methylation      cg25539131
    ## 46 methylation      cg07064406

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
                                               mtry = 2L),
                             hyperparam_list = list(num.trees = 1000L))
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
                                                         k = 2L),
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
    ## p         : 11

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
    ## 1   subject4 0.6637889   0.6793532  0.36752579  0.5569461
    ## 2   subject7 0.4678730   0.1669341  0.47024246  0.3635983
    ## 3   subject8 0.6273369   0.9242417  0.73984722  0.7737407
    ## 4  subject10 0.7341560   0.8031524  0.55480556  0.6902961
    ## 5  subject13 0.5412294   0.2743829  0.08730556  0.2759364
    ## 6  subject15 0.6477873   0.8806897  0.30431706  0.5990090
    ## 7  subject16 0.8169798   0.1946786  0.46707183  0.4668832
    ## 8  subject18 0.6804615   0.1561825  0.02034048  0.2470528
    ## 9  subject23 0.6450365   0.1402103  0.72942460  0.5005914
    ## 10 subject24 0.5658567   0.6941516  0.38927381  0.5437694
    ## 11 subject27 0.3589107   0.1916667  0.43114444  0.3278381
    ## 12 subject31 0.6205544   0.8447563  0.44271230  0.6315077
    ## 13 subject32 0.5908952   0.7145917  0.72011230  0.6830976
    ## 14 subject35 0.4189183   0.8294321  0.62285754  0.6396775
    ## 15 subject36 0.4889690   0.1799242  0.41842579  0.3542279
    ## 16 subject50 0.7387599   0.4915492  0.78699048  0.6706433
    ## 17 subject54 0.7166698   0.7474905  0.83568294  0.7725465
    ## 18 subject55 0.5898306   0.1715333  0.55571190  0.4307124
    ## 19 subject59 0.3705266   0.2178278  0.40865952  0.3316133
    ## 20 subject62 0.4333012   0.2416484  0.49517857  0.3897732
    ## 21 subject63 0.4968206   0.8056214  0.86868611  0.7456762
    ## 22 subject66 0.6390286   0.6639175  0.94965833  0.7654539
    ## 23 subject70 0.4320159   0.2872944  0.31015437  0.3352534

© 2024 Institute of Medical Biometry and Statistics (IMBS). All rights
reserved.
