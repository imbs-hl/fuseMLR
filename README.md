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
    ## 4     geneexpr            CDH3
    ## 5     geneexpr           CHEK2
    ## 6     geneexpr           EIF4E
    ## 7     geneexpr          MAP2K1
    ## 8     geneexpr          MAPK14
    ## 9     geneexpr            PCNA
    ## 10    geneexpr           SMAD4
    ## 11    geneexpr          SQSTM1
    ## 12    geneexpr           YWHAE
    ## 13    geneexpr           YWHAZ
    ## 14 proteinexpr        Bap1.c.4
    ## 15 proteinexpr             Bid
    ## 16 proteinexpr       Cyclin_E2
    ## 17 proteinexpr      P.Cadherin
    ## 18 proteinexpr            Chk1
    ## 19 proteinexpr      Chk1_pS345
    ## 20 proteinexpr            EGFR
    ## 21 proteinexpr     EGFR_pY1173
    ## 22 proteinexpr     HER3_pY1289
    ## 23 proteinexpr           MIG.6
    ## 24 proteinexpr           ETS.1
    ## 25 proteinexpr MEK1_pS217_S221
    ## 26 proteinexpr        p38_MAPK
    ## 27 proteinexpr    c.Met_pY1235
    ## 28 proteinexpr           N.Ras
    ## 29 proteinexpr            PCNA
    ## 30 proteinexpr     PEA15_pS116
    ## 31 proteinexpr PKC.delta_pS664
    ## 32 proteinexpr           Rad50
    ## 33 proteinexpr     C.Raf_pS338
    ## 34 proteinexpr          p70S6K
    ## 35 proteinexpr    p70S6K_pT389
    ## 36 proteinexpr           Smad4
    ## 37 proteinexpr     STAT3_pY705
    ## 38 proteinexpr  14.3.3_epsilon
    ## 39 methylation      cg20139214
    ## 40 methylation      cg18457775
    ## 41 methylation      cg24747396
    ## 42 methylation      cg01306510
    ## 43 methylation      cg11861730
    ## 44 methylation      cg02412050
    ## 45 methylation      cg07566050
    ## 46 methylation      cg02630105
    ## 47 methylation      cg20849549
    ## 48 methylation      cg00547829
    ## 49 methylation      cg25539131
    ## 50 methylation      cg07064406
    ## 51 methylation      cg11816577

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
    ## n         : 22
    ## Missing   : 0
    ## p         : 14

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
    ## 1   subject4 0.6484115   0.6232698  0.31071389  0.5184565
    ## 2   subject7 0.4769810   0.2258972  0.54508016  0.4157126
    ## 3   subject8 0.7140984   0.8477976  0.66553690  0.7423027
    ## 4  subject10 0.6940758   0.7736833  0.64894563  0.7050413
    ## 5  subject13 0.5576877   0.3196278  0.12406032  0.3205452
    ## 6  subject15 0.7106389   0.8446940  0.27681587  0.6005008
    ## 7  subject16 0.6214357   0.3434575  0.24932183  0.3927695
    ## 8  subject18 0.6862171   0.2557313  0.09267262  0.3258879
    ## 9  subject23 0.6038460   0.2769857  0.55668968  0.4752774
    ## 10 subject24 0.4790024   0.6149226  0.45314762  0.5161260
    ## 11 subject27 0.4629325   0.2620349  0.51638849  0.4135396
    ## 12 subject31 0.4329060   0.8181782  0.47140833  0.5783116
    ## 13 subject32 0.5698889   0.7577599  0.65391865  0.6642461
    ## 14 subject35 0.4604948   0.7791980  0.45331230  0.5667484
    ## 15 subject36 0.3806321   0.1859825  0.46996190  0.3462660
    ## 16 subject50 0.7118357   0.5917563  0.64517460  0.6468713
    ## 17 subject54 0.5519837   0.6745282  0.67802460  0.6391312
    ## 18 subject55 0.6578337   0.2442829  0.44351508  0.4395823
    ## 19 subject59 0.4506401   0.2893857  0.45415952  0.3968377
    ## 20 subject62 0.4784972   0.3452619  0.33005794  0.3796489
    ## 21 subject63 0.4671940   0.8077119  0.82196270  0.7109832
    ## 22 subject66 0.6660956   0.7094044  0.85581270  0.7490700
    ## 23 subject70 0.3423341   0.3459401  0.31369524  0.3332727

© 2024 Institute of Medical Biometry and Statistics (IMBS). All rights
reserved.
