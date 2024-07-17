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
    ## 7     geneexpr          MAPK14
    ## 8     geneexpr            PCNA
    ## 9     geneexpr           SMAD4
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
    ## 38 methylation      cg09637363
    ## 39 methylation      cg01306510
    ## 40 methylation      cg02412050
    ## 41 methylation      cg25984124
    ## 42 methylation      cg07566050
    ## 43 methylation      cg02630105
    ## 44 methylation      cg20849549
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
    ## n         : 27
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
    ## 1   subject4 0.4725151   0.6599369   0.3956623  0.5086549
    ## 2   subject7 0.6207952   0.2535532   0.4593956  0.4343414
    ## 3   subject8 0.7858988   0.8617183   0.5170813  0.7124039
    ## 4  subject10 0.7736159   0.8144282   0.7356651  0.7736219
    ## 5  subject13 0.5496202   0.3388135   0.1889472  0.3432458
    ## 6  subject15 0.7520679   0.8456587   0.3008671  0.6170995
    ## 7  subject16 0.6741667   0.3009167   0.4049599  0.4456820
    ## 8  subject18 0.7936623   0.3036036   0.1069857  0.3701630
    ## 9  subject23 0.7684464   0.3103667   0.6047968  0.5498365
    ## 10 subject24 0.4487278   0.6280996   0.4865853  0.5246098
    ## 11 subject27 0.3982817   0.2857008   0.4592833  0.3820728
    ## 12 subject31 0.4925512   0.8178683   0.4383095  0.5846202
    ## 13 subject32 0.5856746   0.7545948   0.5088413  0.6154420
    ## 14 subject35 0.4182294   0.8120417   0.5838194  0.6153982
    ## 15 subject36 0.4785877   0.2373167   0.4264560  0.3760711
    ## 16 subject50 0.8579135   0.5765698   0.5744472  0.6558340
    ## 17 subject54 0.6482135   0.7085040   0.8919758  0.7593542
    ## 18 subject55 0.6911262   0.2668444   0.5258881  0.4835815
    ## 19 subject59 0.5783048   0.2353687   0.4648218  0.4179925
    ## 20 subject62 0.2776175   0.3502944   0.3815302  0.3411933
    ## 21 subject63 0.4978976   0.8386833   0.6942627  0.6881890
    ## 22 subject66 0.7175944   0.6737528   0.7879369  0.7285500
    ## 23 subject70 0.3779357   0.3989810   0.2578671  0.3406884

`&copy;` 2024 Institute of Medical Biometry and Statistics (IMBS). All
rights reserved.
