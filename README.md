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
    ##   ..$ geneexpr   :'data.frame':  20 obs. of  133 variables:
    ##   ..$ proteinexpr:'data.frame':  20 obs. of  162 variables:
    ##   ..$ methylation:'data.frame':  20 obs. of  369 variables:

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
set.seed(546)
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
    ## 9     geneexpr           YWHAE
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
    ## 40 methylation      cg07566050
    ## 41 methylation      cg02630105
    ## 42 methylation      cg20849549
    ## 43 methylation      cg25539131
    ## 44 methylation      cg07064406

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
                        na_rm = FALSE,
                        train_layer = tl_meta)
```

- Train the study with the selected variables.

``` r
set.seed(5462)
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
    ## n         : 27
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

- An upset plot of the training study: Visualize patient overlap across
  layers.

``` r
new_study$upset(order.by = "freq")
```

![](README_files/figure-gfm/upsetplot_new-1.png)<!-- -->

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
    ## 1  subject18 0.7211321   0.2188488  0.03343214  0.2855583
    ## 2  subject35 0.4783885          NA          NA  0.4783885
    ## 3  subject10 0.6770893   0.7388615  0.59987302  0.6724112
    ## 4  subject27 0.2707960   0.1899968  0.37721944  0.2788674
    ## 5  subject16 0.8036722   0.3176702          NA  0.5194458
    ## 6  subject23 0.8346401   0.1822258  0.53435754  0.4823478
    ## 7  subject55 0.5713615   0.1627544  0.52885317  0.4031231
    ## 8  subject36 0.3780171   0.1948250  0.32899484  0.2918028
    ## 9   subject8 0.6879345   0.8791171  0.60852421  0.7309028
    ## 10  subject4 0.6391833   0.6082790  0.31463175  0.5107753
    ## 11 subject62 0.2614496   0.2386063  0.27328175  0.2571594
    ## 12 subject63 0.4530873   0.8879044  0.89626587  0.7753800
    ## 13 subject15 0.6902270   0.8706675  0.36809048  0.6417914
    ## 14 subject24 0.5231448          NA  0.44239960  0.4766878
    ## 15 subject54 0.6748960   0.7518048          NA  0.7198742
    ## 16 subject59 0.3709865   0.2509175  0.32926746  0.3110274
    ## 17 subject31 0.6810714   0.8675401  0.57034087  0.7109998
    ## 18 subject13 0.5174298   0.2034099  0.08103373  0.2427913
    ## 19 subject50 0.8488313   0.6118333  0.75232698  0.7253845
    ## 20 subject66 0.7279552          NA  0.93345317  0.8461891
    ## 24 subject32        NA   0.7895071  0.58528373  0.6893807
    ## 28 subject70        NA   0.2313944  0.23812222  0.2346929
    ## 32  subject7        NA   0.2114790  0.38740833  0.2977334

© 2024 Institute of Medical Biometry and Statistics (IMBS). All rights
reserved.
