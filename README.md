<!-- badges: start -->
  [![R-CMD-check](https://github.com/imbs-hl/fuseMLR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/imbs-hl/fuseMLR/actions/workflows/R-CMD-check.yaml)
  [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
  [![CRAN downloads](http://www.r-pkg.org/badges/version/fuseMLR)](http://cranlogs.r-pkg.org/badges/grand-total/fuseMLR)
  [![Stack Overflow](https://img.shields.io/badge/stackoverflow-questions-orange.svg)](https://stackoverflow.com/questions/tagged/fuseMLR)
<!-- badges: end -->
  
### fuseMLR
Cesaire J. K. Fouodo

### Introduction
Recent technological advances have enabled the simultaneous targeting of multiple pathways to enhance therapies for complex diseases. This often results in the collection of numerous data entities across various layers of patient groups, posing a challenge in integrating all data into a single analysis. Ideally, patient data will overlap across layers, allowing for early or intermediate integrative techniques. However, when patient data does not overlap well, these techniques are difficult to apply. Late integration modeling addresses this by analyzing each data entity separately to obtain layer-specific results, which are then integrated using meta-analysis.

To address this need, we introduce the fuseMLR package for R, designed to facilitate late integration modeling. With fuseMLR, users can define a study with multiple layers and corresponding data entities, and train the entire study as a single task. The package is user-friendly, allowing for the training of different models across layers. Additionally, fuseMLR enables users to perform variable selection at the layer level and make predictions for new studies within a single task.

### Installation
Installation from Github:
```R
devtools::install_github("imbs-hl/fuseMLR")
```

CRAN release coming soon.

### Usage
For usage in R, see ?fuseMLR in R. Most importantly, see the Examples section. 

The provided example, utilizing simulated data, mirrors a common scenario in multi-omics analysis. It involves data collected from three distinct layers (methylation, gene expression, and protein expression), with disease status serving as the response variable. Initially, the data entities are consolidated into a single object. Subsequently, the learner arguments (such as ```ranger```) and feature selection parameters for each entity are specified. Following model training for both the entity-level models and the meta-learner, predictions can be generated for new datasets.

### Load data
```R
data("entities")
```

### Training

#### Training study
```R
train_study <- TrainStudy$new(id = "train_study",
                              ind_col = "IDS",
                              target = "disease")
```

#### Training layers
```R
tl_geneexpr <- TrainLayer$new(id = "geneexpr", train_study = train_study)
tl_proteinexpr <- TrainLayer$new(id = "proteinexpr", train_study = train_study)
tl_methylation <- TrainLayer$new(id = "methylation", train_study = train_study)
tl_meta_layer <- TrainMetaLayer$new(id = "meta_layer", train_study = train_study)
```

#### Training data
```R
train_data_geneexpr <- TrainData$new(id = "geneexpr",
                                     train_layer = tl_geneexpr,
                                     data_frame = entities$training$geneexpr[-10, ])
train_data_proteinexpr <- TrainData$new(id = "proteinexpr",
                                        train_layer = tl_proteinexpr,
                                        data_frame = entities$training$proteinexpr)
train_data_methylation <- TrainData$new(id = "methylation",
                                        train_layer = tl_methylation,
                                        data_frame = entities$training$methylation)
```

#### Variable selection
```R
same_param_varsel <- ParamVarSel$new(id = "ParamVarSel",
                               param_list = list(num.trees = 1000, mtry = 3))
                               
varsel_geneexpr <- VarSel$new(id = "varsel_geneexpr",
                                  package = "Boruta",
                                  varsel_fct = "Boruta",
                                  param = same_param_varsel,
                                  train_layer = tl_geneexpr)
                                  
varsel_proteinexpr <- VarSel$new(id = "varsel_geneexpr",
                                  package = "Boruta",
                                  varsel_fct = "Boruta",
                                  param = same_param_varsel,
                                  train_layer = tl_proteinexpr)
                                  
varsel_methylation <- VarSel$new(id = "varsel_geneexpr",
                                  package = "Boruta",
                                  varsel_fct = "Boruta",
                                  param = same_param_varsel,
                                  train_layer = tl_methylation)
                                  
# Perform variable selection on the entire study
var_sel_res <- train_study$varSelection()
```

#### Learner parameters. Same parameter values at each layer.
```R
same_param <- ParamLrner$new(id = "ParamRanger",
                               param_list = list(probability = TRUE,
                                                 mtry = 1),
                               hyperparam_list = list(num.trees = 1000))
```

#### Learner

```R
lrner_geneexpr <- Lrner$new(id = "ranger",
                            package = "ranger",
                            lrn_fct = "ranger",
                            param = same_param,
                            train_layer = tl_geneexpr)
lrner_proteinexpr <- Lrner$new(id = "ranger",
                               package = "ranger",
                               lrn_fct = "ranger",
                               param = same_param,
                               train_layer = tl_proteinexpr)
lrner_methylation <- Lrner$new(id = "ranger",
                               package = "ranger",
                               lrn_fct = "ranger",
                               param = same_param,
                               train_layer = tl_methylation)
lrner_meta <- Lrner$new(id = "weighted",
                        lrn_fct = "weightedMeanLearner",
                        param = ParamLrner$new(id = "ParamWeighted",
                                                 param_list = list(),
                                                 hyperparam_list = list()),
                        train_layer = tl_meta_layer)

```

#### Train the all study using corss-validation.

```R
trained_study <- train_study$train(resampling_method = "caret::createFolds",
                                   resampling_arg = list(y=train_study$getTargetValues()$disease,
                                                         k = 2),
                                                         use_var_sel = TRUE)
```

### Predicting

#### Create and predict a new study

#### Create a new study

```R
new_study <- NewStudy$new(id = "new_study", ind_col = "IDS")
```

```R
# A meta_layer is not required
new_geneexpr <- NewLayer$new(id = "geneexpr", new_study = new_study)
new_proteinexpr <- NewLayer$new(id = "proteinexpr", new_study = new_study)
new_methylation <- NewLayer$new(id = "methylation", new_study = new_study)
```

#### NewData are mandatory at each layers

```R
new_data_geneexpr <- NewData$new(id = "geneexpr",
                                 new_layer = new_geneexpr,
                                 data_frame = entities$testing$geneexpr)
new_data_proteinexpr <- NewData$new(id = "proteinexpr",
                                    new_layer = new_proteinexpr,
                                    data_frame = entities$testing$proteinexpr)
new_data_methylation <- NewData$new(id = "methylation",
                                    new_layer = new_methylation,
                                    data_frame = entities$testing$methylation)

```

#### Predicting the new study
```R
tmp_red_study <- study$predict(new_study = new_study)
```
