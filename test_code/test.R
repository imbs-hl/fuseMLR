data("entities")

# Training study
train_study <- TrainStudy$new(id = "train_study",
                              ind_col = "IDS",
                              target = "disease")

# Training layers
tl_geneexpr <- TrainLayer$new(id = "geneexpr", train_study = train_study)
tl_proteinexpr <- TrainLayer$new(id = "proteinexpr", train_study = train_study)
tl_methylation <- TrainLayer$new(id = "methylation", train_study = train_study)
tl_meta_layer <- TrainMetaLayer$new(id = "meta_layer", train_study = train_study)

# Training data
train_data_geneexpr <- TrainData$new(id = "geneexpr",
                                     train_layer = tl_geneexpr,
                                     data_frame = entities$training$geneexpr[-10, ])
train_data_proteinexpr <- TrainData$new(id = "proteinexpr",
                                        train_layer = tl_proteinexpr,
                                        data_frame = entities$training$proteinexpr)
train_data_methylation <- TrainData$new(id = "methylation",
                                        train_layer = tl_methylation,
                                        data_frame = entities$training$methylation)

# Learner parameters. Same parameter values at each layer.
same_param <- ParamLrner$new(id = "ParamRanger",
                               param_list = list(probability = TRUE,
                                                 mtry = 1),
                               hyperparam_list = list(num.trees = 1000))
# Learner
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

# Training using the caret corssvalidation with default parameters.
# Train the all study

train_study$createMetaTrainData(resampling_method = "caret::createFolds",
                                resampling_arg = list(y = train_study$getTargetValues()$disease,
                                                      k = 2))

trained_study <- train_study$train(resampling_method = "caret::createFolds",
                                   resampling_arg = list(y = train_study$getTargetValues()$disease,
                                                         k = 2))

# Create and predict a new study

# Create a new study
new_study <- NewStudy$new(id = "new_study", ind_col = "IDS")

# A meta_layer is not reuired
new_geneexpr <- NewLayer$new(id = "geneexpr", new_study = new_study)
new_proteinexpr <- NewLayer$new(id = "proteinexpr", new_study = new_study)
new_methylation <- NewLayer$new(id = "methylation", new_study = new_study)

# NewData are required at each layers
new_data_geneexpr <- NewData$new(id = "geneexpr",
                                 new_layer = new_geneexpr,
                                 data_frame = entities$testing$geneexpr)
new_data_proteinexpr <- NewData$new(id = "proteinexpr",
                                    new_layer = new_proteinexpr,
                                    data_frame = entities$testing$proteinexpr)
new_data_methylation <- NewData$new(id = "methylation",
                                    new_layer = new_methylation,
                                    data_frame = entities$testing$methylation)


tmp_red_study <- train_study$predict(new_study = new_study)

