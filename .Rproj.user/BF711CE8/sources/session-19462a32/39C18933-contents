data("entities")

# Study
study <- Study$new(id = "study")

# Layers
geneexpr <- Layer$new(id = "geneexpr", study = study)
proteinexpr <- Layer$new(id = "proteinexpr", study = study)
methylation <- Layer$new(id = "methylation", study = study)
meta_layer <- MetaLayer$new(id = "meta_layer", study = study)

# Training data
train_data_geneexpr <- TrainData$new(id = "geneexpr",
                                     layer = geneexpr,
                                     ind_col = "IDS",
                                     data_frame = entities$training$geneexpr[-10, ],
                                     target = "disease")
train_data_proteinexpr <- TrainData$new(id = "proteinexpr",
                                        layer = proteinexpr,
                                        ind_col = "IDS",
                                        data_frame = entities$training$proteinexpr,
                                        target = "disease")
train_data_methylation <- TrainData$new(id = "methylation",
                                        layer = methylation,
                                        ind_col = "IDS",
                                        data_frame = entities$training$methylation,
                                        target = "disease")

# Learner parameters. Same parameter values at each layer.
same_param <- ParamLearner$new(id = "ParamRanger",
                               param_list = list(probability = TRUE,
                                                 mtry = 1),
                               hyperparam_list = list(num.trees = 1000))
# Learner
lrner_geneexpr <- Lrner$new(id = "ranger",
                            package = "ranger",
                            lrn_fct = "ranger",
                            param = same_param,
                            layer = geneexpr)
lrner_proteinexpr <- Lrner$new(id = "ranger",
                               package = "ranger",
                               lrn_fct = "ranger",
                               param = same_param,
                               layer = proteinexpr)
lrner_methylation <- Lrner$new(id = "ranger",
                               package = "ranger",
                               lrn_fct = "ranger",
                               param = same_param,
                               layer = methylation)
lrner_meta <- Lrner$new(id = "weighted",
                        lrn_fct = "weightedMeanLearner",
                        param = ParamLearner$new(id = "ParamWeighted",
                                                 param_list = list(),
                                                 hyperparam_list = list()),
                        layer = meta_layer)

# Training using the caret corssvalidation with default parameters.
# Train the all study

study$createMetaTrainData(resampling_method = "caret::createFolds",
                          resampling_arg = list(y = study$getTargetValues()$disease,
                                                k = 2))

study$getTargetValues()
trained_study <- study$train(resampling_method = "caret::createFolds",
                             resampling_arg = list(y = study$getTargetValues()$disease,
                                                   k = 2))

# Create and predict a new study

# Create a new study
new_study <- Study$new(id = "new_study")

# A meta_layer is not reuired
new_geneexpr <- Layer$new(id = "geneexpr", study = new_study)
new_proteinexpr <- Layer$new(id = "proteinexpr", study = new_study)
new_methylation <- Layer$new(id = "methylation", study = new_study)

# NewData are required at each layers
new_data_geneexpr <- NewData$new(id = "geneexpr",
                                     layer = new_geneexpr,
                                     ind_col = "IDS",
                                     data_frame = entities$testing$geneexpr)
new_data_proteinexpr <- NewData$new(id = "proteinexpr",
                                        layer = new_proteinexpr,
                                        ind_col = "IDS",
                                        data_frame = entities$testing$proteinexpr)
new_data_methylation <- NewData$new(id = "methylation",
                                        layer = new_methylation,
                                        ind_col = "IDS",
                                        data_frame = entities$testing$methylation)


tmp_red_study <- study$predict(new_study = new_study)

