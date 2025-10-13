data("multi_omics")

# Training
training <- Training$new(id = "training",
                              ind_col = "IDS",
                              target = "disease")

# Training layers
tl_geneexpr <- TrainLayer$new(id = "geneexpr", training = training)
tl_proteinexpr <- TrainLayer$new(id = "proteinexpr", training = training)
tl_methylation <- TrainLayer$new(id = "methylation", training = training)
tl_meta_layer <- TrainMetaLayer$new(id = "meta_layer", training = training)

# Training data
train_data_geneexpr <- TrainData$new(id = "geneexpr",
                                     train_layer = tl_geneexpr,
                                     data_frame = multi_omics$training$geneexpr[-10, ])
train_data_proteinexpr <- TrainData$new(id = "proteinexpr",
                                        train_layer = tl_proteinexpr,
                                        data_frame = multi_omics$training$proteinexpr)
train_data_methylation <- TrainData$new(id = "methylation",
                                        train_layer = tl_methylation,
                                        data_frame = multi_omics$training$methylation)

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
# Train the all layers

training$createMetaTrainData(resampling_method = "caret::createFolds",
                                resampling_arg = list(y = training$getTargetValues()$disease,
                                                      k = 2))

trained <- training$train(resampling_method = "caret::createFolds",
                                   resampling_arg = list(y = training$getTargetValues()$disease,
                                                         k = 2))

# Create and predict a testing object

# Create a testing object
testing <- Testing$new(id = "testing", ind_col = "IDS")

# A meta_layer is not reuired
new_geneexpr <- TestLayer$new(id = "geneexpr", testing = testing)
new_proteinexpr <- TestLayer$new(id = "proteinexpr", testing = testing)
new_methylation <- TestLayer$new(id = "methylation", testing = testing)

# TestData are required at each layers
testing_data_geneexpr <- TestData$new(id = "geneexpr",
                                 new_layer = new_geneexpr,
                                 data_frame = multi_omics$testing$geneexpr)
testing_data_proteinexpr <- TestData$new(id = "proteinexpr",
                                    new_layer = new_proteinexpr,
                                    data_frame = multi_omics$testing$proteinexpr)
testing_data_methylation <- TestData$new(id = "methylation",
                                    new_layer = new_methylation,
                                    data_frame = multi_omics$testing$methylation)


tmp_red <- training$predict(testing = testing)

