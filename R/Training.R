#' @title Training Class
#'
#' @description
#' This is a primary classes of fuseMLR. An object from this class
#' is designed to contain multiple training layers, but only one meta training layer.
#'
#'  The Training class is structured as followed:
#' * [TrainLayer]: Specific layer containing:
#'    - [Lrner]: Specific learner. This must be set by the user.
#'    - [TrainData]: Specific training dataset. This must be set up by the user.
#'    - [Model]: Specific model. This is set up by training the learner on the training data.
#' * [TrainMetaLayer]: Basically a [TrainLayer], but with some specific properties.
#'    - [Lrner]: This is the meta learner, it must be set up by the user.
#'    - [TrainData]: Specific modality-specific prediction data. This is set up internally after cross-validation.
#'    - [Model]: Specific meta model. This is set up by training the learner on the training data.
#'
#' Use the function \code{train} for training and \code{predict} for predicting.
#'
#' @export
#'
#' @importFrom R6 R6Class
#'
#' @seealso [TrainLayer]
Training <- R6Class("Training",
                    inherit = HashTable,
                    public = list(
                      #' @description
                      #' constructor
                      #'
                      #' @param id `character` \cr
                      #' @param ind_col `character` \cr
                      #' Name of column of individuals IDS.
                      #' @param target `character` \cr
                      #' Name of the target variable.
                      #' @param target_df `data.frame` \cr
                      #' Data frame with two columns: individual IDs and response variable values.
                      #' @param problem_type `character` \cr
                      #' Either "classification" or "regression".
                      #' @param verbose `boolean` \cr
                      #' Warning messages will be displayed if set to TRUE.
                      #' @seealso [Testing] and [Predicting]
                      initialize = function (id,
                                             ind_col,
                                             target,
                                             target_df,
                                             problem_type = "classification",
                                             verbose = TRUE) {
                        if (!is.logical(verbose)) {
                          stop("'verbose' must be bolean.")
                        }
                        if (!is.character(id)) {
                          stop("id must be a string")
                        }
                        if (!is.character(ind_col)) {
                          stop("ind_col must be a string")
                        }
                        if (!is.character(target)) {
                          stop("target must be a string")
                        }
                        if (!is.data.frame(target_df)) {
                          stop("target_df must be a data.frame.")
                        }
                        if ((ncol(target_df) != 2L)) {
                          stop("target_df must be a data.frame with two columns: individual IDs and response variable values.")
                        }
                        if (!all(c(ind_col, target) %in% names(target_df))) {
                          stop(sprintf("%s and %s not foung in target_df.",
                                       ind_col, target))
                        }
                        if (!(problem_type %in% c("classification", "regression"))) {
                          stop("The problem type must be either classification or regression")
                        } else {
                          target_values = target_df[ , target, drop = TRUE]
                          uniq_target_values = length(unique(target_values[complete.cases(target_values)]))
                          if ((problem_type == "classification") & (uniq_target_values > 2L)) {
                            stop("You set up a classification problem for more than two classes. Only binary classification problems are actually handled.\n")
                          }
                          if ((problem_type == "regression") & (uniq_target_values == 2L)) {
                            warning("You set up a regression problem for only two classes.\n")
                          }
                        }
                        super$initialize(id = id)
                        private$ind_col = ind_col
                        private$target = target
                        private$verbose = TRUE
                        private$problem_type = problem_type
                        private$target_obj = Target$new(id = "target",
                                                        data_frame = target_df[ , c(ind_col, target)],
                                                        training = self)
                        private$status = FALSE
                      },
                      #' @description
                      #' Printer
                      #'
                      #' @param ... `any`
                      #'
                      print = function (...) {
                        nb_layers = length(private$hash_table) - 1L
                        if (!private$status) {
                          status = "Not trained"
                        } else {
                          status = "Trained"
                          if (private$use_var_sel) {
                            on_var_sel = "Yes"
                          } else {
                            on_var_sel = "No"
                          }
                        }
                        cat(sprintf("Training        : %s\n", private$id))
                        cat(sprintf("Problem type    : %s\n", private$problem_type))
                        cat(sprintf("Status          : %s\n", status))
                        cat(sprintf("Number of layers: %s\n", nb_layers))
                        cat(sprintf("Layers trained  : %s\n", private$nb_trained_layer))
                        if (private$status) {
                          cat(sprintf("Var. sel. used  : %s\n", on_var_sel))
                        }
                        # cat(sprintf("n               : %s\n", nrow(private$target_obj$getData())))
                        layers = self$getKeyClass()
                        if (private$status) {
                          layers = layers[layers$class %in% c("TrainLayer", "TrainMetaLayer"), ]
                        } else {
                          layers = layers[layers$class %in% "TrainLayer", ]
                        }
                        nb_layers = nrow(layers)
                        if (nb_layers) {
                          layer_ps = NULL
                          layer_ns = NULL
                          lner_nas = NULL
                          for (k in layers$key) {
                            layer = self$getFromHashTable(key = k)
                            train_data = layer$getTrainData()
                            layer_p = ncol(train_data$getData())
                            layer_n = nrow(train_data$getData())
                            lner_na = layer$getLrner()$getNaAction()
                            max_width <- max(nchar(layer_p), nchar(layer_n))
                            layer_ps = c(layer_ps, format(layer_p, width = max_width, justify = "left"))
                            layer_ns = c(layer_ns, format(layer_n, width = max_width, justify = "left"))
                            lner_nas = c(lner_nas, format(lner_na, width = max_width, justify = "left"))
                          }
                          layer_ps = paste0(layer_ps, collapse = " | ")
                          layer_ns = paste0(layer_ns, collapse = " | ")
                          lner_nas = paste0(lner_nas, collapse = " | ")
                          cat(sprintf("p               : %s\n", layer_ps))
                          cat(sprintf("n               : %s\n", layer_ns))
                          cat(sprintf("na.action       : %s\n", lner_nas))
                        }
                        invisible(self)
                      },
                      #' @description
                      #' Train each layer of the current Training.
                      #'
                      #' @param ind_subset `character`\cr
                      #' Subset of individuals IDs to be used for training.
                      #' @param use_var_sel `boolean` \cr
                      #' If TRUE, selected variables available at each layer are used.
                      #' @param verbose `boolean` \cr
                      #' Warning messages will be displayed if set to TRUE.
                      #' @return
                      #' Returns the object itself, with a model for each layer.
                      #' @export
                      #'
                      #'
                      trainLayer = function (ind_subset = NULL,
                                             use_var_sel = FALSE,
                                             verbose = TRUE) {
                        layers = self$getKeyClass()
                        nb_layers = nrow(layers[layers$class %in% "TrainLayer", ])
                        if (nb_layers) {
                          # This code accesses each layer (except MetaLayer) level and trains the corres-
                          # ponding learner.
                          layers = layers[layers$class %in% "TrainLayer", ]
                          for (k in layers$key) {
                            layer = self$getFromHashTable(key = k)
                            layer$train(ind_subset = ind_subset,
                                        use_var_sel = use_var_sel,
                                        verbose = verbose)
                          }
                        } else {
                          stop("No existing layer in the current training object.")
                        }
                        invisible(self)
                      },
                      #' @description
                      #' Predicts values given new data.
                      #'
                      #' @param testing `TestData` \cr
                      #' Object of class [TestData].
                      #' @param ind_subset  `vector` \cr
                      #' Subset of individuals IDs to be used for training.
                      #'
                      #' @return
                      #' A new [Training] with predicted values for each layer.
                      #' @export
                      #'
                      predictLayer = function (testing,
                                               ind_subset = NULL) {
                        # Initialize a Training to store predictions
                        predicting = Predicting$new(id = testing$getId(),
                                                    ind_col = testing$getIndCol())
                        layers = testing$getKeyClass()
                        # This code accesses each layer (except MetaLayer) level
                        # and make predictions for the new layer in input.
                        layers = layers[layers$class %in% c("TestLayer", "TrainLayer"), ]
                        for (k in layers$key) {
                          new_layer = testing$getFromHashTable(key = k)
                          new_layer_kc = new_layer$getKeyClass()
                          m_layer = self$getFromHashTable(key = k)
                          pred_layer = m_layer$predict(new_layer = new_layer,
                                                       ind_subset = ind_subset,
                                                       use_var_sel = self$getUseVarSel())
                          # Add a new predicted layer to the predicted object
                          pred_layer$setPredicting(predicting)
                        }
                        return(predicting)
                      },
                      #' @description
                      #' Creates a meta training dataset and assigns it to the meta layer.
                      #'
                      #'
                      #' @param resampling_method `function` \cr
                      #' Function for internal validation.
                      #' @param resampling_arg `list` \cr
                      #' List of arguments to be passed to the function.
                      #' @param use_var_sel `boolean` \cr
                      #' If TRUE, selected variables available at each layer are used.
                      #' @param impute `boolean` \cr
                      #' If TRUE, mode or median based imputation is performed on the modality-specific predictions.
                      #'
                      #' @return
                      #' The current object is returned, with a meta training dataset assigned to the meta layer.
                      #' @export
                      #'
                      createMetaTrainData = function (resampling_method,
                                                      resampling_arg,
                                                      use_var_sel,
                                                      impute = TRUE) {
                        verbose = self$getVerbose()
                        layers = self$getKeyClass()
                        if (!("TrainMetaLayer" %in% layers$class)) {
                          stop("No existing meta layer. I cannot create meta training data.")
                        }
                        resampling = do.call(eval(parse(text = resampling_method)),
                                             resampling_arg)
                        if (!is.list(resampling)) {
                          stop("The resampling method must return a list of folds, with each fold containing a vector of training IDs.\n See example for details.")
                        } else {
                          if (verbose) {
                            message("Creating fold predictions.\n")
                            pb <- txtProgressBar(min = 0, max = length(resampling), style = 3)
                          }
                          train_layer_res_list = lapply(X = 1:length(resampling),
                                                        function (fold) {
                                                          if (verbose) {
                                                            percent <- round((fold / length(resampling)) * 100L, 1L)
                                                            setTxtProgressBar(pb = pb, value = fold, label = percent)
                                                          }
                                                          test_index = resampling[[fold]]
                                                          train_index = setdiff(unlist(resampling), test_index)
                                                          train_ids = self$getTargetValues()[train_index, 1L]
                                                          self$trainLayer(ind_subset = train_ids,
                                                                          use_var_sel = use_var_sel,
                                                                          verbose = FALSE)
                                                          test_ids = self$getTargetValues()[test_index, 1L]
                                                          # Note: The current object is not a TestStudy, but a Training object.
                                                          predicting = self$predictLayer(testing = self,
                                                                                         ind_subset = test_ids)
                                                          predicting_kc = predicting$getKeyClass()
                                                          ## Assess each layer and extract model
                                                          current_pred = NULL
                                                          for (k in predicting_kc$key) {
                                                            pred_layer = predicting$getFromHashTable(key = k)
                                                            # pred_layer = layer$getFromHashTable(key = "PredictLayer")
                                                            pred_data = pred_layer$getPredictData()
                                                            pred_values = pred_data$getPredictData()
                                                            current_pred = rbind(current_pred, pred_values)
                                                          }
                                                          return(current_pred)
                                                        })
                          close(pb)
                          predicted_values = data.frame(do.call(what = "rbind",
                                                                args = train_layer_res_list))
                          # Will transform modality-specific prediction data.frame into wide format. If predictions are data.frame like
                          # ranger predictions than only the first column will be consider.
                          predicted_values_wide = reshape(predicted_values,
                                                          idvar = colnames(predicted_values)[2],
                                                          timevar = colnames(predicted_values)[1],
                                                          direction = "wide")
                          colname_vector = gsub(pattern = "Prediction.",
                                                replacement = "",
                                                x = names(predicted_values_wide))
                          names(predicted_values_wide) = colname_vector
                          # target_df = self$getTargetValues()
                          target_df = private$target_obj$getData()
                          predicted_values_wide = merge(x = target_df,
                                                        y = predicted_values_wide,
                                                        by = colnames(target_df)[1],
                                                        all.y = TRUE)
                          # Add layer specific predictions to meta layer
                          layers = self$getKeyClass()
                          meta_layer_key = layers[layers$class == "TrainMetaLayer" , "key"]
                          meta_layer = self$getFromHashTable(key = meta_layer_key)
                          meta_layer$openAccess()
                          # predicted20242806 this word just serves as temporary key
                          meta_layer$setTrainData(id = "predicted20242806",
                                                  ind_col = names(predicted_values_wide)[1L],
                                                  data_frame = predicted_values_wide)
                          if (impute) {
                            private$impute = impute
                            meta_layer$impute()
                          }
                          # meta_layer$set2NotTrained()
                          meta_layer$closeAccess()
                          return(predicted_values_wide)
                        }
                      },
                      #' @description
                      #' Trains the current object. All leaners and the meta learner are trained.
                      #'
                      #' @param ind_subset `vector` \cr
                      #' ID subset to be used for training.
                      #' @param use_var_sel `boolean` \cr
                      #' If TRUE, variable selection is performed before training.
                      #' @param resampling_method `function` \cr
                      #' Function for internal validation. If not specify, the \code{resampling} function from the package \code{caret} is used for a 10-folds cross-validation.
                      #' @param resampling_arg `list` \cr
                      #' List of arguments to be passed to the function.
                      #' @param seed `integer` \cr
                      #' Random seed. Default is NULL, which generates the seed from \code{R}.
                      #'
                      #' @return
                      #' The current object is returned, with each learner trained on each layer.
                      #' @export
                      #'
                      train = function (ind_subset = NULL,
                                        use_var_sel = FALSE,
                                        resampling_method = NULL,
                                        resampling_arg = list(),
                                        seed = NULL) {
                        if (!is.logical(use_var_sel)) {
                          stop("'use_var_sel' must be boolean.")
                        } else {
                          private$use_var_sel = use_var_sel
                        }
                        # Test that the training object contains overlapping individuals
                        if (!self$testOverlap()) {
                          stop("This Training object does not contain overlapping individuals.") #nocov
                        }
                        # Check wether all individuals have layer-data
                        layer_ids = self$getIndIDs()[ , 1L]
                        target_df = self$getTargetObj()$getData()
                        ids_in_target = target_df[ , private$ind_col]
                        ids_not_in_target = setdiff(ids_in_target, layer_ids)
                        if (length(ids_not_in_target)) {
                          if (length(ids_not_in_target) == nrow(target_df)) {
                            stop("There is no target individual(s) with modality observations.\n")
                          } else {
                            warning(sprintf("%d individual(s) with target value(s), but no modality observations identified and excluded.\n", length(ids_not_in_target)))
                          }
                          # Remove those individuals in target without modality data.
                          clean_target_df = target_df[ids_in_target %in% layer_ids, ]
                          self$getTargetObj()$setData(clean_target_df)
                        }
                        # 1) Create meta training data
                        if (is.null(resampling_method)) {
                          resampling_method = "caret::createFolds"
                          resampling_arg = list(y = self$getTargetValues()[ , self$getTarget()],
                                                k = 10L)
                        }
                        if (!is.null(seed)) {
                          set.seed(seed = seed)
                        }
                        self$createMetaTrainData(resampling_method,
                                                 resampling_arg,
                                                 use_var_sel = use_var_sel,
                                                 impute = private$impute)
                        cat("\n")
                        # 2) Train each layer
                        self$trainLayer(ind_subset = ind_subset,
                                        use_var_sel = use_var_sel,
                                        verbose = self$getVerbose())
                        # 3) Train the meta layer
                        # Add layer specific predictions to meta training layer
                        layers = self$getKeyClass()
                        meta_layer_key = layers[layers$class == "TrainMetaLayer" , "key"]
                        meta_layer = self$getFromHashTable(key = meta_layer_key)
                        meta_layer$train(ind_subset = ind_subset)
                        return(self)
                      },
                      #' @description
                      #' Compute predictions for a testing object.
                      #'
                      #' @param testing `Testing` \cr
                      #' A new testing object to be predicted.
                      #' @param ind_subset `vector` \cr
                      #' Vector of IDs to be predicted.
                      #'
                      #' @return
                      #' The predicted object. All layers and the meta layer are predicted. This is the final predicted object.
                      #' @export
                      #'
                      # Our predictions based on cross-validation are different from that coming from the original learning method; e.g. that coming from ranger.
                      predict = function (testing,
                                          ind_subset = NULL) {
                        # 0) Check consistency between training and testing layers
                        layer_training = self$getKeyClass()
                        layer_training_key = layer_training[layer_training$class == "TrainLayer", "key"]
                        layer_testing = testing$getKeyClass()
                        layer_testing_key = layer_testing[layer_testing$class == "TestLayer", "key"]
                        name_inter = intersect(layer_training_key, layer_testing_key)
                        testing_not_in = any(!(layer_testing_key %in% layer_training_key))
                        if (length(name_inter) != length(layer_training_key) | testing_not_in) {
                          stop("Inconsistent names identified between training and testing.")
                        }
                        # 1) Layer predictions
                        predicting = self$predictLayer(testing = testing,
                                                       ind_subset = ind_subset)
                        # 2) Meta layer predicted new data; resume layer specific
                        #    predictions and create a new data.
                        meta_layer_id = self$getTrainMetaLayer()$getId()
                        testing_meta_data = predicting$createMetaTestData(
                          meta_layer_id = meta_layer_id)
                        if (private$impute) {
                          # TODO: Can be moved into testMetaLayer; similarly to trainMetaLayer.
                          testing_meta_data$impute(impute_fct = NULL,
                                                   impute_param = NULL,
                                                   target_name = self$getTargetObj()$getTargetName())
                        }
                        # 3) Predict new meta layer by the trained meta layer.
                        layers = self$getKeyClass()
                        meta_layer_key = layers[layers$class == "TrainMetaLayer", "key"]
                        meta_layer = self$getFromHashTable(key = meta_layer_key)
                        # TODO: getTestLayer maybe rename it getLayer?
                        predicted_layer = meta_layer$predict(new_layer = testing_meta_data$getTestLayer(),
                                                             ind_subset = ind_subset)
                        # Store final meta predicted values on meta layer.
                        predicting$removeFromHashTable(key = predicted_layer$getId())
                        predicting$add2HashTable(key = predicted_layer$getId(),
                                                 value = predicted_layer,
                                                 .class = "PredictData")
                        # Reshape format of meta-predictions as those of layer-spec predictions
                        predicted_meta_values = predicted_layer$getPredictData()$getDataFrame()
                        predicted_meta_values_wide = reshape(predicted_layer$getPredictData()$getDataFrame(),
                                                             idvar = colnames(predicted_meta_values)[2L],
                                                             timevar = colnames(predicted_meta_values)[1L],
                                                             direction = "wide")
                        colname_vector = gsub(pattern = "Prediction.",
                                              replacement = "",
                                              x = names(predicted_meta_values_wide))
                        names(predicted_meta_values_wide) = colname_vector
                        predicted_values_wide = merge(x = testing_meta_data$getDataFrame(),
                                                      y = predicted_meta_values_wide,
                                                      by = self$getIndCol(),
                                                      all.y = TRUE)
                        return(list(predicting = predicting,
                                    predicted_values = predicted_values_wide))
                      },
                      #' @description
                      #' Variable selection on the current training object.
                      #'
                      #' @param ind_subset `vector` \cr
                      #' ID subset of individuals to be used for variable selection.
                      #' @param verbose `boolean` \cr
                      #' Warning messages will be displayed if set to TRUE.
                      #'
                      #' @return
                      #' The current layer is returned with the resulting model.
                      #' @export
                      #'
                      varSelection = function (ind_subset = NULL,
                                               verbose = TRUE) {
                        # Check wether all individuals have layer-data
                        layer_ids = self$getIndIDs()[ , 1L]
                        target_df = self$getTargetObj()$getData()
                        ids_in_target = target_df[ , private$ind_col]
                        ids_not_in_target = setdiff(ids_in_target, layer_ids)
                        if (length(ids_not_in_target)) {
                          if (length(ids_not_in_target) == nrow(target_df)) {
                            stop("There is no target individual(s) with modality observations.\n")
                          } else {
                            warning(sprintf("%d individual(s) with target value(s), but no modality observations identified and excluded.\n", length(ids_not_in_target)))
                          }
                          # Remove those individuals in target without modality data.
                          clean_target_df = target_df[ids_in_target %in% layer_ids, ]
                          self$getTargetObj()$setData(clean_target_df)
                        }
                        layers = self$getKeyClass()
                        nb_layers = nrow(layers[layers$class %in% "TrainLayer", ])
                        if (nb_layers) {
                          # This code accesses each layer (except MetaLayer) level and
                          # perform variable selection.
                          layers = layers[layers$class %in% "TrainLayer", ]
                          selected = NULL
                          for (k in layers$key) {
                            layer = self$getFromHashTable(key = k)
                            layer_var_sel = layer$varSelection(ind_subset = ind_subset,
                                                               verbose = self$getVerbose())
                            if (length(layer_var_sel)) {
                              # At least one variable selected
                              selected = rbind(selected,
                                               data.frame(Layer = layer$getId(),
                                                          variable = layer_var_sel))
                            }
                          }
                          private$var_sel_done = TRUE
                        } else {
                          stop("No existing layer in the current training object.")
                        }
                        return(selected)
                      },
                      #' @description
                      #' Gather target values from all layer.
                      #'
                      #' @return
                      #' A \code{data.frame} containing individuals IDs and corresponding target values.
                      #' @export
                      #'
                      getTargetValues = function() {
                        layers = self$getKeyClass()
                        # This code accesses each layer (except TrainMetaLayer) level
                        # and get the target variable
                        layers = layers[layers$class %in% "TrainLayer", ]
                        target_data = NULL
                        train_data = NULL
                        for (k in layers$key) {
                          layer = self$getFromHashTable(key = k)
                          target_data = as.data.frame(rbind(target_data,
                                                            layer$getTargetValues()))
                          train_data = layer$getTrainData()
                        }
                        target_data = target_data[!duplicated(target_data[ , train_data$getIndCol()]), ]
                        return(target_data)
                      },
                      #' @description
                      #' Gather individual IDs from all layer.
                      #'
                      #' @return
                      #' A \code{data.frame} containing individuals IDs.
                      #'
                      getIndIDs = function() {
                        layers = self$getKeyClass()
                        # This code accesses each layer (except TrainMetaLayer) level
                        # and get the individual IDs.
                        layers = layers[layers$class %in% "TrainLayer", ]
                        ids_data = NULL
                        current_data = NULL
                        for (k in layers$key) {
                          layer = self$getFromHashTable(key = k)
                          ids_data = as.data.frame(rbind(ids_data,
                                                         layer$getIndIDs()))
                        }
                        ids_data = ids_data[!duplicated(ids_data[ , 1L]), ,
                                            drop = FALSE]
                        return(ids_data)
                      },
                      #' @description
                      #' Get a layer of a given ID.
                      #'
                      #' @param id `character` \cr
                      #' The ID of the layer to be returned.
                      #'
                      #' @return
                      #' The [TrainLayer] object is returned for the given ID.
                      #' @export
                      #'
                      getLayer = function(id) {
                        layer = self$getFromHashTable(key = id)
                        return(layer)
                      },
                      #' @description
                      #' Getter of the meta layer.
                      #'
                      #' @return
                      #' Object from class [TrainMetaLayer]
                      #' @export
                      #'
                      getTrainMetaLayer = function () {
                        layers = self$getKeyClass()
                        meta_layer_key = layers[layers$class == "TrainMetaLayer", "key"]
                        meta_layer = self$getFromHashTable(key = meta_layer_key)
                        return(meta_layer)
                      },
                      #' @description
                      #' Retrieve models from all layer.
                      #'
                      #' @return
                      #' A \code{list} containing all (base and meta) models.
                      #' @export
                      #'
                      getModel = function() {
                        layers = self$getKeyClass()
                        # This code accesses each layer (except TrainMetaLayer) level
                        # and get the individual IDs.
                        layers = layers[layers$class %in% c("TrainLayer", "TrainMetaLayer"), ]
                        models = list()
                        for (k in layers$key) {
                          layer = self$getFromHashTable(key = k)
                          models[[layer$getId()]] = layer$getModel()$getBaseModel()
                        }
                        return(models)
                      },
                      #' @description
                      #' Retrieve modality-specific predictions.
                      #'
                      #' @return
                      #' A \code{list} containing all (base and meta) models.
                      #' @export
                      #'
                      getData = function() {
                        layers = self$getKeyClass()
                        layers = layers[layers$class %in% c("TrainLayer", "TrainMetaLayer"), ]
                        all_data = list()
                        for (k in layers$key) {
                          layer = self$getFromHashTable(key = k)
                          all_data[[layer$getId()]] = layer$getTrainData()$getDataFrame()
                        }
                        return(all_data)
                      },
                      #' @description
                      #' Remove a layer of a given ID.
                      #'
                      #' @param id `character` \cr
                      #' The ID of the layer to be removed.
                      #'
                      #' @return
                      #' The [TrainLayer] object is returned for the given ID.
                      #'
                      removeLayer = function(id) {
                        if (private$nb_trained_layer > 0L) {
                          private$nb_trained_layer = private$nb_trained_layer - 1L
                          warning(sprintf("%s was already trained. Do not forget to train it again to update its meta layer.", private$id))
                        }
                        self$removeFromHashTable(key = id)
                        invisible(TRUE)
                      },
                      #' @description
                      #' Remove the meta layer from the current [Training] object.
                      #'
                      removeTrainMetaLayer = function() {
                        layers = self$getKeyClass()
                        meta_layer_key = layers[layers$class == "TrainMetaLayer", "key"]
                        self$removeFromHashTable(key = meta_layer_key)
                        invisible(TRUE)
                      },
                      #' @description
                      #' Getter of the individual column name.
                      getIndCol = function () {
                        return(private$ind_col)
                      },
                      #' @description
                      #' Getter of the target variable name.
                      getTarget = function () {
                        return(private$target)
                      },
                      #' @description
                      #' Getter of the verbose setting.
                      getVerbose = function () {
                        return(private$verbose)
                      },
                      #' @description
                      #' Getter of the use_var_sel field.
                      getUseVarSel = function () {
                        return(private$use_var_sel)
                      },
                      #' @description
                      #' Getter of the use_var_sel field.
                      getVarSelDone = function () {
                        return(private$var_sel_done)
                      },
                      #' @description
                      #' Increase the number of trained layer.
                      increaseNbTrainedLayer = function () {
                        private$nb_trained_layer = private$nb_trained_layer + 1L
                        if (private$nb_trained_layer == length(private$hash_table) - 1L) {
                          private$status = TRUE
                        }
                      },
                      #' @description
                      #' Check whether a target object has already been stored.
                      #'
                      #' @return
                      #' Boolean value
                      #'
                      checkTargetExist = function () {
                        return(super$checkClassExist(.class = "Target"))
                      },
                      #' @description
                      #' Getter of the target object.
                      # TODO: Maybe rename this function getTarget and find another appropriate name for the current getTarget function.
                      getTargetObj = function () {
                        return(private$target_obj)
                      },
                      #' @description
                      #' Getter of the problem type.
                      getProblemTyp = function () {
                        return(private$problem_type)
                      },
                      #' @description
                      #' Set imputation action na.action.
                      #'
                      #' @param impute `character` \cr
                      #' How to handle missing values.
                      #'
                      setImpute = function(impute) {
                        private$impute = impute
                        invisible(self)
                      },
                      #' @description
                      #' Test that individuals overlap over layers.
                      #' At least five individuals must overlapped.
                      #'
                      testOverlap = function () {
                        layers = self$getKeyClass()
                        if (nrow(layers) == 1L) {
                          stop ("No layer found in this training object.")
                        }
                        # This code accesses each layer (except TrainMetaLayer) level
                        # and get the individual IDs.
                        layers = layers[layers$class %in% "TrainLayer", ]
                        ids_data = NULL
                        current_data = NULL
                        # TRUE if only one training layer
                        if (length(layers$key) == 1) {
                          return(TRUE)
                        }
                        for (k in layers$key) {
                          layer = self$getFromHashTable(key = k)
                          ids_data = as.data.frame(rbind(ids_data,
                                                         layer$getIndIDs()))
                        }
                        if (sum(duplicated(ids_data[ , 1L])) > 5L) {
                          return(TRUE)
                        } else {
                          return(FALSE) # nocov
                        }
                      },
                      #' @description
                      #' UpSet plot to show an overview of the overlap of individuals across various layers.
                      #'
                      #' @param ... `any` \cr
                      #' Further parameters to be passed to the \code{upset} function from package \code{UpSetR}.
                      #'
                      #' @export
                      #'
                      upset = function (...) {
                        layers = self$getKeyClass()
                        # This code accesses each layer (except TrainMetaLayer) level
                        # and get the individual IDs.
                        layers = layers[layers$class %in% "TrainLayer", ]
                        if (!nrow(layers) | (nrow(layers) == 1L)) {
                          stop("No or only one available layer in this training object.")
                        }
                        ids_list = lapply(layers$key, function (k) {
                          layer = self$getFromHashTable(key = k)
                          return(layer$getIndIDs()[ , 1L])
                        })
                        param_upset = list(...)
                        from_list_ids = do.call(eval(parse(text = "UpSetR::fromList")),
                                                list(input = ids_list))
                        names(from_list_ids) = layers$key
                        param_upset$data = from_list_ids
                        print(do.call(eval(parse(text = "UpSetR::upset")),
                                      param_upset))
                        invisible(TRUE)
                      },
                      #' @description
                      #' Generate training summary
                      #'
                      #' @export
                      #'
                      summary = function () {
                        cat(sprintf("Training %s\n", self$getId()))
                        cat("----------------\n")
                        self$print()
                        cat("----------------\n")
                        cat("\n")
                        layers = self$getKeyClass()
                        layers = layers[layers$class %in% c("TrainLayer", "TrainMetaLayer"), ]
                        for (k in layers$key) {
                          layer = self$getFromHashTable(key = k)
                          layer$summary()
                          cat("\n")
                        }
                        invisible(self)
                      }
                    ),
                    private = list(
                      ind_col = character(0L),
                      target = character(0L),
                      target_obj = NULL,
                      nb_trained_layer = 0L,
                      impute = FALSE,
                      verbose = TRUE,
                      problem_type = "classification",
                      status = FALSE,
                      use_var_sel = FALSE,
                      var_sel_done = FALSE
                    ),
                    cloneable = FALSE
)
