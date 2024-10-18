# Training
training <- Training$new(id = "training",
                              ind_col = "IDS",
                              target = "disease",
                              target_df = entities$training$target)
# Training layers
tl_geneexpr <- TrainLayer$new(id = "geneexpr",
                              training = training)

# Parameters
ranger_param_lrner <- ParamLrner$new(id = "ParamRanger",
                                     param_list = list(seed = 421L),
                                     hyperparam_list = list(probability = TRUE,
                                                            mtry = 1L,
                                                            num.trees = 1000L))

# Learner

expect_error({
  lrner_geneexpr <- Lrner$new(id = "ranger",
                              package = "ranger",
                              lrn_fct = "ranger",
                              param = ranger_param_lrner,
                              train_layer = "not_a_train_layer")
})

expect_error({
  lrner_geneexpr <- Lrner$new(id = "ranger",
                              package = "ranger",
                              lrn_fct = "ranger",
                              param = ranger_param_lrner,
                              na_rm = "not_logical",
                              train_layer = tl_geneexpr)
})

lrner_geneexpr <- Lrner$new(id = "ranger",
                            package = "ranger",
                            lrn_fct = "ranger",
                            param = ranger_param_lrner,
                            train_layer = tl_geneexpr)

expect_no_error({
  print(lrner_geneexpr)
  # Update
  lrner_geneexpr <- Lrner$new(id = "ranger",
                              package = "ranger",
                              lrn_fct = "ranger",
                              param = ranger_param_lrner,
                              train_layer = tl_geneexpr)
})

# Instantiate
test_that("Lrner instantiates correctly", {
  expect_true(R6::is.R6(lrner_geneexpr))
  expect_equal(class(lrner_geneexpr)[1], "Lrner")
})

# Training without training data does not work.
test_that("Lrner cannot be trained without a training data", {
  expect_error(lrner_geneexpr$train())
})

