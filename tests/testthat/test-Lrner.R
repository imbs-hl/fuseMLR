# Training study
train_study <- TrainStudy$new(id = "train_study",
                              ind_col = "IDS",
                              target = "disease")
# Training layers
tl_geneexpr <- TrainLayer$new(id = "geneexpr",
                              train_study = train_study)

# Parameters
ranger_param_lrner <- ParamLrner$new(id = "ParamRanger",
                                     param_list = list(seed = 421L),
                                     hyperparam_list = list(probability = TRUE,
                                                            mtry = 1L,
                                                            num.trees = 1000L))

# Learner
lrner_geneexpr <- Lrner$new(id = "ranger",
                            package = "ranger",
                            lrn_fct = "ranger",
                            param = ranger_param_lrner,
                            train_layer = tl_geneexpr)

# Instantiate
test_that("Lrner instantiates correctly", {
  expect_true(R6::is.R6(lrner_geneexpr))
  expect_equal(class(lrner_geneexpr)[1], "Lrner")
})

# Training without training data does not work.
test_that("Lrner cannot be trained without a training data", {
  expect_error(lrner_geneexpr$train())
})
