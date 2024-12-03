library(InterSIM)
library(this.path)
set.seed(3252)
prop <- c(0.5, 0.5)
effect <- 0.5
source(file.path(dirname(this.path::this.path()), "myInterSIM.R"))
train_sim_data <- myInterSIM(n.sample = 100,
                             cluster.sample.prop = prop,
                             delta.methyl = effect,
                             delta.expr = effect,
                             delta.protein = 0L,
                             p.DMP = 0.2,
                             p.DEG = NULL,
                             p.DEP = NULL,
                             sigma.methyl = NULL,
                             sigma.expr = NULL,
                             sigma.protein = NULL,
                             cor.methyl.expr = NULL,
                             cor.expr.protein = NULL,
                             do.plot = FALSE,
                             sample.cluster = TRUE,
                             feature.cluster = TRUE)

disease <- factor(as.integer(as.integer(train_sim_data$clustering.assignment$cluster.id)  == 2))

# Target data.frame
target_df <- train_sim_data$clustering.assignment
target_df$cluster.id <- as.integer(target_df$cluster.id == 2)
names(target_df) <- c("IDS", "disease")
target_df$IDS <- gsub(pattern = "subject",
                      replacement = "participant",
                      x = target_df$IDS)

index_split <- caret::createDataPartition(y = disease, p = 0.7)
index_split$Fold1 <- index_split$Resample1
index_split$Fold2 <- setdiff(1L:length(disease), index_split$Fold1)
train_disease <- disease[index_split$Fold1]

target_df_train <- target_df[index_split$Fold1, ]
target_df_test <- target_df[index_split$Fold2, ]


# Train disease
set.seed(789)
not_missing1 <- sample(x = 1L:length(index_split$Fold1), size = 50L, replace = FALSE)
subject_ids <- rownames(train_sim_data$dat.expr[index_split$Fold1, ])
geneexpr_train_data <- as.data.frame(train_sim_data$dat.expr[index_split$Fold1[not_missing1], ])
geneexpr_train_data$IDS <- subject_ids[not_missing1]
geneexpr_train_data$IDS <- gsub(pattern = "subject",
                      replacement = "participant",
                      x = geneexpr_train_data$IDS)
# geneexpr_train_data$disease <- train_disease[not_missing1]
geneexpr_train_data$disease <- NULL

# Sample not missing
set.seed(7895)
not_missing2 <- sample(x = 1L:length(index_split$Fold1), size = 50L, replace = FALSE)
proteinexpr_train_data <- as.data.frame(train_sim_data$dat.protein[index_split$Fold1[not_missing2], ])
proteinexpr_train_data$IDS <- subject_ids[not_missing2]
proteinexpr_train_data$IDS <- gsub(pattern = "subject",
                                replacement = "participant",
                                x = proteinexpr_train_data$IDS)
proteinexpr_train_data$disease <- train_disease[not_missing2]
proteinexpr_train_data$disease <- NULL

set.seed(79)
not_missing3 <- sample(x = 1L:length(index_split$Fold1), size = 50L, replace = FALSE)
methylation_train_data <- as.data.frame(train_sim_data$dat.methyl[index_split$Fold1[not_missing3], ])
methylation_train_data$IDS <- subject_ids[not_missing3]
methylation_train_data$IDS <- gsub(pattern = "subject",
                                   replacement = "participant",
                                   x = methylation_train_data$IDS)
methylation_train_data$disease <- train_disease[not_missing3]
methylation_train_data$disease <- NULL

train_entities <- list("geneexpr" = geneexpr_train_data,
                       "proteinexpr" = proteinexpr_train_data,
                       "methylation" = methylation_train_data,
                       "target" = target_df_train)

# Test disease
test_disease <- disease[index_split$Fold2]

test_not_missing1 <- sample(x = 1L:length(index_split$Fold2), size = 20L, replace = FALSE)
subject_ids <- rownames(train_sim_data$dat.expr[index_split$Fold2, ])
geneexpr_test_data <- as.data.frame(train_sim_data$dat.expr[index_split$Fold2[test_not_missing1], ])
geneexpr_test_data$IDS <- subject_ids[test_not_missing1]
geneexpr_test_data$IDS <- gsub(pattern = "subject",
                                   replacement = "participant",
                                   x = geneexpr_test_data$IDS)
# geneexpr_test_data$disease <- test_disease[test_not_missing1]
geneexpr_test_data$disease <- NULL

test_not_missing2 <- sample(x = 1L:length(index_split$Fold2), size = 20L, replace = FALSE)
proteinexpr_test_data <- as.data.frame(train_sim_data$dat.protein[index_split$Fold2[test_not_missing2], ])
proteinexpr_test_data$IDS <- subject_ids[test_not_missing2]
proteinexpr_test_data$IDS <- gsub(pattern = "subject",
                               replacement = "participant",
                               x = proteinexpr_test_data$IDS)
# proteinexpr_test_data$disease <- test_disease[test_not_missing2]
proteinexpr_test_data$disease <- NULL

test_not_missing3 <- sample(x = 1L:length(index_split$Fold2), size = 20L, replace = FALSE)
methylation_test_data <- as.data.frame(train_sim_data$dat.methyl[index_split$Fold2[test_not_missing3], ])
methylation_test_data$IDS <- subject_ids[test_not_missing3]
methylation_test_data$IDS <- gsub(pattern = "subject",
                                  replacement = "participant",
                                  x = methylation_test_data$IDS)
# methylation_test_data$disease <- test_disease[test_not_missing3]
methylation_test_data$disease <- NULL

test_entities <- list("geneexpr" = geneexpr_test_data,
                      "proteinexpr" = proteinexpr_test_data,
                      "methylation" = methylation_test_data,
                      "target" = target_df_test)
train_entities$target$disease <- factor(train_entities$target$disease)
test_entities$target$disease <- factor(test_entities$target$disease)
entities = list(training = train_entities, testing = test_entities)
multi_omics <- entities
save(multi_omics, file = file.path(dirname(dirname(this.path::this.path())), "data/multi_omics.rda"))
# Compress this large dataset.
tools::resaveRdaFiles(paths = file.path(dirname(dirname(this.path::this.path())), "data/multi_omics.rda"))

