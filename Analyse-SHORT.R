#########################
#     MISE EN ROUTE     #
#########################
library(tidyverse)
library(caret)

URL <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00615/MushroomDataset.zip"    # UCI archive
#URL <- "https://github.com/EKRihani/mushrooms/raw/master/MushroomDataset.zip"      # Alternative URL

datafile <- tempfile()
download.file(URL, datafile)
datafile <- unzip(datafile, "MushroomDataset/secondary_data.csv")
dataset <- read.csv(datafile, header = TRUE, sep = ";")

#####################################
#     CREATION LOTS DE DONNEEES     #
#####################################

# Créer lots entrainement/validation (30%) vs evaluation (70%)  (intial : 90/10, p=.1)
test_index <- createDataPartition(y = dataset$cap.diameter, times = 1, p = 0.7, list = FALSE)
trainvalid_set <- dataset[-test_index,]
evaluation_set <- dataset[test_index,]

# Créer lots entrainement (90%) vs validation (10%) sets
test_index <- createDataPartition(y = trainvalid_set$cap.diameter, times = 1, p = 0.1, list = FALSE)
training_set <- trainvalid_set[-test_index,]
validation_set <- trainvalid_set[test_index,]

###############################################
#     ANALYSE LOT ENTRAINEMENT AVEC CARET     #
###############################################
# https://topepo.github.io/caret/available-models.html
# names(getModelInfo())
# getModelInfo(Rborist)

# Fonction : lance modèle avec ces paramètres parameters, évalue la performance, renvoie résultats
fit_test <- function(fcn_model){
   tr_ctrl <- trainControl(classProbs = TRUE, summaryFunction = twoClassSummary, method = "cv", number = 20)
   cmd <- paste0("train(class ~ ., method = '",
                 fcn_model[1], 
                 "', data = trainvalid_set, trControl = tr_ctrl, ", 
                 fcn_model[2],")")
   eval(parse(text = cmd))
}

# TEST PARALLELE (temps divisé par deux/trois)
library(doParallel)
cl <- makePSOCKcluster(5)
registerDoParallel(cl)
set_gamLoess_span <-  c("gamLoess", "tuneGrid  = data.frame(span = seq(from = 0.01, to = 1, by = 0.24), degree = 1)")
Ping <- Sys.time()
fit_gamLoess_span <- fit_test(set_gamLoess_span)
Pong <- Sys.time()
stopCluster(cl)
Pong - Ping

# Generalized Additive Model
set_gamLoess_span <-  c("gamLoess", "tuneGrid  = data.frame(span = seq(from = 0.01, to = 1, by = 0.24), degree = 1)")
set_gamLoess_degree <-  c("gamLoess", "tuneGrid  = data.frame(degree = c(0, 1), span = 0.5)")
fit_gamLoess_span <- fit_test(set_gamLoess_span)
fit_gamLoess_degree <- fit_test(set_gamLoess_degree)

fit_gamLoess_span_plot <- ggplot(fit_gamLoess_span)
fit_gamLoess_span_results <- fit_gamLoess_span$results
fit_gamLoess_degree_plot <- ggplot(fit_gamLoess_degree)
fit_gamLoess_degree_results <- fit_gamLoess_degree$results

# Random Forest
set_ranger_mtry <- c("ranger", "tuneGrid  = data.frame(mtry = seq(from = 1, to = 106, by = 15), splitrule = 'extratrees', min.node.size = 2), num.trees = 6")
set_ranger_splitrule <- c("ranger", "tuneGrid  = data.frame(splitrule = c('gini', 'extratrees'), mtry = 50, min.node.size = 2), num.trees = 6")
set_ranger_nodesize <- c("ranger", "tuneGrid  = data.frame(min.node.size = seq(from = 1, to = 15, by = 2), mtry = 50, splitrule = 'extratrees'), num.trees = 6")
set_Rborist_pred <- c("Rborist", "tuneGrid  = data.frame(predFixed = seq(from = 1, to = 41, by = 5), minNode = 2), ntrees = 3")
set_Rborist_minNode <- c("Rborist", "tuneGrid  = data.frame(minNode = 1:5, predFixed =50), ntrees = 3")
fit_ranger_mtry <- fit_test(set_ranger_mtry)
fit_ranger_splitrule <- fit_test(set_ranger_splitrule)
fit_ranger_nodesize <- fit_test(set_ranger_nodesize)
fit_Rborist_pred <- fit_test(set_Rborist_pred)
fit_Rborist_minNode <- fit_test(set_Rborist_minNode)

fit_ranger_mtry_plot <- ggplot(fit_ranger_mtry)
fit_ranger_mtry_results <- fit_ranger_mtry$results
fit_ranger_mtry_bestTune <- fit_ranger_mtry$bestTune
fit_ranger_splitrule_plot <- ggplot(fit_ranger_splitrule)
fit_ranger_splitrule_results <- fit_ranger_splitrule$results
fit_ranger_splitrule_bestTune <- fit_ranger_splitrule$bestTune
fit_ranger_nodesize_plot <- ggplot(fit_ranger_nodesize)
fit_ranger_nodesize_results <- fit_ranger_nodesize$results
fit_ranger_nodesize_bestTune <- fit_ranger_nodesize$bestTune
fit_Rborist_pred_plot <- ggplot(fit_Rborist_pred)
fit_Rborist_pred_results <- fit_Rborist_pred$results
fit_Rborist_pred_bestTune <- fit_Rborist_pred$bestTune
fit_Rborist_minNode_plot <- ggplot(fit_Rborist_minNode)
fit_Rborist_minNode_results <- fit_Rborist_minNode$results
fit_Rborist_minNode_bestTune <- fit_Rborist_minNode$bestTune


# Ranger optimal
set_ranger_best <- c("ranger", paste0("tuneGrid  = data.frame(min.node.size = ", 
                                            fit_ranger_nodesize_bestTune$min.node.size, 
                                            ", splitrule = '", fit_ranger_splitrule_bestTune$splitrule,
                                            "', mtry = ", fit_ranger_splitrule_bestTune$mtry, ")", 
                                            ", num.trees = 10"))
fit_ranger_best <- fit_test(set_ranger_best)
fit_ranger_best_results <- fit_ranger_best$results
# Rborist optimal
set_Rborist_best <- c("Rborist", paste0("tuneGrid  = data.frame(predFixed = 6, ",    # Value is forced, 6 gives a Spec = 1, and a much better sensitivity
                                        "minNode = ", fit_Rborist_minNode_bestTune$minNode, ")",
                                        ", ntrees = 3"))
fit_Rborist_best <- fit_test(set_Rborist_best)
fit_Rborist_best_results <- fit_Rborist_best$results


# Test complet des combinaisons de facteurs (LEEENT)
# set_ranger <- c("ranger", "tuneGrid = expand.grid(mtry = seq(from = 1, to = 21, by = 5),
#                                                 splitrule = c('gini', 'extratrees'),
#                                                 min.node.size = seq(from = 1, to = 16, by = 5)
#                 )" )
# fit_ranger <- fit_test(set_ranger)
# plot(fit_ranger, metric = "Spec", plotType = "level", scales = list(x = list(rot = 90)))
# plot(fit_ranger, metric = "Spec")
# ggplot(fit_ranger)


