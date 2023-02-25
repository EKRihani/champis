##########################
#     INITIALISATION     #
##########################

# Chargement des bibliothèques
library(tidyverse)    # Outils génériques
library(GGally)       # Outils supplémentaires pour les graphiques
library(caret)        # Outils d'apprentissage machine
library(DiceDesign)    # Hypercubes Latins
library(DiceEval)       # Modélisation sur hypercubes latins

# Récupération, décompression, importation des données
fichier_data <- tempfile()
#URL <- "https://github.com/EKRihani/mushrooms/raw/master/MushroomDataset.zip"      # URL de mon repo
# download.file(URL, fichier_data)
fichier_data <- "~/projects/champis/MushroomDataset.zip" # FICHIER LOCAL

fichier_data <- unzip(fichier_data, "MushroomDataset/secondary_data.csv")
dataset <- read.csv(fichier_data, header = TRUE, sep = ";", stringsAsFactors = TRUE)

##############################################################################
#     CREATION DES LOTS D'ENTRAINEMENT, VALIDATION, EVALUATION + GRAPHES     #
##############################################################################

split1 <- 0.08
split2 <- 0.08
# Creation lots d'entrainement/validation (92%) et evaluation (8%)
set.seed(007)
index1 <- createDataPartition(y = dataset$cap.diameter, times = 1, p = split1, list = FALSE)
lot_appr_opti <- dataset[-index1,]
lot_evaluation <- dataset[index1,]

# Creation lots d'entrainement (92%) et validation (8%)
set.seed(1337)
index2 <- createDataPartition(y = lot_appr_opti$cap.diameter, times = 1, p = split2, list = FALSE)
lot_apprentissage <- lot_appr_opti[-index2,]
lot_evaluation <- lot_appr_opti[index2,]



###############################################################
#     ANALYSE DU LOT D'ENTRAINEMENT AVEC MODELES DE CARET     #
###############################################################
# https://topepo.github.io/caret/available-models.html
# names(getModelInfo())
# getModelInfo(Rborist)

n_folds <- 5
# Définition de fonction : lance le modèle avec les paramètres données, évalue la performance (spécificité), renvoie les résultats de fitting
fit_test <- function(fcn_model){
   set.seed(1)
   tr_ctrl <- trainControl(classProbs = TRUE, summaryFunction = twoClassSummary, method = "cv", number = n_folds)   # Règle paramètres d'évaluation performance à twoClassSummary (ROC, Sens, Spec), avec cross-validation (10-fold)
   cmd <- paste0("train(class ~ ., method = '",      # Construit commande, évaluation de performance par Spécificité
                 fcn_model[1], 
                 "', data = lot_appr_opti, trControl = tr_ctrl, metric = 'Spec', ", 
                 fcn_model[2],")")
   fitting <- eval(parse(text = cmd))        # Lance commande
   fitting
}


# Modèles types arbres (RPART, RPARTCOST, CTREE, C50TREE)

grid_rpart_cp <- data.frame(cp = 10^seq(from = -5, to = -1, by = .5))

LHS <- nolhDesign(dimension = 2, range = c(0, 1))$design     # Hypercube latin quasi-orthogonal
grid_rpartcost <- data.frame(LHS)
colnames(grid_rpartcost) <- c("cp", "Cost")
grid_rpartcost$cp <- (grid_rpartcost$cp*1e-2+1e-5)
grid_rpartcost$Cost <- grid_rpartcost$Cost*2.5+1e-3

set_rpart_cp <- c("rpart", "tuneGrid  = grid_rpart_cp")
set_rpartcost <- c("rpartCost", "tuneGrid  = grid_rpartcost")     # A TESTER !!!

set_ctree_criterion <- c("ctree", "tuneGrid  = data.frame(mincriterion = c(0.01, 0.25, 0.5, 0.75, 0.99))")
set_c50tree <- c("C5.0Tree", "")
system.time(fit_test(set_rpart_cp))    ####### CHRONO
fit_rpart_cp <- fit_test(set_rpart_cp)
fit_rpartcost <- fit_test(set_rpartcost)
fit_ctree_criterion <- fit_test(set_ctree_criterion)
fit_c50tree <- fit_test(set_c50tree)

# Extraire résultats d'intérêt : graphes et resultats
fit_rpart_cp_results <- fit_rpart_cp$results
fit_rpart_cp_graphe <- ggplot(data = fit_rpart_cp$results, aes(x = cp, y = Spec)) + geom_point() + ylab("Spécificité") + scale_x_log10()
fit_rpartcost_results <- fit_rpartcost$results

mod_rpartcost_spec <- modelFit(X=fit_rpartcost_results[,1:2], Y=fit_rpartcost_results$Spec,  type="Kriging", formula=Y~cp+Cost+cp:Cost+I(cp^2)+I(Cost^2))
mod_rpartcost_sens <-  modelFit(X=fit_rpartcost_results[,1:2], Y=fit_rpartcost_results$Sens,  type="Kriging", formula=Y~cp+Cost+cp:Cost+I(cp^2)+I(Cost^2))
pred_rpartcost <- expand.grid(fit_rpartcost_results[,1:2])
colnames(pred_rpartcost) <- c("Cost", "cp")
pred_rpartcost2 <- NULL
pred_rpartcost2$Spec <- modelPredict(mod_rpartcost_spec, pred_rpartcost)
pred_rpartcost2$Sens <- modelPredict(mod_rpartcost_sens, pred_rpartcost)
pred_rpartcost <- cbind(pred_rpartcost, pred_rpartcost2)
fit_rpartcost_spec_graphe <- ggplot() +
   geom_raster(data = pred_rpartcost, aes(x = Cost, y = cp, fill = Spec), interpolate = TRUE) +
   geom_tile(data = fit_rpartcost_results, aes(x = Cost, y = cp, fill = Spec), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "F", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))
fit_rpartcost_sens_graphe <- ggplot() +
   geom_raster(data = pred_rpartcost, aes(x = Cost, y = cp, fill = Sens), interpolate = TRUE) +
   geom_tile(data = fit_rpartcost_results, aes(x = Cost, y = cp, fill = Sens), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "G", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))

fit_ctree_criterion_graphe <- ggplot(fit_ctree_criterion)
fit_ctree_criterion_results <- fit_ctree_criterion$results
fit_c50tree_results <- fit_c50tree$results

# Meilleur modèle CART
set_rpartcost_best <- c("rpartCost", paste0("tuneGrid  = fit_rpartcost$bestTune"))
fit_rpartcost_best <- fit_test(set_rpartcost_best)
fit_rpartcost_best_results <- fit_rpartcost_best$results


# Modèles type Random Forest (RFERNS, RANGER, RBORIST)
set_rFerns_depth <- c("rFerns", "tuneGrid  = data.frame(depth = 2^(1:5)/2)")

grid_ranger <- data.frame(LHS)
grid_ranger <- rbind(grid_ranger,grid_ranger)
colnames(grid_ranger) <- c("mtry", "min.node.size")
#colnames(grid_ranger) <- c("mtry", "min.node.size, num.trees")     # Pour num.treees, à tester
grid_ranger$splitrule <- c(rep("extratrees", 17), rep("gini", 17))
grid_ranger$mtry <- round(1+grid_ranger$mtry*48,0)       # Si arrondi : prendre des multiples de 16 (car 17 points pour 2d)
grid_ranger$min.node.size <- round(1+grid_ranger$min.node.size*32,0)
#grid_ranger$num.trees <- round(1+grid_ranger$num.trees*9,0) # VOIR SI LE NUM.TREES MARCHE ???

grid_Rborist <- data.frame(LHS)
#colnames(grid_Rborist) <- c("predFixed", "minNode", "ntrees")
colnames(grid_Rborist) <- c("predFixed", "minNode")
grid_Rborist$predFixed <- round(1+grid_Rborist$predFixed*32,0)
grid_Rborist$minNode <- round(1+grid_Rborist$minNode*16,0)
#grid_Rborist$ntrees <- round(1+grid_Rborist$ntrees*5,0)

set_ranger <- c("ranger", "tuneGrid  = grid_ranger, num.trees = 6") # A TESTER
set_Rborist <- c("Rborist", "tuneGrid  = grid_Rborist")
#system.time(fit_test(set_ranger_mtry))  ####### CHRONO
fit_rFerns_depth <- fit_test(set_rFerns_depth)
fit_ranger <- fit_test(set_ranger)
fit_Rborist <- fit_test(set_Rborist)

# Extraire résultats d'intérêt : graphes et resultats
fit_rFerns_depth_graphe <- ggplot(fit_rFerns_depth)
fit_rFerns_depth_results <- fit_rFerns_depth$results
fit_ranger_results <- fit_ranger$results
fit_ranger_bestTune <- fit_ranger$bestTune
fit_Rborist_results <- fit_Rborist$results
fit_Rborist_bestTune <- fit_Rborist$bestTune

fit_ranger_GINI <- fit_ranger_results %>% filter (splitrule == "gini")
fit_ranger_ET <- fit_ranger_results %>% filter (splitrule == "extratrees")
mod_ranger_spec_GINI <- modelFit(X=fit_ranger_GINI[,1:2], Y=fit_ranger_GINI$Spec,  type="Kriging", formula=Y~mtry+min.node.size+mtry:min.node.size+I(mtry^2)+I(min.node.size^2))
mod_ranger_sens_GINI <- modelFit(X=fit_ranger_GINI[,1:2], Y=fit_ranger_GINI$Sens,  type="Kriging", formula=Y~mtry+min.node.size+mtry:min.node.size+I(mtry^2)+I(min.node.size^2))
mod_ranger_spec_ET <- modelFit(X=fit_ranger_ET[,1:2], Y=fit_ranger_ET$Spec,  type="Kriging", formula=Y~mtry+min.node.size+mtry:min.node.size+I(mtry^2)+I(min.node.size^2))
mod_ranger_sens_ET <- modelFit(X=fit_ranger_ET[,1:2], Y=fit_ranger_ET$Sens,  type="Kriging", formula=Y~mtry+min.node.size+mtry:min.node.size+I(mtry^2)+I(min.node.size^2))

pred_ranger_GINI <- expand.grid(fit_ranger_GINI[,1:2])
colnames(pred_ranger_GINI) <- c("mtry", "min.node.size")
pred_ranger_GINI2 <- NULL
pred_ranger_GINI2$Spec <- modelPredict(mod_ranger_spec_GINI, pred_ranger_GINI)
pred_ranger_GINI2$Sens <- modelPredict(mod_ranger_sens_GINI, pred_ranger_GINI)
pred_ranger_GINI <- cbind(pred_ranger_GINI, pred_ranger_GINI2)

pred_ranger_ET <- expand.grid(fit_ranger_ET[,1:2])
colnames(pred_ranger_ET) <- c("mtry", "min.node.size")
pred_ranger_ET2 <- NULL
pred_ranger_ET2$Spec <- modelPredict(mod_ranger_spec_ET, pred_ranger_ET)
pred_ranger_ET2$Sens <- modelPredict(mod_ranger_sens_ET, pred_ranger_ET)
pred_ranger_ET <- cbind(pred_ranger_ET, pred_ranger_ET2)

fit_ranger_Gini_spec_graphe <- ggplot() +
   geom_raster(data = pred_ranger_GINI, aes(x = mtry, y = min.node.size, fill = Spec), interpolate = TRUE) +
   geom_tile(data = fit_ranger_GINI, aes(x = mtry, y = min.node.size, fill = Spec), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "F", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))
fit_ranger_Gini_sens_graphe <- ggplot() +
   geom_raster(data = pred_ranger_GINI, aes(x = mtry, y = min.node.size, fill = Sens), interpolate = TRUE) +
   geom_tile(data = fit_ranger_GINI, aes(x = mtry, y = min.node.size, fill = Sens), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "G", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))

fit_ranger_ET_spec_graphe <- ggplot() +
   geom_raster(data = pred_ranger_ET, aes(x = mtry, y = min.node.size, fill = Spec), interpolate = TRUE) +
   geom_tile(data = fit_ranger_ET, aes(x = mtry, y = min.node.size, fill = Spec), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "F", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))
fit_ranger_ET_sens_graphe <- ggplot() +
   geom_raster(data = pred_ranger_ET, aes(x = mtry, y = min.node.size, fill = Sens), interpolate = TRUE) +
   geom_tile(data = fit_ranger_ET, aes(x = mtry, y = min.node.size, fill = Sens), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "G", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))


# Lance modèle RANGER optimal
set_ranger_best <- c("ranger", paste0("tuneGrid  = fit_ranger_bestTune, num.trees = 6"))
fit_ranger_best <- fit_test(set_ranger_best)
fit_ranger_best_results <- fit_ranger_best$results

# Lance modèle RBORIST optimal
set_Rborist_best <- c("Rborist", paste0("tuneGrid  = fit_Rborist_bestTune, ntrees = 2"))
fit_Rborist_best <- fit_test(set_Rborist_best)
fit_Rborist_best_results <- fit_Rborist_best$results


#########################################################
#     PERFORMANCE DES MODELES SUR LOT D'EVALUATION      #
#########################################################

# Règle la liste de prédiction et lance la classification
evaluation <- lot_evaluation
evaluation$reference <- as.logical(as.character(recode_factor(evaluation$class, e = TRUE, p = FALSE))) # Bascule en booléen

# Passe .$reference de booléen à facteur, puis calcule la matrice de confusion
evaluation$reference <- as.factor(evaluation$reference)


start_time <- Sys.time()     # Démarre chrono
cmd <- paste0("train(class ~ ., method = 'ranger', data = lot_appr_opti,", set_ranger_best[2], ")") # Construction de la commande
fit_ranger_final <- eval(parse(text = cmd))     # Exécution de la commande
pred_ranger_final <- predict(object = fit_ranger_final, newdata = lot_evaluation)
CM_ranger_final <- confusionMatrix(data = pred_ranger_final, reference = lot_evaluation$class)
results_ranger <- c(CM_ranger_final$byClass["Sensitivity"], CM_ranger_final$byClass["Specificity"], CM_ranger_final$byClass["F1"])
end_time <- Sys.time()     # Stop chrono
time_ranger <- difftime(end_time, start_time)
time_ranger <- time_ranger %>% as.numeric %>% round(.,2)

start_time <- Sys.time()            # Démarre chrono
cmd <- paste0("train(class ~ ., method = 'Rborist', data = lot_appr_opti,", set_Rborist_best[2], ")") # Construction de la commande
fit_Rborist_final <- eval(parse(text = cmd))     # Exécution de la commande
pred_Rborist_final <- predict(object = fit_Rborist_final, newdata = lot_evaluation)
CM_Rborist_final <- confusionMatrix(data = pred_Rborist_final, reference = lot_evaluation$class)
results_Rborist <- c(CM_Rborist_final$byClass["Sensitivity"], CM_Rborist_final$byClass["Specificity"], CM_Rborist_final$byClass["F1"])
end_time <- Sys.time()              # Stop chrono
time_Rborist <- difftime(end_time, start_time)
time_Rborist <- time_Rborist %>% as.numeric %>% round(.,2)

result_Rborist <- c(CM_Rborist_final$byClass["Sensitivity"], CM_Rborist_final$byClass["Specificity"], CM_Rborist_final$byClass["F1"], time_Rborist)
result_ranger <- c(CM_ranger_final$byClass["Sensitivity"], CM_ranger_final$byClass["Specificity"], CM_ranger_final$byClass["F1"], time_ranger)
rt_result <- rbind(result_ranger, result_Rborist)
colnames(rt_result) <- c("Sensibilité", "Spécificité", "F1 score", "Durée (min)")
rownames(rt_result) <- c("Ranger", "Rborist")

# Suppression gros fichiers intermédiaires, avant sauvegarde
rm(fit_ctree_criterion, fit_c50tree, fit_rFerns_depth, 
   fit_Rborist, fit_Rborist_best, fit_Rborist_final,
   fit_ranger, fit_ranger_best, fit_ranger_final)

save.image(file = "EKR-Champis-AnalyseMulti.RData")     # Sauvegarde données pour rapport
load(file = "EKR-Champis-AnalyseMulti.RData")     # Chargement données pour rapport
