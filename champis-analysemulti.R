##########################
#     INITIALISATION     #
##########################

# Chargement des bibliothèques
library(tidyverse)    # Outils génériques
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
# Creation lots d'entrainement/validation (92%) et MUL_evaluation (8%)
set.seed(007)
index1 <- createDataPartition(y = dataset$cap.diameter, times = 1, p = split1, list = FALSE)
MUL_lot_appr_opti <- dataset[-index1,]
MUL_lot_evaluation <- dataset[index1,]

# Creation lots d'entrainement (92%) et validation (8%)
set.seed(1337)
index2 <- createDataPartition(y = MUL_lot_appr_opti$cap.diameter, times = 1, p = split2, list = FALSE)
MUL_lot_apprentissage <- MUL_lot_appr_opti[-index2,]
MUL_lot_evaluation <- MUL_lot_appr_opti[index2,]


##############################################################################
#     ANALYSE DU LOT D'ENTRAINEMENT AVEC MODELES DE CARET, BICLASSIFIEUR     #
##############################################################################
# https://topepo.github.io/caret/available-models.html
# names(getModelInfo())
# getModelInfo(Rborist)

MUL_ratioSpeSen <- 10
MUL_n_folds <- 5
# Définition de fonction : lance le modèle avec les paramètres données, évalue la performance (spécificité), renvoie les résultats de fitting
fit_test <- function(fcn_model){
   set.seed(1)
   tr_ctrl <- trainControl(classProbs = TRUE, summaryFunction = twoClassSummary, method = "cv", number = MUL_n_folds)   # Règle paramètres d'évaluation performance à twoClassSummary (ROC, Sens, Spec), avec cross-validation (10-fold)
   cmd <- paste0("train(class ~ ., method = '",      # Construit commande, évaluation de performance par Spécificité
                 fcn_model[1], 
                 "', data = MUL_lot_appr_opti, trControl = tr_ctrl, metric = 'Spec', ", 
                 fcn_model[2],")")
   fitting <- eval(parse(text = cmd))        # Lance commande
   fitting
}

# Parallélisation (A TESTER !!!)
# library(doParallel)
# cl <- makeCluster(spec = 5, type = "PSOCK")
# registerDoParallel(cl)
#[truc à tester]
# stopCluster(cl)

# Modèles types arbres (RPART, RPARTCOST, CTREE, C50TREE)

MUL_grid_rpart_cp <- data.frame(cp = 10^seq(from = -5, to = -1, by = .5))

MUL_LHS <- nolhDesign(dimension = 2, range = c(0, 1))$design     # Hypercube latin quasi-orthogonal
MUL_grid_rpartcost <- data.frame(MUL_LHS)
colnames(MUL_grid_rpartcost) <- c("cp", "Cost")
MUL_grid_rpartcost$cp <- (MUL_grid_rpartcost$cp*1e-2+1e-5)
MUL_grid_rpartcost$Cost <- MUL_grid_rpartcost$Cost*2.5+1e-3

MUL_set_rpart_cp <- c("rpart", "tuneGrid  = MUL_grid_rpart_cp")
MUL_set_rpartcost <- c("rpartCost", "tuneGrid  = MUL_grid_rpartcost")     # A TESTER !!!

MUL_set_ctree_criterion <- c("ctree", "tuneGrid  = data.frame(mincriterion = c(0.01, 0.25, 0.5, 0.75, 0.99))")
MUL_set_c50tree <- c("C5.0Tree", "")
system.time(fit_test(MUL_set_rpart_cp))    ####### CHRONO
MUL_fit_rpart_cp <- fit_test(MUL_set_rpart_cp)
MUL_fit_rpartcost <- fit_test(MUL_set_rpartcost)
MUL_fit_ctree_criterion <- fit_test(MUL_set_ctree_criterion)
MUL_fit_c50tree <- fit_test(MUL_set_c50tree)

# Extraire résultats d'intérêt : graphes et resultats
MUL_fit_rpart_cp_results <- MUL_fit_rpart_cp$results
MUL_fit_rpart_cp_graphe <- ggplot(data = MUL_fit_rpart_cp$results, aes(x = cp, y = Spec)) + geom_point() + ylab("Spécificité") + scale_x_log10()

MUL_fit_rpartcost_results <- MUL_fit_rpartcost$results
MUL_mod_rpartcost_spec <- modelFit(X=MUL_fit_rpartcost_results[,1:2], Y=MUL_fit_rpartcost_results$Spec,  type="Kriging", formula=Y~cp+Cost+cp:Cost+I(cp^2)+I(Cost^2))
MUL_mod_rpartcost_sens <-  modelFit(X=MUL_fit_rpartcost_results[,1:2], Y=MUL_fit_rpartcost_results$Sens,  type="Kriging", formula=Y~cp+Cost+cp:Cost+I(cp^2)+I(Cost^2))
MUL_pred_rpartcost <- expand.grid(MUL_fit_rpartcost_results[,1:2])
colnames(MUL_pred_rpartcost) <- c("Cost", "cp")
MUL_pred_rpartcost2 <- NULL
MUL_pred_rpartcost2$Spec <- modelPredict(MUL_mod_rpartcost_spec, MUL_pred_rpartcost)
MUL_pred_rpartcost2$Sens <- modelPredict(MUL_mod_rpartcost_sens, MUL_pred_rpartcost)
MUL_pred_rpartcost <- cbind(MUL_pred_rpartcost, MUL_pred_rpartcost2)
MUL_fit_rpartcost_spec_graphe <- ggplot() +
   geom_raster(data = MUL_pred_rpartcost, aes(x = Cost, y = cp, fill = Spec), interpolate = TRUE) +
   geom_tile(data = MUL_fit_rpartcost_results, aes(x = Cost, y = cp, fill = Spec), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "F", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))
MUL_fit_rpartcost_sens_graphe <- ggplot() +
   geom_raster(data = MUL_pred_rpartcost, aes(x = Cost, y = cp, fill = Sens), interpolate = TRUE) +
   geom_tile(data = MUL_fit_rpartcost_results, aes(x = Cost, y = cp, fill = Sens), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "G", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))

MUL_fit_ctree_criterion_graphe <- ggplot(MUL_fit_ctree_criterion)
MUL_fit_ctree_criterion_results <- MUL_fit_ctree_criterion$results
MUL_fit_c50tree_results <- MUL_fit_c50tree$results

# Meilleur modèle CART
MUL_best_rpartcost <- which.max(MUL_fit_rpartcost_results$Spec^MUL_ratioSpeSen*MUL_fit_rpartcost_results$Sens)
MUL_best_rpartcostgrid <- data.frame(Cost = MUL_fit_rpartcost_results[MUL_best_rpartcost,]$Cost, cp =MUL_fit_rpartcost_results[MUL_best_rpartcost,]$cp)
MUL_set_rpartcost_best <- c("rpartCost", paste0("tuneGrid  = MUL_best_rpartcostgrid"))
MUL_fit_rpartcost_best <- fit_test(MUL_set_rpartcost_best)
MUL_fit_rpartcost_best_results <- MUL_fit_rpartcost_best$results


# Modèles type Random Forest (RFERNS, RANGER, RBORIST)
MUL_set_rFerns_depth <- c("rFerns", "tuneGrid  = data.frame(depth = 2^(1:5)/2)")

MUL_grid_ranger <- data.frame(MUL_LHS)
MUL_grid_ranger <- rbind(MUL_grid_ranger,MUL_grid_ranger)
colnames(MUL_grid_ranger) <- c("mtry", "min.node.size")
#colnames(MUL_grid_ranger) <- c("mtry", "min.node.size, num.trees")     # Pour num.treees, à tester
MUL_grid_ranger$splitrule <- c(rep("extratrees", 17), rep("gini", 17))
MUL_grid_ranger$mtry <- round(1+MUL_grid_ranger$mtry*48,0)       # Si arrondi : prendre des multiples de 16 (car 17 points pour 2d)
MUL_grid_ranger$min.node.size <- round(1+MUL_grid_ranger$min.node.size*32,0)
#MUL_grid_ranger$num.trees <- round(1+MUL_grid_ranger$num.trees*9,0) # VOIR SI LE NUM.TREES MARCHE ???

MUL_grid_Rborist <- data.frame(MUL_LHS)
#colnames(MUL_grid_Rborist) <- c("predFixed", "minNode", "ntrees")
colnames(MUL_grid_Rborist) <- c("predFixed", "minNode")
MUL_grid_Rborist$predFixed <- round(1+MUL_grid_Rborist$predFixed*32,0)
MUL_grid_Rborist$minNode <- round(1+MUL_grid_Rborist$minNode*16,0)
#MUL_grid_Rborist$ntrees <- round(1+MUL_grid_Rborist$ntrees*5,0)

MUL_set_ranger <- c("ranger", "tuneGrid  = MUL_grid_ranger, num.trees = 6") # A TESTER
MUL_set_Rborist <- c("Rborist", "tuneGrid  = MUL_grid_Rborist")
#system.time(fit_test(MUL_set_ranger_mtry))  ####### CHRONO
MUL_fit_ranger <- fit_test(MUL_set_ranger)
MUL_fit_Rborist <- fit_test(MUL_set_Rborist)

# Extraire résultats d'intérêt : graphes et resultats
MUL_fit_ranger_results <- MUL_fit_ranger$results
MUL_fit_ranger_bestTune <- MUL_fit_ranger$bestTune
MUL_fit_Rborist_results <- MUL_fit_Rborist$results
MUL_fit_Rborist_bestTune <- MUL_fit_Rborist$bestTune

MUL_fit_ranger_GINI <- MUL_fit_ranger_results %>% filter (splitrule == "gini")
MUL_fit_ranger_ET <- MUL_fit_ranger_results %>% filter (splitrule == "extratrees")
MUL_mod_ranger_spec_GINI <- modelFit(X=MUL_fit_ranger_GINI[,1:2], Y=MUL_fit_ranger_GINI$Spec,  type="Kriging", formula=Y~mtry+min.node.size+mtry:min.node.size+I(mtry^2)+I(min.node.size^2))
MUL_mod_ranger_sens_GINI <- modelFit(X=MUL_fit_ranger_GINI[,1:2], Y=MUL_fit_ranger_GINI$Sens,  type="Kriging", formula=Y~mtry+min.node.size+mtry:min.node.size+I(mtry^2)+I(min.node.size^2))
MUL_mod_ranger_spec_ET <- modelFit(X=MUL_fit_ranger_ET[,1:2], Y=MUL_fit_ranger_ET$Spec,  type="Kriging", formula=Y~mtry+min.node.size+mtry:min.node.size+I(mtry^2)+I(min.node.size^2))
MUL_mod_ranger_sens_ET <- modelFit(X=MUL_fit_ranger_ET[,1:2], Y=MUL_fit_ranger_ET$Sens,  type="Kriging", formula=Y~mtry+min.node.size+mtry:min.node.size+I(mtry^2)+I(min.node.size^2))

MUL_pred_ranger_GINI <- expand.grid(MUL_fit_ranger_GINI[,1:2])
colnames(MUL_pred_ranger_GINI) <- c("mtry", "min.node.size")
MUL_pred_ranger_GINI2 <- NULL
MUL_pred_ranger_GINI2$Spec <- modelPredict(MUL_mod_ranger_spec_GINI, MUL_pred_ranger_GINI)
MUL_pred_ranger_GINI2$Sens <- modelPredict(MUL_mod_ranger_sens_GINI, MUL_pred_ranger_GINI)
MUL_pred_ranger_GINI <- cbind(MUL_pred_ranger_GINI, MUL_pred_ranger_GINI2)

MUL_pred_ranger_ET <- expand.grid(MUL_fit_ranger_ET[,1:2])
colnames(MUL_pred_ranger_ET) <- c("mtry", "min.node.size")
MUL_pred_ranger_ET2 <- NULL
MUL_pred_ranger_ET2$Spec <- modelPredict(MUL_mod_ranger_spec_ET, MUL_pred_ranger_ET)
MUL_pred_ranger_ET2$Sens <- modelPredict(MUL_mod_ranger_sens_ET, MUL_pred_ranger_ET)
MUL_pred_ranger_ET <- cbind(MUL_pred_ranger_ET, MUL_pred_ranger_ET2)

MUL_fit_ranger_Gini_spec_graphe <- ggplot() +
   geom_raster(data = MUL_pred_ranger_GINI, aes(x = mtry, y = min.node.size, fill = Spec), interpolate = TRUE) +
   geom_tile(data = MUL_fit_ranger_GINI, aes(x = mtry, y = min.node.size, fill = Spec), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "F", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))
MUL_fit_ranger_Gini_sens_graphe <- ggplot() +
   geom_raster(data = MUL_pred_ranger_GINI, aes(x = mtry, y = min.node.size, fill = Sens), interpolate = TRUE) +
   geom_tile(data = MUL_fit_ranger_GINI, aes(x = mtry, y = min.node.size, fill = Sens), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "G", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))

MUL_fit_ranger_ET_spec_graphe <- ggplot() +
   geom_raster(data = MUL_pred_ranger_ET, aes(x = mtry, y = min.node.size, fill = Spec), interpolate = TRUE) +
   geom_tile(data = MUL_fit_ranger_ET, aes(x = mtry, y = min.node.size, fill = Spec), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "F", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))
MUL_fit_ranger_ET_sens_graphe <- ggplot() +
   geom_raster(data = MUL_pred_ranger_ET, aes(x = mtry, y = min.node.size, fill = Sens), interpolate = TRUE) +
   geom_tile(data = MUL_fit_ranger_ET, aes(x = mtry, y = min.node.size, fill = Sens), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "G", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))

MUL_best_ranger <- which.max(MUL_fit_ranger_results$Spec^MUL_ratioSpeSen*MUL_fit_ranger_results$Sens)
MUL_best_rangergrid <- data.frame(mtry = MUL_fit_ranger_results[MUL_best_ranger,]$mtry, min.node.size =MUL_fit_ranger_results[MUL_best_ranger,]$min.node.size, splitrule =MUL_fit_ranger_results[MUL_best_ranger,]$splitrule)
MUL_set_ranger_best <- c("ranger", paste0("tuneGrid  = MUL_best_rangergrid"))
MUL_fit_ranger_best <- fit_test(MUL_set_ranger_best)
MUL_fit_ranger_best_results <- MUL_fit_ranger_best$results

MUL_mod_Rborist_spec <- modelFit(X=MUL_fit_Rborist_results[,1:2], Y=MUL_fit_Rborist_results$Spec,  type="Kriging", formula=Y~predFixed+minNode+predFixed:minNode+I(predFixed^2)+I(minNode^2))
MUL_mod_Rborist_sens <-  modelFit(X=MUL_fit_Rborist_results[,1:2], Y=MUL_fit_Rborist_results$Sens,  type="Kriging", formula=Y~predFixed+minNode+predFixed:minNode+I(predFixed^2)+I(minNode^2))
MUL_pred_Rborist <- expand.grid(MUL_fit_Rborist_results[,1:2])
colnames(MUL_pred_Rborist) <- c("predFixed", "minNode")
MUL_pred_Rborist2 <- NULL
MUL_pred_Rborist2$Spec <- modelPredict(MUL_mod_Rborist_spec, MUL_pred_Rborist)
MUL_pred_Rborist2$Sens <- modelPredict(MUL_mod_Rborist_sens, MUL_pred_Rborist)
MUL_pred_Rborist <- cbind(MUL_pred_Rborist, MUL_pred_Rborist2)

MUL_fit_Rborist_spec_graphe <- ggplot() +
   geom_raster(data = MUL_pred_Rborist, aes(x = predFixed, y = minNode, fill = Spec), interpolate = TRUE) +
   geom_tile(data = MUL_fit_Rborist_results, aes(x = predFixed, y = minNode, fill = Spec), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "F", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))
MUL_fit_Rborist_sens_graphe <- ggplot() +
   geom_raster(data = MUL_pred_Rborist, aes(x = predFixed, y = minNode, fill = Sens), interpolate = TRUE) +
   geom_tile(data = MUL_fit_Rborist_results, aes(x = predFixed, y = minNode, fill = Sens), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "G", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))

MUL_best_Rborist <- which.max(MUL_fit_Rborist_results$Spec^MUL_ratioSpeSen*MUL_fit_Rborist_results$Sens)
MUL_best_Rboristgrid <- data.frame(predFixed = MUL_fit_Rborist_results[MUL_best_Rborist,]$predFixed, minNode =MUL_fit_Rborist_results[MUL_best_Rborist,]$minNode)
MUL_set_Rborist_best <- c("Rborist", paste0("tuneGrid  = MUL_best_Rboristgrid"))
MUL_fit_Rborist_best <- fit_test(MUL_set_Rborist_best)
MUL_fit_Rborist_best_results <- MUL_fit_Rborist_best$results

# Lance modèle RANGER optimal
MUL_set_ranger_best <- c("ranger", paste0("tuneGrid  = MUL_best_rangergrid, num.trees = 6"))
MUL_fit_ranger_best <- fit_test(MUL_set_ranger_best)
MUL_fit_ranger_best_results <- MUL_fit_ranger_best$results

# Lance modèle RBORIST optimal
MUL_set_Rborist_best <- c("Rborist", paste0("tuneGrid  = MUL_best_Rboristgrid, ntrees = 2"))
MUL_fit_Rborist_best <- fit_test(MUL_set_Rborist_best)
MUL_fit_Rborist_best_results <- MUL_fit_Rborist_best$results


#########################################################
#     PERFORMANCE DES MODELES SUR LOT D'EVALUATION      #
#########################################################

# Règle la liste de prédiction et lance la classification
MUL_evaluation <- MUL_lot_evaluation
MUL_evaluation$reference <- as.logical(as.character(recode_factor(MUL_evaluation$class, e = TRUE, p = FALSE))) # Bascule en booléen

# Passe .$reference de booléen à facteur, puis calcule la matrice de confusion
MUL_evaluation$reference <- as.factor(MUL_evaluation$reference)


start_time <- Sys.time()     # Démarre chrono
cmd <- paste0("train(class ~ ., method = 'ranger', data = MUL_lot_appr_opti,", MUL_set_ranger_best[2], ")") # Construction de la commande
MUL_fit_ranger_final <- eval(parse(text = cmd))     # Exécution de la commande
MUL_pred_ranger_final <- predict(object = MUL_fit_ranger_final, newdata = MUL_lot_evaluation)
MUL_CM_ranger_final <- confusionMatrix(data = MUL_pred_ranger_final, reference = MUL_lot_evaluation$class)
MUL_resultats_ranger <- c(MUL_CM_ranger_final$byClass["Sensitivity"], MUL_CM_ranger_final$byClass["Specificity"], MUL_CM_ranger_final$byClass["F1"])
end_time <- Sys.time()     # Stop chrono
MUL_temps_ranger <- difftime(end_time, start_time)
MUL_temps_ranger <- MUL_temps_ranger %>% as.numeric %>% round(.,2)

start_time <- Sys.time()            # Démarre chrono
cmd <- paste0("train(class ~ ., method = 'Rborist', data = MUL_lot_appr_opti,", MUL_set_Rborist_best[2], ")") # Construction de la commande
MUL_fit_Rborist_final <- eval(parse(text = cmd))     # Exécution de la commande
MUL_pred_Rborist_final <- predict(object = MUL_fit_Rborist_final, newdata = MUL_lot_evaluation)
MUL_CM_Rborist_final <- confusionMatrix(data = MUL_pred_Rborist_final, reference = MUL_lot_evaluation$class)
MUL_resultats_Rborist <- c(MUL_CM_Rborist_final$byClass["Sensitivity"], MUL_CM_Rborist_final$byClass["Specificity"], MUL_CM_Rborist_final$byClass["F1"])
end_time <- Sys.time()              # Stop chrono
MUL_temps_Rborist <- difftime(end_time, start_time)
MUL_temps_Rborist <- MUL_temps_Rborist %>% as.numeric %>% round(.,2)

MUL_resultat_Rborist <- c(MUL_CM_Rborist_final$byClass["Sensitivity"], MUL_CM_Rborist_final$byClass["Specificity"], MUL_CM_Rborist_final$byClass["F1"], MUL_temps_Rborist)
MUL_resultat_ranger <- c(MUL_CM_ranger_final$byClass["Sensitivity"], MUL_CM_ranger_final$byClass["Specificity"], MUL_CM_ranger_final$byClass["F1"], MUL_temps_ranger)
MUL_RF_resultat <- rbind(MUL_resultat_ranger, MUL_resultat_Rborist)
colnames(MUL_RF_resultat) <- c("Sensibilité", "Spécificité", "F1 score", "Durée (min)")
rownames(MUL_RF_resultat) <- c("Ranger", "Rborist")

# Suppression gros fichiers intermédiaires, avant sauvegarde
rm(dataset, MUL_evaluation, #MUL_lot_appr_opti, MUL_lot_apprentissage, MUL_lot_evaluation,
   MUL_fit_pda_lambda, MUL_fit_lda2_dim, MUL_fit_gamLoess_degree, MUL_fit_gamLoess_span,
   MUL_fit_rpart_cp, MUL_fit_rpartcost,
   MUL_fit_ctree_criterion, MUL_fit_c50tree, MUL_fit_rFerns_depth, 
   MUL_fit_Rborist, MUL_fit_Rborist_best, MUL_fit_Rborist_final,
   MUL_fit_ranger, MUL_fit_ranger_best, MUL_fit_ranger_final)
save.image(file = "EKR-Champis-AnalyseBi.RData")     # Sauvegarde données pour rapport
load(file = "EKR-Champis-AnalyseBi.RData")     # Chargement données pour rapport

save.image(file = "EKR-Champis-AnalyseMulti.RData")     # Sauvegarde données pour rapport
load(file = "EKR-Champis-AnalyseMulti.RData")     # Chargement données pour rapport
