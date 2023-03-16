##########################
#     INITIALISATION     #
##########################

# Chargement des bibliothèques
library(tidyverse)    # Outils génériques
library(caret)        # Outils d'apprentissage machine
library(DiceDesign)    # Hypercubes Latins
library(DiceEval)       # Modélisation sur hypercubes latins
library(twinning)       # Découpage équilibré des jeux de données (plus efficient que split!)

# Récupération, décompression, importation des données
fichier_data <- tempfile()
#URL <- "https://github.com/EKRihani/champis/raw/master/lot_test.zip"      # URL de mon repo
# download.file(URL, fichier_data)
fichier_data <- "~/projects/champis/lot_test.zip" # FICHIER LOCAL

fichier_data <- unzip(fichier_data, "lot_test.csv")
dataset <- read.csv(fichier_data, header = TRUE, sep = ",", stringsAsFactors = TRUE)   # sep ; ou, selon fichier


##############################################################################
#     CREATION DES LOTS D'ENTRAINEMENT, VALIDATION, EVALUATION + GRAPHES     #
##############################################################################
# Suppression family/name/classe + renommage correct
dataset$class <- NULL      # Censé être inconnu !!!
dataset$name <- NULL       # On y va mollo : d'abord les familles...
dataset$family <- as.factor(str_replace_all(dataset$family, "[:space:]|-", "_"))

MUL_n_champis <- nrow(dataset)
MUL_split_p <- sqrt(MUL_n_champis)
MUL_split_facteur <- round(sqrt(MUL_split_p)+1)

set.seed(007)
index1 <- twin(data = dataset, r = MUL_split_facteur)
MUL_lot_appr_opti <- dataset[-index1,]
MUL_lot_evaluation <- dataset[index1,]

##############################################################################
#     ANALYSE DU LOT D'ENTRAINEMENT AVEC MODELES DE CARET, BICLASSIFIEUR     #
##############################################################################
# https://topepo.github.io/caret/available-models.html
# names(getModelInfo())
# getModelInfo(Rborist)

#MUL_ratioSpeSen <- 10

# Définition de fonction : lance le modèle avec les paramètres données, évalue la performance (spécificité), renvoie les résultats de fitting
fit_test <- function(fcn_model){
   set.seed(1)
   tr_ctrl <- trainControl(classProbs = TRUE, 
                           summaryFunction = multiClassSummary, 
                           method = "cv", 
                           number = MUL_split_facteur)   # Règle paramètres d'évaluation performance à multiClassSummary (kappa...), avec cross-validation
   cmd <- paste0("train(family ~ ., method = '",      # Construit commande, évaluation de performance
                 fcn_model[1], 
                 "', data = MUL_lot_appr_opti, trControl = tr_ctrl, ", 
                 fcn_model[2],")")
   fitting <- eval(parse(text = cmd))        # Lance commande
   fitting
}

# Définition de fonction : graphique 2D
# graphe2D <- function(fcn_donnees, fcn_modele, fcn_x, fcn_y, fcn_metrique, fcn_couleur){
#    fcn_x <- enquo(fcn_x)
#    fcn_y <- enquo(fcn_y)
#    fcn_metrique <- enquo(fcn_metrique)
#    ggplot() +
#       geom_raster(data = BI_pred_rpartcost, aes(x = !!fcn_x, y = !!fcn_y, fill = !!fcn_metrique), interpolate = TRUE) +
#       geom_tile(data = fcn_modele, aes(x = !!fcn_x, y = !!fcn_y, fill = !!fcn_metrique), color = "black", linewidth =.5) +
#       scale_fill_viridis_c(option = fcn_couleur, direction = 1) +
#       theme_bw() +
#       theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5)) +
#       theme(legend.position = "bottom")
# }
graphe2D <- function(fcn_donnees, fcn_modele, fcn_x, fcn_y, fcn_metrique, fcn_couleur){
   cmd <- paste0(fcn_donnees, " %>% ggplot() +
   geom_raster(data =", fcn_donnees, ", aes(x =", fcn_x, ", y =", fcn_y, ", fill =", fcn_metrique, "), interpolate = TRUE) +
   geom_tile(data =", fcn_modele, ", aes(x =", fcn_x, ", y =", fcn_y, ", fill =", fcn_metrique, "), color = 'black', linewidth =.5) +
   scale_fill_viridis_c(option ='" , fcn_couleur, "', direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5)) +
   theme(legend.position='bottom')"
   )
   eval(parse(text = cmd))
}

# Définition de fonction : graphique nuage/ligne de kappa
grapheKappa <- function(fcn_donnees, fcn_abcisse){
   fcn_abcisse <- enquo(fcn_abcisse)
   ggplot(data = fcn_donnees, aes(x = !!fcn_abcisse)) +
      geom_line(aes(y = kappa)) +
      geom_point(aes(y = kappa)) +
      theme_bw()
}

# Modèles types arbres (RPART, RPARTCOST, CTREE, C50TREE)

MUL_grid_rpart_cp <- data.frame(cp = 10^seq(from = -5, to = -1, by = .5))

MUL_LHS <- nolhDesign(dimension = 2, range = c(0, 1))$design     # Hypercube latin quasi-orthogonal
MUL_LHS <- data.frame(MUL_LHS)
colnames(MUL_LHS) <- c("X1", "X2")
MUL_grid_rpartcost <- MUL_LHS
colnames(MUL_grid_rpartcost) <- c("cp", "Cost")
MUL_grid_rpartcost$cp <- (MUL_grid_rpartcost$cp*1e-2+1e-5)
MUL_grid_rpartcost$Cost <- MUL_grid_rpartcost$Cost*2.5+1e-3

MUL_set_rpart_cp <- c("rpart", "tuneGrid  = MUL_grid_rpart_cp")
MUL_set_rpartcost <- c("rpartCost", "tuneGrid  = MUL_grid_rpartcost[c('cp', 'Cost')]")

MUL_set_ctree_criterion <- c("ctree", "tuneGrid  = data.frame(mincriterion = c(0.01, 0.25, 0.5, 0.75, 0.99))")
MUL_set_c50tree <- c("C5.0Tree", "")
#system.time(fit_test(MUL_set_rpart_cp))    ####### CHRONO
MUL_fit_rpart_cp <- fit_test(MUL_set_rpart_cp)
MUL_fit_rpartcost <- fit_test(MUL_set_rpartcost)      # NE MARCHE PAS ????
MUL_fit_ctree_criterion <- fit_test(MUL_set_ctree_criterion)
MUL_fit_c50tree <- fit_test(MUL_set_c50tree)    # NE MARCHE PAS ????

# Extraire résultats d'intérêt : graphes et resultats
MUL_fit_rpart_cp_results <- MUL_fit_rpart_cp$results
MUL_fit_rpart_cp_graphe <- ggplot(data = MUL_fit_rpart_cp$results, aes(x = cp, y = Kappa)) + geom_point() +  scale_x_log10()

MUL_fit_rpartcost_results <- MUL_fit_rpartcost$results      # NE MARCHE PAS ????
MUL_mod_rpartcost_kappa <- modelFit(X=MUL_fit_rpartcost_results[,1:2], Y=MUL_fit_rpartcost_results$Kappa,  type="Kriging", formula=Y~cp+Cost+cp:Cost+I(cp^2)+I(Cost^2))
MUL_pred_rpartcost <- expand.grid(MUL_fit_rpartcost_results[,1:2])
colnames(MUL_pred_rpartcost) <- c("Cost", "cp")
MUL_pred_rpartcost2 <- NULL
MUL_pred_rpartcost2$Kappa <- modelPredict(MUL_mod_rpartcost_spec, MUL_pred_rpartcost)
MUL_pred_rpartcost <- cbind(MUL_pred_rpartcost, MUL_pred_rpartcost2)
MUL_fit_rpartcost_kappa_graphe <- ggplot() +
   geom_raster(data = MUL_pred_rpartcost, aes(x = Cost, y = cp, fill = Kappa), interpolate = TRUE) +
   geom_tile(data = MUL_fit_rpartcost_results, aes(x = Cost, y = cp, fill = Kappa), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "F", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))

MUL_fit_ctree_criterion_graphe <- ggplot(MUL_fit_ctree_criterion)
MUL_fit_ctree_criterion_results <- MUL_fit_ctree_criterion$results
MUL_fit_c50tree_results <- MUL_fit_c50tree$results       # NE MARCHE PAS ???

# Meilleur modèle CART      # RPARTCOST NE MARCHE PAS ???
MUL_best_rpartcost <- which.max(MUL_fit_rpartcost_results$Spec^MUL_ratioSpeSen*MUL_fit_rpartcost_results$Sens)
MUL_best_rpartcostgrid <- data.frame(Cost = MUL_fit_rpartcost_results[MUL_best_rpartcost,]$Cost, cp =MUL_fit_rpartcost_results[MUL_best_rpartcost,]$cp)
MUL_set_rpartcost_best <- c("rpartCost", paste0("tuneGrid  = MUL_best_rpartcostgrid"))
MUL_fit_rpartcost_best <- fit_test(MUL_set_rpartcost_best)
MUL_fit_rpartcost_best_results <- MUL_fit_rpartcost_best$results


# Modèles type Random Forest (RANGER, RBORIST)
MUL_grid_ranger <- data.frame(MUL_LHS)
MUL_grid_ranger <- rbind(MUL_grid_ranger,MUL_grid_ranger)
colnames(MUL_grid_ranger) <- c("mtry", "min.node.size")
#colnames(MUL_grid_ranger) <- c("mtry", "min.node.size, num.trees")     # Pour num.trees, à tester
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

MUL_mod_ranger_kappa_GINI <- modelFit(X=MUL_fit_ranger_GINI[,1:2], Y=MUL_fit_ranger_GINI$Kappa,  type="Kriging", formula=Y~mtry+min.node.size+mtry:min.node.size+I(mtry^2)+I(min.node.size^2))
MUL_mod_ranger_accu_GINI <- modelFit(X=MUL_fit_ranger_GINI[,1:2], Y=MUL_fit_ranger_GINI$Accuracy,  type="Kriging", formula=Y~mtry+min.node.size+mtry:min.node.size+I(mtry^2)+I(min.node.size^2))
MUL_mod_ranger_kappa_ET <- modelFit(X=MUL_fit_ranger_ET[,1:2], Y=MUL_fit_ranger_ET$Kappa,  type="Kriging", formula=Y~mtry+min.node.size+mtry:min.node.size+I(mtry^2)+I(min.node.size^2))
MUL_mod_ranger_accu_ET <- modelFit(X=MUL_fit_ranger_ET[,1:2], Y=MUL_fit_ranger_ET$Accuracy,  type="Kriging", formula=Y~mtry+min.node.size+mtry:min.node.size+I(mtry^2)+I(min.node.size^2))

MUL_pred_ranger_GINI <- expand.grid(MUL_fit_ranger_GINI[,1:2])
colnames(MUL_pred_ranger_GINI) <- c("mtry", "min.node.size")
MUL_pred_ranger_GINI2 <- NULL
MUL_pred_ranger_GINI2$Kappa <- modelPredict(MUL_mod_ranger_kappa_GINI, MUL_pred_ranger_GINI)
MUL_pred_ranger_GINI2$Accuracy <- modelPredict(MUL_mod_ranger_accu_GINI, MUL_pred_ranger_GINI)
MUL_pred_ranger_GINI <- cbind(MUL_pred_ranger_GINI, MUL_pred_ranger_GINI2)

MUL_pred_ranger_ET <- expand.grid(MUL_fit_ranger_ET[,1:2])
colnames(MUL_pred_ranger_ET) <- c("mtry", "min.node.size")
MUL_pred_ranger_ET2 <- NULL
MUL_pred_ranger_ET2$Kappa <- modelPredict(MUL_mod_ranger_kappa_ET, MUL_pred_ranger_ET)
MUL_pred_ranger_ET2$Accuracy <- modelPredict(MUL_mod_ranger_accu_ET, MUL_pred_ranger_ET)
MUL_pred_ranger_ET <- cbind(MUL_pred_ranger_ET, MUL_pred_ranger_ET2)

MUL_fit_ranger_Gini_kappa_graphe <- ggplot() +
   geom_raster(data = MUL_pred_ranger_GINI, aes(x = mtry, y = min.node.size, fill = Kappa), interpolate = TRUE) +
   geom_tile(data = MUL_fit_ranger_GINI, aes(x = mtry, y = min.node.size, fill = Kappa), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "F", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5)) +
   theme(legend.position="bottom")
MUL_fit_ranger_Gini_accu_graphe <- ggplot() +
   geom_raster(data = MUL_pred_ranger_GINI, aes(x = mtry, y = min.node.size, fill = Accuracy), interpolate = TRUE) +
   geom_tile(data = MUL_fit_ranger_GINI, aes(x = mtry, y = min.node.size, fill = Accuracy), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "G", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5)) +
   theme(legend.position="bottom")

MUL_fit_ranger_ET_kappa_graphe <- ggplot() +
   geom_raster(data = MUL_pred_ranger_ET, aes(x = mtry, y = min.node.size, fill = Kappa), interpolate = TRUE) +
   geom_tile(data = MUL_fit_ranger_ET, aes(x = mtry, y = min.node.size, fill = Kappa), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "F", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5)) +
   theme(legend.position="bottom")
MUL_fit_ranger_ET_accu_graphe <- ggplot() +
   geom_raster(data = MUL_pred_ranger_ET, aes(x = mtry, y = min.node.size, fill = Accuracy), interpolate = TRUE) +
   geom_tile(data = MUL_fit_ranger_ET, aes(x = mtry, y = min.node.size, fill = Accuracy), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "G", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5)) +
   theme(legend.position="bottom")

MUL_best_ranger <- which.max(MUL_fit_ranger_results$Kappa)
MUL_best_rangergrid <- data.frame(mtry = MUL_fit_ranger_results[MUL_best_ranger,]$mtry, min.node.size =MUL_fit_ranger_results[MUL_best_ranger,]$min.node.size, splitrule =MUL_fit_ranger_results[MUL_best_ranger,]$splitrule)
MUL_set_ranger_best <- c("ranger", paste0("tuneGrid  = MUL_best_rangergrid"))
MUL_fit_ranger_best <- fit_test(MUL_set_ranger_best)
MUL_fit_ranger_best_results <- MUL_fit_ranger_best$results

MUL_mod_Rborist_kappa <- modelFit(X=MUL_fit_Rborist_results[,1:2], Y=MUL_fit_Rborist_results$Kappa,  type="Kriging", formula=Y~predFixed+minNode+predFixed:minNode+I(predFixed^2)+I(minNode^2))
MUL_mod_Rborist_accu <-  modelFit(X=MUL_fit_Rborist_results[,1:2], Y=MUL_fit_Rborist_results$Accuracy,  type="Kriging", formula=Y~predFixed+minNode+predFixed:minNode+I(predFixed^2)+I(minNode^2))
MUL_pred_Rborist <- expand.grid(MUL_fit_Rborist_results[,1:2])
colnames(MUL_pred_Rborist) <- c("predFixed", "minNode")
MUL_pred_Rborist2 <- NULL
MUL_pred_Rborist2$Kappa <- modelPredict(MUL_mod_Rborist_kappa, MUL_pred_Rborist)
MUL_pred_Rborist2$Accuracy <- modelPredict(MUL_mod_Rborist_accu, MUL_pred_Rborist)
MUL_pred_Rborist <- cbind(MUL_pred_Rborist, MUL_pred_Rborist2)

MUL_fit_Rborist_kappa_graphe <- ggplot() +
   geom_raster(data = MUL_pred_Rborist, aes(x = predFixed, y = minNode, fill = Kappa), interpolate = TRUE) +
   geom_tile(data = MUL_fit_Rborist_results, aes(x = predFixed, y = minNode, fill = Kappa), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "F", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5)) +
   theme(legend.position="bottom")
MUL_fit_Rborist_accu_graphe <- ggplot() +
   geom_raster(data = MUL_pred_Rborist, aes(x = predFixed, y = minNode, fill = Accuracy), interpolate = TRUE) +
   geom_tile(data = MUL_fit_Rborist_results, aes(x = predFixed, y = minNode, fill = Accuracy), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "G", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5)) +
   theme(legend.position="bottom")

MUL_best_Rborist <- which.max(MUL_fit_Rborist_results$Kappa)
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
MUL_evaluation$reference <- as.factor(MUL_evaluation$family)

start_time <- Sys.time()     # Démarre chrono
cmd <- paste0("train(family ~ ., method = 'ranger', data = MUL_lot_appr_opti,", MUL_set_ranger_best[2], ")") # Construction de la commande
MUL_fit_ranger_final <- eval(parse(text = cmd))     # Exécution de la commande
MUL_pred_ranger_final <- predict(object = MUL_fit_ranger_final, newdata = MUL_lot_evaluation)
MUL_CM_ranger_final <- confusionMatrix(data = MUL_pred_ranger_final, reference = MUL_lot_evaluation$family)
MUL_resultats_ranger <- c(MUL_CM_ranger_final$byClass["Accuracy"], MUL_CM_ranger_final$byClass["Kappa"])
end_time <- Sys.time()     # Stop chrono
MUL_temps_ranger <- difftime(end_time, start_time)
MUL_temps_ranger <- MUL_temps_ranger %>% as.numeric %>% round(.,2)

start_time <- Sys.time()            # Démarre chrono
cmd <- paste0("train(family ~ ., method = 'Rborist', data = MUL_lot_appr_opti,", MUL_set_Rborist_best[2], ")") # Construction de la commande
MUL_fit_Rborist_final <- eval(parse(text = cmd))     # Exécution de la commande
MUL_pred_Rborist_final <- predict(object = MUL_fit_Rborist_final, newdata = MUL_lot_evaluation)
MUL_CM_Rborist_final <- confusionMatrix(data = MUL_pred_Rborist_final, reference = MUL_lot_evaluation$family)
MUL_resultats_Rborist <- c(MUL_CM_Rborist_final$byClass["Accuracy"], MUL_CM_Rborist_final$byClass["Kappa"])
end_time <- Sys.time()              # Stop chrono
MUL_temps_Rborist <- difftime(end_time, start_time)
MUL_temps_Rborist <- MUL_temps_Rborist %>% as.numeric %>% round(.,2)

MUL_resultat_Rborist <- c(MUL_CM_Rborist_final$overall["Accuracy"], MUL_CM_Rborist_final$overall["Kappa"], MUL_temps_Rborist)
MUL_resultat_ranger <- c(MUL_CM_ranger_final$overall["Accuracy"], MUL_CM_ranger_final$overall["Kappa"], MUL_temps_ranger)
MUL_RF_resultat <- rbind(MUL_resultat_ranger, MUL_resultat_Rborist)
colnames(MUL_RF_resultat) <- c("Précision", "Kappa", "Durée (min)")
rownames(MUL_RF_resultat) <- c("Ranger", "Rborist")

# Suppression gros fichiers intermédiaires, avant sauvegarde
rm(dataset, MUL_evaluation, MUL_lot_appr_opti, MUL_lot_apprentissage, MUL_lot_evaluation,
   MUL_fit_rpart_cp, MUL_fit_rpartcost, MUL_fit_rpartcost_best, MUL_fit_ctree_criterion, MUL_fit_c50tree,
   MUL_fit_Rborist, MUL_fit_Rborist_best, MUL_fit_Rborist_final,
   MUL_fit_ranger, MUL_fit_ranger_best, MUL_fit_ranger_final)

save.image(file = "EKR-Champis-AnalyseMulti.RData")     # Sauvegarde données pour rapport
load(file = "EKR-Champis-AnalyseMulti.RData")     # Chargement données pour rapport
