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

MULFAM_n_champis <- nrow(dataset)
MULFAM_split_p <- sqrt(MULFAM_n_champis)
MULFAM_split_facteur <- round(sqrt(MULFAM_split_p)+1)

set.seed(007)
index1 <- twin(data = dataset, r = MULFAM_split_facteur)
MULFAM_lot_appr_opti <- dataset[-index1,]
MULFAM_lot_evaluation <- dataset[index1,]

##############################################################################
#     ANALYSE DU LOT D'ENTRAINEMENT AVEC MODELES DE CARET, BICLASSIFIEUR     #
##############################################################################
# https://topepo.github.io/caret/available-models.html
# names(getModelInfo())
# getModelInfo(Rborist)

#MULFAM_ratioSpeSen <- 10

# Définition de fonction : lance le modèle avec les paramètres données, évalue la performance (spécificité), renvoie les résultats de fitting
fit_test <- function(fcn_model){
   set.seed(1)
   tr_ctrl <- trainControl(classProbs = TRUE, 
                           summaryFunction = multiClassSummary, 
                           method = "cv", 
                           number = MULFAM_split_facteur)   # Règle paramètres d'évaluation performance à multiClassSummary (kappa...), avec cross-validation
   cmd <- paste0("train(family ~ ., method = '",      # Construit commande, évaluation de performance
                 fcn_model[1], 
                 "', data = MULFAM_lot_appr_opti, trControl = tr_ctrl, ", 
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
      geom_line(aes(y = Kappa)) +
      geom_point(aes(y = Kappa)) +
      theme_bw()
}

# Modèles types arbres (RPART, RPARTCOST, CTREE, C50TREE)

MULFAM_grid_rpart_cp <- data.frame(cp = 10^seq(from = -5, to = -1, by = .5))

MULFAM_LHS <- nolhDesign(dimension = 2, range = c(0, 1))$design     # Hypercube latin quasi-orthogonal
MULFAM_LHS <- data.frame(MULFAM_LHS)
colnames(MULFAM_LHS) <- c("X1", "X2")
# MULFAM_grid_rpartcost <- MULFAM_LHS
# colnames(MULFAM_grid_rpartcost) <- c("cp", "Cost")
# MULFAM_grid_rpartcost$cp <- (MULFAM_grid_rpartcost$cp*1e-2+1e-5)
# MULFAM_grid_rpartcost$Cost <- MULFAM_grid_rpartcost$Cost*2.5+1e-3

MULFAM_set_rpart_cp <- c("rpart", "tuneGrid  = MULFAM_grid_rpart_cp")
# MULFAM_set_rpartcost <- c("rpartCost", "tuneGrid  = MULFAM_grid_rpartcost[c('cp', 'Cost')]")

MULFAM_set_ctree_criterion <- c("ctree", "tuneGrid  = data.frame(mincriterion = c(0.01, 0.25, 0.5, 0.75, 0.99))")
# MULFAM_set_c50tree <- c("C5.0Tree", "")
MULFAM_fit_rpart_cp <- fit_test(MULFAM_set_rpart_cp)
# MULFAM_fit_rpartcost <- fit_test(MULFAM_set_rpartcost)      # NE MARCHE PAS ????
MULFAM_fit_ctree_criterion <- fit_test(MULFAM_set_ctree_criterion)
# MULFAM_fit_c50tree <- fit_test(MULFAM_set_c50tree)    # NE MARCHE PAS ????

# Extraire résultats d'intérêt : graphes et resultats
MULFAM_fit_rpart_cp_resultats <- MULFAM_fit_rpart_cp$results
MULFAM_fit_rpart_cp_graphe <- grapheKappa(MULFAM_fit_rpart_cp_resultats, cp)+ scale_x_log10()

# MULFAM_fit_rpartcost_resultats <- MULFAM_fit_rpartcost$results      # NE MARCHE PAS ????
# MULFAM_mod_rpartcost_kappa <- modelFit(X=MULFAM_fit_rpartcost_resultats[,1:2], Y=MULFAM_fit_rpartcost_resultats$Kappa,  type="Kriging", formula=Y~cp+Cost+cp:Cost+I(cp^2)+I(Cost^2))
# MULFAM_pred_rpartcost <- expand.grid(MULFAM_fit_rpartcost_resultats[,1:2])
# colnames(MULFAM_pred_rpartcost) <- c("Cost", "cp")
# MULFAM_pred_rpartcost2 <- NULL
# MULFAM_pred_rpartcost2$Kappa <- modelPredict(MULFAM_mod_rpartcost_spec, MULFAM_pred_rpartcost)
# MULFAM_pred_rpartcost <- cbind(MULFAM_pred_rpartcost, MULFAM_pred_rpartcost2)
# MULFAM_fit_rpartcost_kappa_graphe <- ggplot() +
#    geom_raster(data = MULFAM_pred_rpartcost, aes(x = Cost, y = cp, fill = Kappa), interpolate = TRUE) +
#    geom_tile(data = MULFAM_fit_rpartcost_resultats, aes(x = Cost, y = cp, fill = Kappa), color = "black", linewidth =.5) +
#    scale_fill_viridis_c(option = "F", direction = 1) +
#    theme_bw() +
#    theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))

MULFAM_fit_ctree_criterion_resultats <- MULFAM_fit_ctree_criterion$results
MULFAM_fit_ctree_criterion_graphe <- grapheKappa(MULFAM_fit_ctree_criterion_resultats, mincriterion)
# MULFAM_fit_c50tree_resultats <- MULFAM_fit_c50tree$results       # NE MARCHE PAS ???


# Meilleur modèle CART      # RPARTCOST NE MARCHE PAS ???
# MULFAM_best_rpartcost <- which.max(MULFAM_fit_rpartcost_resultats$Spec^MULFAM_ratioSpeSen*MULFAM_fit_rpartcost_resultats$Sens)
# MULFAM_best_rpartcostgrid <- data.frame(Cost = MULFAM_fit_rpartcost_resultats[MULFAM_best_rpartcost,]$Cost, cp =MULFAM_fit_rpartcost_resultats[MULFAM_best_rpartcost,]$cp)
# MULFAM_set_rpartcost_best <- c("rpartCost", paste0("tuneGrid  = MULFAM_best_rpartcostgrid"))
# MULFAM_fit_rpartcost_best <- fit_test(MULFAM_set_rpartcost_best)
# MULFAM_fit_rpartcost_best_resultats <- MULFAM_fit_rpartcost_best$results


# Modèles type Random Forest (RANGER, RBORIST)
MULFAM_grid_ranger <- data.frame(MULFAM_LHS)
MULFAM_grid_ranger <- rbind(MULFAM_grid_ranger,MULFAM_grid_ranger)
colnames(MULFAM_grid_ranger) <- c("mtry", "min.node.size")
#colnames(MULFAM_grid_ranger) <- c("mtry", "min.node.size, num.trees")     # Pour num.trees, à tester
MULFAM_grid_ranger$splitrule <- c(rep("extratrees", 17), rep("gini", 17))
MULFAM_grid_ranger$mtry <- round(1+MULFAM_grid_ranger$mtry*48,0)       # Si arrondi : prendre des multiples de 16 (car 17 points pour 2d)
MULFAM_grid_ranger$min.node.size <- round(1+MULFAM_grid_ranger$min.node.size*32,0)
#MULFAM_grid_ranger$num.trees <- round(1+MULFAM_grid_ranger$num.trees*9,0) # VOIR SI LE NUM.TREES MARCHE ???

MULFAM_grid_Rborist <- data.frame(MULFAM_LHS)
#colnames(MULFAM_grid_Rborist) <- c("predFixed", "minNode", "ntrees")
colnames(MULFAM_grid_Rborist) <- c("predFixed", "minNode")
MULFAM_grid_Rborist$predFixed <- round(1+MULFAM_grid_Rborist$predFixed*32,0)
MULFAM_grid_Rborist$minNode <- round(1+MULFAM_grid_Rborist$minNode*16,0)
#MULFAM_grid_Rborist$ntrees <- round(1+MULFAM_grid_Rborist$ntrees*5,0)

MULFAM_set_ranger <- c("ranger", "tuneGrid  = MULFAM_grid_ranger, num.trees = 6") # A TESTER
MULFAM_set_Rborist <- c("Rborist", "tuneGrid  = MULFAM_grid_Rborist")
MULFAM_fit_ranger <- fit_test(MULFAM_set_ranger)
MULFAM_fit_Rborist <- fit_test(MULFAM_set_Rborist)

# Extraire résultats d'intérêt : graphes et resultats
MULFAM_fit_ranger_resultats <- MULFAM_fit_ranger$results
MULFAM_fit_ranger_bestTune <- MULFAM_fit_ranger$bestTune
MULFAM_fit_Rborist_resultats <- MULFAM_fit_Rborist$results
MULFAM_fit_Rborist_bestTune <- MULFAM_fit_Rborist$bestTune

MULFAM_fit_ranger_GINI <- MULFAM_fit_ranger_resultats %>% filter (splitrule == "gini")
MULFAM_fit_ranger_ET <- MULFAM_fit_ranger_resultats %>% filter (splitrule == "extratrees")

MULFAM_mod_ranger_kappa_GINI <- modelFit(X=MULFAM_fit_ranger_GINI[,1:2], Y=MULFAM_fit_ranger_GINI$Kappa,  type="Kriging", formula=Y~mtry+min.node.size+mtry:min.node.size+I(mtry^2)+I(min.node.size^2))
MULFAM_mod_ranger_accu_GINI <- modelFit(X=MULFAM_fit_ranger_GINI[,1:2], Y=MULFAM_fit_ranger_GINI$Accuracy,  type="Kriging", formula=Y~mtry+min.node.size+mtry:min.node.size+I(mtry^2)+I(min.node.size^2))
MULFAM_mod_ranger_kappa_ET <- modelFit(X=MULFAM_fit_ranger_ET[,1:2], Y=MULFAM_fit_ranger_ET$Kappa,  type="Kriging", formula=Y~mtry+min.node.size+mtry:min.node.size+I(mtry^2)+I(min.node.size^2))
MULFAM_mod_ranger_accu_ET <- modelFit(X=MULFAM_fit_ranger_ET[,1:2], Y=MULFAM_fit_ranger_ET$Accuracy,  type="Kriging", formula=Y~mtry+min.node.size+mtry:min.node.size+I(mtry^2)+I(min.node.size^2))

MULFAM_pred_ranger_GINI <- expand.grid(MULFAM_fit_ranger_GINI[,1:2])
colnames(MULFAM_pred_ranger_GINI) <- c("mtry", "min.node.size")
MULFAM_pred_ranger_GINI2 <- NULL
MULFAM_pred_ranger_GINI2$Kappa <- modelPredict(MULFAM_mod_ranger_kappa_GINI, MULFAM_pred_ranger_GINI)
MULFAM_pred_ranger_GINI2$Accuracy <- modelPredict(MULFAM_mod_ranger_accu_GINI, MULFAM_pred_ranger_GINI)
MULFAM_pred_ranger_GINI <- cbind(MULFAM_pred_ranger_GINI, MULFAM_pred_ranger_GINI2)

MULFAM_pred_ranger_ET <- expand.grid(MULFAM_fit_ranger_ET[,1:2])
colnames(MULFAM_pred_ranger_ET) <- c("mtry", "min.node.size")
MULFAM_pred_ranger_ET2 <- NULL
MULFAM_pred_ranger_ET2$Kappa <- modelPredict(MULFAM_mod_ranger_kappa_ET, MULFAM_pred_ranger_ET)
MULFAM_pred_ranger_ET2$Accuracy <- modelPredict(MULFAM_mod_ranger_accu_ET, MULFAM_pred_ranger_ET)
MULFAM_pred_ranger_ET <- cbind(MULFAM_pred_ranger_ET, MULFAM_pred_ranger_ET2)

MULFAM_fit_ranger_Gini_kappa_graphe <- ggplot() +
   geom_raster(data = MULFAM_pred_ranger_GINI, aes(x = mtry, y = min.node.size, fill = Kappa), interpolate = TRUE) +
   geom_tile(data = MULFAM_fit_ranger_GINI, aes(x = mtry, y = min.node.size, fill = Kappa), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "F", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5)) +
   theme(legend.position="bottom")
MULFAM_fit_ranger_Gini_accu_graphe <- ggplot() +
   geom_raster(data = MULFAM_pred_ranger_GINI, aes(x = mtry, y = min.node.size, fill = Accuracy), interpolate = TRUE) +
   geom_tile(data = MULFAM_fit_ranger_GINI, aes(x = mtry, y = min.node.size, fill = Accuracy), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "G", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5)) +
   theme(legend.position="bottom")

MULFAM_fit_ranger_ET_kappa_graphe <- ggplot() +
   geom_raster(data = MULFAM_pred_ranger_ET, aes(x = mtry, y = min.node.size, fill = Kappa), interpolate = TRUE) +
   geom_tile(data = MULFAM_fit_ranger_ET, aes(x = mtry, y = min.node.size, fill = Kappa), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "F", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5)) +
   theme(legend.position="bottom")
MULFAM_fit_ranger_ET_accu_graphe <- ggplot() +
   geom_raster(data = MULFAM_pred_ranger_ET, aes(x = mtry, y = min.node.size, fill = Accuracy), interpolate = TRUE) +
   geom_tile(data = MULFAM_fit_ranger_ET, aes(x = mtry, y = min.node.size, fill = Accuracy), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "G", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5)) +
   theme(legend.position="bottom")

MULFAM_best_ranger <- which.max(MULFAM_fit_ranger_resultats$Kappa)
MULFAM_best_rangergrid <- data.frame(mtry = MULFAM_fit_ranger_resultats[MULFAM_best_ranger,]$mtry, min.node.size =MULFAM_fit_ranger_resultats[MULFAM_best_ranger,]$min.node.size, splitrule =MULFAM_fit_ranger_resultats[MULFAM_best_ranger,]$splitrule)
MULFAM_set_ranger_best <- c("ranger", paste0("tuneGrid  = MULFAM_best_rangergrid"))
MULFAM_fit_ranger_best <- fit_test(MULFAM_set_ranger_best)
MULFAM_fit_ranger_best_resultats <- MULFAM_fit_ranger_best$results

MULFAM_mod_Rborist_kappa <- modelFit(X=MULFAM_fit_Rborist_resultats[,1:2], Y=MULFAM_fit_Rborist_resultats$Kappa,  type="Kriging", formula=Y~predFixed+minNode+predFixed:minNode+I(predFixed^2)+I(minNode^2))
MULFAM_mod_Rborist_accu <-  modelFit(X=MULFAM_fit_Rborist_resultats[,1:2], Y=MULFAM_fit_Rborist_resultats$Accuracy,  type="Kriging", formula=Y~predFixed+minNode+predFixed:minNode+I(predFixed^2)+I(minNode^2))
MULFAM_pred_Rborist <- expand.grid(MULFAM_fit_Rborist_resultats[,1:2])
colnames(MULFAM_pred_Rborist) <- c("predFixed", "minNode")
MULFAM_pred_Rborist2 <- NULL
MULFAM_pred_Rborist2$Kappa <- modelPredict(MULFAM_mod_Rborist_kappa, MULFAM_pred_Rborist)
MULFAM_pred_Rborist2$Accuracy <- modelPredict(MULFAM_mod_Rborist_accu, MULFAM_pred_Rborist)
MULFAM_pred_Rborist <- cbind(MULFAM_pred_Rborist, MULFAM_pred_Rborist2)

MULFAM_fit_Rborist_kappa_graphe <- ggplot() +
   geom_raster(data = MULFAM_pred_Rborist, aes(x = predFixed, y = minNode, fill = Kappa), interpolate = TRUE) +
   geom_tile(data = MULFAM_fit_Rborist_resultats, aes(x = predFixed, y = minNode, fill = Kappa), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "F", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5)) +
   theme(legend.position="bottom")
MULFAM_fit_Rborist_accu_graphe <- ggplot() +
   geom_raster(data = MULFAM_pred_Rborist, aes(x = predFixed, y = minNode, fill = Accuracy), interpolate = TRUE) +
   geom_tile(data = MULFAM_fit_Rborist_resultats, aes(x = predFixed, y = minNode, fill = Accuracy), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "G", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5)) +
   theme(legend.position="bottom")

MULFAM_best_Rborist <- which.max(MULFAM_fit_Rborist_resultats$Kappa)
MULFAM_best_Rboristgrid <- data.frame(predFixed = MULFAM_fit_Rborist_resultats[MULFAM_best_Rborist,]$predFixed, minNode =MULFAM_fit_Rborist_resultats[MULFAM_best_Rborist,]$minNode)
MULFAM_set_Rborist_best <- c("Rborist", paste0("tuneGrid  = MULFAM_best_Rboristgrid"))
MULFAM_fit_Rborist_best <- fit_test(MULFAM_set_Rborist_best)
MULFAM_fit_Rborist_best_resultats <- MULFAM_fit_Rborist_best$results

# Lance modèle RANGER optimal
MULFAM_set_ranger_best <- c("ranger", paste0("tuneGrid  = MULFAM_best_rangergrid, num.trees = 6"))
MULFAM_fit_ranger_best <- fit_test(MULFAM_set_ranger_best)
MULFAM_fit_ranger_best_resultats <- MULFAM_fit_ranger_best$results

# Lance modèle RBORIST optimal
MULFAM_set_Rborist_best <- c("Rborist", paste0("tuneGrid  = MULFAM_best_Rboristgrid, ntrees = 2"))
MULFAM_fit_Rborist_best <- fit_test(MULFAM_set_Rborist_best)
MULFAM_fit_Rborist_best_resultats <- MULFAM_fit_Rborist_best$results


#########################################################
#     PERFORMANCE DES MODELES SUR LOT D'EVALUATION      #
#########################################################

# Règle la liste de prédiction et lance la classification
MULFAM_evaluation <- MULFAM_lot_evaluation
MULFAM_evaluation$reference <- as.factor(MULFAM_evaluation$family)

start_time <- Sys.time()     # Démarre chrono
cmd <- paste0("train(family ~ ., method = 'ranger', data = MULFAM_lot_appr_opti,", MULFAM_set_ranger_best[2], ")") # Construction de la commande
MULFAM_fit_ranger_final <- eval(parse(text = cmd))     # Exécution de la commande
MULFAM_pred_ranger_final <- predict(object = MULFAM_fit_ranger_final, newdata = MULFAM_lot_evaluation)
MULFAM_CM_ranger_final <- confusionMatrix(data = MULFAM_pred_ranger_final, reference = MULFAM_lot_evaluation$family)
MULFAM_resultats_ranger <- c(MULFAM_CM_ranger_final$byClass["Accuracy"], MULFAM_CM_ranger_final$byClass["Kappa"])
end_time <- Sys.time()     # Stop chrono
MULFAM_temps_ranger <- difftime(end_time, start_time)
MULFAM_temps_ranger <- MULFAM_temps_ranger %>% as.numeric %>% round(.,2)

start_time <- Sys.time()            # Démarre chrono
cmd <- paste0("train(family ~ ., method = 'Rborist', data = MULFAM_lot_appr_opti,", MULFAM_set_Rborist_best[2], ")") # Construction de la commande
MULFAM_fit_Rborist_final <- eval(parse(text = cmd))     # Exécution de la commande
MULFAM_pred_Rborist_final <- predict(object = MULFAM_fit_Rborist_final, newdata = MULFAM_lot_evaluation)
MULFAM_CM_Rborist_final <- confusionMatrix(data = MULFAM_pred_Rborist_final, reference = MULFAM_lot_evaluation$family)
MULFAM_resultats_Rborist <- c(MULFAM_CM_Rborist_final$byClass["Accuracy"], MULFAM_CM_Rborist_final$byClass["Kappa"])
end_time <- Sys.time()              # Stop chrono
MULFAM_temps_Rborist <- difftime(end_time, start_time)
MULFAM_temps_Rborist <- MULFAM_temps_Rborist %>% as.numeric %>% round(.,2)

MULFAM_resultat_Rborist <- c(MULFAM_CM_Rborist_final$overall["Accuracy"], MULFAM_CM_Rborist_final$overall["Kappa"], MULFAM_temps_Rborist)
MULFAM_resultat_ranger <- c(MULFAM_CM_ranger_final$overall["Accuracy"], MULFAM_CM_ranger_final$overall["Kappa"], MULFAM_temps_ranger)
MULFAM_RF_resultat <- rbind(MULFAM_resultat_ranger, MULFAM_resultat_Rborist)
colnames(MULFAM_RF_resultat) <- c("Précision", "Kappa", "Durée (min)")
rownames(MULFAM_RF_resultat) <- c("Ranger", "Rborist")

# Suppression gros fichiers intermédiaires, avant sauvegarde
# rm(dataset, MULFAM_evaluation, MULFAM_lot_appr_opti, MULFAM_lot_evaluation,
#    MULFAM_fit_rpart_cp, MULFAM_fit_rpartcost, MULFAM_fit_rpartcost_best, MULFAM_fit_ctree_criterion, MULFAM_fit_c50tree,
#    MULFAM_fit_Rborist, MULFAM_fit_Rborist_best, MULFAM_fit_Rborist_final,
#    MULFAM_fit_ranger, MULFAM_fit_ranger_best, MULFAM_fit_ranger_final)

save.image(file = "EKR-Champis-AnalyseMultiFam.RData")     # Sauvegarde données pour rapport
load(file = "EKR-Champis-AnalyseMultiFam.RData")     # Chargement données pour rapport
