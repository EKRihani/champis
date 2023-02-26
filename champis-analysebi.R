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
#URL <- "https://github.com/EKRihani/champis/raw/master/MushroomDataset.zip"      # URL de mon repo
# download.file(URL, fichier_data)
fichier_data <- "~/projects/champis/MushroomDataset.zip" # FICHIER LOCAL

fichier_data <- unzip(fichier_data, "MushroomDataset/secondary_data.csv")
dataset <- read.csv(fichier_data, header = TRUE, sep = ";", stringsAsFactors = TRUE)


##############################################################################
#     CREATION DES LOTS D'ENTRAINEMENT, VALIDATION, EVALUATION + GRAPHES     #
##############################################################################

split1 <- 0.08
split2 <- 0.08
# Creation lots d'entrainement/validation (92%) et BI_evaluation (8%)
set.seed(007)
index1 <- createDataPartition(y = dataset$cap.diameter, times = 1, p = split1, list = FALSE)
BI_lot_appr_opti <- dataset[-index1,]
BI_lot_evaluation <- dataset[index1,]

# Creation lots d'entrainement (92%) et validation (8%)
set.seed(1337)
index2 <- createDataPartition(y = BI_lot_appr_opti$cap.diameter, times = 1, p = split2, list = FALSE)
BI_lot_apprentissage <- BI_lot_appr_opti[-index2,]
BI_lot_evaluation <- BI_lot_appr_opti[index2,]


##############################################################################
#     ANALYSE DU LOT D'ENTRAINEMENT AVEC MODELES DE CARET, BICLASSIFIEUR     #
##############################################################################
# https://topepo.github.io/caret/available-models.html
# names(getModelInfo())
# getModelInfo(Rborist)

BI_ratioSpeSen <- 10
BI_n_folds <- 5
# Définition de fonction : lance le modèle avec les paramètres données, évalue la performance (spécificité), renvoie les résultats de fitting
fit_test <- function(fcn_model){
   set.seed(1)
   tr_ctrl <- trainControl(classProbs = TRUE, summaryFunction = twoClassSummary, method = "cv", number = BI_n_folds)   # Règle paramètres d'évaluation performance à twoClassSummary (ROC, Sens, Spec), avec cross-validation (10-fold)
   cmd <- paste0("train(class ~ ., method = '",      # Construit commande, évaluation de performance par Spécificité
                 fcn_model[1], 
                 "', data = BI_lot_appr_opti, trControl = tr_ctrl, metric = 'Spec', ", 
                 fcn_model[2],")")
   fitting <- eval(parse(text = cmd))        # Lance commande
   fitting
}

# Parallélisation (A TESTER !!!)
# library(doParallel)
# cl <- makeCluster(spec = 5, type = "PSOCK")
# registerDoParallel(cl)

# Modèles type Discriminant Analysis (LDA2, PDA)
BI_grid_lda_dimen <- data.frame(dimen = seq(from = 1, to = 52, by = 3))
BI_grid_pda_lambda <- data.frame(lambda = seq(from = 1, to = 61, by = 3))
BI_set_lda2_dim <- c("lda2", "tuneGrid  = BI_grid_lda_dimen")
BI_set_pda_lambda <- c("pda", "tuneGrid  = BI_grid_pda_lambda")
BI_fit_lda2_dim <- fit_test(BI_set_lda2_dim)
system.time(fit_test(BI_set_lda2_dim))      #### CHRONO
BI_fit_pda_lambda <- fit_test(BI_set_pda_lambda)
system.time(fit_test(BI_set_pda_lambda))  #### CHRONO
# Extraire résultats d'intérêt : graphes et resultats
BI_fit_lda2_dim_graphe <- ggplot(data = BI_fit_lda2_dim$results, aes(x = dimen, y = Spec)) + geom_point() + ylab("Spécificité")
BI_fit_lda2_dim_grapheROC <- ggplot(data = BI_fit_lda2_dim$results, aes(x = dimen, y = ROC)) + geom_point() + ylab("ROC")
BI_fit_lda2_dim_results <- BI_fit_lda2_dim$results
BI_fit_pda_lambda_results <- BI_fit_pda_lambda$results
BI_fit_pda_lambda_graphe <- ggplot(data = BI_fit_pda_lambda$results, aes(x = lambda, y = Spec)) + geom_point() + ylab("Spécificité")
BI_fit_pda_lambda_grapheROC <- ggplot(data = BI_fit_pda_lambda$results, aes(x = lambda, y = ROC)) + geom_point() + ylab("ROC")

## Fin parallélisation (A TESTER !!!)
# stopCluster(cl)


# Modèles type Generalized Additive Model (GAM LOESS)          A RATIONNALISER !!!
BI_set_gamLoess_span <-  c("gamLoess", "tuneGrid  = data.frame(span = seq(from = 0.01, to = 1, by = 0.24), degree = 1)")
BI_set_gamLoess_degree <-  c("gamLoess", "tuneGrid  = data.frame(degree = c(0, 1), span = 0.5)")
BI_fit_gamLoess_span <- fit_test(BI_set_gamLoess_span)
BI_fit_gamLoess_degree <- fit_test(BI_set_gamLoess_degree)
# Extraire résultats d'intérêt : graphes et resultats
BI_fit_gamLoess_span_graphe <- ggplot(BI_fit_gamLoess_span)
BI_fit_gamLoess_span_results <- BI_fit_gamLoess_span$results
BI_fit_gamLoess_degree_graphe <- ggplot(BI_fit_gamLoess_degree)
BI_fit_gamLoess_degree_results <- BI_fit_gamLoess_degree$results

# Modèles types arbres (RPART, RPARTCOST, CTREE, C50TREE)

BI_grid_rpart_cp <- data.frame(cp = 10^seq(from = -5, to = -1, by = .5))

BI_LHS <- nolhDesign(dimension = 2, range = c(0, 1))$design     # Hypercube latin quasi-orthogonal
BI_grid_rpartcost <- data.frame(BI_LHS)
colnames(BI_grid_rpartcost) <- c("cp", "Cost")
BI_grid_rpartcost$cp <- (BI_grid_rpartcost$cp*1e-2+1e-5)
BI_grid_rpartcost$Cost <- BI_grid_rpartcost$Cost*2.5+1e-3

BI_set_rpart_cp <- c("rpart", "tuneGrid  = BI_grid_rpart_cp")
BI_set_rpartcost <- c("rpartCost", "tuneGrid  = BI_grid_rpartcost")     # A TESTER !!!

BI_set_ctree_criterion <- c("ctree", "tuneGrid  = data.frame(mincriterion = c(0.01, 0.25, 0.5, 0.75, 0.99))")
BI_set_c50tree <- c("C5.0Tree", "")
system.time(fit_test(BI_set_rpart_cp))    ####### CHRONO
BI_fit_rpart_cp <- fit_test(BI_set_rpart_cp)
BI_fit_rpartcost <- fit_test(BI_set_rpartcost)
BI_fit_ctree_criterion <- fit_test(BI_set_ctree_criterion)
BI_fit_c50tree <- fit_test(BI_set_c50tree)

# Extraire résultats d'intérêt : graphes et resultats
BI_fit_rpart_cp_results <- BI_fit_rpart_cp$results
BI_fit_rpart_cp_graphe <- ggplot(data = BI_fit_rpart_cp$results, aes(x = cp, y = Spec)) + geom_point() + ylab("Spécificité") + scale_x_log10()

BI_fit_rpartcost_results <- BI_fit_rpartcost$results
BI_mod_rpartcost_spec <- modelFit(X=BI_fit_rpartcost_results[,1:2], Y=BI_fit_rpartcost_results$Spec,  type="Kriging", formula=Y~cp+Cost+cp:Cost+I(cp^2)+I(Cost^2))
BI_mod_rpartcost_sens <-  modelFit(X=BI_fit_rpartcost_results[,1:2], Y=BI_fit_rpartcost_results$Sens,  type="Kriging", formula=Y~cp+Cost+cp:Cost+I(cp^2)+I(Cost^2))
BI_pred_rpartcost <- expand.grid(BI_fit_rpartcost_results[,1:2])
colnames(BI_pred_rpartcost) <- c("Cost", "cp")
BI_pred_rpartcost2 <- NULL
BI_pred_rpartcost2$Spec <- modelPredict(BI_mod_rpartcost_spec, BI_pred_rpartcost)
BI_pred_rpartcost2$Sens <- modelPredict(BI_mod_rpartcost_sens, BI_pred_rpartcost)
BI_pred_rpartcost <- cbind(BI_pred_rpartcost, BI_pred_rpartcost2)
BI_fit_rpartcost_spec_graphe <- ggplot() +
   geom_raster(data = BI_pred_rpartcost, aes(x = Cost, y = cp, fill = Spec), interpolate = TRUE) +
   geom_tile(data = BI_fit_rpartcost_results, aes(x = Cost, y = cp, fill = Spec), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "F", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))
BI_fit_rpartcost_sens_graphe <- ggplot() +
   geom_raster(data = BI_pred_rpartcost, aes(x = Cost, y = cp, fill = Sens), interpolate = TRUE) +
   geom_tile(data = BI_fit_rpartcost_results, aes(x = Cost, y = cp, fill = Sens), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "G", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))

BI_fit_ctree_criterion_graphe <- ggplot(BI_fit_ctree_criterion)
BI_fit_ctree_criterion_results <- BI_fit_ctree_criterion$results
BI_fit_c50tree_results <- BI_fit_c50tree$results

# Meilleur modèle CART
BI_best_rpartcost <- which.max(BI_fit_rpartcost_results$Spec^BI_ratioSpeSen*BI_fit_rpartcost_results$Sens)
BI_best_rpartcostgrid <- data.frame(Cost = BI_fit_rpartcost_results[BI_best_rpartcost,]$Cost, cp =BI_fit_rpartcost_results[BI_best_rpartcost,]$cp)
BI_set_rpartcost_best <- c("rpartCost", paste0("tuneGrid  = BI_best_rpartcostgrid"))
BI_fit_rpartcost_best <- fit_test(BI_set_rpartcost_best)
BI_fit_rpartcost_best_results <- BI_fit_rpartcost_best$results


# Modèles type Random Forest (RFERNS, RANGER, RBORIST)
BI_set_rFerns_depth <- c("rFerns", "tuneGrid  = data.frame(depth = 2^(1:5)/2)")

BI_grid_ranger <- data.frame(BI_LHS)
BI_grid_ranger <- rbind(BI_grid_ranger,BI_grid_ranger)
colnames(BI_grid_ranger) <- c("mtry", "min.node.size")
#colnames(BI_grid_ranger) <- c("mtry", "min.node.size, num.trees")     # Pour num.treees, à tester
BI_grid_ranger$splitrule <- c(rep("extratrees", 17), rep("gini", 17))
BI_grid_ranger$mtry <- round(1+BI_grid_ranger$mtry*48,0)       # Si arrondi : prendre des multiples de 16 (car 17 points pour 2d)
BI_grid_ranger$min.node.size <- round(1+BI_grid_ranger$min.node.size*32,0)
#BI_grid_ranger$num.trees <- round(1+BI_grid_ranger$num.trees*9,0) # VOIR SI LE NUM.TREES MARCHE ???

BI_grid_Rborist <- data.frame(BI_LHS)
#colnames(BI_grid_Rborist) <- c("predFixed", "minNode", "ntrees")
colnames(BI_grid_Rborist) <- c("predFixed", "minNode")
BI_grid_Rborist$predFixed <- round(1+BI_grid_Rborist$predFixed*32,0)
BI_grid_Rborist$minNode <- round(1+BI_grid_Rborist$minNode*16,0)
#BI_grid_Rborist$ntrees <- round(1+BI_grid_Rborist$ntrees*5,0)

BI_set_ranger <- c("ranger", "tuneGrid  = BI_grid_ranger, num.trees = 6") # A TESTER
BI_set_Rborist <- c("Rborist", "tuneGrid  = BI_grid_Rborist")
#system.time(fit_test(BI_set_ranger_mtry))  ####### CHRONO
BI_fit_rFerns_depth <- fit_test(BI_set_rFerns_depth)
BI_fit_ranger <- fit_test(BI_set_ranger)
BI_fit_Rborist <- fit_test(BI_set_Rborist)

# Extraire résultats d'intérêt : graphes et resultats
BI_fit_rFerns_depth_graphe <- ggplot(BI_fit_rFerns_depth)
BI_fit_rFerns_depth_results <- BI_fit_rFerns_depth$results
BI_fit_ranger_results <- BI_fit_ranger$results
BI_fit_ranger_bestTune <- BI_fit_ranger$bestTune
BI_fit_Rborist_results <- BI_fit_Rborist$results
BI_fit_Rborist_bestTune <- BI_fit_Rborist$bestTune

BI_fit_ranger_GINI <- BI_fit_ranger_results %>% filter (splitrule == "gini")
BI_fit_ranger_ET <- BI_fit_ranger_results %>% filter (splitrule == "extratrees")
BI_mod_ranger_spec_GINI <- modelFit(X=BI_fit_ranger_GINI[,1:2], Y=BI_fit_ranger_GINI$Spec,  type="Kriging", formula=Y~mtry+min.node.size+mtry:min.node.size+I(mtry^2)+I(min.node.size^2))
BI_mod_ranger_sens_GINI <- modelFit(X=BI_fit_ranger_GINI[,1:2], Y=BI_fit_ranger_GINI$Sens,  type="Kriging", formula=Y~mtry+min.node.size+mtry:min.node.size+I(mtry^2)+I(min.node.size^2))
BI_mod_ranger_spec_ET <- modelFit(X=BI_fit_ranger_ET[,1:2], Y=BI_fit_ranger_ET$Spec,  type="Kriging", formula=Y~mtry+min.node.size+mtry:min.node.size+I(mtry^2)+I(min.node.size^2))
BI_mod_ranger_sens_ET <- modelFit(X=BI_fit_ranger_ET[,1:2], Y=BI_fit_ranger_ET$Sens,  type="Kriging", formula=Y~mtry+min.node.size+mtry:min.node.size+I(mtry^2)+I(min.node.size^2))

BI_pred_ranger_GINI <- expand.grid(BI_fit_ranger_GINI[,1:2])
colnames(BI_pred_ranger_GINI) <- c("mtry", "min.node.size")
BI_pred_ranger_GINI2 <- NULL
BI_pred_ranger_GINI2$Spec <- modelPredict(BI_mod_ranger_spec_GINI, BI_pred_ranger_GINI)
BI_pred_ranger_GINI2$Sens <- modelPredict(BI_mod_ranger_sens_GINI, BI_pred_ranger_GINI)
BI_pred_ranger_GINI <- cbind(BI_pred_ranger_GINI, BI_pred_ranger_GINI2)

BI_pred_ranger_ET <- expand.grid(BI_fit_ranger_ET[,1:2])
colnames(BI_pred_ranger_ET) <- c("mtry", "min.node.size")
BI_pred_ranger_ET2 <- NULL
BI_pred_ranger_ET2$Spec <- modelPredict(BI_mod_ranger_spec_ET, BI_pred_ranger_ET)
BI_pred_ranger_ET2$Sens <- modelPredict(BI_mod_ranger_sens_ET, BI_pred_ranger_ET)
BI_pred_ranger_ET <- cbind(BI_pred_ranger_ET, BI_pred_ranger_ET2)

BI_fit_ranger_Gini_spec_graphe <- ggplot() +
   geom_raster(data = BI_pred_ranger_GINI, aes(x = mtry, y = min.node.size, fill = Spec), interpolate = TRUE) +
   geom_tile(data = BI_fit_ranger_GINI, aes(x = mtry, y = min.node.size, fill = Spec), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "F", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))
BI_fit_ranger_Gini_sens_graphe <- ggplot() +
   geom_raster(data = BI_pred_ranger_GINI, aes(x = mtry, y = min.node.size, fill = Sens), interpolate = TRUE) +
   geom_tile(data = BI_fit_ranger_GINI, aes(x = mtry, y = min.node.size, fill = Sens), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "G", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))

BI_fit_ranger_ET_spec_graphe <- ggplot() +
   geom_raster(data = BI_pred_ranger_ET, aes(x = mtry, y = min.node.size, fill = Spec), interpolate = TRUE) +
   geom_tile(data = BI_fit_ranger_ET, aes(x = mtry, y = min.node.size, fill = Spec), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "F", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))
BI_fit_ranger_ET_sens_graphe <- ggplot() +
   geom_raster(data = BI_pred_ranger_ET, aes(x = mtry, y = min.node.size, fill = Sens), interpolate = TRUE) +
   geom_tile(data = BI_fit_ranger_ET, aes(x = mtry, y = min.node.size, fill = Sens), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "G", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))

BI_best_ranger <- which.max(BI_fit_ranger_results$Spec^BI_ratioSpeSen*BI_fit_ranger_results$Sens)
BI_best_rangergrid <- data.frame(mtry = BI_fit_ranger_results[BI_best_ranger,]$mtry, min.node.size =BI_fit_ranger_results[BI_best_ranger,]$min.node.size, splitrule =BI_fit_ranger_results[BI_best_ranger,]$splitrule)
BI_set_ranger_best <- c("ranger", paste0("tuneGrid  = BI_best_rangergrid"))
BI_fit_ranger_best <- fit_test(BI_set_ranger_best)
BI_fit_ranger_best_results <- BI_fit_ranger_best$results

BI_mod_Rborist_spec <- modelFit(X=BI_fit_Rborist_results[,1:2], Y=BI_fit_Rborist_results$Spec,  type="Kriging", formula=Y~predFixed+minNode+predFixed:minNode+I(predFixed^2)+I(minNode^2))
BI_mod_Rborist_sens <-  modelFit(X=BI_fit_Rborist_results[,1:2], Y=BI_fit_Rborist_results$Sens,  type="Kriging", formula=Y~predFixed+minNode+predFixed:minNode+I(predFixed^2)+I(minNode^2))
BI_pred_Rborist <- expand.grid(BI_fit_Rborist_results[,1:2])
colnames(BI_pred_Rborist) <- c("predFixed", "minNode")
BI_pred_Rborist2 <- NULL
BI_pred_Rborist2$Spec <- modelPredict(BI_mod_Rborist_spec, BI_pred_Rborist)
BI_pred_Rborist2$Sens <- modelPredict(BI_mod_Rborist_sens, BI_pred_Rborist)
BI_pred_Rborist <- cbind(BI_pred_Rborist, BI_pred_Rborist2)

BI_fit_Rborist_spec_graphe <- ggplot() +
   geom_raster(data = BI_pred_Rborist, aes(x = predFixed, y = minNode, fill = Spec), interpolate = TRUE) +
   geom_tile(data = BI_fit_Rborist_results, aes(x = predFixed, y = minNode, fill = Spec), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "F", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))
BI_fit_Rborist_sens_graphe <- ggplot() +
   geom_raster(data = BI_pred_Rborist, aes(x = predFixed, y = minNode, fill = Sens), interpolate = TRUE) +
   geom_tile(data = BI_fit_Rborist_results, aes(x = predFixed, y = minNode, fill = Sens), color = "black", linewidth =.5) +
   scale_fill_viridis_c(option = "G", direction = 1) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))

BI_best_Rborist <- which.max(BI_fit_Rborist_results$Spec^BI_ratioSpeSen*BI_fit_Rborist_results$Sens)
BI_best_Rboristgrid <- data.frame(predFixed = BI_fit_Rborist_results[BI_best_Rborist,]$predFixed, minNode =BI_fit_Rborist_results[BI_best_Rborist,]$minNode)
BI_set_Rborist_best <- c("Rborist", paste0("tuneGrid  = BI_best_Rboristgrid"))
BI_fit_Rborist_best <- fit_test(BI_set_Rborist_best)
BI_fit_Rborist_best_results <- BI_fit_Rborist_best$results

# Lance modèle RANGER optimal
BI_set_ranger_best <- c("ranger", paste0("tuneGrid  = BI_best_rangergrid, num.trees = 6"))
BI_fit_ranger_best <- fit_test(BI_set_ranger_best)
BI_fit_ranger_best_results <- BI_fit_ranger_best$results

# Lance modèle RBORIST optimal
BI_set_Rborist_best <- c("Rborist", paste0("tuneGrid  = BI_best_Rboristgrid, ntrees = 2"))
BI_fit_Rborist_best <- fit_test(BI_set_Rborist_best)
BI_fit_Rborist_best_results <- BI_fit_Rborist_best$results


#########################################################
#     PERFORMANCE DES MODELES SUR LOT D'EVALUATION      #
#########################################################

# Règle la liste de prédiction et lance la classification
BI_evaluation <- BI_lot_evaluation
BI_evaluation$reference <- as.logical(as.character(recode_factor(BI_evaluation$class, e = TRUE, p = FALSE))) # Bascule en booléen

# Passe .$reference de booléen à facteur, puis calcule la matrice de confusion
BI_evaluation$reference <- as.factor(BI_evaluation$reference)


start_time <- Sys.time()     # Démarre chrono
cmd <- paste0("train(class ~ ., method = 'ranger', data = BI_lot_appr_opti,", BI_set_ranger_best[2], ")") # Construction de la commande
BI_fit_ranger_final <- eval(parse(text = cmd))     # Exécution de la commande
BI_pred_ranger_final <- predict(object = BI_fit_ranger_final, newdata = BI_lot_evaluation)
BI_CM_ranger_final <- confusionMatrix(data = BI_pred_ranger_final, reference = BI_lot_evaluation$class)
BI_resultats_ranger <- c(BI_CM_ranger_final$byClass["Sensitivity"], BI_CM_ranger_final$byClass["Specificity"], BI_CM_ranger_final$byClass["F1"])
end_time <- Sys.time()     # Stop chrono
BI_temps_ranger <- difftime(end_time, start_time)
BI_temps_ranger <- BI_temps_ranger %>% as.numeric %>% round(.,2)

start_time <- Sys.time()            # Démarre chrono
cmd <- paste0("train(class ~ ., method = 'Rborist', data = BI_lot_appr_opti,", BI_set_Rborist_best[2], ")") # Construction de la commande
BI_fit_Rborist_final <- eval(parse(text = cmd))     # Exécution de la commande
BI_pred_Rborist_final <- predict(object = BI_fit_Rborist_final, newdata = BI_lot_evaluation)
BI_CM_Rborist_final <- confusionMatrix(data = BI_pred_Rborist_final, reference = BI_lot_evaluation$class)
BI_resultats_Rborist <- c(BI_CM_Rborist_final$byClass["Sensitivity"], BI_CM_Rborist_final$byClass["Specificity"], BI_CM_Rborist_final$byClass["F1"])
end_time <- Sys.time()              # Stop chrono
BI_temps_Rborist <- difftime(end_time, start_time)
BI_temps_Rborist <- BI_temps_Rborist %>% as.numeric %>% round(.,2)

BI_resultat_Rborist <- c(BI_CM_Rborist_final$byClass["Sensitivity"], BI_CM_Rborist_final$byClass["Specificity"], BI_CM_Rborist_final$byClass["F1"], BI_temps_Rborist)
BI_resultat_ranger <- c(BI_CM_ranger_final$byClass["Sensitivity"], BI_CM_ranger_final$byClass["Specificity"], BI_CM_ranger_final$byClass["F1"], BI_temps_ranger)
BI_RF_resultat <- rbind(BI_resultat_ranger, BI_resultat_Rborist)
colnames(BI_RF_resultat) <- c("Sensibilité", "Spécificité", "F1 score", "Durée (min)")
rownames(BI_RF_resultat) <- c("Ranger", "Rborist")

# Suppression gros fichiers intermédiaires, avant sauvegarde
rm(dataset, BI_evaluation, BI_lot_appr_opti, BI_lot_apprentissage, BI_lot_evaluation,
   BI_fit_pda_lambda, BI_fit_lda2_dim, BI_fit_gamLoess_degree, BI_fit_gamLoess_span,
   BI_fit_rpart_cp, BI_fit_rpartcost, BI_fit_rpartcost_best,
   BI_fit_ctree_criterion, BI_fit_c50tree, BI_fit_rFerns_depth, 
   BI_fit_Rborist, BI_fit_Rborist_best, BI_fit_Rborist_final,
   BI_fit_ranger, BI_fit_ranger_best, BI_fit_ranger_final)
save.image(file = "EKR-Champis-AnalyseBi.RData")     # Sauvegarde données pour rapport
load(file = "EKR-Champis-AnalyseBi.RData")     # Chargement données pour rapport
