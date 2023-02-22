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

########################################
#  FORMATTAGE / NETTOYAGE DES DONNEES  #
########################################

# Récupération de la structure des données
structure_initial <- sapply(X = dataset, FUN = class, simplify = TRUE)  # Classes des variables
unique_length <- function (x) {length(unique(x))}                       # FONCTION : compter les niveaux d'une variable
structure_uniques <- sapply(dataset, FUN = unique_length)               # Comptage des niveaux de toutes les variables
structure_dataset <- data.frame(cbind(structure_initial, structure_uniques))
colnames(structure_dataset) <- c("Type", "Niveaux")
structure_dataset$Niveaux <- as.numeric(as.character(structure_dataset$Niveaux))
dataset_noms <- row.names(structure_dataset)


#Réduction taille à 10% POUR BROUILLON
#reduc_index <- createDataPartition(y = dataset$cap.diameter, times = 1, p = 0.1, list = FALSE)
#dataset <- dataset[reduc_index,]

################################
#     ANALYSE INTRODUCTIVE     #
################################

# Résumés introductifs
synthese_nombre <- nrow(dataset)  # Comptage champis
synthese_dataset <- summary(dataset) # Résumé des catégories

##############################################################################
#     CREATION DES LOTS D'ENTRAINEMENT, VALIDATION, EVALUATION + GRAPHES     #
##############################################################################

split1 <- 0.08
split2 <- 0.08
# Creation lots d'entrainement/validation (92%) et evaluation (8%)
set.seed(1)
index1 <- createDataPartition(y = dataset$cap.diameter, times = 1, p = split1, list = FALSE)
lot_appr_opti <- dataset[-index1,]
lot_evaluation <- dataset[index1,]

# Creation lots d'entrainement (92%) et validation (8%)
set.seed(1)
index2 <- createDataPartition(y = lot_appr_opti$cap.diameter, times = 1, p = split2, list = FALSE)
lot_apprentissage <- lot_appr_opti[-index2,]
lot_evaluation <- lot_appr_opti[index2,]

# Tracage des distributions monovariées du lot (entrainement+validation)
l <- nrow(structure_dataset)
for (n in 1:l){
   titre_graphe <- paste("Distribution de", dataset_noms[n], "des champignons")
   plot <- lot_appr_opti %>%
      ggplot(aes_string(x = dataset_noms[n])) +      # aes_string permet l'utilisation de chaîne au lieu de nom de variable
      ggtitle(titre_graphe) +
      ylab("") +
      xlab(dataset_noms[n]) +
      theme_bw()
   if(structure_dataset$Type[n] %in% c("integer", "numeric")) # Histogramme pour numériques, Barplot pour autres
   {plot <- plot + geom_histogram(fill = "gray45")}
   else
   {plot <- plot + geom_bar(fill = "gray45")}
   nom_graph <- paste0("study_distrib_", dataset_noms[n])   # Concaténer "plot_distrib" avec nom de colonne
   assign(nom_graph, plot)                 # Attribuer le graphique au nom plot_distrib_colname
}


#####################################################
#     ANALYSE DESCRIPTIVE DU LOT D'ENTRAINEMENT     #
#####################################################

# Trace toutes les distributions monovariées du lot d'entrainement (toxique vs comestible)
l <- nrow(structure_dataset)

for (n in 2:l){    # La colonne 1 (class) n'est pas tracée (attribut de fill/couleur !)
   titre_graphe <- paste("Distribution de", dataset_noms[n], "des champignons")
   plot <- lot_apprentissage %>%
      ggplot(aes_string(x = dataset_noms[n], fill = lot_apprentissage$class)) + # aes_string permet l'utilisation de chaîne au lieu de nom de variable
      ggtitle(titre_graphe) +
      ylab("Frequency") +
      xlab(dataset_noms[n]) +
      scale_y_log10() +
      theme_bw()
   if(structure_dataset$Type[n] %in% c("integer", "numeric"))   # Histogramme pour numériques, Barplot pour autres
   {plot <- plot + geom_histogram()}
   else
   {plot <- plot + geom_bar()}
   nom_graph <- paste0("train_distrib_",dataset_noms[n])    # Concaténer "plot_distrib" avec nom de colonne
   assign(nom_graph, plot)                 # Attribuer le graphique au nom plot_distrib_colname
}

# Graphes de corrélation pour une (petite) sélection de critères
paires_graphes <- ggpairs(
   lot_apprentissage,
   columns = c(2,15,17,10),
   lower = NULL,
   diag = list(continuous = wrap("densityDiag", alpha = .6), 
               discrete = wrap("barDiag")
   ),
   upper = list(continuous = wrap("points", alpha = .3, shape = 20), 
                combo = wrap("dot", alpha = .3, shape = 20),
                discrete = wrap("dot_no_facet", alpha = .3, shape = 20)
   ),
   ggplot2::aes(color = class)
)

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

# Parallélisation (A TESTER !!!)
# library(doParallel)
# cl <- makeCluster(spec = 5, type = "PSOCK")
# registerDoParallel(cl)

# Modèles type Discriminant Analysis (LDA2, PDA)
grid_lda_dimen <- data.frame(dimen = seq(from = 1, to = 52, by = 3))
grid_pda_lambda <- data.frame(lambda = seq(from = 1, to = 61, by = 3))
set_lda2_dim <- c("lda2", "tuneGrid  = grid_lda_dimen")
set_pda_lambda <- c("pda", "tuneGrid  = grid_pda_lambda")
fit_lda2_dim <- fit_test(set_lda2_dim)
system.time(fit_test(set_lda2_dim))      #### CHRONO
fit_pda_lambda <- fit_test(set_pda_lambda)
system.time(fit_test(set_pda_lambda))  #### CHRONO
# Extraire résultats d'intérêt : graphes et resultats
fit_lda2_dim_graphe <- ggplot(data = fit_lda2_dim$results, aes(x = dimen, y = Spec)) + geom_point() + ylab("Spécificité")
fit_lda2_dim_grapheROC <- ggplot(data = fit_lda2_dim$results, aes(x = dimen, y = ROC)) + geom_point() + ylab("ROC")
fit_lda2_dim_results <- fit_lda2_dim$results
fit_pda_lambda_results <- fit_pda_lambda$results
fit_pda_lambda_graphe <- ggplot(data = fit_pda_lambda$results, aes(x = lambda, y = Spec)) + geom_point() + ylab("Spécificité")
fit_pda_lambda_grapheROC <- ggplot(data = fit_pda_lambda$results, aes(x = lambda, y = ROC)) + geom_point() + ylab("ROC")

## Fin parallélisation (A TESTER !!!)
# stopCluster(cl)


# Modèles type Generalized Additive Model (GAM LOESS)
set_gamLoess_span <-  c("gamLoess", "tuneGrid  = data.frame(span = seq(from = 0.01, to = 1, by = 0.24), degree = 1)")
set_gamLoess_degree <-  c("gamLoess", "tuneGrid  = data.frame(degree = c(0, 1), span = 0.5)")
fit_gamLoess_span <- fit_test(set_gamLoess_span)
fit_gamLoess_degree <- fit_test(set_gamLoess_degree)
# Extraire résultats d'intérêt : graphes et resultats
fit_gamLoess_span_graphe <- ggplot(fit_gamLoess_span)
fit_gamLoess_span_results <- fit_gamLoess_span$results
fit_gamLoess_degree_graphe <- ggplot(fit_gamLoess_degree)
fit_gamLoess_degree_results <- fit_gamLoess_degree$results

# Modèles types arbres (RPART, RPARTCOST, CTREE, C50TREE)

grid_rpart_cp <- data.frame(cp = 10^seq(from = -5, to = -1, by = .5))

LHS <- nolhDesign(dimension =2, range = c(0, 1))$design     # Hypercube latin quasi-orthogonal
grid_rpartcost <- data.frame(LHS)
colnames(grid_rpartcost) <- c("cp", "Cost")
grid_rpartcost$cp <- (grid_rpartcost$cp*1e-2+1e-5)
grid_rpartcost$Cost <- grid_rpartcost$Cost*2.5+1e-3

set_rpart_cp <- c("rpart", "tuneGrid  = grid_rpart_cp")
#set_rpartcost_complexity <- c("rpartCost", "tuneGrid  = data.frame(cp = c(1e-5, 1e-4, 1e-3, 1e-2, 0.05), Cost = 1)")
#set_rpartcost_cost <- c("rpartCost", "tuneGrid  = data.frame(Cost = c(0.01, 0.4, 0.7, 1, 1.5, 2, 2.5), cp = .01)")
set_rpartcost <- c("rpartCost", "tuneGrid  = grid_rpartcost")     # A TESTER !!!

set_ctree_criterion <- c("ctree", "tuneGrid  = data.frame(mincriterion = c(0.01, 0.25, 0.5, 0.75, 0.99))")
set_c50tree <- c("C5.0Tree", "")
system.time(fit_test(set_rpart_cp))    ####### CHRONO
fit_rpart_cp <- fit_test(set_rpart_cp)
#fit_rpartcost_complexity <- fit_test(set_rpartcost_complexity)
#fit_rpartcost_cost <- fit_test(set_rpartcost_cost)
fit_rpartcost <- fit_test(set_rpartcost)
fit_ctree_criterion <- fit_test(set_ctree_criterion)
fit_c50tree <- fit_test(set_c50tree)
# Extraire résultats d'intérêt : graphes et resultats
fit_rpart_cp_results <- fit_rpart_cp$results
fit_rpart_cp_graphe <- ggplot(data = fit_rpart_cp$results, aes(x = cp, y = Spec)) + geom_point() + ylab("Spécificité") + scale_x_log10()
#fit_rpartcost_complexity_graphe <- ggplot(fit_rpartcost_complexity)
#fit_rpartcost_complexity_results <- fit_rpartcost_complexity$results
#fit_rpartcost_complexity_bestTune <- fit_rpartcost_complexity$bestTune
#fit_rpartcost_cost_graphe <- ggplot(fit_rpartcost_cost)
#fit_rpartcost_cost_results <- fit_rpartcost_cost$results
#fit_rpartcost_cost_bestTune <- fit_rpartcost_cost$bestTune
fit_rpartcost_results <- fit_rpartcost$results

mod_rpartcost <- modelFit(X=fit_rpartcost_results[,1:2], Y=fit_rpartcost_results$Spec,  type="Kriging", formula=Y~cp+Cost+cp:Cost+I(cp^2)+I(Cost^2))
pred_rpartcost <- expand.grid(fit_rpartcost_results[,1:2])
colnames(pred_rpartcost) <- c("Cost", "cp")
pred_rpartcost$Spec <- modelPredict(mod_rpartcost, pred_rpartcost)
fit_rpartcost_graphe
ggplot() +
   geom_tile(data = pred_rpartcost, aes(x = Cost, y = cp, fill = Spec)) +
   geom_tile(data = fit_rpartcost_results, aes(x = Cost, y = cp, fill = Spec), color = "blue", linewidth =.5) +
   scale_fill_viridis_c(option = "F", direction = 1) +
   theme_bw()
   

fit_ctree_criterion_graphe <- ggplot(fit_ctree_criterion)
fit_ctree_criterion_results <- fit_ctree_criterion$results
fit_c50tree_results <- fit_c50tree$results
# Run best CART model
set_rpartcost_best <- c("rpartCost", paste0("tuneGrid  = data.frame(cp = ",
                                            fit_rpartcost_complexity$bestTune$cp, 
                                            ", Cost = ", fit_rpartcost_cost$bestTune$Cost, ")" ))
fit_rpart_cp_resultsfit_rpart_cp_resultsfit_rpartcost_best <- fit_test(set_rpartcost_best)
fit_rpartcost_best_results <- fit_rpartcost_best$results


# Modèles type Random Forest (RFERNS, RANGER, RBORIST)
set_rFerns_depth <- c("rFerns", "tuneGrid  = data.frame(depth = 2^(1:5)/2)")

LHSa <- lhsDesign(n = 20, dimension = 3, randomized=TRUE, seed=1337)$design
LHSb <- lhsDesign(n = 20, dimension = 3, randomized=TRUE, seed=007)$design
LHS <- rbind(LHSa, LHSb)
LHS <- maximinESE_LHS(LHS)$design
grid_ranger <- data.frame(LHS)
colnames(grid_ranger) <- c("mtry", "min.node.size", "num.trees")
grid_ranger$splitrule <- c(rep("extratrees", 20), rep("gini", 20))
grid_ranger$mtry <- round(1+grid_ranger$mtry*99,0)
grid_ranger$min.node.size <- round(1+grid_ranger$min.node.size*39,0)
grid_ranger$num.trees <- round(1+grid_ranger$num.trees*9,0) # VOIR SI LE NUM.TREES MARCHE ???

LHSa <- maximinESE_LHS(LHSa)$design
grid_Rborist <- data.frame(LHSa)
colnames(grid_Rborist) <- c("predFixed", "minNode", "ntrees")
grid_Rborist$predFixed <- round(1+grid_Rborist$predFixed*39,0)
grid_Rborist$minNode <- round(1+grid_Rborist$minNode*9,0)
grid_Rborist$ntrees <- round(1+grid_Rborist$ntrees*5,0)

#set_ranger_mtry <- c("ranger", "tuneGrid  = data.frame(mtry = seq(from = 1, to = 106, by = 15), splitrule = 'extratrees', min.node.size = 2), num.trees = 6")
#set_ranger_splitrule <- c("ranger", "tuneGrid  = data.frame(splitrule = c('gini', 'extratrees'), mtry = 50, min.node.size = 2), num.trees = 6")
#set_ranger_nodesize <- c("ranger", "tuneGrid  = data.frame(min.node.size = seq(from = 1, to = 15, by = 2), mtry = 50, splitrule = 'extratrees'), num.trees = 6")
set_ranger <- c("ranger", "tuneGrid  = grid_ranger") # A TESTER
set_Rborist <- c("Rborist", "tuneGrid  = grid_Rborist")
#set_Rborist_pred <- c("Rborist", "tuneGrid  = data.frame(predFixed = seq(from = 1, to = 41, by = 10), minNode = 2), ntrees = 3")
#set_Rborist_minNode <- c("Rborist", "tuneGrid  = data.frame(minNode = seq(from = 1, to = 5, by = 1), predFixed =50), ntrees = 3")
#system.time(fit_test(set_ranger_mtry))  ####### CHRONO
fit_rFerns_depth <- fit_test(set_rFerns_depth)
fit_ranger <- fit_test(set_ranger)
#fit_ranger_mtry <- fit_test(set_ranger_mtry)
#fit_ranger_splitrule <- fit_test(set_ranger_splitrule)
#fit_ranger_nodesize <- fit_test(set_ranger_nodesize)
fit_Rborist <- fit_test(set_Rborist)
#fit_Rborist_pred <- fit_test(set_Rborist_pred)
#fit_Rborist_minNode <- fit_test(set_Rborist_minNode)

# Extraire résultats d'intérêt : graphes et resultats
fit_rFerns_depth_graphe <- ggplot(fit_rFerns_depth)
fit_rFerns_depth_results <- fit_rFerns_depth$results
fit_ranger_results <- fit_ranger$results
fit_ranger_bestTune <- fit_ranger$bestTune
#fit_ranger_mtry_graphe <- ggplot(fit_ranger_mtry)
#fit_ranger_mtry_results <- fit_ranger_mtry$results
#fit_ranger_mtry_bestTune <- fit_ranger_mtry$bestTune
#fit_ranger_splitrule_graphe <- ggplot(fit_ranger_splitrule)
#fit_ranger_splitrule_results <- fit_ranger_splitrule$results
#fit_ranger_splitrule_bestTune <- fit_ranger_splitrule$bestTune
#fit_ranger_nodesize_graphe <- ggplot(fit_ranger_nodesize)
#fit_ranger_nodesize_results <- fit_ranger_nodesize$results
#fit_ranger_nodesize_bestTune <- fit_ranger_nodesize$bestTune
#fit_Rborist_pred_graphe <- ggplot(fit_Rborist_pred)
#fit_Rborist_pred_results <- fit_Rborist_pred$results
#fit_Rborist_pred_bestTune <- fit_Rborist_pred$bestTune
#fit_Rborist_minNode_graphe <- ggplot(fit_Rborist_minNode)
#fit_Rborist_minNode_results <- fit_Rborist_minNode$results
#fit_Rborist_minNode_bestTune <- fit_Rborist_minNode$bestTune
fit_Rborist_results <- fit_Rborist$results
fit_Rborist_bestTune <- fit_Rborist$bestTune

# Lance modèle RANGER optimal
set_ranger_best <- c("ranger", paste0("tuneGrid  = data.frame(min.node.size = ", 
                                      fit_ranger_nodesize_bestTune$min.node.size, 
                                      ", splitrule = '", fit_ranger_splitrule_bestTune$splitrule,
                                      "', mtry = ", fit_ranger_splitrule_bestTune$mtry, ")", 
                                      ", num.trees = 3"))
fit_ranger_best <- fit_test(set_ranger_best)
fit_ranger_best_results <- fit_ranger_best$results
# Lance modèle RBORIST optimal
set_Rborist_best <- c("Rborist", paste0("tuneGrid  = data.frame(predFixed = 6, ",    # Value is forced, 6 gives a Spec = 1, and a much better sensitivity
                                        "minNode = ", fit_Rborist_minNode_bestTune$minNode, ")",
                                        ", ntrees = 2"))
fit_Rborist_best <- fit_test(set_Rborist_best)
fit_Rborist_best_results <- fit_Rborist_best$results

# Pour test complet des combinaisons de facteurs (SUPER LENT)
# set_ranger <- c("ranger", "tuneGrid = expand.grid(mtry = seq(from = 1, to = 21, by = 5),
#                                                 splitrule = c('gini', 'extratrees'),
#                                                 min.node.size = seq(from = 1, to = 16, by = 5)
#                 )" )
# fit_ranger <- fit_test(set_ranger)
# plot(fit_ranger, metric = "Spec", plotType = "level", scales = list(x = list(rot = 90)))
# plot(fit_ranger, metric = "Spec")
# ggplot(fit_ranger)

#########################################################
#     PERFORMANCE DES MODELES SUR LOT D'EVALUATION      #
#########################################################

# Règle la liste de prédiction et lance la classification
evaluation <- lot_evaluation
evaluation$reference <- as.logical(as.character(recode_factor(evaluation$class, edible = TRUE, poisonous = FALSE))) # Switch to logical values

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
colnames(rt_result) <- c("Sensitivity", "Specificity", "F1 score", "Run time (min)")
rownames(rt_result) <- c("Ranger", "Rborist")

save.image(file = "EKR-Champis-Analyse.RData")     # Sauvegarde données pour rapport
load(file = "EKR-Champis-Analyse.RData")     # Sauvegarde données pour rapport
