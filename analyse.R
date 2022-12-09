##########################
#     INITIALISATION     #
##########################

# Chargement des bibliothèques
library(tidyverse)    # Outils génériques
library(GGally)       # Outils supplémentaires pour les graphiques
library(caret)        # Outils d'apprentissage machine

# Récupération, décompression, importation des données
datafile <- tempfile()

#URL <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00615/MushroomDataset.zip"    # UCI archive
#URL <- "https://github.com/EKRihani/mushrooms/raw/master/MushroomDataset.zip"      # Alternative URL
# download.file(URL, datafile)

datafile <- "~/projects/mushrooms/MushroomDataset.zip" # FICHIER LOCAL

datafile <- unzip(datafile, "MushroomDataset/secondary_data.csv")
dataset <- read.csv(datafile, header = TRUE, sep = ";")

########################################
#  FORMATTAGE / NETTOYAGE DES DONNEES  #
########################################

# Get initial dataset structure information
structure_initial <- sapply(X = dataset, FUN = class, simplify = TRUE)  # Get all initial dataset variables classes
unique_length <- function (x) {length(unique(x))}                       # Define function : count levels of a variable
structure_uniques <- sapply(dataset, FUN = unique_length)               # Count levels of all dataset variables

# Get final dataset structure information
structure_final <- sapply(X = dataset, FUN = class, simplify = TRUE)    # Get all final dataset variables classes

# Merge initial and final dataset structure information
structure_dataset <- data.frame(cbind(structure_initial, structure_uniques, structure_final))
colnames(structure_dataset) <- c("Initial", "Levels", "Final")
structure_dataset$Levels <- as.numeric(as.character(structure_dataset$Levels))

################################
#     ANALYSE INTRODUCTIVE     #
################################

# Résumés introductifs
summary_number <- nrow(dataset)  # Comptage champis
summary_dataset <- summary(dataset) # Résumé des catégories

dataset_names <- row.names(structure_dataset)

##############################################################################
#     CREATION DES LOTS D'ENTRAINEMENT, VALIDATION, EVALUATION + GRAPHES     #
##############################################################################

# Creation lots d'entrainement/validation (90%) et evaluation (10%)
set.seed(1)
test_index <- createDataPartition(y = dataset$cap.diameter, times = 1, p = 0.1, list = FALSE)
trainvalid_set <- dataset[-test_index,]
evaluation_set <- dataset[test_index,]

# Creation lots d'entrainement (90%) et validation (10%)
set.seed(1)
test_index <- createDataPartition(y = trainvalid_set$cap.diameter, times = 1, p = 0.1, list = FALSE)
training_set <- trainvalid_set[-test_index,]
validation_set <- trainvalid_set[test_index,]

# Tracage des distributions monovariables du lot (entrainement+validation)
l <- nrow(structure_dataset)
for (n in 1:l){
  plot_title <- paste("Distribution de", dataset_names[n], "des champignons")
  plot <- trainvalid_set %>%
    ggplot(aes_string(x = dataset_names[n])) +      # aes_string permet l'utilisation de chaîne au lieu de nom de variable
    ggtitle(plot_title) +
    ylab("") +
    xlab(dataset_names[n]) +
    theme_bw()
  if(structure_dataset$Final[n] %in% c("integer", "numeric")) # Histogramme pour numériques, Barplot pour autres
  {plot <- plot + geom_histogram(fill = "gray45")}
  else
  {plot <- plot + geom_bar(fill = "gray45")}
  plotname <- paste0("study_distrib_", dataset_names[n])   # Concaténer "plot_distrib" avec nom de colonne
  assign(plotname, plot)                 # Attribuer le graphique au nom plot_distrib_colname
}


#####################################################
#     ANALYSE DESCRIPTIVE DU LOT D'ENTRAINEMENT     #
#####################################################

# Trace toutes les distributions monovariées du lot d'entrainement (all monovariate distributions of the training set (toxique vs comestible)
l <- nrow(structure_dataset)

for (n in 2:l){    # La colonne 1 (class) n'est pas tracée (attribut de fill/couleur !)
  plot_title <- paste("Distribution de", dataset_names[n], "des champignons")
  plot <- training_set %>%
    ggplot(aes_string(x = dataset_names[n], fill = training_set$class)) + # aes_string permet l'utilisation de chaîne au lieu de nom de variable
    ggtitle(plot_title) +
    ylab("Frequency") +
    xlab(dataset_names[n]) +
    scale_y_log10() +
    theme_bw()
  if(structure_dataset$Final[n] %in% c("integer", "numeric"))   # Histogramme pour numériques, Barplot pour autres
  {plot <- plot + geom_histogram()}
  else
  {plot <- plot + geom_bar()}
  plotname <- paste0("train_distrib_",dataset_names[n])    # Concaténer "plot_distrib" avec nom de colonne
  assign(plotname, plot)                 # Attribuer le graphique au nom plot_distrib_colname
}

# Graphes de corrélation pour une (petite) sélection de critères
pair_plots <- ggpairs(
  training_set,
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

# Définition de fonction : lance le modèle avec les paramètres données, évalue la performance (spécificité), renvoie les résultats de fitting
fit_test <- function(fcn_model){
  set.seed(1)
  tr_ctrl <- trainControl(classProbs = TRUE, summaryFunction = twoClassSummary, method = "cv", number = 10)   # Règle paramètres d'évaluation performance à twoClassSummary (ROC, Sens, Spec), avec cross-validation (10-fold)
  cmd <- paste0("train(class ~ ., method = '",      # Build command, set performance metric to Specificity
                fcn_model[1], 
                "', data = trainvalid_set, trControl = tr_ctrl, metric = 'Spec', ", 
                fcn_model[2],")")
  fitting <- eval(parse(text = cmd))        # Lance commande
  fitting
}

# Parallélisation (A TESTER !!!)
library(doParallel)
cl <- makeCluster(spec = 5, type = "PSOCK")
registerDoParallel(cl)

# Modèles type Discriminant Analysis (LDA2, PDA)
set_lda2_dim <- c("lda2", "tuneGrid  = data.frame(dimen = seq(from = 1, to = 16, by = 3))")
set_pda_lambda <-  c("pda", "tuneGrid  = data.frame(lambda = seq(from = 1, to = 51, by = 10))")
fit_lda2_dim <- fit_test(set_lda2_dim)
system.time(fit_test(set_lda2_dim))      #### CHRONO
fit_pda_lambda <- fit_test(set_pda_lambda)
system.time(fit_test(set_pda_lambda))  #### CHRONO
# Extraire résultats d'intérêt : graphes et resultats
fit_lda2_dim_plot <- ggplot(fit_lda2_dim)
fit_lda2_dim_results <- fit_lda2_dim$results
fit_pda_lambda_plot <- ggplot(fit_pda_lambda)
fit_pda_lambda_results <- fit_pda_lambda$results

## Fin parallélisation (A TESTER !!!)
stopCluster(cl)


# Modèles type Generalized Additive Model (GAM LOESS)
set_gamLoess_span <-  c("gamLoess", "tuneGrid  = data.frame(span = seq(from = 0.01, to = 1, by = 0.24), degree = 1)")
set_gamLoess_degree <-  c("gamLoess", "tuneGrid  = data.frame(degree = c(0, 1), span = 0.5)")
fit_gamLoess_span <- fit_test(set_gamLoess_span)
fit_gamLoess_degree <- fit_test(set_gamLoess_degree)
# Extraire résultats d'intérêt : graphes et resultats
fit_gamLoess_span_plot <- ggplot(fit_gamLoess_span)
fit_gamLoess_span_results <- fit_gamLoess_span$results
fit_gamLoess_degree_plot <- ggplot(fit_gamLoess_degree)
fit_gamLoess_degree_results <- fit_gamLoess_degree$results

# Modèles types arbres (RPART, RPARTCOST, CTREE, C50TREE)
set_rpart_cp <- c("rpart", "tuneGrid  = data.frame(cp = c(1e-5, 1e-4, 1e-3, 1e-2, 5e-2))")
set_rpartcost_complexity <- c("rpartCost", "tuneGrid  = data.frame(cp = c(1e-5, 1e-4, 1e-3, 1e-2, 0.05), Cost = 1)")
set_rpartcost_cost <- c("rpartCost", "tuneGrid  = data.frame(Cost = c(0.01, 0.4, 0.7, 1, 1.5, 2, 2.5), cp = .01)")
set_ctree_criterion <- c("ctree", "tuneGrid  = data.frame(mincriterion = c(0.01, 0.25, 0.5, 0.75, 0.99))")
set_c50tree <- c("C5.0Tree", "")
system.time(fit_test(set_rpart_cp))    ####### CHRONO
fit_rpart_cp <- fit_test(set_rpart_cp)
fit_rpartcost_complexity <- fit_test(set_rpartcost_complexity)
fit_rpartcost_cost <- fit_test(set_rpartcost_cost)
fit_ctree_criterion <- fit_test(set_ctree_criterion)
fit_c50tree <- fit_test(set_c50tree)
# Extraire résultats d'intérêt : graphes et resultats
fit_rpart_cp_results <- fit_rpart_cp$results
fit_rpartcost_complexity_plot <- ggplot(fit_rpartcost_complexity)
fit_rpartcost_complexity_results <- fit_rpartcost_complexity$results
fit_rpartcost_complexity_bestTune <- fit_rpartcost_complexity$bestTune
fit_rpartcost_cost_plot <- ggplot(fit_rpartcost_cost)
fit_rpartcost_cost_results <- fit_rpartcost_cost$results
fit_rpartcost_cost_bestTune <- fit_rpartcost_cost$bestTune
fit_ctree_criterion_plot <- ggplot(fit_ctree_criterion)
fit_ctree_criterion_results <- fit_ctree_criterion$results
fit_c50tree_results <- fit_c50tree$results
# Run best CART model
set_rpartcost_best <- c("rpartCost", paste0("tuneGrid  = data.frame(cp = ",
                                            fit_rpartcost_complexity$bestTune$cp, 
                                            ", Cost = ", fit_rpartcost_cost$bestTune$Cost, ")" ))
fit_rpartcost_best <- fit_test(set_rpartcost_best)
fit_rpartcost_best_results <- fit_rpartcost_best$results


# Modèles type Random Forest (RFERNS, RANGER, RBORIST)
set_rFerns_depth <- c("rFerns", "tuneGrid  = data.frame(depth = 2^(1:5)/2)")
set_ranger_mtry <- c("ranger", "tuneGrid  = data.frame(mtry = seq(from = 1, to = 106, by = 15), splitrule = 'extratrees', min.node.size = 2), num.trees = 6")
set_ranger_splitrule <- c("ranger", "tuneGrid  = data.frame(splitrule = c('gini', 'extratrees'), mtry = 50, min.node.size = 2), num.trees = 6")
set_ranger_nodesize <- c("ranger", "tuneGrid  = data.frame(min.node.size = seq(from = 1, to = 15, by = 2), mtry = 50, splitrule = 'extratrees'), num.trees = 6")
set_Rborist_pred <- c("Rborist", "tuneGrid  = data.frame(predFixed = seq(from = 1, to = 41, by = 5), minNode = 2), ntrees = 3")
set_Rborist_minNode <- c("Rborist", "tuneGrid  = data.frame(minNode = 1:5, predFixed =50), ntrees = 3")
system.time(fit_test(set_ranger_mtry))  ####### CHRONO
fit_rFerns_depth <- fit_test(set_rFerns_depth)
fit_ranger_mtry <- fit_test(set_ranger_mtry)
fit_ranger_splitrule <- fit_test(set_ranger_splitrule)
fit_ranger_nodesize <- fit_test(set_ranger_nodesize)
fit_Rborist_pred <- fit_test(set_Rborist_pred)
fit_Rborist_minNode <- fit_test(set_Rborist_minNode)
# Extraire résultats d'intérêt : graphes et resultats
fit_rFerns_depth_plot <- ggplot(fit_rFerns_depth)
fit_rFerns_depth_results <- fit_rFerns_depth$results
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
# Lance modèle RANGER optimal
set_ranger_best <- c("ranger", paste0("tuneGrid  = data.frame(min.node.size = ", 
                                      fit_ranger_nodesize_bestTune$min.node.size, 
                                      ", splitrule = '", fit_ranger_splitrule_bestTune$splitrule,
                                      "', mtry = ", fit_ranger_splitrule_bestTune$mtry, ")", 
                                      ", num.trees = 10"))
fit_ranger_best <- fit_test(set_ranger_best)
fit_ranger_best_results <- fit_ranger_best$results
# Lance modèle RBORIST optimal
set_Rborist_best <- c("Rborist", paste0("tuneGrid  = data.frame(predFixed = 6, ",    # Value is forced, 6 gives a Spec = 1, and a much better sensitivity
                                        "minNode = ", fit_Rborist_minNode_bestTune$minNode, ")",
                                        ", ntrees = 3"))
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
evaluation <- evaluation_set
evaluation$reference <- as.logical(as.character(recode_factor(evaluation$class, edible = TRUE, poisonous = FALSE))) # Switch to logical values

# Passe .$reference de booléen à facteur, puis calcule la matrice de confusion
evaluation$reference <- as.factor(evaluation$reference)


start_time <- Sys.time()     # Démarre chrono
cmd <- paste0("train(class ~ ., method = 'ranger', data = trainvalid_set,", set_ranger_best[2], ")") # Construction de la commande
fit_ranger_final <- eval(parse(text = cmd))     # Exécution de la commande
pred_ranger_final <- predict(object = fit_ranger_final, newdata = evaluation_set)
CM_ranger_final <- confusionMatrix(data = pred_ranger_final, reference = evaluation_set$class)
results_ranger <- c(CM_ranger_final$byClass["Sensitivity"], CM_ranger_final$byClass["Specificity"], CM_ranger_final$byClass["F1"])
end_time <- Sys.time()     # Stop chrono
time_ranger <- difftime(end_time, start_time)
time_ranger <- time_ranger %>% as.numeric %>% round(.,2)

start_time <- Sys.time()            # Démarre chrono
cmd <- paste0("train(class ~ ., method = 'Rborist', data = trainvalid_set,", set_Rborist_best[2], ")") # Construction de la commande
fit_Rborist_final <- eval(parse(text = cmd))     # Exécution de la commande
pred_Rborist_final <- predict(object = fit_Rborist_final, newdata = evaluation_set)
CM_Rborist_final <- confusionMatrix(data = pred_Rborist_final, reference = evaluation_set$class)
results_Rborist <- c(CM_Rborist_final$byClass["Sensitivity"], CM_Rborist_final$byClass["Specificity"], CM_Rborist_final$byClass["F1"])
end_time <- Sys.time()              # Stop chrono
time_Rborist <- difftime(end_time, start_time)
time_Rborist <- time_Rborist %>% as.numeric %>% round(.,2)

result_Rborist <- c(CM_Rborist_final$byClass["Sensitivity"], CM_Rborist_final$byClass["Specificity"], CM_Rborist_final$byClass["F1"], time_Rborist)
result_ranger <- c(CM_ranger_final$byClass["Sensitivity"], CM_ranger_final$byClass["Specificity"], CM_ranger_final$byClass["F1"], time_ranger)
rt_result <- rbind(result_ranger, result_Rborist)
colnames(rt_result) <- c("Sensitivity", "Specificity", "F1 score", "Run time (min)")
rownames(rt_result) <- c("Ranger", "Rborist")

save.image(file = "EKR-mushrooms.RData")     # Sauvegarde données pour rapport