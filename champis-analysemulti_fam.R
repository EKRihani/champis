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
dataset$family <- dataset$family %>% str_replace_all(., "[:space:]|-", "_") %>% str_remove_all(., "_Family") %>% as.factor(.)
#dataset$family <- as.factor(str_replace_all(dataset$family, "[:space:]|-", "_"))

MULFAM_n_champis <- nrow(dataset)
MULFAM_split_p <- sqrt(MULFAM_n_champis)
MULFAM_split_facteur <- round(sqrt(MULFAM_split_p)+1)

set.seed(007)
index1 <- twin(data = dataset, r = MULFAM_split_facteur)
MULFAM_lot_appr_opti <- dataset[-index1,]
MULFAM_lot_evaluation <- dataset[index1,]

######################################################################
#     MULTICLASSIFIEUR : INITIALISATION ET DEFINITIONS FONCTIONS     #
######################################################################
# https://topepo.github.io/caret/available-models.html
# names(getModelInfo())
# getModelInfo(Rborist)

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

##################################################
#     MULTICLASSIFIEUR : ARBRES DECISIONNELS     #
##################################################

# Définition de l'hypercube latin quasi-orthogonal (NOLH)
MULFAM_LHS <- nolhDesign(dimension = 2, range = c(0, 1))$design     # Hypercube latin quasi-orthogonal
MULFAM_LHS <- data.frame(MULFAM_LHS)
colnames(MULFAM_LHS) <- c("X1", "X2")


### CTREE ###
MULFAM_set_ctree_criterion <- c("ctree", "tuneGrid  = data.frame(mincriterion = c(0.01, 0.25, 0.5, 0.75, 0.99))")
MULFAM_fit_ctree_criterion <- fit_test(MULFAM_set_ctree_criterion)
MULFAM_fit_ctree_criterion_resultats <- MULFAM_fit_ctree_criterion$results
MULFAM_fit_ctree_criterion_graphe <- grapheKappa(MULFAM_fit_ctree_criterion_resultats, mincriterion)


### C 5.0 TREE ###
# MULFAM_set_c50tree <- c("C5.0Tree", "")
# MULFAM_fit_c50tree <- fit_test(MULFAM_set_c50tree)
# MULFAM_fit_c50tree_resultats <- MULFAM_fit_c50tree$results


### RPART ###
MULFAM_grid_rpart_cp <- data.frame(cp = 10^seq(from = -5, to = -1, by = .5))
MULFAM_set_rpart_cp <- c("rpart", "tuneGrid  = MULFAM_grid_rpart_cp")
MULFAM_fit_rpart_cp <- fit_test(MULFAM_set_rpart_cp)
MULFAM_fit_rpart_cp_resultats <- MULFAM_fit_rpart_cp$results
MULFAM_fit_rpart_cp_graphe <- grapheKappa(MULFAM_fit_rpart_cp_resultats, cp)+ scale_x_log10()


### RPARTCOST ###    Ne fonctionne pas ?
# MULFAM_grid_rpartcost <- MULFAM_LHS
# colnames(MULFAM_grid_rpartcost) <- c("cp", "Cost")
# MULFAM_grid_rpartcost$cp <- (MULFAM_grid_rpartcost$cp*1e-2+1e-5)
# MULFAM_grid_rpartcost$Cost <- MULFAM_grid_rpartcost$Cost*2.5+1e-3

# MULFAM_set_rpartcost <- c("rpartCost", "tuneGrid  = MULFAM_grid_rpartcost[c('cp', 'Cost')]")
# MULFAM_fit_rpartcost <- fit_test(MULFAM_set_rpartcost)
# MULFAM_fit_rpartcost_resultats <- MULFAM_fit_rpartcost$results
# MULFAM_mod_rpartcost_kappa <- modelFit(X=MULFAM_fit_rpartcost_resultats[,1:2], 
                                          # Y=MULFAM_fit_rpartcost_resultats$Kappa,
                                          # type="Kriging",
                                          # formula=Y~cp+Cost+cp:Cost+I(cp^2)+I(Cost^2))
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

# MULFAM_best_rpartcost <- which.max(MULFAM_fit_rpartcost_resultats$Spec^MULFAM_ratioSpeSen*MULFAM_fit_rpartcost_resultats$Sens)
# MULFAM_best_rpartcostgrid <- data.frame(Cost = MULFAM_fit_rpartcost_resultats[MULFAM_best_rpartcost,]$Cost, cp =MULFAM_fit_rpartcost_resultats[MULFAM_best_rpartcost,]$cp)
# MULFAM_set_rpartcost_best <- c("rpartCost", paste0("tuneGrid  = MULFAM_best_rpartcostgrid"))
# MULFAM_fit_rpartcost_best <- fit_test(MULFAM_set_rpartcost_best)
# MULFAM_fit_rpartcost_best_resultats <- MULFAM_fit_rpartcost_best$results


################################################
#     MULTICLASSIFIEUR : FORETS ALEATOIRES     #
################################################

### RANGER ###
MULFAM_grid_ranger <- rbind(MULFAM_LHS,MULFAM_LHS) %>%
   mutate(X3 = c(rep(0, 17), rep(1, 17))) %>%
   mutate(mtry = round(1+X1*48,0)) %>%    # Prendre des multiples de 16 (car 17 points pour 2d)
   mutate(min.node.size = round(1+X2*32,0)) %>%
   mutate(splitrule = case_when(X3 == 0 ~ "gini", X3 == 1 ~ "extratrees"))

MULFAM_set_ranger <- c("ranger", "tuneGrid  = MULFAM_grid_ranger[,c('mtry','min.node.size','splitrule')], num.trees = 6")
MULFAM_fit_ranger <- fit_test(MULFAM_set_ranger)
MULFAM_fit_ranger_resultats <- MULFAM_fit_ranger$results %>% 
   left_join(., MULFAM_grid_ranger, by = c("mtry", "min.node.size", "splitrule"))   # Ajout des facteurs réduits

MULFAM_fit_ranger_bestTune <- MULFAM_fit_ranger$bestTune

# Modélisation quadratique avec interactions
MULFAM_mod_ranger_kappa <-  modelFit(X=MULFAM_fit_ranger_resultats[,c("mtry", "min.node.size", "X3")], 
                                Y=MULFAM_fit_ranger_resultats$Kappa,  
                                type="Kriging", 
                                formula=Y~mtry+min.node.size+X3+mtry:min.node.size+min.node.size:X3+mtry:X3+I(mtry^2)+I(min.node.size^2))
MULFAM_mod_ranger_accu <-  modelFit(X=MULFAM_fit_ranger_resultats[,c("mtry", "min.node.size", "X3")], 
                                Y=MULFAM_fit_ranger_resultats$Accuracy,  
                                type="Kriging", 
                                formula=Y~mtry+min.node.size+X3+mtry:min.node.size+min.node.size:X3+mtry:X3+I(mtry^2)+I(min.node.size^2))

MULFAM_pred_ranger <- expand(MULFAM_fit_ranger_resultats[,c("X1","X2","X3")], X1, X2, X3) %>%
   data.frame() %>%
   mutate(mtry = round(1+X1*48,0)) %>%
   mutate(min.node.size = round(1+X2*32,0)) %>%
   mutate(splitrule = case_when(X3 == 0 ~ "gini", X3 == 1 ~ "extratrees")) %>%
   mutate(Kappa = modelPredict(MULFAM_mod_ranger_kappa, .[,c("mtry", "min.node.size", "X3")])) %>%
   mutate(Accuracy = modelPredict(MULFAM_mod_ranger_accu, .[,c("mtry", "min.node.size", "X3")]))

MULFAM_pred_ranger_ET <- MULFAM_pred_ranger %>% filter(splitrule == "extratrees")
MULFAM_pred_ranger_GINI <- MULFAM_pred_ranger %>% filter(splitrule == "gini")
MULFAM_fit_ranger_ET <- MULFAM_fit_ranger$results %>% filter(splitrule == "extratrees")
MULFAM_fit_ranger_GINI <- MULFAM_fit_ranger$results %>% filter(splitrule == "gini")

MULFAM_fit_ranger_Gini_kappa_graphe <- graphe2D("MULFAM_pred_ranger_GINI", "MULFAM_fit_ranger_GINI", "mtry", "min.node.size", "Kappa", "F")
MULFAM_fit_ranger_Gini_accu_graphe <- graphe2D("MULFAM_pred_ranger_GINI", "MULFAM_fit_ranger_GINI", "mtry", "min.node.size", "Accuracy", "G")
MULFAM_fit_ranger_ET_kappa_graphe <- graphe2D("MULFAM_pred_ranger_ET", "MULFAM_fit_ranger_ET", "mtry", "min.node.size", "Kappa", "F")
MULFAM_fit_ranger_ET_accu_graphe <- graphe2D("MULFAM_pred_ranger_ET", "MULFAM_fit_ranger_ET", "mtry", "min.node.size", "Accuracy", "G")


MULFAM_best_ranger <- which.max(MULFAM_fit_ranger_resultats$Kappa)
MULFAM_best_rangergrid <- data.frame(mtry = MULFAM_fit_ranger_resultats[MULFAM_best_ranger,]$mtry, min.node.size =MULFAM_fit_ranger_resultats[MULFAM_best_ranger,]$min.node.size, splitrule =MULFAM_fit_ranger_resultats[MULFAM_best_ranger,]$splitrule)
MULFAM_set_ranger_best <- c("ranger", paste0("tuneGrid  = MULFAM_best_rangergrid"))
MULFAM_fit_ranger_best <- fit_test(MULFAM_set_ranger_best)
MULFAM_fit_ranger_best_resultats <- MULFAM_fit_ranger_best$results

# Lance modèle RANGER optimal
MULFAM_set_ranger_best <- c("ranger", paste0("tuneGrid  = MULFAM_best_rangergrid, num.trees = 6"))
MULFAM_fit_ranger_best <- fit_test(MULFAM_set_ranger_best)
MULFAM_fit_ranger_best_resultats <- MULFAM_fit_ranger_best$results


### RBORIST ###
MULFAM_grid_Rborist <- data.frame(MULFAM_LHS) %>%
   mutate(predFixed = round(1+X1*16,0)) %>%     #32
   mutate(minNode = round(1+X2*16,0))

MULFAM_set_Rborist <- c("Rborist", "tuneGrid  = MULFAM_grid_Rborist[,c('predFixed','minNode')]")
MULFAM_fit_Rborist <- fit_test(MULFAM_set_Rborist)
MULFAM_fit_Rborist_resultats <- MULFAM_fit_Rborist$results %>%
   left_join(., MULFAM_grid_Rborist, by = c("predFixed", "minNode"))   # Ajout des facteurs réduits
MULFAM_fit_Rborist_bestTune <- MULFAM_fit_Rborist$bestTune

MULFAM_mod_Rborist_kappa <- modelFit(X=MULFAM_fit_Rborist_resultats[,c("predFixed", "minNode")], 
                                     Y=MULFAM_fit_Rborist_resultats$Kappa, 
                                     type="Kriging", 
                                     formula=Y~predFixed+minNode+predFixed:minNode+I(predFixed^2)+I(minNode^2))
MULFAM_mod_Rborist_accu <-  modelFit(X=MULFAM_fit_Rborist_resultats[,c("predFixed", "minNode")], 
                                     Y=MULFAM_fit_Rborist_resultats$Accuracy, 
                                     type="Kriging", 
                                     formula=Y~predFixed+minNode+predFixed:minNode+I(predFixed^2)+I(minNode^2))



MULFAM_pred_Rborist <- expand.grid(MULFAM_fit_Rborist_resultats[,c("X1","X2")]) %>%
   mutate(predFixed = round(1+X1*16,0)) %>%
   mutate(minNode = round(1+X2*16,0)) %>%
   mutate(Kappa = modelPredict(MULFAM_mod_Rborist_kappa, .[,c("predFixed", "minNode")])) %>%
   mutate(Accuracy = modelPredict(MULFAM_mod_Rborist_accu, .[,c("predFixed", "minNode")]))

MULFAM_fit_Rborist_kappa_graphe <- graphe2D("MULFAM_pred_Rborist", "MULFAM_fit_Rborist_resultats", "predFixed", "minNode", "Kappa", "F")     # A,B,D,F,G
MULFAM_fit_Rborist_accu_graphe <- graphe2D("MULFAM_pred_Rborist", "MULFAM_fit_Rborist_resultats", "predFixed", "minNode", "Accuracy", "G")

MULFAM_best_Rborist <- which.max(MULFAM_fit_Rborist_resultats$Kappa)
MULFAM_best_Rboristgrid <- data.frame(predFixed = MULFAM_fit_Rborist_resultats[MULFAM_best_Rborist,]$predFixed, minNode =MULFAM_fit_Rborist_resultats[MULFAM_best_Rborist,]$minNode)
MULFAM_set_Rborist_best <- c("Rborist", paste0("tuneGrid  = MULFAM_best_Rboristgrid"))
MULFAM_fit_Rborist_best <- fit_test(MULFAM_set_Rborist_best)
MULFAM_fit_Rborist_best_resultats <- MULFAM_fit_Rborist_best$results

# Lance modèle RBORIST optimal
MULFAM_set_Rborist_best <- c("Rborist", paste0("tuneGrid  = MULFAM_best_Rboristgrid, ntrees = 2"))
MULFAM_fit_Rborist_best <- fit_test(MULFAM_set_Rborist_best)
MULFAM_fit_Rborist_best_resultats <- MULFAM_fit_Rborist_best$results


#########################################################
#     PERFORMANCE DES MODELES SUR LOT D'EVALUATION      #
#########################################################    PAS ENCORE LANCE, A FAIRE !!!

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
