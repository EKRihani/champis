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
dataset$family <- NULL       # Idem.
dataset$name <- dataset$name %>% str_replace_all(., "[:space:]|-", "_")  %>% str_replace_all(., "\'", "_") %>% as.factor(.)

MULESP_n_champis <- nrow(dataset)
MULESP_split_p <- sqrt(MULESP_n_champis)
MULESP_split_facteur <- round(sqrt(MULESP_split_p)+1)

set.seed(007)
index1 <- twin(data = dataset, r = MULESP_split_facteur)
MULESP_lot_appr_opti <- dataset[-index1,]
MULESP_lot_evaluation <- dataset[index1,]

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
                           number = MULESP_split_facteur)   # Règle paramètres d'évaluation performance à multiClassSummary (kappa...), avec cross-validation
   cmd <- paste0("train(name ~ ., method = '",      # Construit commande, évaluation de performance
                 fcn_model[1], 
                 "', data = MULESP_lot_appr_opti, trControl = tr_ctrl, ", 
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
MULESP_LHS <- nolhDesign(dimension = 2, range = c(0, 1))$design     # Hypercube latin quasi-orthogonal
MULESP_LHS <- data.frame(MULESP_LHS)
colnames(MULESP_LHS) <- c("X1", "X2")


### CTREE ### TRES LENT
# MULESP_set_ctree_criterion <- c("ctree", "tuneGrid  = data.frame(mincriterion = c(0.01, 0.25, 0.5, 0.75, 0.99))")
# MULESP_fit_ctree_criterion <- fit_test(MULESP_set_ctree_criterion)
# MULESP_fit_ctree_criterion_resultats <- MULESP_fit_ctree_criterion$results
# MULESP_fit_ctree_criterion_graphe <- grapheKappa(MULESP_fit_ctree_criterion_resultats, mincriterion)


### C 5.0 TREE ###
# MULESP_set_c50tree <- c("C5.0Tree", "")
# MULESP_fit_c50tree <- fit_test(MULESP_set_c50tree)
# MULESP_fit_c50tree_resultats <- MULESP_fit_c50tree$results


### RPART ###
MULESP_grid_rpart_cp <- data.frame(cp = 10^seq(from = -5, to = -1, by = .5))
MULESP_set_rpart_cp <- c("rpart", "tuneGrid  = MULESP_grid_rpart_cp")
MULESP_fit_rpart_cp <- fit_test(MULESP_set_rpart_cp)
MULESP_fit_rpart_cp_resultats <- MULESP_fit_rpart_cp$results
MULESP_fit_rpart_cp_graphe <- grapheKappa(MULESP_fit_rpart_cp_resultats, cp)+ scale_x_log10()


### RPARTCOST ###    Ne fonctionne pas ?
# MULESP_grid_rpartcost <- MULESP_LHS
# colnames(MULESP_grid_rpartcost) <- c("cp", "Cost")
# MULESP_grid_rpartcost$cp <- (MULESP_grid_rpartcost$cp*1e-2+1e-5)
# MULESP_grid_rpartcost$Cost <- MULESP_grid_rpartcost$Cost*2.5+1e-3

# MULESP_set_rpartcost <- c("rpartCost", "tuneGrid  = MULESP_grid_rpartcost[c('cp', 'Cost')]")
# MULESP_fit_rpartcost <- fit_test(MULESP_set_rpartcost)
# MULESP_fit_rpartcost_resultats <- MULESP_fit_rpartcost$results
# MULESP_mod_rpartcost_kappa <- modelFit(X=MULESP_fit_rpartcost_resultats[,1:2], 
                                          # Y=MULESP_fit_rpartcost_resultats$Kappa,
                                          # type="Kriging",
                                          # formula=Y~cp+Cost+cp:Cost+I(cp^2)+I(Cost^2))
# MULESP_pred_rpartcost <- expand.grid(MULESP_fit_rpartcost_resultats[,1:2])
# colnames(MULESP_pred_rpartcost) <- c("Cost", "cp")
# MULESP_pred_rpartcost2 <- NULL
# MULESP_pred_rpartcost2$Kappa <- modelPredict(MULESP_mod_rpartcost_spec, MULESP_pred_rpartcost)
# MULESP_pred_rpartcost <- cbind(MULESP_pred_rpartcost, MULESP_pred_rpartcost2)
# MULESP_fit_rpartcost_kappa_graphe <- ggplot() +
#    geom_raster(data = MULESP_pred_rpartcost, aes(x = Cost, y = cp, fill = Kappa), interpolate = TRUE) +
#    geom_tile(data = MULESP_fit_rpartcost_resultats, aes(x = Cost, y = cp, fill = Kappa), color = "black", linewidth =.5) +
#    scale_fill_viridis_c(option = "F", direction = 1) +
#    theme_bw() +
#    theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))

# MULESP_best_rpartcost <- which.max(MULESP_fit_rpartcost_resultats$Spec^MULESP_ratioSpeSen*MULESP_fit_rpartcost_resultats$Sens)
# MULESP_best_rpartcostgrid <- data.frame(Cost = MULESP_fit_rpartcost_resultats[MULESP_best_rpartcost,]$Cost, cp =MULESP_fit_rpartcost_resultats[MULESP_best_rpartcost,]$cp)
# MULESP_set_rpartcost_best <- c("rpartCost", paste0("tuneGrid  = MULESP_best_rpartcostgrid"))
# MULESP_fit_rpartcost_best <- fit_test(MULESP_set_rpartcost_best)
# MULESP_fit_rpartcost_best_resultats <- MULESP_fit_rpartcost_best$results


################################################
#     MULTICLASSIFIEUR : FORETS ALEATOIRES     #
################################################

### RANGER ###
MULESP_grid_ranger <- rbind(MULESP_LHS,MULESP_LHS) %>%
   mutate(X3 = c(rep(0, 17), rep(1, 17))) %>%
   mutate(mtry = round(1+X1*48,0)) %>%    # Prendre des multiples de 16 (car 17 points pour 2d)
   mutate(min.node.size = round(1+X2*32,0)) %>%
   mutate(splitrule = case_when(X3 == 0 ~ "gini", X3 == 1 ~ "extratrees"))

MULESP_set_ranger <- c("ranger", "tuneGrid  = MULESP_grid_ranger[,c('mtry','min.node.size','splitrule')], num.trees = 6")
MULESP_fit_ranger <- fit_test(MULESP_set_ranger)
#save.image(file = "EKR-Champis-AnalyseMultiEsp2.RData")
MULESP_fit_ranger_resultats <- MULESP_fit_ranger$results %>% 
   left_join(., MULESP_grid_ranger, by = c("mtry", "min.node.size", "splitrule"))   # Ajout des facteurs réduits

MULESP_fit_ranger_bestTune <- MULESP_fit_ranger$bestTune

# Modélisation quadratique avec interactions
MULESP_mod_ranger_kappa <-  modelFit(X=MULESP_fit_ranger_resultats[,c("mtry", "min.node.size", "X3")], 
                                Y=MULESP_fit_ranger_resultats$Kappa,  
                                type="Kriging", 
                                formula=Y~mtry+min.node.size+X3+mtry:min.node.size+min.node.size:X3+mtry:X3+I(mtry^2)+I(min.node.size^2))
MULESP_mod_ranger_accu <-  modelFit(X=MULESP_fit_ranger_resultats[,c("mtry", "min.node.size", "X3")], 
                                Y=MULESP_fit_ranger_resultats$Accuracy,  
                                type="Kriging", 
                                formula=Y~mtry+min.node.size+X3+mtry:min.node.size+min.node.size:X3+mtry:X3+I(mtry^2)+I(min.node.size^2))

MULESP_pred_ranger <- expand(MULESP_fit_ranger_resultats[,c("X1","X2","X3")], X1, X2, X3) %>%
   data.frame() %>%
   mutate(mtry = round(1+X1*48,0)) %>%
   mutate(min.node.size = round(1+X2*32,0)) %>%
   mutate(splitrule = case_when(X3 == 0 ~ "gini", X3 == 1 ~ "extratrees")) %>%
   mutate(Kappa = modelPredict(MULESP_mod_ranger_kappa, .[,c("mtry", "min.node.size", "X3")])) %>%
   mutate(Accuracy = modelPredict(MULESP_mod_ranger_accu, .[,c("mtry", "min.node.size", "X3")]))

MULESP_pred_ranger_ET <- MULESP_pred_ranger %>% filter(splitrule == "extratrees")
MULESP_pred_ranger_GINI <- MULESP_pred_ranger %>% filter(splitrule == "gini")
MULESP_fit_ranger_ET <- MULESP_fit_ranger$results %>% filter(splitrule == "extratrees")
MULESP_fit_ranger_GINI <- MULESP_fit_ranger$results %>% filter(splitrule == "gini")

MULESP_fit_ranger_Gini_kappa_graphe <- graphe2D("MULESP_pred_ranger_GINI", "MULESP_fit_ranger_GINI", "mtry", "min.node.size", "Kappa", "F")
MULESP_fit_ranger_Gini_accu_graphe <- graphe2D("MULESP_pred_ranger_GINI", "MULESP_fit_ranger_GINI", "mtry", "min.node.size", "Accuracy", "G")
MULESP_fit_ranger_ET_kappa_graphe <- graphe2D("MULESP_pred_ranger_ET", "MULESP_fit_ranger_ET", "mtry", "min.node.size", "Kappa", "F")
MULESP_fit_ranger_ET_accu_graphe <- graphe2D("MULESP_pred_ranger_ET", "MULESP_fit_ranger_ET", "mtry", "min.node.size", "Accuracy", "G")


MULESP_best_ranger <- which.max(MULESP_fit_ranger_resultats$Kappa)
MULESP_best_rangergrid <- data.frame(mtry = MULESP_fit_ranger_resultats[MULESP_best_ranger,]$mtry, min.node.size =MULESP_fit_ranger_resultats[MULESP_best_ranger,]$min.node.size, splitrule =MULESP_fit_ranger_resultats[MULESP_best_ranger,]$splitrule)
save.image(file = "EKR-Champis-AnalyseMultiEsp.RData")

# Lance modèle RANGER optimal
MULESP_set_ranger_best <- c("ranger", paste0("tuneGrid  = MULESP_best_rangergrid, num.trees = 6"))
MULESP_fit_ranger_best <- fit_test(MULESP_set_ranger_best)
MULESP_fit_ranger_best_resultats <- MULESP_fit_ranger_best$results
save.image(file = "EKR-Champis-AnalyseMultiEsp2.RData")

### RBORIST ###
MULESP_grid_Rborist <- data.frame(MULESP_LHS) %>%
   mutate(predFixed = round(1+X1*16,0)) %>%     #32
   mutate(minNode = round(1+X2*16,0))

MULESP_set_Rborist <- c("Rborist", "tuneGrid  = MULESP_grid_Rborist[,c('predFixed','minNode')]")
MULESP_fit_Rborist <- fit_test(MULESP_set_Rborist)
save.image(file = "EKR-Champis-AnalyseMultiEsp2.RData")

MULESP_fit_Rborist_resultats <- MULESP_fit_Rborist$results %>%
   left_join(., MULESP_grid_Rborist, by = c("predFixed", "minNode"))   # Ajout des facteurs réduits
MULESP_fit_Rborist_bestTune <- MULESP_fit_Rborist$bestTune

MULESP_mod_Rborist_kappa <- modelFit(X=MULESP_fit_Rborist_resultats[,c("predFixed", "minNode")], 
                                     Y=MULESP_fit_Rborist_resultats$Kappa, 
                                     type="Kriging", 
                                     formula=Y~predFixed+minNode+predFixed:minNode+I(predFixed^2)+I(minNode^2))
MULESP_mod_Rborist_accu <-  modelFit(X=MULESP_fit_Rborist_resultats[,c("predFixed", "minNode")], 
                                     Y=MULESP_fit_Rborist_resultats$Accuracy, 
                                     type="Kriging", 
                                     formula=Y~predFixed+minNode+predFixed:minNode+I(predFixed^2)+I(minNode^2))



MULESP_pred_Rborist <- expand.grid(MULESP_fit_Rborist_resultats[,c("X1","X2")]) %>%
   mutate(predFixed = round(1+X1*16,0)) %>%
   mutate(minNode = round(1+X2*16,0)) %>%
   mutate(Kappa = modelPredict(MULESP_mod_Rborist_kappa, .[,c("predFixed", "minNode")])) %>%
   mutate(Accuracy = modelPredict(MULESP_mod_Rborist_accu, .[,c("predFixed", "minNode")]))

MULESP_fit_Rborist_kappa_graphe <- graphe2D("MULESP_pred_Rborist", "MULESP_fit_Rborist_resultats", "predFixed", "minNode", "Kappa", "F")     # A,B,D,F,G
MULESP_fit_Rborist_accu_graphe <- graphe2D("MULESP_pred_Rborist", "MULESP_fit_Rborist_resultats", "predFixed", "minNode", "Accuracy", "G")

MULESP_best_Rborist <- which.max(MULESP_fit_Rborist_resultats$Kappa)
MULESP_best_Rboristgrid <- data.frame(predFixed = MULESP_fit_Rborist_resultats[MULESP_best_Rborist,]$predFixed, minNode =MULESP_fit_Rborist_resultats[MULESP_best_Rborist,]$minNode)
save.image(file = "EKR-Champis-AnalyseMultiEsp.RData")

# Lance modèle RBORIST optimal
MULESP_set_Rborist_best <- c("Rborist", paste0("tuneGrid  = MULESP_best_Rboristgrid, ntrees = 2"))
MULESP_fit_Rborist_best <- fit_test(MULESP_set_Rborist_best)
MULESP_fit_Rborist_best_resultats <- MULESP_fit_Rborist_best$results
save.image(file = "EKR-Champis-AnalyseMultiEsp2.RData")

#########################################################
#     PERFORMANCE DES MODELES SUR LOT D'EVALUATION      #
#########################################################    PAS ENCORE LANCE, A FAIRE !!!

# Règle la liste de prédiction et lance la classification
MULESP_evaluation <- MULESP_lot_evaluation
MULESP_evaluation$reference <- as.factor(MULESP_evaluation$name)
save.image(file = "EKR-Champis-AnalyseMultiEsp.RData")

start_time <- Sys.time()     # Démarre chrono
cmd <- paste0("train(name ~ ., method = 'ranger', data = MULESP_lot_appr_opti,", MULESP_set_ranger_best[2], ")") # Construction de la commande
MULESP_fit_ranger_final <- eval(parse(text = cmd))     # Exécution de la commande
MULESP_pred_ranger_final <- predict(object = MULESP_fit_ranger_final, newdata = MULESP_lot_evaluation)
MULESP_CM_ranger_final <- confusionMatrix(data = MULESP_pred_ranger_final, reference = MULESP_lot_evaluation$name)
MULESP_resultats_ranger <- c(MULESP_CM_ranger_final$byClass["Accuracy"], MULESP_CM_ranger_final$byClass["Kappa"])
end_time <- Sys.time()     # Stop chrono
MULESP_temps_ranger <- difftime(end_time, start_time)
MULESP_temps_ranger <- MULESP_temps_ranger %>% as.numeric %>% round(.,2)
save.image(file = "EKR-Champis-AnalyseMultiEsp2.RData")

start_time <- Sys.time()            # Démarre chrono
cmd <- paste0("train(name ~ ., method = 'Rborist', data = MULESP_lot_appr_opti,", MULESP_set_Rborist_best[2], ")") # Construction de la commande
MULESP_fit_Rborist_final <- eval(parse(text = cmd))     # Exécution de la commande
MULESP_pred_Rborist_final <- predict(object = MULESP_fit_Rborist_final, newdata = MULESP_lot_evaluation)
MULESP_CM_Rborist_final <- confusionMatrix(data = MULESP_pred_Rborist_final, reference = MULESP_lot_evaluation$name)
MULESP_resultats_Rborist <- c(MULESP_CM_Rborist_final$byClass["Accuracy"], MULESP_CM_Rborist_final$byClass["Kappa"])
end_time <- Sys.time()              # Stop chrono
MULESP_temps_Rborist <- difftime(end_time, start_time)
MULESP_temps_Rborist <- MULESP_temps_Rborist %>% as.numeric %>% round(.,2)
save.image(file = "EKR-Champis-AnalyseMultiEsp2.RData")

MULESP_resultat_Rborist <- c(MULESP_CM_Rborist_final$overall["Accuracy"], MULESP_CM_Rborist_final$overall["Kappa"], MULESP_temps_Rborist)
MULESP_resultat_ranger <- c(MULESP_CM_ranger_final$overall["Accuracy"], MULESP_CM_ranger_final$overall["Kappa"], MULESP_temps_ranger)
MULESP_RF_resultat <- rbind(MULESP_resultat_ranger, MULESP_resultat_Rborist)
colnames(MULESP_RF_resultat) <- c("Précision", "Kappa", "Durée (min)")
rownames(MULESP_RF_resultat) <- c("Ranger", "Rborist")

save.image(file = "EKR-Champis-AnalyseMultiEsp2.RData")     # Sauvegarde données pour rapport

# Suppression gros fichiers intermédiaires, avant sauvegarde
rm(dataset, MULESP_evaluation, MULESP_lot_appr_opti, MULESP_lot_evaluation,
   MULESP_fit_rpart_cp,
   MULESP_fit_Rborist, MULESP_fit_Rborist_best, MULESP_fit_Rborist_final,
   MULESP_fit_ranger, MULESP_fit_ranger_best, MULESP_fit_ranger_final)
save.image(file = "EKR-Champis-AnalyseMultiEsp-Light.RData")     # Sauvegarde données pour rapport

load(file = "EKR-Champis-AnalyseMultiEsp.RData")     # Chargement données pour rapport
