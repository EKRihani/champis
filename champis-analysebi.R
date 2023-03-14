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
#URL <- "https://github.com/EKRihani/champis/raw/master/MushroomDataset.zip"      # URL de mon repo
# download.file(URL, fichier_data)
fichier_data <- "~/projects/champis/MushroomDataset.zip" # FICHIER LOCAL

fichier_data <- unzip(fichier_data, "MushroomDataset/secondary_data.csv")
dataset <- read.csv(fichier_data, header = TRUE, sep = ";", stringsAsFactors = TRUE)
dataset$class <- recode_factor(dataset$class, e = "comestible", p = "toxique")
dataset$class <- relevel(dataset$class, ref = "toxique")

####################################################################
#     CREATION DES LOTS D'ENTRAINEMENT, VALIDATION, EVALUATION     #
####################################################################

# Creation lots d'entrainement/optimisation (92%) et d'évaluation (8%) (ratio 13:1)

BI_n_champis <- nrow(dataset)
BI_split_p <- sqrt(BI_n_champis)
BI_split_facteur <- round(sqrt(BI_split_p)+1)

set.seed(007)
index1 <- twin(data = dataset, r = BI_split_facteur)
#index1 <- createDataPartition(y = dataset$cap.diameter, times = 1, p = split_ratio, list = FALSE)
BI_lot_appr_opti <- dataset[-index1,]
BI_lot_evaluation <- dataset[index1,]

# Creation lots d'entrainement (92%) et validation (8%)  # NOOOOOPE, optimisation via cross-validation !!!
# set.seed(1337)
# index2 <- createDataPartition(y = BI_lot_appr_opti$cap.diameter, times = 1, p = split2, list = FALSE)
# BI_lot_apprentissage <- BI_lot_appr_opti[-index2,]
# BI_lot_evaluation <- BI_lot_appr_opti[index2,]


##############################################################################
#     ANALYSE DU LOT D'ENTRAINEMENT AVEC MODELES DE CARET, BICLASSIFIEUR     #
##############################################################################
# https://topepo.github.io/caret/available-models.html
# names(getModelInfo())
# getModelInfo(Rborist)

BI_w <- 10/11        # Ou 1/11
BI_RatioSens <- 2*BI_w
BI_RatioSpec <- 2*(1-BI_w)

# Définition de fonction : lance le modèle avec les paramètres données, évalue la performance (spécificité), renvoie les résultats de fitting
fit_test <- function(fcn_modele){
   set.seed(1)
   tr_ctrl <- trainControl(classProbs = TRUE, 
                           summaryFunction = twoClassSummary, 
                           method = "cv", 
                           number = BI_split_facteur)   # Règle paramètres d'évaluation performance à twoClassSummary (ROC, Sens, Spec), avec cross-validation (n-fold)
   cmd <- paste0("train(class ~ ., method = '",      # Construit commande, évaluation de performance par Spécificité
                 fcn_modele[1], 
                 "', data = BI_lot_appr_opti, trControl = tr_ctrl, metric = 'Sens', ", 
                 fcn_modele[2],")")
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


# Définition de fonction : graphique nuage/ligne de Sens+Spec+Youden
grapheSpeSenJw <- function(fcn_donnees, fcn_abcisse){
   fcn_abcisse <- enquo(fcn_abcisse)
   ggplot(data = fcn_donnees, aes(x = !!fcn_abcisse)) +
      geom_line(aes(y = Sens, color = "Sensibilité")) +
      geom_point(aes(y = Sens, color = "Sensibilité")) +
      geom_line(aes(y = Spec, color = "Spécificité")) +
      geom_point(aes(y = Spec, color = "Spécificité")) +
      geom_line(aes(y = Jw, color = "Jw")) +
      geom_point(aes(y = Jw, color = "Jw")) +
      ylab(NULL) + labs(colour = "Performance") + theme_bw()
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
BI_fit_lda2_dim_resultats <- BI_fit_lda2_dim$results
BI_fit_lda2_dim_resultats <- BI_fit_lda2_dim_resultats %>% mutate(Jw = Sens*BI_RatioSens + Spec*BI_RatioSpec - 1)
BI_fit_lda2_dim_graphe <- grapheSpeSenJw(BI_fit_lda2_dim_resultats, "dimen")

BI_fit_pda_lambda_resultats <- BI_fit_pda_lambda$results
BI_fit_pda_lambda_resultats <- BI_fit_pda_lambda_resultats %>% mutate(Jw = Sens*BI_RatioSens + Spec*BI_RatioSpec - 1)
BI_fit_pda_lambda_graphe <- grapheSpeSenJw(BI_fit_pda_lambda_resultats, "lambda")

## Fin parallélisation (A TESTER !!!)
# stopCluster(cl)


# Modèles type Generalized Additive Model (GAM LOESS)          A RATIONNALISER !!!
BI_set_gamLoess_span <-  c("gamLoess", "tuneGrid  = data.frame(span = seq(from = 0.01, to = 1, by = 0.24), degree = 1)")
BI_set_gamLoess_degree <-  c("gamLoess", "tuneGrid  = data.frame(degree = c(0, 1), span = 0.5)")
BI_fit_gamLoess_span <- fit_test(BI_set_gamLoess_span)
BI_fit_gamLoess_degree <- fit_test(BI_set_gamLoess_degree)
# Extraire résultats d'intérêt : graphes et resultats
BI_fit_gamLoess_span_resultats <- BI_fit_gamLoess_span$results
BI_fit_gamLoess_span_resultats <- BI_fit_gamLoess_span_resultats %>% mutate(Jw = Sens*BI_RatioSens + Spec*BI_RatioSpec - 1)
BI_fit_gamLoess_span_graphe <- grapheSpeSenJw(BI_fit_gamLoess_span_resultats, "span")

BI_fit_gamLoess_degree_resultats <- BI_fit_gamLoess_degree$results
BI_fit_gamLoess_degree_resultats <- BI_fit_gamLoess_degree_resultats %>% mutate(Jw = Sens*BI_RatioSens + Spec*BI_RatioSpec - 1)
BI_fit_gamLoess_degree_graphe <- grapheSpeSenJw(BI_fit_gamLoess_degree_resultats, "degree")


# Modèles types arbres (RPART, RPARTCOST, CTREE, C50TREE)

BI_grid_rpart_cp <- data.frame(cp = 10^seq(from = -5, to = -1, by = .5))

BI_LHS <- nolhDesign(dimension = 2, range = c(0, 1))$design     # Hypercube latin quasi-orthogonal
BI_LHS <- data.frame(BI_LHS)
colnames(BI_LHS) <- c("X1", "X2")
BI_grid_rpartcost <- BI_LHS
BI_grid_rpartcost$cp <- BI_grid_rpartcost$X1*1e-2+1e-5
BI_grid_rpartcost$Cost <- BI_grid_rpartcost$X2*2.5+1e-3

BI_set_rpart_cp <- c("rpart", "tuneGrid  = BI_grid_rpart_cp")
BI_set_rpartcost <- c("rpartCost", "tuneGrid  = BI_grid_rpartcost[c('cp', 'Cost')]")

BI_set_ctree_criterion <- c("ctree", "tuneGrid  = data.frame(mincriterion = c(0.01, 0.25, 0.5, 0.75, 0.99))")
BI_set_c50tree <- c("C5.0Tree", "")
system.time(fit_test(BI_set_rpart_cp))    ####### CHRONO
BI_fit_rpart_cp <- fit_test(BI_set_rpart_cp)
BI_fit_rpartcost <- fit_test(BI_set_rpartcost)
BI_fit_ctree_criterion <- fit_test(BI_set_ctree_criterion)
BI_fit_c50tree <- fit_test(BI_set_c50tree)

# Extraire résultats d'intérêt : graphes et resultats
BI_fit_rpart_cp_resultats <- BI_fit_rpart_cp$results
BI_fit_rpart_cp_resultats <- BI_fit_rpart_cp_resultats %>% mutate(Jw = Sens*BI_RatioSens + Spec*BI_RatioSpec - 1)
BI_fit_rpart_cp_graphe <- grapheSpeSenJw(BI_fit_rpart_cp_resultats, cp) + scale_x_log10()

# Modèle quadratique
BI_fit_rpartcost_resultats <- BI_fit_rpartcost$results
BI_fit_rpartcost_resultats <- BI_fit_rpartcost_resultats %>% mutate(Jw = Sens*BI_RatioSens + Spec*BI_RatioSpec - 1)     # Calcul du Jw
BI_fit_rpartcost_resultats <- left_join(BI_fit_rpartcost_resultats, BI_grid_rpartcost, by = c("Cost", "cp"))      # Ajout des facteurs réduits

BI_mod_rpartcost_spec <- modelFit(X=BI_fit_rpartcost_resultats[,c("cp", "Cost")], Y=BI_fit_rpartcost_resultats$Spec,  type="Kriging", formula=Y~cp+Cost+cp:Cost+I(cp^2)+I(Cost^2))
BI_mod_rpartcost_sens <-  modelFit(X=BI_fit_rpartcost_resultats[,c("cp", "Cost")], Y=BI_fit_rpartcost_resultats$Sens,  type="Kriging", formula=Y~cp+Cost+cp:Cost+I(cp^2)+I(Cost^2))
BI_mod_rpartcost_jw <-  modelFit(X=BI_fit_rpartcost_resultats[,c("X1", "X2")], Y=BI_fit_rpartcost_resultats$Jw,  type="Kriging", formula=Y~X1+X2+X1:X2+I(X1^2)+I(X2^2))

BI_pred_rpartcost <- expand.grid(BI_fit_rpartcost_resultats[,1:2])
BI_pred_rpartcost <- left_join(BI_pred_rpartcost, BI_grid_rpartcost[,c("cp", "X1")], by = "cp")
BI_pred_rpartcost <- left_join(BI_pred_rpartcost, BI_grid_rpartcost[,c("Cost", "X2")], by = "Cost")
BI_pred_rpartcost2 <- NULL
BI_pred_rpartcost2$Spec <- modelPredict(BI_mod_rpartcost_spec, BI_pred_rpartcost[,c("cp", "Cost")])
BI_pred_rpartcost2$Sens <- modelPredict(BI_mod_rpartcost_sens, BI_pred_rpartcost[,c("cp", "Cost")])
BI_pred_rpartcost2$Jw <- modelPredict(BI_mod_rpartcost_jw, BI_pred_rpartcost[,c("X1", "X2")])
BI_pred_rpartcost <- cbind(BI_pred_rpartcost, BI_pred_rpartcost2)

# Graphes 2D
# BI_fit_rpartcost_spec_graphe <- graphe2D(BI_pred_rpartcost, BI_fit_rpartcost_resultats, Cost, cp, Spec, "F")
# BI_fit_rpartcost_sens_graphe <- graphe2D(BI_pred_rpartcost, BI_fit_rpartcost_resultats, Cost, cp, Sens, "G")
# BI_fit_rpartcost_jw_graphe <- graphe2D(BI_pred_rpartcost, BI_fit_rpartcost_resultats, X2, X1, Jw, "D")
BI_fit_rpartcost_spec_graphe <- graphe2D("BI_pred_rpartcost", "BI_fit_rpartcost_resultats", "Cost", "cp", "Spec", "F")
BI_fit_rpartcost_sens_graphe <- graphe2D("BI_pred_rpartcost", "BI_fit_rpartcost_resultats", "Cost", "cp", "Sens", "G")
BI_fit_rpartcost_jw_graphe <- graphe2D("BI_pred_rpartcost", "BI_fit_rpartcost_resultats", "X2", "X1", "Jw", "D")

# ctree pas dans le rapport ?????
BI_fit_ctree_criterion_resultats <- BI_fit_ctree_criterion$results
BI_fit_ctree_criterion_resultats <- BI_fit_ctree_criterion_resultats %>% mutate(Jw = Sens*BI_RatioSens + Spec*BI_RatioSpec - 1)
BI_fit_ctree_criterion_graphe <- grapheSpeSenJw(BI_fit_ctree_criterion_resultats, mincriterion)

BI_fit_c50tree_results <- BI_fit_c50tree$results


# Meilleur modèle CART
BI_best_rpartcost <- which.max(BI_fit_rpartcost_results$Spec^BI_RatioSpec*BI_fit_rpartcost_results$Sens^BI_RatioSens)
BI_best_rpartcostgrid <- data.frame(Cost = BI_fit_rpartcost_results[BI_best_rpartcost,]$Cost, cp =BI_fit_rpartcost_results[BI_best_rpartcost,]$cp)
BI_set_rpartcost_best <- c("rpartCost", paste0("tuneGrid  = BI_best_rpartcostgrid"))
BI_fit_rpartcost_best <- fit_test(BI_set_rpartcost_best)
BI_fit_rpartcost_best_results <- BI_fit_rpartcost_best$results


# Modèles type Random Forest (RFERNS, RANGER, RBORIST)
# Ranger OK, faire RBORIST avec les matrices X1 X2
BI_set_rFerns_depth <- c("rFerns", "tuneGrid  = data.frame(depth = 2^(1:5)/2)")

BI_grid_ranger <- BI_LHS
BI_grid_ranger <- rbind(BI_grid_ranger,BI_grid_ranger)
BI_grid_ranger$X3 <- c(rep(0, 17), rep(1, 17))
BI_grid_ranger$mtry <- round(1+BI_grid_ranger$X1*48,0)       # Si arrondi : prendre des multiples de 16 (car 17 points pour 2d)
BI_grid_ranger$min.node.size <- round(1+BI_grid_ranger$X2*32,0)
BI_grid_ranger$splitrule <- recode_factor(BI_grid_ranger$X3, "0" = "gini", "1" = "extratrees")


BI_grid_Rborist <- data.frame(BI_LHS)
colnames(BI_grid_Rborist) <- c("predFixed", "minNode")
BI_grid_Rborist$predFixed <- round(1+BI_grid_Rborist$predFixed*32,0)
BI_grid_Rborist$minNode <- round(1+BI_grid_Rborist$minNode*16,0)

BI_set_ranger <- c("ranger", "tuneGrid  = BI_grid_ranger[c('mtry', 'min.node.size', 'splitrule')], num.trees = 6") # OK

BI_set_Rborist <- c("Rborist", "tuneGrid  = BI_grid_Rborist")

BI_fit_rFerns_depth <- fit_test(BI_set_rFerns_depth)
BI_fit_ranger <- fit_test(BI_set_ranger)
BI_fit_Rborist <- fit_test(BI_set_Rborist)

# Extraire résultats d'intérêt : graphes et resultats
BI_fit_rFerns_depth_resultats <- BI_fit_rFerns_depth$results
BI_fit_rFerns_depth_resultats <-BI_fit_rFerns_depth_resultats %>% mutate(Jw = Sens*BI_RatioSens + Spec*BI_RatioSpec - 1)
BI_fit_rFerns_depth_graphe <- grapheSpeSenJw(BI_fit_rFerns_depth_resultats, depth)


BI_fit_ranger_resultats <- BI_fit_ranger$results
BI_fit_ranger_bestTune <- BI_fit_ranger$bestTune
BI_fit_Rborist_resultats <- BI_fit_Rborist$results
BI_fit_Rborist_bestTune <- BI_fit_Rborist$bestTune

BI_fit_ranger_resultats <- BI_fit_ranger_resultats %>% mutate(Jw = Sens*BI_RatioSens + Spec*BI_RatioSpec - 1)     # Calcul du Jw
BI_fit_ranger_resultats <- left_join(BI_fit_ranger_resultats, BI_grid_ranger, by = c("mtry", "min.node.size", "splitrule"))      # Ajout des facteurs réduits
BI_fit_ranger_GINI <- BI_fit_ranger_resultats %>% filter (splitrule == "gini")
BI_fit_ranger_ET <- BI_fit_ranger_resultats %>% filter (splitrule == "extratrees")
BI_mod_ranger_spec_GINI <- modelFit(X=BI_fit_ranger_GINI[,c("mtry", "min.node.size")], Y=BI_fit_ranger_GINI$Spec, type="Kriging", formula=Y~mtry+min.node.size+mtry:min.node.size+I(mtry^2)+I(min.node.size^2))
BI_mod_ranger_sens_GINI <- modelFit(X=BI_fit_ranger_GINI[,c("mtry", "min.node.size")], Y=BI_fit_ranger_GINI$Sens, type="Kriging", formula=Y~mtry+min.node.size+mtry:min.node.size+I(mtry^2)+I(min.node.size^2))
BI_mod_ranger_spec_ET <- modelFit(X=BI_fit_ranger_ET[,c("mtry", "min.node.size")], Y=BI_fit_ranger_ET$Spec, type="Kriging", formula=Y~mtry+min.node.size+mtry:min.node.size+I(mtry^2)+I(min.node.size^2))
BI_mod_ranger_sens_ET <- modelFit(X=BI_fit_ranger_ET[,c("mtry", "min.node.size")], Y=BI_fit_ranger_ET$Sens, type="Kriging", formula=Y~mtry+min.node.size+mtry:min.node.size+I(mtry^2)+I(min.node.size^2))
BI_mod_ranger_jw_GINI <- modelFit(X=BI_fit_ranger_GINI[,c("X1", "X2")], Y=BI_fit_ranger_ET$Jw, type="Kriging", formula=Y~X1+X2+X1:X2+I(X1^2)+I(X2^2))
BI_mod_ranger_jw_ET <- modelFit(X=BI_fit_ranger_ET[,c("X1", "X2")], Y=BI_fit_ranger_ET$Jw, type="Kriging", formula=Y~X1+X2+X1:X2+I(X1^2)+I(X2^2))
BI_mod_ranger_jw <-  modelFit(X=BI_fit_ranger_resultats[,c("X1", "X2", "X3")], 
                              Y=BI_fit_ranger_resultats$Jw,  
                              type="Kriging", 
                              formula=Y~X1+X2+X3+X1:X2+X2:X3+I(X1^2)+I(X2^2)+I(X3^2)) # BUGGGG ????#


BI_pred_ranger_GINI <- expand.grid(BI_fit_ranger_GINI[,1:2])
BI_pred_ranger_GINI$splitrule <- "gini"
BI_pred_ranger_GINI <- left_join(BI_pred_ranger_GINI, BI_grid_ranger[,c("mtry", "X1")], by = "mtry")
BI_pred_ranger_GINI <- left_join(BI_pred_ranger_GINI, BI_grid_ranger[,c("min.node.size", "X2")], by = "min.node.size")
BI_pred_ranger_GINI <- left_join(BI_pred_ranger_GINI, BI_grid_ranger[,c("splitrule", "X3")], by = "splitrule")
BI_pred_ranger_GINI <- unique(BI_pred_ranger_GINI)       # Nettoyage valeurs uniques (car BI_grid_ranger contient les lignes ET...)
BI_pred_ranger_GINI2 <- NULL
BI_pred_ranger_GINI2$Spec <- modelPredict(BI_mod_ranger_spec_GINI, BI_pred_ranger_GINI[,c("mtry", "min.node.size")])
BI_pred_ranger_GINI2$Sens <- modelPredict(BI_mod_ranger_sens_GINI, BI_pred_ranger_GINI[,c("mtry", "min.node.size")])
BI_pred_ranger_GINI2$Jw <- modelPredict(BI_mod_ranger_jw_GINI, BI_pred_ranger_GINI[,c("X1", "X2")])
BI_pred_ranger_GINI <- cbind(BI_pred_ranger_GINI, BI_pred_ranger_GINI2)

BI_pred_ranger_ET <- expand.grid(BI_fit_ranger_ET[,1:2])
BI_pred_ranger_ET$splitrule <- "extratrees"
BI_pred_ranger_ET <- left_join(BI_pred_ranger_ET, BI_grid_ranger[,c("mtry", "X1")], by = "mtry")
BI_pred_ranger_ET <- left_join(BI_pred_ranger_ET, BI_grid_ranger[,c("min.node.size", "X2")], by = "min.node.size")
BI_pred_ranger_ET <- left_join(BI_pred_ranger_ET, BI_grid_ranger[,c("splitrule", "X3")], by = "splitrule")
BI_pred_ranger_ET <- unique(BI_pred_ranger_ET)        # Nettoyage valeurs uniques (car BI_grid_ranger contient les lignes Gini...)
BI_pred_ranger_ET2 <- NULL
BI_pred_ranger_ET2$Spec <- modelPredict(BI_mod_ranger_spec_ET, BI_pred_ranger_ET[,c("mtry", "min.node.size")])
BI_pred_ranger_ET2$Sens <- modelPredict(BI_mod_ranger_sens_ET, BI_pred_ranger_ET[,c("mtry", "min.node.size")])
BI_pred_ranger_ET2$Jw <- modelPredict(BI_mod_ranger_jw_ET, BI_pred_ranger_ET[,c("X1", "X2")])
BI_pred_ranger_ET <- cbind(BI_pred_ranger_ET, BI_pred_ranger_ET2)

# Graphes 2D
BI_fit_ranger_Gini_spec_graphe <- graphe2D("BI_pred_ranger_GINI", "BI_fit_ranger_GINI", "mtry", "min.node.size", "Spec", "F")
BI_fit_ranger_Gini_sens_graphe <- graphe2D("BI_pred_ranger_GINI", "BI_fit_ranger_GINI", "mtry", "min.node.size", "Sens", "G")
BI_fit_ranger_Gini_jw_graphe <- graphe2D("BI_pred_ranger_GINI", "BI_fit_ranger_GINI", "X1", "X2", "Jw", "D")
BI_fit_ranger_ET_spec_graphe <- graphe2D("BI_pred_ranger_ET", "BI_fit_ranger_ET", "mtry", "min.node.size", "Spec", "F")
BI_fit_ranger_ET_sens_graphe <- graphe2D("BI_pred_ranger_GINI", "BI_fit_ranger_GINI", "mtry", "min.node.size", "Sens", "G")
BI_fit_ranger_ET_jw_graphe <- graphe2D("BI_pred_ranger_ET", "BI_fit_ranger_ET", "X1", "X2", "Jw", "D")

# BI_fit_ranger_Gini_spec_graphe <- graphe2D(BI_pred_ranger_GINI, BI_fit_ranger_GINI, mtry, min.node.size, Spec, "F")
# BI_fit_ranger_Gini_sens_graphe <- graphe2D(BI_pred_ranger_GINI, BI_fit_ranger_GINI, mtry, min.node.size, Sens, "G")
# BI_fit_ranger_Gini_jw_graphe <- graphe2D(BI_pred_ranger_GINI, BI_fit_ranger_GINI, X1, X2, Jw, "D")
# BI_fit_ranger_ET_spec_graphe <- graphe2D(BI_pred_ranger_ET, BI_fit_ranger_ET, mtry, min.node.size, Spec, "F")
# BI_fit_ranger_ET_sens_graphe <- graphe2D(BI_pred_ranger_ET, BI_fit_ranger_ET, mtry, min.node.size, Sens, "G")
# BI_fit_ranger_ET_jw_graphe <- graphe2D(BI_pred_ranger_ET, BI_fit_ranger_ET, X1, X2, Jw, "D")

BI_best_ranger <- which.max(BI_fit_ranger_results$Spec*BI_RatioSpec+BI_fit_ranger_results$Sens*BI_RatioSens)
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

# Graphes 2D
# BI_fit_Rborist_spec_graphe <- graphe2D(BI_pred_Rborist, BI_fit_Rborist_results, predFixed, minNode, Spec, "F")
# BI_fit_Rborist_sens_graphe <- graphe2D(BI_pred_Rborist, BI_fit_Rborist_results, predFixed, minNode, Sens, "G")
BI_fit_Rborist_spec_graphe <- graphe2D("BI_pred_Rborist", "BI_fit_Rborist_results", "predFixed", "minNode", "Spec", "F")
BI_fit_Rborist_sens_graphe <- graphe2D("BI_pred_Rborist", "BI_fit_Rborist_results", "predFixed", "minNode", "Sens", "G")

BI_best_Rborist <- which.max(BI_fit_Rborist_results$Spec*BI_RatioSpec+BI_fit_Rborist_results$Sens*BI_RatioSens)
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
BI_evaluation$reference <- as.logical(as.character(recode_factor(BI_evaluation$class, toxique = TRUE, comestible = FALSE))) # Bascule en booléen

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
# rm(dataset, BI_evaluation, BI_lot_appr_opti, BI_lot_apprentissage, BI_lot_evaluation,
#    BI_fit_pda_lambda, BI_fit_lda2_dim, BI_fit_gamLoess_degree, BI_fit_gamLoess_span,
#    BI_fit_rpart_cp, BI_fit_rpartcost, BI_fit_rpartcost_best,
#    BI_fit_ctree_criterion, BI_fit_c50tree, BI_fit_rFerns_depth, 
#    BI_fit_Rborist, BI_fit_Rborist_best, BI_fit_Rborist_final,
#    BI_fit_ranger, BI_fit_ranger_best, BI_fit_ranger_final)
save.image(file = "EKR-Champis-AnalyseBi.RData")     # Sauvegarde données pour rapport
load(file = "EKR-Champis-AnalyseBi.RData")     # Chargement données pour rapport
