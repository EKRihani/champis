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
#URL <- "https://github.com/EKRihani/champis/raw/master/lot_champis.zip"      # URL de mon repo
# download.file(URL, fichier_data)

fichier_data <- "~/projects/champis/lot_champis.zip" # FICHIER LOCAL
fichier_data <- unzip(fichier_data, "lot_champis.csv")
dataset <- read.csv(fichier_data, header = TRUE, sep = ",", stringsAsFactors = TRUE)
dataset$Type <- relevel(dataset$Type, ref = "Rejeter")
dataset <- dataset %>% select(!Nom) %>% select(!Groupe)

####################################################################
#     CREATION DES LOTS D'ENTRAINEMENT, VALIDATION, EVALUATION     #
####################################################################

BI_n_champis <- nrow(dataset)
BI_split_p <- sqrt(BI_n_champis)
BI_split_facteur <- round(sqrt(BI_split_p)+1)

set.seed(007)
index1 <- twin(data = dataset, r = BI_split_facteur)
BI_lot_appr_opti <- dataset[-index1,]
BI_lot_evaluation <- dataset[index1,]


###################################################################
#     BICLASSIFIEUR : INITIALISATION ET DEFINITIONS FONCTIONS     #
###################################################################
# https://topepo.github.io/caret/available-models.html
# names(getModelInfo())
# getModelInfo(Rborist)

# Définition index de Youden
BI_w <- 10
BI_RatioSens <- 2*BI_w/(BI_w+1)
BI_RatioSpec <- 2*(1-BI_w/(BI_w+1))


# Définition de fonction : lance le modèle avec les paramètres donnés, évalue la performance (spécificité), renvoie les résultats de fitting
fit_test <- function(fcn_modele){
   set.seed(1)
   tr_ctrl <- trainControl(classProbs = TRUE, 
                           summaryFunction = twoClassSummary, 
                           method = "cv", 
                           number = BI_split_facteur)   # Règle paramètres d'évaluation performance à twoClassSummary (ROC, Sens, Spec), avec cross-validation (n-fold)
   cmd <- paste0("train(Type ~ ., method = '",      # Construit commande, évaluation de performance par Spécificité
                 fcn_modele[1], 
                 "', data = BI_lot_appr_opti, trControl = tr_ctrl, metric = 'Spec',", # voir si métrique adaptée?
                 fcn_modele[2],")")
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
   theme(legend.position='bottom') + 
   theme(legend.text = element_text(angle = -45, vjust = 1, hjust = 0))"
   )
   eval(parse(text = cmd))
}

# Version PLUS PROPRE, mais à débuguer...
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
      ylab(NULL) + labs(colour = "Performance") + theme_bw() +
      scale_color_viridis_d(option = "B", direction = 1, begin = 0, end = .8)
}

########################################################
#     BICLASSIFIEUR : ANALYSE DISCRIMINANTE ET GAM     #
########################################################

# Début Parallélisation (A TESTER !!!)
# library(doParallel)
# cl <- makeCluster(spec = 5, type = "PSOCK")
# registerDoParallel(cl)
# [Code à paralléliser]
# stopCluster(cl)

### LDA2 ###
set.seed(362)
start_time <- Sys.time()
#BI_grid_lda_dimen <- data.frame(dimen = seq(from = 1, to = 50, by = 6))
BI_grid_lda_dimen <- data.frame(dimen = round(seq(from = 1, to = 20, length.out = 6),0))
BI_set_lda2_dim <- c("lda2", "tuneGrid  = BI_grid_lda_dimen")
BI_fit_lda2_dim <- fit_test(BI_set_lda2_dim)
BI_fit_lda2_dim_resultats <- BI_fit_lda2_dim$results %>% mutate(Jw = Sens*BI_RatioSens + Spec*BI_RatioSpec - 1)
BI_fit_lda2_dim_graphe <- grapheSpeSenJw(BI_fit_lda2_dim_resultats, dimen)
end_time <- Sys.time()
BI_temps_lda2 <- difftime(end_time, start_time, units = "mins") %>% as.numeric
BI_temps_lda2 <- round(BI_temps_lda2/nrow(BI_grid_lda_dimen) ,2)

 ### PDA ###
set.seed(67)
start_time <- Sys.time()
BI_grid_pda_lambda <- data.frame(lambda = round(seq(from = 1, to = 65, length.out = 6),0))
BI_set_pda_lambda <- c("pda", "tuneGrid  = BI_grid_pda_lambda")
BI_fit_pda_lambda <- fit_test(BI_set_pda_lambda)
BI_fit_pda_lambda_resultats <- BI_fit_pda_lambda$results %>% mutate(Jw = Sens*BI_RatioSens + Spec*BI_RatioSpec - 1)
BI_fit_pda_lambda_graphe <- grapheSpeSenJw(BI_fit_pda_lambda_resultats, lambda)
end_time <- Sys.time()
BI_temps_pda <- difftime(end_time, start_time, units = "mins") %>% as.numeric
BI_temps_pda <- round(BI_temps_pda/nrow(BI_grid_pda_lambda) ,2)

###############################################
#     BICLASSIFIEUR : ARBRES DECISIONNELS     #
###############################################

# Définition de l'hypercube latin quasi-orthogonal (NOLH)
BI_LHS <- nolhDesign(dimension = 2, range = c(0, 1))$design     # Hypercube latin quasi-orthogonal
BI_LHS <- data.frame(BI_LHS)
colnames(BI_LHS) <- c("X1", "X2")


### C 5.0 TREE ###
set.seed(62)
start_time <- Sys.time()
BI_set_c50tree <- c("C5.0Tree", "")
BI_fit_c50tree <- fit_test(BI_set_c50tree)
BI_fit_c50tree_resultats <- BI_fit_c50tree$results %>% mutate(Jw = Sens*BI_RatioSens + Spec*BI_RatioSpec - 1)
end_time <- Sys.time()
BI_temps_c50tree <- difftime(end_time, start_time, units = "mins") %>% as.numeric %>% round(.,2)

### RPART ###
set.seed(262)
start_time <- Sys.time()
BI_grid_rpart_cp <- data.frame(cp = 10^seq(from = -5, to = -1, by = .5))
BI_set_rpart_cp <- c("rpart", "tuneGrid  = BI_grid_rpart_cp")
BI_fit_rpart_cp <- fit_test(BI_set_rpart_cp)
#system.time(fit_test(BI_set_rpart_cp))    ####### CHRONO
BI_fit_rpart_cp_resultats <- BI_fit_rpart_cp$results %>% mutate(Jw = Sens*BI_RatioSens + Spec*BI_RatioSpec - 1)
BI_fit_rpart_cp_graphe <- grapheSpeSenJw(BI_fit_rpart_cp_resultats, cp) + scale_x_log10()
end_time <- Sys.time()
BI_temps_rpart <- difftime(end_time, start_time, units = "mins") %>% as.numeric
BI_temps_rpart <- round(BI_temps_rpart/nrow(BI_grid_rpart_cp) ,2)

### RPARTCOST ###
set.seed(3)
start_time <- Sys.time()
BI_grid_rpartcost <- BI_LHS
BI_grid_rpartcost <- BI_grid_rpartcost %>%
   mutate(cp = X1*1e-2+1e-5) %>%
   mutate(Cost = X2*2.5+1e-3)
BI_set_rpartcost <- c("rpartCost", "tuneGrid  = BI_grid_rpartcost[c('cp', 'Cost')]")
BI_fit_rpartcost <- fit_test(BI_set_rpartcost)
end_time <- Sys.time()
BI_temps_rpartcost <- difftime(end_time, start_time, units = "mins") %>% as.numeric
BI_temps_rpartcost <- round(BI_temps_rpartcost/nrow(BI_grid_rpartcost) ,2)

# Modèle quadratique
BI_fit_rpartcost_resultats <- BI_fit_rpartcost$results %>% mutate(Jw = Sens*BI_RatioSens + Spec*BI_RatioSpec - 1)     # Calcul du Jw
BI_fit_rpartcost_resultats <- left_join(BI_fit_rpartcost_resultats, BI_grid_rpartcost, by = c("Cost", "cp"))      # Ajout des facteurs réduits

BI_mod_rpartcost_spec <- modelFit(X=BI_fit_rpartcost_resultats[,c("cp", "Cost")], 
                                  Y=BI_fit_rpartcost_resultats$Spec,  
                                  type="Kriging", 
                                  formula=Y~cp+Cost+cp:Cost+I(cp^2)+I(Cost^2))
BI_mod_rpartcost_sens <-  modelFit(X=BI_fit_rpartcost_resultats[,c("cp", "Cost")], 
                                   Y=BI_fit_rpartcost_resultats$Sens,  
                                   type="Kriging", 
                                   formula=Y~cp+Cost+cp:Cost+I(cp^2)+I(Cost^2))
BI_mod_rpartcost_jw <-  modelFit(X=BI_fit_rpartcost_resultats[,c("X1", "X2")], 
                                 Y=BI_fit_rpartcost_resultats$Jw,  
                                 type="Kriging", 
                                 formula=Y~X1+X2+X1:X2+I(X1^2)+I(X2^2))

BI_pred_rpartcost <- expand.grid(BI_fit_rpartcost_resultats[,c("X1", "X2")]) %>%
   mutate(cp = X1*1e-2+1e-5) %>%
   mutate(Cost = X2*2.5+1e-3) %>%
   mutate(Spec = modelPredict(BI_mod_rpartcost_spec, .[,c("cp", "Cost")])) %>%
   mutate(Sens = modelPredict(BI_mod_rpartcost_sens, .[,c("cp", "Cost")])) %>%
   mutate(Jw =  modelPredict(BI_mod_rpartcost_jw, .[,c("X1", "X2")]))

# Modélisation quadratique rpartcost
BI_modelquad_rpartcost <- expand.grid(X1 = seq(from = 0, to = 1, by = 0.01), X2 = seq(from = 0, to = 1, by = 0.01))
BI_modelquad_rpartcost <- BI_modelquad_rpartcost %>% 
   mutate(cp = X1*1e-2+1e-5) %>%
   mutate(Cost = X2*2.5+1e-3) %>% 
   mutate(Jw = BI_mod_rpartcost_jw$model@trend.coef[1] +
               BI_mod_rpartcost_jw$model@trend.coef[2]*X1 +
               BI_mod_rpartcost_jw$model@trend.coef[3]*X2 +
               BI_mod_rpartcost_jw$model@trend.coef[4]*X1^2 +
               BI_mod_rpartcost_jw$model@trend.coef[5]*X2^2 +
               BI_mod_rpartcost_jw$model@trend.coef[6]*X1*X2)
BI_modelquad_rpartcost_top <- BI_modelquad_rpartcost[which.max(BI_modelquad_rpartcost$Jw),]

# Erreur de modélisation quadratique
BI_Compar_rpartcost <- BI_fit_rpartcost_resultats %>% 
   select(c("X1","X2","Jw")) %>%
   mutate(Jw2 = BI_mod_rpartcost_jw$model@trend.coef[1] +
             BI_mod_rpartcost_jw$model@trend.coef[2]*X1 +
             BI_mod_rpartcost_jw$model@trend.coef[3]*X2 +
             BI_mod_rpartcost_jw$model@trend.coef[4]*X1^2 +
             BI_mod_rpartcost_jw$model@trend.coef[5]*X2^2 +
             BI_mod_rpartcost_jw$model@trend.coef[6]*X1*X2)
BI_RMSE_rpartcost <-  RMSE(BI_Compar_rpartcost$Jw, BI_Compar_rpartcost$Jw2)
BI_MAE_rpartcost <-  MAE(BI_Compar_rpartcost$Jw, BI_Compar_rpartcost$Jw2)

# Meilleur modèle rpartcost
BI_best_rpartcostgrid <- BI_modelquad_rpartcost_top[c("cp", "Cost")]
BI_set_rpartcost_best <- c("rpartCost", paste0("tuneGrid  = BI_best_rpartcostgrid"))
BI_fit_rpartcost_best <- fit_test(BI_set_rpartcost_best)
BI_fit_rpartcost_best_resultats <- BI_fit_rpartcost_best$results %>% mutate(Jw = Sens*BI_RatioSens + Spec*BI_RatioSpec - 1)

# Graphes 2D
# BI_fit_rpartcost_spec_graphe <- graphe2D(BI_pred_rpartcost, BI_fit_rpartcost_resultats, Cost, cp, Spec, "F")
BI_fit_rpartcost_spec_graphe <- graphe2D("BI_pred_rpartcost", "BI_fit_rpartcost_resultats", "Cost", "cp", "Spec", "F")
BI_fit_rpartcost_sens_graphe <- graphe2D("BI_pred_rpartcost", "BI_fit_rpartcost_resultats", "Cost", "cp", "Sens", "G")
BI_fit_rpartcost_jw_graphe <- graphe2D("BI_pred_rpartcost", "BI_fit_rpartcost_resultats", "X2", "X1", "Jw", "D")

# + geom_point(x = BI_modelquad_rpartcost_top$X2, 
#              y =BI_modelquad_rpartcost_top$X1, 
#              shape = 3, color = "red", size = 4)

#############################################
#     BICLASSIFIEUR : FORETS ALEATOIRES     #
#############################################

### RFERNS ###
set.seed(6945)

start_time <- Sys.time()
BI_grid_rFerns_depth <- data.frame(depth = 2^(1:5)/2)
BI_set_rFerns_depth <- c("rFerns", "tuneGrid  = BI_grid_rFerns_depth")
BI_fit_rFerns_depth <- fit_test(BI_set_rFerns_depth)
BI_fit_rFerns_depth_resultats <- BI_fit_rFerns_depth$results %>% mutate(Jw = Sens*BI_RatioSens + Spec*BI_RatioSpec - 1)
BI_fit_rFerns_depth_graphe <- grapheSpeSenJw(BI_fit_rFerns_depth_resultats, depth)
end_time <- Sys.time()
BI_temps_rferns <- difftime(end_time, start_time) %>% as.numeric
BI_temps_rferns <- round(BI_temps_rferns/nrow(BI_grid_rFerns_depth) ,2)

### RANGER ###
set.seed(694)
start_time <- Sys.time()
BI_grid_ranger <- rbind(BI_LHS,BI_LHS) %>%
   mutate(X3 = c(rep(0, 17), rep(1, 17))) %>%
   mutate(mtry = round(1+X1*48,0)) %>%    # Prendre des multiples de 16 (car 17 points pour 2d)
   mutate(min.node.size = round(1+X2*32,0)) %>%
   mutate(splitrule = case_when(X3 == 0 ~ "gini", X3 == 1 ~ "extratrees"))
BI_set_ranger <- c("ranger", "tuneGrid  = BI_grid_ranger[c('mtry', 'min.node.size', 'splitrule')], num.trees = 6") # OK
BI_fit_ranger <- fit_test(BI_set_ranger)
end_time <- Sys.time()
BI_temps_ranger <- difftime(end_time, start_time, units = "mins") %>% as.numeric
BI_temps_ranger <- round(BI_temps_ranger/nrow(BI_grid_ranger) ,2)

BI_fit_ranger_resultats <- BI_fit_ranger$results %>% 
   mutate(Jw = Sens*BI_RatioSens + Spec*BI_RatioSpec - 1) %>%   # Calcul du Jw
   left_join(., BI_grid_ranger, by = c("mtry", "min.node.size", "splitrule"))   # Ajout des facteurs réduits

# Modélisation quadratique avec interactions
BI_mod_ranger_sens <-  modelFit(X=BI_fit_ranger_resultats[,c("mtry", "min.node.size", "X3")], 
                              Y=BI_fit_ranger_resultats$Sens,  
                              type="Kriging", 
                              formula=Y~mtry+min.node.size+X3+mtry:min.node.size+min.node.size:X3+mtry:X3+I(mtry^2)+I(min.node.size^2))
BI_mod_ranger_spec <-  modelFit(X=BI_fit_ranger_resultats[,c("mtry", "min.node.size", "X3")], 
                              Y=BI_fit_ranger_resultats$Spec,  
                              type="Kriging", 
                              formula=Y~mtry+min.node.size+X3+mtry:min.node.size+min.node.size:X3+mtry:X3+I(mtry^2)+I(min.node.size^2))
BI_mod_ranger_jw <-  modelFit(X=BI_fit_ranger_resultats[,c("X1", "X2", "X3")], 
                              Y=BI_fit_ranger_resultats$Jw,  
                              type="Kriging", 
                              formula=Y~X1+X2+X3+X1:X2+X2:X3+X1:X3+I(X1^2)+I(X2^2))

BI_pred_ranger <- expand(BI_fit_ranger_resultats[,c("X1","X2","X3")], X1, X2, X3) %>%
   data.frame() %>%
   mutate(mtry = round(1+X1*48,0)) %>%
   mutate(min.node.size = round(1+X2*32,0)) %>%
   mutate(splitrule = case_when(X3 == 0 ~ "gini", X3 == 1 ~ "extratrees")) %>%
   mutate(Spec = modelPredict(BI_mod_ranger_spec, .[,c("mtry", "min.node.size", "X3")])) %>%
   mutate(Sens = modelPredict(BI_mod_ranger_sens, .[,c("mtry", "min.node.size", "X3")])) %>%
   mutate(Jw = modelPredict(BI_mod_ranger_jw, .[,c("X1","X2","X3")]))

BI_pred_ranger_ET <- BI_pred_ranger %>% filter(splitrule == "extratrees")
BI_pred_ranger_GINI <- BI_pred_ranger %>% filter(splitrule == "gini")

BI_fit_ranger_ET <- BI_fit_ranger_resultats %>% filter(splitrule == "extratrees")
BI_fit_ranger_GINI <- BI_fit_ranger_resultats %>% filter(splitrule == "gini")


# Optimisation quadratique
BI_modelquad_ranger <- expand.grid(X1 = seq(from = 0, to = 1, length.out = 49), X2 = seq(from = 0, to = 1, length.out = 33), X3 = c(0,1))
BI_modelquad_ranger <- BI_modelquad_ranger %>% 
   mutate(mtry = round(1+X1*48,0)) %>%
   mutate(min.node.size = round(1+X2*32,0)) %>%
   mutate(splitrule = case_when(X3 == 0 ~ "gini", X3 == 1 ~ "extratrees")) %>%
   mutate(Jw = BI_mod_ranger_jw$model@trend.coef[1] +
             BI_mod_ranger_jw$model@trend.coef[2]*X1 +
             BI_mod_ranger_jw$model@trend.coef[3]*X2 +
             BI_mod_ranger_jw$model@trend.coef[4]*X3 +
             BI_mod_ranger_jw$model@trend.coef[5]*X1^2 +
             BI_mod_ranger_jw$model@trend.coef[6]*X2^2 +
             BI_mod_ranger_jw$model@trend.coef[7]*X1*X2 +
             BI_mod_ranger_jw$model@trend.coef[8]*X2*X3 +
             BI_mod_ranger_jw$model@trend.coef[9]*X1*X3)

# Erreur de modélisation quadratique
BI_Compar_Ranger <- BI_fit_ranger_resultats %>%
   select(c("X1","X2","X3","Jw")) %>%
   mutate(Jw2 = BI_mod_ranger_jw$model@trend.coef[1] +
             BI_mod_ranger_jw$model@trend.coef[2]*X1 +
             BI_mod_ranger_jw$model@trend.coef[3]*X2 +
             BI_mod_ranger_jw$model@trend.coef[4]*X3 +
             BI_mod_ranger_jw$model@trend.coef[5]*X1^2 +
             BI_mod_ranger_jw$model@trend.coef[6]*X2^2 +
             BI_mod_ranger_jw$model@trend.coef[7]*X1*X2 +
             BI_mod_ranger_jw$model@trend.coef[8]*X2*X3 +
             BI_mod_ranger_jw$model@trend.coef[9]*X1*X3)
BI_RMSE_ranger <-  RMSE(BI_Compar_ranger$Jw, BI_Compar_Ranger$Jw2)
BI_MAE_ranger <-  MAE(BI_Compar_ranger$Jw, BI_Compar_Ranger$Jw2)

set.seed(945)
BI_modelquad_ranger_top <- BI_modelquad_ranger[which.max(BI_modelquad_ranger$Jw),]
BI_best_rangergrid <- BI_modelquad_ranger_top %>% select(c("mtry", "min.node.size", "splitrule"))
BI_set_ranger_best <- c("ranger", paste0("tuneGrid  = BI_best_rangergrid"))
BI_fit_ranger_best <- fit_test(BI_set_ranger_best)
BI_fit_ranger_best_resultats <- BI_fit_ranger_best$results %>% mutate(Jw = Sens*BI_RatioSens + Spec*BI_RatioSpec - 1)

# Graphes 2D
BI_fit_ranger_Gini_spec_graphe <- graphe2D("BI_pred_ranger_GINI", "BI_fit_ranger_GINI", "mtry", "min.node.size", "Spec", "F")
BI_fit_ranger_Gini_sens_graphe <- graphe2D("BI_pred_ranger_GINI", "BI_fit_ranger_GINI", "mtry", "min.node.size", "Sens", "G")
BI_fit_ranger_Gini_jw_graphe <- graphe2D("BI_pred_ranger_GINI", "BI_fit_ranger_GINI", "X1", "X2", "Jw", "D")
BI_fit_ranger_ET_spec_graphe <- graphe2D("BI_pred_ranger_ET", "BI_fit_ranger_ET", "mtry", "min.node.size", "Spec", "F")
BI_fit_ranger_ET_sens_graphe <- graphe2D("BI_pred_ranger_GINI", "BI_fit_ranger_GINI", "mtry", "min.node.size", "Sens", "G")
BI_fit_ranger_ET_jw_graphe <- graphe2D("BI_pred_ranger_ET", "BI_fit_ranger_ET", "X1", "X2", "Jw", "D")

# + geom_point(x = BI_modelquad_ranger_top$X1, 
#              y =BI_modelquad_ranger_top$X2, 
#              shape = 3, color = "red", size = 4)


### RBORIST ###
set.seed(645)
start_time <- Sys.time()
BI_grid_Rborist <- data.frame(BI_LHS) %>%
   mutate(predFixed = round(1+X1*16,0)) %>%
   mutate(minNode = round(1+X2*16,0))
BI_set_Rborist <- c("Rborist", "tuneGrid  = BI_grid_Rborist[c('predFixed', 'minNode')]")
BI_fit_Rborist <- fit_test(BI_set_Rborist)
end_time <- Sys.time()
BI_temps_Rborist <- difftime(end_time, start_time, units = "mins") %>% as.numeric
BI_temps_Rborist <- round(BI_temps_Rborist/nrow(BI_grid_Rborist) ,2)

BI_fit_Rborist_resultats <- BI_fit_Rborist$results %>% mutate(Jw = Sens*BI_RatioSens + Spec*BI_RatioSpec - 1) %>%   # Calcul du Jw
   left_join(., BI_grid_Rborist, by = c("predFixed", "minNode"))   # Ajout des facteurs réduits

BI_mod_Rborist_spec <- modelFit(X=BI_fit_Rborist_resultats[,c("predFixed", "minNode")], 
                                Y=BI_fit_Rborist_resultats$Spec,  
                                type="Kriging", 
                                formula=Y~predFixed+minNode+predFixed:minNode+I(predFixed^2)+I(minNode^2))
BI_mod_Rborist_sens <-  modelFit(X=BI_fit_Rborist_resultats[,c("predFixed", "minNode")], 
                                 Y=BI_fit_Rborist_resultats$Sens,  
                                 type="Kriging", 
                                 formula=Y~predFixed+minNode +predFixed:minNode+I(predFixed^2)+I(minNode^2))
BI_mod_Rborist_jw <-  modelFit(X=BI_fit_Rborist_resultats[,c("X1", "X2")], 
                               Y=BI_fit_Rborist_resultats$Jw,  
                               type="Kriging", 
                               formula=Y~X1+X2+X1:X2+I(X1^2)+I(X2^2))


BI_pred_Rborist <- expand.grid(BI_fit_Rborist_resultats[,c("X1","X2")]) %>%
   mutate(predFixed = round(1+X1*16,0)) %>%
   mutate(minNode = round(1+X2*16,0)) %>%
   mutate(Spec = modelPredict(BI_mod_Rborist_spec, .[,c("predFixed", "minNode")])) %>%
   mutate(Sens = modelPredict(BI_mod_Rborist_sens, .[,c("predFixed", "minNode")])) %>%
   mutate(Jw = modelPredict(BI_mod_Rborist_jw, .[,c("X1","X2")]))


# Optimisation quadratique
BI_modelquad_Rborist <- expand.grid(X1 = seq(from = 0, to = 1, length.out = 17), X2 = seq(from = 0, to = 1, length.out = 17)) %>% 
   mutate(predFixed = round(1+X1*16,0)) %>%
   mutate(minNode = round(1+X2*16,0)) %>%
   mutate(Jw = BI_mod_Rborist_jw$model@trend.coef[1] +
             BI_mod_Rborist_jw$model@trend.coef[2]*X1 +
             BI_mod_Rborist_jw$model@trend.coef[3]*X2 +
             BI_mod_Rborist_jw$model@trend.coef[4]*X1^2 +
             BI_mod_Rborist_jw$model@trend.coef[5]*X2^2 +
             BI_mod_Rborist_jw$model@trend.coef[6]*X1*X2)

# Erreur de modélisation quadratique
BI_Compar_Rborist <- BI_fit_ranger_resultats %>% 
   select(c("X1","X2","Jw")) %>%
   mutate(Jw2 = BI_mod_Rborist_jw$model@trend.coef[1] +
             BI_mod_Rborist_jw$model@trend.coef[2]*X1 +
             BI_mod_Rborist_jw$model@trend.coef[3]*X2 +
             BI_mod_Rborist_jw$model@trend.coef[4]*X1^2 +
             BI_mod_Rborist_jw$model@trend.coef[5]*X2^2 +
             BI_mod_Rborist_jw$model@trend.coef[6]*X1*X2)
BI_RMSE_Rborist <-  RMSE(BI_Compar_Rborist$Jw, BI_Compar_Rborist$Jw2)
BI_MAE_Rborist <-  MAE(BI_Compar_Rborist$Jw, BI_Compar_Rborist$Jw2)

set.seed(65)
BI_modelquad_Rborist_top <- BI_modelquad_Rborist[which.max(BI_modelquad_Rborist$Jw),]
BI_best_Rboristgrid <- BI_modelquad_Rborist_top %>% select(c("predFixed", "minNode"))
BI_set_Rborist_best <- c("Rborist", paste0("tuneGrid  = BI_modelquad_Rborist_top, ntrees = 3"))
BI_fit_Rborist_best <- fit_test(BI_set_Rborist_best)
BI_fit_Rborist_best_resultats <- BI_fit_Rborist_best$results %>% mutate(Jw = Sens*BI_RatioSens + Spec*BI_RatioSpec - 1)


# Graphes 2D
BI_fit_Rborist_spec_graphe <- graphe2D("BI_pred_Rborist", "BI_fit_Rborist_resultats", "predFixed", "minNode", "Spec", "F")
BI_fit_Rborist_sens_graphe <- graphe2D("BI_pred_Rborist", "BI_fit_Rborist_resultats", "predFixed", "minNode", "Sens", "G")
BI_fit_Rborist_jw_graphe <- graphe2D("BI_pred_Rborist", "BI_fit_Rborist_resultats", "X1", "X2", "Jw", "D")

# + geom_point(x = BI_modelquad_Rborist_top$X1, 
#              y =BI_modelquad_Rborist_top$X2, 
#              shape = 3, color = "red", size = 4)

#########################################################
#     PERFORMANCE DES MODELES SUR LOT D'EVALUATION      #
#########################################################

# Règle la liste de prédiction et lance la classification
BI_evaluation <- BI_lot_evaluation %>%
   mutate(reference = as.factor(case_when(Type == "Rejeter" ~ TRUE, Type == "Conserver" ~ FALSE)))

# A SUPPRIMER ???
# BI_evaluation$reference <- as.logical(as.character(recode_factor(BI_evaluation$class, toxique = TRUE, comestible = FALSE))) # Bascule en booléen
# 
# # Passe .$reference de booléen à facteur, puis calcule la matrice de confusion
# BI_evaluation$reference <- as.factor(BI_evaluation$reference)

set.seed(695)
start_time <- Sys.time()
cmd <- paste0("train(Type ~ ., method = 'ranger', data = BI_lot_appr_opti,", BI_set_ranger_best[2], ")")
BI_fit_ranger_final <- eval(parse(text = cmd)) 
BI_pred_ranger_final <- predict(object = BI_fit_ranger_final, newdata = BI_lot_evaluation)
BI_CM_ranger_final <- confusionMatrix(data = BI_pred_ranger_final, reference = BI_lot_evaluation$Type)
end_time <- Sys.time()
BI_temps_ranger_final <- difftime(end_time, start_time, units = "mins") %>% as.numeric %>% round(.,2)
BI_resultats_ranger <- BI_CM_ranger_final$byClass %>% 
   t(.) %>% as.data.frame(.) %>% 
   select(c(Sensitivity, Specificity)) %>% 
   mutate(Jw = Sensitivity*BI_RatioSens + Specificity*BI_RatioSpec - 1) %>%
   mutate(temps = BI_temps_ranger)

# Test Matrice de confusion plus belle... A TESTER/FINIR
# BI_CM <- BI_CM_ranger_final$table %>%
#    as.data.frame(.) %>%
#    pivot_wider(., names_from = Reference, values_from= Freq) %>%
#    as.data.frame(.)

set.seed(45)
start_time <- Sys.time()
cmd <- paste0("train(Type ~ ., method = 'Rborist', data = BI_lot_appr_opti,", BI_set_Rborist_best[2], ")") # Construction de la commande
BI_fit_Rborist_final <- eval(parse(text = cmd))     # Exécution de la commande
BI_pred_Rborist_final <- predict(object = BI_fit_Rborist_final, newdata = BI_lot_evaluation)
BI_CM_Rborist_final <- confusionMatrix(data = BI_pred_Rborist_final, reference = BI_lot_evaluation$Type)
end_time <- Sys.time()
BI_temps_Rborist_final <- difftime(end_time, start_time, units = "mins") %>% as.numeric %>% round(.,2)
BI_resultats_Rborist <- BI_CM_Rborist_final$byClass %>% 
   t(.) %>% as.data.frame(.) %>% 
   select(c(Sensitivity, Specificity)) %>% 
   mutate(Jw = Sensitivity*BI_RatioSens + Specificity*BI_RatioSpec - 1) %>%
   mutate(temps = BI_temps_Rborist)

BI_RF_resultat <- rbind(BI_resultats_ranger, BI_resultats_Rborist)
rownames(BI_RF_resultat) <- c("Ranger", "Rborist")
colnames(BI_RF_resultat) <- c("Sensibilité", "Spécificité", "J de Youden", "Durée (min)")


save.image(file = "EKR-Champis-AnalyseBi.RData")     # Sauvegarde données complètes

# Suppression gros fichiers intermédiaires, avant sauvegarde
rm(dataset, BI_evaluation, BI_lot_appr_opti, BI_lot_evaluation,
   BI_fit_pda_lambda, BI_fit_lda2_dim, 
   BI_fit_rpart_cp, BI_fit_rpartcost, BI_fit_rpartcost_best,
   BI_fit_c50tree, BI_fit_rFerns_depth,
   BI_fit_Rborist, BI_fit_Rborist_best, BI_fit_Rborist_final,
   BI_fit_ranger, BI_fit_ranger_best, BI_fit_ranger_final)


save.image(file = "EKR-Champis-AnalyseBi-Light.RData")     # Sauvegarde données pour rapport
load(file = "EKR-Champis-AnalyseBi.RData")     # Chargement données complètes
