##########################
#     INITIALISATION     #
##########################

library(tidyverse)
library(DiceDesign)
library(DiceEval)
library(caret)
library(twinning)


fichier_data <- tempfile()
fichier_data <- "~/projects/champis/MushroomDataset.zip"
fichier_data <- unzip(fichier_data, "MushroomDataset/secondary_data.csv")
dataset <- read.csv(fichier_data, 
                    header = TRUE, 
                    sep = ";", 
                    stringsAsFactors = TRUE)
dataset$class <- recode_factor(dataset$class, e = "comestible", p = "toxique")
dataset$class <- relevel(dataset$class, ref = "toxique")


########################################
#     CREATION DES JEUX DE DONNEES     #
########################################

CodBI_n_champis <- nrow(dataset)
CodBI_split_p <- sqrt(CodBI_n_champis)
CodBI_split_facteur <- round(sqrt(CodBI_split_p)+1)


#####################################
#     ARBRE DECISIONNEL : RPART     #
#####################################

CodBI_n_champis <- nrow(dataset)
CodBI_split_p <- sqrt(CodBI_n_champis)
CodBI_split_facteur <- round(sqrt(CodBI_split_p)+1)


set.seed(7)
index1 <- twin(data = dataset, r = CodBI_split_facteur)
CodBI_lot_appr_opti <- dataset[-index1,]
CodBI_lot_evaluation <- dataset[index1,]


CodBI_w <- 10
CodBI_RatioSens <- 2*CodBI_w/(CodBI_w+1)
CodBI_RatioSpec <- 2*(1-CodBI_w/(CodBI_w+1))


CodBI_grid_rpart_cp <- data.frame(cp = 10^seq(from = -5, to = -1, by = .5))


set.seed(1)
tr_ctrl <- trainControl(classProbs = TRUE,
                        summaryFunction = twoClassSummary,
                        method = "cv",
                        number = CodBI_split_facteur)


CodBI_fit_rpart_cp <- train(class ~ .,
                         method = "rpart",
                         data = CodBI_lot_appr_opti,
                         trControl = tr_ctrl,
                         tuneGrid  = CodBI_grid_rpart_cp)



CodBI_fit_rpart_cp_resultats <- CodBI_fit_rpart_cp$results %>% 
   mutate(Jw = Sens*CodBI_RatioSens + Spec*CodBI_RatioSpec - 1)

CodBI_fit_rpart_cp_graphe <- ggplot(data = CodBI_fit_rpart_cp_resultats, aes(x = cp)) +
      geom_line(aes(y = Sens, color = "Sensibilité")) +
      geom_point(aes(y = Sens, color = "Sensibilité")) +
      geom_line(aes(y = Spec, color = "Spécificité")) +
      geom_point(aes(y = Spec, color = "Spécificité")) +
      geom_line(aes(y = Jw, color = "Jw")) +
      geom_point(aes(y = Jw, color = "Jw")) +
      ylab(NULL) + 
      labs(color = "Performance") + 
      theme_bw() + 
      scale_x_log10()
CodBI_fit_rpart_cp_graphe


#####################################
#     FORET ALEATOIRE : RBORIST     #
#####################################


CodBI_LHS <- nolhDesign(dimension = 2, range = c(0, 1))$design 
CodBI_LHS <- data.frame(CodBI_LHS)
colnames(CodBI_LHS) <- c("X1", "X2")


CodBI_grid_Rborist <- data.frame(CodBI_LHS) %>%
   mutate(predFixed = round(1+X1*16,0)) %>%
   mutate(minNode = round(1+X2*16,0))


set.seed(1)
tr_ctrl <- trainControl(classProbs = TRUE,
                        summaryFunction = twoClassSummary,
                        method = "cv",
                        number = CodBI_split_facteur)
CodBI_fit_Rborist <- train(class ~ .,
                         method = "Rborist",
                         data = CodBI_lot_appr_opti,
                         trControl = tr_ctrl,
                         tuneGrid  = CodBI_grid_Rborist[c('predFixed', 'minNode')])


CodBI_fit_Rborist_resultats <- CodBI_fit_Rborist$results %>% 
   mutate(Jw = Sens*CodBI_RatioSens + Spec*CodBI_RatioSpec - 1) %>%
   left_join(x = .,
             y = CodBI_grid_Rborist,
             by = c("predFixed", "minNode"))


CodBI_mod_Rborist_jw <-  modelFit(X = CodBI_fit_Rborist_resultats[,c("X1", "X2")], 
                               Y = CodBI_fit_Rborist_resultats$Jw,  
                               type="Kriging", 
                               formula= Y ~ X1 + X2 + X1:X2 + I(X1^2) + I(X2^2))


CodBI_pred_Rborist <- expand.grid(CodBI_fit_Rborist_resultats[,c("X1","X2")]) %>%
   mutate(Jw = modelPredict(CodBI_mod_Rborist_jw, .[,c("X1","X2")]))


Code_Rborist_graphe_raster <- CodBI_pred_Rborist %>% ggplot() +
   geom_raster(data = CodBI_pred_Rborist,
               aes(x = X1, y = X2, fill = Jw), interpolate = TRUE) +
   scale_fill_viridis_c(option = "D", direction = 1) +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5)) +
   theme(legend.position='bottom') +
   theme_bw()

Code_Rborist_graphe_tiles <- CodBI_pred_Rborist %>% ggplot() +
   geom_tile(data = CodBI_fit_Rborist_resultats,
             aes(x = X1, y = X2, fill = Jw), color = 'black', linewidth =.5) +
   scale_fill_viridis_c(option = "D", direction = 1) +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5)) +
   theme(legend.position='bottom') +
   theme_bw()

Code_Rborist_graphe_full <- CodBI_pred_Rborist %>% ggplot() +
   geom_raster(data = CodBI_pred_Rborist,
               aes(x = X1, y = X2, fill = Jw), interpolate = TRUE) +
   geom_tile(data = CodBI_fit_Rborist_resultats,
             aes(x = X1, y = X2, fill = Jw), color = 'black', linewidth =.5) +
   scale_fill_viridis_c(option = "D", direction = 1) +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5)) +
   theme(legend.position='bottom') +
   theme_bw()


save.image(file = "EKR-Champis-CodeSourceBi.RData")

load(file = "EKR-Champis-CodeSourceBi.RData")     # Chargement données pour rapport
