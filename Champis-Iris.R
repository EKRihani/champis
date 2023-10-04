library(caret)
library(tidyverse)
data(iris)

###############################
#     CLASSIFIEUR BINAIRE     #
###############################

# Moyennes intraclasses
iris_lot <- iris %>% filter(Species != "virginica") %>% droplevels()
colnames(iris_lot) <- c("Lon.S.", "Lar.S.", "Lon.P.", "Lar.P.", "Espece")

# iris_moyennes <- iris_lot %>%
#    group_by(Espece) %>% 
#    summarise(across(where(is.numeric), mean)) %>%
#    data.frame()
iris_moyennes <- iris_lot %>% aggregate(. ~ Espece, mean)

# Différences des moyennes interclasses (table II)
# iris_D <- as.matrix(iris_moyennes[which(iris_moyennes$Espece == "setosa"),2:5]
#                     - iris_moyennes[which(iris_moyennes$Espece == "versicolor"),2:5])
# 
# iris_M <- rbind(iris_moyennes[,2:5], iris_D)
# rownames(iris_M) <- c("setosa", "versicolor", "diff.")

iris_M <- iris_lot %>% 
   aggregate(. ~ Espece, mean) %>%
   add_row(cbind("Espece" = "difference", 
                 .[1, names(.)!="Espece"] - .[2, names(.)!="Espece"])) %>%
   column_to_rownames("Espece")

# Produits des carrés : 1. Différences avec moyennes
iris_deltas <- iris_lot %>%
   group_by(Espece) %>%
   mutate_all(~. - mean(.)) %>%
   ungroup() %>% select(!Espece) %>%
   as.matrix()
colnames(iris_deltas) <- c("Lon.S.","Lar.S.","Lon.P.","Lar.P.")

# Produits des carrés : 2. Carrés des différences (table III)
iris_prods <- rbind(iris_deltas[,1] %*% iris_deltas,
                    iris_deltas[,2] %*% iris_deltas,
                    iris_deltas[,3] %*% iris_deltas,
                    iris_deltas[,4] %*% iris_deltas)
rownames(iris_prods) <- colnames(iris_prods)

# Matrice inverse des produits des carrés des différences (table IV)
iris_InvProds <- solve(iris_prods) %>% as.matrix()

# Multiplication de la matrice inverse avec les différences
iris_Coeffs <- as.matrix(iris_M["difference",]) %*% iris_InvProds

# Normalisation
iris_CoeffsNorm <- iris_Coeffs/iris_Coeffs[1]

# Comparaison avec LDA via package MASS
#library(MASS)
iris_lda <- MASS::lda(formula = Espece ~ ., 
                data = iris_lot)
iris_lda$means
iris_moyennes
iris_lda$scaling/iris_lda$scaling[1,1]
iris_CoeffsNorm


# Coeffs et graphiques LDA

iris_totale <- iris_lot %>% 
   mutate(X = rowSums(mapply(`*`,.[,names(.)!="Espece"],iris_CoeffsNorm)))

iris_grapheMAX <- iris_totale %>% ggplot(aes(x = Lar.P., y = Lon.P., color= Espece)) +
   labs(x = "Largeur Pétale", y = "Longueur Pétale", color = "Variété") +
   geom_point() +
   scale_color_viridis_d(end = .75, option = "D") +
   theme_bw()

iris_grapheMin <- iris_totale %>% ggplot(aes(x = Lar.S., y = Lon.S., color= Espece)) +
   labs(x = "Largeur Sépale", y = "Longueur Sépale", color = "Variété") +
   geom_point() +
   scale_color_viridis_d(end = .75, option = "D") +
   theme_bw()

iris_grapheX <- iris_totale %>%
   ggplot(aes(x = X, fill = Espece)) +
   labs(x = "X", y = "Nombre", fill = "Variété") +   
   geom_histogram(alpha = .8, color = "black") +
   #   geom_vline(xintercept = mean(iris_totale$X), color = "red", linetype = "dashed", alpha = .8) +
   scale_fill_viridis_d(end = .75, option = "D") +
   ylim(0, 15) +
   theme_bw()

iris_norm <- iris_totale %>% 
   mutate_at(., scale, .vars = which(names(.)!="Espece")) %>% 
   pivot_longer(data = ., cols = which(names(.)!="Espece"))

iris_grapheTotale <- iris_norm %>%
   ggplot(aes(x = name, y = value, fill = Espece, color = Espece)) +
   labs(x = "", y = "Valeur", color = "Variété", fill = "Variété") +   
   geom_boxplot(alpha = .8, color = "black") +
   scale_fill_viridis_d(end = .75, option = "D") +
   theme_bw()# + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# rpart avec caret
n_iris <- nrow(iris)
iris_split_p <- sqrt(n_iris)
iris_split_facteur <- round(sqrt(iris_split_p)+1)

iris_grid_rpart_cp <- data.frame(cp = 10^seq(from = -11, to = -1, by = 2))

set.seed(1)
tr_ctrl <- trainControl(classProbs = TRUE,
                        summaryFunction = multiClassSummary,     # multiClassSummary, twoClassSummary
                        method = "cv",
                        number = iris_split_facteur)

iris_rpart <- train(Espece ~ .,
                  method = "rpart",
                  data =  iris,
                  trControl = tr_ctrl,
                  tuneGrid  = iris_grid_rpart_cp)


library(rpart.plot)
pdf("IrisCARTArbre.pdf", width = 3, height = 3, pointsize = 16)
rpart.plot(x = iris_rpart$finalModel, type = 4, extra = 8, branch = 1.0, under = TRUE, box.palette = "Blues")
dev.off()

iris_graphe_arbre <- iris %>% ggplot(aes(x = Petal.Width, y = Petal.Length, color= Espece)) +
   labs(x = "Largeur Pétale", y = "Longueur Pétale", color = "Variété") +
   xlim(0,2.5) + ylim(1,7) +
   geom_point(size = 1) +
   scale_color_viridis_d(end = .75, option = "D") +
   geom_hline(yintercept = 2.5, linetype = "dotted", colour = "black", linewidth= .3) +
   geom_segment(aes(x =1.8, y=2.5, xend=1.8, yend=7), linetype = "dotted", colour = "black", linewidth= .3) +
   theme_bw()
   

pdf("IrisCARTGraphe.pdf", width = 3.5, height = 3, pointsize = 5)
iris_graphe_arbre
dev.off()


save.image(file = "EKR-Champis-Iris.RData")
load(file = "EKR-Champis-Iris.RData")


###############################
#     ARBRES SUR CHAMPIS      #
###############################

# Initialisation
library(twinning)
library(rpart.plot)

# Récupération, décompression, importation des données
fichier_data <- tempfile()
#URL <- "https://github.com/EKRihani/champis/raw/master/lot_champis.zip"      # URL de mon repo
# download.file(URL, fichier_data)
fichier_data <- "~/projects/champis/lot_champis.zip" # FICHIER LOCAL
fichier_data <- unzip(fichier_data, "lot_champis.csv")
dataset <- read.csv(fichier_data, header = TRUE, sep = ",", stringsAsFactors = TRUE)
dataset$Type <- relevel(dataset$Type, ref = "Rejeter")
dataset <- dataset %>% select(-c("Nom", "Groupe"))

# Création des lots d'entraînement, validation, évaluation

INTRO_n_champis <- nrow(dataset)
INTRO_split_p <- sqrt(INTRO_n_champis)
INTRO_split_facteur <- round(sqrt(INTRO_split_p)+1)

set.seed(007)
index1 <- twin(data = dataset, r = INTRO_split_facteur)
INTRO_lot_appr_opti <- dataset[-index1,]
INTRO_lot_evaluation <- dataset[index1,]

# Initialisation du biclassifieur

tr_ctrl <- trainControl(classProbs = TRUE,
                        summaryFunction = twoClassSummary,
                        method = "cv",
                        number = INTRO_split_facteur)

# Lancement du biclassifieur

INTRO_fit_rpart2 <- train(Type ~ .,
                         method = "rpart2",
                         data = INTRO_lot_appr_opti,
                         trControl = tr_ctrl,
                         tuneGrid  = data.frame(maxdepth = 4)) 


pdf("IntroChampisCART2Arbre.pdf", width = 8, height = 5, pointsize = 16)
rpart.plot(x = INTRO_fit_rpart2$finalModel, type = 4, extra = 8, branch = 1.0, under = TRUE, box.palette = "Blues")
dev.off()


INTRO_fit_rpart <- train(Type ~ .,
                         method = "rpart",
                         data = INTRO_lot_appr_opti,
                         trControl = tr_ctrl,
                         tuneGrid  = data.frame(cp = 1e-5)) 

pdf("IntroChampisCART1Arbre.pdf", width = 8, height = 10, pointsize = 0.1)
plot(INTRO_fit_rpart$finalModel)
dev.off()


save.image(file = "EKR-Champis-Iris.RData")

rm(dataset, iris_rpart, index1,
   INTRO_fit_rpart, INTRO_fit_rpart2, INTRO_lot_appr_opti, INTRO_lot_evaluation)

save.image(file = "EKR-Champis-Iris-Light.RData")
load(file = "EKR-Champis-Iris.RData")
