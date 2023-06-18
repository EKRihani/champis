library(MASS)
library(caret)
library(tidyverse)
data(iris)

###############################
#     CLASSIFIEUR BINAIRE     #
###############################

# Moyennes intraclasses
iris_lot <- iris %>%
   filter(Species %in% c("versicolor", "setosa")) %>% 
   droplevels()

iris_moyennes <- iris_lot %>%
   group_by(Species) %>% 
   summarise(across(where(is.numeric), mean)) %>%
   data.frame()


# Différences des moyennes interclasses (table II)
iris_D <- as.matrix(iris_moyennes[which(iris_moyennes$Species == "setosa"),2:5]
                    - iris_moyennes[which(iris_moyennes$Species == "versicolor"),2:5])

iris_M <- rbind(iris_moyennes[,2:5], iris_D)
rownames(iris_M) <- c("setosa", "versicolor", "diff.")


# Produits des carrés : 1. Différences avec moyennes
iris_delta <- iris_lot %>%
   group_by(Species) %>% 
   mutate(S.l = Sepal.Length - mean(Sepal.Length),
           S.w = Sepal.Width - mean(Sepal.Width),
           P.l = Petal.Length - mean(Petal.Length),
           P.w = Petal.Width - mean(Petal.Width))

# Produits des carrés : 2. Carrés des différences (table III)
iris_deltas <-as.matrix(iris_delta[,c("S.l","S.w","P.l","P.w")])

iris_prods <- rbind(iris_deltas[,1] %*% iris_deltas,
                    iris_deltas[,2] %*% iris_deltas,
                    iris_deltas[,3] %*% iris_deltas,
                    iris_deltas[,4] %*% iris_deltas)
rownames(iris_prods) <- c("S.l", "S.w", "P.l", "P.w")

# Matrice inverse des produits des carrés des différences (table IV)
iris_InvProds <- solve(iris_prods) %>% as.matrix()

# Multiplication de la matrice inverse avec les différences
iris_Coeffs <- iris_D %*% iris_InvProds

# Normalisation
iris_CoeffsNorm <- iris_Coeffs/iris_Coeffs[1]

iris_L_set <- iris_moyennes %>%
   filter(Species == "setosa") %>%
   select(-Species) %>%
   as.matrix() %>%
   "*" (iris_CoeffsNorm) %>%
   sum()

iris_L_ver <- iris_moyennes %>%
   filter(Species == "versicolor") %>%
   select(-Species) %>%
   as.matrix() %>%
   "*" (iris_CoeffsNorm) %>%
   sum()


# LDA avec package MASS
iris_lda <- lda(formula = Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                data = iris_lot)
iris_lda

iris_lda$means
iris_moyennes
iris_lda$scaling/iris_lda$scaling[1,1]
iris_CoeffsNorm


# Coeffs et graphiques LDA

iris_totale <- iris_lot %>% mutate(X = Sepal.Length*iris_CoeffsNorm[1] +
                                      Sepal.Width*iris_CoeffsNorm[2] +
                                      Petal.Length*iris_CoeffsNorm[3] +
                                      Petal.Width*iris_CoeffsNorm[4])

iris_grapheMAX <- iris_lot %>% ggplot(aes(x = Petal.Width, y = Petal.Length, color= Species)) +
   labs(x = "Largeur Pétale", y = "Longueur Pétale", color = "Variété") +
   geom_point() +
   scale_color_viridis_d(end = .75, option = "D") +
   theme_bw()

iris_grapheMin <- iris_lot %>% ggplot(aes(x = Sepal.Width, y = Sepal.Length, color= Species)) +
   labs(x = "Largeur Sépale", y = "Longueur Sépale", color = "Variété") +
   geom_point() +
   scale_color_viridis_d(end = .75, option = "D") +
   theme_bw()

iris_grapheX <- iris_totale %>%
   ggplot(aes(x = X, fill = Species)) +
   labs(x = "X", y = "Nombre", fill = "Variété") +   
   geom_histogram(alpha = .8, color = "black") +
   #   geom_vline(xintercept = mean(iris_totale$X), color = "red", linetype = "dashed", alpha = .8) +
   scale_fill_viridis_d(end = .75, option = "D") +
   ylim(0, 15) +
   theme_bw()

iris_norm <- iris_totale %>%
   mutate(Ls = scale(Sepal.Length),
          ls = scale(Sepal.Width),
          lp = scale(Petal.Width),
          Lp = scale(Petal.Length),
          X = scale(X))

iris_norm <-pivot_longer(data = iris_norm, cols = c("Ls", "ls", "lp", "Lp", "X"))  

iris_grapheTotale <- iris_norm %>%
   ggplot(aes(x = name, y = value, fill = Species, color = Species)) +
   labs(x = "", y = "Valeur", color = "Variété", fill = "Variété") +   
   geom_boxplot(alpha = .8, color = "black") +
   scale_fill_viridis_d(end = .75, option = "D") +
   theme_bw()


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


iris_rpart <- train(Species ~ .,
                  method = "rpart",
                  data =  iris,
                  trControl = tr_ctrl,
                  tuneGrid  = iris_grid_rpart_cp)



library(rpart.plot)
pdf("IrisCARTArbre.pdf", width = 3, height = 3, pointsize = 16)
rpart.plot(x = iris_rpart$finalModel, type = 4, extra = 8, branch = 1.0, under = TRUE, box.palette = "Blues")
dev.off()

iris_graphe_arbre <- iris %>% ggplot(aes(x = Petal.Width, y = Petal.Length, color= Species)) +
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


#########
# TRUCS #
#########

# Produits des carrés des différences (table VII)
# deltas_set <- delta %>% 
#    filter(Species == "setosa") %>%
#    select(SL:PW) %>%
#    as.matrix()
# prods_set <- rbind(deltas_set[,1] %*% deltas_set,
#                    deltas_set[,2] %*% deltas_set,
#                    deltas_set[,3] %*% deltas_set,
#                    deltas_set[,4] %*% deltas_set)
# 
# deltas_ver <- delta %>% 
#    filter(Species == "versicolor") %>%
#    select(SL:PW) %>%
#    as.matrix()
# prods_ver <- rbind(deltas_ver[,1] %*% deltas_ver,
#                    deltas_ver[,2] %*% deltas_ver,
#                    deltas_ver[,3] %*% deltas_ver,
#                    deltas_ver[,4] %*% deltas_ver)



############################
#     ARBRES / CHAMPIS     #
############################

# Initialisation
library(twinning)
library(rpart.plot)

fichier_data <- tempfile()
#URL <- "https://github.com/EKRihani/champis/raw/master/MushroomDataset.zip"      # URL de mon repo
# download.file(URL, fichier_data)
fichier_data <- "~/projects/champis/MushroomDataset.zip" # FICHIER LOCAL
fichier_data <- unzip(fichier_data, "MushroomDataset/secondary_data.csv")
dataset <- read.csv(fichier_data, header = TRUE, sep = ";", stringsAsFactors = TRUE)
dataset$class <- recode_factor(dataset$class, e = "comestible", p = "toxique")
dataset$class <- relevel(dataset$class, ref = "toxique")

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

INTRO_fit_rpart2 <- train(class ~ .,
                         method = "rpart2",
                         data = INTRO_lot_appr_opti,
                         trControl = tr_ctrl,
                         tuneGrid  = data.frame(maxdepth = 4)) 


pdf("IntroChampisCART2Arbre.pdf", width = 8, height = 5, pointsize = 16)
rpart.plot(x = INTRO_fit_rpart2$finalModel, type = 4, extra = 8, branch = 1.0, under = TRUE, box.palette = "Blues")
dev.off()


INTRO_fit_rpart <- train(class ~ .,
                         method = "rpart",
                         data = INTRO_lot_appr_opti,
                         trControl = tr_ctrl,
                         tuneGrid  = data.frame(cp = 1e-5)) 

pdf("IntroChampisCART1Arbre.pdf", width = 8, height = 10, pointsize = 0.1)
plot(INTRO_fit_rpart$finalModel)
dev.off()




save.image(file = "EKR-Champis-Iris.RData")
load(file = "EKR-Champis-Iris.RData")
