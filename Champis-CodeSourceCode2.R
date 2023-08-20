library(tidyverse)
RMD_Iris_Lot <- iris %>% filter(Species != "virginica") %>% droplevels()
colnames(RMD_Iris_Lot) <- c("Long.Sep", "Larg.Sep", "Long.Pet", "Larg.Pet", "Espece")

# Moyennes intraclasses
RMD_Iris_Moyennes <- RMD_Iris_Lot %>% 
   aggregate(. ~ Espece, mean) %>%
   add_row(cbind("Espece"="difference", 
                 .[1, names(.) != "Espece"] - .[2, names(.) != "Espece"])) %>%
   column_to_rownames("Espece")

# Différences, puis carrés des différences (table III)
RMD_Iris_Deltas <- RMD_Iris_Lot %>%
   group_by(Espece) %>%
   mutate_all(~. - mean(.)) %>%
   ungroup() %>% select(!Espece) %>%
   as.matrix()
RMD_Iris_Produits <- rbind(RMD_Iris_Deltas[,1] %*% RMD_Iris_Deltas,
                    RMD_Iris_Deltas[,2] %*% RMD_Iris_Deltas,
                    RMD_Iris_Deltas[,3] %*% RMD_Iris_Deltas,
                    RMD_Iris_Deltas[,4] %*% RMD_Iris_Deltas)
rownames(RMD_Iris_Produits) <- colnames(RMD_Iris_Produits)

RMD_Iris_InvProduits <- solve(RMD_Iris_Produits) %>% as.matrix() # Matrice inverse (table IV)
RMD_Iris_Coeffs <- as.matrix(RMD_Iris_Moyennes["difference",]) %*% RMD_Iris_InvProduits # Coeffs bruts
RMD_Iris_CoeffsNorm <- RMD_Iris_Coeffs / RMD_Iris_Coeffs[1]  # Normalisation

# Coeffs et graphiques LDA
RMD_Iris_Lot <- RMD_Iris_Lot %>% 
   mutate(X=rowSums(mapply(`*`,.[,names(.) != "Espece"],RMD_Iris_CoeffsNorm)))

RMD_Iris_GraphMAX <-ggplot(data=RMD_Iris_Lot, aes(x=Larg.Pet, y=Long.Pet, color=Espece)) + geom_point()

RMD_Iris_GraphMin <-ggplot(data=RMD_Iris_Lot, aes(x=Larg.Sep, y=Long.Sep, color=Espece)) + geom_point()

RMD_Iris_GraphX <- ggplot(data=RMD_Iris_Lot, aes(x=X, fill=Espece)) + geom_histogram()

RMD_Iris_Norm <- RMD_Iris_Lot %>% 
   mutate_at(., scale, .vars=which(names(.) != "Espece")) %>% 
   pivot_longer(data=., cols=which(names(.) != "Espece"))

RMD_Iris_GraphTotale <- ggplot(data=RMD_Iris_Norm, aes(x=name, y=value, fill=Espece)) + geom_boxplot()

save.image(file="EKR-Champis-CodeSourceIris.RData")
load(file="EKR-Champis-CodeSourceIris.RData")