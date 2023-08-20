library(tidyverse)
iris_lot <- iris %>% filter(Species != "virginica") %>% droplevels()

# Moyennes intraclasses
iris_M <- iris_lot %>% 
   aggregate(. ~ Species, mean) %>%
   add_row(cbind("Species" = "difference", 
                 .[1, names(.)!="Species"] - .[2, names(.)!="Species"])) %>%
   column_to_rownames("Species")

# Différences, puis carrés des différences (table III)
iris_deltas <- iris_lot %>%
   group_by(Species) %>%
   mutate_all(~. - mean(.)) %>%
   ungroup() %>% select(!Species) %>%
   as.matrix()

iris_prods <- rbind(iris_deltas[,1] %*% iris_deltas,
                    iris_deltas[,2] %*% iris_deltas,
                    iris_deltas[,3] %*% iris_deltas,
                    iris_deltas[,4] %*% iris_deltas)
rownames(iris_prods) <- colnames(iris_prods)

iris_InvProds <- solve(iris_prods) %>% as.matrix() # Matrice inverse (table IV)
iris_Coeffs <- as.matrix(iris_M["difference",]) %*% iris_InvProds # Coeffs bruts
iris_CoeffsNorm <- iris_Coeffs/iris_Coeffs[1]  # Normalisation

# Coeffs et graphiques LDA
iris_lot <- iris_lot %>% 
   mutate(X = rowSums(mapply(`*`,.[,names(.)!="Species"],iris_CoeffsNorm)))

iris_grapheMAX <- iris_lot %>% 
   ggplot(aes(x = Petal.Width, y = Petal.Length, color= Species)) +
   geom_point()

iris_grapheMin <- iris_lot %>% 
   ggplot(aes(x = Sepal.Width, y = Sepal.Length, color= Species)) +
   geom_point()

iris_grapheX <- iris_lot %>% 
   ggplot(aes(x = X, fill = Species)) +
   geom_histogram()

iris_norm <- iris_lot %>% 
   mutate_at(., scale, .vars = which(names(.)!="Species")) %>% 
   pivot_longer(data = ., cols = which(names(.)!="Species"))

iris_grapheTotale <- iris_norm %>%
   ggplot(aes(x = name, y = value, fill = Species)) +
   geom_boxplot()

save.image(file = "EKR-Champis-CodeSource2.RData")
load(file = "EKR-Champis-CodeSource2.RData")