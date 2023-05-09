library(MASS)
library(tidyverse)
data(iris)

###############################
#     CLASSIFIEUR BINAIRE     #
###############################

# Moyennes intraclasses
iris_moyennes <- iris %>%
   filter(Species %in% c("versicolor", "setosa")) %>%
   group_by(Species) %>% 
   summarise(across(where(is.numeric), mean)) %>%
   data.frame()


# Différences des moyennes interclasses (table II)
iris_D <- as.matrix(iris_moyennes[which(iris_moyennes$Species == "setosa"),2:5]
                    - iris_moyennes[which(iris_moyennes$Species == "versicolor"),2:5])

iris_M <- rbind(iris_moyennes[,2:5], iris_D)
rownames(iris_M) <- c("setosa", "versicolor", "diff.")


# Produits des carrés : 1. Différences avec moyennes
iris_delta <- iris %>%
   filter(Species %in% c("versicolor", "setosa")) %>%
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
iris_SV <- iris %>% filter(Species %in% c("setosa", "versicolor") ) %>% droplevels()

iris_lda <- lda(formula = Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                data = iris_SV)
iris_lda

iris_lda$means
iris_moyennes
iris_lda$scaling/iris_lda$scaling[1,1]
iris_CoeffsNorm

save.image(file = "EKR-Champis-Iris.RData")
load(file = "EKR-Champis-Iris.RData")


#########
# TRUCS #
#########

# Produits des carrés des différences (table VII)
deltas_set <- delta %>% 
   filter(Species == "setosa") %>%
   select(SL:PW) %>%
   as.matrix()
prods_set <- rbind(deltas_set[,1] %*% deltas_set,
                   deltas_set[,2] %*% deltas_set,
                   deltas_set[,3] %*% deltas_set,
                   deltas_set[,4] %*% deltas_set)

deltas_ver <- delta %>% 
   filter(Species == "versicolor") %>%
   select(SL:PW) %>%
   as.matrix()
prods_ver <- rbind(deltas_ver[,1] %*% deltas_ver,
                   deltas_ver[,2] %*% deltas_ver,
                   deltas_ver[,3] %*% deltas_ver,
                   deltas_ver[,4] %*% deltas_ver)


