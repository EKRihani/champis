library(tidyverse)   # Trousse à outils générique
library(microbenchmark)   # Benchmark de fonctions
#library(caret)       # Apprentissage machine
library(DataExplorer)   # Outils pour EDA
library(FactoMineR)
# library(factoextra)

N_diamant <- 2e2   # Facteur de répétition
sd_diamant <- .02  # Facteur de dispersion

diamants <- diamonds %>% slice(rep(1:n(), each = N_diamant)) # Répétition des données

# Appliquer un facteur aléatoire aux valeurs numériques
col_num_diamant <- names(select_if(diamants, is.numeric))
for(i in 1:length(col_num_diamant))
  {
  ma_colonne <- paste0("diamants$", col_num_diamant[i])
  ma_commande <- paste0(ma_colonne, "<-", ma_colonne, "* rnorm(n = nrow(diamants), mean = 1, sd = sd_diamant)")
  eval(parse(text = ma_commande))
  }


MB_graphes_diamant <- microbenchmark(
  UN = plot(x = diamants[1:1e1,]$x, y = diamants[1:1e1,]$y),
  DEUX = plot(x = diamants[1:1e2,]$x, y = diamants[1:1e2,]$y),
  TROIS = plot(x = diamants[1:1e3,]$x, y = diamants[1:1e3,]$y),
  QUATRE = plot(x = diamants[1:1e4,]$x, y = diamants[1:1e4,]$y),
  CINQ = plot(x = diamants[1:1e5,]$x, y = diamants[1:1e5,]$y),
  SIX = plot(x = diamants[1:1e6,]$x, y = diamants[1:1e6,]$y),
  times = 30,
  unit = "s"
)
microbenchmark:::boxplot.microbenchmark(MB_graphes_diamant, unit = "ms", log = FALSE)

MB_EDA_diamant <- microbenchmark(
   # UN = create_report(data = diamants[1:1e1,], y = "price"),
   # DEUX = create_report(data = diamants[1:1e2,], y = "price"),
   # TROIS = create_report(data = diamants[1:1e3,], y = "price"),
   # QUATRE = create_report(data = diamants[1:1e4,], y = "price"),
   CINQ = create_report(data = diamants[1:1e5,], y = "price"),
   SIX = create_report(data = diamants[1:1e6,], y = "price"),
   SEPT = create_report(data = diamants[1:1e7,], y = "price"),
   times = 2,
   unit = "s"
)

microbenchmark:::boxplot.microbenchmark(MB_EDA_diamant, unit = "s", log = TRUE)
   
r_pca <- FactoMineR::PCA(df)
r_ca <- FactoMineR::CA(df)
r_pca$eig
r_pca$var$contrib

factoextra::facto_summarize(r_pca, "var")
factoextra::facto_summarize(r_ca, "col")

factoextra::fviz_contrib(X=r_pca, choice="var", axes = 2)
factoextra::fviz_contrib(X=r_ca, choice="col")
