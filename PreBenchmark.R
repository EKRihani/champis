library(tidyverse)
library(microbenchmark)   # Benchmark de fonctions
N_diamant <- 5   # Facteur de répétition
sd_diamant <- .02  # Facteur de dispersion

diamants <- diamonds %>% slice(rep(1:n(), each = N_diamant)) # Répétition des données
#diamants <- diamants[1:64,]   # 64 premières valeurs    [POUR DEBUG]

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
  times = 30,
  unit = "s"
)
microbenchmark:::boxplot.microbenchmark(MB_graphes_diamant, unit = "ms", log = FALSE)
