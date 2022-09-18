##########################
#     INITIALISATION     #
##########################

# Chargement des bibliothèques
library(tidyverse)    # Outils génériques
#library(rlang)      # Outils d'évaluation d'expression (parse/deparse)

# Récupération, décompression, importation des données
data_fichier <- tempfile()
#URL <- "https://github.com/EKRihani/champis/raw/master/Champignons.csv"      # Fichier distant
# download.file(URL, data_fichier)

data_fichier <- "~/projects/champis/fake.csv" # FICHIER LOCAL SIMULE
data_champis <- read.csv(data_fichier,
                         header = TRUE,
                         sep = ";",
                         dec = ",",
                         na.strings = TRUE,
                         stringsAsFactors = FALSE,
                         encoding = "UTF-8")


data_champis <- na_if(data_champis, "NA")
data_champis <- na_if(data_champis, "")

structure <- sapply(X = data_champis,
                    FUN = class,
                    simplify = TRUE)

n_especes <- nrow(data_champis)

champ_liste <- paste0("champ", data_champis$N)

for (n in 1:n_especes){
  assign(champ_liste[n], as.list(data_champis[n,]))
}

champ242$Habitat <- str_split(champ242$Habitat, ",\\s*", simplify = TRUE)

lot242 <- NULL
n_champis <- 1e5
f_crois <- 2
lot242$Habitat <- sample(x = champ242$Habitat, size = n_champis, replace = TRUE)
lot242$FacteurTaille <- rbeta(n = n_champis, shape1 = 6*f_crois, shape2 =4, ncp = .5*f_crois)
lot242$Chapeau.Diametre <- lot242$FacteurTaille*champ242$Chapeau.Diametre*rnorm(n = n_champis, mean = 1, sd = .1)
lot242$Pied.Hauteur <- lot242$FacteurTaille*champ242$Pied.Hauteur*rnorm(n = n_champis, mean = 1, sd = .1)
lot242$Pied.Largeur <- lot242$FacteurTaille*champ242$Pied.Largeur*rnorm(n = n_champis, mean = 1, sd = .1)
#plot(x= lot242$Chapeau.Diametre, y = lot242$Pied.Hauteur, col = rgb(0,0,0, alpha = 0.01), pch = 16)

ggplot(data = as.data.frame(lot242), aes(x = Chapeau.Diametre, y = Pied.Hauteur)) +
  # geom_density2d_filled(bins = 100) +
  # geom_density2d(bins=15, color = "white", alpha = .2) +
  # scale_fill_viridis_d(option = "H", direction = 1) + #B,F,G (H)
  # theme(legend.position="none") +
   stat_density_2d(geom = "polygon", contour = TRUE, contour_var = "density",
                   aes(fill = after_stat(level)),
                   bins = 200, n = 35) +
   scale_fill_viridis_c(option = "G", direction = -1) + #B,F,G (H)
  theme_classic() +
  geom_vline(xintercept = champ242$Chapeau.Diametre, linetype = "dotted", color = "red") +
  geom_hline(yintercept = champ242$Pied.Hauteur, linetype = "dotted", color = "red")

