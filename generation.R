##########################
#     INITIALISATION     #
##########################

# Chargement des bibliothèques
library(tidyverse)    # Outils génériques


# Récupération, décompression, importation des données
data_fichier <- tempfile()
#URL <- "https://github.com/EKRihani/champis/raw/master/Champignons.csv"      # Fichier distant
# download.file(URL, data_fichier)

data_fichier <- "~/projects/champis/Champignons.csv" # FICHIER LOCAL
data_champis <- read.csv(data_fichier,
                         header = TRUE,
                         sep = ";",
                         na.strings = TRUE,
                         stringsAsFactors = FALSE)

structure <- sapply(X = data_champis,
                    FUN = class,
                    simplify = TRUE)

champ242 <- as.list(data_champis[242,])
champ242 <- na_if(champ242, "NA")
champ242 <- na_if(champ242, "")
champ242$Chapeau.Diametre <- as.numeric(champ242$Chapeau.Diametre)  # Arranger csv, ne sera plus nécessaire
champ242$Pied.Hauteur <- as.numeric(champ242$Pied.Hauteur)  # Arranger csv, ne sera plus nécessaire
champ242$Pied.Largeur <- as.numeric(champ242$Pied.Largeur)  # Arranger csv, ne sera plus nécessaire
champ242$Habitat <- str_split(champ242$Habitat, ",\\s*", simplify = TRUE)


lot242 <- NULL
n_champis <- 1e5
f_crois <- 1.8
lot242$Habitat <- sample(x = champ242$Habitat, size = n_champis, replace = TRUE)
lot242$FacteurTaille <- rbeta(n = n_champis, shape1 = 6*f_crois, shape2 =4, ncp = .5*f_crois)
lot242$Chapeau.Diametre <- lot242$FacteurTaille*champ242$Chapeau.Diametre*rnorm(n = n_champis, mean = 1, sd = .05)
lot242$Pied.Hauteur <- lot242$FacteurTaille*champ242$Pied.Hauteur*rnorm(n = n_champis, mean = 1, sd = .05)
lot242$Pied.Largeur <- lot242$FacteurTaille*champ242$Pied.Largeur*rnorm(n = n_champis, mean = 1, sd = .05)
plot(x= lot242$Chapeau.Diametre, y = lot242$Pied.Hauteur)

ggplot(data = as.data.frame(lot242), aes(x = Chapeau.Diametre, y = Pied.Hauteur)) +
#  geom_density2d_filled(bins = 100) +
#  scale_fill_viridis_d(option = "F", direction = -1) + #B,F,G
#  scale_fill_grey(start =1, end =0) +
  theme_classic() +
  geom_point(alpha=.03, shape = 21, fill = "black")  +
  geom_vline(xintercept = champ242$Chapeau.Diametre, linetype = "dotted", color = "red") +
  geom_hline(yintercept = champ242$Pied.Hauteur, linetype = "dotted", color = "red") +
    theme(legend.position="none")
