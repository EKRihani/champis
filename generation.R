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
data_champis <- read.csv(data_fichier, header = TRUE, sep = ";")


data_champis[242,]
