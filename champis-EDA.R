##########################
#     INITIALISATION     #
##########################

# Chargement des bibliothèques
library(tidyverse)    # Outils génériques
library(GGally)       # Outils supplémentaires pour les graphiques
library(caret)       # Outils d'apprentissage machine (split des partitions)

# Récupération, décompression, importation des données
fichier_data <- tempfile()
#URL <- "https://github.com/EKRihani/mushrooms/raw/master/MushroomDataset.zip"      # URL de mon repo
# download.file(URL, fichier_data)
fichier_data <- "~/projects/champis/MushroomDataset.zip" # FICHIER LOCAL

fichier_data <- unzip(fichier_data, "MushroomDataset/secondary_data.csv")
dataset <- read.csv(fichier_data, header = TRUE, sep = ";", stringsAsFactors = TRUE)

########################################
#  FORMATTAGE / NETTOYAGE DES DONNEES  #
########################################

# Récupération de la structure des données
structure_initial <- sapply(X = dataset, FUN = class, simplify = TRUE)  # Classes des variables
unique_length <- function (x) {length(unique(x))}                       # FONCTION : compter les niveaux d'une variable
structure_uniques <- sapply(dataset, FUN = unique_length)               # Comptage des niveaux de toutes les variables
structure_dataset <- data.frame(cbind(structure_initial, structure_uniques))
colnames(structure_dataset) <- c("Type", "Niveaux")
structure_dataset$Niveaux <- as.numeric(as.character(structure_dataset$Niveaux))
dataset_noms <- row.names(structure_dataset)

################################
#     ANALYSE INTRODUCTIVE     #
################################

# Résumés introductifs
synthese_nombre <- nrow(dataset)  # Comptage champis
synthese_dataset <- summary(dataset) # Résumé des catégories

##############################################################################
#     CREATION DES LOTS D'ENTRAINEMENT, VALIDATION, EVALUATION + GRAPHES     #
##############################################################################

split1 <- 0.08
split2 <- 0.08
# Creation lots d'entrainement/validation (92%) et evaluation (8%)
set.seed(007)
index1 <- createDataPartition(y = dataset$cap.diameter, times = 1, p = split1, list = FALSE)
lot_appr_opti <- dataset[-index1,]
lot_evaluation <- dataset[index1,]

# Creation lots d'entrainement (92%) et validation (8%)
set.seed(1337)
index2 <- createDataPartition(y = lot_appr_opti$cap.diameter, times = 1, p = split2, list = FALSE)
lot_apprentissage <- lot_appr_opti[-index2,]
lot_evaluation <- lot_appr_opti[index2,]

# Tracage des distributions monovariées du lot (entrainement+validation)
l <- nrow(structure_dataset)
for (n in 1:l){
   titre_graphe <- paste("Distribution de", dataset_noms[n], "des champignons")
   plot <- lot_appr_opti %>%
      ggplot(aes_string(x = dataset_noms[n])) +      # aes_string permet l'utilisation de chaîne au lieu de nom de variable
      ggtitle(titre_graphe) +
      ylab("") +
      xlab(dataset_noms[n]) +
      theme_bw()
   if(structure_dataset$Type[n] %in% c("integer", "numeric")) # Histogramme pour numériques, Barplot pour autres
   {plot <- plot + geom_histogram(fill = "gray45")}
   else
   {plot <- plot + geom_bar(fill = "gray45")}
   nom_graph <- paste0("study_distrib_", dataset_noms[n])   # Concaténer "plot_distrib" avec nom de colonne
   assign(nom_graph, plot)                 # Attribuer le graphique au nom plot_distrib_colname
}


#####################################################
#     ANALYSE DESCRIPTIVE DU LOT D'ENTRAINEMENT     #
#####################################################

# Trace toutes les distributions monovariées du lot d'entrainement (toxique vs comestible)
l <- nrow(structure_dataset)

for (n in 2:l){    # La colonne 1 (class) n'est pas tracée (attribut de fill/couleur !)
   titre_graphe <- paste("Distribution de", dataset_noms[n], "des champignons")
   plot <- lot_apprentissage %>%
      ggplot(aes_string(x = dataset_noms[n], fill = lot_apprentissage$class)) + # aes_string permet l'utilisation de chaîne au lieu de nom de variable
      ggtitle(titre_graphe) +
      ylab("Frequency") +
      xlab(dataset_noms[n]) +
      scale_y_log10() +
      theme_bw()
   if(structure_dataset$Type[n] %in% c("integer", "numeric"))   # Histogramme pour numériques, Barplot pour autres
   {plot <- plot + geom_histogram()}
   else
   {plot <- plot + geom_bar()}
   nom_graph <- paste0("train_distrib_",dataset_noms[n])    # Concaténer "plot_distrib" avec nom de colonne
   assign(nom_graph, plot)                 # Attribuer le graphique au nom plot_distrib_colname
}

# Graphes de corrélation pour une (petite) sélection de critères
paires_graphes <- ggpairs(
   lot_apprentissage,
   columns = c(2,15,17,10),
   lower = NULL,
   diag = list(continuous = wrap("densityDiag", alpha = .6), 
               discrete = wrap("barDiag")
   ),
   upper = list(continuous = wrap("points", alpha = .3, shape = 20), 
                combo = wrap("dot", alpha = .3, shape = 20),
                discrete = wrap("dot_no_facet", alpha = .3, shape = 20)
   ),
   ggplot2::aes(color = class)
)

# Nettoyage données et sauvegarde
rm(dataset, index1, index2, lot_appr_opti, lot_apprentissage, lot_evaluation, unique_length)
save.image(file = "EKR-Champis-EDA.RData")     # Sauvegarde données pour rapport
#load(file = "EKR-Champis-EDA.RData")     # Chargement données