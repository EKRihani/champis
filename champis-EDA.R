##########################
#     INITIALISATION     #
##########################

# Chargement des bibliothèques
library(tidyverse)    # Outils génériques
library(GGally)       # Outils supplémentaires pour les graphiques
library(twinning)       # Découpage des jeux de données


# Récupération, décompression, importation des données
fichier_data <- tempfile()
#URL <- "https://github.com/EKRihani/champis/raw/master/lot_champis.zip"      # URL de mon repo
# download.file(URL, fichier_data)
fichier_data <- "~/projects/champis/lot_champis.zip" # FICHIER LOCAL
fichier_data <- unzip(fichier_data, "lot_champis.csv")
dataset <- read.csv(fichier_data, header = TRUE, sep = ",", stringsAsFactors = TRUE)
dataset <- dataset %>% select(-c("Groupe", "Nom"))

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

EDA_n_champis <- nrow(dataset)
EDA_split_p <- sqrt(EDA_n_champis)
EDA_split_facteur <- round(sqrt(EDA_split_p)+1)
EDA_n_cols <- ncol(dataset)

# Creation lots d'entrainement/validation et evaluation
set.seed(007)
index1 <- twin(data = dataset, r = EDA_split_facteur)
lot_appr_opti <- dataset[-index1,]
lot_evaluation <- dataset[index1,]

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


###################################################################
#     ANALYSE DESCRIPTIVE DU LOT D'ENTRAINEMENT ET VALIDATION     #
###################################################################

# Trace toutes les distributions monovariées du lot d'entrainement (toxique vs comestible)
l <- nrow(structure_dataset)

for (n in 2:l){    # La colonne 1 (Type) n'est pas tracée (attribut de fill/couleur !)
   titre_graphe <- paste("Distribution de", dataset_noms[n], "des champignons")
   plot <- lot_appr_opti %>%
      ggplot(aes_string(x = dataset_noms[n], fill = lot_appr_opti$Type)) + # aes_string permet l'utilisation de chaîne au lieu de nom de variable
      ggtitle(titre_graphe) +
      ylab("Frequency") +
      xlab(dataset_noms[n]) +
#      scale_y_log10() +
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
      lot_appr_opti,
      columns = c(2,4,9,10), #12,18,19,20,23,24,25,26
#      lower = NULL,
      lower = list(continuous = wrap("density"), #densityDiag, dot, points, cor, smooth, smooth_loess, density
                   combo = wrap("facetdensity", #scale_x_continuous(trans = "log10")),  #dot, trends, box_no_facet, facethist, facetdensity, facetdensitystrip
                   discrete = wrap("count")  #barDiag, dot_no_facet, count, facetbar
      ),
      legend = 1,
      diag = list(continuous = wrap("densityDiag", alpha = .6), 
                  discrete = wrap("barDiag")
      ),
      upper = list(continuous = wrap("points", alpha = .3, shape = 20), 
                   combo = wrap("dot", alpha = .3, shape = 20),
                   discrete = wrap("dot_no_facet", alpha = .3, shape = 20)
      ),
      ggplot2::aes(color = Type)
   ) + 
   theme_bw() +
   scale_fill_viridis_d(begin = .1, end = .8, option = "D") + 
   scale_color_viridis_d(begin = .1, end = .8, option = "D") +
   theme(legend.position = "bottom") +
   labs(fill = "Type")

paires_graphes

# Nettoyage données inutiles et sauvegarde
save.image(file = "EKR-Champis-EDA2.RData")     # Sauvegarde données pour rapport
load(file = "EKR-Champis-EDA2.RData")     # Chargement données

rm(dataset, index1, lot_appr_opti, unique_length)
save.image(file = "EKR-Champis-EDA.RData")     # Sauvegarde données pour rapport


load(file = "EKR-Champis-EDA.RData")     # Chargement données
