library(tidyverse)

DATA_URL_Schlimmer <- "https://archive.ics.uci.edu/static/public/73/mushroom.zip"
DATA_Fichier_Schlimmer <- tempfile()
download.file(DATA_URL_Schlimmer, DATA_Fichier_Schlimmer)
DATA_Fichier_Schlimmer <- unzip(DATA_Fichier_Schlimmer, "agaricus-lepiota.data")
DATA_Lot_Schlimmer <- read.csv(DATA_Fichier_Schlimmer, header = FALSE, sep = ",")


DATA_URL_Wagner <- "https://archive.ics.uci.edu/static/public/848/secondary+mushroom+dataset.zip"
DATA_Fichier_Wagner <- tempfile()
download.file(DATA_URL_Wagner, DATA_Fichier_Wagner)
DATA_Fichier_Wagner <- unzip(DATA_Fichier_Wagner, "MushroomDataset.zip")
DATA_Fichier_Wagner <- unzip(DATA_Fichier_Wagner, "MushroomDataset/secondary_data.csv")
DATA_Lot_Wagner <- read.csv(DATA_Fichier_Wagner, header = TRUE, sep = ";")


names(DATA_Lot_Schlimmer) <- c("class", "cap-shape", "cap-surface", "cap-color", "bruises", "odor", 
                               "gill-attachment", "gill-spacing", "gill-size", "gill-color",
                               "stalk-shape", "stalk-root", 
                               "stalk-surface-above-ring", "stalk-surface-below-ring", 
                               "stalk-color-above-ring", "stalk-color-below-ring", 
                               "veil-type", "veil-color", "ring-number", "ring-type", 
                               "spore-print-color", "population", "habitat")

DATA_n_Schlimmer <- nrow(DATA_Lot_Schlimmer)
DATA_n_Wagner <- nrow(DATA_Lot_Wagner)


DATA_structure_Schlimmer <- sapply(X = DATA_Lot_Schlimmer, FUN = class, simplify = TRUE)
DATA_structure_Wagner <- sapply(X = DATA_Lot_Wagner, FUN = class, simplify = TRUE)

DATA_quanti_Schlimmer <- sum(DATA_structure_Schlimmer == "numeric")
DATA_quali_Schlimmer <- sum(DATA_structure_Schlimmer == "character")

DATA_quanti_Wagner <- sum(DATA_structure_Wagner == "numeric")
DATA_quali_Wagner <- sum(DATA_structure_Wagner == "character")


save.image(file="EKR-DataSets.RData")