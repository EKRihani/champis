##########################
#     INITIALISATION     #
##########################

# Chargement des bibliothèques
library(tidyverse)    # Outils génériques

fichier_data <- tempfile()
#URL <- "https://github.com/EKRihani/mushrooms/raw/master/MushroomDataset.zip"      # URL de mon repo
# download.file(URL, fichier_data)
fichier_data <- "~/projects/champis/MushroomDataset.zip" # FICHIER LOCAL

fichier_data <- unzip(fichier_data, "MushroomDataset/primary_data.csv")
data_champis <- read.csv(fichier_data, header = TRUE, sep = ";", stringsAsFactors = TRUE)

#############################
#     NETTOYAGE DONNEES     #
#############################
dataset <- data_champis

dataset <- dataset %>% 
  mutate_all(funs(str_remove(., '\\['))) %>% 
  mutate_all(funs(str_remove(., '\\]')))

# dataset$cap.diameter.min <- dataset$cap.diameter %>% str_extract(., "[:digit:]+,") %>% str_remove(., ',') %>% as.numeric(.)
# dataset$cap.diameter.max <- dataset$cap.diameter %>% str_extract(., ", [:digit:]+") %>% str_remove(., ',') %>% as.numeric(.)
# dataset$stem.height.min <- dataset$stem.height %>% str_extract(., "[:digit:]+,") %>% str_remove(., ',') %>% as.numeric(.)
# dataset$stem.height.max <- dataset$stem.height %>% str_extract(., ", [:digit:]+") %>% str_remove(., ',') %>% as.numeric(.)
# dataset$stem.width.min <- dataset$stem.width %>% str_extract(., "[:digit:]+,") %>% str_remove(., ',') %>% as.numeric(.)
# dataset$stem.width.max <- dataset$stem.width %>% str_extract(., ", [:digit:]+") %>% str_remove(., ',') %>% as.numeric(.)
# dataset$cap.diameter <- NULL      # Supprime les 3 colonnes devenues inutiles...
# dataset$stem.height <- NULL
# dataset$cap.width <- NULL

dataset$cap.diameter <- dataset$cap.diameter %>% str_extract(., ", [:digit:]+") %>% str_remove(., ',') %>% as.numeric(.)
dataset$stem.height <- dataset$stem.height %>% str_extract(., ", [:digit:]+") %>% str_remove(., ',') %>% as.numeric(.)
dataset$stem.width <- dataset$stem.width %>% str_extract(., ", [:digit:]+") %>% str_remove(., ',') %>% as.numeric(.)


dataset[is.na(dataset)] <- 0    #Remplace les dimensions NA par 0



structure <- sapply(X = dataset, FUN = class, simplify = TRUE)
numeriques <- which(structure %in% c("integer", "numeric"))
n_especes <- nrow(dataset)
dataset$N <- 1:n_especes
champ_liste <- paste0("champ", dataset$N)
###############################
#     LISTES DES ESPECES      #
###############################
fonc_split <- function(x){str_split(x, ",\\s*", simplify = TRUE)}

# Séparation des espèces et des critères
for (n in 1:n_especes){
  assign(champ_liste[n], NULL)
  assign(champ_liste[n], as.list(dataset[n,]))
  assign(champ_liste[n], map(.x = eval(parse(text = champ_liste[n])), .f = fonc_split))
  ordre <- paste0(champ_liste[n],"[numeriques] <- map(.x = ", champ_liste[n], "[numeriques], .f = as.numeric)")
  eval(parse(text = ordre))
}

#############################
#     CREATION DES LOTS     #
#############################

lots_liste <- paste0("lot", dataset$N)

n_champis <- 1e3      # Nombre de champignons pour chaque espèce
f_crois <- 2          # Facteur de croissance
#tailles <- names(structure[numeriques[-c(1,2)]])    # Facteurs de taille
tailles <- names(structure[numeriques])    # Facteurs de taille
#textes <- names(structure[-numeriques & 1:2])
textes <- names(structure[-numeriques])

func_alea <- function(x){x * rnorm(n = n_champis, mean = 1, sd = .05)}

for (n in 1:n_especes){
  assign(lots_liste[n], NULL)
#  ordre_num0 <- paste0(lots_liste[n], "[1:2] <-", champ_liste[n], "[1:2]")
  ordre_texte <- paste0(lots_liste[n], "$", textes, 
                         "<- sample(x = ", champ_liste[n], "$", textes, ", size = n_champis, replace = TRUE)")
  ordre_fac <-paste0(lots_liste[n], "$FacteurTaille <- rbeta(n = n_champis, shape1 = 6*f_crois, shape2 =4, ncp = .5*f_crois)")
  ordre_num1 <- paste0(lots_liste[n], "[tailles] <- lapply(", 
                       champ_liste[n], "[tailles], '*', ",
                       lots_liste[n], "$FacteurTaille)")
  ordre_num2 <- paste0(lots_liste[n], "[tailles] <- map(.x = ",
                       lots_liste[n], "[tailles], .f = func_alea)")
  ordre_df <- paste0("lot",n, " <- data.frame(lot", n, ")")        # Transformation liste en dataframe
  ordre_suppr <- paste0("lot",n, "$FacteurTaille <- NULL ")       # Suppression du facteur de taille
  ordre_rm <- paste0("rm(champ",n, ")")   # Suppression de la table champi de base (devenue inutile)
  #  eval(parse(text = ordre_num0))
  eval(parse(text = ordre_texte))
  eval(parse(text = ordre_fac))
  eval(parse(text = ordre_num1))
  eval(parse(text = ordre_num2))
  eval(parse(text = ordre_df))
  eval(parse(text = ordre_suppr))
  eval(parse(text = ordre_rm))
}

lot_la_totale <- do.call(rbind, mget(paste0("lot",1:n_especes)))   # Fusion de tous les lots

lot_final <- lot_la_totale[sample(1:nrow(lot_la_totale)), ]
write_csv(x = lot_final, file = "lot_test.csv")

# Gros nettoyage
for (n in 1:n_especes){
  ordre_rm1 <- paste0("rm(champ",n, ")")   # Suppression de la table champi de base
  ordre_rm2 <- paste0("rm(lot",n, ")")   # Suppression des lots champis de base
  eval(parse(text = ordre_rm1))
  eval(parse(text = ordre_rm2))
}
