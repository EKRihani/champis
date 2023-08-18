##########################
#     INITIALISATION     #
##########################

# Chargement des bibliothèques et données initiales
library(tidyverse)    # Outils génériques
dataset <- read.csv("ChampiTest.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)

###### SIMPLIFICATION (à virer une fois appliqué?)

# Retirer valeurs numériques EXCEPTIONNELLES (PROVISOIRE)
dataset <- dataset %>% 
  mutate_all(funs(str_remove(., "[[:space:]]*\\[[[:digit:]]*\\]"))) %>% 
  mutate_all(funs(str_remove(., "[[:space:]]*\\[[[:digit:]]*.[[:digit:]]*\\]")))

# Retire les MATURES (PROVISOIRE)
dataset <- lapply(dataset, gsub, pattern='\\]', replacement='')
dataset <- lapply(dataset, gsub, pattern='\\[', replacement='')
dataset <- data.frame(dataset)

# Suppression valeurs minimales (PROVISOIRE)
dataset$Chapeau.Diametre <- dataset$Chapeau.Diametre %>% str_remove(., ".+-") %>% as.numeric(.)
dataset$Pied.Hauteur <- dataset$Pied.Hauteur %>% str_remove(., ".+-") %>% as.numeric(.)
dataset$Pied.Largeur <- dataset$Pied.Largeur %>% str_remove(., ".+-") %>% as.numeric(.)

dataset[is.na(dataset)] <- 0    # Remplace les dimensions NA par 0 (a priori inutile, mais bon...)

dataset$Type <- recode_factor(dataset$Type, 
                                   bon = "Conserver", comestible = "Conserver", "comestible cuit" = "Conserver",
                                   mediocre = "Rejeter", "non comestible" = "Rejeter", toxique = "Rejeter", Mortel = "Rejeter")

write_csv2(x = dataset, file = "donnees_champis.csv")

#############################
#     NETTOYAGE DONNEES     #
#############################

fichier_data <- "donnees_champis.csv"

dataset <- read.csv(fichier_data, header = TRUE, sep = ";", dec = "," , stringsAsFactors = FALSE)

structure <- sapply(X = dataset, FUN = class, simplify = TRUE)
numeriques <- which(structure %in% c("integer", "numeric"))

textes <- names(structure[-numeriques])
n_especes <- nrow(dataset)
dataset$N <- 1:n_especes
champ_liste <- paste0("champ", dataset$N)


#########################################
#     ADAPTATION DES DONNEES SOURCE     #
#########################################

# Conversion des mois, du format DEBUT-FIN vers liste complète
ConversionMois <- function(fcn_mois){
  date_extraction <- str_extract_all(fcn_mois, '[:digit:]+')[[1]]
  ifelse(
    test = as.numeric(date_extraction[1]) < as.numeric(date_extraction[2]),  # Teste l'absence de passage décembre/janiver
    yes = liste_mois <- seq.int(from = date_extraction[1], to = date_extraction[2]),
    no = liste_mois <- c(seq.int(from = date_extraction[1], to = 12), seq.int(from = 1, to = date_extraction[2]))
  )
  str_flatten(liste_mois, collapse = ", ")
}

dataset$Mois <- lapply(X = dataset$Mois, FUN = ConversionMois)


ratio_cr <- 10  # Ratio commun/rare

ConversionRares <- function(fcn_facteur){
  fcn_facteur <- as.character(fcn_facteur)
  if(str_detect(fcn_facteur, pattern ="\\([[:alpha:]]|[[:space:]]+\\)"))  # Détecte si présence effective de valeurs rares
    {
    valeurs <- strsplit(fcn_facteur, split = ",")[[1]]
    n_repet <- valeurs %>% str_match(string = ., pattern ="\\([[:alpha:]]|[[:space:]]+\\)") %>% is.na() %>% "*"(ratio_cr-1)+1
    rep(valeurs, n_repet) %>% str_remove(., ' \\(') %>% str_remove(., '\\)') %>% str_flatten(., collapse = ", ")
    }
  else
    {
      fcn_facteur
    }
}

for (n in 1:n_especes){
  ordre_rares <- paste0("dataset[",n,",]$", textes, " <- ConversionRares(dataset[",n,",]$", textes, ")")
  #print(ordre_rares)
  eval(parse(text = ordre_rares))
}


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

n_champis <- 3e2      # Nombre de champignons pour chaque espèce
f_crois <- 2          # Facteur de croissance
#tailles <- names(structure[numeriques[-c(1,2)]])    # Facteurs de taille
tailles <- names(structure[numeriques])    # Facteurs de taille


func_alea <- function(x){round(x * rnorm(n = n_champis, mean = 1, sd = .05), digits = 2)}

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

# Ajustement des concolores
Concol_Pied <- which(lot_final$Pied.Couleur %in% c("concolore", "subconcolore"))
lot_final$Pied.Couleur[Concol_Pied] <- lot_final$Chapeau.Couleur[Concol_Pied]
Concol_Chair <- which(lot_final$Chair.Couleur %in% c("concolore", "subconcolore"))
lot_final$Chair.Couleur[Concol_Chair] <- lot_final$Chapeau.Couleur[Concol_Chair]
Concol_Lames <- which(lot_final$Lames.Couleur %in% c("concolore", "subconcolore"))
lot_final$Lames.Couleur[Concol_Lames] <- lot_final$Chapeau.Couleur[Concol_Lames]

# Sauvegarde
write_csv(x = lot_final, file = "lot_champis.csv")
zip(zipfile = "lot_champis.zip", files = "lot_champis.csv")

# Gros nettoyage
for (n in 1:n_especes){
  ordre_rm1 <- paste0("rm(champ",n, ")")   # Suppression de la table champi de base
  ordre_rm2 <- paste0("rm(lot",n, ")")   # Suppression des lots champis de base
  eval(parse(text = ordre_rm1))
  eval(parse(text = ordre_rm2))
}


#Analyse niveaux pour nettoyage (A SUPPRIMER)
fichier_data <- "~/projects/champis/lot_champis.zip"
fichier_data <- unzip(fichier_data, "lot_champis.csv")
dataset2 <- read.csv(fichier_data, header = TRUE, sep = ",", stringsAsFactors = TRUE)
str(dataset2)
summary(dataset2$Type)
summary(dataset2$Hyménophore1)
summary(dataset2$Hyménophore2)
summary(dataset2$Chair.Type)
summary(dataset2$Chair.Couleur)
summary(dataset2$Chapeau.Forme)
summary(dataset2$Chapeau.Surface)
summary(dataset2$Chapeau.Couleur)
summary(dataset2$Chapeau.Marge)
summary(dataset2$Spore.Couleur)
summary(dataset2$Lames.Attache)
summary(dataset2$Lames.Espace)
summary(dataset2$Lames.Couleur)
summary(dataset2$Pied.Forme)
summary(dataset2$Pied.Surface)
summary(dataset2$Pied.Couleur)
summary(dataset2$Habitat)
summary(dataset2$Odeur)
summary(dataset2$VG.Type)
summary(dataset2$VG.Type2)
summary(dataset2$VP.Type)
summary(dataset2$VP.Type2)

dataset[dataset$Pied.Surface=="chin",1]


test <- dataset2[1:4,]


