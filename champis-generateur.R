##########################
#     INITIALISATION     #
##########################

# Chargement des bibliothèques et données initiales
library(tidyverse)    # Outils génériques
GEN_dataset <- read.csv("Champignons.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)

###### SIMPLIFICATION (à virer une fois appliqué?)

# Retirer valeurs numériques EXCEPTIONNELLES (PROVISOIRE)
GEN_dataset <- GEN_dataset %>% 
  mutate_all(funs(str_remove(., "[[:space:]]*\\[[[:digit:]]*\\]"))) %>% 
  mutate_all(funs(str_remove(., "[[:space:]]*\\[[[:digit:]]*.[[:digit:]]*\\]")))

# Retire les MATURES (PROVISOIRE)
GEN_dataset <- lapply(GEN_dataset, gsub, pattern='\\]', replacement='')
GEN_dataset <- lapply(GEN_dataset, gsub, pattern='\\[', replacement='')
GEN_dataset <- data.frame(GEN_dataset)

# Suppression valeurs minimales (PROVISOIRE)
GEN_dataset$Chapeau.Diametre <- GEN_dataset$Chapeau.Diametre %>% str_remove(., ".+-") %>% as.numeric(.)
GEN_dataset$Pied.Hauteur <- GEN_dataset$Pied.Hauteur %>% str_remove(., ".+-") %>% as.numeric(.)
GEN_dataset$Pied.Largeur <- GEN_dataset$Pied.Largeur %>% str_remove(., ".+-") %>% as.numeric(.)

GEN_dataset[is.na(GEN_dataset)] <- 0    # Remplace les dimensions NA par 0 (a priori inutile, mais bon...)

GEN_dataset$Type <- recode_factor(GEN_dataset$Type, 
                                   bon = "Conserver", comestible = "Conserver", "comestible cuit" = "Conserver",
                                   mediocre = "Rejeter", "non comestible" = "Rejeter", toxique = "Rejeter", Mortel = "Rejeter")

write_csv2(x = GEN_dataset, file = "donnees_champis.csv")

#############################
#     NETTOYAGE DONNEES     #
#############################

GEN_fichier_data <- "donnees_champis.csv"

GEN_dataset <- read.csv(GEN_fichier_data, header = TRUE, sep = ";", dec = "," , stringsAsFactors = FALSE)

GEN_structure <- sapply(X = GEN_dataset, FUN = class, simplify = TRUE)
GEN_numeriques <- which(GEN_structure %in% c("integer", "numeric"))

GEN_textes <- names(GEN_structure[-GEN_numeriques])
GEN_n_especes <- nrow(GEN_dataset)
GEN_dataset$N <- 1:GEN_n_especes
GEN_champ_liste <- paste0("champ", GEN_dataset$N)


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

GEN_dataset$Mois <- lapply(X = GEN_dataset$Mois, FUN = ConversionMois)


GEN_ratio_cr <- 10  # Ratio commun/rare

ConversionRares <- function(fcn_facteur){
  fcn_facteur <- as.character(fcn_facteur)
  if(str_detect(fcn_facteur, pattern ="\\([[:alpha:]]|[[:space:]]+\\)"))  # Détecte si présence effective de valeurs rares
    {
    valeurs <- strsplit(fcn_facteur, split = ",")[[1]]
    n_repet <- valeurs %>% str_match(string = ., pattern ="\\([[:alpha:]]|[[:space:]]+\\)") %>% is.na() %>% "*"(GEN_ratio_cr-1)+1
    rep(valeurs, n_repet) %>% str_remove(., ' \\(') %>% str_remove(., '\\)') %>% str_flatten(., collapse = ", ")
    }
  else
    {
      fcn_facteur
    }
}

for (n in 1:GEN_n_especes){
  ordre_rares <- paste0("GEN_dataset[",n,",]$", GEN_textes, " <- ConversionRares(GEN_dataset[",n,",]$", GEN_textes, ")")
  #print(ordre_rares)
  eval(parse(text = ordre_rares))
}


###############################
#     LISTES DES ESPECES      #
###############################
fonc_split <- function(x){str_split(x, ",\\s*", simplify = TRUE)}

# Séparation des espèces et des critères
for (n in 1:GEN_n_especes){
  assign(GEN_champ_liste[n], NULL)
  assign(GEN_champ_liste[n], as.list(GEN_dataset[n,]))
  assign(GEN_champ_liste[n], map(.x = eval(parse(text = GEN_champ_liste[n])), .f = fonc_split))
  ordre <- paste0(GEN_champ_liste[n],"[GEN_numeriques] <- map(.x = ", GEN_champ_liste[n], "[GEN_numeriques], .f = as.numeric)")
  eval(parse(text = ordre))
}


#############################
#     CREATION DES LOTS     #
#############################

GEN_lots_liste <- paste0("lot", GEN_dataset$N)

GEN_n_champis <- 1e2      # Nombre de champignons pour chaque espèce (300?)
GEN_f_crois <- 2          # Facteur de croissance
#GEN_tailles <- names(GEN_structure[GEN_numeriques[-c(1,2)]])    # Facteurs de taille
GEN_tailles <- names(GEN_structure[GEN_numeriques])    # Facteurs de taille


func_alea <- function(x){round(x * rnorm(n = GEN_n_champis, mean = 1, sd = .05), digits = 2)}

for (n in 1:GEN_n_especes){
  assign(GEN_lots_liste[n], NULL)
#  ordre_num0 <- paste0(GEN_lots_liste[n], "[1:2] <-", GEN_champ_liste[n], "[1:2]")
  ordre_texte <- paste0(GEN_lots_liste[n], "$", GEN_textes, 
                         "<- sample(x = ", GEN_champ_liste[n], "$", GEN_textes, ", size = GEN_n_champis, replace = TRUE)")
  ordre_fac <-paste0(GEN_lots_liste[n], "$FacteurTaille <- rbeta(n = GEN_n_champis, shape1 = 6*GEN_f_crois, shape2 =4, ncp = .5*GEN_f_crois)")
  ordre_num1 <- paste0(GEN_lots_liste[n], "[GEN_tailles] <- lapply(", 
                       GEN_champ_liste[n], "[GEN_tailles], '*', ",
                       GEN_lots_liste[n], "$FacteurTaille)")
  ordre_num2 <- paste0(GEN_lots_liste[n], "[GEN_tailles] <- map(.x = ",
                       GEN_lots_liste[n], "[GEN_tailles], .f = func_alea)")
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

GEN_lot_la_totale <- do.call(rbind, mget(paste0("lot",1:GEN_n_especes)))   # Fusion de tous les lots
GEN_lot_final <- GEN_lot_la_totale[sample(1:nrow(GEN_lot_la_totale)), ]

# Ajustement des concolores
GEN_Concol_Pied <- which(GEN_lot_final$Pied.Couleur %in% c("concolore", "subconcolore"))
GEN_lot_final$Pied.Couleur[GEN_Concol_Pied] <- GEN_lot_final$Chapeau.Couleur[GEN_Concol_Pied]
GEN_Concol_Chair <- which(GEN_lot_final$Chair.Couleur %in% c("concolore", "subconcolore"))
GEN_lot_final$Chair.Couleur[GEN_Concol_Chair] <- GEN_lot_final$Chapeau.Couleur[GEN_Concol_Chair]
GEN_Concol_Lames <- which(GEN_lot_final$Lames.Couleur %in% c("concolore", "subconcolore"))
GEN_lot_final$Lames.Couleur[GEN_Concol_Lames] <- GEN_lot_final$Chapeau.Couleur[GEN_Concol_Lames]

# Sauvegarde
write_csv(x = GEN_lot_final, file = "lot_champis.csv")
zip(zipfile = "lot_champis.zip", files = "lot_champis.csv")

# Gros nettoyage
for (n in 1:GEN_n_especes){
  ordre_rm1 <- paste0("rm(champ",n, ")")   # Suppression de la table champi de base
  ordre_rm2 <- paste0("rm(lot",n, ")")   # Suppression des lots champis de base
  eval(parse(text = ordre_rm1))
  eval(parse(text = ordre_rm2))
}


#Analyse niveaux pour nettoyage (A SUPPRIMER)
GEN_fichier_data <- "~/projects/champis/lot_champis.zip"
GEN_fichier_data <- unzip(GEN_fichier_data, "lot_champis.csv")
GEN_dataset2 <- read.csv(GEN_fichier_data, header = TRUE, sep = ",", stringsAsFactors = TRUE)
str(GEN_dataset2)
#summary(GEN_dataset2$Type)
#summary(GEN_dataset2$Groupe)
#summary(GEN_dataset2$Groupe2)
#summary(GEN_dataset2$Hymenophore1)
summary(GEN_dataset2$Hymenophore2)
#summary(GEN_dataset2$Chair.Type)
#summary(GEN_dataset2$Chair.Couleur)
summary(GEN_dataset2$Chapeau.Forme)
summary(GEN_dataset2$Chapeau.Surface)
#summary(GEN_dataset2$Chapeau.Couleur)
summary(GEN_dataset2$Chapeau.Marge)
#summary(GEN_dataset2$Spore.Couleur)
summary(GEN_dataset2$Lames.Attache)
summary(GEN_dataset2$Lames.Espace)
summary(GEN_dataset2$Lames.Couleur)
summary(GEN_dataset2$Pied.Forme)
summary(GEN_dataset2$Pied.Surface)
#summary(GEN_dataset2$Pied.Couleur)
summary(GEN_dataset2$Habitat)
summary(GEN_dataset2$Odeur)
#summary(GEN_dataset2$VG.Type)
summary(GEN_dataset2$VG.Type2)
#summary(GEN_dataset2$VP.Type)
summary(GEN_dataset2$VP.Type2)

rm(GEN_dataset2, GEN_lot_final, GEN_lot_la_totale)
save.image(file="EKR-Generateur.RData")
