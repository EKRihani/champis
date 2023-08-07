##########################
#     INITIALISATION     #
##########################

# Chargement des bibliothèques et données initiales
library(tidyverse)    # Outils génériques
data_champis <- read.csv("ChampiTest.csv", header = TRUE, sep = ";", stringsAsFactors = TRUE)

dataset <- data_champis

###### SIMPLIFICATION (à virer une fois appliqué?)

# Retirer valeurs numériques EXCEPTIONNELLES (PROVISOIRE)
dataset <- dataset %>% 
  mutate_all(funs(str_remove(., "[[:space:]]*\\[[[:digit:]]*\\]"))) %>% 
  mutate_all(funs(str_remove(., "[[:space:]]*\\[[[:digit:]]*.[[:digit:]]*\\]")))

# Retire les MATURES (PROVISOIRE)
dataset <- dataset %>% 
  mutate_all(funs(str_remove(., '\\['))) %>% 
  mutate_all(funs(str_remove(., '\\]')))

# Suppression des valeurs RARES (PROVISOIRE)
# dataset <- dataset %>% 
#   mutate_all(funs(str_remove(., '\\('))) %>% 
#   mutate_all(funs(str_remove(., '\\)')))

# Suppression valeurs minimales (PROVISOIRE)
dataset$Chapeau.Diametre <- dataset$Chapeau.Diametre %>% str_remove(., ".+-") %>% as.numeric(.)
dataset$Pied.Hauteur <- dataset$Pied.Hauteur %>% str_remove(., ".+-") %>% as.numeric(.)
dataset$Pied.Largeur <- dataset$Pied.Largeur %>% str_remove(., ".+-") %>% as.numeric(.)


#############################
#     NETTOYAGE DONNEES     #
#############################

dataset[is.na(dataset)] <- 0    # Remplace les dimensions NA par 0 (a priori inutile, mais bon...)

structure <- sapply(X = dataset, FUN = class, simplify = TRUE)
numeriques <- which(structure %in% c("integer", "numeric"))
n_especes <- nrow(dataset)
dataset$N <- 1:n_especes
champ_liste <- paste0("champ", dataset$N)


#########################################
#     ADAPTATION DES DONNEES SOURCE     #
#########################################

# Critère de comestibilité binaire (conserver/rejeter)
dataset$Type <- recode_factor(dataset$Type, 
                              Bon = "A conserver", Comestible = "A conserver", "Comestible cuit" = "A conserver",
                              Mediocre = "A rejeter", "Non Comestible" = "A rejeter", Toxique = "A rejeter", Mortel = "A rejeter")

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

# Facteurs rares de (facteur_rare) à liste complète avec ratio adapté
TEST1 <- data_champis[35:45,c(1,2,3,5,12,26)]
TEST2 <- data_champis[37,26]
TEST3 <- data_champis[40,26]

ratio_cr <- 10  # Ratio commun/rare

ConversionRares <- function(fcn_facteur){
  fcn_facteur <- as.character(fcn_facteur)
  valeurs <- strsplit(fcn_facteur, split = ",")[[1]]
  n_repet <- valeurs %>% str_match(string = ., pattern ="\\([[:alpha:]]+\\)") %>% is.na() %>% "*"(ratio_cr-1)+1
  nv_valeur <- rep(valeurs, n_repet) %>% str_remove(., ' \\(') %>% str_remove(., '\\)') %>% str_flatten(., collapse = ", ")
  ifelse(
  test = str_detect(fcn_facteur, pattern ="\\([[:alpha:]]+\\)"),  # Détecte si présence effective de rares
  yes = nv_valeur,
  no = fcn_facteur
  )
  }
ConversionRares(TEST2)


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
write_csv(x = lot_final, file = "lot_test.csv")

# Gros nettoyage
for (n in 1:n_especes){
  ordre_rm1 <- paste0("rm(champ",n, ")")   # Suppression de la table champi de base
  ordre_rm2 <- paste0("rm(lot",n, ")")   # Suppression des lots champis de base
  eval(parse(text = ordre_rm1))
  eval(parse(text = ordre_rm2))
}
