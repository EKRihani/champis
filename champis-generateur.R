##########################
#     INITIALISATION     #
##########################

# Chargement des bibliothèques et données initiales
library(tidyverse)    # Outils génériques
data_champis <- read.csv("ChampiTest.csv", header = TRUE, sep = ";", stringsAsFactors = TRUE)

###### SIMPLIFICATION (à virer une fois appliqué)
dataset <- data_champis

# Critere de comestibilité binaire (conserver/rejeter)
dataset$Type <- recode_factor(dataset$Type, 
                                  Bon = "A conserver", Comestible = "A conserver", "Comestible cuit" = "A conserver",
                                  Mediocre = "A rejeter", "Non Comestible" = "A rejeter", Toxique = "A rejeter", Mortel = "A rejeter")

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

######################
#   FACTEURS RARES   #
######################
ratio_cr <- 10

#data_TEST_RARE <- data_champis[35:45,c(1,2,3,5,9,10,12,26)]

#TEST <- data_champis[37,26] %>% as.character(.)

TEST <- "comA, comB, (RAREA), (RAREB)"
TEST <- strsplit(TEST, split = ",")[[1]]

n_repet <- TEST %>% str_match(string = ., pattern ="\\([[:alpha:]]+\\)") %>% 
  is.na() %>%
  "*"(ratio_cr-1)+1

TEST_RARES <- rep(TEST, n_repet) %>% 
  str_remove(., '\\(') %>% 
  str_remove(., '\\)') %>% 
  str_flatten(., collapse = ", ")

TEST_RARES



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
