##########################
#     INITIALISATION     #
##########################

# Chargement des bibliothèques et données initiales
library(tidyverse)    # Outils génériques

#URL <- "https://github.com/EKRihani/mushrooms/raw/master/MushroomDataset.zip"
#download.file(URL, fichier_data)
fichier_data <- "donnees_champis.csv"

data_champis <- read.csv(fichier_data, 
                         header = TRUE, 
                         sep = ";", 
                         dec = "," , 
                         stringsAsFactors = FALSE)


#################################
#     STRUCTURE DES DONNEES     #
#################################

structure <- sapply(X = data_champis, FUN = class, simplify = TRUE)
numeriques <- which(structure %in% c("integer", "numeric"))
textes <- names(structure[-numeriques])

n_especes <- nrow(data_champis)
data_champis$N <- 1:n_especes



#########################################
#     ADAPTATION DES DONNEES SOURCE     #
#########################################

# Conversion des mois, du format DEBUT-FIN vers liste complète
ConversionMois <- function(fcn_mois){
   date_extraction <- str_extract_all(fcn_mois, '[:digit:]+')[[1]]
   ifelse(
      test = as.numeric(date_extraction[1]) < as.numeric(date_extraction[2]),
      yes = liste_mois <- seq.int(from = date_extraction[1], to = date_extraction[2]),
      no = liste_mois <- c(seq.int(from = date_extraction[1], to = 12),
                           seq.int(from = 1, to = date_extraction[2]))
   )
   str_flatten(liste_mois, collapse = ", ")
}

data_champis$Mois <- lapply(X = data_champis$Mois, FUN = ConversionMois)


ratio_cr <- 10    # Ratio communs/rares


ConversionRares <- function(fcn_facteur){
   if(str_detect(fcn_facteur, pattern ="\\([[:alpha:]]|[[:space:]]+\\)"))
   {
      valeurs <- strsplit(fcn_facteur, split = ",")[[1]]
      
      n_repet <- valeurs %>%
         str_match(string = ., pattern ="\\([[:alpha:]]|[[:space:]]+\\)") %>% 
         is.na() %>% 
         "*"(ratio_cr-1)+1
      
      rep(valeurs, n_repet) %>%
         str_remove(., ' \\(') %>%
         str_remove(., '\\)') %>%
         str_flatten(., collapse = ", ")
   }
   else    # Si non rare, retourner la valeur d'entrée non modifiée
   {
      fcn_facteur
   }
}

for (n in 1:n_especes){
   ordre_rares <- paste0("data_champis[",n,",]$", textes,
                         " <- ConversionRares(data_champis[",n,",]$", textes, ")")
   eval(parse(text = ordre_rares))
}


###############################
#     LISTES DES ESPECES      #
###############################
fonc_split <- function(fcn_split){str_split(string = fcn_split, 
                                            pattern = ",\\s*",
                                            simplify = TRUE)}

# Séparation des espèces et des critères

champ_liste <- paste0("champ", data_champis$N)


for (n in 1:n_especes){
   assign(champ_liste[n], NULL)
   assign(champ_liste[n], as.list(data_champis[n,]))
   assign(champ_liste[n], map(.x = eval(parse(text = champ_liste[n])), 
                              .f = fonc_split))
   
   ordre <- paste0(champ_liste[n],"[numeriques] <-", 
                   "map(.x = ", champ_liste[n], "[numeriques],", 
                   ".f = as.numeric)")
   eval(parse(text = ordre))
}


#############################
#     CREATION DES LOTS     #
#############################

lots_liste <- paste0("lot", data_champis$N)

n_champis <- 3e2      # Nombre de champignons pour chaque espèce
f_crois <- 2          # Facteur de croissance
#tailles <- names(structure[numeriques[-c(1,2)]])    # Facteurs de taille
tailles <- names(structure[numeriques])    # Facteurs de taille


func_alea <- function(x){round(x * rnorm(n = n_champis, 
                                         mean = 1, 
                                         sd = .05),
                               digits = 2)}

for (n in 1:n_especes){
   assign(lots_liste[n], NULL)
   
   ordre_texte <- paste0(lots_liste[n], "$", textes, 
                         "<- sample(x = ", champ_liste[n], "$", textes,
                         ", size = n_champis, replace = TRUE)")
   ordre_fac <-paste0(lots_liste[n], "$FacteurTaille <- rbeta(n = n_champis, 
                                                             shape1 = 6*f_crois, 
                                                             shape2 =4, 
                                                             ncp = .5*f_crois)")
   ordre_num1 <- paste0(lots_liste[n], "[tailles] <- lapply(", 
                        champ_liste[n], "[tailles], '*', ",
                        lots_liste[n], "$FacteurTaille)")
   ordre_num2 <- paste0(lots_liste[n], "[tailles] <- map(.x = ",
                        lots_liste[n], "[tailles], .f = func_alea)")
   ordre_df <- paste0("lot",n, " <- data.frame(lot", n, ")")
   ordre_suppr <- paste0("lot",n, "$FacteurTaille <- NULL ")
   ordre_rm <- paste0("rm(champ",n, ")")
   
   eval(parse(text = ordre_texte))
   eval(parse(text = ordre_fac))
   eval(parse(text = ordre_num1))
   eval(parse(text = ordre_num2))
   eval(parse(text = ordre_df))
   eval(parse(text = ordre_suppr))
   eval(parse(text = ordre_rm))
}

lot_la_totale <- do.call(rbind, mget(paste0("lot",1:n_especes)))   # Fusion de tous les lots
lot_final <- lot_la_totale[sample(1:nrow(lot_la_totale)),]

# Ajustement concolores
Concol_Pied <- which(lot_final$Pied.Couleur %in% c("concolore", "subconcolore"))
lot_final$Pied.Couleur[Concol_Pied] <- lot_final$Chapeau.Couleur[Concol_Pied]
Concol_Chair <- which(lot_final$Chair.Couleur %in% c("concolore", "subconcolore"))
lot_final$Chair.Couleur[Concol_Chair] <- lot_final$Chapeau.Couleur[Concol_Chair]
Concol_Lames <- which(lot_final$Lames.Couleur %in% c("concolore", "subconcolore"))
lot_final$Lames.Couleur[Concol_Lames] <- lot_final$Chapeau.Couleur[Concol_Lames]



write_csv(x = lot_final, file = "lot_champis.csv")
zip(zipfile = "lot_champis.zip", files = "lot_champis.csv")

# Gros nettoyage
for (n in 1:n_especes){
  ordre_rm1 <- paste0("rm(champ",n, ")")   # Suppression de la table champi de base
  ordre_rm2 <- paste0("rm(lot",n, ")")   # Suppression des lots champis de base
  eval(parse(text = ordre_rm1))
  eval(parse(text = ordre_rm2))
}

