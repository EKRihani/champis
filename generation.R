##########################
#     INITIALISATION     #
##########################

# Chargement des bibliothèques
library(tidyverse)    # Outils génériques
#library(rlang)      # Outils d'évaluation d'expression (parse/deparse)

# Récupération, décompression, importation des données
data_fichier <- tempfile()
#URL <- "https://github.com/EKRihani/champis/raw/master/Champignons.csv"      # Fichier distant
# download.file(URL, data_fichier)

data_fichier <- "~/projects/champis/fake.csv" # FICHIER LOCAL SIMULE
data_champis <- read.csv(data_fichier,
                         header = TRUE,
                         sep = ";",
                         dec = ",",
                         na.strings = TRUE,
                         stringsAsFactors = FALSE,
                         encoding = "UTF-8")


###############################
#     PREPARATION DONNEES     #
###############################

# Ajout de valeurs NA si NA ou vide
data_champis <- na_if(data_champis, "NA")
data_champis <- na_if(data_champis, "")

# Récupération infos de base
structure <- sapply(X = data_champis,
                    FUN = class,
                    simplify = TRUE)
numeriques <- which(structure %in% c("integer", "numeric"))
n_especes <- nrow(data_champis)
champ_liste <- paste0("champ", data_champis$N)

# Fonction : séparation des chaînes (séparateur = virgule)
fonc_split <- function(x){str_split(x, ",\\s*", simplify = TRUE)}

###############################
#     LISTES DES ESPECES      #
###############################

# Séparation des espèces et des critères
for (n in 1:n_especes){
  assign(champ_liste[n], NULL)
  assign(champ_liste[n], as.list(data_champis[n,]))
  assign(champ_liste[n], map(.x = eval(parse(text = champ_liste[n])), .f = fonc_split))
  ordre <- paste0(champ_liste[n],"[numeriques] <- map(.x = ", champ_liste[n], "[numeriques], .f = as.numeric)")
  eval(parse(text = ordre))
}

#############################
#     CREATION DES LOTS     #
#############################

lots_liste <- paste0("lot", data_champis$N)

n_champis <- 1e3      # Nombre de champignons pour chaque espèce
f_crois <- 2          # Facteur de croissance
tailles <- names(structure[numeriques[-c(1,2)]])    # Facteurs de taille
textes <- names(structure[-numeriques & 1:2])

func_alea <- function(x){x * rnorm(n = n_champis, mean = 1, sd = .1)}

for (n in 1:n_especes){
  assign(lots_liste[n], NULL)
  ordre_num0 <- paste0(lots_liste[n], "[1:2] <-", champ_liste[n], "[1:2]")
  ordre_texte <- paste0(lots_liste[n], "$", textes, 
                         "<- sample(x = ", champ_liste[n], "$", textes, ", size = n_champis, replace = TRUE)")
  ordre_fac <-paste0(lots_liste[n], "$FacteurTaille <- rbeta(n = n_champis, shape1 = 6*f_crois, shape2 =4, ncp = .5*f_crois)")
  ordre_num1 <- paste0(lots_liste[n], "[tailles] <- lapply(", 
                       champ_liste[n], "[tailles], '*', ",
                       lots_liste[n], "$FacteurTaille)")
  ordre_num2 <- paste0(lots_liste[n], "[tailles] <- map(.x = ",
                       lots_liste[n], "[tailles], .f = func_alea)")
  eval(parse(text = ordre_num0))
  eval(parse(text = ordre_texte))
  eval(parse(text = ordre_fac))
  eval(parse(text = ordre_num1))
  eval(parse(text = ordre_num2))
}

# sapply(lot242, function(x) x[2]) # LECTURE DONNEES CHAMPI No2

plot(x= lot242$Chapeau.Diametre, y = lot242$Pied.Hauteur, col = rgb(0,0,0, alpha = 0.05), pch = 16)

ggplot(data = as.data.frame(lot242), aes(x = Chapeau.Diametre, y = Pied.Hauteur)) +
  # geom_density2d_filled(bins = 100) +
  # geom_density2d(bins=15, color = "white", alpha = .2) +
  # scale_fill_viridis_d(option = "H", direction = 1) + #B,F,G (H)
  # theme(legend.position="none") +
   stat_density_2d(geom = "polygon", contour = TRUE, contour_var = "count", #density, count, ndensity
                   aes(fill = after_stat(level)),
                   bins = 50, n = 20) +
   scale_fill_viridis_c(option = "F", direction = -1) + #B,F,G (H)
  theme_classic() +
  geom_vline(xintercept = champ242$Chapeau.Diametre, linetype = "dotted", color = "red") +
  geom_hline(yintercept = champ242$Pied.Hauteur, linetype = "dotted", color = "red")

