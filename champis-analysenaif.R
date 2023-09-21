##########################
#     INITIALISATION     #
##########################

# Chargement des bibliothèques
library(tidyverse)    # Outils génériques
library(twinning)       # Découpage équilibré des jeux de données (plus efficient que split!)
library(caret)    # Pour ConfusionMatrix

# Récupération, décompression, importation des données
fichier_data <- tempfile()
#URL <- "https://github.com/EKRihani/champis/raw/master/lot_champis.zip"      # URL de mon repo
# download.file(URL, fichier_data)

fichier_data <- "~/projects/champis/lot_champis.zip" # FICHIER LOCAL
fichier_data <- unzip(fichier_data, "lot_champis.csv")
dataset <- read.csv(fichier_data, header = TRUE, sep = ",", stringsAsFactors = TRUE)
dataset$Type <- relevel(dataset$Type, ref = "Rejeter")
dataset <- dataset %>% select(!Nom)

####################################################################
#     CREATION DES LOTS D'ENTRAINEMENT, VALIDATION, EVALUATION     #
####################################################################

NAIF_n_champis <- nrow(dataset)
NAIF_split_p <- sqrt(NAIF_n_champis)
NAIF_split_facteur <- round(sqrt(NAIF_split_p)+1)

set.seed(007)
index1 <- twin(data = dataset, r = NAIF_split_facteur)
NAIF_lot_appr_opti <- dataset[-index1,]
NAIF_lot_evaluation <- dataset[index1,]

set.seed(007)
index1 <- twin(data = NAIF_lot_appr_opti, r = NAIF_split_facteur)
NAIF_lot_appr <- NAIF_lot_appr_opti[-index1,]
NAIF_lot_opti <- NAIF_lot_appr_opti[index1,]

# Définition index de Youden
NAIF_w <- 10
NAIF_RatioSens <- 2*NAIF_w/(NAIF_w+1)
NAIF_RatioSpec <- 2*(1-NAIF_w/(NAIF_w+1))

############################################################
#     MODELE DE CLASSIFICATION SIMPLE : LISTE CRITERES     #
############################################################

# Créer listes de critères (facteur + type + valeurs) pour classification simple
facteurs_liste <- NAIF_lot_appr %>% 
   select(where(is.factor) | where(is.logical)) %>% 
   gather(facteur, niveau) %>% 
   unique() %>% 
   select(facteur, niveau) %>% 
   filter(facteur != "Type")     # Récupère tous les niveaux de facteurs + logique

facteurs_type <- NAIF_lot_appr %>% 
   summary.default %>% 
   as.data.frame %>% 
   group_by(Var1) %>% 
   spread(Var2, Freq) %>% 
   as.data.frame        # Extraire structure du lot d'entraînement

facteurs_type$Class <- if_else(facteurs_type$Class == "-none-", facteurs_type$Mode, facteurs_type$Class)     # Créer une colonne Type cohérente

facteurs_type <- facteurs_type %>% 
   select(-Mode, -Length)              # Nettoie facteurs_type

colnames(facteurs_type) <- c("facteur", "type")

liste_numeriques <- facteurs_type %>% 
   filter(type == "numeric") %>% select(facteur) %>%         # Sélectionne valeurs numériques, extrait le nom du facteur
   slice(rep(1:n(), each = 2)) %>%                          # Duplique chaque facteur numérique
   mutate(niveau = rep(c("min", "max"), times = n()/2))      # Crée les valeurs min et max pour chaque facteur numérique

facteurs_liste <- rbind(facteurs_liste, liste_numeriques)           # Fusionne les listes facteur/logique et numérique
facteurs_liste <- left_join(facteurs_liste, facteurs_type)       # Ajoute le type adéquat pour chaque facteur

# Construire liste de facteurs pour classification monovariable
facteurs_liste1 <- facteurs_liste
facteurs_liste1$tous_comestibles <- FALSE                  # Par défaut : non-comestible

# Liste de facteurs pour analyse bivariée
m <- nrow(facteurs_liste)
index_list2 <- t(combn(m, 2))                            # Créer toutes les combinaisons de 2 nombres de 1 à m (indices)
moitie1_list2 <- facteurs_liste[index_list2[,1],]            # Extraire 1er attribut, d'après le 1er index
colnames(moitie1_list2) <- c("facteur1", "niveau1", "type1")
moitie2_list2 <- facteurs_liste[index_list2[,2],]            # Extraire 2e attribut, d'après le 2e index
colnames(moitie2_list2) <- c("facteur2", "niveau2", "type2")
facteurs_liste2 <- cbind(moitie1_list2, moitie2_list2)
facteurs_liste2 <- facteurs_liste2 %>% filter(facteur1 != facteur2)     # Retire doublons
facteurs_liste2$tous_comestibles <- FALSE                        # Par défaut : non-comestible

###########################################################
#     CONSTRUCTION DU MODELE DE CLASSIFICATION SIMPLE     #
###########################################################

# Définition fonction : sélection min/max et association à +/- pour marges
minmaxing <- function(fcn_niveau_entree, fcn_marges){
   margin <- as.numeric(as.character(recode_factor(fcn_niveau_entree, min = -fcn_marges, max = fcn_marges))) # Attribue marge +/- margin selon max ou min
   c(match.fun(fcn_niveau_entree), margin)               # Crée vecteur avec min/max (fonction) en [[1]] et marge +/- valeur (numérique) en [[2]]
}

# Définition fonction : arrondi + calcul de marge + ajouter ">" ou "<" devant la valeur de marge.
infsup <- function(fcn_liste_nom, fcn_valeur_entree, fcn_min_max, fcn_niveau_valeur, fcn_n_iter){
   niveau <- paste0(fcn_liste_nom, "$niveau", fcn_niveau_valeur, "[", fcn_n_iter, "]")  # Sélectionne facteurs_liste, facteurs_liste1 ou facteurs_liste2 .$niveaus
   fcn_valeur_entree %>% 
      round(., digits = 1) %>% 
      + fcn_min_max[[2]] %>% 
      max(., 0) %>% # Arrondie, ajoute ou retire la valeur de la marge, force à zéro si négatif
      paste0(eval(parse(text = niveau)), .) %>% # Colle "min" or "max" devant la valeur arrondie
      str_replace_all(., "min", "< ") %>%       # Remplace "min" par "< "
      str_replace_all(., "max", "> ")           # Remplace "max" par "> "
}

# Définition fonction : trouver tous critères "100% comestibles"
recherche_simple <- function(fcn_lot_entrainement, fcn_liste_criteres, fcn_marge){
   l <- nrow(fcn_liste_criteres)
   crit_list_name <- deparse(substitute(fcn_liste_criteres))
   for (n in 1:l){
      if(fcn_liste_criteres$type[n] %in% c("logical", "factor", "character"))
      {
         fcn_liste_criteres$tous_comestibles[n] <- fcn_lot_entrainement %>% 
            filter(Type == "Rejeter", get(fcn_liste_criteres$facteur[n]) == fcn_liste_criteres$niveau[n]) %>% 
            nrow() == 0  # Trouve si (pour cette combinaison facteur/niveau) il n'y a aucun non-comestible
      }
      else          # Si type numérique (ou integer)
      {
         minmax <- minmaxing(fcn_liste_criteres$niveau[n], fcn_marge)     # Pose min/max et valeurs arrondies pour ".$niveau"
         valeur_actuelle <- fcn_lot_entrainement %>% 
            filter(Type == "Rejeter") %>% 
            select(fcn_liste_criteres$facteur[n]) %>% 
            minmax[[1]](.)
         extremum <- fcn_lot_entrainement %>%  
            select(fcn_liste_criteres$facteur[n]) %>% 
            minmax[[1]](.)
         fcn_liste_criteres$tous_comestibles[n] <- valeur_actuelle != extremum
         fcn_liste_criteres$niveau[n] <- infsup(crit_list_name, valeur_actuelle, minmax, "", n)
      }
   }
   fcn_liste_criteres
}

# Définition fonction : trouver toutes combinaison de 2 critères "100% comestibles"
recherche_double <- function(fcn_lot_entrainement, fcn_liste_criteres, fcn_marge){
   l <- nrow(fcn_liste_criteres)
   crit_list_name <- deparse(substitute(fcn_liste_criteres))
   for (n in 1:l){
      if(fcn_liste_criteres$type1[n] %in% c("logical", "factor", "character") & fcn_liste_criteres$type2[n] %in% c("logical", "factor", "character")) # facteur 1 & facteur 2 textuels
      {
         comptage <- fcn_lot_entrainement %>%
            filter(get(fcn_liste_criteres$facteur1[n]) == fcn_liste_criteres$niveau1[n], get(fcn_liste_criteres$facteur2[n]) == fcn_liste_criteres$niveau2[n]) %>%
            nrow
         comptage_poison <- fcn_lot_entrainement %>%
            filter(Type == "Rejeter", get(fcn_liste_criteres$facteur1[n]) == fcn_liste_criteres$niveau1[n],
                   get(fcn_liste_criteres$facteur2[n]) == fcn_liste_criteres$niveau2[n]) %>%
            nrow
         fcn_liste_criteres$tous_comestibles[n] <- comptage != 0 & comptage_poison == 0 # Trouve si (pour cette combinaison facteur/niveau) il y a des champis ET aucun vénéneux
      }
      else          # si facteur1 texte & facteur2 quantitatif
      {if(fcn_liste_criteres$type1[n] %in% c("logical", "factor", "character") & fcn_liste_criteres$type2[n] %in% c("numeric", "integer"))
      {
         minmax <- minmaxing(fcn_liste_criteres$niveau2[n], fcn_marge)
         valeur_actuelle <- fcn_lot_entrainement %>%
            filter(Type == "Rejeter", get(fcn_liste_criteres$facteur1[n]) == fcn_liste_criteres$niveau1[n]) %>%
            select(fcn_liste_criteres$facteur2[n]) %>%
            minmax[[1]](.)
         extremum <- fcn_lot_entrainement %>% filter(get(fcn_liste_criteres$facteur1[n]) == fcn_liste_criteres$niveau1[n]) %>%
            select(fcn_liste_criteres$facteur2[n]) %>%
            minmax[[1]](.)
         fcn_liste_criteres$tous_comestibles[n] <- valeur_actuelle != extremum
         fcn_liste_criteres$niveau2[n] <- infsup(crit_list_name, valeur_actuelle, minmax, 2, n)
      }
         else     # si facteur1 & facteur2 quantitatifs
         {
            minmax1 <- minmaxing(fcn_liste_criteres$niveau1[n], fcn_marge)
            minmax2 <- minmaxing(fcn_liste_criteres$niveau2[n], fcn_marge)
            valeur_actuelle1 <- fcn_lot_entrainement %>%
               filter(Type == "Rejeter") %>%
               select(fcn_liste_criteres$facteur1[n]) %>%
               minmax1[[1]](.)
            extremum1 <- fcn_lot_entrainement %>%
               select(fcn_liste_criteres$facteur1[n]) %>%
               minmax1[[1]](.)
            valeur_actuelle2 <- fcn_lot_entrainement %>%
               filter(Type == "Rejeter") %>%
               select(fcn_liste_criteres$facteur2[n]) %>%
               minmax2[[1]](.)
            extremum2 <- fcn_lot_entrainement %>%
               select(fcn_liste_criteres$facteur2[n]) %>%
               minmax2[[1]](.)
            fcn_liste_criteres$tous_comestibles[n] <- valeur_actuelle1 != extremum1 & valeur_actuelle2 != extremum2
            fcn_liste_criteres$niveau1[n] <- infsup(crit_list_name, valeur_actuelle1, minmax1, 1, n)  # Extrait "fcn_liste_criteres" en caractères dans la fonction infsup
            fcn_liste_criteres$niveau2[n] <- infsup(crit_list_name, valeur_actuelle2, minmax2, 2, n)
         }
      }
   }
   fcn_liste_criteres
}

# Définir fonction : concaténer tous les facteurs/niveaux monocritères en 1 chaîne + retrait de la liste bicritères
retrait_simple <- function(fcn_liste_crit_simple, fcn_liste_crit_double){
   noms_liste_bicritere <- deparse(substitute(fcn_liste_crit_double))
   facteurs_a_virer <- fcn_liste_crit_simple %>% 
      filter(tous_comestibles == TRUE, type %in% c("factor", "logical", "character")) %>% 
      select(facteur, niveau)
   monocrit1 <- paste0("(", noms_liste_bicritere, "$facteur1 == '", facteurs_a_virer$facteur, "' & ", 
                       noms_liste_bicritere, "$niveau1 == '", facteurs_a_virer$niveau, "')", collapse = " | ")
   monocrit2 <- paste0("(", noms_liste_bicritere, "$facteur2 == '", facteurs_a_virer$facteur, "' & ", 
                       noms_liste_bicritere, "$niveau2 == '", facteurs_a_virer$niveau, "')", collapse = " | ")
   suppr_monocrit <- paste(monocrit1, monocrit2, sep = " | ")            # Concatener listes facteur1 and facteur2
   index_monocrit <- which(eval(parse(text = suppr_monocrit)))        # Extraire tous les indices
   
   fcn_liste_crit_double[-index_monocrit,]
}

# Définir fonctions : convertit tous les facteurs textuels en chaînes de caractères pour l'étape de prédiction
# Chaine monocritère : extraire & concaténer CRIT + "==/>/<" + NIVEAU + "|" ....
crit2string1 <- function(fcn_liste_crit_simple){
   str_facteurs1 <- fcn_liste_crit_simple %>% 
      filter(tous_comestibles == TRUE, type %in% c("factor", "logical", "character")) %>% 
      mutate(niveau = str_c("== '", niveau, "'"))
   monocrit <- fcn_liste_crit_simple %>% 
      filter (tous_comestibles == TRUE, type %in% c("numeric", "integer")) %>% 
      rbind(str_facteurs1, .)
   paste(monocrit$facteur, monocrit$niveau, collapse = " | ")
}
#Chaine bicritère : extraire & concaténer CRIT1 + "==/>/<" + NIVEAU1 + "&" + CRIT2 + "==/</>" + NIVEAU2 "|" ....
crit2string2 <- function(fcn_liste_crit_simple, fcn_liste_crit_double){
   liste_monocrit <- crit2string1(fcn_liste_crit_simple)
   str_facteurs2f <- fcn_liste_crit_double %>%
      filter(tous_comestibles == TRUE, type1 %in% c("factor", "logical", "character")) %>%
      mutate(niveau1 = str_c("== '", niveau1, "'"))
   str_facteurs2ff <- str_facteurs2f %>%
      filter(type2 %in% c("factor", "logical", "character")) %>%
      mutate(niveau2 = str_c("== '", niveau2, "'"))
   str_facteurs2f <- str_facteurs2f %>%
      filter(type2 %in% c("numeric", "integer"))  %>%
      rbind(str_facteurs2ff, .)
   double_crit <- fcn_liste_crit_double %>%
      filter(tous_comestibles == TRUE, type1 %in% c("numeric", "integer")) %>%
      rbind(str_facteurs2f, .)
   double_crit_list <- paste("(", double_crit$facteur1, double_crit$niveau1, "&", double_crit$facteur2, double_crit$niveau2, ")",collapse = " | ")
   liste_bicrit <- paste(liste_monocrit, "|", double_crit_list)

   c(liste_monocrit, liste_bicrit)
}

# Lance l'analyse des critères pour les modèles mono et bicritères
START <- Sys.time()
marges1 <- 0
facteurs_liste1a <- recherche_simple(NAIF_lot_appr, facteurs_liste1, marges1)
facteurs_liste2a <- retrait_simple(facteurs_liste1a, facteurs_liste2)
STOP1 <- Sys.time()
marges2 <- 0
facteurs_liste2b <- recherche_double(NAIF_lot_appr, facteurs_liste2a, marges2)
STOP2 <- Sys.time()

NAIF_temps_mono <- difftime(STOP1, START, units = "secs") %>% round(.,1)
NAIF_temps_bi <- difftime(STOP2, START, units = "secs") %>% round(.,1)

# Montre les facteurs pertinents (i.e. 100% comestibles) + types données et niveaux (critères)
facteurs_pertinents1 <- facteurs_liste1a %>% filter(tous_comestibles == TRUE) %>% select(facteur, niveau, type)
facteurs_pertinents2 <- facteurs_liste2b %>% filter(tous_comestibles == TRUE) %>% select(facteur1, niveau1, type1, facteur2, niveau2, type2)

list_criteres_prediction <- crit2string2(facteurs_liste1a, facteurs_liste2b)

# Crée un lot de données predictions, avec facteurs booléens (VRAI = toxique) en .$reference
predictions <- NAIF_lot_evaluation
predictions$reference <- as.logical(as.character(recode_factor(predictions$Type, Conserver = FALSE, Rejeter = TRUE))) # Convertit en booléens

# Applique les 3 modèles prédictifs : stupide , monocritère, double-critère
predictions <- predictions%>% mutate(predict_stupide = TRUE) %>% # Considère tous les champignons comme toxiques
                              mutate(predict_mono = !eval(parse(text = list_criteres_prediction[1]))) %>%
                              mutate(predict_double = !eval(parse(text = list_criteres_prediction[2]))) %>%
                              select(reference, predict_stupide, predict_mono, predict_double)

# Convertit .$reference de booléen à facteur (confusionMatrix fonctionne avec des facteurs!)
predictions$reference <- as.factor(predictions$reference)
predictions$predict_stupide <- factor(predictions$predict_stupide, levels = c("FALSE","TRUE")) # Créer manuellement le niveau FALSE (pas présent dans le stupide) pour ConfusionMatrix
predictions$predict_mono <- as.factor(predictions$predict_mono)
predictions$predict_double <- as.factor(predictions$predict_double)

# Matrices de confusion
NAIF_CM_stupide <- confusionMatrix(data = predictions$predict_stupide, reference = predictions$reference, positive = "TRUE")
NAIF_CM_monocrit <- confusionMatrix(data = predictions$predict_mono, reference = predictions$reference, positive = "TRUE")
NAIF_CM_bicrit <- confusionMatrix(data = predictions$predict_double, reference = predictions$reference, positive = "TRUE")

NAIF_Resultats <- NULL
NAIF_Resultats$Stupide <-c(0, round(NAIF_CM_stupide$byClass["Sensitivity"],3), round(NAIF_CM_stupide$byClass["Specificity"],3), round(NAIF_CM_stupide$overall["Kappa"],3), 0)
NAIF_Resultats$MonoCritere <-c(1, round(NAIF_CM_monocrit$byClass["Sensitivity"],3), round(NAIF_CM_monocrit$byClass["Specificity"],3), round(NAIF_CM_monocrit$overall["Kappa"],3), NAIF_temps_mono)
NAIF_Resultats$BiCritere <-c(2, round(NAIF_CM_bicrit$byClass["Sensitivity"],3), round(NAIF_CM_bicrit$byClass["Specificity"],3), round(NAIF_CM_bicrit$overall["Kappa"],3), NAIF_temps_bi)

NAIF_Resultats <- data.frame(t(data.frame(NAIF_Resultats)))

colnames(NAIF_Resultats) <- c("n", "Sens", "Spec", "Kappa", "Temps (s)")
NAIF_Resultats <- NAIF_Resultats %>% mutate(Jw = round(Sens*NAIF_RatioSens + Spec*NAIF_RatioSpec - 1,3))

NAIF_Resultats <- NAIF_Resultats[, c("n", "Sens", "Spec", "Jw", "Kappa", "Temps (s)")]

save.image(file = "EKR-Champis-Naif.RData")
load(file = "EKR-Champis-Naif.RData")



#################################### FIN DU CLASSIFIEUR NAIF DE BASE ######################"

#####################################################################
#        REGLAGES HYPERPARAMETRES (MARGE) : LENT, A VIRER ???       #
#####################################################################
# library(parallel)       # Calcul parallèle pour les sapply
# 
# # Définir fonctions : mesure sensibilité/specificité selon la marge, pour réglage hyperparemètre monocritère
# tuning1a <- function(fcn_entrain, fcn_facteurs, fcn_marge){
#    nomfactliste <- deparse(substitute(fcn_facteurs))               # Extrait la liste des noms de facteurs comme chaîne
#    SCS <- paste0("recherche_simple(fcn_entrain, ", nomfactliste, ", fcn_marge)")      # Crée le texte correspondant à la recherche
#    fact_list <- eval(parse(text = SCS))               # Evalue la chaîne de caractères de la recherche (sinon les fonctions imbriquées ne vont pas détecter les noms de facteurs...)
#    critlist_prediction <- crit2string1(fact_list)
#    predictions$tuning <- TRUE                  # Règle .$tuning à VRAI (toxique) par défaut
#    predictions <- predictions %>% mutate(tuning = eval(parse(text = critlist_prediction[[1]])))    # Switch .$tuning to TRUE if criterion is met
#    predictions$tuning <- as.factor(predictions$tuning)
#    CM <- confusionMatrix(data = predictions$tuning, reference = predictions$reference, positive = "TRUE")
#    Sens <- round(CM$byClass["Sensitivity"], 4)
#    Spec <- round(CM$byClass["Specificity"], 4)
#    F1 <- round(CM$byClass["F1"], 4)
#    names(fcn_marge) <- "Marge"
#    c(fcn_marge, Sens, Spec, F1)    # Output 1-crit margin, sensitivity, specificity, F1-Score
# }
# 
# tuning2a <- function(fcn_entrain, fcn_facteurs1, fcn_facteurs2, fcn_marge2){
#    nomfactliste2 <- deparse(substitute(fcn_facteurs2))            # Extrait la liste des noms de facteurs comme chaîne
#    DCS <- paste0("recherche_double(fcn_entrain, ", nomfactliste2, ", fcn_marge2)")      # Crée le texte correspondant à la recherche
#    fact2_list <- eval(parse(text = DCS))            # Evalue la chaîne de caractères de la recherche
#    critlist_prediction <- crit2string2(fcn_facteurs1, fact2_list)
#    predictions$tuning <- TRUE                  # Règle .$tuning à VRAI (toxique) par défaut
#    predictions <- predictions %>% mutate(tuning = eval(parse(text = critlist_prediction[[2]])))    # Switch .$tuning to TRUE if criteria are met
#    predictions$tuning <- as.factor(predictions$tuning)
#    CM <- confusionMatrix(data = predictions$tuning, reference = predictions$reference, positive = "TRUE")
#    Sens <- round(CM$byClass["Sensitivity"], 4)
#    Spec <- round(CM$byClass["Specificity"], 4)
#    F1 <- round(CM$byClass["F1"], 4)
#    names(fcn_marge2) <- "Marge"
#    c(fcn_marge2, Sens, Spec, F1)    # Output 2-crit margin, sensitivity, specificity, F1-Score
# }
# 
# # Définir fonctions : réglage (1 entrée)
# tuning1b <- function(fcn_marge){
#    tuning1a(NAIF_lot_appr, facteurs_liste1, fcn_marge)
# }
# 
# tuning2b <- function(fcn_marge){
#    tuning2a(NAIF_lot_appr, facteurs_liste1a, facteurs_liste2a, fcn_marge)
# }
# 
# # Règle les marges et lance le réglage du modèle monocritère
# marge_1 <- c(seq(from = 0, to = 0.4, by = 0.1), seq(from = 0.5, to = 4, by = 0.5))
# START <- Sys.time()
# tuning_mono <- mclapply(X = marge_1, FUN = tuning1b)
# tuning_mono <- do.call(rbind.data.frame, tuning_mono)
# colnames(tuning_mono) <- c("Marge", "Sensitivity", "Specificity", "F1")
# #tuning_mono <- t(sapply(marge_1, FUN = tuning1b))
# tuning_mono <- as.data.frame(tuning_mono)
# STOP <- Sys.time()
# temps_mono <- STOP - START
# 
# # Choisit la meilleure marge (haute spécificité, puis sensibilité, puis marge la plus sûre)
# best_marges1 <- tuning_mono %>% 
#    filter(Specificity == max(Specificity)) %>% 
#    filter(Sensitivity == max(Sensitivity)) %>% 
#    filter(Marge == max(Marge))
# 
# # Lance classifieur monocritère avec marge optimale, règle la liste des critères pour le facteur2
# facteurs_liste1a <- recherche_simple(NAIF_lot_appr, facteurs_liste1, as.numeric(best_marges1["Marge"]))
# facteurs_liste2a <- retrait_simple(facteurs_liste1a, facteurs_liste2)
# 
# # Run double list classifier tuning
# marge_2 <- seq(from = 0, to = 2, by = 0.2)
# dual_crit_tune <- t(sapply(marge_2, FUN = tuning2b))
# dual_crit_tune <- as.data.frame(dual_crit_tune)
# 
# START <- Sys.time()
# test_tuning2 <- mclapply(X = marge_2, FUN = tuning2b)
# #test_tuning <- lapply(marge_1, FUN = tuning1b)
# test_tuning2 <- do.call(rbind.data.frame, test_tuning2)
# colnames(test_tuning2) <- c("Marge", "Sensitivity", "Specificity", "F1")
# STOP <- Sys.time()
# but_temps <- STOP - START
# 
# # Get best margin (highest specificity, then highest sensitivity, then safest margin)
# best_marges2 <- dual_crit_tune %>% 
#    filter(Specificity == max(Specificity)) %>% 
#    filter(Sensitivity == max(Sensitivity)) %>% 
#    filter(Marge == max(Marge))
# 
# # Définir fonction : Graphe Sensibilité, Spécificité, F1-Score
# SenSpeF1plot <- function(fcn_tuneresult, fcn_n){
#    fcn_tuneresult %>%
#       ggplot(aes_string(x = "Marge", y = names(fcn_tuneresult)[fcn_n])) + #aes_string permet d'utiliser des chaînes au lieu de noms de variables
#       geom_point() +
#       theme_bw()
# }
# 
# # Graphe paramètres réglages monocritères
# l <- ncol(tuning_mono)
# for (n in 2:l){         # Ne pas tracer la première colonne, c'est l'axe des x
#    plot <- SenSpeF1plot(tuning_mono, n)
#    plotname <- paste0("SCtune", names(tuning_mono)[n])   # Concaténer "SCtune" avec nom colonne
#    assign(plotname, plot)     # Attribuer le graphe au nom SCtune_colonne
# }
# 
# # Graphe paramètres réglages bicritères
# l <- ncol(dual_crit_tune)
# for (n in 2:l){         # Ne pas tracer la première colonne, c'est l'axe des x
#    plot <- SenSpeF1plot(dual_crit_tune, n)
#    plotname <- paste0("DCtune", names(dual_crit_tune)[n])   # Concaténer "DCtune" avec nom colonne
#    assign(plotname, plot)     # Attribuer le graphe au nom SCtune_colonne
# }
