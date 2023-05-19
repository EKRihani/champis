##########################
#     INITIALISATION     #
##########################

# Chargement des bibliothèques
library(tidyverse)    # Outils génériques
library(caret)        # Outils d'apprentissage machine
library(DiceDesign)    # Hypercubes Latins
library(DiceEval)       # Modélisation sur hypercubes latins
library(twinning)       # Découpage équilibré des jeux de données (plus efficient que split!)

# Récupération, décompression, importation des données
fichier_data <- tempfile()
#URL <- "https://github.com/EKRihani/champis/raw/master/MushroomDataset.zip"      # URL de mon repo
# download.file(URL, fichier_data)
fichier_data <- "~/projects/champis/MushroomDataset.zip" # FICHIER LOCAL

fichier_data <- unzip(fichier_data, "MushroomDataset/secondary_data.csv")
dataset <- read.csv(fichier_data, header = TRUE, sep = ";", stringsAsFactors = TRUE)
dataset$class <- recode_factor(dataset$class, e = "comestible", p = "toxique")
dataset$class <- relevel(dataset$class, ref = "toxique")

####################################################################
#     CREATION DES LOTS D'ENTRAINEMENT, VALIDATION, EVALUATION     #
####################################################################

BI_n_champis <- nrow(dataset)
BI_split_p <- sqrt(BI_n_champis)
BI_split_facteur <- round(sqrt(BI_split_p)+1)

set.seed(007)
index1 <- twin(data = dataset, r = BI_split_facteur)
BI_lot_appr_opti <- dataset[-index1,]
BI_lot_evaluation <- dataset[index1,]

set.seed(007)
index1 <- twin(data = BI_lot_appr_opti, r = BI_split_facteur)
BI_lot_appr <- BI_lot_appr_opti[-index1,]
BI_lot_opti <- BI_lot_appr_opti[index1,]

save.image(file = "test.RData")
load(file = "test.RData")
############################################################
#     MODELE DE CLASSIFICATION SIMPLE : LISTE CRITERES     #
############################################################

# Créer listes de critères (facteur + type + valeurs) pour classification simple
facteurs_liste <- BI_lot_appr %>% 
   select(where(is.factor) | where(is.logical)) %>% 
   gather(facteur, niveau) %>% 
   unique() %>% 
   select(facteur, niveau) %>% 
   filter(facteur != "class")     # Récupère tous les niveaux de facteurs + logique

facteurs_type <- BI_lot_appr %>% 
   summary.default %>% 
   as.data.frame %>% 
   group_by(Var1) %>% 
   spread(Var2, Freq) %>% 
   as.data.frame        # Extraire structure du lot d'entraînement

facteurs_type$Class <- if_else(facteurs_type$Class == "-none-", facteurs_type$Mode, facteurs_type$Class)     # Créer une colonne Class cohérente

facteurs_type <- facteurs_type %>% 
   select(-Mode, -Length)              # Nettoie facteurs_type

colnames(facteurs_type) <- c("facteur", "type")     ############ A TRADUIRE ###############

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
half1_list2 <- facteurs_liste[index_list2[,1],]            # Extraire 1er attribut, d'après le 1er index
colnames(half1_list2) <- c("facteur1", "niveau1", "type1")
half2_list2 <- facteurs_liste[index_list2[,2],]            # Extraire 2e attribut, d'après le 2e index
colnames(half2_list2) <- c("facteur2", "niveau2", "type2")
facteurs_liste2 <- cbind(half1_list2, half2_list2)
facteurs_liste2 <- facteurs_liste2 %>% filter(facteur1 != facteur2)     # Retire doublons
facteurs_liste2$tous_comestibles <- FALSE                        # Par défaut : non-comestible


save.image(file = "test.RData")
load(file = "test.RData")
###########################################################
#     CONSTRUCTION DU MODELE DE CLASSIFICATION SIMPLE     #
###########################################################

# Vérifie structure facteurs_liste2 structure : en théorie, par construction, il ne devrait PAS avoir de texte facteur2 + numérique facteur1
# factors_check <- facteurs_liste2 %>% 
#    filter(type2 %in% c("logical", "factor", "character"), type1 %in% c("integer", "numeric")) %>% 
#    nrow

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
            filter(class == "toxique", get(fcn_liste_criteres$facteur[n]) == fcn_liste_criteres$niveau[n]) %>% 
            nrow() == 0  # Trouve si (pour cette combinaison facteur/niveau) il n'y a aucun non-comestible
      }
      else          # Si type numérique (ou integer)
      {
         minmax <- minmaxing(fcn_liste_criteres$niveau[n], fcn_marge)     # Pose min/max et valeurs arrondies pour ".$niveau"
         valeur_actuelle <- fcn_lot_entrainement %>% 
            filter(class == "toxique") %>% 
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
            filter(class == "toxique", get(fcn_liste_criteres$facteur1[n]) == fcn_liste_criteres$niveau1[n],
                   get(fcn_liste_criteres$facteur2[n]) == fcn_liste_criteres$niveau2[n]) %>%
            nrow
         fcn_liste_criteres$tous_comestibles[n] <- comptage != 0 & comptage_poison == 0 # Trouve si (pour cette combinaison facteur/niveau) il y a des champis ET aucun vénéneux
      }
      else          # si facteur1 texte & facteur2 quantitatif
      {if(fcn_liste_criteres$type1[n] %in% c("logical", "factor", "character") & fcn_liste_criteres$type2[n] %in% c("numeric", "integer"))
      {
         minmax <- minmaxing(fcn_liste_criteres$niveau2[n], fcn_marge)
         valeur_actuelle <- fcn_lot_entrainement %>%
            filter(class == "toxique", get(fcn_liste_criteres$facteur1[n]) == fcn_liste_criteres$niveau1[n]) %>%
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
               filter(class == "toxique") %>%
               select(fcn_liste_criteres$facteur1[n]) %>%
               minmax1[[1]](.)
            extremum1 <- fcn_lot_entrainement %>%
               select(fcn_liste_criteres$facteur1[n]) %>%
               minmax1[[1]](.)
            valeur_actuelle2 <- fcn_lot_entrainement %>%
               filter(class == "toxique") %>%
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
   double_criteria <- fcn_liste_crit_double %>%
      filter(tous_comestibles == TRUE, type1 %in% c("numeric", "integer")) %>%
      rbind(str_facteurs2f, .)
   double_criteria_list <- paste("(", double_criteria$facteur1, double_criteria$niveau1, "&", double_criteria$facteur2, double_criteria$niveau2, ")",collapse = " | ")
   liste_bicrit <- paste(liste_monocrit, "|", double_criteria_list)

   c(liste_monocrit, liste_bicrit)
}

save.image(file = "test.RData")
load(file = "test.RData")

# Run criteria analyses for single and dual-criteria models
marges1 <- 2
facteurs_liste1a <- recherche_simple(BI_lot_appr, facteurs_liste1, marges1)
facteurs_liste2a <- retrait_simple(facteurs_liste1a, facteurs_liste2)
marges2 <- 3
facteurs_liste2b <- recherche_double(BI_lot_appr, facteurs_liste2a, marges2)

# Show relevant (i.e. edible-only) factors, data types and levels (criterion)
facteurs_pertinents1 <- facteurs_liste1a %>% filter(tous_comestibles == TRUE) %>% select(facteur, niveau, type)
facteurs_pertinents2 <- facteurs_liste2b %>% filter(tous_comestibles == TRUE) %>% select(facteur1, niveau1, type1, facteur2, niveau2, type2)

criteria_list_prediction <- crit2string2(facteurs_liste1a, facteurs_liste2b)

# Create a prediction dataset, with boolean factors (meaning "is.edible") as .$reference
predictions <- BI_lot_evaluation
predictions$reference <- as.logical(as.character(recode_factor(predictions$class, comestible = TRUE, toxique = FALSE))) # Switch to logical values

# Apply the three predictive models : stupid , single-criterion, double-criteria
predictions$stupid_predict = FALSE   # Consider all mushrooms as toxique
predictions <- predictions %>% mutate(mono_predict = eval(parse(text = criteria_list_prediction[1])))
predictions <- predictions %>% mutate(bi_predict = eval(parse(text = criteria_list_prediction[2])))

# Convert .$reference from logical to factor (confusionMatrix works with factors)
predictions$reference <- as.factor(predictions$reference)
predictions$stupid_predict <- factor(predictions$stupid_predict, levels = c("FALSE","TRUE")) # Create level TRUE (not present in stupid model) for confusionMatrix use (reference & prediction must have the same levels)
predictions$mono_predict <- as.factor(predictions$mono_predict)
predictions$bi_predict <- as.factor(predictions$bi_predict)

# Confusion matrices
CM_stupid <- confusionMatrix(data = predictions$stupid_predict, reference = predictions$reference, positive = "TRUE")
CM_monocrit <- confusionMatrix(data = predictions$mono_predict, reference = predictions$reference, positive = "TRUE")
CM_bicrit <- confusionMatrix(data = predictions$bi_predict, reference = predictions$reference, positive = "TRUE")

save.image(file = "test.RData")
load(file = "test.RData")


# Define functions : get sensitivity/specificity according to margin, for single-crit tuning
tuning1a <- function(fcn_trainset, fcn_factorlist, fcn_marge){
   factlistname <- deparse(substitute(fcn_factorlist))               # Get factor list name as string
   SCS <- paste0("recherche_simple(fcn_trainset, ", factlistname, ", fcn_marge)")      # Create single-search string
   fact_list <- eval(parse(text = SCS))               # Evaluate single-search string (or nested functions will not detect accurately the original factor list name)
   critlist_prediction <- crit2string1(fact_list)
   predictions$tuning <- FALSE                  # Set .$tuning to false by default
   predictions <- predictions %>% mutate(tuning = eval(parse(text = critlist_prediction[[1]])))    # Switch .$tuning to TRUE if criterion is met
   predictions$tuning <- as.factor(predictions$tuning)
   CM <- confusionMatrix(data = predictions$tuning, reference = predictions$reference, positive = "TRUE")
   sensitivity <- round(CM$byClass["Sensitivity"], 4)
   specificity <- round(CM$byClass["Specificity"], 4)
   F1 <- round(CM$byClass["F1"], 4)
   names(fcn_marge) <- "Margin"
   c(fcn_marge, sensitivity, specificity, F1)    # Output 1-crit margin, sensitivity, specificity, F1-Score
}

tuning2a <- function(fcn_trainset, fcn_factorlist1, fcn_factorlist2, fcn_marge2){
   factlistname2 <- deparse(substitute(fcn_factorlist2))            # Get factor list name as string
   DCS <- paste0("recherche_double(fcn_trainset, ", factlistname2, ", fcn_marge2)")      # Create dual-search string
   fact2_list <- eval(parse(text = DCS))            # Evaluate dual-search string (or nested functions will not detect accurately the original factor list name)
   critlist_prediction <- crit2string2(fcn_factorlist1, fact2_list)
   predictions$tuning <- FALSE                  # Set .$tuning to false by default
   predictions <- predictions %>% mutate(tuning = eval(parse(text = critlist_prediction[[2]])))    # Switch .$tuning to TRUE if criteria are met
   predictions$tuning <- as.factor(predictions$tuning)
   CM <- confusionMatrix(data = predictions$tuning, reference = predictions$reference, positive = "TRUE")
   sensitivity <- round(CM$byClass["Sensitivity"], 4)
   specificity <- round(CM$byClass["Specificity"], 4)
   F1 <- round(CM$byClass["F1"], 4)
   names(fcn_marge2) <- "Margin"
   c(fcn_marge2, sensitivity, specificity, F1)    # Output 2-crit margin, sensitivity, specificity, F1-Score
}

# Define functions : synthetic tuning functions (need only one input)
tuning1b <- function(fcn_marge){
   tuning1a(BI_lot_appr, facteurs_liste1, fcn_marge)
}

tuning2b <- function(fcn_marge){
   tuning2a(BI_lot_appr, facteurs_liste1a, facteurs_liste2a, fcn_marge)
}

# Set margin list and run single list classifier tuning
margin_1 <- c(seq(from = 0, to = 0.4, by = 0.1), seq(from = 0.5, to = 4, by = 0.5))
single_crit_tune <- t(sapply(margin_1, FUN = tuning1b))
single_crit_tune <- as.data.frame(single_crit_tune)

# Get best margin (highest specificity, then highest sensitivity, then safest margin)
best_marges1 <- single_crit_tune %>% 
   filter(Specificity == max(Specificity)) %>% 
   filter(Sensitivity == max(Sensitivity)) %>% 
   filter(Margin == max(Margin))

# Run single list  classifier with optimum margin, set crit list for factor 2
facteurs_liste1a <- recherche_simple(BI_lot_appr, facteurs_liste1, as.numeric(best_marges1["Margin"]))
facteurs_liste2a <- retrait_simple(facteurs_liste1a, facteurs_liste2)

# Run double list classifier tuning
margin_2 <- seq(from = 0, to = 2, by = 0.2)
dual_crit_tune <- t(sapply(margin_2, FUN = tuning2b))
dual_crit_tune <- as.data.frame(dual_crit_tune)

# Get best margin (highest specificity, then highest sensitivity, then safest margin)
best_marges2 <- dual_crit_tune %>% 
   filter(Specificity == max(Specificity)) %>% 
   filter(Sensitivity == max(Sensitivity)) %>% 
   filter(Margin == max(Margin))

# Define function : Sensitivity, Specificity, F1-Score plot
SenSpeF1plot <- function(fcn_tuneresult, fcn_n){
   fcn_tuneresult %>%
      ggplot(aes_string(x = "Margin", y = names(fcn_tuneresult)[fcn_n])) + #aes_string allows use of string instead of variable name
      geom_point() +
      theme_bw()
}

# Plot single_crit tuning parameters
l <- ncol(single_crit_tune)
for (n in 2:l){         # Don't plot first col : it is the x axis
   plot <- SenSpeF1plot(single_crit_tune, n)
   plotname <- paste0("SCtune", names(single_crit_tune)[n])   # Concatenate "SCtune" with the column name
   assign(plotname, plot)     # Assign the plot to the SCtune_colname name
}

# Plot dual_crit tuning parameters
l <- ncol(dual_crit_tune)
for (n in 2:l){         # Don't plot first col : it is the x axis
   plot <- SenSpeF1plot(dual_crit_tune, n)
   plotname <- paste0("DCtune", names(dual_crit_tune)[n])   # Concatenate "DCtune" with the column name
   assign(plotname, plot)     # Assign the plot to the DCtune_colname name
}

