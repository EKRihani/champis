##########################
#     INITIALISATION     #
##########################

# Chargement des bibliothèques
library(tidyverse)    # Outils génériques
library(microbenchmark) # Chrono
#library(MASS)   # Calcul densité 3D

##################################
#       FACTEUR CROISSANCE       #
##################################
set.seed(1337)       # Pour reproductibilité
n_chrono <- 1e6   # Nombre de valeurs pour microbenchmark
fois_chrono <- 1e2    # Nombre d'itérations pour microbenchmark
n_graph <- 1e5    # Nombre de valeurs pour graphiques

# Lois de distribution
binom_taille <- 20
fonction_binomiale <- function(x,N){rbinom(n = N, size = binom_taille, prob = .65)}
temps_binomiale <- microbenchmark(fonction_binomiale(x, n_chrono), times = fois_chrono, unit = "ms")

fonction_uniforme <- function(x,N){runif(n = N)}
temps_uniforme <- microbenchmark(fonction_uniforme(x, n_chrono), times = fois_chrono, unit = "ms")

fonction_normale <- function(x,N){rnorm(n = N)}
temps_normale <- microbenchmark(fonction_normale(x, n_chrono), times = fois_chrono, unit = "ms")

fonction_beta <- function(x,N){rbeta(n = N, shape1 = 6, shape2 = 4, ncp = .5)}
temps_beta <- microbenchmark(fonction_beta(x, n_chrono), times = fois_chrono, unit = "ms")

fonction_poisson <- function(x,N){rpois(n = N, lambda = binom_taille/3)}
temps_poisson <- microbenchmark(fonction_poisson(x, n_chrono), times = fois_chrono, unit = "ms")

fonction_weibull <- function(x,N){rweibull(n = N, shape = 8)}
temps_weibull <- microbenchmark(fonction_weibull(x, n_chrono), times = fois_chrono, unit = "ms")

# Calculs chrono
chrono_fonctions  <- bind_rows(temps_binomiale, temps_uniforme, temps_normale,
                              temps_beta, temps_poisson, temps_weibull)
chrono_fonctions$names <- chrono_fonctions$expr %>% str_remove_all(., "fonction_|(x, n_chrono)|[:punct:]") %>% str_to_title(.)
chrono_typique <- summary(chrono_fonctions)$uq %>% max(.) %>% `+`(50) %>% round(., digits = -2)  #Arrondir à la centaine (digits -2) par excès (+50)

# Graphe chrono
chrono_distrib2 <- ggplot(data = chrono_fonctions, aes(y = time/1e6, x = reorder(names, time))) +
  geom_boxplot(fill = "grey70", outlier.alpha = 0.3) +
  xlab(NULL) +
  ylab("Temps (ms)") +
  theme_bw() +
   theme(panel.grid.major.x = element_blank(),
         axis.ticks.x = element_blank()) +
   scale_y_continuous(trans = "log2")

# Focus sur la loi Beta pour génération de champignons
facteurs <- c(.5, 1, 1.5, 2)

loi_beta <- function(facteur){rbeta(n = n_graph, shape1 = 6*facteur, shape2 = 4, ncp = .5*facteur)}

beta <-lapply(X = facteurs, FUN = loi_beta)
names(beta) <- facteurs
beta <- as.data.frame(beta)
beta <- pivot_longer(beta, cols = 1:ncol(beta))
beta$name <- str_remove(beta$name, "X")

lois_beta <- ggplot(data = beta, aes(x = value, colour = name)) +
  labs(colour= "Fc") +
  geom_density(alpha = .7, bw = .025, linewidth = .6) +
  scale_color_viridis_d(option = "C", direction = -1, begin = 0, end = .85) +
  xlab("Valeur (Ft)") +
  ylab("Densité") +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.y = element_blank())


###############################
#       DISTRIBUTION 2D       #
###############################

set.seed(1337)       # Pour reproductibilité
INTRO_n_champis <- 1e5      # Nombre de champignons
n_reduit <- 5e4      # Nombre réduit (pour nuages de points 2D)
f_crois <- 2          # Facteur de croissance

Chap.Diam <- 10
Pied.Haut <- 8
Pied.Large <- 2

Champi_demo <- NULL
Champi_demo$FacteurTaille <- rbeta(n = INTRO_n_champis, shape1 = 6*f_crois, shape2 =4, ncp = .5*f_crois)
Champi_demo$Chapeau.Diametre <- Chap.Diam*Champi_demo$FacteurTaille*rnorm(n = INTRO_n_champis, mean = 1, sd = .05) # sd=.1?
Champi_demo$Pied.Hauteur <- Pied.Haut*Champi_demo$FacteurTaille*rnorm(n = INTRO_n_champis, mean = 1, sd = .05)
Champi_demo$Pied.Largeur <- Pied.Large*Champi_demo$FacteurTaille*rnorm(n = INTRO_n_champis, mean = 1, sd = .05)
Champi_demo <- as.data.frame(Champi_demo)

INTRO_taux_gros_diam <- round(mean(Champi_demo$Chapeau.Diametre > Chap.Diam)*100 , 1)
INTRO_taux_supergros_diam <- round(mean(Champi_demo$Chapeau.Diametre > (1.1*Chap.Diam) )+0.00005,4)*100

nuage_avecdispersion <- ggplot(data = Champi_demo[1:n_reduit,], aes(x = Chapeau.Diametre, y = Pied.Hauteur)) +
  geom_point(shape = 20, alpha = 2e3/INTRO_n_champis, size = .5) +
  theme_bw() +
  geom_vline(xintercept = Chap.Diam, linetype = "dotted", color = "red") +
  geom_hline(yintercept = Pied.Haut, linetype = "dotted", color = "red") +
  ylab(NULL) + xlab("Diamètre du chapeau (Dc)")

nuage_sansdispersion <-ggplot(data = Champi_demo[1:n_reduit,], aes(x = Chapeau.Diametre, y = Chapeau.Diametre*.8)) +
  geom_point(shape = 20, alpha = 2e3/INTRO_n_champis, size= .5) +
  theme_bw() +
  geom_vline(xintercept = Chap.Diam, linetype = "dotted", color = "red") +
  geom_hline(yintercept = Chap.Diam*.8, linetype = "dotted", color = "red") +
  ylab("Longueur du stipe (Ls)") + xlab("Diamètre du chapeau (Dc)")


# Nettoyage données et sauvegarde
save.image(file = "EKR-Champis-CodeSource.RData")
load("EKR-Champis-CodeSource.RData")
