##########################
#     INITIALISATION     #
##########################

# Chargement des bibliothèques
library(tidyverse)    # Outils génériques
library(microbenchmark)

##################################
#       FACTEUR CROISSANCE       #
##################################

n_distrib <- 1e6

# Lois de distribution

binom_size <- 20
fonction_binomiale <- function(x){rbinom(n = n_distrib, size = binom_size, prob = .65)}
temps_binomiale <- microbenchmark(fonction_binomiale(x))
valeurs_binomiale <- data.frame(val = fonction_binomiale(x))
distrib_binomiale <- ggplot(data = valeurs_binomiale, aes(x = val)) +
  ggtitle("Loi binomiale") +
  xlim(min =0, max = 1) +
  xlab("Valeur") +
  ylab("Densité") +
  geom_histogram(fill = "grey", color= "black", bins = 4*binom_size, center = 0.5) + 
  theme_bw()

fonction_uniforme <- function(x){runif(n = n_distrib)}
temps_uniforme <- microbenchmark(fonction_uniforme(x))
valeurs_uniforme <- data.frame(val = fonction_uniforme(x))
distrib_uniforme <- ggplot(data = valeurs_uniforme, aes(x = val)) +
  ggtitle("Loi uniforme") +
  xlim(min =0, max = 1) +
  xlab("Valeur") +
  ylab("Densité") +
  geom_histogram(fill = "grey", color= "black", bins = 4*binom_size, center = 0.5) + 
  theme_bw()

fonction_normale <- function(x){rnorm(n = n_distrib)}
temps_normale <- microbenchmark(fonction_normale(x))
valeurs_normale <- data.frame(val = fonction_normale(x))
distrib_normale <- ggplot(data = valeurs_normale, aes(x = val)) +
  ggtitle("Loi normale") +
#  xlim(min =0, max = 1) +
  xlab("Valeur") +
  ylab("Densité") +
  geom_histogram(fill = "grey", color= "black", bins = 4*binom_size, center = 0.5) + 
  theme_bw()

fonction_beta <- function(x){rbeta(n = n_distrib, shape1 = 6, shape2 = 4, ncp = .5)}
temps_beta <- microbenchmark(fonction_beta(x))
valeurs_beta <- data.frame(val = fonction_beta(x))
distrib_beta <- ggplot(data = valeurs_beta, aes(x = val)) +
  ggtitle("Loi beta") +
  xlim(min =0, max = 1) +
  xlab("Valeur") +
  ylab("Densité") +
  geom_histogram(fill = "grey", color= "black", bins = 2*binom_size, center = 0.5) + 
  theme_bw()

fonction_poisson <- function(x){rpois(n = n_distrib, lambda = binom_size/3)/binom_size}
temps_poisson <- microbenchmark(fonction_poisson(x))
valeurs_poisson <- data.frame(val = fonction_poisson(x))
distrib_poisson <- ggplot(data = valeurs_poisson, aes(x = val)) +
  ggtitle("Loi de Poisson") +
  xlim(min =0, max = 1) +
  xlab("Valeur") +
  ylab("Densité") +
  geom_histogram(fill = "grey", color= "black", center = 0.5, bins = 4*binom_size) + 
  theme_bw()

# Focus sur la loi Beta
facteurs <- c(.5, 1, 1.5, 2)

loi_beta <- function(facteur){rbeta(n = n_distrib, shape1 = 6*facteur, shape2 = 4, ncp = .5*facteur)}

beta <-lapply(X = facteurs, FUN = loi_beta)
names(beta) <- facteurs
beta <- as.data.frame(beta)
beta <- pivot_longer(beta, cols = 1:ncol(beta))
beta$name <- str_remove(beta$name, "X")

lois_beta <- ggplot(data = beta, aes(x = value, colour = name)) +
  ggtitle("Distribution de différentes lois beta") +
  labs(colour= "Fact. crois.") +
  xlab("Valeur") +
  ylab("Densité") +
  geom_density(alpha = .7, bw = .025, size = .6) + 
  theme_bw()





###############################
#       DISTRIBUTION 2D       #
###############################

n_champis <- 1e5      # Nombre de champignons
f_crois <- 2          # Facteur de croissance

Chap.Diam <- 10
Pied.Haut <- 8

Champi_demo <- NULL
Champi_demo$FacteurTaille <- rbeta(n = n_champis, shape1 = 6*f_crois, shape2 =4, ncp = .5*f_crois)
Champi_demo$Chapeau.Diametre <- Chap.Diam*Champi_demo$FacteurTaille*rnorm(n = n_champis, mean = 1, sd = .05) #.1
Champi_demo$Pied.Hauteur <- Pied.Haut*Champi_demo$FacteurTaille*rnorm(n = n_champis, mean = 1, sd = .05)
Champi_demo <- as.data.frame(Champi_demo)

outliers_diam <- mean(Champi_demo$Chapeau.Diametre > Chap.Diam)

scatter2d <-ggplot(data = Champi_demo, aes(x = Chapeau.Diametre, y = Pied.Hauteur)) +
  ggtitle(paste0("Distribution de taille de ", n_champis, " champignons générés aléatoirement")) +
  geom_point(shape = 3, alpha = 2e3/n_champis) +
  theme_bw() +
  geom_vline(xintercept = Chap.Diam, linetype = "dotted", color = "red") +
  geom_hline(yintercept = Pied.Haut, linetype = "dotted", color = "red")

densite2d <- ggplot(data = Champi_demo, aes(x = Chapeau.Diametre, y = Pied.Hauteur)) +
  ggtitle(paste0("Distribution de taille de ", n_champis, " champignons générés aléatoirement")) +
  # geom_density2d_filled(bins = 100) +
  # geom_density2d(bins=15, color = "white", alpha = .2) +
  # scale_fill_viridis_d(option = "H", direction = 1) + #B,F,G (H)
  # theme(legend.position="none") +
  stat_density_2d(geom = "polygon", contour = TRUE, contour_var = "count",       #density, count, ndensity
                  aes(fill = after_stat(level)),
                  bins = 50, n = 30) +
  scale_fill_viridis_c(option = "B", direction = -1, name = "Nombre") + #B,F,G (H)
  theme_bw() +
  geom_vline(xintercept = Chap.Diam, linetype = "dotted", color = "red") +
  geom_hline(yintercept = Pied.Haut, linetype = "dotted", color = "red")

distrib_diametre <- ggplot(data = Champi_demo, aes(x = Chapeau.Diametre)) +
  ggtitle(paste0("Distribution de diamètre de chapeau de ", n_champis, " champignons générés aléatoirement")) +
#  geom_histogram(bins = 40, color = "black", fill = "grey") +
  geom_density(bw = .2, fill = "grey", alpha = .2) +
  theme_bw() +
  geom_vline(xintercept = Chap.Diam, linetype = "dashed", color = "red")

lois_beta           # Profil des lois beta selon facteur de croissance
scatter2d           # Nuage de points des tailles/diamètres
densite2d           # Graphique de densité des tailles/diamètres
distrib_diametre    # Distribution du diamètre
outliers_diam*100       # % de diamètres hors-norme


save.image(file = "EKR-Champis-Intro.RData")
