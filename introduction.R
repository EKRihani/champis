##########################
#     INITIALISATION     #
##########################

# Chargement des bibliothèques
library(tidyverse)    # Outils génériques

##################################
#       FACTEUR CROISSANCE       #
##################################

n_distrib <- 1e3
facteurs <- c(.5, 1, 1.5, 2)

loi_beta <- function(facteur){rbeta(n = n_distrib, shape1 = 6*facteur, shape2 =4, ncp = .5*facteur)}

beta <-lapply(X = facteurs, FUN = loi_beta)
names(beta) <- facteurs
beta <- as.data.frame(beta)
beta <- pivot_longer(beta, cols = 1:ncol(beta))

ggplot(data = beta, aes(x = value, colour = name)) +
  geom_density(alpha = .7)

###############################
#       DISTRIBUTION 2D       #
###############################

n_champis <- 1e5      # Nombre de champignons
f_crois <- 2          # Facteur de croissance

Chap.Diam <- 10
Pied.Haut <- 8

Champi_demo <- NULL
Champi_demo$FacteurTaille <- rbeta(n = n_champis, shape1 = 6*f_crois, shape2 =4, ncp = .5*f_crois)
Champi_demo$Chapeau.Diametre <- Chap.Diam*Champi_demo$FacteurTaille*rnorm(n = n_champis, mean = 1, sd = .1)
Champi_demo$Pied.Hauteur <- Pied.Haut*Champi_demo$FacteurTaille*rnorm(n = n_champis, mean = 1, sd = .1)
Champi_demo <- as.data.frame(Champi_demo)

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
  scale_fill_viridis_c(option = "F", direction = -1, name = "Nombre") + #B,F,G (H)
  theme_bw() +
  geom_vline(xintercept = Chap.Diam, linetype = "dotted", color = "red") +
  geom_hline(yintercept = Pied.Haut, linetype = "dotted", color = "red")

beta
scatter2d
densite2d
