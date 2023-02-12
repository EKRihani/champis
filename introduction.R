##########################
#     INITIALISATION     #
##########################

# Chargement des bibliothèques
library(tidyverse)    # Outils génériques
library(microbenchmark) # Chrono
library(MASS)   # Calcul densité 3D
library(plotly)   # Graphes avancés

##################################
#       FACTEUR CROISSANCE       #
##################################

n_chrono <- 1e6   # Nombre de valeurs pour microbenchmark
n_graph <- 1e5    # Nombre de valeurs pour graphiques

# Lois de distribution

binom_size <- 20
fonction_binomiale <- function(x,N){rbinom(n = N, size = binom_size, prob = .65)}
temps_binomiale <- microbenchmark(fonction_binomiale(x, n_chrono), unit = "ms")
valeurs_binomiale <- data.frame(val = fonction_binomiale(x, n_graph))
distrib_binomiale <- ggplot(data = valeurs_binomiale, aes(x = val)) +
#  ggtitle("Loi binomiale") +
  xlab("Valeur") +
  ylab("") +
  geom_histogram(fill = "grey40", bins = 4*binom_size, center = 0.5) + 
  theme_bw() +
  theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))

fonction_uniforme <- function(x,N){runif(n = N)}
temps_uniforme <- microbenchmark(fonction_uniforme(x, n_chrono), unit = "ms")
valeurs_uniforme <- data.frame(val = fonction_uniforme(x, n_graph))
distrib_uniforme <- ggplot(data = valeurs_uniforme, aes(x = val)) +
#  ggtitle("Loi uniforme") +
  xlim(min =0, max = 1) +
  xlab("Valeur") +
  ylab("") +
  geom_histogram(fill = "grey40", bins = 4*binom_size, center = 0.5) + 
  theme_bw() +
  theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))

fonction_normale <- function(x,N){rnorm(n = N)}
temps_normale <- microbenchmark(fonction_normale(x, n_chrono), unit = "ms")
valeurs_normale <- data.frame(val = fonction_normale(x, n_graph))
distrib_normale <- ggplot(data = valeurs_normale, aes(x = val)) +
#  ggtitle("Loi normale") +
  xlab("Valeur") +
  ylab("") +
  geom_histogram(fill = "grey40", bins = 4*binom_size, center = 0.5) + 
  theme_bw() +
  theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))

fonction_beta <- function(x,N){rbeta(n = N, shape1 = 6, shape2 = 4, ncp = .5)}
temps_beta <- microbenchmark(fonction_beta(x, n_chrono), unit = "ms")
valeurs_beta <- data.frame(val = fonction_beta(x, n_graph))
distrib_beta <- ggplot(data = valeurs_beta, aes(x = val)) +
#  ggtitle("Loi beta") +
  xlim(min =0, max = 1) +
  xlab("Valeur") +
  ylab("") +
  geom_histogram(fill = "grey40", bins = 2*binom_size, center = 0.5) + 
  theme_bw() +
  theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))

fonction_poisson <- function(x,N){rpois(n = N, lambda = binom_size/3)}
temps_poisson <- microbenchmark(fonction_poisson(x, n_chrono), unit = "ms")
valeurs_poisson <- data.frame(val = fonction_poisson(x, n_graph))
distrib_poisson <- ggplot(data = valeurs_poisson, aes(x = val)) +
#  ggtitle("Loi de Poisson") +
  xlab("Valeur") +
  ylab("") +
  geom_histogram(fill = "grey40", center = 0.5, bins = 4*binom_size) + 
  theme_bw() +
  theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))

fonction_weibull <- function(x,N){rweibull(n = N, shape = 8)}
temps_weibull <- microbenchmark(fonction_weibull(x, n_chrono), unit = "ms")
valeurs_weibull <- data.frame(val = fonction_weibull(x, n_graph))
distrib_weibull <- ggplot(data = valeurs_weibull, aes(x = val)) +
#  ggtitle("Loi de Weibull") +
  xlab("Valeur") +
  ylab("") +
  geom_histogram(fill = "grey40", center = 0.5, bins = 4*binom_size) + 
  theme_bw() +
  theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))

chrono_fonctions  <- bind_rows(temps_binomiale, temps_uniforme, temps_normale, 
                              temps_beta, temps_poisson, temps_weibull)
chrono_fonctions$names <- chrono_fonctions$expr %>% str_remove_all(., "fonction_|(x, n_chrono)|[:punct:]") %>% str_to_title(.)

temps_fonctions <- summary(chrono_fonctions)
rownames(temps_fonctions) <- temps_fonctions$expr %>% str_remove_all(., "fonction_|(x, n_chrono)|[:punct:]") %>% str_to_title(.)
chrono_typique <- temps_fonctions$uq %>% max(.) %>% round(., digits = -2)
temps_fonctions <- temps_fonctions %>% arrange(., mean) %>% select(min, mean, median, max) %>% round(.,2)
colnames(temps_fonctions) <- c("Mininum", "Moyenne", "Médiane", "Maximum")

chrono_distrib <- ggplot(data = chrono_fonctions, aes(y = time/1e6, x = reorder(names, time))) +
  ylab("Temps (ms)") +
  xlab(NULL) +
  geom_boxplot(fill = "grey70") +
  theme_bw()

# Focus sur la loi Beta pour génération champis
facteurs <- c(.5, 1, 1.5, 2)

loi_beta <- function(facteur){rbeta(n = n_graph, shape1 = 6*facteur, shape2 = 4, ncp = .5*facteur)}

beta <-lapply(X = facteurs, FUN = loi_beta)
names(beta) <- facteurs
beta <- as.data.frame(beta)
beta <- pivot_longer(beta, cols = 1:ncol(beta))
beta$name <- str_remove(beta$name, "X")

lois_beta <- ggplot(data = beta, aes(x = value, colour = name)) +
#  ggtitle("Distribution de différentes lois bêta") +
  labs(colour= "Fc") +
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
Pied.Large <- 2

Champi_demo <- NULL
Champi_demo$FacteurTaille <- rbeta(n = n_champis, shape1 = 6*f_crois, shape2 =4, ncp = .5*f_crois)
Champi_demo$Chapeau.Diametre <- Chap.Diam*Champi_demo$FacteurTaille*rnorm(n = n_champis, mean = 1, sd = .05) #.1
Champi_demo$Pied.Hauteur <- Pied.Haut*Champi_demo$FacteurTaille*rnorm(n = n_champis, mean = 1, sd = .05)
Champi_demo$Pied.Largeur <- Pied.Large*Champi_demo$FacteurTaille*rnorm(n = n_champis, mean = 1, sd = .05)
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

dens3Ddouble <- MASS::kde2d(Champi_demo$Chapeau.Diametre, Champi_demo$Pied.Hauteur, n= 500)
dens3Dsimple <- MASS::kde2d(Champi_demo$Chapeau.Diametre, Champi_demo$Chapeau.Diametre, n= 500)

graphe3Ddouble <- plot_ly(x=dens3Ddouble$x, y=dens3Ddouble$y, z=dens3Ddouble$z) %>% 
  add_surface(colorscale ="YlGnBu", contours = list(z = list(project=list(z=TRUE), show=TRUE, usecolormap=TRUE, start = 0, end = 1, size = max(dens3Ddouble$z)/20)))
#Blackbody, Cividis, Electric, Hot, Jet, Portland, RdBu, Viridis, YlGnBu, YlOrRd
graphe3Dsimple <- plot_ly(x=dens3Dsimple$x, y=dens3Dsimple$y, z=dens3Dsimple$z) %>% 
  add_surface(colorscale ="Viridis", contours = list(z = list(project=list(z=TRUE), show=TRUE, usecolormap=TRUE, start = 0, end = 1, size = max(dens3Dsimple$z)/20)))

scatter3Ddouble <- plot_ly(x=Champi_demo$Chapeau.Diametre, y=Champi_demo$Pied.Hauteur, z=Champi_demo$Pied.Largeur, marker = list(size=1)) %>% 
  add_markers()
scatter3Dsimple <- plot_ly(x=Champi_demo$Chapeau.Diametre, y=Champi_demo$Chapeau.Diametre, z=Champi_demo$Chapeau.Diametre, marker = list(size=1)) %>% 
  add_markers()

##########################
#     DONNEES FINALES    #
##########################

# Lois de distribution (temps et graphique de distribution)
temps_fonctions

distrib_binomiale
distrib_uniforme
distrib_normale
distrib_beta
distrib_poisson
distrib_weibull
chrono_distrib

# Distribution Champis
lois_beta           # Profil des lois beta selon facteur de croissance
scatter2d           # Nuage de points des tailles/diamètres
densite2d           # Graphique de densité 2D des tailles/diamètres

graphe3Ddouble      # Graphique de densité 3D des tailles/diamètres
graphe3Ddouble      # Graphique de densité 3D des diamètres/diamètres (sans dispersion)
scatter3Ddouble           # Nuage de points 3D des tailles/diamètres
scatter3Dsimple           # Nuage de points 3D des tailles/diamètres (sans dispersion)

distrib_diametre    # Distribution du diamètre
outliers_diam*100       # % de diamètres hors-norme


save.image(file = "EKR-Champis-Intro.RData")
