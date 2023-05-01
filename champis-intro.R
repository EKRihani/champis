##########################
#     INITIALISATION     #
##########################

# Chargement des bibliothèques
library(tidyverse)    # Outils génériques
library(microbenchmark) # Chrono
library(MASS)   # Calcul densité 3D
library(plotly)   # Graphes avancés
library(DiceDesign)   # Hypercubes latins

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
valeurs_binomiale <- data.frame(val = fonction_binomiale(x, n_graph))
distrib_binomiale <- ggplot(data = valeurs_binomiale, aes(x = val)) +
  geom_histogram(fill = "grey40", bins = 4*binom_taille, center = 0.5) +
  xlab("Valeur") +
  ylab("") +
  theme_bw() +
  theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))

fonction_uniforme <- function(x,N){runif(n = N)}
temps_uniforme <- microbenchmark(fonction_uniforme(x, n_chrono), times = fois_chrono, unit = "ms")
valeurs_uniforme <- data.frame(val = fonction_uniforme(x, n_graph))
distrib_uniforme <- ggplot(data = valeurs_uniforme, aes(x = val)) +
  geom_histogram(fill = "grey40", bins = 4*binom_taille, center = 0.5) +
  xlim(min =0, max = 1) +
  xlab("Valeur") +
  ylab("") +
  theme_bw() +
  theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))

fonction_normale <- function(x,N){rnorm(n = N)}
temps_normale <- microbenchmark(fonction_normale(x, n_chrono), times = fois_chrono, unit = "ms")
valeurs_normale <- data.frame(val = fonction_normale(x, n_graph))
distrib_normale <- ggplot(data = valeurs_normale, aes(x = val)) +
  geom_histogram(fill = "grey40", bins = 4*binom_taille, center = 0.5) +
  xlab("Valeur") +
  ylab("") +
  theme_bw() +
  theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))

fonction_beta <- function(x,N){rbeta(n = N, shape1 = 6, shape2 = 4, ncp = .5)}
temps_beta <- microbenchmark(fonction_beta(x, n_chrono), times = fois_chrono, unit = "ms")
valeurs_beta <- data.frame(val = fonction_beta(x, n_graph))
distrib_beta <- ggplot(data = valeurs_beta, aes(x = val)) +
  geom_histogram(fill = "grey40", bins = 2*binom_taille, center = 0.5) + 
  xlim(min =0, max = 1) +
  xlab("Valeur") +
  ylab("") +
  theme_bw() +
  theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))

fonction_poisson <- function(x,N){rpois(n = N, lambda = binom_taille/3)}
temps_poisson <- microbenchmark(fonction_poisson(x, n_chrono), times = fois_chrono, unit = "ms")
valeurs_poisson <- data.frame(val = fonction_poisson(x, n_graph))
distrib_poisson <- ggplot(data = valeurs_poisson, aes(x = val)) +
  geom_histogram(fill = "grey40", color = "grey40", center = 0.5, bins = 4*binom_taille) + 
  xlab("Valeur") +
  ylab("") +
  theme_bw() +
  theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))

fonction_weibull <- function(x,N){rweibull(n = N, shape = 8)}
temps_weibull <- microbenchmark(fonction_weibull(x, n_chrono), times = fois_chrono, unit = "ms")
valeurs_weibull <- data.frame(val = fonction_weibull(x, n_graph))
distrib_weibull <- ggplot(data = valeurs_weibull, aes(x = val)) +
  geom_histogram(fill = "grey40", color = "grey40", center = 0.5, bins = 4*binom_taille) +
  xlab("Valeur") +
  ylab("") +
  theme_bw() +
  theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5))

# Calculs chrono
chrono_fonctions  <- bind_rows(temps_binomiale, temps_uniforme, temps_normale,
                              temps_beta, temps_poisson, temps_weibull)
chrono_fonctions$names <- chrono_fonctions$expr %>% str_remove_all(., "fonction_|(x, n_chrono)|[:punct:]") %>% str_to_title(.)
chrono_typique <- summary(chrono_fonctions)$uq %>% max(.) %>% `+`(50) %>% round(., digits = -2)  #Arrondir à la centaine (digits -2) par excès (+50)

# Graphe chrono
chrono_distrib <- ggplot(data = chrono_fonctions, aes(y = time/1e6, x = reorder(names, time))) +
  geom_boxplot(fill = "grey70", outlier.alpha = 0.3) +
  xlab(NULL) +
  ylab("Temps (ms)") +
  theme_bw()

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
  xlab("Valeur") +
  ylab("Densité") +
  theme_bw()


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

densite2d <- ggplot(data = Champi_demo, aes(x = Chapeau.Diametre, y = Pied.Hauteur)) +
  stat_density_2d(geom = "polygon", contour = TRUE, contour_var = "count",       #density, count, ndensity
                  aes(fill = after_stat(level)),
                  bins = 50, n = 30) +
  scale_fill_viridis_c(option = "B", direction = -1, name = "Nombre") + #B,F,G (H)
  theme_bw() +
  geom_vline(xintercept = Chap.Diam, linetype = "dotted", color = "red") +
  geom_hline(yintercept = Pied.Haut, linetype = "dotted", color = "red") +
  ylab("Longueur du stipe (Ls)") + xlab("Diamètre du chapeau (Dc)")

distrib_diametre <- ggplot(data = Champi_demo, aes(x = Chapeau.Diametre)) +
  geom_density(bw = .2, fill = "grey", alpha = .2) +
  theme_bw() + xlab("Diamètre de stipe (Ds)") + ylab("Densité") +
  geom_vline(xintercept = Chap.Diam, linetype = "dashed", color = "red")

# Graphiques 3D (pas utilisés ?)
#dens3Ddouble <- MASS::kde2d(Champi_demo$Chapeau.Diametre, Champi_demo$Pied.Hauteur, n= 500)
#dens3Dsimple <- MASS::kde2d(Champi_demo$Chapeau.Diametre, Champi_demo$Chapeau.Diametre, n= 500)

# graphe3D_avecdispersion <- plot_ly(x=dens3Ddouble$x, y=dens3Ddouble$y, z=dens3Ddouble$z) %>% 
#   add_surface(colorscale ="YlGnBu", contours = list(z = list(project=list(z=TRUE), show=TRUE, usecolormap=TRUE, start = 0, end = 1, size = max(dens3Ddouble$z)/20)))
# #Blackbody, Cividis, Electric, Hot, Jet, Portland, RdBu, Viridis, YlGnBu, YlOrRd
# graphe3D_sansdispersion <- plot_ly(x=dens3Dsimple$x, y=dens3Dsimple$y, z=dens3Dsimple$z) %>% 
#   add_surface(colorscale ="Viridis", contours = list(z = list(project=list(z=TRUE), show=TRUE, usecolormap=TRUE, start = 0, end = 1, size = max(dens3Dsimple$z)/20)))
# 
# nuage3D_avecdispersion <- plot_ly(x=Champi_demo$Chapeau.Diametre, y=Champi_demo$Pied.Hauteur, z=Champi_demo$Pied.Largeur, marker = list(size=1)) %>% 
#   add_markers() %>% 
#   layout(scene = list(xaxis = list(title = "Dc"), yaxis = list(title = "Ls"), zaxis = list(title = "Ds")))
# nuage3D_sansdispersion <- plot_ly(x=Champi_demo$Chapeau.Diametre, y=Champi_demo$Chapeau.Diametre, z=Champi_demo$Chapeau.Diametre, marker = list(size=1)) %>% 
#   add_markers() %>% 
#   layout(scene = list(xaxis = list(title = "Dc"), yaxis = list(title = "Ls"), zaxis = list(title = "Ds")))

############################
#     HYPERCUBES LATINS    #
############################

LHS <- lhsDesign(n = 17, dimension = 2, randomized=FALSE, seed=11)$design
opti_LHS <- maximinESE_LHS(LHS)$design
NOHLD <- nolhDesign(dimension =2, range = c(0, 1))$design
LHS <- data.frame(LHS)
opti_LHS <- data.frame(opti_LHS)
NOHLD <- data.frame(NOHLD)

colnames(LHS) <- c("X1", "X2")
colnames(opti_LHS) <- c("X1", "X2")
colnames(NOHLD) <- c("X1", "X2")

graphe_LHS <- ggplot(data = LHS, aes(x = X1, y = X2)) +
   geom_point(shape = 20, size = 5) + 
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5)) +
   ylab(NULL)

graphe_optiLHS <- ggplot(data = opti_LHS, aes(x = X1, y = X2)) +
   geom_point(shape = 20, size = 5) + 
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5)) +
   ylab(NULL)

graphe_NOHLD <- ggplot(data = NOHLD, aes(x = X1, y = X2)) +
   geom_point(shape = 20, size = 5) +
   theme_bw() +
   theme(axis.text.y = element_text(angle=90, vjust=.5, hjust=.5)) +
   ylab(NULL)


##########################
#     DONNEES FINALES    #
##########################

# Lois de distribution (temps et graphique de distribution)
distrib_binomiale
distrib_uniforme
distrib_normale
distrib_beta
distrib_poisson
distrib_weibull

# Hypercubes latins
graphe_LHS
graphe_optiLHS
graphe_NOHLD

#temps_fonctions
chrono_distrib

# Distribution Champis
lois_beta                  # Profil des lois beta selon facteur de croissance
nuage_sansdispersion       # Nuage de points des tailles/diamètres (sans dispersion)
nuage_avecdispersion       # Nuage de points des tailles/diamètres
densite2d                  # Graphique de densité 2D des tailles/diamètres

#graphe3D_avecdispersion      # Graphique de densité 3D des tailles/diamètres
#graphe3D_sansdispersion      # Graphique de densité 3D des diamètres/diamètres (sans dispersion)
#nuage3D_avecdispersion           # Nuage de points 3D des tailles/diamètres
#nuage3D_sansdispersion           # Nuage de points 3D des tailles/diamètres (sans dispersion)
# orca() pour export en image ??????

distrib_diametre    # Distribution du diamètre
INTRO_taux_gros_diam       # % de diamètres hors-norme (> 100% max)
INTRO_taux_supergros_diam    # % de diamètres super-hors-norme (>110% max), arrondi par excès au 0.1%

# Nettoyage données et sauvegarde

save.image(file = "EKR-Champis-Intro.RData")
rm(valeurs_beta, valeurs_binomiale, valeurs_normale, valeurs_poisson, valeurs_uniforme, valeurs_weibull,
  temps_beta, temps_binomiale, temps_normale, temps_poisson, temps_uniforme, temps_weibull,
  fonction_beta, fonction_binomiale, fonction_normale, fonction_poisson, fonction_uniforme, fonction_weibull,
  beta, loi_beta, chrono_fonctions, Champi_demo)

save.image(file = "EKR-Champis-Intro-Light.RData")
load("EKR-Champis-Intro.RData")
