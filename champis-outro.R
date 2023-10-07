##########################
#     INITIALISATION     #
##########################

# Chargement des bibliothèques
library(tidyverse)    # Outils génériques
library(droll)    # Simulation dés

#Lois de distribution
OUT_pas <- 0.01

OUT_normale <- data.frame(val = seq(from = -4, to = 4, by = OUT_pas))
OUT_normale <- OUT_normale %>%
   mutate(y1 = dnorm(val, mean = 0, sd = 1)) %>%
   mutate(y2 = dnorm(val, mean = 2, sd = 1)) %>%
   mutate(y3 = dnorm(val, mean = 0, sd = 1.5)) %>%
   pivot_longer(cols=c("y1","y2","y3"))

OUT_graphe_normale <- ggplot(data = OUT_normale, aes(x = val, y = value, colour = name)) +
   geom_smooth(method = "loess", se = FALSE, span = 5*OUT_pas, n=1e3) +
   scale_color_viridis_d(option = "C", direction = -1, begin = 0, end = .85) +
   xlab("Valeur") +
   ylab("Densité") +
   theme_bw() +
   theme(axis.text.y = element_blank(),
         panel.grid = element_blank(),
         axis.ticks.y = element_blank(),
         legend.position = "none")

OUT_weibull <- data.frame(val = seq(from = 0, to = 4, by = 0.5*OUT_pas))
OUT_weibull <- OUT_weibull %>%
   mutate(y1 = dweibull(val, shape = 1)) %>%
   mutate(y2 = dweibull(val, shape = 1.5)) %>%
   mutate(y3 = dweibull(val, shape = 2)) %>%
   mutate(y4 = dweibull(val, shape = 3)) %>%
   pivot_longer(cols=c("y1","y2","y3","y4"))

OUT_graphe_weibull <- ggplot(data = OUT_weibull, aes(x = val, y = value, colour = name)) +
   geom_smooth(method = "loess", se = FALSE, span = 2*OUT_pas, n=1e3) +
   scale_color_viridis_d(option = "C", direction = -1, begin = 0, end = .85) +
   xlab("Valeur") +
   ylab("Densité") +
   theme_bw() +
   theme(axis.text.y = element_blank(),
         panel.grid = element_blank(),
         axis.ticks.y = element_blank(),
         legend.position = "none")

OUT_beta <- data.frame(val = seq(from = 0, to = 1, by = 0.5*OUT_pas))
OUT_beta <- OUT_beta %>%
   mutate(y1 = dbeta(val, shape1 = 2, shape2 = 1.6, ncp = 5)) %>%
   mutate(y2 = dbeta(val, shape1 = 2, shape2 = 1, ncp = 4)) %>%
   mutate(y3 = dbeta(val, shape1 = .55, shape2 = .58, ncp = 0)) %>%
   mutate(y4 = dbeta(val, shape1 = .5, shape2 = 8, ncp = 5)) %>%
   mutate(y5 = dbeta(val, shape1 = 1, shape2 = 5, ncp = 10)) %>%
   pivot_longer(cols=c("y1","y2","y3","y4","y5"))

OUT_graphe_beta <- ggplot(data = OUT_beta, aes(x = val, y = value, colour = name)) +
   geom_smooth(method = "loess", se = FALSE, span = 2.1*OUT_pas, n=1e3) +
   scale_color_viridis_d(option = "C", direction = -1, begin = 0, end = .85) +
   xlab("Valeur") +
   ylab("Densité") +
   theme_bw() +
   theme(axis.text.y = element_blank(),
         panel.grid = element_blank(),
         axis.ticks.y = element_blank(),
         legend.position = "none")

OUT_n_bino <- 30
OUT_p_bino <- 0.5
OUT_binomiale <- data.frame(val =  1:OUT_n_bino)
OUT_binomiale <- OUT_binomiale %>%
   mutate(y1 = dbinom(val, size = OUT_n_bino, prob = 0.3*OUT_p_bino)) %>%
   mutate(y2 = dbinom(val, size = OUT_n_bino, prob = OUT_p_bino)) %>%
   mutate(y3 = dbinom(val, size = OUT_n_bino, prob = 1.8*OUT_p_bino)) %>%
   pivot_longer(cols=c("y1", "y2", "y3"))

OUT_graphe_binomiale <- ggplot(data = OUT_binomiale, aes(x = val, y = value, colour = name)) +
   geom_point() + geom_segment(aes(xend=val, yend=0)) +
   scale_color_viridis_d(option = "C", direction = -1, begin = 0, end = .85) +
   xlab("Valeur") +
   ylab("Densité") +
   theme_bw() +
   theme(axis.text.y = element_blank(),
         panel.grid = element_blank(),
         axis.ticks.y = element_blank(),
         legend.position = "none")

OUT_bimodale <- data.frame(val = seq(from = -5, to = 5, by = OUT_pas))
OUT_bimodale <- OUT_bimodale %>%
   mutate(y1 = dnorm(val, mean = -3, sd = 1) + dnorm(val, mean = 3, sd = .7)) %>%
   mutate(y2 = dnorm(val, mean = 0, sd = 0.8)) %>%
   pivot_longer(cols=c("y1", "y2"))
OUT_bimodaleMoy <- 0

OUT_graphe_bimodale <- ggplot(data = OUT_bimodale, aes(x = val, y = value, color = name)) +
   geom_smooth(method = "loess", se = FALSE, span = 5*OUT_pas, n=1e3) +
   scale_color_viridis_d(option = "C", direction = -1, begin = 0, end = .75) +
   xlab("Valeur") + ylab("Densité") +
   geom_vline(xintercept = OUT_bimodaleMoy, color = "red", linetype = "dashed") +
   theme_bw() +
   theme(axis.text.y = element_blank(),
         panel.grid = element_blank(),
         axis.ticks.y = element_blank(),
         legend.position = "none")

nombre <- 1e3
OUT_bimodale2D <- data.frame(
   x1 = c(rnorm(n = nombre, mean = -3, sd = 1)[1:nombre], 
         rnorm(n = nombre, mean = 3, sd = 0.7)[1:nombre]),
   x2 = rnorm(n = 2*nombre, mean = 0, sd = 0.8),
   y = rnorm(n = 2*nombre, mean = 0, sd = 2)) %>%
   pivot_longer(cols=c("x1", "x2"))

OUT_graphe_bimodale2D <- ggplot(data = OUT_bimodale2D, aes(x = value, y = y, color = name)) +
   geom_point(alpha = .3) +
   xlab("X1") + ylab("X2") + labs(colour = NULL) +
   scale_color_viridis_d(option = "C", direction = -1, begin = 0, end = .75) +
   geom_vline(xintercept = OUT_bimodaleMoy, color = "red", linetype = "dashed") +
   theme_bw() +
   theme(axis.text.y = element_blank(),
         panel.grid = element_blank(),
         axis.ticks.y = element_blank(),
         legend.position = "none")

OUT_monomodale <- data.frame(val = seq(from = -5, to = 3, by = OUT_pas))
OUT_monomodale <- OUT_monomodale %>%
   mutate(y1 = dnorm(val, mean = -2, sd = 1)) %>%
   mutate(y2 = dnorm(val, mean = 0, sd = 0.8)) %>%
   pivot_longer(cols=c("y1", "y2"))

OUT_graphe_monomodale <- ggplot(data = OUT_monomodale, aes(x = val, y = value, color = name)) +
   geom_smooth(method = "loess", se = FALSE, span = 5*OUT_pas, n=1e3) +
   scale_color_viridis_d(option = "D", direction = -1, begin = 0, end = .75) +
   xlab("Valeur") + ylab("Densité") +
   geom_vline(xintercept = OUT_monomodaleMoy, color = "red", linetype = "dashed") +
   theme_bw() +
   theme(axis.text.y = element_blank(),
         panel.grid = element_blank(),
         axis.ticks.y = element_blank(),
         legend.position = "none")

OUT_monomodale2D <- data.frame(
   x1 = c(rnorm(n = nombre, mean = -2, sd = 1)),
   x2 = rnorm(n = nombre, mean = 0, sd = 0.8),
   y = rnorm(n = nombre, mean = 0, sd = 2)) %>%
   pivot_longer(cols=c("x1", "x2"))
OUT_monomodaleMoy <- -1

OUT_graphe_monomodale2D <- ggplot(data = OUT_monomodale2D, aes(x = value, y = y, color = name)) +
   geom_point(alpha = .3) +
   xlab("X1") + ylab("X2") +
   geom_vline(xintercept = OUT_monomodaleMoy, color = "red", linetype = "dashed") +
   scale_color_viridis_d(option = "D", direction = -1, begin = 0, end = .75) +
   theme_bw() +
   theme(axis.text.y = element_blank(),
         panel.grid = element_blank(),
         axis.ticks.y = element_blank(),
         legend.position = "none")

OUT_graphe_uniforme <- ggplot(data = OUT_monomodale, aes(x = val, y = 1)) +
   geom_smooth(method = "loess", se = FALSE, span = 5*OUT_pas, n=1e3) +
   scale_color_viridis_d(option = "D", direction = -1, begin = 0, end = .75) +
   xlab("Valeur") + ylab("Densité") +
   theme_bw() +
   theme(axis.text.y = element_blank(),
         panel.grid = element_blank(),
         axis.ticks.y = element_blank(),
         legend.position = "none")


OUT_n_bino <- 30
OUT_p_bino <- 0.5
OUT_binomiale <- data.frame(val =  1:OUT_n_bino)
OUT_binomiale <- OUT_binomiale %>%
   mutate(y1 = dbinom(val, size = OUT_n_bino, prob = 0.3*OUT_p_bino)) %>%
   mutate(y2 = dbinom(val, size = OUT_n_bino, prob = OUT_p_bino)) %>%
   mutate(y3 = dbinom(val, size = OUT_n_bino, prob = 1.8*OUT_p_bino)) %>%
   pivot_longer(cols=c("y1", "y2", "y3"))

des <- data.frame(score =  1:24)
des <- des %>%
   mutate(X6D4 = c(rep(0,5), droll(x = score, roll = 6*d4))) %>%
   mutate(X4D6 = c(rep(0,3), droll(x = score, roll = 4*d6))) %>%
   mutate(X3D8 = c(rep(0,2), droll(x = score, roll = 3*d8))) %>%
   mutate(X2D12 = c(0, droll(x = score, roll = 2*d12))) %>%
   mutate(X1D20 = c(droll(x = score, roll = 1*d20), rep(0,4))) %>%
   pivot_longer(cols=c("X6D4", "X4D6", "X3D8", "X2D12", "X1D20"))

OUT_graphe_des <- ggplot(data = des, aes(x = score, y = value, color = name)) +
   geom_point() + 
   geom_smooth (linetype = "dotted", linewidth=0.3, method = "loess", se = FALSE, span = 0.2, n=1e3) +
   scale_color_viridis_d(option = "C", direction = -1, begin = 0, end = .7, name = "Dés",
                         labels = c("1D20", "2D12", "3D8", "4D6", "6D4")) +
   xlab("Valeur") + ylab("Probabilité") +
   theme_bw() +
   theme(panel.grid = element_blank())


save.image(file = "EKR-Champis-Outro.RData")     # Sauvegarde données pour rapport
load(file = "EKR-Champis-Outro.RData")


OUT_graphe_uniforme #
OUT_graphe_normale #
OUT_graphe_beta #
OUT_graphe_binomiale #
OUT_graphe_weibull #
OUT_graphe_monomodale
OUT_graphe_monomodale2D
OUT_graphe_bimodale
OUT_graphe_bimodale2D
OUT_graphe_des

