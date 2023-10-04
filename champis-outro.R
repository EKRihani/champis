##########################
#     INITIALISATION     #
##########################

# Chargement des bibliothèques
library(tidyverse)    # Outils génériques

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
         axis.ticks.y = element_blank())

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
         axis.ticks.y = element_blank())

OUT_beta <- data.frame(val = seq(from = 0, to = 1, by = 0.5*OUT_pas))
OUT_beta <- OUT_beta %>%
   mutate(y1 = dbeta(val, shape1 = 2, shape2 = 1.6, ncp = 5)) %>%
   mutate(y2 = dbeta(val, shape1 = 2, shape2 = 1, ncp = 4)) %>%
   mutate(y3 = dbeta(val, shape1 = .55, shape2 = .58, ncp = 0)) %>%
   mutate(y4 = dbeta(val, shape1 = .5, shape2 = 8, ncp = 5)) %>%
   mutate(y5 = dbeta(val, shape1 = 1, shape2 = 5, ncp = 10)) %>%
   pivot_longer(cols=c("y1","y2","y3","y4","y5"))

OUT_graphe_beta <- ggplot(data = OUT_beta, aes(x = val, y = value, colour = name)) +
   geom_smooth(method = "loess", se = FALSE, span = 2*OUT_pas, n=1e3) +
   scale_color_viridis_d(option = "C", direction = -1, begin = 0, end = .85) +
   xlab("Valeur") +
   ylab("Densité") +
   theme_bw() +
   theme(axis.text.y = element_blank(),
         panel.grid = element_blank(),
         axis.ticks.y = element_blank())

OUT_n_bino <- 30
OUT_p_bino <- 0.5
OUT_binomiale <- data.frame(val =  1:OUT_n_bino)
OUT_binomiale <- OUT_binomiale %>%
   mutate(y1 = dbinom(val, size = OUT_n_bino, prob = OUT_p_bino)) %>%
   mutate(y2 = dnorm(val, mean = OUT_n_bino*OUT_p_bino, sd = sqrt(OUT_n_bino*OUT_p_bino*(1-OUT_p_bino)))) %>%
   pivot_longer(cols=c("y1", "y2"))

OUT_graphe_binomiale <- ggplot(data = OUT_binomiale, aes(x = val, y = value, colour = name)) +
   geom_point() + geom_segment(aes(xend=val, yend=0)) +
   scale_color_viridis_d(option = "C", direction = -1, begin = 0, end = .85) +
   xlab("Valeur") +
   ylab("Densité") +
   theme_bw() +
   theme(axis.text.y = element_blank(),
         panel.grid = element_blank(),
         axis.ticks.y = element_blank())


OUT_bimodale <- data.frame(val = seq(from = -5, to = 5, by = OUT_pas))
OUT_bimodale <- OUT_bimodale %>%
   mutate(y = dnorm(val, mean = -3, sd = 1) + dnorm(val, mean = 3, sd = .7)) %>%
   mutate(x = dnorm(val, mean = 3, sd = 1))

OUT_graphe_bimodale <- ggplot(data = OUT_bimodale, aes(x = val, y = y)) +
   geom_smooth(method = "loess", se = FALSE, span = 5*OUT_pas, n=1e3) +
   scale_color_viridis_d(option = "C", direction = -1, begin = 0, end = .85) +
   xlab("Valeur") +
   ylab("Densité") +
   theme_bw() +
   theme(axis.text.y = element_blank(),
         panel.grid = element_blank(),
         axis.ticks.y = element_blank())


nombre <- 1e3
OUT_bimodale2D <- data.frame(
   x = c(rnorm(n = nombre, mean = -3, sd = 1)[1:nombre], 
         rnorm(n = nombre, mean = 3, sd = 0.7)[1:nombre]),
   y = rnorm(n = 2*nombre, mean = 0, sd = 2))

OUT_graphe_bimodale2D <- ggplot(data = OUT_bimodale2D, aes(x = x, y = y)) +
   geom_point(alpha = .5) +
   xlab("X1") +
   ylab("X2") +
   theme_bw() +
   theme(axis.text.y = element_blank(),
         panel.grid = element_blank(),
         axis.ticks.y = element_blank())


save.image(file = "EKR-Champis-Outro.RData")     # Sauvegarde données pour rapport


OUT_graphe_beta
OUT_graphe_binomiale
OUT_graphe_weibull
OUT_graphe_normale
OUT_graphe_bimodale
OUT_graphe_bimodale2D
