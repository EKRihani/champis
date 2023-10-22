rm(list = ls(all.names = TRUE))
gc()
source(file = "champis-analysemulti_fam.R", echo = TRUE) # Environ 3h de calcul
rm(list = ls(all.names = TRUE))
gc()
source(file = "champis-analysemulti_esp.R", echo = TRUE) # Environ 6h de calcul
rm(list = ls(all.names = TRUE))
gc()
source(file = "champis-analysenaif.R", echo = TRUE) # Environ ??? de calcul
rm(list = ls(all.names = TRUE))
gc()
source(file = "Champis-Iris.R", echo = TRUE) # Environ ??? de calcul
rm(list = ls(all.names = TRUE))
gc()

Sys.time()
#début 19:10