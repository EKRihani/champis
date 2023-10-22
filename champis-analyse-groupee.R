rm(list = ls(all.names = TRUE))
gc()
source(file = "champis-analysemulti_fam.R") # Environ 3h de calcul
rm(list = ls(all.names = TRUE))
gc()
source(file = "champis-analysemulti_esp.R", echo = TRUE, verbose = TRUE) # Environ 5h30 de calcul
rm(list = ls(all.names = TRUE))
gc()
source(file = "champis-analysenaif.R", echo = TRUE, verbose = TRUE) # Environ ??? de calcul
#rm(list = ls(all.names = TRUE))
#gc()
source(file = "Champis-Iris.R", echo = TRUE, verbose = TRUE) # Environ ??? de calcul
#rm(list = ls(all.names = TRUE))
#gc()

Sys.time()
#début 12:50