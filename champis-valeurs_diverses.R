####################################################
#     VALEURS DIVERSES UTILISEES DANS LA THESE     #
####################################################


# Index de Jouden pondéré
Jw_ratio <- 10    # Importance relative sensibilité/spécificité
Jw_frac <- Jw_ratio/(Jw_ratio+1)
Jw_min <- 0.999    # Index minimum à atteindre
Jw_Sens_min <- round(((Jw_min+1)/2 +Jw_frac-1)/(Jw_frac),5)      # Sensibilité minimale pour Spec = 1
Jw_Spec_min <- round(((Jw_min+1)/2 -Jw_frac)/(1-Jw_frac),5)       # Spécificité minimale pour Sens = 1
   
save.image(file = "EKR-Champis-Valeurs.RData")     # Sauvegarde données pour rapport
