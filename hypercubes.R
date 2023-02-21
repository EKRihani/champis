library(tidyverse)
library(lhs)

geneticLHS(n = 10, k = 3, pop = 50, gen = 2, criterium = "Maximin")


library(DiceDesign)
LHS <- lhsDesign(n = 15, dimension = 3, randomized=TRUE, seed=13)$design
opti_LHS <- maximinESE_LHS(LHS)$design
plot(opti_LHS)


LIN <- function(x, y, z){30*x+ 25*y - 12*z}
BILIN <- function(x, y, z){30*x+ 25*y - 12*z + 17*x*y - 3*x*z + 8*y*z}
QUAD <- function(x, y, z){30*x+ 25*y - 12*z + 7*x^2 + 15*y^2 - 5*z^2}
BIQUAD <- function(x, y, z){30*x+ 25*y - 12*z + 17*x*y - 3*x*z + 8*y*z + 7*x^2 + 15*y^2 - 5*z^2}
lin <- LIN(opti_LHS[,1], opti_LHS[,2], opti_LHS[,3])
bilin <- BILIN(opti_LHS[,1], opti_LHS[,2], opti_LHS[,3])
quad <- QUAD(opti_LHS[,1], opti_LHS[,2], opti_LHS[,3])
biquad <- BIQUAD(opti_LHS[,1], opti_LHS[,2], opti_LHS[,3])

library(DiceEval)
modelComparison(X=opti_LHS,Y=quad,type="all", formula=Y~X1+X2+X1:X2+I(X1^2)+I(X2^2))

#modelComparison(X=opti_LHS,Y=quad,type="all", formula=Y~ poly(X1,2)+poly(X2,2)+X1:X2) # A TESTER

Modele_Mars <- modelFit(X=opti_LHS, Y=biquad, type="MARS", degree=4)
Modele_PolyMars <- modelFit(X=opti_LHS, Y=biquad, type="PolyMARS", gcv=4)
Modele_Kriging <- modelFit(X=opti_LHS, Y=biquad, type="Kriging", formula=Y~X1+X2+X3+X1:X2+X1:X3+X2:X3+I(X1^2)+I(X2^2)+I(X3^2))
#Modele_Kriging <- modelFit(X=opti_LHS, Y=biquad, type="Kriging", formula=Y~poly(X1,2)+poly(X2,2)+poly(X3,2)+X1:X2+X1:X3+X2:X3) # A TESTER

Test <- BIQUAD(LHS[,1], LHS[,2], LHS[,3])
Pred_Mars <- modelPredict(Modele_Mars, LHS)
Pred_PolyMars <- modelPredict(Modele_PolyMars, LHS)
Pred_Kriging <- modelPredict(Modele_Kriging, LHS)


library(DiceKriging)
KM <- km(design=opti_LHS, response=biquad)
predict(object=KM, newdata=LHS, type="UK")


data.frame(Test, Pred_Mars, Pred_PolyMars, Pred_Kriging)
