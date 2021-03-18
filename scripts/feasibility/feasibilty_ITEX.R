source("scripts/feasibility/Code_NEE_Tabi et al 2020.R")

load(file = "outputs/modDAtime_earlytrtoutput.RData")
load(file = "outputs/modDAtime_midtrtoutput.RData")
load(file = "outputs/modDAtime_latetrtoutput.RData")
load(file = "outputs/modDAtime_XXXoutput.RData")

#XXX
alphaXXX<-modDAtimeXXX$parameters$alphaMu
KXXX<-parameterization_center(alphaXXX,9)
