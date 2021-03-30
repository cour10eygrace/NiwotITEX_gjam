source("scripts/feasibility/Code_NEE_Tabi et al 2020.R")

load(file = "outputs/modDAtime_earlytrtoutput.RData")
load(file = "outputs/modDAtime_midtrtoutput.RData")
load(file = "outputs/modDAtime_latetrtoutput.RData")
load(file = "outputs/modDAtime_PXXoutput.RData")
load(file = "outputs/modDAtime_snowoutput.RData")
load(file = "outputs/modDAtime_ctlsnowoutput.RData")

#snow
alphasnow<-modDAtime_snow$parameters$alphaMu
Omega(alphasnow)

alphactlsnow<-modDAtime_ctlsnow$parameters$alphaMu
Omega(alphactlsnow)

alphaPXX<-modDAtimePXX$parameters$alphaMu
Omega(alphaPXX)

#XXX
alphaXXX<-modDAtimeXXX$parameters$alphaMu
KXXX<-parameterization_center(alphaXXX,9)
Omega(alphaXXX)
