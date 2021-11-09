load("~/Git/NiwotITEX_gjam/outputs/modDAtime_XXXoutput_dom.RData")
load("~/Git/NiwotITEX_gjam/outputs/modDAtime_XXWoutput_dom.RData")
load("~/Git/NiwotITEX_gjam/outputs/modDAtime_XNWoutput_dom.RData")
load("~/Git/NiwotITEX_gjam/outputs/modDAtime_PXWoutput_dom.RData")
load("~/Git/NiwotITEX_gjam/outputs/modDAtime_PNWoutput_dom.RData")

rhosens<-as.data.frame(modDAtimeXXX$parameters$sensRho) 
rhosens$treat<-"XXX"
asens<-as.data.frame(modDAtimeXXX$parameters$sensAlpha)
asens$treat<-'XXX'

rhosens1<-as.data.frame(modDAtimeXXW$parameters$sensRho) 
rhosens1$treat<-"XXW"
asens1<-as.data.frame(modDAtimeXXW$parameters$sensAlpha)
asens1$treat<-'XXW'

rhosens2<-as.data.frame(modDAtimeXNW$parameters$sensRho) 
rhosens2$treat<-"XNW"
asens2<-as.data.frame(modDAtimeXNW$parameters$sensAlpha)
asens2$treat<-'XNW'

rhosens3<-as.data.frame(modDAtimePXW$parameters$sensRho) 
rhosens3$treat<-"PXW"
asens3<-as.data.frame(modDAtimePXW$parameters$sensAlpha)
asens3$treat<-'PXW'

rhosens4<-as.data.frame(modDAtimePNW$parameters$sensRho) 
rhosens4$treat<-"PNW"
asens4<-as.data.frame(modDAtimePNW$parameters$sensAlpha)
asens4$treat<-'PNW'

rhosens<-rbind(rhosens, rhosens1, rhosens2, rhosens3, rhosens4)
rhosens<-rename(rhosens, rho_est=Estimate, rho_se=SE)
rhosens$class<-row.names(rhosensx)

asens<-rbind(asens, asens1, asens2, asens3, asens4)
asens<-rename(asens, alpha_est=Estimate, alpha_se=SE)
asens$class<-row.names(asens)

sens_table<-left_join(asens, rhosens)


modDAtimePNW$parameters$sensRho


