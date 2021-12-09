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
rhosens$group<-row.names(rhosens)

asens<-rbind(asens, asens1, asens2, asens3, asens4)
asens<-rename(asens, alpha_est=Estimate, alpha_se=SE)
asens$group<-row.names(asens)

sens_table<-left_join(asens, rhosens)%>%select(-rho_se, -alpha_se)%>%select(group, treat, rho_est, alpha_est)
#write.csv(sens_table, "tables/table2.csv")


#Stabilty

eigxxx<-modDAtimeXXX$parameters$alphaEigen%>%as.data.frame()
eigxxw<-modDAtimeXXW$parameters$alphaEigen %>%as.data.frame()
eigxnw<-modDAtimeXNW$parameters$alphaEigen %>%as.data.frame()
eigpxw<-modDAtimePXW$parameters$alphaEigen %>%as.data.frame()
eigpnw<-modDAtimePNW$parameters$alphaEigen %>%as.data.frame()

eigxxx$treat<-"XXX"
eigxxw$treat<-"XXW"
eigxnw$treat<-"XNW"
eigpxw$treat<-"PXW"
eigpnw$treat<-"PNW"


eigs<-rbind(eigxxx, eigxxw, eigxnw, eigpxw, eigpnw)
eigs<-separate(eigs, col = ., into = c("real", 'imaginary'), sep =  "([+i])")

write.csv(eigs, "tables/eigenvalues.csv")
