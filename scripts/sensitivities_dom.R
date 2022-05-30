#load("~/Git/NiwotITEX_gjam/outputs/modDAtime_XXXoutput_dom.RData")
#load("~/Git/NiwotITEX_gjam/outputs/modDAtime_XXWoutput_dom.RData")
#load("~/Git/NiwotITEX_gjam/outputs/modDAtime_XNWoutput_dom.RData")
#load("~/Git/NiwotITEX_gjam/outputs/modDAtime_PXWoutput_dom.RData")
#load("~/Git/NiwotITEX_gjam/outputs/modDAtime_PNWoutput_dom.RData")

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

#Table 2
#write.csv(eigs, "tables/eigenvalues.csv")
