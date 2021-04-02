library(bayestestR)
library(dplyr)
library(tidyr)
load(file = "outputs/wstar_XXXoutput.RData")

wstarmu<-as.data.frame(wstarXXX$ccMu)%>%
  pivot_longer(. , cols=everything(),names_to="spp", 
                values_to="mu")
wstarsd<-as.data.frame(wstarXXX$ccSd)%>%
  pivot_longer(. , cols=everything(),names_to="spp", 
               values_to="sd")
env<-as.data.frame(wstarXXX$x)


wstarXXX<-cbind(wstarmu, select(wstarsd, -spp))
artsco<-subset(wstarXXX, spp=="ARTSCO")
artsco<-cbind(artsco, env)

geum<-subset(wstarXXX, spp=="GEUROS")
geum<-cbind(geum, env)
cor.test(artsco$mu, artsco$depthcm, method = "spearman")
pivot_longer(. , cols=everything(),names_to="spp", 
               values_to="mu")
plot(geum$mu~geum$depthcm)
cor.test(geum$mu, geum$depthcm, method = "spearman")
library(wdm)
indep_test(geum$depthcm, geum$mu, method = "kendall")


library(mgcv)
summary(gam(geum$mu ~ geum$depthcm))
summary(gam(geum$mu ~ geum$avgT))
library(nlcor)

nlcor(geum$mu, geum$depthcm)
nlcor(geum$mu, geum$Ndep)
nlcor(geum$mu, geum$avgT)


CIXXX<-sapply(wstarXXX,ci,ci=0.95)%>%rbind.data.frame(.)
CIXXX<-as.data.frame(t(CIXXX))
CIXXX$spp<-row.names(CIXXX)

test <- matrix(ncol=3, nrow=ncol(wstarXXX))

test[1,]<-ci(wstarXXX[,1], 0.95)

test[2,]<-ci(wstarXXX[,2], 0.95)
