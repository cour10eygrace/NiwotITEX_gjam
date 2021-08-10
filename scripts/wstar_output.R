library(bayestestR)
library(dplyr)
library(tidyr)
library(brms)
library(ggplot2)

#XXX----
load(file = "outputs/wstar_XXXoutput.RData")

#pull out of lists 
wstarmu<-as.data.frame(wstarXXX$ccMu)%>%
  pivot_longer(. , cols=everything(),names_to="spp", 
                values_to="mu")
wstarsd<-as.data.frame(wstarXXX$ccSd)%>%
  pivot_longer(. , cols=everything(),names_to="spp", 
               values_to="sd")

env<-as.data.frame(wstarXXX$x)

#combine into one df
wstarxxx<-cbind(wstarmu, select(wstarsd, -spp))
wstarxxx<-arrange(wstarxxx, spp)#rearrange by spp
wstarxxx<-cbind(wstarxxx, env)

#plots
#plot raw data-don't know how to get sds in there 
ggplot(wstarxxx, aes(x=depthcm, y=mu, col=spp)) +geom_smooth()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
 facet_wrap(~spp, scales ="free")

ggplot(wstarxxx, aes(x=as.factor(depthcm), y=mu, col=spp)) +geom_boxplot()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")+ theme_classic()

ggplot(wstarxxx, aes(x=Ndep, y=mu, col=spp)) +geom_smooth()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")

ggplot(wstarxxx, aes(x=as.factor(Ndep), y=mu, col=spp)) +geom_boxplot()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")

ggplot(wstarxxx, aes(x=avgT, y=mu, col=spp)) +geom_smooth()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")

ggplot(wstarxxx, aes(x=as.factor(avgT), y=mu, col=spp)) +geom_boxplot()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")

#subset by spp 
artsco=subset(wstarxxx, spp=="ARTSCO")
desc=subset(wstarxxx, spp=="DESCAE")
geum=subset(wstarxxx, spp=="GEUROS")
tripar=subset(wstarxxx, spp=="TRIPAR")
genalg=subset(wstarxxx, spp=="GENALG")
rarespp=subset(wstarxxx, spp=="RARESPP")
carsco=subset(wstarxxx, spp=="CARSCO")
bisbis=subset(wstarxxx, spp=="BISBIS")
callep=subset(wstarxxx, spp=="CALLEP")

#snow
modsnowNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(depthcm))

fit_modsnowxxx_geumNL<- brm(modsnowNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxx_artscoNL<- brm(modsnowNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modsnowxxx_descNL<- brm(modsnowNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxx_genalgNL<- brm(modsnowNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxx_triparNL<- brm(modsnowNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxx_raresppNL<- brm(modsnowNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxx_callepNL<- brm(modsnowNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxx_carscoNL<- brm(modsnowNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxx_bisbisNL<- brm(modsnowNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save moodels 
save( fit_modsnowxxx_artscoNL,
      fit_modsnowxxx_bisbisNL, 
      fit_modsnowxxx_callepNL, 
      fit_modsnowxxx_carscoNL, 
      fit_modsnowxxx_descNL, 
      fit_modsnowxxx_genalgNL, 
      fit_modsnowxxx_geumNL, 
      fit_modsnowxxx_raresppNL, 
      fit_modsnowxxx_triparNL, 
      file = "outputs/wstarNL/mod_wstar_snowxxx.RData")

#model summary
summary(fit_modsnowxxx_geumNL)#significant smoothing term 

#temp
modtempNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(avgT))

fit_modtempxxx_geumNL<- brm(modtempNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxx_artscoNL<- brm(modtempNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=10000, family=gaussian())
fit_modtempxxx_descNL<- brm(modtempNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxx_genalgNL<- brm(modtempNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxx_triparNL<- brm(modtempNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxx_raresppNL<- brm(modtempNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxx_callepNL<- brm(modtempNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxx_carscoNL<- brm(modtempNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxx_bisbisNL<- brm(modtempNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save(fit_modtempxxx_artscoNL,
     fit_modtempxxx_bisbisNL, 
     fit_modtempxxx_callepNL, 
     fit_modtempxxx_carscoNL, 
     fit_modtempxxx_descNL, 
     fit_modtempxxx_genalgNL, 
     fit_modtempxxx_geumNL, 
     fit_modtempxxx_raresppNL, 
     fit_modtempxxx_triparNL, 
     file = "outputs/wstarNL/mod_wstar_tempxxx.RData")

#Ndep
modNdepNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(Ndep))

fit_modNdepxxx_geumNL<- brm(modNdepNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxx_artscoNL<- brm(modNdepNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modNdepxxx_descNL<- brm(modNdepNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxx_genalgNL<- brm(modNdepNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxx_triparNL<- brm(modNdepNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxx_raresppNL<- brm(modNdepNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxx_callepNL<- brm(modNdepNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxx_carscoNL<- brm(modNdepNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxx_bisbisNL<- brm(modNdepNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save(fit_modNdepxxx_artscoNL,
     fit_modNdepxxx_bisbisNL, 
     fit_modNdepxxx_callepNL, 
     fit_modNdepxxx_carscoNL, 
     fit_modNdepxxx_descNL, 
     fit_modNdepxxx_genalgNL, 
     fit_modNdepxxx_geumNL, 
     fit_modNdepxxx_raresppNL, 
     fit_modNdepxxx_triparNL, 
     file = "outputs/wstarNL/mod_wstar_Ndepxxx.RData")

#XXW----
load(file = "outputs/wstar_XXWoutput.RData")

#pull out of lists 
wstarmu<-as.data.frame(wstarXXW$ccMu)%>%
  pivot_longer(. , cols=everything(),names_to="spp", 
               values_to="mu")
wstarsd<-as.data.frame(wstarXXW$ccSd)%>%
  pivot_longer(. , cols=everything(),names_to="spp", 
               values_to="sd")
env<-as.data.frame(wstarXXW$x)

#combine into one df
wstarxxw<-cbind(wstarmu, select(wstarsd, -spp))
wstarxxw<-arrange(wstarxxw, spp)#rearrange by spp
wstarxxw<-cbind(wstarxxw, env)

#plots
#plot raw data-don't know how to get sds in there 
ggplot(wstarxxw, aes(x=depthcm, y=mu, col=spp)) +geom_smooth()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")

ggplot(wstarxxw, aes(x=Ndep, y=mu, col=spp)) +geom_smooth()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")

ggplot(wstarxxw, aes(x=avgT, y=mu, col=spp)) +geom_smooth()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")

#fit mods 
library(mgcv)
#subset by spp 
artsco=subset(wstarxxw, spp=="ARTSCO")
desc=subset(wstarxxw, spp=="DESCAE")
geum=subset(wstarxxw, spp=="GEUROS")
tripar=subset(wstarxxw, spp=="TRIPAR")
genalg=subset(wstarxxw, spp=="GENALG")
rarespp=subset(wstarxxw, spp=="RARESPP")
carsco=subset(wstarxxw, spp=="CARSCO")
bisbis=subset(wstarxxw, spp=="BISBIS")
callep=subset(wstarxxw, spp=="CALLEP")

#snow
modsnowNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(depthcm))

fit_modsnowxxw_geumNL<- brm(modsnowNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxw_artscoNL<- brm(modsnowNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modsnowxxw_descNL<- brm(modsnowNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxw_genalgNL<- brm(modsnowNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxw_triparNL<- brm(modsnowNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxw_raresppNL<- brm(modsnowNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxw_callepNL<- brm(modsnowNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxw_carscoNL<- brm(modsnowNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxw_bisbisNL<- brm(modsnowNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save moodels 
save( fit_modsnowxxw_artscoNL,
      fit_modsnowxxw_bisbisNL, 
      fit_modsnowxxw_callepNL, 
      fit_modsnowxxw_carscoNL, 
      fit_modsnowxxw_descNL, 
      fit_modsnowxxw_genalgNL, 
      fit_modsnowxxw_geumNL, 
      fit_modsnowxxw_raresppNL, 
      fit_modsnowxxw_triparNL, 
      file = "outputs/wstarNL/mod_wstar_snowxxw.RData")


#temp
modtempNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(avgT))

fit_modtempxxw_geumNL<- brm(modtempNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxw_artscoNL<- brm(modtempNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=10000, family=gaussian())
fit_modtempxxw_descNL<- brm(modtempNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxw_genalgNL<- brm(modtempNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxw_triparNL<- brm(modtempNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxw_raresppNL<- brm(modtempNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxw_callepNL<- brm(modtempNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxw_carscoNL<- brm(modtempNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxw_bisbisNL<- brm(modtempNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save( fit_modtempxxw_artscoNL,
      fit_modtempxxw_bisbisNL, 
      fit_modtempxxw_callepNL, 
      fit_modtempxxw_carscoNL, 
      fit_modtempxxw_descNL, 
      fit_modtempxxw_genalgNL, 
      fit_modtempxxw_geumNL, 
      fit_modtempxxw_raresppNL, 
      fit_modtempxxw_triparNL, 
      file = "outputs/wstarNL/mod_wstar_tempxxw.RData")

#model comparisons 
summary(fit_modtempxxw_carscoNL)#significant 

#Ndep
modNdepNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(Ndep))

fit_modNdepxxw_geumNL<- brm(modNdepNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxw_artscoNL<- brm(modNdepNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modNdepxxw_descNL<- brm(modNdepNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxw_genalgNL<- brm(modNdepNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxw_triparNL<- brm(modNdepNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxw_raresppNL<- brm(modNdepNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxw_callepNL<- brm(modNdepNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxw_carscoNL<- brm(modNdepNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxw_bisbisNL<- brm(modNdepNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save( fit_modNdepxxw_artscoNL,
      fit_modNdepxxw_bisbisNL, 
      fit_modNdepxxw_callepNL, 
      fit_modNdepxxw_carscoNL, 
      fit_modNdepxxw_descNL, 
      fit_modNdepxxw_genalgNL, 
      fit_modNdepxxw_geumNL, 
      fit_modNdepxxw_raresppNL, 
      fit_modNdepxxw_triparNL, 
      file = "outputs/wstarNL/mod_wstar_Ndepxxw.RData")

#model comparisons 
summary(fit_modNdepxxw_descNL)#sig


#XNX----
load(file = "outputs/wstar_XNXoutput.RData")

#pull out of lists 
wstarmu<-as.data.frame(wstarXNX$ccMu)%>%
  pivot_longer(. , cols=everything(),names_to="spp", 
               values_to="mu")
wstarsd<-as.data.frame(wstarXNX$ccSd)%>%
  pivot_longer(. , cols=everything(),names_to="spp", 
               values_to="sd")
env<-as.data.frame(wstarXNX$x)

#combine into one df
wstarxnx<-cbind(wstarmu, select(wstarsd, -spp))
wstarxnx<-arrange(wstarxnx, spp)#rearrange by spp
wstarxnx<-cbind(wstarxnx, env)

#plots
#plot raw data-don't know how to get sds in there 
ggplot(wstarxnx, aes(x=depthcm, y=mu, col=spp)) +geom_smooth()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")

ggplot(wstarxnx, aes(x=Ndep, y=mu, col=spp)) +geom_smooth()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")

ggplot(wstarxnx, aes(x=avgT, y=mu, col=spp)) +geom_smooth()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")

#fit mods 
library(mgcv)
#subset by spp 
artsco=subset(wstarxnx, spp=="ARTSCO")
desc=subset(wstarxnx, spp=="DESCAE")
geum=subset(wstarxnx, spp=="GEUROS")
tripar=subset(wstarxnx, spp=="TRIPAR")
genalg=subset(wstarxnx, spp=="GENALG")
rarespp=subset(wstarxnx, spp=="RARESPP")
carsco=subset(wstarxnx, spp=="CARSCO")
bisbis=subset(wstarxnx, spp=="BISBIS")
callep=subset(wstarxnx, spp=="CALLEP")

#snow
modsnowNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(depthcm))

fit_modsnowxnx_geumNL<- brm(modsnowNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnx_artscoNL<- brm(modsnowNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modsnowxnx_descNL<- brm(modsnowNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnx_genalgNL<- brm(modsnowNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnx_triparNL<- brm(modsnowNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnx_raresppNL<- brm(modsnowNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnx_callepNL<- brm(modsnowNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnx_carscoNL<- brm(modsnowNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnx_bisbisNL<- brm(modsnowNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save moodels 
save(fit_modsnowxnx_artscoNL, # fit_modsnowxnx_artscoNL,
     fit_modsnowxnx_bisbisNL, #fit_modsnowxnx_bisbisNL, 
     fit_modsnowxnx_callepNL, #fit_modsnowxnx_callepNL, 
     fit_modsnowxnx_carscoNL, #fit_modsnowxnx_carscoNL, 
     fit_modsnowxnx_descNL, #fit_modsnowxnx_descNL, 
     fit_modsnowxnx_genalgNL, #fit_modsnowxnx_genalgNL, 
     fit_modsnowxnx_geumNL, #fit_modsnowxnx_geumNL, 
     fit_modsnowxnx_raresppNL, #fit_modsnowxnx_raresppNL, 
     fit_modsnowxnx_triparNL, #fit_modsnowxnx_triparNL, 
     file = "outputs/wstarNL/mod_wstar_snowxnx.RData")

#model comparisons 
summary(fit_modsnowxnx_descNL)#sig linear and smoothing term
summary(fit_modsnowxnx_raresppNL)#weakly sig linear term

#temp
modtempNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(avgT))

fit_modtempxnx_geumNL<- brm(modtempNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnx_artscoNL<- brm(modtempNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=10000, family=gaussian())
fit_modtempxnx_descNL<- brm(modtempNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnx_genalgNL<- brm(modtempNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnx_triparNL<- brm(modtempNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnx_raresppNL<- brm(modtempNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnx_callepNL<- brm(modtempNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnx_carscoNL<- brm(modtempNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnx_bisbisNL<- brm(modtempNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save(fit_modtempxnx_artscoNL,  #fit_modtempxnx_artscoNL,
     fit_modtempxnx_bisbisNL, #fit_modtempxnx_bisbisNL, 
     fit_modtempxnx_callepNL, #fit_modtempxnx_callepNL, 
     fit_modtempxnx_carscoNL, #fit_modtempxnx_carscoNL, 
     fit_modtempxnx_descNL, #fit_modtempxnx_descNL, 
     fit_modtempxnx_genalgNL, #fit_modtempxnx_genalgNL, 
     fit_modtempxnx_geumNL, #fit_modtempxnx_geumNL, 
     fit_modtempxnx_raresppNL, #fit_modtempxnx_raresppNL, 
     fit_modtempxnx_triparNL, #fit_modtempxnx_triparNL, 
     file = "outputs/wstarNL/mod_wstar_tempxnx.RData")

#Ndep
modNdepNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(Ndep))

fit_modNdepxnx_geumNL<- brm(modNdepNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnx_artscoNL<- brm(modNdepNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modNdepxnx_descNL<- brm(modNdepNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnx_genalgNL<- brm(modNdepNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnx_triparNL<- brm(modNdepNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnx_raresppNL<- brm(modNdepNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnx_callepNL<- brm(modNdepNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnx_carscoNL<- brm(modNdepNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnx_bisbisNL<- brm(modNdepNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save(fit_modNdepxnx_artscoNL,  #fit_modNdepxnx_artscoNL,
     fit_modNdepxnx_bisbisNL, #fit_modNdepxnx_bisbisNL, 
     fit_modNdepxnx_callepNL, #fit_modNdepxnx_callepNL, 
     fit_modNdepxnx_carscoNL, #fit_modNdepxnx_carscoNL, 
     fit_modNdepxnx_descNL, #fit_modNdepxnx_descNL, 
     fit_modNdepxnx_genalgNL, #fit_modNdepxnx_genalgNL, 
     fit_modNdepxnx_geumNL, #fit_modNdepxnx_geumNL, 
     fit_modNdepxnx_raresppNL, #fit_modNdepxnx_raresppNL, 
     fit_modNdepxnx_triparNL, #fit_modNdepxnx_triparNL, 
     file = "outputs/wstarNL/mod_wstar_Ndepxnx.RData")

#model comparisons 
summary(fit_modNdepxnx_descNL)#sig linear effect and smoothing term positive 
summary(fit_modNdepxnx_genalgNL)#weakly sig linear effect negative



#XNW----
load(file = "outputs/wstar_XNWoutput.RData")

#pull out of lists 
wstarmu<-as.data.frame(wstarXNW$ccMu)%>%
  pivot_longer(. , cols=everything(),names_to="spp", 
               values_to="mu")
wstarsd<-as.data.frame(wstarXNW$ccSd)%>%
  pivot_longer(. , cols=everything(),names_to="spp", 
               values_to="sd")
env<-as.data.frame(wstarXNW$x)

#combine into one df
wstarxnw<-cbind(wstarmu, select(wstarsd, -spp))
wstarxnw<-arrange(wstarxnw, spp)#rearrange by spp
wstarxnw<-cbind(wstarxnw, env)

#plots
#plot raw data-don't know how to get sds in there 
ggplot(wstarxnw, aes(x=depthcm, y=mu, col=spp)) +geom_smooth()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")

ggplot(wstarxnw, aes(x=Ndep, y=mu, col=spp)) +geom_smooth()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")

ggplot(wstarxnw, aes(x=avgT, y=mu, col=spp)) +geom_smooth()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")

#fit mods 
library(mgcv)
#subset by spp 
artsco=subset(wstarxnw, spp=="ARTSCO")
desc=subset(wstarxnw, spp=="DESCAE")
geum=subset(wstarxnw, spp=="GEUROS")
tripar=subset(wstarxnw, spp=="TRIPAR")
genalg=subset(wstarxnw, spp=="GENALG")
rarespp=subset(wstarxnw, spp=="RARESPP")
carsco=subset(wstarxnw, spp=="CARSCO")
bisbis=subset(wstarxnw, spp=="BISBIS")
callep=subset(wstarxnw, spp=="CALLEP")

#snow
modsnowNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(depthcm))

fit_modsnowxnw_geumNL<- brm(modsnowNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnw_artscoNL<- brm(modsnowNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modsnowxnw_descNL<- brm(modsnowNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnw_genalgNL<- brm(modsnowNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnw_triparNL<- brm(modsnowNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnw_raresppNL<- brm(modsnowNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnw_callepNL<- brm(modsnowNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnw_carscoNL<- brm(modsnowNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnw_bisbisNL<- brm(modsnowNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save moodels 
save(fit_modsnowxnw_artscoNL,  #fit_modsnowxnw_artscoNL,
     fit_modsnowxnw_bisbisNL, #fit_modsnowxnw_bisbisNL, 
     fit_modsnowxnw_callepNL, #fit_modsnowxnw_callepNL, 
     fit_modsnowxnw_carscoNL, #fit_modsnowxnw_carscoNL, 
     fit_modsnowxnw_descNL, #fit_modsnowxnw_descNL, 
     fit_modsnowxnw_genalgNL, #fit_modsnowxnw_genalgNL, 
     fit_modsnowxnw_geumNL, #fit_modsnowxnw_geumNL, 
     fit_modsnowxnw_raresppNL, #fit_modsnowxnw_raresppNL, 
     fit_modsnowxnw_triparNL, #fit_modsnowxnw_triparNL, 
     file = "outputs/wstarNL/mod_wstar_snowxnw.RData")
#model comparisons 

#temp
modtempNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(avgT))

fit_modtempxnw_geumNL<- brm(modtempNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnw_artscoNL<- brm(modtempNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=10000, family=gaussian())
fit_modtempxnw_descNL<- brm(modtempNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnw_genalgNL<- brm(modtempNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnw_triparNL<- brm(modtempNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnw_raresppNL<- brm(modtempNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnw_callepNL<- brm(modtempNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnw_carscoNL<- brm(modtempNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnw_bisbisNL<- brm(modtempNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save(fit_modtempxnw_artscoNL,  #fit_modtempxnw_artscoNL,
     fit_modtempxnw_bisbisNL, #fit_modtempxnw_bisbisNL, 
     fit_modtempxnw_callepNL, #fit_modtempxnw_callepNL, 
     fit_modtempxnw_carscoNL, #fit_modtempxnw_carscoNL, 
     fit_modtempxnw_descNL, #fit_modtempxnw_descNL, 
     fit_modtempxnw_genalgNL,# fit_modtempxnw_genalgNL, 
     fit_modtempxnw_geumNL, #fit_modtempxnw_geumNL, 
     fit_modtempxnw_raresppNL,# fit_modtempxnw_raresppNL, 
     fit_modtempxnw_triparNL, #fit_modtempxnw_triparNL, 
     file = "outputs/wstarNL/mod_wstar_tempxnw.RData")
#model summary 
summary(fit_modtempxnw_descNL)#significant linear term negative 

#Ndep
modNdepNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(Ndep))

fit_modNdepxnw_geumNL<- brm(modNdepNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnw_artscoNL<- brm(modNdepNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modNdepxnw_descNL<- brm(modNdepNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnw_genalgNL<- brm(modNdepNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnw_triparNL<- brm(modNdepNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnw_raresppNL<- brm(modNdepNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnw_callepNL<- brm(modNdepNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnw_carscoNL<- brm(modNdepNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnw_bisbisNL<- brm(modNdepNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save(fit_modNdepxnw_artscoNL,  #fit_modNdepxnw_artscoNL,
     fit_modNdepxnw_bisbisNL, #fit_modNdepxnw_bisbisNL, 
     fit_modNdepxnw_callepNL, #fit_modNdepxnw_callepNL, 
     fit_modNdepxnw_carscoNL,# fit_modNdepxnw_carscoNL, 
     fit_modNdepxnw_descNL, #fit_modNdepxnw_descNL, 
     fit_modNdepxnw_genalgNL,# fit_modNdepxnw_genalgNL, 
     fit_modNdepxnw_geumNL, #fit_modNdepxnw_geumNL, 
     fit_modNdepxnw_raresppNL, #fit_modNdepxnw_raresppNL, 
     fit_modNdepxnw_triparNL, #fit_modNdepxnw_triparNL, 
     file = "outputs/wstarNL/mod_wstar_Ndepxnw.RData")
#model comparisons 
summary(fit_modNdepxnw_descNL)#significant linear term positive 
summary(fit_modNdepxnw_artscoNL)#weakly significant linear term negative 
summary(fit_modNdepxnw_genalgNL)#weakly significant linear term negative 

#PXX----
load(file = "outputs/wstar_PXXoutput.RData")

#pull out of lists 
wstarmu<-as.data.frame(wstarPXX$ccMu)%>%
  pivot_longer(. , cols=everything(),names_to="spp", 
               values_to="mu")
wstarsd<-as.data.frame(wstarPXX$ccSd)%>%
  pivot_longer(. , cols=everything(),names_to="spp", 
               values_to="sd")
env<-as.data.frame(wstarPXX$x)

#combine into one df
wstarpxx<-cbind(wstarmu, select(wstarsd, -spp))
wstarpxx<-arrange(wstarpxx, spp)#rearrange by spp
wstarpxx<-cbind(wstarpxx, env)

#plots
#plot raw data-don't know how to get sds in there 
ggplot(wstarpxx, aes(x=depthcm, y=mu, col=spp)) +geom_smooth()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")+
  theme_classic()+ylab("mu equilibrium abundance")+
  facet_wrap(~spp, scales ="free")+ theme_classic()+ xlab("snow depth PXX")+
  theme(legend.position = "none")



ggplot(wstarpxx, aes(x=Ndep, y=mu, col=spp)) +geom_smooth()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")+
  theme_classic()+ylab("mu equilibrium abundance")+ 
  facet_wrap(~spp, scales ="free")+ theme_classic()+ xlab("N dep PXX")+
  theme(legend.position = "none")



ggplot(wstarpxx, aes(x=avgT, y=mu, col=spp)) +geom_smooth()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")

#fit mods 
library(mgcv)
#subset by spp 
artsco=subset(wstarpxx, spp=="ARTSCO")
desc=subset(wstarpxx, spp=="DESCAE")
geum=subset(wstarpxx, spp=="GEUROS")
tripar=subset(wstarpxx, spp=="TRIPAR")
genalg=subset(wstarpxx, spp=="GENALG")
rarespp=subset(wstarpxx, spp=="RARESPP")
carsco=subset(wstarpxx, spp=="CARSCO")
bisbis=subset(wstarpxx, spp=="BISBIS")
callep=subset(wstarpxx, spp=="CALLEP")

#snow
modsnowNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(depthcm))

fit_modsnowpxx_geumNL<- brm(modsnowNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxx_artscoNL<- brm(modsnowNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modsnowpxx_descNL<- brm(modsnowNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxx_genalgNL<- brm(modsnowNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxx_triparNL<- brm(modsnowNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxx_raresppNL<- brm(modsnowNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxx_callepNL<- brm(modsnowNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxx_carscoNL<- brm(modsnowNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxx_bisbisNL<- brm(modsnowNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save moodels 
save(  fit_modsnowpxx_artscoNL,
       fit_modsnowpxx_bisbisNL, 
       fit_modsnowpxx_callepNL, 
       fit_modsnowpxx_carscoNL, 
       fit_modsnowpxx_descNL, 
       fit_modsnowpxx_genalgNL, 
       fit_modsnowpxx_geumNL, 
       fit_modsnowpxx_raresppNL, 
       fit_modsnowpxx_triparNL, 
       file = "outputs/wstarNL/mod_wstar_snowpxx.RData")
#model comparisons 
summary(fit_modsnowpxx_descNL)#NS

#temp
modtempNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(avgT))

fit_modtemppxx_geumNL<- brm(modtempNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxx_artscoNL<- brm(modtempNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=10000, family=gaussian())
fit_modtemppxx_descNL<- brm(modtempNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxx_genalgNL<- brm(modtempNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxx_triparNL<- brm(modtempNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxx_raresppNL<- brm(modtempNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxx_callepNL<- brm(modtempNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxx_carscoNL<- brm(modtempNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxx_bisbisNL<- brm(modtempNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save(fit_modtemppxx_artscoNL,
     fit_modtemppxx_bisbisNL, 
     fit_modtemppxx_callepNL, 
     fit_modtemppxx_carscoNL, 
     fit_modtemppxx_descNL, 
     fit_modtemppxx_genalgNL, 
     fit_modtemppxx_geumNL, 
     fit_modtemppxx_raresppNL, 
     fit_modtemppxx_triparNL, 
     file = "outputs/wstarNL/mod_wstar_temppxx.RData")
#model comparisons 
summary(fit_modtemppxx_descNL)#sig


#Ndep
modNdepNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(Ndep))

fit_modNdeppxx_geumNL<- brm(modNdepNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxx_artscoNL<- brm(modNdepNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modNdeppxx_descNL<- brm(modNdepNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxx_genalgNL<- brm(modNdepNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxx_triparNL<- brm(modNdepNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxx_raresppNL<- brm(modNdepNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxx_callepNL<- brm(modNdepNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxx_carscoNL<- brm(modNdepNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxx_bisbisNL<- brm(modNdepNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save(fit_modNdeppxx_artscoNL,
     fit_modNdeppxx_bisbisNL, 
     fit_modNdeppxx_callepNL, 
     fit_modNdeppxx_carscoNL, 
     fit_modNdeppxx_descNL, 
     fit_modNdeppxx_genalgNL, 
     fit_modNdeppxx_geumNL, 
     fit_modNdeppxx_raresppNL, 
     fit_modNdeppxx_triparNL, 
     file = "outputs/wstarNL/mod_wstar_Ndeppxx.RData")
#model comparisons 
summary(fit_modNdeppxx_descNL)#NS
summary(fit_modNdeppxx_triparNL)#sig


#PXW----
load(file = "outputs/wstar_PXWoutput.RData")

#pull out of lists 
wstarmu<-as.data.frame(wstarPXW$ccMu)%>%
  pivot_longer(. , cols=everything(),names_to="spp", 
               values_to="mu")
wstarsd<-as.data.frame(wstarPXW$ccSd)%>%
  pivot_longer(. , cols=everything(),names_to="spp", 
               values_to="sd")
env<-as.data.frame(wstarPXW$x)

#combine into one df
wstarpxw<-cbind(wstarmu, select(wstarsd, -spp))
wstarpxw<-arrange(wstarpxw, spp)#rearrange by spp
wstarpxw<-cbind(wstarpxw, env)

#plots
#plot raw data-don't know how to get sds in there 
ggplot(wstarpxw, aes(x=depthcm, y=mu, col=spp)) +geom_smooth()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")

ggplot(wstarpxw, aes(x=Ndep, y=mu, col=spp)) +geom_smooth()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")

ggplot(wstarpxw, aes(x=avgT, y=mu, col=spp)) +geom_smooth()+
#geom_ribbon(aes(ymin=mu-sd, ymax=mu+sd), 
# fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")+ theme_classic()+ xlab("avgT PXW")+
  ylab("mu equilibrium abundance")+ theme(legend.position = "none")

#fit mods 
library(mgcv)
#subset by spp 
artsco=subset(wstarpxw, spp=="ARTSCO")
desc=subset(wstarpxw, spp=="DESCAE")
geum=subset(wstarpxw, spp=="GEUROS")
tripar=subset(wstarpxw, spp=="TRIPAR")
genalg=subset(wstarpxw, spp=="GENALG")
rarespp=subset(wstarpxw, spp=="RARESPP")
carsco=subset(wstarpxw, spp=="CARSCO")
bisbis=subset(wstarpxw, spp=="BISBIS")
callep=subset(wstarpxw, spp=="CALLEP")


#snow
modsnowNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(depthcm))

fit_modsnowpxw_geumNL<- brm(modsnowNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxw_artscoNL<- brm(modsnowNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modsnowpxw_descNL<- brm(modsnowNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxw_genalgNL<- brm(modsnowNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxw_triparNL<- brm(modsnowNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxw_raresppNL<- brm(modsnowNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxw_callepNL<- brm(modsnowNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxw_carscoNL<- brm(modsnowNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxw_bisbisNL<- brm(modsnowNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save moodels 
save(fit_modsnowpxw_artscoNL,
     fit_modsnowpxw_bisbisNL, 
     fit_modsnowpxw_callepNL, 
     fit_modsnowpxw_carscoNL, 
     fit_modsnowpxw_descNL, 
     fit_modsnowpxw_genalgNL, 
     fit_modsnowpxw_geumNL, 
     fit_modsnowpxw_raresppNL, 
     fit_modsnowpxw_triparNL, 
     file = "outputs/wstarNL/mod_wstar_snowpxw.RData")
#model comparisons 

#temp
modtempNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(avgT))

fit_modtemppxw_geumNL<- brm(modtempNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxw_artscoNL<- brm(modtempNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=10000, family=gaussian())
fit_modtemppxw_descNL<- brm(modtempNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxw_genalgNL<- brm(modtempNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxw_triparNL<- brm(modtempNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxw_raresppNL<- brm(modtempNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxw_callepNL<- brm(modtempNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxw_carscoNL<- brm(modtempNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxw_bisbisNL<- brm(modtempNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save(fit_modtemppxw_artscoNL,
     fit_modtemppxw_bisbisNL, 
     fit_modtemppxw_callepNL, 
     fit_modtemppxw_carscoNL, 
     fit_modtemppxw_descNL, 
     fit_modtemppxw_genalgNL, 
     fit_modtemppxw_geumNL, 
     fit_modtemppxw_raresppNL, 
     fit_modtemppxw_triparNL, 
     file = "outputs/wstarNL/mod_wstar_temppxw.RData")
#model comparisons 

#Ndep
modNdepNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(Ndep))

fit_modNdeppxw_geumNL<- brm(modNdepNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxw_artscoNL<- brm(modNdepNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modNdeppxw_descNL<- brm(modNdepNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxw_genalgNL<- brm(modNdepNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxw_triparNL<- brm(modNdepNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxw_raresppNL<- brm(modNdepNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxw_callepNL<- brm(modNdepNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxw_carscoNL<- brm(modNdepNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxw_bisbisNL<- brm(modNdepNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save(fit_modNdeppxw_artscoNL,
     fit_modNdeppxw_bisbisNL, 
     fit_modNdeppxw_callepNL, 
     fit_modNdeppxw_carscoNL, 
     fit_modNdeppxw_descNL, 
     fit_modNdeppxw_genalgNL, 
     fit_modNdeppxw_geumNL, 
     fit_modNdeppxw_raresppNL, 
     fit_modNdeppxw_triparNL, 
     file = "outputs/wstarNL/mod_wstar_Ndeppxw.RData")
#model comparisons 


#PNX----
load(file = "outputs/wstar_PNXoutput.RData")

#pull out of lists 
wstarmu<-as.data.frame(wstarPNX$ccMu)%>%
  pivot_longer(. , cols=everything(),names_to="spp", 
               values_to="mu")
wstarsd<-as.data.frame(wstarPNX$ccSd)%>%
  pivot_longer(. , cols=everything(),names_to="spp", 
               values_to="sd")
env<-as.data.frame(wstarPNX$x)

#combine into one df
wstarpnx<-cbind(wstarmu, select(wstarsd, -spp))
wstarpnx<-arrange(wstarpnx, spp)#rearrange by spp
wstarpnx<-cbind(wstarpnx, env)

#plots
#plot raw data-don't know how to get sds in there 
ggplot(wstarpnx, aes(x=depthcm, y=mu, col=spp)) +geom_smooth()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")

ggplot(wstarpnx, aes(x=Ndep, y=mu, col=spp)) +geom_smooth()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")

ggplot(wstarpnx, aes(x=avgT, y=mu, col=spp)) +geom_smooth()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")

#fit mods 
library(mgcv)
#subset by spp 
artsco=subset(wstarpnx, spp=="ARTSCO")
desc=subset(wstarpnx, spp=="DESCAE")
geum=subset(wstarpnx, spp=="GEUROS")
tripar=subset(wstarpnx, spp=="TRIPAR")
genalg=subset(wstarpnx, spp=="GENALG")
rarespp=subset(wstarpnx, spp=="RARESPP")
carsco=subset(wstarpnx, spp=="CARSCO")
bisbis=subset(wstarpnx, spp=="BISBIS")
callep=subset(wstarpnx, spp=="CALLEP")

#snow
modsnowNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(depthcm))

fit_modsnowpnx_geumNL<- brm(modsnowNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnx_artscoNL<- brm(modsnowNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modsnowpnx_descNL<- brm(modsnowNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnx_genalgNL<- brm(modsnowNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnx_triparNL<- brm(modsnowNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnx_raresppNL<- brm(modsnowNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnx_callepNL<- brm(modsnowNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnx_carscoNL<- brm(modsnowNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnx_bisbisNL<- brm(modsnowNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save moodels 
save(fit_modsnowpnx_artscoNL,  #fit_modsnowpnx_artscoNL,
     fit_modsnowpnx_bisbisNL, #fit_modsnowpnx_bisbisNL, 
     fit_modsnowpnx_callepNL, #fit_modsnowpnx_callepNL, 
     fit_modsnowpnx_carscoNL, #fit_modsnowpnx_carscoNL, 
     fit_modsnowpnx_descNL, #fit_modsnowpnx_descNL, 
     fit_modsnowpnx_genalgNL,# fit_modsnowpnx_genalgNL, 
     fit_modsnowpnx_geumNL, #fit_modsnowpnx_geumNL, 
     fit_modsnowpnx_raresppNL,# fit_modsnowpnx_raresppNL, 
     fit_modsnowpnx_triparNL, #fit_modsnowpnx_triparNL, 
     file = "outputs/wstarNL/mod_wstar_snowpnx.RData")
#model comparisons 
summary(fit_modsnowpnx_carscoNL)#significant smoothing term 

#temp
modtempNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(avgT))

fit_modtemppnx_geumNL<- brm(modtempNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnx_artscoNL<- brm(modtempNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=10000, family=gaussian())
fit_modtemppnx_descNL<- brm(modtempNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnx_genalgNL<- brm(modtempNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnx_triparNL<- brm(modtempNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnx_raresppNL<- brm(modtempNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnx_callepNL<- brm(modtempNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnx_carscoNL<- brm(modtempNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnx_bisbisNL<- brm(modtempNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save(fit_modtemppnx_artscoNL,  #fit_modtemppnx_artscoNL,
     fit_modtemppnx_bisbisNL, #fit_modtemppnx_bisbisNL, 
     fit_modtemppnx_callepNL,# fit_modtemppnx_callepNL, 
     fit_modtemppnx_carscoNL,#fit_modtemppnx_carscoNL, 
     fit_modtemppnx_descNL, #fit_modtemppnx_descNL, 
     fit_modtemppnx_genalgNL, #fit_modtemppnx_genalgNL, 
     fit_modtemppnx_geumNL, #fit_modtemppnx_geumNL, 
     fit_modtemppnx_raresppNL,# fit_modtemppnx_raresppNL, 
     fit_modtemppnx_triparNL, #fit_modtemppnx_triparNL, 
     file = "outputs/wstarNL/mod_wstar_temppnx.RData")
#model comparisons 

#Ndep
modNdepNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(Ndep))

fit_modNdeppnx_geumNL<- brm(modNdepNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnx_artscoNL<- brm(modNdepNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modNdeppnx_descNL<- brm(modNdepNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnx_genalgNL<- brm(modNdepNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnx_triparNL<- brm(modNdepNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnx_raresppNL<- brm(modNdepNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnx_callepNL<- brm(modNdepNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnx_carscoNL<- brm(modNdepNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnx_bisbisNL<- brm(modNdepNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save(fit_modNdeppnx_artscoNL, # fit_modNdeppnx_artscoNL,
     fit_modNdeppnx_bisbisNL, #fit_modNdeppnx_bisbisNL, 
     fit_modNdeppnx_callepNL, #fit_modNdeppnx_callepNL, 
     fit_modNdeppnx_carscoNL, #fit_modNdeppnx_carscoNL, 
     fit_modNdeppnx_descNL, #fit_modNdeppnx_descNL, 
     fit_modNdeppnx_genalgNL,# fit_modNdeppnx_genalgNL, 
     fit_modNdeppnx_geumNL, #fit_modNdeppnx_geumNL, 
     fit_modNdeppnx_raresppNL,# fit_modNdeppnx_raresppNL, 
     fit_modNdeppnx_triparNL,# fit_modNdeppnx_triparNL, 
     file = "outputs/wstarNL/mod_wstar_Ndeppnx.RData")
#model comparisons 
summary(fit_modNdeppnx_carscoNL)#significant smoothing term 
summary(fit_modNdeppnx_descNL)#significant smoothing term and linear term



#PNW----
load(file = "outputs/wstar_PNWoutput.RData")

#pull out of lists 
wstarmu<-as.data.frame(wstarPNW$ccMu)%>%
  pivot_longer(. , cols=everything(),names_to="spp", 
               values_to="mu")
wstarsd<-as.data.frame(wstarPNW$ccSd)%>%
  pivot_longer(. , cols=everything(),names_to="spp", 
               values_to="sd")
env<-as.data.frame(wstarPNW$x)

#combine into one df
wstarpnw<-cbind(wstarmu, select(wstarsd, -spp))
wstarpnw<-arrange(wstarpnw, spp)#rearrange by spp
wstarpnw<-cbind(wstarpnw, env)

#plots
#plot raw data-don't know how to get sds in there 
ggplot(wstarpnw, aes(x=depthcm, y=mu, col=spp)) +geom_smooth()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")

ggplot(wstarpnw, aes(x=Ndep, y=mu, col=spp)) +geom_smooth()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")

ggplot(wstarpnw, aes(x=avgT, y=mu, col=spp)) +geom_smooth()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")

#fit mods 
library(mgcv)
#subset by spp 
artsco=subset(wstarpnw, spp=="ARTSCO")
desc=subset(wstarpnw, spp=="DESCAE")
geum=subset(wstarpnw, spp=="GEUROS")
tripar=subset(wstarpnw, spp=="TRIPAR")
genalg=subset(wstarpnw, spp=="GENALG")
rarespp=subset(wstarpnw, spp=="RARESPP")
carsco=subset(wstarpnw, spp=="CARSCO")
bisbis=subset(wstarpnw, spp=="BISBIS")
callep=subset(wstarpnw, spp=="CALLEP")

#snow
modsnowNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(depthcm))

fit_modsnowpnw_geumNL<- brm(modsnowNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnw_artscoNL<- brm(modsnowNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modsnowpnw_descNL<- brm(modsnowNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnw_genalgNL<- brm(modsnowNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnw_triparNL<- brm(modsnowNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnw_raresppNL<- brm(modsnowNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnw_callepNL<- brm(modsnowNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnw_carscoNL<- brm(modsnowNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnw_bisbisNL<- brm(modsnowNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save moodels 
save(fit_modsnowpnw_artscoNL,  #fit_modsnowpnw_artscoNL,
     fit_modsnowpnw_bisbisNL, #fit_modsnowpnw_bisbisNL, 
     fit_modsnowpnw_callepNL, #fit_modsnowpnw_callepNL, 
     fit_modsnowpnw_carscoNL,# fit_modsnowpnw_carscoNL, 
     fit_modsnowpnw_descNL, #fit_modsnowpnw_descNL, 
     fit_modsnowpnw_genalgNL,# fit_modsnowpnw_genalgNL, 
     fit_modsnowpnw_geumNL, #fit_modsnowpnw_geumNL, 
     fit_modsnowpnw_raresppNL,# fit_modsnowpnw_raresppNL, 
     fit_modsnowpnw_triparNL, #fit_modsnowpnw_triparNL, 
     file = "outputs/wstarNL/mod_wstar_snowpnw.RData")
#model comparisons 

#temp
modtempNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(avgT))

fit_modtemppnw_geumNL<- brm(modtempNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnw_artscoNL<- brm(modtempNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=10000, family=gaussian())
fit_modtemppnw_descNL<- brm(modtempNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnw_genalgNL<- brm(modtempNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnw_triparNL<- brm(modtempNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnw_callepNL<- brm(modtempNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnw_carscoNL<- brm(modtempNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnw_bisbisNL<- brm(modtempNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save(fit_modtemppnw_artscoNL,  #fit_modtemppnw_artscoNL,
     fit_modtemppnw_bisbisNL, #fit_modtemppnw_bisbisNL, 
     fit_modtemppnw_callepNL, #fit_modtemppnw_callepNL, 
     fit_modtemppnw_carscoNL, #fit_modtemppnw_carscoNL, 
     fit_modtemppnw_descNL, #fit_modtemppnw_descNL, 
     fit_modtemppnw_genalgNL,# fit_modtemppnw_genalgNL, 
     fit_modtemppnw_geumNL, #fit_modtemppnw_geumNL, 
     fit_modtemppnw_raresppNL, #fit_modtemppnw_raresppNL, 
     fit_modtemppnw_triparNL, #fit_modtemppnw_triparNL, 
     file = "outputs/wstarNL/mod_wstar_temppnw.RData")
#model comparisons 
summary(fit_modtemppnw_artscoNL)#weakly sig linear neg
summary(fit_modtemppnw_callepNL)#weakly sig linear neg
summary(fit_modtemppnw_descNL)#sig linear neg

#Ndep
modNdepNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(Ndep))

fit_modNdeppnw_geumNL<- brm(modNdepNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnw_artscoNL<- brm(modNdepNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modNdeppnw_descNL<- brm(modNdepNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnw_genalgNL<- brm(modNdepNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnw_triparNL<- brm(modNdepNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnw_raresppNL<- brm(modNdepNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnw_callepNL<- brm(modNdepNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnw_carscoNL<- brm(modNdepNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnw_bisbisNL<- brm(modNdepNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save(fit_modNdeppnw_artscoNL, # fit_modNdeppnw_artscoNL,
     fit_modNdeppnw_bisbisNL,# fit_modNdeppnw_bisbisNL, 
     fit_modNdeppnw_callepNL, #fit_modNdeppnw_callepNL, 
     fit_modNdeppnw_carscoNL, #fit_modNdeppnw_carscoNL, 
     fit_modNdeppnw_descNL, #fit_modNdeppnw_descNL, 
     fit_modNdeppnw_genalgNL,# fit_modNdeppnw_genalgNL, 
     fit_modNdeppnw_geumNL, #fit_modNdeppnw_geumNL, 
     fit_modNdeppnw_raresppNL,# fit_modNdeppnw_raresppNL, 
     fit_modNdeppnw_triparNL, #fit_modNdeppnw_triparNL, 
     file = "outputs/wstarNL/mod_wstar_Ndeppnw.RData")
#model comparisons 
summary(fit_modNdeppnw_descNL)#sig linear pos


#model summaries---- 
load(file="outputs/wstarNL/mod_wstar_snowxnw.RData")
#snow
summary(fit_modsnowpxx_descNL)#significant smoothing term 
summary(fit_modsnowxnx_descNL)#sig linear (already in rho) and smoothing term 
summary(fit_modsnowxnw_descNL)#significant smoothing term--doesn't look like it in the graph
#other spp 
summary(fit_modsnowpxx_raresppNL)#significant smoothing term 
summary(fit_modsnowpnx_carscoNL)#significant smoothing term 
summary(fit_modsnowxxx_geumNL)#sig smoothing term 
summary(fit_modsnowxxw_carscoNL)#sig smoothing term 

#XXX-1, XXW-1, XNX-1, PXX-2, PNX-1, XNW-1
#PXW, PNW, snow nothing significant

#temp
#load(file="outputs/wstarNL/mod_wstar_temppxw.RData")

summary(fit_modtempxxw_carscoNL)#significant smoothing term
summary(fit_modtempxnx_descNL)#significant smoothing term-not clear in graph
summary(fit_modtemppxw_triparNL) #sig smoothing term 
summary(fit_modtemppxw_descNL) #sig smoothing term 

#XXW-1, XNX-1, PXW-1
#XXX, PXX, PNX, XNW?, PNW temp nothing significant 

#Ndep
#load(file="outputs/wstarNL/mod_wstar_Ndeppnw.RData")

summary(fit_modNdepxxw_carscoNL)#sig smoothing term 
summary(fit_modNdepxnx_descNL)#sig smoothing and linear term  
summary(fit_modNdeppxx_descNL)#sig smoothing term
summary(fit_modNdeppxx_triparNL)#sig smoothing term 
summary(fit_modNdeppnx_carscoNL)#significant smoothing term 
summary(fit_modNdeppnx_descNL)#significant smoothing term and linear term
summary(fit_modNdepxnw_descNL)#sig linear pos (NOT SIG FOR RHO)
summary(fit_modNdeppnw_descNL)#sig linear pos (NOT SIG FOR RHO)

#XXW-1, XNX-1, PXX-2, PNX-2, XNW-1, PNW-1
#XXX,PXW Ndep nothing significant 

#deschampsia all----
descxxx<-subset(wstarxxx, spp=="DESCAE")%>%
  mutate(treat="XXX")
descxxw<-subset(wstarxxw, spp=="DESCAE")%>%
  mutate(treat="XXW")
descxnx<-subset(wstarxnx, spp=="DESCAE")%>%
  mutate(treat="XNX")
descpxx<-subset(wstarpxx, spp=="DESCAE")%>%
  mutate(treat="PXX")
descpnx<-subset(wstarpnx, spp=="DESCAE")%>%
  mutate(treat="PNX")
descpxw<-subset(wstarpxw, spp=="DESCAE")%>%
  mutate(treat="PXW")
descxnw<-subset(wstarxnw, spp=="DESCAE")%>%
  mutate(treat="XNW")
descpnw<-subset(wstarpnw, spp=="DESCAE")%>%
  mutate(treat="PNW")
desc_wstar<-rbind(descxxx, descxxw, descxnx, descpxx, 
                  descpnx, descxnw, descpxw, descpnw )

ggplot(desc_wstar, aes(x=avgT, y=mu, col=treat)) +
geom_smooth()+ylab("mu equilibrium abundance")+
  theme_classic()

ggplot(desc_wstar, aes(x=depthcm, y=mu, col=treat)) +
  geom_smooth()+ylab("mu equilibrium abundance")+
  theme_classic()

ggplot(desc_wstar, aes(x=Ndep, y=mu, col=treat)) +
  geom_smooth()+ylab("mu equilibrium abundance")+
  theme_classic()

#non linear looking 
#geum
ggplot(wstarxxx, aes(x=as.factor(depthcm), y=mu, col=spp)) +geom_boxplot()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")+ theme_classic()+ theme(legend.position="none") 

#carsco, tripar
ggplot(wstarpnx, aes(x=as.factor(depthcm), y=mu, col=spp)) +geom_boxplot()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")+ theme_classic()+ theme(legend.position="none")
#carsco
ggplot(wstarpnx, aes(x=as.factor(Ndep), y=mu, col=spp)) +geom_boxplot()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")+ theme_classic()+ theme(legend.position="none")
#bisbis
ggplot(wstarxnw, aes(x=as.factor(Ndep), y=mu, col=spp)) +geom_boxplot()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")+ theme_classic()+ theme(legend.position="none")
#rare spp
ggplot(wstarpxx, aes(x=as.factor(depthcm), y=mu, col=spp)) +geom_boxplot()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")+ theme_classic()+ theme(legend.position="none")
#tripar, callep? desc?
ggplot(wstarpxx, aes(x=as.factor(Ndep), y=mu, col=spp)) +geom_boxplot()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")+ theme_classic()+ theme(legend.position="none")
#bisbis, rarespp?
ggplot(wstarxnx, aes(x=as.factor(depthcm), y=mu, col=spp)) +geom_boxplot()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")+ theme_classic()+ theme(legend.position="none")
#tripar, gemalg?
ggplot(wstarxnx, aes(x=as.factor(Ndep), y=mu, col=spp)) +geom_boxplot()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")+ theme_classic()+ theme(legend.position="none")
#Desc, genalg, tripar, artsco?
ggplot(wstarpxw, aes(x=as.factor(avgT), y=mu, col=spp)) +geom_boxplot()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")+ theme_classic()+ theme(legend.position="none")

