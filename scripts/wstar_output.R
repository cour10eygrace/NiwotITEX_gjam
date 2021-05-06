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
  facet_wrap(~spp, scales ="free")

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

#fit mods 
library(mgcv)
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
modsnow<-bf(mu|resp_se(sd, sigma = TRUE)~depthcm)
modsnowNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(depthcm))

fit_modsnowxxx_geum<- brm(modsnow, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxx_geumNL<- brm(modsnowNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxx_artsco<- brm(modsnow, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxx_artscoNL<- brm(modsnowNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modsnowxxx_desc<- brm(modsnow, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxx_descNL<- brm(modsnowNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxx_genalg<- brm(modsnow, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxx_genalgNL<- brm(modsnowNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxx_tripar<- brm(modsnow, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxx_triparNL<- brm(modsnowNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxx_rarespp<- brm(modsnow, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxx_raresppNL<- brm(modsnowNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxx_callep<- brm(modsnow, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxx_callepNL<- brm(modsnowNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxx_carsco<- brm(modsnow, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxx_carscoNL<- brm(modsnowNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxx_bisbis<- brm(modsnow, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxx_bisbisNL<- brm(modsnowNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save moodels 
save(fit_modsnowxxx_artsco,  fit_modsnowxxx_artscoNL,
     fit_modsnowxxx_bisbis, fit_modsnowxxx_bisbisNL, 
     fit_modsnowxxx_callep, fit_modsnowxxx_callepNL, 
     fit_modsnowxxx_carsco, fit_modsnowxxx_carscoNL, 
     fit_modsnowxxx_desc, fit_modsnowxxx_descNL, 
     fit_modsnowxxx_genalg, fit_modsnowxxx_genalgNL, 
     fit_modsnowxxx_geum, fit_modsnowxxx_geumNL, 
     fit_modsnowxxx_rarespp, fit_modsnowxxx_raresppNL, 
     fit_modsnowxxx_tripar, fit_modsnowxxx_triparNL, 
     file = "outputs/wstarNL/mod_wstar_snowxxx.RData")

#model summary
summary(fit_modsnowxxx_geum) #NS
summary(fit_modsnowxxx_geumNL)#significant smoothing term 
WAIC(fit_modsnowxxx_geum, fit_modsnowxxx_geumNL)#NL fit much better -2.3/0.3
summary(fit_modsnowxxx_desc) #significant positive 
summary(fit_modsnowxxx_descNL)#NS
waic(fit_modsnowxxx_desc, fit_modsnowxxx_descNL)#linear fit better but NS

#temp
modtemp<-bf(mu|resp_se(sd, sigma = TRUE)~avgT)
modtempNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(avgT))

fit_modtempxxx_geum<- brm(modtemp, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxx_geumNL<- brm(modtempNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxx_artsco<- brm(modtemp, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxx_artscoNL<- brm(modtempNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=10000, family=gaussian())
fit_modtempxxx_desc<- brm(modtemp, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxx_descNL<- brm(modtempNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxx_genalg<- brm(modtemp, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxx_genalgNL<- brm(modtempNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxx_tripar<- brm(modtemp, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxx_triparNL<- brm(modtempNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxx_rarespp<- brm(modtemp, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxx_raresppNL<- brm(modtempNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxx_callep<- brm(modtemp, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxx_callepNL<- brm(modtempNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxx_carsco<- brm(modtemp, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxx_carscoNL<- brm(modtempNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxx_bisbis<- brm(modtemp, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxx_bisbisNL<- brm(modtempNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save(fit_modtempxxx_artsco,  fit_modtempxxx_artscoNL,
     fit_modtempxxx_bisbis, fit_modtempxxx_bisbisNL, 
     fit_modtempxxx_callep, fit_modtempxxx_callepNL, 
     fit_modtempxxx_carsco, fit_modtempxxx_carscoNL, 
     fit_modtempxxx_desc, fit_modtempxxx_descNL, 
     fit_modtempxxx_genalg, fit_modtempxxx_genalgNL, 
     fit_modtempxxx_geum, fit_modtempxxx_geumNL, 
     fit_modtempxxx_rarespp, fit_modtempxxx_raresppNL, 
     fit_modtempxxx_tripar, fit_modtempxxx_triparNL, 
     file = "outputs/wstarNL/mod_wstar_tempxxx.RData")

#Ndep
modNdep<-bf(mu|resp_se(sd, sigma = TRUE)~Ndep)
modNdepNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(Ndep))

fit_modNdepxxx_geum<- brm(modNdep, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxx_geumNL<- brm(modNdepNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxx_artsco<- brm(modNdep, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxx_artscoNL<- brm(modNdepNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modNdepxxx_desc<- brm(modNdep, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxx_descNL<- brm(modNdepNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxx_genalg<- brm(modNdep, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxx_genalgNL<- brm(modNdepNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxx_tripar<- brm(modNdep, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxx_triparNL<- brm(modNdepNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxx_rarespp<- brm(modNdep, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxx_raresppNL<- brm(modNdepNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxx_callep<- brm(modNdep, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxx_callepNL<- brm(modNdepNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxx_carsco<- brm(modNdep, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxx_carscoNL<- brm(modNdepNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxx_bisbis<- brm(modNdep, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxx_bisbisNL<- brm(modNdepNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save(fit_modNdepxxx_artsco,  fit_modNdepxxx_artscoNL,
     fit_modNdepxxx_bisbis, fit_modNdepxxx_bisbisNL, 
     fit_modNdepxxx_callep, fit_modNdepxxx_callepNL, 
     fit_modNdepxxx_carsco, fit_modNdepxxx_carscoNL, 
     fit_modNdepxxx_desc, fit_modNdepxxx_descNL, 
     fit_modNdepxxx_genalg, fit_modNdepxxx_genalgNL, 
     fit_modNdepxxx_geum, fit_modNdepxxx_geumNL, 
     fit_modNdepxxx_rarespp, fit_modNdepxxx_raresppNL, 
     fit_modNdepxxx_tripar, fit_modNdepxxx_triparNL, 
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
modsnow<-bf(mu|resp_se(sd, sigma = TRUE)~depthcm)
modsnowNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(depthcm))

fit_modsnowxxw_geum<- brm(modsnow, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxw_geumNL<- brm(modsnowNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxw_artsco<- brm(modsnow, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxw_artscoNL<- brm(modsnowNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modsnowxxw_desc<- brm(modsnow, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxw_descNL<- brm(modsnowNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxw_genalg<- brm(modsnow, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxw_genalgNL<- brm(modsnowNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxw_tripar<- brm(modsnow, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxw_triparNL<- brm(modsnowNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxw_rarespp<- brm(modsnow, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxw_raresppNL<- brm(modsnowNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxw_callep<- brm(modsnow, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxw_callepNL<- brm(modsnowNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxw_carsco<- brm(modsnow, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxw_carscoNL<- brm(modsnowNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxw_bisbis<- brm(modsnow, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxxw_bisbisNL<- brm(modsnowNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save moodels 
save(fit_modsnowxxw_artsco,  fit_modsnowxxw_artscoNL,
     fit_modsnowxxw_bisbis, fit_modsnowxxw_bisbisNL, 
     fit_modsnowxxw_callep, fit_modsnowxxw_callepNL, 
     fit_modsnowxxw_carsco, fit_modsnowxxw_carscoNL, 
     fit_modsnowxxw_desc, fit_modsnowxxw_descNL, 
     fit_modsnowxxw_genalg, fit_modsnowxxw_genalgNL, 
     fit_modsnowxxw_geum, fit_modsnowxxw_geumNL, 
     fit_modsnowxxw_rarespp, fit_modsnowxxw_raresppNL, 
     fit_modsnowxxw_tripar, fit_modsnowxxw_triparNL, 
     file = "outputs/wstarNL/mod_wstar_snowxxw.RData")
#model comparisons 
summary(fit_modsnowxxw_desc)#significant 
summary(fit_modsnowxxw_descNL)#NS
waic(fit_modsnowxxw_desc, fit_modsnowxxw_descNL)


#temp
modtemp<-bf(mu|resp_se(sd, sigma = TRUE)~avgT)
modtempNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(avgT))

fit_modtempxxw_geum<- brm(modtemp, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxw_geumNL<- brm(modtempNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxw_artsco<- brm(modtemp, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxw_artscoNL<- brm(modtempNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=10000, family=gaussian())
fit_modtempxxw_desc<- brm(modtemp, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxw_descNL<- brm(modtempNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxw_genalg<- brm(modtemp, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxw_genalgNL<- brm(modtempNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxw_tripar<- brm(modtemp, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxw_triparNL<- brm(modtempNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxw_rarespp<- brm(modtemp, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxw_raresppNL<- brm(modtempNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxw_callep<- brm(modtemp, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxw_callepNL<- brm(modtempNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxw_carsco<- brm(modtemp, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxw_carscoNL<- brm(modtempNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxw_bisbis<- brm(modtemp, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxxw_bisbisNL<- brm(modtempNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save(fit_modtempxxw_artsco,  fit_modtempxxw_artscoNL,
     fit_modtempxxw_bisbis, fit_modtempxxw_bisbisNL, 
     fit_modtempxxw_callep, fit_modtempxxw_callepNL, 
     fit_modtempxxw_carsco, fit_modtempxxw_carscoNL, 
     fit_modtempxxw_desc, fit_modtempxxw_descNL, 
     fit_modtempxxw_genalg, fit_modtempxxw_genalgNL, 
     fit_modtempxxw_geum, fit_modtempxxw_geumNL, 
     fit_modtempxxw_rarespp, fit_modtempxxw_raresppNL, 
     fit_modtempxxw_tripar, fit_modtempxxw_triparNL, 
     file = "outputs/wstarNL/mod_wstar_tempxxw.RData")
#model comparisons 
summary(fit_modtempxxw_carsco)#NS
summary(fit_modtempxxw_carscoNL)#significant 
waic(fit_modtempxxw_carsco, fit_modtempxxw_carscoNL)

#Ndep
modNdep<-bf(mu|resp_se(sd, sigma = TRUE)~Ndep)
modNdepNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(Ndep))

fit_modNdepxxw_geum<- brm(modNdep, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxw_geumNL<- brm(modNdepNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxw_artsco<- brm(modNdep, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxw_artscoNL<- brm(modNdepNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modNdepxxw_desc<- brm(modNdep, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxw_descNL<- brm(modNdepNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxw_genalg<- brm(modNdep, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxw_genalgNL<- brm(modNdepNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxw_tripar<- brm(modNdep, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxw_triparNL<- brm(modNdepNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxw_rarespp<- brm(modNdep, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxw_raresppNL<- brm(modNdepNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxw_callep<- brm(modNdep, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxw_callepNL<- brm(modNdepNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxw_carsco<- brm(modNdep, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxw_carscoNL<- brm(modNdepNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxw_bisbis<- brm(modNdep, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxxw_bisbisNL<- brm(modNdepNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save(fit_modNdepxxw_artsco,  fit_modNdepxxw_artscoNL,
     fit_modNdepxxw_bisbis, fit_modNdepxxw_bisbisNL, 
     fit_modNdepxxw_callep, fit_modNdepxxw_callepNL, 
     fit_modNdepxxw_carsco, fit_modNdepxxw_carscoNL, 
     fit_modNdepxxw_desc, fit_modNdepxxw_descNL, 
     fit_modNdepxxw_genalg, fit_modNdepxxw_genalgNL, 
     fit_modNdepxxw_geum, fit_modNdepxxw_geumNL, 
     fit_modNdepxxw_rarespp, fit_modNdepxxw_raresppNL, 
     fit_modNdepxxw_tripar, fit_modNdepxxw_triparNL, 
     file = "outputs/wstarNL/mod_wstar_Ndepxxw.RData")

#model comparisons 
summary(fit_modNdepxxw_desc)#sig
summary(fit_modNdepxxw_descNL)#sig
waic(fit_modNdepxxw_desc, fit_modNdepxxw_descNL)#linear fit better but NS


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
modsnow<-bf(mu|resp_se(sd, sigma = TRUE)~depthcm)
modsnowNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(depthcm))

fit_modsnowxnx_geum<- brm(modsnow, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnx_geumNL<- brm(modsnowNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnx_artsco<- brm(modsnow, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnx_artscoNL<- brm(modsnowNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modsnowxnx_desc<- brm(modsnow, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnx_descNL<- brm(modsnowNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnx_genalg<- brm(modsnow, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnx_genalgNL<- brm(modsnowNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnx_tripar<- brm(modsnow, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnx_triparNL<- brm(modsnowNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnx_rarespp<- brm(modsnow, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnx_raresppNL<- brm(modsnowNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnx_callep<- brm(modsnow, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnx_callepNL<- brm(modsnowNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnx_carsco<- brm(modsnow, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnx_carscoNL<- brm(modsnowNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnx_bisbis<- brm(modsnow, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnx_bisbisNL<- brm(modsnowNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save moodels 
save(fit_modsnowxnx_artsco,  fit_modsnowxnx_artscoNL,
     fit_modsnowxnx_bisbis, fit_modsnowxnx_bisbisNL, 
     fit_modsnowxnx_callep, fit_modsnowxnx_callepNL, 
     fit_modsnowxnx_carsco, fit_modsnowxnx_carscoNL, 
     fit_modsnowxnx_desc, fit_modsnowxnx_descNL, 
     fit_modsnowxnx_genalg, fit_modsnowxnx_genalgNL, 
     fit_modsnowxnx_geum, fit_modsnowxnx_geumNL, 
     fit_modsnowxnx_rarespp, fit_modsnowxnx_raresppNL, 
     fit_modsnowxnx_tripar, fit_modsnowxnx_triparNL, 
     file = "outputs/wstarNL/mod_wstar_snowxnx.RData")
#model comparisons 
summary(fit_modsnowxnx_desc)#sig
summary(fit_modsnowxnx_descNL)#NS
waic(fit_modsnowxnx_desc, fit_modsnowxnx_descNL)#linear fit better but NS

#temp
modtemp<-bf(mu|resp_se(sd, sigma = TRUE)~avgT)
modtempNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(avgT))

fit_modtempxnx_geum<- brm(modtemp, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnx_geumNL<- brm(modtempNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnx_artsco<- brm(modtemp, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnx_artscoNL<- brm(modtempNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=10000, family=gaussian())
fit_modtempxnx_desc<- brm(modtemp, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnx_descNL<- brm(modtempNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnx_genalg<- brm(modtemp, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnx_genalgNL<- brm(modtempNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnx_tripar<- brm(modtemp, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnx_triparNL<- brm(modtempNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnx_rarespp<- brm(modtemp, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnx_raresppNL<- brm(modtempNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnx_callep<- brm(modtemp, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnx_callepNL<- brm(modtempNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnx_carsco<- brm(modtemp, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnx_carscoNL<- brm(modtempNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnx_bisbis<- brm(modtemp, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnx_bisbisNL<- brm(modtempNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save(fit_modtempxnx_artsco,  fit_modtempxnx_artscoNL,
     fit_modtempxnx_bisbis, fit_modtempxnx_bisbisNL, 
     fit_modtempxnx_callep, fit_modtempxnx_callepNL, 
     fit_modtempxnx_carsco, fit_modtempxnx_carscoNL, 
     fit_modtempxnx_desc, fit_modtempxnx_descNL, 
     fit_modtempxnx_genalg, fit_modtempxnx_genalgNL, 
     fit_modtempxnx_geum, fit_modtempxnx_geumNL, 
     fit_modtempxnx_rarespp, fit_modtempxnx_raresppNL, 
     fit_modtempxnx_tripar, fit_modtempxnx_triparNL, 
     file = "outputs/wstarNL/mod_wstar_tempxnx.RData")
#model comparisons 
summary(fit_modtempxnx_callep)#sig
summary(fit_modtempxnx_callepNL)#NS
waic(fit_modtempxnx_callep, fit_modtempxnx_callepNL)#linear fit better but NS

#Ndep
modNdep<-bf(mu|resp_se(sd, sigma = TRUE)~Ndep)
modNdepNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(Ndep))

fit_modNdepxnx_geum<- brm(modNdep, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnx_geumNL<- brm(modNdepNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnx_artsco<- brm(modNdep, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnx_artscoNL<- brm(modNdepNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modNdepxnx_desc<- brm(modNdep, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnx_descNL<- brm(modNdepNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnx_genalg<- brm(modNdep, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnx_genalgNL<- brm(modNdepNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnx_tripar<- brm(modNdep, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnx_triparNL<- brm(modNdepNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnx_rarespp<- brm(modNdep, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnx_raresppNL<- brm(modNdepNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnx_callep<- brm(modNdep, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnx_callepNL<- brm(modNdepNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnx_carsco<- brm(modNdep, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnx_carscoNL<- brm(modNdepNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnx_bisbis<- brm(modNdep, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnx_bisbisNL<- brm(modNdepNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save(fit_modNdepxnx_artsco,  fit_modNdepxnx_artscoNL,
     fit_modNdepxnx_bisbis, fit_modNdepxnx_bisbisNL, 
     fit_modNdepxnx_callep, fit_modNdepxnx_callepNL, 
     fit_modNdepxnx_carsco, fit_modNdepxnx_carscoNL, 
     fit_modNdepxnx_desc, fit_modNdepxnx_descNL, 
     fit_modNdepxnx_genalg, fit_modNdepxnx_genalgNL, 
     fit_modNdepxnx_geum, fit_modNdepxnx_geumNL, 
     fit_modNdepxnx_rarespp, fit_modNdepxnx_raresppNL, 
     fit_modNdepxnx_tripar, fit_modNdepxnx_triparNL, 
     file = "outputs/wstarNL/mod_wstar_Ndepxnx.RData")
#model comparisons 
summary(fit_modNdepxnx_tripar)#significant but no error estimated.... 
summary(fit_modNdepxnx_triparNL)#NS 
waic(fit_modNdepxnx_tripar, fit_modNdepxnx_triparNL)#linear fit better but NS


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
modsnow<-bf(mu|resp_se(sd, sigma = TRUE)~depthcm)
modsnowNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(depthcm))

fit_modsnowxnw_geum<- brm(modsnow, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnw_geumNL<- brm(modsnowNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnw_artsco<- brm(modsnow, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnw_artscoNL<- brm(modsnowNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modsnowxnw_desc<- brm(modsnow, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnw_descNL<- brm(modsnowNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnw_genalg<- brm(modsnow, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnw_genalgNL<- brm(modsnowNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnw_tripar<- brm(modsnow, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnw_triparNL<- brm(modsnowNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnw_rarespp<- brm(modsnow, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnw_raresppNL<- brm(modsnowNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnw_callep<- brm(modsnow, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnw_callepNL<- brm(modsnowNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnw_carsco<- brm(modsnow, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnw_carscoNL<- brm(modsnowNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnw_bisbis<- brm(modsnow, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowxnw_bisbisNL<- brm(modsnowNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save moodels 
save(fit_modsnowxnw_artsco,  fit_modsnowxnw_artscoNL,
     fit_modsnowxnw_bisbis, fit_modsnowxnw_bisbisNL, 
     fit_modsnowxnw_callep, fit_modsnowxnw_callepNL, 
     fit_modsnowxnw_carsco, fit_modsnowxnw_carscoNL, 
     fit_modsnowxnw_desc, fit_modsnowxnw_descNL, 
     fit_modsnowxnw_genalg, fit_modsnowxnw_genalgNL, 
     fit_modsnowxnw_geum, fit_modsnowxnw_geumNL, 
     fit_modsnowxnw_rarespp, fit_modsnowxnw_raresppNL, 
     fit_modsnowxnw_tripar, fit_modsnowxnw_triparNL, 
     file = "outputs/wstarNL/mod_wstar_snowxnw.RData")
#model comparisons 
summary(fit_modsnowxnw_artsco)#sig
summary(fit_modsnowxnw_artscoNL)#NS
waic(fit_modsnowxnw_artsco, fit_modsnowxnw_artscoNL)

#temp
modtemp<-bf(mu|resp_se(sd, sigma = TRUE)~avgT)
modtempNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(avgT))

fit_modtempxnw_geum<- brm(modtemp, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnw_geumNL<- brm(modtempNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnw_artsco<- brm(modtemp, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnw_artscoNL<- brm(modtempNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=10000, family=gaussian())
fit_modtempxnw_desc<- brm(modtemp, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnw_descNL<- brm(modtempNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnw_genalg<- brm(modtemp, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnw_genalgNL<- brm(modtempNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnw_tripar<- brm(modtemp, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnw_triparNL<- brm(modtempNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnw_rarespp<- brm(modtemp, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnw_raresppNL<- brm(modtempNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnw_callep<- brm(modtemp, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnw_callepNL<- brm(modtempNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnw_carsco<- brm(modtemp, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnw_carscoNL<- brm(modtempNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnw_bisbis<- brm(modtemp, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtempxnw_bisbisNL<- brm(modtempNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save(fit_modtempxnw_artsco,  fit_modtempxnw_artscoNL,
     fit_modtempxnw_bisbis, fit_modtempxnw_bisbisNL, 
     fit_modtempxnw_callep, fit_modtempxnw_callepNL, 
     fit_modtempxnw_carsco, fit_modtempxnw_carscoNL, 
     fit_modtempxnw_desc, fit_modtempxnw_descNL, 
     fit_modtempxnw_genalg, fit_modtempxnw_genalgNL, 
     fit_modtempxnw_geum, fit_modtempxnw_geumNL, 
     fit_modtempxnw_rarespp, fit_modtempxnw_raresppNL, 
     fit_modtempxnw_tripar, fit_modtempxnw_triparNL, 
     file = "outputs/wstarNL/mod_wstar_tempxnw.RData")
#model summary 
summary(fit_modtempxnw_descNL)#significant linear term 

#Ndep
modNdep<-bf(mu|resp_se(sd, sigma = TRUE)~Ndep)
modNdepNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(Ndep))

fit_modNdepxnw_geum<- brm(modNdep, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnw_geumNL<- brm(modNdepNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnw_artsco<- brm(modNdep, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnw_artscoNL<- brm(modNdepNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modNdepxnw_desc<- brm(modNdep, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnw_descNL<- brm(modNdepNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnw_genalg<- brm(modNdep, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnw_genalgNL<- brm(modNdepNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnw_tripar<- brm(modNdep, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnw_triparNL<- brm(modNdepNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnw_rarespp<- brm(modNdep, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnw_raresppNL<- brm(modNdepNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnw_callep<- brm(modNdep, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnw_callepNL<- brm(modNdepNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnw_carsco<- brm(modNdep, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnw_carscoNL<- brm(modNdepNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnw_bisbis<- brm(modNdep, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdepxnw_bisbisNL<- brm(modNdepNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save(fit_modNdepxnw_artsco,  fit_modNdepxnw_artscoNL,
     fit_modNdepxnw_bisbis, fit_modNdepxnw_bisbisNL, 
     fit_modNdepxnw_callep, fit_modNdepxnw_callepNL, 
     fit_modNdepxnw_carsco, fit_modNdepxnw_carscoNL, 
     fit_modNdepxnw_desc, fit_modNdepxnw_descNL, 
     fit_modNdepxnw_genalg, fit_modNdepxnw_genalgNL, 
     fit_modNdepxnw_geum, fit_modNdepxnw_geumNL, 
     fit_modNdepxnw_rarespp, fit_modNdepxnw_raresppNL, 
     fit_modNdepxnw_tripar, fit_modNdepxnw_triparNL, 
     file = "outputs/wstarNL/mod_wstar_Ndepxnw.RData")
#model comparisons 

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
  facet_wrap(~spp, scales ="free")

ggplot(wstarpxx, aes(x=Ndep, y=mu, col=spp)) +geom_smooth()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")

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
modsnow<-bf(mu|resp_se(sd, sigma = TRUE)~depthcm)
modsnowNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(depthcm))

fit_modsnowpxx_geum<- brm(modsnow, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxx_geumNL<- brm(modsnowNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxx_artsco<- brm(modsnow, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxx_artscoNL<- brm(modsnowNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modsnowpxx_desc<- brm(modsnow, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxx_descNL<- brm(modsnowNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxx_genalg<- brm(modsnow, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxx_genalgNL<- brm(modsnowNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxx_tripar<- brm(modsnow, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxx_triparNL<- brm(modsnowNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxx_rarespp<- brm(modsnow, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxx_raresppNL<- brm(modsnowNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxx_callep<- brm(modsnow, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxx_callepNL<- brm(modsnowNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxx_carsco<- brm(modsnow, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxx_carscoNL<- brm(modsnowNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxx_bisbis<- brm(modsnow, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxx_bisbisNL<- brm(modsnowNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save moodels 
save(fit_modsnowpxx_artsco,  fit_modsnowpxx_artscoNL,
     fit_modsnowpxx_bisbis, fit_modsnowpxx_bisbisNL, 
     fit_modsnowpxx_callep, fit_modsnowpxx_callepNL, 
     fit_modsnowpxx_carsco, fit_modsnowpxx_carscoNL, 
     fit_modsnowpxx_desc, fit_modsnowpxx_descNL, 
     fit_modsnowpxx_genalg, fit_modsnowpxx_genalgNL, 
     fit_modsnowpxx_geum, fit_modsnowpxx_geumNL, 
     fit_modsnowpxx_rarespp, fit_modsnowpxx_raresppNL, 
     fit_modsnowpxx_tripar, fit_modsnowpxx_triparNL, 
     file = "outputs/wstarNL/mod_wstar_snowpxx.RData")
#model comparisons 
summary(fit_modsnowpxx_desc)#sig
summary(fit_modsnowpxx_descNL)#NS
waic(fit_modsnowpxx_desc, fit_modsnowpxx_descNL)

#temp
modtemp<-bf(mu|resp_se(sd, sigma = TRUE)~avgT)
modtempNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(avgT))

fit_modtemppxx_geum<- brm(modtemp, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxx_geumNL<- brm(modtempNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxx_artsco<- brm(modtemp, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxx_artscoNL<- brm(modtempNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=10000, family=gaussian())
fit_modtemppxx_desc<- brm(modtemp, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxx_descNL<- brm(modtempNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxx_genalg<- brm(modtemp, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxx_genalgNL<- brm(modtempNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxx_tripar<- brm(modtemp, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxx_triparNL<- brm(modtempNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxx_rarespp<- brm(modtemp, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxx_raresppNL<- brm(modtempNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxx_callep<- brm(modtemp, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxx_callepNL<- brm(modtempNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxx_carsco<- brm(modtemp, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxx_carscoNL<- brm(modtempNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxx_bisbis<- brm(modtemp, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxx_bisbisNL<- brm(modtempNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save(fit_modtemppxx_artsco,  fit_modtemppxx_artscoNL,
     fit_modtemppxx_bisbis, fit_modtemppxx_bisbisNL, 
     fit_modtemppxx_callep, fit_modtemppxx_callepNL, 
     fit_modtemppxx_carsco, fit_modtemppxx_carscoNL, 
     fit_modtemppxx_desc, fit_modtemppxx_descNL, 
     fit_modtemppxx_genalg, fit_modtemppxx_genalgNL, 
     fit_modtemppxx_geum, fit_modtemppxx_geumNL, 
     fit_modtemppxx_rarespp, fit_modtemppxx_raresppNL, 
     fit_modtemppxx_tripar, fit_modtemppxx_triparNL, 
     file = "outputs/wstarNL/mod_wstar_temppxx.RData")
#model comparisons 
summary(fit_modtemppxx_desc)#sig
summary(fit_modtemppxx_descNL)#sig
waic(fit_modtemppxx_desc,  fit_modtemppxx_descNL)


#Ndep
modNdep<-bf(mu|resp_se(sd, sigma = TRUE)~Ndep)
modNdepNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(Ndep))

fit_modNdeppxx_geum<- brm(modNdep, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxx_geumNL<- brm(modNdepNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxx_artsco<- brm(modNdep, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxx_artscoNL<- brm(modNdepNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modNdeppxx_desc<- brm(modNdep, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxx_descNL<- brm(modNdepNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxx_genalg<- brm(modNdep, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxx_genalgNL<- brm(modNdepNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxx_tripar<- brm(modNdep, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxx_triparNL<- brm(modNdepNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxx_rarespp<- brm(modNdep, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxx_raresppNL<- brm(modNdepNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxx_callep<- brm(modNdep, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxx_callepNL<- brm(modNdepNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxx_carsco<- brm(modNdep, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxx_carscoNL<- brm(modNdepNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxx_bisbis<- brm(modNdep, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxx_bisbisNL<- brm(modNdepNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save(fit_modNdeppxx_artsco,  fit_modNdeppxx_artscoNL,
     fit_modNdeppxx_bisbis, fit_modNdeppxx_bisbisNL, 
     fit_modNdeppxx_callep, fit_modNdeppxx_callepNL, 
     fit_modNdeppxx_carsco, fit_modNdeppxx_carscoNL, 
     fit_modNdeppxx_desc, fit_modNdeppxx_descNL, 
     fit_modNdeppxx_genalg, fit_modNdeppxx_genalgNL, 
     fit_modNdeppxx_geum, fit_modNdeppxx_geumNL, 
     fit_modNdeppxx_rarespp, fit_modNdeppxx_raresppNL, 
     fit_modNdeppxx_tripar, fit_modNdeppxx_triparNL, 
     file = "outputs/wstarNL/mod_wstar_Ndeppxx.RData")
#model comparisons 
summary(fit_modNdeppxx_desc)#sig
summary(fit_modNdeppxx_descNL)#NS
waic(fit_modNdeppxx_desc,  fit_modNdeppxx_descNL)

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
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")

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
modsnow<-bf(mu|resp_se(sd, sigma = TRUE)~depthcm)
modsnowNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(depthcm))

fit_modsnowpxw_geum<- brm(modsnow, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxw_geumNL<- brm(modsnowNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxw_artsco<- brm(modsnow, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxw_artscoNL<- brm(modsnowNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modsnowpxw_desc<- brm(modsnow, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxw_descNL<- brm(modsnowNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxw_genalg<- brm(modsnow, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxw_genalgNL<- brm(modsnowNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxw_tripar<- brm(modsnow, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxw_triparNL<- brm(modsnowNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxw_rarespp<- brm(modsnow, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxw_raresppNL<- brm(modsnowNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxw_callep<- brm(modsnow, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxw_callepNL<- brm(modsnowNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxw_carsco<- brm(modsnow, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxw_carscoNL<- brm(modsnowNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxw_bisbis<- brm(modsnow, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpxw_bisbisNL<- brm(modsnowNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save moodels 
save(fit_modsnowpxw_artsco,  fit_modsnowpxw_artscoNL,
     fit_modsnowpxw_bisbis, fit_modsnowpxw_bisbisNL, 
     fit_modsnowpxw_callep, fit_modsnowpxw_callepNL, 
     fit_modsnowpxw_carsco, fit_modsnowpxw_carscoNL, 
     fit_modsnowpxw_desc, fit_modsnowpxw_descNL, 
     fit_modsnowpxw_genalg, fit_modsnowpxw_genalgNL, 
     fit_modsnowpxw_geum, fit_modsnowpxw_geumNL, 
     fit_modsnowpxw_rarespp, fit_modsnowpxw_raresppNL, 
     fit_modsnowpxw_tripar, fit_modsnowpxw_triparNL, 
     file = "outputs/wstarNL/mod_wstar_snowpxw.RData")
#model comparisons 
#temp
modtemp<-bf(mu|resp_se(sd, sigma = TRUE)~avgT)
modtempNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(avgT))

fit_modtemppxw_geum<- brm(modtemp, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxw_geumNL<- brm(modtempNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxw_artsco<- brm(modtemp, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxw_artscoNL<- brm(modtempNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=10000, family=gaussian())
fit_modtemppxw_desc<- brm(modtemp, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxw_descNL<- brm(modtempNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxw_genalg<- brm(modtemp, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxw_genalgNL<- brm(modtempNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxw_tripar<- brm(modtemp, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxw_triparNL<- brm(modtempNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxw_rarespp<- brm(modtemp, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxw_raresppNL<- brm(modtempNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxw_callep<- brm(modtemp, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxw_callepNL<- brm(modtempNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxw_carsco<- brm(modtemp, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxw_carscoNL<- brm(modtempNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxw_bisbis<- brm(modtemp, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppxw_bisbisNL<- brm(modtempNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save(fit_modtemppxw_artsco,  fit_modtemppxw_artscoNL,
     fit_modtemppxw_bisbis, fit_modtemppxw_bisbisNL, 
     fit_modtemppxw_callep, fit_modtemppxw_callepNL, 
     fit_modtemppxw_carsco, fit_modtemppxw_carscoNL, 
     fit_modtemppxw_desc, fit_modtemppxw_descNL, 
     fit_modtemppxw_genalg, fit_modtemppxw_genalgNL, 
     fit_modtemppxw_geum, fit_modtemppxw_geumNL, 
     fit_modtemppxw_rarespp, fit_modtemppxw_raresppNL, 
     fit_modtemppxw_tripar, fit_modtemppxw_triparNL, 
     file = "outputs/wstarNL/mod_wstar_temppxw.RData")
#model comparisons 

#Ndep
modNdep<-bf(mu|resp_se(sd, sigma = TRUE)~Ndep)
modNdepNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(Ndep))

fit_modNdeppxw_geum<- brm(modNdep, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxw_geumNL<- brm(modNdepNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxw_artsco<- brm(modNdep, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxw_artscoNL<- brm(modNdepNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modNdeppxw_desc<- brm(modNdep, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxw_descNL<- brm(modNdepNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxw_genalg<- brm(modNdep, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxw_genalgNL<- brm(modNdepNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxw_tripar<- brm(modNdep, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxw_triparNL<- brm(modNdepNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxw_rarespp<- brm(modNdep, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxw_raresppNL<- brm(modNdepNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxw_callep<- brm(modNdep, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxw_callepNL<- brm(modNdepNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxw_carsco<- brm(modNdep, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxw_carscoNL<- brm(modNdepNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxw_bisbis<- brm(modNdep, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppxw_bisbisNL<- brm(modNdepNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save(fit_modNdeppxw_artsco,  fit_modNdeppxw_artscoNL,
     fit_modNdeppxw_bisbis, fit_modNdeppxw_bisbisNL, 
     fit_modNdeppxw_callep, fit_modNdeppxw_callepNL, 
     fit_modNdeppxw_carsco, fit_modNdeppxw_carscoNL, 
     fit_modNdeppxw_desc, fit_modNdeppxw_descNL, 
     fit_modNdeppxw_genalg, fit_modNdeppxw_genalgNL, 
     fit_modNdeppxw_geum, fit_modNdeppxw_geumNL, 
     fit_modNdeppxw_rarespp, fit_modNdeppxw_raresppNL, 
     fit_modNdeppxw_tripar, fit_modNdeppxw_triparNL, 
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
modsnow<-bf(mu|resp_se(sd, sigma = TRUE)~depthcm)
modsnowNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(depthcm))

fit_modsnowpnx_geum<- brm(modsnow, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnx_geumNL<- brm(modsnowNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnx_artsco<- brm(modsnow, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnx_artscoNL<- brm(modsnowNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modsnowpnx_desc<- brm(modsnow, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnx_descNL<- brm(modsnowNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnx_genalg<- brm(modsnow, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnx_genalgNL<- brm(modsnowNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnx_tripar<- brm(modsnow, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnx_triparNL<- brm(modsnowNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnx_rarespp<- brm(modsnow, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnx_raresppNL<- brm(modsnowNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnx_callep<- brm(modsnow, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnx_callepNL<- brm(modsnowNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnx_carsco<- brm(modsnow, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnx_carscoNL<- brm(modsnowNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnx_bisbis<- brm(modsnow, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnx_bisbisNL<- brm(modsnowNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save moodels 
save(fit_modsnowpnx_artsco,  fit_modsnowpnx_artscoNL,
     fit_modsnowpnx_bisbis, fit_modsnowpnx_bisbisNL, 
     fit_modsnowpnx_callep, fit_modsnowpnx_callepNL, 
     fit_modsnowpnx_carsco, fit_modsnowpnx_carscoNL, 
     fit_modsnowpnx_desc, fit_modsnowpnx_descNL, 
     fit_modsnowpnx_genalg, fit_modsnowpnx_genalgNL, 
     fit_modsnowpnx_geum, fit_modsnowpnx_geumNL, 
     fit_modsnowpnx_rarespp, fit_modsnowpnx_raresppNL, 
     fit_modsnowpnx_tripar, fit_modsnowpnx_triparNL, 
     file = "outputs/wstarNL/mod_wstar_snowpnx.RData")
#model comparisons 
#temp
modtemp<-bf(mu|resp_se(sd, sigma = TRUE)~avgT)
modtempNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(avgT))

fit_modtemppnx_geum<- brm(modtemp, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnx_geumNL<- brm(modtempNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnx_artsco<- brm(modtemp, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnx_artscoNL<- brm(modtempNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=10000, family=gaussian())
fit_modtemppnx_desc<- brm(modtemp, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnx_descNL<- brm(modtempNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnx_genalg<- brm(modtemp, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnx_genalgNL<- brm(modtempNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnx_tripar<- brm(modtemp, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnx_triparNL<- brm(modtempNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnx_rarespp<- brm(modtemp, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnx_raresppNL<- brm(modtempNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnx_callep<- brm(modtemp, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnx_callepNL<- brm(modtempNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnx_carsco<- brm(modtemp, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnx_carscoNL<- brm(modtempNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnx_bisbis<- brm(modtemp, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnx_bisbisNL<- brm(modtempNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save(fit_modtemppnx_artsco,  fit_modtemppnx_artscoNL,
     fit_modtemppnx_bisbis, fit_modtemppnx_bisbisNL, 
     fit_modtemppnx_callep, fit_modtemppnx_callepNL, 
     fit_modtemppnx_carsco, fit_modtemppnx_carscoNL, 
     fit_modtemppnx_desc, fit_modtemppnx_descNL, 
     fit_modtemppnx_genalg, fit_modtemppnx_genalgNL, 
     fit_modtemppnx_geum, fit_modtemppnx_geumNL, 
     fit_modtemppnx_rarespp, fit_modtemppnx_raresppNL, 
     fit_modtemppnx_tripar, fit_modtemppnx_triparNL, 
     file = "outputs/wstarNL/mod_wstar_temppnx.RData")
#model comparisons 

#Ndep
modNdep<-bf(mu|resp_se(sd, sigma = TRUE)~Ndep)
modNdepNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(Ndep))

fit_modNdeppnx_geum<- brm(modNdep, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnx_geumNL<- brm(modNdepNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnx_artsco<- brm(modNdep, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnx_artscoNL<- brm(modNdepNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modNdeppnx_desc<- brm(modNdep, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnx_descNL<- brm(modNdepNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnx_genalg<- brm(modNdep, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnx_genalgNL<- brm(modNdepNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnx_tripar<- brm(modNdep, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnx_triparNL<- brm(modNdepNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnx_rarespp<- brm(modNdep, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnx_raresppNL<- brm(modNdepNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnx_callep<- brm(modNdep, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnx_callepNL<- brm(modNdepNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnx_carsco<- brm(modNdep, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnx_carscoNL<- brm(modNdepNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnx_bisbis<- brm(modNdep, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnx_bisbisNL<- brm(modNdepNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save(fit_modNdeppnx_artsco,  fit_modNdeppnx_artscoNL,
     fit_modNdeppnx_bisbis, fit_modNdeppnx_bisbisNL, 
     fit_modNdeppnx_callep, fit_modNdeppnx_callepNL, 
     fit_modNdeppnx_carsco, fit_modNdeppnx_carscoNL, 
     fit_modNdeppnx_desc, fit_modNdeppnx_descNL, 
     fit_modNdeppnx_genalg, fit_modNdeppnx_genalgNL, 
     fit_modNdeppnx_geum, fit_modNdeppnx_geumNL, 
     fit_modNdeppnx_rarespp, fit_modNdeppnx_raresppNL, 
     fit_modNdeppnx_tripar, fit_modNdeppnx_triparNL, 
     file = "outputs/wstarNL/mod_wstar_Ndeppnx.RData")
#model comparisons 


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
modsnow<-bf(mu|resp_se(sd, sigma = TRUE)~depthcm)
modsnowNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(depthcm))

fit_modsnowpnw_geum<- brm(modsnow, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnw_geumNL<- brm(modsnowNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnw_artsco<- brm(modsnow, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnw_artscoNL<- brm(modsnowNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modsnowpnw_desc<- brm(modsnow, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnw_descNL<- brm(modsnowNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnw_genalg<- brm(modsnow, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnw_genalgNL<- brm(modsnowNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnw_tripar<- brm(modsnow, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnw_triparNL<- brm(modsnowNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnw_rarespp<- brm(modsnow, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnw_raresppNL<- brm(modsnowNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnw_callep<- brm(modsnow, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnw_callepNL<- brm(modsnowNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnw_carsco<- brm(modsnow, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnw_carscoNL<- brm(modsnowNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnw_bisbis<- brm(modsnow, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modsnowpnw_bisbisNL<- brm(modsnowNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save moodels 
save(fit_modsnowpnw_artsco,  fit_modsnowpnw_artscoNL,
     fit_modsnowpnw_bisbis, fit_modsnowpnw_bisbisNL, 
     fit_modsnowpnw_callep, fit_modsnowpnw_callepNL, 
     fit_modsnowpnw_carsco, fit_modsnowpnw_carscoNL, 
     fit_modsnowpnw_desc, fit_modsnowpnw_descNL, 
     fit_modsnowpnw_genalg, fit_modsnowpnw_genalgNL, 
     fit_modsnowpnw_geum, fit_modsnowpnw_geumNL, 
     fit_modsnowpnw_rarespp, fit_modsnowpnw_raresppNL, 
     fit_modsnowpnw_tripar, fit_modsnowpnw_triparNL, 
     file = "outputs/wstarNL/mod_wstar_snowpnw.RData")
#model comparisons 
#temp
modtemp<-bf(mu|resp_se(sd, sigma = TRUE)~avgT)
modtempNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(avgT))

fit_modtemppnw_geum<- brm(modtemp, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnw_geumNL<- brm(modtempNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnw_artsco<- brm(modtemp, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnw_artscoNL<- brm(modtempNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=10000, family=gaussian())
fit_modtemppnw_desc<- brm(modtemp, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnw_descNL<- brm(modtempNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnw_genalg<- brm(modtemp, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnw_genalgNL<- brm(modtempNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnw_tripar<- brm(modtemp, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnw_triparNL<- brm(modtempNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnw_rarespp<- brm(modtemp, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnw_raresppNL<- brm(modtempNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnw_callep<- brm(modtemp, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnw_callepNL<- brm(modtempNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnw_carsco<- brm(modtemp, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnw_carscoNL<- brm(modtempNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnw_bisbis<- brm(modtemp, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modtemppnw_bisbisNL<- brm(modtempNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save(fit_modtemppnw_artsco,  fit_modtemppnw_artscoNL,
     fit_modtemppnw_bisbis, fit_modtemppnw_bisbisNL, 
     fit_modtemppnw_callep, fit_modtemppnw_callepNL, 
     fit_modtemppnw_carsco, fit_modtemppnw_carscoNL, 
     fit_modtemppnw_desc, fit_modtemppnw_descNL, 
     fit_modtemppnw_genalg, fit_modtemppnw_genalgNL, 
     fit_modtemppnw_geum, fit_modtemppnw_geumNL, 
     fit_modtemppnw_rarespp, fit_modtemppnw_raresppNL, 
     fit_modtemppnw_tripar, fit_modtemppnw_triparNL, 
     file = "outputs/wstarNL/mod_wstar_temppnw.RData")
#model comparisons 

#Ndep
modNdep<-bf(mu|resp_se(sd, sigma = TRUE)~Ndep)
modNdepNL<-bf(mu|resp_se(sd, sigma = TRUE)~s(Ndep))

fit_modNdeppnw_geum<- brm(modNdep, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnw_geumNL<- brm(modNdepNL, data = geum, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnw_artsco<- brm(modNdep, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnw_artscoNL<- brm(modNdepNL, data = artsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=5000, family=gaussian())
fit_modNdeppnw_desc<- brm(modNdep, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnw_descNL<- brm(modNdepNL, data = desc, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnw_genalg<- brm(modNdep, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnw_genalgNL<- brm(modNdepNL, data = genalg, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnw_tripar<- brm(modNdep, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnw_triparNL<- brm(modNdepNL, data = tripar, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnw_rarespp<- brm(modNdep, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnw_raresppNL<- brm(modNdepNL, data = rarespp, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnw_callep<- brm(modNdep, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnw_callepNL<- brm(modNdepNL, data = callep, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnw_carsco<- brm(modNdep, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnw_carscoNL<- brm(modNdepNL, data = carsco, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnw_bisbis<- brm(modNdep, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())
fit_modNdeppnw_bisbisNL<- brm(modNdepNL, data = bisbis, control = list(adapt_delta=0.99, max_treedepth = 15), cores=2, chains=2, iter=2000, family=gaussian())

#save models 
save(fit_modNdeppnw_artsco,  fit_modNdeppnw_artscoNL,
     fit_modNdeppnw_bisbis, fit_modNdeppnw_bisbisNL, 
     fit_modNdeppnw_callep, fit_modNdeppnw_callepNL, 
     fit_modNdeppnw_carsco, fit_modNdeppnw_carscoNL, 
     fit_modNdeppnw_desc, fit_modNdeppnw_descNL, 
     fit_modNdeppnw_genalg, fit_modNdeppnw_genalgNL, 
     fit_modNdeppnw_geum, fit_modNdeppnw_geumNL, 
     fit_modNdeppnw_rarespp, fit_modNdeppnw_raresppNL, 
     fit_modNdeppnw_tripar, fit_modNdeppnw_triparNL, 
     file = "outputs/wstarNL/mod_wstar_Ndeppnw.RData")
#model comparisons 







#model summaries 
#snow-enhanced effect with N, reduced effect with warming 
summary(fit_modsnowxxx_geumNL)#significant smoothing term 
summary(fit_modsnowpnx_geumNL)#significant smoothing term
summary(fit_modsnowpnx_carscoNL)#significant smoothing term
summary(fit_modsnowxnx_descNL)#sig linear term positive 
summary(fit_modsnowxxx_descNL)#weakly sig linear term ~90% positive
summary(fit_modsnowxnx_raresppNL)#weakly sig linear term ~90% positive 

#XXW, PXX, XNW, PNW, PXW snow nothing significant 

#temp-strongest influence in warming treatments (except pxx deschampsia)
summary(fit_modtemppxx_descNL)#significant smoothing term
summary(fit_modtemppxw_descNL) #sig smoothing term 
summary(fit_modtemppnw_descNL) #sig smoothing term
summary(fit_modtemppxw_triparNL) #sig smoothing term 
summary(fit_modtempxxw_carscoNL)#significant smoothing term
summary(fit_modtempxnw_descNL) #sig linear term negative 
summary(fit_modtemppnw_artscoNL) #weakly sig linear term 

#XXX, PNX,XNX temp nothing significant 

#Ndep-any plots where N added, N not significant driver.
#Baseline N only significant when snow added (soil moisture)
summary(fit_modNdeppxx_descNL)#sig linear term negative
#PXX only plot where significant effect of N and it was a negative effect--don't get this??


#extra----
library(bayesplot)
pp_check(fit_modsnow)
pp_check(fit_modsnow, type = "ecdf_overlay")

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
