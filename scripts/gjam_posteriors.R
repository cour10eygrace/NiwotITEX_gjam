library(dplyr)
library(bayestestR)
library(ggplot2)
library(tidyr)
library(ggpubr)

#alphas
#BY TREATMENT----
#XXX----
load(file = "outputs/modDAtime_XXXoutput.RData")
alphas<-as.data.frame(modDAtimeXXX$chains$alphaGibbs)
names<-modDAtimeXXX$parameters$alphaTable$`alpha_{to, from}`
colnames(alphas)<-names

#artsco
artsco<-select(alphas, contains("ARTSCO"))
artsco<-rowwise(artsco)%>%
  mutate(inter_eff=(sum(c_across(`BISBIS, ARTSCO`:`RARESPP, ARTSCO`)))/8)%>%
  mutate(inter_res=(sum(c_across(`ARTSCO, BISBIS`:`ARTSCO, RARESPP`)))/8)%>%
  mutate(intra=`ARTSCO, ARTSCO`)%>%mutate(intra_inter_eff=intra/inter_eff, 
                                          intra_inter_res=intra/inter_res)
artsco_cie<-ci(log(artsco$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
artsco_cie$mu<-mean(log(artsco$intra_inter_eff)) 
artsco_cie$spp<-"ARTSCO"

artsco_cir<-ci(log(artsco$intra_inter_res), ci=c(0.95, 0.9, 0.85))
artsco_cir$mu<-mean(log(artsco$intra_inter_res)) 
artsco_cir$spp<-"ARTSCO"

#carsco
carsco<-select(alphas, contains("CARSCO"))
carsco<-rowwise(carsco)%>%
  mutate(inter1=sum(c_across(`ARTSCO, CARSCO`:`CALLEP, CARSCO`)))%>%
  mutate(inter2=sum(c_across(`DESCAE, CARSCO`:`RARESPP, CARSCO`)))%>%
  mutate(inter3=sum(c_across(`CARSCO, ARTSCO`:`CARSCO, CALLEP`)))%>%
  mutate(inter4=sum(c_across(`CARSCO, DESCAE`:`CARSCO, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`CARSCO, CARSCO`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)

carsco_cie<-ci(log(carsco$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
carsco_cie$mu<-mean(log(carsco$intra_inter_eff)) 
carsco_cie$spp<-"CARSCO"

carsco_cir<-ci(log(carsco$intra_inter_res), ci=c(0.95, 0.9, 0.85))
carsco_cir$mu<-mean(log(carsco$intra_inter_res)) 
carsco_cir$spp<-"CARSCO"


#descae 
descae<-select(alphas, contains("DESCAE"))
descae<-rowwise(descae)%>%
  mutate(inter1=sum(c_across(`ARTSCO, DESCAE`:`CARSCO, DESCAE`)))%>%
  mutate(inter2=sum(c_across(`GENALG, DESCAE`:`RARESPP, DESCAE`)))%>%
  mutate(inter3=sum(c_across(`DESCAE, ARTSCO`:`DESCAE, CARSCO`)))%>%
  mutate(inter4=sum(c_across(`DESCAE, GENALG`:`DESCAE, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`DESCAE, DESCAE`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)

descae_cie<-ci(log(descae$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
descae_cie$mu<-mean(log(descae$intra_inter_eff)) 
descae_cie$spp<-"DESCAE"

descae_cir<-ci(log(descae$intra_inter_res), ci=c(0.95, 0.9, 0.85))
descae_cir$mu<-mean(log(descae$intra_inter_res)) 
descae_cir$spp<-"DESCAE"

#geum  
geuros<-select(alphas, contains("GEUROS"))
geuros<-rowwise(geuros)%>%
  mutate(inter1=sum(c_across(`ARTSCO, GEUROS`:`GENALG, GEUROS`)))%>%
  mutate(inter2=sum(c_across(`TRIPAR, GEUROS`:`RARESPP, GEUROS`)))%>%
  mutate(inter3=sum(c_across(`GEUROS, ARTSCO`:`GEUROS, GENALG`)))%>%
  mutate(inter4=sum(c_across(`GEUROS, TRIPAR`:`GEUROS, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`GEUROS, GEUROS`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)

geuros_cie<-ci(log(geuros$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
geuros_cie$mu<-mean(log(geuros$intra_inter_eff))
geuros_cie$spp<-"GEUROS"

geuros_cir<-ci(log(geuros$intra_inter_res),ci=c(0.95, 0.9, 0.85))
geuros_cir$mu<-mean(log(geuros$intra_inter_res))
geuros_cir$spp<-"GEUROS"

#combine spp 
alphas1e<-rbind(geuros_cie, artsco_cie, descae_cie)
alphas1e$treat<-"XXX"
alphas1r<-rbind(geuros_cir, artsco_cir, descae_cir)
alphas1r$treat<-"XXX"

artsco$spp<-"ARTSCO"
descae$spp<-"DESCAE"
geuros$spp<-"GEUROS"

post1<-select(artsco, intra_inter_eff, intra_inter_res, spp)
post2<-select(descae, intra_inter_eff, intra_inter_res, spp)
post3<-select(geuros, intra_inter_eff, intra_inter_res, spp)

posta<-rbind(post1, post2, post3)
posta$treat<-"XXX"

#effect
XXXplot_eff<-ggplot(posta, aes(x=log(intra_inter_eff),..scaled.., fill=spp))+ geom_density(alpha=0.5)+
  geom_vline(aes(xintercept=0), lty=2, color="red")+xlim(-4,2)+
 xlab("Log Intra/Inter effect competition")+ theme_classic()+ 
ggtitle("Control")

#response 
XXXplot_res<-ggplot(posta, aes(x=log(intra_inter_res),..scaled.., fill=spp))+ geom_density(alpha=0.5)+
  geom_vline(aes(xintercept=0), lty=2, color="red")+xlim(-4,2)+
  xlab("Log Intra/Inter response competition")+ theme_classic()+
  theme(legend.position = "none")+
  ggtitle("Control")

#effect
#in control plots, geum definitely NFD, 
#descae and artsco have means >1 but confidence intervals cross 1

#response 
#deschampsia NFD others are not 

#XNX----
load(file = "outputs/modDAtime_XNXoutput.RData")
alphas<-as.data.frame(modDAtimeXNX$chains$alphaGibbs)
names<-modDAtimeXNX$parameters$alphaTable$`alpha_{to, from}`
colnames(alphas)<-names

#artsco
artsco<-select(alphas, contains("ARTSCO"))
artsco<-rowwise(artsco)%>%
  mutate(inter_eff=(sum(c_across(`BISBIS, ARTSCO`:`RARESPP, ARTSCO`)))/8)%>%
  mutate(inter_res=(sum(c_across(`ARTSCO, BISBIS`:`ARTSCO, RARESPP`)))/8)%>%
  mutate(intra=`ARTSCO, ARTSCO`)%>%mutate(intra_inter_eff=intra/inter_eff, 
                                          intra_inter_res=intra/inter_res)
artsco_cie<-ci(log(artsco$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
artsco_cie$mu<-mean(log(artsco$intra_inter_eff)) 
artsco_cie$spp<-"ARTSCO"

artsco_cir<-ci(log(artsco$intra_inter_res), ci=c(0.95, 0.9, 0.85))
artsco_cir$mu<-mean(log(artsco$intra_inter_res)) 
artsco_cir$spp<-"ARTSCO"

#descae 
descae<-select(alphas, contains("DESCAE"))
descae<-rowwise(descae)%>%
  mutate(inter1=sum(c_across(`ARTSCO, DESCAE`:`CARSCO, DESCAE`)))%>%
  mutate(inter2=sum(c_across(`GENALG, DESCAE`:`RARESPP, DESCAE`)))%>%
  mutate(inter3=sum(c_across(`DESCAE, ARTSCO`:`DESCAE, CARSCO`)))%>%
  mutate(inter4=sum(c_across(`DESCAE, GENALG`:`DESCAE, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`DESCAE, DESCAE`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)

descae_cie<-ci(log(descae$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
descae_cie$mu<-mean(log(descae$intra_inter_eff)) 
descae_cie$spp<-"DESCAE"

descae_cir<-ci(log(descae$intra_inter_res), ci=c(0.95, 0.9, 0.85))
descae_cir$mu<-mean(log(descae$intra_inter_res)) 
descae_cir$spp<-"DESCAE"

#geum  
geuros<-select(alphas, contains("GEUROS"))
geuros<-rowwise(geuros)%>%
  mutate(inter1=sum(c_across(`ARTSCO, GEUROS`:`GENALG, GEUROS`)))%>%
  mutate(inter2=sum(c_across(`TRIPAR, GEUROS`:`RARESPP, GEUROS`)))%>%
  mutate(inter3=sum(c_across(`GEUROS, ARTSCO`:`GEUROS, GENALG`)))%>%
  mutate(inter4=sum(c_across(`GEUROS, TRIPAR`:`GEUROS, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`GEUROS, GEUROS`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)

geuros_cie<-ci(log(geuros$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
geuros_cie$mu<-mean(log(geuros$intra_inter_eff))
geuros_cie$spp<-"GEUROS"

geuros_cir<-ci(log(geuros$intra_inter_res),ci=c(0.95, 0.9, 0.85))
geuros_cir$mu<-mean(log(geuros$intra_inter_res))
geuros_cir$spp<-"GEUROS"

#combine spp
alphas2e<-rbind(geuros_cie, artsco_cie, descae_cie)
alphas2e$treat<-"XNX"
alphas2r<-rbind(geuros_cir, artsco_cir, descae_cir)
alphas2r$treat<-"XNX"

artsco$spp<-"ARTSCO"
descae$spp<-"DESCAE"
geuros$spp<-"GEUROS"

post1<-select(artsco, intra_inter_eff, intra_inter_res, spp)
post2<-select(descae, intra_inter_eff, intra_inter_res, spp)
post3<-select(geuros, intra_inter_eff, intra_inter_res, spp)

postb<-rbind(post1, post2, post3)
postb$treat<-"XNX"

#effect
XNXplot_eff<-ggplot(postb, aes(x=log(intra_inter_eff),..scaled.., fill=spp))+ geom_density(alpha=0.5)+
  geom_vline(aes(xintercept=0), lty=2, color="red")+xlim(-4,2)+
  xlab("Log Intra/Inter effect competition")+ theme_classic()+ 
  ggtitle("Nitrogen addition")

#response 
XNXplot_res<-ggplot(postb, aes(x=log(intra_inter_res),..scaled.., fill=spp))+ geom_density(alpha=0.5)+
 geom_vline(aes(xintercept=0), lty=2, color="red")+xlim(-4,2)+
  xlab("Log Intra/Inter response competition")+ theme_classic() +#theme(legend.position = "none")+
  ggtitle("Nitrogen addition")

#effect and response 
#under N, Descae shifts to definitely PFD, 
#Geum and Artsco means >1 but confidence intervals cross 1

#XXW----
load(file = "outputs/modDAtime_XXWoutput.RData")
alphas<-as.data.frame(modDAtimeXXW$chains$alphaGibbs)
names<-modDAtimeXXW$parameters$alphaTable$`alpha_{to, from}`
colnames(alphas)<-names

#artsco
artsco<-select(alphas, contains("ARTSCO"))
artsco<-rowwise(artsco)%>%
  mutate(inter_eff=(sum(c_across(`BISBIS, ARTSCO`:`RARESPP, ARTSCO`)))/8)%>%
  mutate(inter_res=(sum(c_across(`ARTSCO, BISBIS`:`ARTSCO, RARESPP`)))/8)%>%
  mutate(intra=`ARTSCO, ARTSCO`)%>%mutate(intra_inter_eff=intra/inter_eff, 
                                          intra_inter_res=intra/inter_res)
artsco_cie<-ci(log(artsco$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
artsco_cie$mu<-mean(log(artsco$intra_inter_eff)) 
artsco_cie$spp<-"ARTSCO"

artsco_cir<-ci(log(artsco$intra_inter_res), ci=c(0.95, 0.9, 0.85))
artsco_cir$mu<-mean(log(artsco$intra_inter_res)) 
artsco_cir$spp<-"ARTSCO"

#descae 
descae<-select(alphas, contains("DESCAE"))
descae<-rowwise(descae)%>%
  mutate(inter1=sum(c_across(`ARTSCO, DESCAE`:`CARSCO, DESCAE`)))%>%
  mutate(inter2=sum(c_across(`GENALG, DESCAE`:`RARESPP, DESCAE`)))%>%
  mutate(inter3=sum(c_across(`DESCAE, ARTSCO`:`DESCAE, CARSCO`)))%>%
  mutate(inter4=sum(c_across(`DESCAE, GENALG`:`DESCAE, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`DESCAE, DESCAE`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)

descae_cie<-ci(log(descae$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
descae_cie$mu<-mean(log(descae$intra_inter_eff)) 
descae_cie$spp<-"DESCAE"

descae_cir<-ci(log(descae$intra_inter_res), ci=c(0.95, 0.9, 0.85))
descae_cir$mu<-mean(log(descae$intra_inter_res)) 
descae_cir$spp<-"DESCAE"

#geum  
geuros<-select(alphas, contains("GEUROS"))
geuros<-rowwise(geuros)%>%
  mutate(inter1=sum(c_across(`ARTSCO, GEUROS`:`GENALG, GEUROS`)))%>%
  mutate(inter2=sum(c_across(`TRIPAR, GEUROS`:`RARESPP, GEUROS`)))%>%
  mutate(inter3=sum(c_across(`GEUROS, ARTSCO`:`GEUROS, GENALG`)))%>%
  mutate(inter4=sum(c_across(`GEUROS, TRIPAR`:`GEUROS, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`GEUROS, GEUROS`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)

geuros_cie<-ci(log(geuros$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
geuros_cie$mu<-mean(log(geuros$intra_inter_eff))
geuros_cie$spp<-"GEUROS"

geuros_cir<-ci(log(geuros$intra_inter_res),ci=c(0.95, 0.9, 0.85))
geuros_cir$mu<-mean(log(geuros$intra_inter_res))
geuros_cir$spp<-"GEUROS"

#combine spp 
alphas3e<-rbind(geuros_cie, artsco_cie, descae_cie)
alphas3e$treat<-"XXW"
alphas3r<-rbind(geuros_cir, artsco_cir, descae_cir)
alphas3r$treat<-"XXW"

artsco$spp<-"ARTSCO"
descae$spp<-"DESCAE"
geuros$spp<-"GEUROS"

post1<-select(artsco, intra_inter_eff, intra_inter_res, spp)
post2<-select(descae, intra_inter_eff, intra_inter_res, spp)
post3<-select(geuros, intra_inter_eff, intra_inter_res, spp)

postc<-rbind(post1, post2, post3)
postc$treat<-"XXW"

#effect
XXWplot_eff<-ggplot(postc, aes(x=log(intra_inter_eff),..scaled.., fill=spp))+ geom_density(alpha=0.5)+
  geom_vline(aes(xintercept=0), lty=2, color="red")+xlim(-4,2)+
  xlab("Log Intra/Inter effect competition")+ theme_classic()+ 
  ggtitle("Warming")

#response 
XXWplot_res<-ggplot(postc, aes(log(x=intra_inter_res),..scaled.., fill=spp))+ geom_density(alpha=0.5)+
  geom_vline(aes(xintercept=0), lty=2, color="red")+xlim(-4,2)+
  xlab("Log Intra/Inter response competition")+ theme_classic()+ # theme(legend.position = "none")+
  ggtitle("Warming")

#nothing significantly different from 1 under warming 

#PXX----
load(file = "outputs/modDAtime_PXXoutput.RData")
alphas<-as.data.frame(modDAtimePXX$chains$alphaGibbs)
names<-modDAtimePXX$parameters$alphaTable$`alpha_{to, from}`
colnames(alphas)<-names

#artsco
artsco<-select(alphas, contains("ARTSCO"))
artsco<-rowwise(artsco)%>%
  mutate(inter_eff=(sum(c_across(`BISBIS, ARTSCO`:`RARESPP, ARTSCO`)))/8)%>%
  mutate(inter_res=(sum(c_across(`ARTSCO, BISBIS`:`ARTSCO, RARESPP`)))/8)%>%
  mutate(intra=`ARTSCO, ARTSCO`)%>%mutate(intra_inter_eff=intra/inter_eff, 
                                          intra_inter_res=intra/inter_res)
artsco_cie<-ci(log(artsco$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
artsco_cie$mu<-mean(log(artsco$intra_inter_eff)) 
artsco_cie$spp<-"ARTSCO"

artsco_cir<-ci(log(artsco$intra_inter_res), ci=c(0.95, 0.9, 0.85))
artsco_cir$mu<-mean(log(artsco$intra_inter_res)) 
artsco_cir$spp<-"ARTSCO"

#descae 
descae<-select(alphas, contains("DESCAE"))
descae<-rowwise(descae)%>%
  mutate(inter1=sum(c_across(`ARTSCO, DESCAE`:`CARSCO, DESCAE`)))%>%
  mutate(inter2=sum(c_across(`GENALG, DESCAE`:`RARESPP, DESCAE`)))%>%
  mutate(inter3=sum(c_across(`DESCAE, ARTSCO`:`DESCAE, CARSCO`)))%>%
  mutate(inter4=sum(c_across(`DESCAE, GENALG`:`DESCAE, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`DESCAE, DESCAE`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)

descae_cie<-ci(log(descae$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
descae_cie$mu<-mean(log(descae$intra_inter_eff)) 
descae_cie$spp<-"DESCAE"

descae_cir<-ci(log(descae$intra_inter_res), ci=c(0.95, 0.9, 0.85))
descae_cir$mu<-mean(log(descae$intra_inter_res)) 
descae_cir$spp<-"DESCAE"

#geum  
geuros<-select(alphas, contains("GEUROS"))
geuros<-rowwise(geuros)%>%
  mutate(inter1=sum(c_across(`ARTSCO, GEUROS`:`GENALG, GEUROS`)))%>%
  mutate(inter2=sum(c_across(`TRIPAR, GEUROS`:`RARESPP, GEUROS`)))%>%
  mutate(inter3=sum(c_across(`GEUROS, ARTSCO`:`GEUROS, GENALG`)))%>%
  mutate(inter4=sum(c_across(`GEUROS, TRIPAR`:`GEUROS, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`GEUROS, GEUROS`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)

geuros_cie<-ci(log(geuros$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
geuros_cie$mu<-mean(log(geuros$intra_inter_eff))
geuros_cie$spp<-"GEUROS"

geuros_cir<-ci(log(geuros$intra_inter_res),ci=c(0.95, 0.9, 0.85))
geuros_cir$mu<-mean(log(geuros$intra_inter_res))
geuros_cir$spp<-"GEUROS"

#combines spp
alphas4e<-rbind(geuros_cie, artsco_cie, descae_cie)
alphas4e$treat<-"PXX"
alphas4r<-rbind(geuros_cir, artsco_cir, descae_cir)
alphas4r$treat<-"PXX"

artsco$spp<-"ARTSCO"
descae$spp<-"DESCAE"
geuros$spp<-"GEUROS"

post1<-select(artsco, intra_inter_eff, intra_inter_res, spp)
post2<-select(descae, intra_inter_eff, intra_inter_res, spp)
post3<-select(geuros, intra_inter_eff, intra_inter_res, spp)

postd<-rbind(post1, post2, post3)
postd$treat<-"PXX"

#effect
PXXplot_eff<-ggplot(postd, aes(x=log(intra_inter_eff),..scaled.., fill=spp))+ geom_density(alpha=0.5)+
  geom_vline(aes(xintercept=0), lty=2, color="red")+xlim(-4,2)+
  xlab("Log Intra/Inter effect competition")+ theme_classic()+ 
  ggtitle("Snow addition")

#response 
PXXplot_res<-ggplot(postd, aes(x=log(intra_inter_res),..scaled.., fill=spp))+ geom_density(alpha=0.5)+
  geom_vline(aes(xintercept=0), lty=2, color="red")+xlim(-4,2)+
  xlab("Log Intra/Inter response competition")+ theme_classic()+ # theme(legend.position = "none")+
  ggtitle("Snow addition")

#PNX----
load(file = "outputs/modDAtime_PNXoutput.RData")
alphas<-as.data.frame(modDAtimePNX$chains$alphaGibbs)
names<-modDAtimePNX$parameters$alphaTable$`alpha_{to, from}`
colnames(alphas)<-names

#artsco
artsco<-select(alphas, contains("ARTSCO"))
artsco<-rowwise(artsco)%>%
  mutate(inter_eff=(sum(c_across(`BISBIS, ARTSCO`:`RARESPP, ARTSCO`)))/8)%>%
  mutate(inter_res=(sum(c_across(`ARTSCO, BISBIS`:`ARTSCO, RARESPP`)))/8)%>%
  mutate(intra=`ARTSCO, ARTSCO`)%>%mutate(intra_inter_eff=intra/inter_eff, 
                                          intra_inter_res=intra/inter_res)
artsco_cie<-ci(log(artsco$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
artsco_cie$mu<-mean(log(artsco$intra_inter_eff)) 
artsco_cie$spp<-"ARTSCO"

artsco_cir<-ci(log(artsco$intra_inter_res), ci=c(0.95, 0.9, 0.85))
artsco_cir$mu<-mean(log(artsco$intra_inter_res)) 
artsco_cir$spp<-"ARTSCO"

#descae 
descae<-select(alphas, contains("DESCAE"))
descae<-rowwise(descae)%>%
  mutate(inter1=sum(c_across(`ARTSCO, DESCAE`:`CARSCO, DESCAE`)))%>%
  mutate(inter2=sum(c_across(`GENALG, DESCAE`:`RARESPP, DESCAE`)))%>%
  mutate(inter3=sum(c_across(`DESCAE, ARTSCO`:`DESCAE, CARSCO`)))%>%
  mutate(inter4=sum(c_across(`DESCAE, GENALG`:`DESCAE, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`DESCAE, DESCAE`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)

descae_cie<-ci(log(descae$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
descae_cie$mu<-mean(log(descae$intra_inter_eff)) 
descae_cie$spp<-"DESCAE"

descae_cir<-ci(log(descae$intra_inter_res), ci=c(0.95, 0.9, 0.85))
descae_cir$mu<-mean(log(descae$intra_inter_res)) 
descae_cir$spp<-"DESCAE"

#geum  
geuros<-select(alphas, contains("GEUROS"))
geuros<-rowwise(geuros)%>%
  mutate(inter1=sum(c_across(`ARTSCO, GEUROS`:`GENALG, GEUROS`)))%>%
  mutate(inter2=sum(c_across(`TRIPAR, GEUROS`:`RARESPP, GEUROS`)))%>%
  mutate(inter3=sum(c_across(`GEUROS, ARTSCO`:`GEUROS, GENALG`)))%>%
  mutate(inter4=sum(c_across(`GEUROS, TRIPAR`:`GEUROS, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`GEUROS, GEUROS`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)

geuros_cie<-ci(log(geuros$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
geuros_cie$mu<-mean(log(geuros$intra_inter_eff))
geuros_cie$spp<-"GEUROS"

geuros_cir<-ci(log(geuros$intra_inter_res),ci=c(0.95, 0.9, 0.85))
geuros_cir$mu<-mean(log(geuros$intra_inter_res))
geuros_cir$spp<-"GEUROS"

#Combine spp 
alphas5e<-rbind(geuros_cie, artsco_cie, descae_cie)
alphas5e$treat<-"PNX"
alphas5r<-rbind(geuros_cir, artsco_cir, descae_cir)
alphas5r$treat<-"PNX"

artsco$spp<-"ARTSCO"
descae$spp<-"DESCAE"
geuros$spp<-"GEUROS"

post1<-select(artsco, intra_inter_eff, intra_inter_res, spp)
post2<-select(descae, intra_inter_eff, intra_inter_res, spp)
post3<-select(geuros, intra_inter_eff, intra_inter_res, spp)

poste<-rbind(post1, post2, post3)
poste$treat<-"PNX"

#effect
PNXplot_eff<-ggplot(poste, aes(x=log(intra_inter_eff),..scaled.., fill=spp))+ geom_density(alpha=0.5)+
  geom_vline(aes(xintercept=0), lty=2, color="red")+xlim(-4,2)+
  xlab("Log Intra/Inter effect competition")+ theme_classic()+ 
  ggtitle("Snow + N")

#response 
PNXplot_res<-ggplot(poste, aes(x=log(intra_inter_res),..scaled.., fill=spp))+ geom_density(alpha=0.5)+
  geom_vline(aes(xintercept=0), lty=2, color="red")+xlim(-4,2)+
  xlab("Log Intra/Inter response competition")+ theme_classic()+ # theme(legend.position = "none")+
  ggtitle("Snow + N ")

#PNW----
load(file = "outputs/modDAtime_PNWoutput.RData")
alphas<-as.data.frame(modDAtimePNW$chains$alphaGibbs)
names<-modDAtimePNW$parameters$alphaTable$`alpha_{to, from}`
colnames(alphas)<-names

#artsco
artsco<-select(alphas, contains("ARTSCO"))
artsco<-rowwise(artsco)%>%
  mutate(inter_eff=(sum(c_across(`BISBIS, ARTSCO`:`RARESPP, ARTSCO`)))/8)%>%
  mutate(inter_res=(sum(c_across(`ARTSCO, BISBIS`:`ARTSCO, RARESPP`)))/8)%>%
  mutate(intra=`ARTSCO, ARTSCO`)%>%mutate(intra_inter_eff=intra/inter_eff, 
                                          intra_inter_res=intra/inter_res)
artsco_cie<-ci(log(artsco$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
artsco_cie$mu<-mean(log(artsco$intra_inter_eff)) 
artsco_cie$spp<-"ARTSCO"

artsco_cir<-ci(log(artsco$intra_inter_res), ci=c(0.95, 0.9, 0.85))
artsco_cir$mu<-mean(log(artsco$intra_inter_res)) 
artsco_cir$spp<-"ARTSCO"

#descae 
descae<-select(alphas, contains("DESCAE"))
descae<-rowwise(descae)%>%
  mutate(inter1=sum(c_across(`ARTSCO, DESCAE`:`CARSCO, DESCAE`)))%>%
  mutate(inter2=sum(c_across(`GENALG, DESCAE`:`RARESPP, DESCAE`)))%>%
  mutate(inter3=sum(c_across(`DESCAE, ARTSCO`:`DESCAE, CARSCO`)))%>%
  mutate(inter4=sum(c_across(`DESCAE, GENALG`:`DESCAE, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`DESCAE, DESCAE`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)

descae_cie<-ci(log(descae$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
descae_cie$mu<-mean(log(descae$intra_inter_eff)) 
descae_cie$spp<-"DESCAE"

descae_cir<-ci(log(descae$intra_inter_res), ci=c(0.95, 0.9, 0.85))
descae_cir$mu<-mean(log(descae$intra_inter_res)) 
descae_cir$spp<-"DESCAE"

#geum  
geuros<-select(alphas, contains("GEUROS"))
geuros<-rowwise(geuros)%>%
  mutate(inter1=sum(c_across(`ARTSCO, GEUROS`:`GENALG, GEUROS`)))%>%
  mutate(inter2=sum(c_across(`TRIPAR, GEUROS`:`RARESPP, GEUROS`)))%>%
  mutate(inter3=sum(c_across(`GEUROS, ARTSCO`:`GEUROS, GENALG`)))%>%
  mutate(inter4=sum(c_across(`GEUROS, TRIPAR`:`GEUROS, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`GEUROS, GEUROS`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)

geuros_cie<-ci(log(geuros$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
geuros_cie$mu<-mean(log(geuros$intra_inter_eff))
geuros_cie$spp<-"GEUROS"

geuros_cir<-ci(log(geuros$intra_inter_res),ci=c(0.95, 0.9, 0.85))
geuros_cir$mu<-mean(log(geuros$intra_inter_res))
geuros_cir$spp<-"GEUROS"

#Combine spp 
alphas6e<-rbind(geuros_cie, artsco_cie, descae_cie)
alphas6e$treat<-"PNW"
alphas6r<-rbind(geuros_cir, artsco_cir, descae_cir)
alphas6r$treat<-"PNW"

artsco$spp<-"ARTSCO"
descae$spp<-"DESCAE"
geuros$spp<-"GEUROS"

post1<-select(artsco, intra_inter_eff, intra_inter_res, spp)
post2<-select(descae, intra_inter_eff, intra_inter_res, spp)
post3<-select(geuros, intra_inter_eff, intra_inter_res, spp)

postf<-rbind(post1, post2, post3)
postf$treat<-"PNW"

#effect
PNWplot_eff<-ggplot(postf, aes(x=log(intra_inter_eff),..scaled.., fill=spp))+ geom_density(alpha=0.5)+
  geom_vline(aes(xintercept=0), lty=2, color="red")+xlim(-4,2)+
  xlab("Log Intra/Inter effect competition")+ theme_classic()+ 
  ggtitle("Warming + Snow + N")

#response 
PNWplot_res<-ggplot(postf, aes(x=log(intra_inter_res),..scaled.., fill=spp))+ 
  geom_density(alpha=0.5, geom='area')+
  geom_vline(aes(xintercept=0), lty=2, color="red")+xlim(-4,2)+
  xlab("Log Intra/Inter response competition")+ theme_classic()+  #theme(legend.position = "none")+
  ggtitle("Warming + Snow + N")



#XNW----
load(file = "outputs/modDAtime_XNWoutput.RData")
alphas<-as.data.frame(modDAtimeXNW$chains$alphaGibbs)
names<-modDAtimeXNW$parameters$alphaTable$`alpha_{to, from}`
colnames(alphas)<-names

#artsco
artsco<-select(alphas, contains("ARTSCO"))
artsco<-rowwise(artsco)%>%
  mutate(inter_eff=(sum(c_across(`BISBIS, ARTSCO`:`RARESPP, ARTSCO`)))/8)%>%
  mutate(inter_res=(sum(c_across(`ARTSCO, BISBIS`:`ARTSCO, RARESPP`)))/8)%>%
  mutate(intra=`ARTSCO, ARTSCO`)%>%mutate(intra_inter_eff=intra/inter_eff, 
                                          intra_inter_res=intra/inter_res)
artsco_cie<-ci(log(artsco$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
artsco_cie$mu<-mean(log(artsco$intra_inter_eff)) 
artsco_cie$spp<-"ARTSCO"

artsco_cir<-ci(log(artsco$intra_inter_res), ci=c(0.95, 0.9, 0.85))
artsco_cir$mu<-mean(log(artsco$intra_inter_res)) 
artsco_cir$spp<-"ARTSCO"

#descae 
descae<-select(alphas, contains("DESCAE"))
descae<-rowwise(descae)%>%
  mutate(inter1=sum(c_across(`ARTSCO, DESCAE`:`CARSCO, DESCAE`)))%>%
  mutate(inter2=sum(c_across(`GENALG, DESCAE`:`RARESPP, DESCAE`)))%>%
  mutate(inter3=sum(c_across(`DESCAE, ARTSCO`:`DESCAE, CARSCO`)))%>%
  mutate(inter4=sum(c_across(`DESCAE, GENALG`:`DESCAE, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`DESCAE, DESCAE`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)

descae_cie<-ci(log(descae$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
descae_cie$mu<-mean(log(descae$intra_inter_eff)) 
descae_cie$spp<-"DESCAE"

descae_cir<-ci(log(descae$intra_inter_res), ci=c(0.95, 0.9, 0.85))
descae_cir$mu<-mean(log(descae$intra_inter_res)) 
descae_cir$spp<-"DESCAE"

#geum  
geuros<-select(alphas, contains("GEUROS"))
geuros<-rowwise(geuros)%>%
  mutate(inter1=sum(c_across(`ARTSCO, GEUROS`:`GENALG, GEUROS`)))%>%
  mutate(inter2=sum(c_across(`TRIPAR, GEUROS`:`RARESPP, GEUROS`)))%>%
  mutate(inter3=sum(c_across(`GEUROS, ARTSCO`:`GEUROS, GENALG`)))%>%
  mutate(inter4=sum(c_across(`GEUROS, TRIPAR`:`GEUROS, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`GEUROS, GEUROS`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)

geuros_cie<-ci(log(geuros$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
geuros_cie$mu<-mean(log(geuros$intra_inter_eff))
geuros_cie$spp<-"GEUROS"

geuros_cir<-ci(log(geuros$intra_inter_res),ci=c(0.95, 0.9, 0.85))
geuros_cir$mu<-mean(log(geuros$intra_inter_res))
geuros_cir$spp<-"GEUROS"

#Combine spp 
alphas7e<-rbind(geuros_cie, artsco_cie, descae_cie)
alphas7e$treat<-"XNW"
alphas7r<-rbind(geuros_cir, artsco_cir, descae_cir)
alphas7r$treat<-"XNW"

artsco$spp<-"ARTSCO"
descae$spp<-"DESCAE"
geuros$spp<-"GEUROS"

post1<-select(artsco, intra_inter_eff, intra_inter_res, spp)
post2<-select(descae, intra_inter_eff, intra_inter_res, spp)
post3<-select(geuros, intra_inter_eff, intra_inter_res, spp)

postg<-rbind(post1, post2, post3)
postg$treat<-"XNW"

#effect
XNWplot_eff<-ggplot(postg, aes(x=log(intra_inter_eff),..scaled.., fill=spp))+ geom_density(alpha=0.5)+
  geom_vline(aes(xintercept=0), lty=2, color="red")+xlim(-4,2)+
  xlab("Log Intra/Inter effect competition")+ theme_classic()+ 
  ggtitle("Warming + N")

#response 
XNWplot_res<-ggplot(postg, aes(x=log(intra_inter_res),..scaled.., fill=spp))+ 
  geom_density(alpha=0.5, geom='area')+
  geom_vline(aes(xintercept=0), lty=2, color="red")+xlim(-4,2)+
  xlab("Log Intra/Inter response competition")+ theme_classic()+ # theme(legend.position = "none")+
  ggtitle("Warming + N")



#PXW----
load(file = "outputs/modDAtime_PXWoutput.RData")
alphas<-as.data.frame(modDAtimePXW$chains$alphaGibbs)
names<-modDAtimePXW$parameters$alphaTable$`alpha_{to, from}`
colnames(alphas)<-names

#artsco
artsco<-select(alphas, contains("ARTSCO"))
artsco<-rowwise(artsco)%>%
  mutate(inter_eff=(sum(c_across(`BISBIS, ARTSCO`:`RARESPP, ARTSCO`)))/8)%>%
  mutate(inter_res=(sum(c_across(`ARTSCO, BISBIS`:`ARTSCO, RARESPP`)))/8)%>%
  mutate(intra=`ARTSCO, ARTSCO`)%>%mutate(intra_inter_eff=intra/inter_eff, 
                                          intra_inter_res=intra/inter_res)
artsco_cie<-ci(log(artsco$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
artsco_cie$mu<-mean(log(artsco$intra_inter_eff)) 
artsco_cie$spp<-"ARTSCO"

artsco_cir<-ci(log(artsco$intra_inter_res), ci=c(0.95, 0.9, 0.85))
artsco_cir$mu<-mean(log(artsco$intra_inter_res)) 
artsco_cir$spp<-"ARTSCO"

#descae 
descae<-select(alphas, contains("DESCAE"))
descae<-rowwise(descae)%>%
  mutate(inter1=sum(c_across(`ARTSCO, DESCAE`:`CARSCO, DESCAE`)))%>%
  mutate(inter2=sum(c_across(`GENALG, DESCAE`:`RARESPP, DESCAE`)))%>%
  mutate(inter3=sum(c_across(`DESCAE, ARTSCO`:`DESCAE, CARSCO`)))%>%
  mutate(inter4=sum(c_across(`DESCAE, GENALG`:`DESCAE, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`DESCAE, DESCAE`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)

descae_cie<-ci(log(descae$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
descae_cie$mu<-mean(log(descae$intra_inter_eff)) 
descae_cie$spp<-"DESCAE"

descae_cir<-ci(log(descae$intra_inter_res), ci=c(0.95, 0.9, 0.85))
descae_cir$mu<-mean(log(descae$intra_inter_res)) 
descae_cir$spp<-"DESCAE"

#geum  
geuros<-select(alphas, contains("GEUROS"))
geuros<-rowwise(geuros)%>%
  mutate(inter1=sum(c_across(`ARTSCO, GEUROS`:`GENALG, GEUROS`)))%>%
  mutate(inter2=sum(c_across(`TRIPAR, GEUROS`:`RARESPP, GEUROS`)))%>%
  mutate(inter3=sum(c_across(`GEUROS, ARTSCO`:`GEUROS, GENALG`)))%>%
  mutate(inter4=sum(c_across(`GEUROS, TRIPAR`:`GEUROS, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`GEUROS, GEUROS`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)

geuros_cie<-ci(log(geuros$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
geuros_cie$mu<-mean(log(geuros$intra_inter_eff))
geuros_cie$spp<-"GEUROS"

geuros_cir<-ci(log(geuros$intra_inter_res),ci=c(0.95, 0.9, 0.85))
geuros_cir$mu<-mean(log(geuros$intra_inter_res))
geuros_cir$spp<-"GEUROS"

#Combine spp 
alphas8e<-rbind(geuros_cie, artsco_cie, descae_cie)
alphas8e$treat<-"PXW"
alphas8r<-rbind(geuros_cir, artsco_cir, descae_cir)
alphas8r$treat<-"PXW"

artsco$spp<-"ARTSCO"
descae$spp<-"DESCAE"
geuros$spp<-"GEUROS"

post1<-select(artsco, intra_inter_eff, intra_inter_res, spp)
post2<-select(descae, intra_inter_eff, intra_inter_res, spp)
post3<-select(geuros, intra_inter_eff, intra_inter_res, spp)

posth<-rbind(post1, post2, post3)
posth$treat<-"PXW"

#effect
PXWplot_eff<-ggplot(posth, aes(x=log(intra_inter_eff),..scaled.., fill=spp))+ geom_density(alpha=0.5)+
  geom_vline(aes(xintercept=0), lty=2, color="red")+xlim(-4,2)+
  xlab("Log Intra/Inter effect competition")+ theme_classic()+ 
  ggtitle("Warming + Snow ")

#response 
PXWplot_res<-ggplot(posth, aes(x=log(intra_inter_res),..scaled.., fill=spp))+ 
  geom_density(alpha=0.5, geom='area')+
  geom_vline(aes(xintercept=0), lty=2, color="red")+xlim(-4,2)+
  xlab("Log Intra/Inter response competition")+ theme_classic()+ # theme(legend.position = "none")+
  ggtitle("Warming + Snow ")

#combine all treatments----
alphas_eff<-rbind(alphas1e, alphas2e, alphas3e, alphas4e, alphas5e, alphas6e)
alphas_res<-rbind(alphas1r, alphas2r, alphas3r, alphas4r, alphas5r, alphas6r)

#plot 
ggarrange(XXXplot_res, XXWplot_res, PXXplot_res, XNXplot_res, PXWplot_res, 
          XNWplot_res, PNXplot_res,  PNWplot_res, common.legend = TRUE, legend = "right") 

gridExtra::grid.arrange(XXXplot_res, XXWplot_res, PXXplot_res, XNXplot_res, PXWplot_res, 
                        XNWplot_res, PNXplot_res,  PNWplot_res) 

#plot just N treatments 
gridExtra::grid.arrange(XXXplot_res, XNXplot_res, PNXplot_res, 
                        XNWplot_res, PNWplot_res) 


post<-rbind(posta, postb, postc, postd)

postx<-rbind(posta, postb, postc, postd, poste, postf,postg, posth)
#plot just N treatments 
postN<-rbind(posta, postb,  poste, postf,postg)

ggplot(post, aes(x=log(intra_inter_res), fill=treat))+ geom_density(alpha=0.3)+
  geom_vline(aes(xintercept=0), lty=2, color="red")+ xlim (-3, 2)+
  xlab("Log Intra/Inter response competition")+ theme_classic() + facet_wrap(~spp)
  
ggplot(post, aes(x=log(intra_inter_eff), fill=treat))+ geom_density( alpha=0.3)+
    geom_vline(aes(xintercept=0), lty=2, color="red")+ xlim (-3, 2)+
    xlab("Log Intra/Inter effect competition")+ theme_classic()+ facet_wrap(~spp)
  
ggplot(postx, aes(x=log(intra_inter_eff), fill=treat))+ geom_density( alpha=0.3)+
  geom_vline(aes(xintercept=0), lty=2, color="red")+ xlim (-3, 2)+
  xlab("Log Intra/Inter effect competition")+ theme_classic()+ facet_wrap(~spp)

ggplot(postx, aes(x=log(intra_inter_res), fill=treat))+ geom_density( alpha=0.3)+
  geom_vline(aes(xintercept=0), lty=2, color="red")+ xlim (-3, 2)+
  xlab("Log Intra/Inter response competition")+ theme_classic()+ facet_wrap(~spp)

ggplot(postx, aes(x=log(intra_inter_eff), fill=spp))+ geom_density( alpha=0.3)+
  geom_vline(aes(xintercept=0), lty=2, color="red")+ xlim (-3, 2)+
  xlab("Log Intra/Inter effect competition")+ theme_classic()+ facet_wrap(~treat)

ggplot(postx, aes(x=log(intra_inter_res), fill=spp))+ geom_density( alpha=0.3)+
  geom_vline(aes(xintercept=0), lty=2, color="red")+ xlim (-3, 2)+
  xlab("Log Intra/Inter response competition")+ theme_classic()+ facet_wrap(~treat)

ggplot(postx, aes(x=log(intra_inter_res), fill=spp))+ geom_boxplot( )+
  geom_vline(aes(xintercept=0), lty=2, color="red")+ xlim (-3, 2)+
  xlab("Log Intra/Inter response competition")+ theme_classic()+ facet_wrap(~treat) 
  
ggplot(postx, aes(x=spp, y=log(intra_inter_res), fill=treat))+ geom_boxplot( )+
  geom_hline(aes(yintercept=0), lty=2, color="red")+ ylim (-3, 2)+
  #scale_fill_manual(values =specColor)+fct_reorder(
  xlab("Log Intra/Inter response competition")+ theme_classic()#+ facet_wrap(~spp) 

ggplot(postx, aes(x=log(intra_inter_res), fill=treat))+ geom_boxplot( )+
  geom_vline(aes(xintercept=0), lty=2, color="red")+ xlim (-3, 2)+
  #scale_fill_manual(values =specColor)+fct_reorder(
  xlab("Log Intra/Inter response competition")+ theme_classic()+ facet_wrap(~spp) 

#BY TIME----
#early----
load(file = "outputs/modDAtime_earlytrtoutput.RData")
alphas<-as.data.frame(modDAtime_earlytrt$chains$alphaGibbs)
names<-modDAtime_earlytrt$parameters$alphaTable$`alpha_{to, from}`
colnames(alphas)<-names

#artsco
artsco<-select(alphas, contains("ARTSCO"))
artsco<-rowwise(artsco)%>%
  mutate(inter_eff=(sum(c_across(`BISBIS, ARTSCO`:`RARESPP, ARTSCO`)))/8)%>%
  mutate(inter_res=(sum(c_across(`ARTSCO, BISBIS`:`ARTSCO, RARESPP`)))/8)%>%
  mutate(intra=`ARTSCO, ARTSCO`)%>%mutate(intra_inter_eff=intra/inter_eff, 
                                          intra_inter_res=intra/inter_res)
artsco_cie<-ci(log(artsco$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
artsco_cie$mu<-mean(log(artsco$intra_inter_eff)) 
artsco_cie$spp<-"ARTSCO"

artsco_cir<-ci(log(artsco$intra_inter_res), ci=c(0.95, 0.9, 0.85))
artsco_cir$mu<-mean(log(artsco$intra_inter_res)) 
artsco_cir$spp<-"ARTSCO"

#descae 
descae<-select(alphas, contains("DESCAE"))
descae<-rowwise(descae)%>%
  mutate(inter1=sum(c_across(`ARTSCO, DESCAE`:`CARSCO, DESCAE`)))%>%
  mutate(inter2=sum(c_across(`GENALG, DESCAE`:`RARESPP, DESCAE`)))%>%
  mutate(inter3=sum(c_across(`DESCAE, ARTSCO`:`DESCAE, CARSCO`)))%>%
  mutate(inter4=sum(c_across(`DESCAE, GENALG`:`DESCAE, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`DESCAE, DESCAE`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)

descae_cie<-ci(log(descae$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
descae_cie$mu<-mean(log(descae$intra_inter_eff)) 
descae_cie$spp<-"DESCAE"

descae_cir<-ci(log(descae$intra_inter_res), ci=c(0.95, 0.9, 0.85))
descae_cir$mu<-mean(log(descae$intra_inter_res)) 
descae_cir$spp<-"DESCAE"

#geum  
geuros<-select(alphas, contains("GEUROS"))
geuros<-rowwise(geuros)%>%
  mutate(inter1=sum(c_across(`ARTSCO, GEUROS`:`GENALG, GEUROS`)))%>%
  mutate(inter2=sum(c_across(`TRIPAR, GEUROS`:`RARESPP, GEUROS`)))%>%
  mutate(inter3=sum(c_across(`GEUROS, ARTSCO`:`GEUROS, GENALG`)))%>%
  mutate(inter4=sum(c_across(`GEUROS, TRIPAR`:`GEUROS, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`GEUROS, GEUROS`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)

geuros_cie<-ci(log(geuros$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
geuros_cie$mu<-mean(log(geuros$intra_inter_eff))
geuros_cie$spp<-"GEUROS"

geuros_cir<-ci(log(geuros$intra_inter_res),ci=c(0.95, 0.9, 0.85))
geuros_cir$mu<-mean(log(geuros$intra_inter_res))
geuros_cir$spp<-"GEUROS"

#Combine spp 
alphas1e<-rbind(geuros_cie, artsco_cie, descae_cie)
alphas1e$time<-"Early"
alphas1r<-rbind(geuros_cir, artsco_cir, descae_cir)
alphas1r$time<-"Early"

artsco$spp<-"ARTSCO"
descae$spp<-"DESCAE"
geuros$spp<-"GEUROS"

post1<-select(artsco, intra_inter_eff, intra_inter_res, spp)
post2<-select(descae, intra_inter_eff, intra_inter_res, spp)
post3<-select(geuros, intra_inter_eff, intra_inter_res, spp)

posta<-rbind(post1, post2, post3)
posta$time<-"Early"

#effect
Earlyplot_eff<-ggplot(posta, aes(x=log(intra_inter_eff),..scaled.., fill=spp))+ geom_density(alpha=0.5)+
  geom_vline(aes(xintercept=0), lty=2, color="red")+xlim(-4,2)+
  xlab("Log Intra/Inter effect competition")+ theme_classic()+ 
  ggtitle("Early")

#response 
Earlyplot_res<-ggplot(posta, aes(x=log(intra_inter_res),..scaled.., fill=spp))+ 
  geom_density(alpha=0.5)+
  geom_vline(aes(xintercept=0), lty=2, color="red")+xlim(-4,2)+
  xlab("Log Intra/Inter response competition")+ theme_classic()+ 
  ggtitle("Early")

#mid----
load(file = "outputs/modDAtime_midtrtoutput.RData")
alphas<-as.data.frame(modDAtime_midtrt$chains$alphaGibbs)
names<-modDAtime_midtrt$parameters$alphaTable$`alpha_{to, from}`
colnames(alphas)<-names

#artsco
artsco<-select(alphas, contains("ARTSCO"))
artsco<-rowwise(artsco)%>%
  mutate(inter_eff=(sum(c_across(`BISBIS, ARTSCO`:`RARESPP, ARTSCO`)))/8)%>%
  mutate(inter_res=(sum(c_across(`ARTSCO, BISBIS`:`ARTSCO, RARESPP`)))/8)%>%
  mutate(intra=`ARTSCO, ARTSCO`)%>%mutate(intra_inter_eff=intra/inter_eff, 
                                          intra_inter_res=intra/inter_res)
artsco_cie<-ci(log(artsco$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
artsco_cie$mu<-mean(log(artsco$intra_inter_eff)) 
artsco_cie$spp<-"ARTSCO"

artsco_cir<-ci(log(artsco$intra_inter_res), ci=c(0.95, 0.9, 0.85))
artsco_cir$mu<-mean(log(artsco$intra_inter_res)) 
artsco_cir$spp<-"ARTSCO"

#descae 
descae<-select(alphas, contains("DESCAE"))
descae<-rowwise(descae)%>%
  mutate(inter1=sum(c_across(`ARTSCO, DESCAE`:`CARSCO, DESCAE`)))%>%
  mutate(inter2=sum(c_across(`GENALG, DESCAE`:`RARESPP, DESCAE`)))%>%
  mutate(inter3=sum(c_across(`DESCAE, ARTSCO`:`DESCAE, CARSCO`)))%>%
  mutate(inter4=sum(c_across(`DESCAE, GENALG`:`DESCAE, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`DESCAE, DESCAE`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)

descae_cie<-ci(log(descae$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
descae_cie$mu<-mean(log(descae$intra_inter_eff)) 
descae_cie$spp<-"DESCAE"

descae_cir<-ci(log(descae$intra_inter_res), ci=c(0.95, 0.9, 0.85))
descae_cir$mu<-mean(log(descae$intra_inter_res)) 
descae_cir$spp<-"DESCAE"

#geum  
geuros<-select(alphas, contains("GEUROS"))
geuros<-rowwise(geuros)%>%
  mutate(inter1=sum(c_across(`ARTSCO, GEUROS`:`GENALG, GEUROS`)))%>%
  mutate(inter2=sum(c_across(`TRIPAR, GEUROS`:`RARESPP, GEUROS`)))%>%
  mutate(inter3=sum(c_across(`GEUROS, ARTSCO`:`GEUROS, GENALG`)))%>%
  mutate(inter4=sum(c_across(`GEUROS, TRIPAR`:`GEUROS, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`GEUROS, GEUROS`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)

geuros_cie<-ci(log(geuros$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
geuros_cie$mu<-mean(log(geuros$intra_inter_eff))
geuros_cie$spp<-"GEUROS"

geuros_cir<-ci(log(geuros$intra_inter_res),ci=c(0.95, 0.9, 0.85))
geuros_cir$mu<-mean(log(geuros$intra_inter_res))
geuros_cir$spp<-"GEUROS"

#Combine spp 
alphas2e<-rbind(geuros_cie, artsco_cie, descae_cie)
alphas2e$time<-"Mid"
alphas2r<-rbind(geuros_cir, artsco_cir, descae_cir)
alphas1r$time<-"Mid"

artsco$spp<-"ARTSCO"
descae$spp<-"DESCAE"
geuros$spp<-"GEUROS"

post1<-select(artsco, intra_inter_eff, intra_inter_res, spp)
post2<-select(descae, intra_inter_eff, intra_inter_res, spp)
post3<-select(geuros, intra_inter_eff, intra_inter_res, spp)

postb<-rbind(post1, post2, post3)
postb$time<-"Mid"

#effect
Midplot_eff<-ggplot(postb, aes(x=log(intra_inter_eff),..scaled.., fill=spp))+ geom_density(alpha=0.5)+
  geom_vline(aes(xintercept=0), lty=2, color="red")+xlim(-4,2)+
  xlab("Log Intra/Inter effect competition")+ theme_classic()+ 
  ggtitle("Mid")

#response 
Midplot_res<-ggplot(postb, aes(x=log(intra_inter_res),..scaled.., fill=spp))+ 
  geom_density(alpha=0.5)+
  geom_vline(aes(xintercept=0), lty=2, color="red")+xlim(-4,2)+
  xlab("Log Intra/Inter response competition")+ theme_classic()+ 
  ggtitle("Mid")





#late----
load(file = "outputs/modDAtime_latetrtoutput.RData")
alphas<-as.data.frame(modDAtime_latetrt$chains$alphaGibbs)
names<-modDAtime_latetrt$parameters$alphaTable$`alpha_{to, from}`
colnames(alphas)<-names

#artsco
artsco<-select(alphas, contains("ARTSCO"))
artsco<-rowwise(artsco)%>%
  mutate(inter_eff=(sum(c_across(`BISBIS, ARTSCO`:`RARESPP, ARTSCO`)))/8)%>%
  mutate(inter_res=(sum(c_across(`ARTSCO, BISBIS`:`ARTSCO, RARESPP`)))/8)%>%
  mutate(intra=`ARTSCO, ARTSCO`)%>%mutate(intra_inter_eff=intra/inter_eff, 
                                          intra_inter_res=intra/inter_res)
artsco_cie<-ci(log(artsco$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
artsco_cie$mu<-mean(log(artsco$intra_inter_eff)) 
artsco_cie$spp<-"ARTSCO"

artsco_cir<-ci(log(artsco$intra_inter_res), ci=c(0.95, 0.9, 0.85))
artsco_cir$mu<-mean(log(artsco$intra_inter_res)) 
artsco_cir$spp<-"ARTSCO"

#descae 
descae<-select(alphas, contains("DESCAE"))
descae<-rowwise(descae)%>%
  mutate(inter1=sum(c_across(`ARTSCO, DESCAE`:`CARSCO, DESCAE`)))%>%
  mutate(inter2=sum(c_across(`GENALG, DESCAE`:`RARESPP, DESCAE`)))%>%
  mutate(inter3=sum(c_across(`DESCAE, ARTSCO`:`DESCAE, CARSCO`)))%>%
  mutate(inter4=sum(c_across(`DESCAE, GENALG`:`DESCAE, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`DESCAE, DESCAE`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)

descae_cie<-ci(log(descae$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
descae_cie$mu<-mean(log(descae$intra_inter_eff)) 
descae_cie$spp<-"DESCAE"

descae_cir<-ci(log(descae$intra_inter_res), ci=c(0.95, 0.9, 0.85))
descae_cir$mu<-mean(log(descae$intra_inter_res)) 
descae_cir$spp<-"DESCAE"

#geum  
geuros<-select(alphas, contains("GEUROS"))
geuros<-rowwise(geuros)%>%
  mutate(inter1=sum(c_across(`ARTSCO, GEUROS`:`GENALG, GEUROS`)))%>%
  mutate(inter2=sum(c_across(`TRIPAR, GEUROS`:`RARESPP, GEUROS`)))%>%
  mutate(inter3=sum(c_across(`GEUROS, ARTSCO`:`GEUROS, GENALG`)))%>%
  mutate(inter4=sum(c_across(`GEUROS, TRIPAR`:`GEUROS, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`GEUROS, GEUROS`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)

geuros_cie<-ci(log(geuros$intra_inter_eff), ci=c(0.95, 0.9, 0.85))
geuros_cie$mu<-mean(log(geuros$intra_inter_eff))
geuros_cie$spp<-"GEUROS"

geuros_cir<-ci(log(geuros$intra_inter_res),ci=c(0.95, 0.9, 0.85))
geuros_cir$mu<-mean(log(geuros$intra_inter_res))
geuros_cir$spp<-"GEUROS"

#Combine spp 
alphas3e<-rbind(geuros_cie, artsco_cie, descae_cie)
alphas3e$time<-"Late"
alphas3r<-rbind(geuros_cir, artsco_cir, descae_cir)
alphas1r$time<-"Late"

artsco$spp<-"ARTSCO"
descae$spp<-"DESCAE"
geuros$spp<-"GEUROS"

post1<-select(artsco, intra_inter_eff, intra_inter_res, spp)
post2<-select(descae, intra_inter_eff, intra_inter_res, spp)
post3<-select(geuros, intra_inter_eff, intra_inter_res, spp)

postc<-rbind(post1, post2, post3)
postc$time<-"Late"

#effect
Lateplot_eff<-ggplot(postc, aes(x=log(intra_inter_eff),..scaled.., fill=spp))+ geom_density(alpha=0.5)+
  geom_vline(aes(xintercept=0), lty=2, color="red")+xlim(-4,2)+
  xlab("Log Intra/Inter effect competition")+ theme_classic()+ 
  ggtitle("Late")

#response 
Lateplot_res<-ggplot(postc, aes(x=log(intra_inter_res),..scaled.., fill=spp))+ 
  geom_density(alpha=0.5)+
  geom_vline(aes(xintercept=0), lty=2, color="red")+xlim(-4,2)+
  xlab("Log Intra/Inter response competition")+ theme_classic()+ 
  ggtitle("Late")


#combine times----
gridExtra::grid.arrange(Earlyplot_eff, Midplot_eff, Lateplot_eff)
gridExtra::grid.arrange(Earlyplot_res, Midplot_res, Lateplot_res)

alphas_eff<-rbind(alphas1e, alphas2e, alphas3e)
alphas_res<-rbind(alphas1r, alphas2r, alphas3r)

post<-rbind(posta, postb, postc)

ggplot(post, aes(x=log(intra_inter_res),..scaled.., fill=time))+ geom_density(alpha=0.3)+
  geom_vline(aes(xintercept=0), lty=2, color="red")+#+ xlim (-3, 2)+
  xlab("Log Intra/Inter response competition")+ theme_classic() + facet_wrap(~spp)

ggplot(post, aes(x=log(intra_inter_res), fill=fct_reorder(time,.x = log(intra_inter_res), .fun = median)))+
 geom_boxplot()+ 
  geom_vline(aes(xintercept=0), lty=2, color="red")+ xlim (-3, 2)+
  xlab("Log Intra/Inter response competition")+ theme_classic() + 
  scale_fill_discrete(name ="Time")+
  facet_wrap(~spp)

#calculate rhos---- 
#Adler 2018 Ecol Letters

#XXX----
  load(file = "outputs/modDAtime_XXXoutput.RData")
  alphas<-as.data.frame(modDAtimeXXX$chains$alphaGibbs)
  names<-modDAtimeXXX$parameters$alphaTable$`alpha_{to, from}`
  colnames(alphas)<-names

  #subset to 3 dominant, 3 subdominant spp (>10% rel abund control plots)
alphas<-select(alphas, matches("ARTSCO|DESCAE|GEUROS|CALLEP|CARSCO|GENALG"))
alphas<-select(alphas, !matches("BISBIS|RARESPP|TRIPAR"))

#spp
#1-ARTSCO
#2-CALLEP
#3-CARSCO
#4-DESCAE
#5-GENALG
#6-GEUROS

rhos<-mutate(alphas, 
  rho_1_2=sqrt((`ARTSCO, CALLEP`*`CALLEP, ARTSCO`)/(`ARTSCO, ARTSCO`*`CALLEP, CALLEP`)), 
  rho_1_3=sqrt((`ARTSCO, CARSCO`*`CARSCO, ARTSCO`)/(`ARTSCO, ARTSCO`*`CARSCO, CARSCO`)),
  rho_1_4=sqrt((`ARTSCO, DESCAE`*`DESCAE, ARTSCO`)/(`ARTSCO, ARTSCO`*`DESCAE, DESCAE`)),
  rho_1_5=sqrt((`ARTSCO, GENALG`*`GENALG, ARTSCO`)/(`ARTSCO, ARTSCO`*`GENALG, GENALG`)),
  rho_1_6=sqrt((`ARTSCO, GEUROS`*`GEUROS, ARTSCO`)/(`ARTSCO, ARTSCO`*`GEUROS, GEUROS`)), 
  rho_2_3=sqrt((`CALLEP, CARSCO`*`CARSCO, CALLEP`)/(`CALLEP, CALLEP`*`CARSCO, CARSCO`)),
  rho_2_4=sqrt((`CALLEP, DESCAE`*`DESCAE, CALLEP`)/(`CALLEP, CALLEP`*`DESCAE, DESCAE`)),
  rho_2_5=sqrt((`CALLEP, GENALG`*`GENALG, CALLEP`)/(`CALLEP, CALLEP`*`GENALG, GENALG`)),
  rho_2_6=sqrt((`CALLEP, GEUROS`*`GEUROS, CALLEP`)/(`CALLEP, CALLEP`*`GEUROS, GEUROS`)), 
  rho_3_4=sqrt((`CARSCO, DESCAE`*`DESCAE, CARSCO`)/(`CARSCO, CARSCO`*`DESCAE, DESCAE`)),
  rho_3_5=sqrt((`CARSCO, GENALG`*`GENALG, CARSCO`)/(`CARSCO, CARSCO`*`GENALG, GENALG`)),
  rho_3_6=sqrt((`CARSCO, GEUROS`*`GEUROS, CARSCO`)/(`CARSCO, CARSCO`*`GEUROS, GEUROS`)), 
  rho_4_5=sqrt((`DESCAE, GENALG`*`GENALG, DESCAE`)/(`DESCAE, DESCAE`*`GENALG, GENALG`)),
  rho_4_6=sqrt((`DESCAE, GEUROS`*`GEUROS, DESCAE`)/(`DESCAE, DESCAE`*`GEUROS, GEUROS`)), 
  rho_5_6=sqrt((`GENALG, GEUROS`*`GEUROS, GENALG`)/(`GENALG, GENALG`*`GEUROS, GEUROS`))) 
  
rhos<-select(rhos, contains("rho"))          
#add remainder for full pairs 
rhos$rho_2_1<-rhos$rho_1_2
rhos$rho_3_1<-rhos$rho_1_3
rhos$rho_3_2<-rhos$rho_2_3
rhos$rho_4_1<-rhos$rho_1_4
rhos$rho_4_2<-rhos$rho_2_4
rhos$rho_4_3<-rhos$rho_3_4
rhos$rho_5_1<-rhos$rho_1_5
rhos$rho_5_2<-rhos$rho_2_5
rhos$rho_5_3<-rhos$rho_3_5
rhos$rho_5_4<-rhos$rho_4_5
rhos$rho_6_1<-rhos$rho_1_6
rhos$rho_6_2<-rhos$rho_2_6
rhos$rho_6_3<-rhos$rho_3_6
rhos$rho_6_4<-rhos$rho_4_6
rhos$rho_6_5<-rhos$rho_5_6

ctl_ci12<-ci(log(rhos$rho_1_2))
ctl_ci12$spp<-"ARTSCO/DESCAE"
ctl_ci13<-ci(log(rhos$rho_1_3))
ctl_ci13$spp<-"ARTSCO/GEUROS" 
ctl_ci23<-ci(log(rhos$rho_2_3))
ctl_ci23$spp<-"DESCAE/GEUROS"

ctl_ci<-rbind(ctl_ci12, ctl_ci13, ctl_ci23)

rhos<-pivot_longer(rhos, cols=everything(), names_to = "spp", values_to = "rho")%>%
  separate(spp, c("param", "spp1", "spp2"), remove = F)%>%
  mutate(spp1=case_when(spp1=="1"~"ARTSCO",
                         spp1=="2"~"CALLEP",
                         spp1=="3"~"CARSCO", 
                         spp1=="4"~"DESCAE",
                         spp1=="5"~"GENALG",
                         spp1=="6"~"GEUROS"))%>% 
  mutate(spp2=case_when(spp2=="1"~"ARTSCO",
         spp2=="2"~"CALLEP",
         spp2=="3"~"CARSCO", 
         spp2=="4"~"DESCAE",
         spp2=="5"~"GENALG",
         spp2=="6"~"GEUROS"))%>%mutate(dom1=case_when(spp1=="ARTSCO"~"DOM",
                                                       spp1=="GEUROS"~"DOM",
                                                       spp1=="DESCAE"~"DOM",
                                                       spp1=="CALLEP"~"SUBDOM",
                                                       spp1=="CARSCO"~"SUBDOM",  
                                                       spp1=="GENALG"~"SUBDOM"))%>%
  mutate(dom2=case_when(spp2=="ARTSCO"~"DOM",
                        spp2=="GEUROS"~"DOM",
                        spp2=="DESCAE"~"DOM",
                        spp2=="CALLEP"~"SUBDOM",
                        spp2=="CARSCO"~"SUBDOM",  
                        spp2=="GENALG"~"SUBDOM"))

#ggplot(rhos, aes(x=log(rho),fill=spp2))+ geom_density(alpha=0.3)+
#  geom_vline(aes(xintercept=0), lty=2, color="red")+ #xlim (-3, 2)+
#  xlab("Log Rho")+ theme_classic()+ facet_wrap(~spp1)+ ggtitle("Control")

#ggplot(rhos, aes(x=log(rho),fill=dom2))+ geom_density(alpha=0.3)+
 # geom_vline(aes(xintercept=0), lty=2, color="red")+ #xlim (-3, 2)+
  #xlab("Log Rho")+ theme_classic()+ facet_wrap(~spp1)+ ggtitle("Control")

#plot<-ggplot(rhos, aes(x=log(rho),fill=dom2))+ geom_density(alpha=0.3)+
# geom_vline(aes(xintercept=0), lty=2, color="red")+ #xlim (-3, 2)+
#  xlab("Log Rho")+ theme_classic()+ facet_wrap(~spp1)+ ggtitle("Control")

CTLplot<-ggplot(subset(rhos, dom1=='DOM'& dom2=="DOM"), aes(x=log(rho),fill=spp2))+ geom_density(alpha=0.3)+
  geom_vline(aes(xintercept=0), lty=2, color="red")+ #xlim (-3, 2)+
  xlab("Log Rho")+ theme_classic()+ facet_wrap(~spp1)+ ggtitle("Control")
 

#XNX----
load(file = "outputs/modDAtime_XNXoutput.RData")
alphas<-as.data.frame(modDAtimeXNX$chains$alphaGibbs)
names<-modDAtimeXNX$parameters$alphaTable$`alpha_{to, from}`
colnames(alphas)<-names

#subset to 3 dominant, 3 subdominant spp (>10% rel abund control plots)
alphas<-select(alphas, matches("ARTSCO|DESCAE|GEUROS|CALLEP|CARSCO|GENALG"))
alphas<-select(alphas, !matches("BISBIS|RARESPP|TRIPAR"))

#spp
#1-ARTSCO
#2-CALLEP
#3-CARSCO
#4-DESCAE
#5-GENALG
#6-GEUROS

rhos<-mutate(alphas, 
             rho_1_2=sqrt((`ARTSCO, CALLEP`*`CALLEP, ARTSCO`)/(`ARTSCO, ARTSCO`*`CALLEP, CALLEP`)), 
             rho_1_3=sqrt((`ARTSCO, CARSCO`*`CARSCO, ARTSCO`)/(`ARTSCO, ARTSCO`*`CARSCO, CARSCO`)),
             rho_1_4=sqrt((`ARTSCO, DESCAE`*`DESCAE, ARTSCO`)/(`ARTSCO, ARTSCO`*`DESCAE, DESCAE`)),
             rho_1_5=sqrt((`ARTSCO, GENALG`*`GENALG, ARTSCO`)/(`ARTSCO, ARTSCO`*`GENALG, GENALG`)),
             rho_1_6=sqrt((`ARTSCO, GEUROS`*`GEUROS, ARTSCO`)/(`ARTSCO, ARTSCO`*`GEUROS, GEUROS`)), 
             rho_2_3=sqrt((`CALLEP, CARSCO`*`CARSCO, CALLEP`)/(`CALLEP, CALLEP`*`CARSCO, CARSCO`)),
             rho_2_4=sqrt((`CALLEP, DESCAE`*`DESCAE, CALLEP`)/(`CALLEP, CALLEP`*`DESCAE, DESCAE`)),
             rho_2_5=sqrt((`CALLEP, GENALG`*`GENALG, CALLEP`)/(`CALLEP, CALLEP`*`GENALG, GENALG`)),
             rho_2_6=sqrt((`CALLEP, GEUROS`*`GEUROS, CALLEP`)/(`CALLEP, CALLEP`*`GEUROS, GEUROS`)), 
             rho_3_4=sqrt((`CARSCO, DESCAE`*`DESCAE, CARSCO`)/(`CARSCO, CARSCO`*`DESCAE, DESCAE`)),
             rho_3_5=sqrt((`CARSCO, GENALG`*`GENALG, CARSCO`)/(`CARSCO, CARSCO`*`GENALG, GENALG`)),
             rho_3_6=sqrt((`CARSCO, GEUROS`*`GEUROS, CARSCO`)/(`CARSCO, CARSCO`*`GEUROS, GEUROS`)), 
             rho_4_5=sqrt((`DESCAE, GENALG`*`GENALG, DESCAE`)/(`DESCAE, DESCAE`*`GENALG, GENALG`)),
             rho_4_6=sqrt((`DESCAE, GEUROS`*`GEUROS, DESCAE`)/(`DESCAE, DESCAE`*`GEUROS, GEUROS`)), 
             rho_5_6=sqrt((`GENALG, GEUROS`*`GEUROS, GENALG`)/(`GENALG, GENALG`*`GEUROS, GEUROS`))) 

rhos<-select(rhos, contains("rho"))          
#add remainder for full pairs 
rhos$rho_2_1<-rhos$rho_1_2
rhos$rho_3_1<-rhos$rho_1_3
rhos$rho_3_2<-rhos$rho_2_3
rhos$rho_4_1<-rhos$rho_1_4
rhos$rho_4_2<-rhos$rho_2_4
rhos$rho_4_3<-rhos$rho_3_4
rhos$rho_5_1<-rhos$rho_1_5
rhos$rho_5_2<-rhos$rho_2_5
rhos$rho_5_3<-rhos$rho_3_5
rhos$rho_5_4<-rhos$rho_4_5
rhos$rho_6_1<-rhos$rho_1_6
rhos$rho_6_2<-rhos$rho_2_6
rhos$rho_6_3<-rhos$rho_3_6
rhos$rho_6_4<-rhos$rho_4_6
rhos$rho_6_5<-rhos$rho_5_6

rhos<-pivot_longer(rhos, cols=everything(), names_to = "spp", values_to = "rho")%>%
  separate(spp, c("param", "spp1", "spp2"), remove = F)%>%
  mutate(spp1=case_when(spp1=="1"~"ARTSCO",
                        spp1=="2"~"CALLEP",
                        spp1=="3"~"CARSCO", 
                        spp1=="4"~"DESCAE",
                        spp1=="5"~"GENALG",
                        spp1=="6"~"GEUROS"))%>% 
  mutate(spp2=case_when(spp2=="1"~"ARTSCO",
                        spp2=="2"~"CALLEP",
                        spp2=="3"~"CARSCO", 
                        spp2=="4"~"DESCAE",
                        spp2=="5"~"GENALG",
                        spp2=="6"~"GEUROS"))%>%
mutate(dom1=case_when(spp1=="ARTSCO"~"DOM",
                      spp1=="GEUROS"~"DOM",
                      spp1=="DESCAE"~"DOM",
                      spp1=="CALLEP"~"SUBDOM",
                      spp1=="CARSCO"~"SUBDOM",  
                      spp1=="GENALG"~"SUBDOM"))%>%
  mutate(dom2=case_when(spp2=="ARTSCO"~"DOM",
                        spp2=="GEUROS"~"DOM",
                        spp2=="DESCAE"~"DOM",
                        spp2=="CALLEP"~"SUBDOM",
                        spp2=="CARSCO"~"SUBDOM",  
                        spp2=="GENALG"~"SUBDOM"))


#ggplot(rhos, aes(x=log(rho),fill=spp2))+ geom_density(alpha=0.3)+
  #geom_vline(aes(xintercept=0), lty=2, color="red")+ #xlim (-3, 2)+
 # xlab("Log Rho")+ theme_classic()+ facet_wrap(~spp1)+ggtitle("N addition")

#ggplot(rhos, aes(x=log(rho),fill=dom2))+ geom_density(alpha=0.3)+
  #geom_vline(aes(xintercept=0), lty=2, color="red")+ #xlim (-3, 2)+
 # xlab("Log Rho")+ theme_classic()+ facet_wrap(~spp1)+ ggtitle("N addition")

#ggplot(subset(rhos, dom1=='DOM'), aes(x=log(rho),fill=dom2))+ geom_density(alpha=0.3)+
#  geom_vline(aes(xintercept=0), lty=2, color="red")+ #xlim (-3, 2)+
#  xlab("Log Rho")+ theme_classic()+ facet_wrap(~spp1)+ ggtitle("N addition")

Nplot<-ggplot(subset(rhos, dom1=='DOM'&dom2=="DOM"), aes(x=log(rho),fill=spp2))+ geom_density(alpha=0.3)+
  geom_vline(aes(xintercept=0), lty=2, color="red")+ #xlim (-3, 2)+
  xlab("Log Rho")+ theme_classic()+ facet_wrap(~spp1)+ ggtitle("N addition")

#XXW----
load(file = "outputs/modDAtime_XXWoutput.RData")
alphas<-as.data.frame(modDAtimeXXW$chains$alphaGibbs)
names<-modDAtimeXXW$parameters$alphaTable$`alpha_{to, from}`
colnames(alphas)<-names

#subset to 3 dominant, 3 subdominant spp (>10% rel abund control plots)
alphas<-select(alphas, matches("ARTSCO|DESCAE|GEUROS|CALLEP|CARSCO|GENALG"))
alphas<-select(alphas, !matches("BISBIS|RARESPP|TRIPAR"))

#spp
#1-ARTSCO
#2-CALLEP
#3-CARSCO
#4-DESCAE
#5-GENALG
#6-GEUROS

rhos<-mutate(alphas, 
             rho_1_2=sqrt((`ARTSCO, CALLEP`*`CALLEP, ARTSCO`)/(`ARTSCO, ARTSCO`*`CALLEP, CALLEP`)), 
             rho_1_3=sqrt((`ARTSCO, CARSCO`*`CARSCO, ARTSCO`)/(`ARTSCO, ARTSCO`*`CARSCO, CARSCO`)),
             rho_1_4=sqrt((`ARTSCO, DESCAE`*`DESCAE, ARTSCO`)/(`ARTSCO, ARTSCO`*`DESCAE, DESCAE`)),
             rho_1_5=sqrt((`ARTSCO, GENALG`*`GENALG, ARTSCO`)/(`ARTSCO, ARTSCO`*`GENALG, GENALG`)),
             rho_1_6=sqrt((`ARTSCO, GEUROS`*`GEUROS, ARTSCO`)/(`ARTSCO, ARTSCO`*`GEUROS, GEUROS`)), 
             rho_2_3=sqrt((`CALLEP, CARSCO`*`CARSCO, CALLEP`)/(`CALLEP, CALLEP`*`CARSCO, CARSCO`)),
             rho_2_4=sqrt((`CALLEP, DESCAE`*`DESCAE, CALLEP`)/(`CALLEP, CALLEP`*`DESCAE, DESCAE`)),
             rho_2_5=sqrt((`CALLEP, GENALG`*`GENALG, CALLEP`)/(`CALLEP, CALLEP`*`GENALG, GENALG`)),
             rho_2_6=sqrt((`CALLEP, GEUROS`*`GEUROS, CALLEP`)/(`CALLEP, CALLEP`*`GEUROS, GEUROS`)), 
             rho_3_4=sqrt((`CARSCO, DESCAE`*`DESCAE, CARSCO`)/(`CARSCO, CARSCO`*`DESCAE, DESCAE`)),
             rho_3_5=sqrt((`CARSCO, GENALG`*`GENALG, CARSCO`)/(`CARSCO, CARSCO`*`GENALG, GENALG`)),
             rho_3_6=sqrt((`CARSCO, GEUROS`*`GEUROS, CARSCO`)/(`CARSCO, CARSCO`*`GEUROS, GEUROS`)), 
             rho_4_5=sqrt((`DESCAE, GENALG`*`GENALG, DESCAE`)/(`DESCAE, DESCAE`*`GENALG, GENALG`)),
             rho_4_6=sqrt((`DESCAE, GEUROS`*`GEUROS, DESCAE`)/(`DESCAE, DESCAE`*`GEUROS, GEUROS`)), 
             rho_5_6=sqrt((`GENALG, GEUROS`*`GEUROS, GENALG`)/(`GENALG, GENALG`*`GEUROS, GEUROS`))) 

rhos<-select(rhos, contains("rho"))          
#add remainder for full pairs 
rhos$rho_2_1<-rhos$rho_1_2
rhos$rho_3_1<-rhos$rho_1_3
rhos$rho_3_2<-rhos$rho_2_3
rhos$rho_4_1<-rhos$rho_1_4
rhos$rho_4_2<-rhos$rho_2_4
rhos$rho_4_3<-rhos$rho_3_4
rhos$rho_5_1<-rhos$rho_1_5
rhos$rho_5_2<-rhos$rho_2_5
rhos$rho_5_3<-rhos$rho_3_5
rhos$rho_5_4<-rhos$rho_4_5
rhos$rho_6_1<-rhos$rho_1_6
rhos$rho_6_2<-rhos$rho_2_6
rhos$rho_6_3<-rhos$rho_3_6
rhos$rho_6_4<-rhos$rho_4_6
rhos$rho_6_5<-rhos$rho_5_6

rhos<-pivot_longer(rhos, cols=everything(), names_to = "spp", values_to = "rho")%>%
  separate(spp, c("param", "spp1", "spp2"), remove = F)%>%
  mutate(spp1=case_when(spp1=="1"~"ARTSCO",
                        spp1=="2"~"CALLEP",
                        spp1=="3"~"CARSCO", 
                        spp1=="4"~"DESCAE",
                        spp1=="5"~"GENALG",
                        spp1=="6"~"GEUROS"))%>% 
  mutate(spp2=case_when(spp2=="1"~"ARTSCO",
                        spp2=="2"~"CALLEP",
                        spp2=="3"~"CARSCO", 
                        spp2=="4"~"DESCAE",
                        spp2=="5"~"GENALG",
                        spp2=="6"~"GEUROS"))%>%
  mutate(dom1=case_when(spp1=="ARTSCO"~"DOM",
                        spp1=="GEUROS"~"DOM",
                        spp1=="DESCAE"~"DOM",
                        spp1=="CALLEP"~"SUBDOM",
                        spp1=="CARSCO"~"SUBDOM",  
                        spp1=="GENALG"~"SUBDOM"))%>%
  mutate(dom2=case_when(spp2=="ARTSCO"~"DOM",
                        spp2=="GEUROS"~"DOM",
                        spp2=="DESCAE"~"DOM",
                        spp2=="CALLEP"~"SUBDOM",
                        spp2=="CARSCO"~"SUBDOM",  
                        spp2=="GENALG"~"SUBDOM"))

#ggplot(rhos, aes(x=log(rho),fill=spp2))+ geom_density(alpha=0.3)+
 # geom_vline(aes(xintercept=0), lty=2, color="red")+ #xlim (-3, 2)+
  #xlab("Log Rho")+ theme_classic()+ facet_wrap(~spp1)+ggtitle("Warming")

Warmplot<-ggplot(subset(rhos, dom1=='DOM'&dom2=="DOM"), aes(x=log(rho),fill=spp2))+ geom_density(alpha=0.3)+
  geom_vline(aes(xintercept=0), lty=2, color="red")+ #xlim (-3, 2)+
  xlab("Log Rho")+ theme_classic()+ facet_wrap(~spp1)+ ggtitle("Warming")

#PXX----
load(file = "outputs/modDAtime_PXXoutput.RData")
alphas<-as.data.frame(modDAtimePXX$chains$alphaGibbs)
names<-modDAtimePXX$parameters$alphaTable$`alpha_{to, from}`
colnames(alphas)<-names

#subset to 3 dominant, 3 subdominant spp (>10% rel abund control plots)
alphas<-select(alphas, matches("ARTSCO|DESCAE|GEUROS|CALLEP|CARSCO|GENALG"))
alphas<-select(alphas, !matches("BISBIS|RARESPP|TRIPAR"))

#spp
#1-ARTSCO
#2-CALLEP
#3-CARSCO
#4-DESCAE
#5-GENALG
#6-GEUROS

rhos<-mutate(alphas, 
             rho_1_2=sqrt((`ARTSCO, CALLEP`*`CALLEP, ARTSCO`)/(`ARTSCO, ARTSCO`*`CALLEP, CALLEP`)), 
             rho_1_3=sqrt((`ARTSCO, CARSCO`*`CARSCO, ARTSCO`)/(`ARTSCO, ARTSCO`*`CARSCO, CARSCO`)),
             rho_1_4=sqrt((`ARTSCO, DESCAE`*`DESCAE, ARTSCO`)/(`ARTSCO, ARTSCO`*`DESCAE, DESCAE`)),
             rho_1_5=sqrt((`ARTSCO, GENALG`*`GENALG, ARTSCO`)/(`ARTSCO, ARTSCO`*`GENALG, GENALG`)),
             rho_1_6=sqrt((`ARTSCO, GEUROS`*`GEUROS, ARTSCO`)/(`ARTSCO, ARTSCO`*`GEUROS, GEUROS`)), 
             rho_2_3=sqrt((`CALLEP, CARSCO`*`CARSCO, CALLEP`)/(`CALLEP, CALLEP`*`CARSCO, CARSCO`)),
             rho_2_4=sqrt((`CALLEP, DESCAE`*`DESCAE, CALLEP`)/(`CALLEP, CALLEP`*`DESCAE, DESCAE`)),
             rho_2_5=sqrt((`CALLEP, GENALG`*`GENALG, CALLEP`)/(`CALLEP, CALLEP`*`GENALG, GENALG`)),
             rho_2_6=sqrt((`CALLEP, GEUROS`*`GEUROS, CALLEP`)/(`CALLEP, CALLEP`*`GEUROS, GEUROS`)), 
             rho_3_4=sqrt((`CARSCO, DESCAE`*`DESCAE, CARSCO`)/(`CARSCO, CARSCO`*`DESCAE, DESCAE`)),
             rho_3_5=sqrt((`CARSCO, GENALG`*`GENALG, CARSCO`)/(`CARSCO, CARSCO`*`GENALG, GENALG`)),
             rho_3_6=sqrt((`CARSCO, GEUROS`*`GEUROS, CARSCO`)/(`CARSCO, CARSCO`*`GEUROS, GEUROS`)), 
             rho_4_5=sqrt((`DESCAE, GENALG`*`GENALG, DESCAE`)/(`DESCAE, DESCAE`*`GENALG, GENALG`)),
             rho_4_6=sqrt((`DESCAE, GEUROS`*`GEUROS, DESCAE`)/(`DESCAE, DESCAE`*`GEUROS, GEUROS`)), 
             rho_5_6=sqrt((`GENALG, GEUROS`*`GEUROS, GENALG`)/(`GENALG, GENALG`*`GEUROS, GEUROS`))) 

rhos<-select(rhos, contains("rho"))          
#add remainder for full pairs 
rhos$rho_2_1<-rhos$rho_1_2
rhos$rho_3_1<-rhos$rho_1_3
rhos$rho_3_2<-rhos$rho_2_3
rhos$rho_4_1<-rhos$rho_1_4
rhos$rho_4_2<-rhos$rho_2_4
rhos$rho_4_3<-rhos$rho_3_4
rhos$rho_5_1<-rhos$rho_1_5
rhos$rho_5_2<-rhos$rho_2_5
rhos$rho_5_3<-rhos$rho_3_5
rhos$rho_5_4<-rhos$rho_4_5
rhos$rho_6_1<-rhos$rho_1_6
rhos$rho_6_2<-rhos$rho_2_6
rhos$rho_6_3<-rhos$rho_3_6
rhos$rho_6_4<-rhos$rho_4_6
rhos$rho_6_5<-rhos$rho_5_6

rhos<-pivot_longer(rhos, cols=everything(), names_to = "spp", values_to = "rho")%>%
  separate(spp, c("param", "spp1", "spp2"), remove = F)%>%
  mutate(spp1=case_when(spp1=="1"~"ARTSCO",
                        spp1=="2"~"CALLEP",
                        spp1=="3"~"CARSCO", 
                        spp1=="4"~"DESCAE",
                        spp1=="5"~"GENALG",
                        spp1=="6"~"GEUROS"))%>% 
  mutate(spp2=case_when(spp2=="1"~"ARTSCO",
                        spp2=="2"~"CALLEP",
                        spp2=="3"~"CARSCO", 
                        spp2=="4"~"DESCAE",
                        spp2=="5"~"GENALG",
                        spp2=="6"~"GEUROS"))%>%
  mutate(dom1=case_when(spp1=="ARTSCO"~"DOM",
                        spp1=="GEUROS"~"DOM",
                        spp1=="DESCAE"~"DOM",
                        spp1=="CALLEP"~"SUBDOM",
                        spp1=="CARSCO"~"SUBDOM",  
                        spp1=="GENALG"~"SUBDOM"))%>%
  mutate(dom2=case_when(spp2=="ARTSCO"~"DOM",
                        spp2=="GEUROS"~"DOM",
                        spp2=="DESCAE"~"DOM",
                        spp2=="CALLEP"~"SUBDOM",
                        spp2=="CARSCO"~"SUBDOM",  
                        spp2=="GENALG"~"SUBDOM"))


#ggplot(rhos, aes(x=log(rho),fill=spp2))+ geom_density(alpha=0.3)+
 # geom_vline(aes(xintercept=0), lty=2, color="red")+ #xlim (-3, 2)+
  #xlab("Log Rho")+ theme_classic()+ facet_wrap(~spp1)+ggtitle("Snow addition")

Snowplot<-ggplot(subset(rhos, dom1=='DOM'&dom2=="DOM"), aes(x=log(rho),fill=spp2))+ geom_density(alpha=0.3)+
  geom_vline(aes(xintercept=0), lty=2, color="red")+ #xlim (-3, 2)+
  xlab("Log Rho")+ theme_classic()+ facet_wrap(~spp1)+ ggtitle("Snow addition")

#PNX----
load(file = "outputs/modDAtime_PNXoutput.RData")
alphas<-as.data.frame(modDAtimePNX$chains$alphaGibbs)
names<-modDAtimePNX$parameters$alphaTable$`alpha_{to, from}`
colnames(alphas)<-names

#subset to 3 dominant, 3 subdominant spp (>10% rel abund control plots)
alphas<-select(alphas, matches("ARTSCO|DESCAE|GEUROS|CALLEP|CARSCO|GENALG"))
alphas<-select(alphas, !matches("BISBIS|RARESPP|TRIPAR"))

#spp
#1-ARTSCO
#2-CALLEP
#3-CARSCO
#4-DESCAE
#5-GENALG
#6-GEUROS

rhos<-mutate(alphas, 
             rho_1_2=sqrt((`ARTSCO, CALLEP`*`CALLEP, ARTSCO`)/(`ARTSCO, ARTSCO`*`CALLEP, CALLEP`)), 
             rho_1_3=sqrt((`ARTSCO, CARSCO`*`CARSCO, ARTSCO`)/(`ARTSCO, ARTSCO`*`CARSCO, CARSCO`)),
             rho_1_4=sqrt((`ARTSCO, DESCAE`*`DESCAE, ARTSCO`)/(`ARTSCO, ARTSCO`*`DESCAE, DESCAE`)),
             rho_1_5=sqrt((`ARTSCO, GENALG`*`GENALG, ARTSCO`)/(`ARTSCO, ARTSCO`*`GENALG, GENALG`)),
             rho_1_6=sqrt((`ARTSCO, GEUROS`*`GEUROS, ARTSCO`)/(`ARTSCO, ARTSCO`*`GEUROS, GEUROS`)), 
             rho_2_3=sqrt((`CALLEP, CARSCO`*`CARSCO, CALLEP`)/(`CALLEP, CALLEP`*`CARSCO, CARSCO`)),
             rho_2_4=sqrt((`CALLEP, DESCAE`*`DESCAE, CALLEP`)/(`CALLEP, CALLEP`*`DESCAE, DESCAE`)),
             rho_2_5=sqrt((`CALLEP, GENALG`*`GENALG, CALLEP`)/(`CALLEP, CALLEP`*`GENALG, GENALG`)),
             rho_2_6=sqrt((`CALLEP, GEUROS`*`GEUROS, CALLEP`)/(`CALLEP, CALLEP`*`GEUROS, GEUROS`)), 
             rho_3_4=sqrt((`CARSCO, DESCAE`*`DESCAE, CARSCO`)/(`CARSCO, CARSCO`*`DESCAE, DESCAE`)),
             rho_3_5=sqrt((`CARSCO, GENALG`*`GENALG, CARSCO`)/(`CARSCO, CARSCO`*`GENALG, GENALG`)),
             rho_3_6=sqrt((`CARSCO, GEUROS`*`GEUROS, CARSCO`)/(`CARSCO, CARSCO`*`GEUROS, GEUROS`)), 
             rho_4_5=sqrt((`DESCAE, GENALG`*`GENALG, DESCAE`)/(`DESCAE, DESCAE`*`GENALG, GENALG`)),
             rho_4_6=sqrt((`DESCAE, GEUROS`*`GEUROS, DESCAE`)/(`DESCAE, DESCAE`*`GEUROS, GEUROS`)), 
             rho_5_6=sqrt((`GENALG, GEUROS`*`GEUROS, GENALG`)/(`GENALG, GENALG`*`GEUROS, GEUROS`))) 

rhos<-select(rhos, contains("rho"))          
#add remainder for full pairs 
rhos$rho_2_1<-rhos$rho_1_2
rhos$rho_3_1<-rhos$rho_1_3
rhos$rho_3_2<-rhos$rho_2_3
rhos$rho_4_1<-rhos$rho_1_4
rhos$rho_4_2<-rhos$rho_2_4
rhos$rho_4_3<-rhos$rho_3_4
rhos$rho_5_1<-rhos$rho_1_5
rhos$rho_5_2<-rhos$rho_2_5
rhos$rho_5_3<-rhos$rho_3_5
rhos$rho_5_4<-rhos$rho_4_5
rhos$rho_6_1<-rhos$rho_1_6
rhos$rho_6_2<-rhos$rho_2_6
rhos$rho_6_3<-rhos$rho_3_6
rhos$rho_6_4<-rhos$rho_4_6
rhos$rho_6_5<-rhos$rho_5_6

rhos<-pivot_longer(rhos, cols=everything(), names_to = "spp", values_to = "rho")%>%
  separate(spp, c("param", "spp1", "spp2"), remove = F)%>%
  mutate(spp1=case_when(spp1=="1"~"ARTSCO",
                        spp1=="2"~"CALLEP",
                        spp1=="3"~"CARSCO", 
                        spp1=="4"~"DESCAE",
                        spp1=="5"~"GENALG",
                        spp1=="6"~"GEUROS"))%>% 
  mutate(spp2=case_when(spp2=="1"~"ARTSCO",
                        spp2=="2"~"CALLEP",
                        spp2=="3"~"CARSCO", 
                        spp2=="4"~"DESCAE",
                        spp2=="5"~"GENALG",
                        spp2=="6"~"GEUROS"))%>%
  mutate(dom1=case_when(spp1=="ARTSCO"~"DOM",
                        spp1=="GEUROS"~"DOM",
                        spp1=="DESCAE"~"DOM",
                        spp1=="CALLEP"~"SUBDOM",
                        spp1=="CARSCO"~"SUBDOM",  
                        spp1=="GENALG"~"SUBDOM"))%>%
  mutate(dom2=case_when(spp2=="ARTSCO"~"DOM",
                        spp2=="GEUROS"~"DOM",
                        spp2=="DESCAE"~"DOM",
                        spp2=="CALLEP"~"SUBDOM",
                        spp2=="CARSCO"~"SUBDOM",  
                        spp2=="GENALG"~"SUBDOM"))
#SnowNplot<-ggplot(rhos, aes(x=log(rho),fill=spp2))+ geom_density(alpha=0.3)+
 # geom_vline(aes(xintercept=0), lty=2, color="red")+ #xlim (-3, 2)+
  #xlab("Log Rho")+ theme_classic()+ facet_wrap(~spp1)+ggtitle("Snow + N addition")

SnowNplot<-ggplot(subset(rhos, dom1=='DOM'&dom2=="DOM"), aes(x=log(rho),fill=spp2))+ geom_density(alpha=0.3)+
  geom_vline(aes(xintercept=0), lty=2, color="red")+ #xlim (-3, 2)+
  xlab("Log Rho")+ theme_classic()+ facet_wrap(~spp1)+ ggtitle("Snow + N addition")

#ggplot(rhos, aes(x=log(rho),fill=dom2))+ geom_density(alpha=0.3)+
 # geom_vline(aes(xintercept=0), lty=2, color="red")+ #xlim (-3, 2)+
  #xlab("Log Rho")+ theme_classic()+ facet_wrap(~spp1)+ ggtitle("Snow + N addition")

#PNW----
load(file = "outputs/modDAtime_PNWoutput.RData")
alphas<-as.data.frame(modDAtimePNW$chains$alphaGibbs)
names<-modDAtimePNW$parameters$alphaTable$`alpha_{to, from}`
colnames(alphas)<-names

#subset to 3 dominant, 3 subdominant spp (>10% rel abund control plots)
alphas<-select(alphas, matches("ARTSCO|DESCAE|GEUROS|CALLEP|CARSCO|GENALG"))
alphas<-select(alphas, !matches("BISBIS|RARESPP|TRIPAR"))

#spp
#1-ARTSCO
#2-CALLEP
#3-CARSCO
#4-DESCAE
#5-GENALG
#6-GEUROS

rhos<-mutate(alphas, 
             rho_1_2=sqrt((`ARTSCO, CALLEP`*`CALLEP, ARTSCO`)/(`ARTSCO, ARTSCO`*`CALLEP, CALLEP`)), 
             rho_1_3=sqrt((`ARTSCO, CARSCO`*`CARSCO, ARTSCO`)/(`ARTSCO, ARTSCO`*`CARSCO, CARSCO`)),
             rho_1_4=sqrt((`ARTSCO, DESCAE`*`DESCAE, ARTSCO`)/(`ARTSCO, ARTSCO`*`DESCAE, DESCAE`)),
             rho_1_5=sqrt((`ARTSCO, GENALG`*`GENALG, ARTSCO`)/(`ARTSCO, ARTSCO`*`GENALG, GENALG`)),
             rho_1_6=sqrt((`ARTSCO, GEUROS`*`GEUROS, ARTSCO`)/(`ARTSCO, ARTSCO`*`GEUROS, GEUROS`)), 
             rho_2_3=sqrt((`CALLEP, CARSCO`*`CARSCO, CALLEP`)/(`CALLEP, CALLEP`*`CARSCO, CARSCO`)),
             rho_2_4=sqrt((`CALLEP, DESCAE`*`DESCAE, CALLEP`)/(`CALLEP, CALLEP`*`DESCAE, DESCAE`)),
             rho_2_5=sqrt((`CALLEP, GENALG`*`GENALG, CALLEP`)/(`CALLEP, CALLEP`*`GENALG, GENALG`)),
             rho_2_6=sqrt((`CALLEP, GEUROS`*`GEUROS, CALLEP`)/(`CALLEP, CALLEP`*`GEUROS, GEUROS`)), 
             rho_3_4=sqrt((`CARSCO, DESCAE`*`DESCAE, CARSCO`)/(`CARSCO, CARSCO`*`DESCAE, DESCAE`)),
             rho_3_5=sqrt((`CARSCO, GENALG`*`GENALG, CARSCO`)/(`CARSCO, CARSCO`*`GENALG, GENALG`)),
             rho_3_6=sqrt((`CARSCO, GEUROS`*`GEUROS, CARSCO`)/(`CARSCO, CARSCO`*`GEUROS, GEUROS`)), 
             rho_4_5=sqrt((`DESCAE, GENALG`*`GENALG, DESCAE`)/(`DESCAE, DESCAE`*`GENALG, GENALG`)),
             rho_4_6=sqrt((`DESCAE, GEUROS`*`GEUROS, DESCAE`)/(`DESCAE, DESCAE`*`GEUROS, GEUROS`)), 
             rho_5_6=sqrt((`GENALG, GEUROS`*`GEUROS, GENALG`)/(`GENALG, GENALG`*`GEUROS, GEUROS`))) 

rhos<-select(rhos, contains("rho"))          
#add remainder for full pairs 
rhos$rho_2_1<-rhos$rho_1_2
rhos$rho_3_1<-rhos$rho_1_3
rhos$rho_3_2<-rhos$rho_2_3
rhos$rho_4_1<-rhos$rho_1_4
rhos$rho_4_2<-rhos$rho_2_4
rhos$rho_4_3<-rhos$rho_3_4
rhos$rho_5_1<-rhos$rho_1_5
rhos$rho_5_2<-rhos$rho_2_5
rhos$rho_5_3<-rhos$rho_3_5
rhos$rho_5_4<-rhos$rho_4_5
rhos$rho_6_1<-rhos$rho_1_6
rhos$rho_6_2<-rhos$rho_2_6
rhos$rho_6_3<-rhos$rho_3_6
rhos$rho_6_4<-rhos$rho_4_6
rhos$rho_6_5<-rhos$rho_5_6

rhos<-pivot_longer(rhos, cols=everything(), names_to = "spp", values_to = "rho")%>%
  separate(spp, c("param", "spp1", "spp2"), remove = F)%>%
  mutate(spp1=case_when(spp1=="1"~"ARTSCO",
                        spp1=="2"~"CALLEP",
                        spp1=="3"~"CARSCO", 
                        spp1=="4"~"DESCAE",
                        spp1=="5"~"GENALG",
                        spp1=="6"~"GEUROS"))%>% 
  mutate(spp2=case_when(spp2=="1"~"ARTSCO",
                        spp2=="2"~"CALLEP",
                        spp2=="3"~"CARSCO", 
                        spp2=="4"~"DESCAE",
                        spp2=="5"~"GENALG",
                        spp2=="6"~"GEUROS"))%>%
  mutate(dom1=case_when(spp1=="ARTSCO"~"DOM",
                        spp1=="GEUROS"~"DOM",
                        spp1=="DESCAE"~"DOM",
                        spp1=="CALLEP"~"SUBDOM",
                        spp1=="CARSCO"~"SUBDOM",  
                        spp1=="GENALG"~"SUBDOM"))%>%
  mutate(dom2=case_when(spp2=="ARTSCO"~"DOM",
                        spp2=="GEUROS"~"DOM",
                        spp2=="DESCAE"~"DOM",
                        spp2=="CALLEP"~"SUBDOM",
                        spp2=="CARSCO"~"SUBDOM",  
                        spp2=="GENALG"~"SUBDOM"))

#WSnowNplot<-ggplot(rhos, aes(x=log(rho),fill=spp2))+ geom_density(alpha=0.3)+
#  geom_vline(aes(xintercept=0), lty=2, color="red")+ #xlim (-3, 2)+
 # xlab("Log Rho")+ theme_classic()+ facet_wrap(~spp1)+ggtitle("Warm + Snow + N addition")

WSnowNplot<-ggplot(subset(rhos, dom1=='DOM'&dom2=='DOM'), aes(x=log(rho),fill=spp2))+ geom_density(alpha=0.3)+
  geom_vline(aes(xintercept=0), lty=2, color="red")+ #xlim (-3, 2)+
  xlab("Log Rho")+ theme_classic()+ facet_wrap(~spp1)+ ggtitle("Warm + Snow + N addition")

#WSnowNplot<-ggplot(rhos, aes(x=log(rho),fill=dom2))+ geom_density(alpha=0.3)+
 # geom_vline(aes(xintercept=0), lty=2, color="red")+ #xlim (-3, 2)+
  #xlab("Log Rho")+ theme_classic()+ facet_wrap(~spp1)+ggtitle("Warm + Snow + N addition")

#cobine all----
gridExtra::grid.arrange(CTLplot, Snowplot, Warmplot, Nplot, SnowNplot, WSnowNplot)

