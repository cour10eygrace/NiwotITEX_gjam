library(dplyr)
library(bayestestR)
library(ggplot2)
library(tidyr)
library(ggpubr)

#rhos---- 
#XXX
load(file = "outputs/modDAtime_XXXoutput.RData")
rhosXXX<-as.data.frame(modDAtimeXXX$chains$lgibbs)
rhosXXX<-select(rhosXXX, matches("DESCAE"))
rhosXXX$treat<-"XXX"

#XXW
load(file = "outputs/modDAtime_XXWoutput.RData")
rhosXXW<-as.data.frame(modDAtimeXXW$chains$lgibbs)
rhosXXW<-select(rhosXXW, matches("DESCAE"))
rhosXXW$treat<-"XXW"

#PXX
load(file = "outputs/modDAtime_PXXoutput.RData")
rhosPXX<-as.data.frame(modDAtimePXX$chains$lgibbs)
rhosPXX<-select(rhosPXX, matches("DESCAE"))
rhosPXX$treat<-"PXX"

#XNX
load(file = "outputs/modDAtime_XNXoutput.RData")
rhosXNX<-as.data.frame(modDAtimeXNX$chains$lgibbs)
rhosXNX<-select(rhosXNX, matches("DESCAE"))
rhosXNX$treat<-"XNX"

#PNX
load(file = "outputs/modDAtime_PNXoutput.RData")
rhosPNX<-as.data.frame(modDAtimePNX$chains$lgibbs)
rhosPNX<-select(rhosPNX, matches("DESCAE"))
rhosPNX$treat<-"PNX"

#XNW
load(file = "outputs/modDAtime_XNWoutput.RData")
rhosXNW<-as.data.frame(modDAtimeXNW$chains$lgibbs)
rhosXNW<-select(rhosXNW, matches("DESCAE"))
rhosXNW$treat<-"XNW"

#PXW
load(file = "outputs/modDAtime_PXWoutput.RData")
rhosPXW<-as.data.frame(modDAtimePXW$chains$lgibbs)
rhosPXW<-select(rhosPXW, matches("DESCAE"))
rhosPXW$treat<-"PXW"

#PNW
load(file = "outputs/modDAtime_PNWoutput.RData")
rhosPNW<-as.data.frame(modDAtimePNW$chains$lgibbs)
rhosPNW<-select(rhosPNW, matches("DESCAE"))
rhosPNW$treat<-"PNW"

#plot all 
rhosall<-rbind(rhosXXX, rhosXXW, rhosPXX, rhosXNX, rhosPNX,  rhosXNW, rhosPXW, rhosPNW)
ggplot(rhosall, aes(y=DESCAE_depthcm, fill=treat))+ geom_boxplot( )+
  geom_hline(aes(yintercept=0), lty=2, color="red")+# xlim (-3, 2)+
  theme_classic()+ ylab("Effect of snow depth")+ 
  xlab(" ")+ theme(axis.text.x=element_blank(),
                   axis.ticks.x=element_blank())

ggplot(rhosall, aes(y=DESCAE_Ndep, fill=treat))+ geom_boxplot( )+
  geom_hline(aes(yintercept=0), lty=2, color="red")+# xlim (-3, 2)+
  theme_classic()+ ylab("Effect of N deposition")+
  xlab(" ")+ theme(axis.text.x=element_blank(),
                   axis.ticks.x=element_blank())

ggplot(rhosall, aes(y=DESCAE_avgT, fill=treat))+ geom_boxplot( )+
  geom_hline(aes(yintercept=0), lty=2, color="red")+# xlim (-3, 2)+
  theme_classic()+ ylab("Effect of temperature")+
  xlab(" ")+ theme(axis.text.x=element_blank(),
                   axis.ticks.x=element_blank())

#a-priori contrasts---- 
#XXW vs XXX----
deltarho_snow_XXW_XXX<-as.data.frame(
  rhosXXW$DESCAE_depthcm-rhosXXX$DESCAE_depthcm)
deltarho_snow_XXW_XXX<-rename(
  deltarho_snow_XXW_XXX, 
  delta_rho=`rhosXXW$DESCAE_depthcm - rhosXXX$DESCAE_depthcm`)
deltarho_snow_XXW_XXX$treat<-'depthcm' 

deltarho_Ndep_XXW_XXX<-as.data.frame(
  rhosXXW$DESCAE_Ndep-rhosXXX$DESCAE_Ndep)
deltarho_Ndep_XXW_XXX<-rename(
  deltarho_Ndep_XXW_XXX, 
  delta_rho=`rhosXXW$DESCAE_Ndep - rhosXXX$DESCAE_Ndep`)
deltarho_Ndep_XXW_XXX$treat<-"Ndep"

deltarho_temp_XXW_XXX<-as.data.frame(
  rhosXXW$DESCAE_avgT-rhosXXX$DESCAE_avgT)
deltarho_temp_XXW_XXX<-rename(
  deltarho_temp_XXW_XXX, 
  delta_rho=`rhosXXW$DESCAE_avgT - rhosXXX$DESCAE_avgT`)
deltarho_temp_XXW_XXX$treat<-'avgT'

XXW_XXX_ci<-rbind(deltarho_snow_XXW_XXX, deltarho_Ndep_XXW_XXX, 
                        deltarho_temp_XXW_XXX)%>%
  group_by(treat)%>%mutate(ci(delta_rho, ci=0.95))%>%
  select(treat,CI, CI_low, CI_high)%>%distinct(.)

deltarho_XXW_XXX<-rbind(deltarho_snow_XXW_XXX, deltarho_Ndep_XXW_XXX, 
                        deltarho_temp_XXW_XXX)
ggplot(deltarho_XXW_XXX, aes(y=delta_rho, fill=treat))+ geom_boxplot( )+
  geom_hline(aes(yintercept=0), lty=2, color="red")+# xlim (-3, 2)+
  xlab("Delta XXW vs XXX")+ theme_classic()+ facet_wrap(~treat) 

#PXX vs XXX----
deltarho_snow_PXX_XXX<-as.data.frame(
  rhosPXX$DESCAE_depthcm-rhosXXX$DESCAE_depthcm)
deltarho_snow_PXX_XXX<-rename(
  deltarho_snow_PXX_XXX, 
  delta_rho=`rhosPXX$DESCAE_depthcm - rhosXXX$DESCAE_depthcm`)
deltarho_snow_PXX_XXX$treat<-'depthcm' 

deltarho_Ndep_PXX_XXX<-as.data.frame(
  rhosPXX$DESCAE_Ndep-rhosXXX$DESCAE_Ndep)
deltarho_Ndep_PXX_XXX<-rename(
  deltarho_Ndep_PXX_XXX, 
  delta_rho=`rhosPXX$DESCAE_Ndep - rhosXXX$DESCAE_Ndep`)
deltarho_Ndep_PXX_XXX$treat<-"Ndep"

deltarho_temp_PXX_XXX<-as.data.frame(
  rhosPXX$DESCAE_avgT-rhosXXX$DESCAE_avgT)
deltarho_temp_PXX_XXX<-rename(
  deltarho_temp_PXX_XXX, 
  delta_rho=`rhosPXX$DESCAE_avgT - rhosXXX$DESCAE_avgT`)
deltarho_temp_PXX_XXX$treat<-'avgT'

PXX_XXX_ci<-rbind(deltarho_snow_PXX_XXX, deltarho_Ndep_PXX_XXX, 
                  deltarho_temp_PXX_XXX)%>%
  group_by(treat)%>%mutate(ci(delta_rho, ci=0.95))%>%
  select(treat,CI, CI_low, CI_high)%>%distinct(.)

deltarho_PXX_XXX<-rbind(deltarho_snow_PXX_XXX, deltarho_Ndep_PXX_XXX, 
                        deltarho_temp_PXX_XXX)
ggplot(deltarho_PXX_XXX, aes(y=delta_rho, fill=treat))+ geom_boxplot( )+
  geom_hline(aes(yintercept=0), lty=2, color="red")+# xlim (-3, 2)+
  xlab("Delta PXX vs XXX")+ theme_classic()+ facet_wrap(~treat) 

#XNW vs XXW----
deltarho_snow_XNW_XXW<-as.data.frame(
  rhosXNW$DESCAE_depthcm-rhosXXW$DESCAE_depthcm)
deltarho_snow_XNW_XXW<-rename(
  deltarho_snow_XNW_XXW, 
  delta_rho=`rhosXNW$DESCAE_depthcm - rhosXXW$DESCAE_depthcm`)
deltarho_snow_XNW_XXW$treat<-'depthcm' 

deltarho_Ndep_XNW_XXW<-as.data.frame(
  rhosXNW$DESCAE_Ndep-rhosXXW$DESCAE_Ndep)
deltarho_Ndep_XNW_XXW<-rename(
  deltarho_Ndep_XNW_XXW, 
  delta_rho=`rhosXNW$DESCAE_Ndep - rhosXXW$DESCAE_Ndep`)
deltarho_Ndep_XNW_XXW$treat<-'Ndep' 

deltarho_temp_XNW_XXW<-as.data.frame(
  rhosXNW$DESCAE_avgT-rhosXXW$DESCAE_avgT)
deltarho_temp_XNW_XXW<-rename(
  deltarho_temp_XNW_XXW, 
  delta_rho=`rhosXNW$DESCAE_avgT - rhosXXW$DESCAE_avgT`)
deltarho_temp_XNW_XXW$treat<-'avgT' 

deltarho_XNW_XXW<-rbind(deltarho_snow_XNW_XXW, 
                        deltarho_Ndep_XNW_XXW, 
                        deltarho_temp_XNW_XXW)
plotb<-ggplot(deltarho_XNW_XXW, aes(y=delta_rho, fill=treat))+ geom_boxplot( )+
  geom_hline(aes(yintercept=0), lty=2, color="red")+# xlim (-3, 2)+
  xlab("Delta XNW vs XXW")+ theme_classic()+ facet_wrap(~treat) 

#PXW vs XXW----
deltarho_snow_PXW_XXW<-as.data.frame(
  rhosPXW$DESCAE_depthcm-rhosXXW$DESCAE_depthcm)
deltarho_snow_PXW_XXW<-rename(
  deltarho_snow_PXW_XXW, 
  delta_rho=`rhosPXW$DESCAE_depthcm - rhosXXW$DESCAE_depthcm`)
deltarho_snow_PXW_XXW$treat<-'depthcm' 

deltarho_Ndep_PXW_XXW<-as.data.frame(
  rhosPXW$DESCAE_Ndep-rhosXXW$DESCAE_Ndep)
deltarho_Ndep_PXW_XXW<-rename(
  deltarho_Ndep_PXW_XXW, 
  delta_rho=`rhosPXW$DESCAE_Ndep - rhosXXW$DESCAE_Ndep`)
deltarho_Ndep_PXW_XXW$treat<-'Ndep' 

deltarho_temp_PXW_XXW<-as.data.frame(
  rhosPXW$DESCAE_avgT-rhosXXW$DESCAE_avgT)
deltarho_temp_PXW_XXW<-rename(
  deltarho_temp_PXW_XXW, 
  delta_rho=`rhosPXW$DESCAE_avgT - rhosXXW$DESCAE_avgT`)
deltarho_temp_PXW_XXW$treat<-'avgT' 

deltarho_PXW_XXW<-rbind(deltarho_snow_PXW_XXW, 
                        deltarho_Ndep_PXW_XXW, 
                        deltarho_temp_PXW_XXW)
plotc<-ggplot(deltarho_PXW_XXW, aes(y=delta_rho, fill=treat))+ geom_boxplot( )+
  geom_hline(aes(yintercept=0), lty=2, color="red")+# xlim (-3, 2)+
  xlab("Delta PXW vs XXW")+ theme_classic()+ facet_wrap(~treat) 

#PNW vs XNW----
deltarho_snow_PNW_XNW<-as.data.frame(
  rhosPNW$DESCAE_depthcm-rhosXNW$DESCAE_depthcm)
deltarho_snow_PNW_XNW<-rename(
  deltarho_snow_PNW_XNW, 
  delta_rho=`rhosPNW$DESCAE_depthcm - rhosXNW$DESCAE_depthcm`)
deltarho_snow_PNW_XNW$treat<-'depthcm' 

deltarho_Ndep_PNW_XNW<-as.data.frame(
  rhosPNW$DESCAE_Ndep-rhosXNW$DESCAE_Ndep)
deltarho_Ndep_PNW_XNW<-rename(
  deltarho_Ndep_PNW_XNW, 
  delta_rho=`rhosPNW$DESCAE_Ndep - rhosXNW$DESCAE_Ndep`)
deltarho_Ndep_PNW_XNW$treat<-'Ndep' 

deltarho_temp_PNW_XNW<-as.data.frame(
  rhosPNW$DESCAE_avgT-rhosXNW$DESCAE_avgT)
deltarho_temp_PNW_XNW<-rename(
  deltarho_temp_PNW_XNW, 
  delta_rho=`rhosPNW$DESCAE_avgT - rhosXNW$DESCAE_avgT`)
deltarho_temp_PNW_XNW$treat<-'avgT' 

deltarho_PNW_XNW<-rbind(deltarho_snow_PNW_XNW, 
                        deltarho_Ndep_PNW_XNW, 
                        deltarho_temp_PNW_XNW)
plotd<-ggplot(deltarho_PNW_XNW, aes(y=delta_rho, fill=treat))+ geom_boxplot( )+
  geom_hline(aes(yintercept=0), lty=2, color="red")+# xlim (-3, 2)+
  xlab("Delta PNW vs XNW")+ theme_classic()+ facet_wrap(~treat) 

#PNW vs PXW----
deltarho_snow_PNW_PXW<-as.data.frame(
  rhosPNW$DESCAE_depthcm-rhosPXW$DESCAE_depthcm)
deltarho_snow_PNW_PXW<-rename(
  deltarho_snow_PNW_PXW, 
  delta_rho=`rhosPNW$DESCAE_depthcm - rhosPXW$DESCAE_depthcm`)
deltarho_snow_PNW_PXW$treat<-'depthcm' 

deltarho_Ndep_PNW_PXW<-as.data.frame(
  rhosPNW$DESCAE_Ndep-rhosPXW$DESCAE_Ndep)
deltarho_Ndep_PNW_PXW<-rename(
  deltarho_Ndep_PNW_PXW, 
  delta_rho=`rhosPNW$DESCAE_Ndep - rhosPXW$DESCAE_Ndep`)
deltarho_Ndep_PNW_PXW$treat<-'Ndep' 

deltarho_temp_PNW_PXW<-as.data.frame(
  rhosPNW$DESCAE_avgT-rhosPXW$DESCAE_avgT)
deltarho_temp_PNW_PXW<-rename(
  deltarho_temp_PNW_PXW, 
  delta_rho=`rhosPNW$DESCAE_avgT - rhosPXW$DESCAE_avgT`)
deltarho_temp_PNW_PXW$treat<-'avgT' 

deltarho_PNW_PXW<-rbind(deltarho_snow_PNW_PXW, 
                        deltarho_Ndep_PNW_PXW, 
                        deltarho_temp_PNW_PXW)
plote<-ggplot(deltarho_PNW_PXW, aes(y=delta_rho, fill=treat))+ geom_boxplot( )+
  geom_hline(aes(yintercept=0), lty=2, color="red")+# xlim (-3, 2)+
  xlab("Delta PNW vs PXW")+ theme_classic()+ facet_wrap(~treat) 

gridExtra::grid.arrange(plota, plotb, plotc, plotd, plote)






#alphas---- 

#XXX
load(file = "outputs/modDAtime_XXXoutput.RData")
alphasXXX<-as.data.frame(modDAtimeXXX$chains$alphaGibbs)
names<-modDAtimeXXX$parameters$alphaTable$`alpha_{to, from}`
colnames(alphasXXX)<-names
alphasXXX<-select(alphasXXX, matches("DESCAE"))
alphasXXX<-rowwise(alphasXXX)%>%
  mutate(inter1=sum(c_across(`ARTSCO, DESCAE`:`CARSCO, DESCAE`)))%>%
  mutate(inter2=sum(c_across(`GENALG, DESCAE`:`RARESPP, DESCAE`)))%>%
  mutate(inter3=sum(c_across(`DESCAE, ARTSCO`:`DESCAE, CARSCO`)))%>%
  mutate(inter4=sum(c_across(`DESCAE, GENALG`:`DESCAE, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`DESCAE, DESCAE`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)%>%
  select(inter_eff, inter_res, intra)
alphasXXX$treat<-"XXX"


#XXW
load(file = "outputs/modDAtime_XXWoutput.RData")
alphasXXW<-as.data.frame(modDAtimeXXW$chains$alphaGibbs)
names<-modDAtimeXXW$parameters$alphaTable$`alpha_{to, from}`
colnames(alphasXXW)<-names
alphasXXW<-select(alphasXXW, matches("DESCAE"))
alphasXXW<-rowwise(alphasXXW)%>%
  mutate(inter1=sum(c_across(`ARTSCO, DESCAE`:`CARSCO, DESCAE`)))%>%
  mutate(inter2=sum(c_across(`GENALG, DESCAE`:`RARESPP, DESCAE`)))%>%
  mutate(inter3=sum(c_across(`DESCAE, ARTSCO`:`DESCAE, CARSCO`)))%>%
  mutate(inter4=sum(c_across(`DESCAE, GENALG`:`DESCAE, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`DESCAE, DESCAE`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)%>%
  select(inter_eff, inter_res, intra)
alphasXXW$treat<-"XXW"

#PXX
load(file = "outputs/modDAtime_PXXoutput.RData")
alphasPXX<-as.data.frame(modDAtimePXX$chains$alphaGibbs)
names<-modDAtimePXX$parameters$alphaTable$`alpha_{to, from}`
colnames(alphasPXX)<-names
alphasPXX<-select(alphasPXX, matches("DESCAE"))
alphasPXX<-rowwise(alphasPXX)%>%
  mutate(inter1=sum(c_across(`ARTSCO, DESCAE`:`CARSCO, DESCAE`)))%>%
  mutate(inter2=sum(c_across(`GENALG, DESCAE`:`RARESPP, DESCAE`)))%>%
  mutate(inter3=sum(c_across(`DESCAE, ARTSCO`:`DESCAE, CARSCO`)))%>%
  mutate(inter4=sum(c_across(`DESCAE, GENALG`:`DESCAE, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`DESCAE, DESCAE`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)%>%
  select(inter_eff, inter_res, intra)
alphasPXX$treat<-"PXX"

#XNX
load(file = "outputs/modDAtime_XNXoutput.RData")
alphasXNX<-as.data.frame(modDAtimeXNX$chains$alphaGibbs)
names<-modDAtimeXNX$parameters$alphaTable$`alpha_{to, from}`
colnames(alphasXNX)<-names
alphasXNX<-select(alphasXNX, matches("DESCAE"))
alphasXNX<-rowwise(alphasXNX)%>%
  mutate(inter1=sum(c_across(`ARTSCO, DESCAE`:`CARSCO, DESCAE`)))%>%
  mutate(inter2=sum(c_across(`GENALG, DESCAE`:`RARESPP, DESCAE`)))%>%
  mutate(inter3=sum(c_across(`DESCAE, ARTSCO`:`DESCAE, CARSCO`)))%>%
  mutate(inter4=sum(c_across(`DESCAE, GENALG`:`DESCAE, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`DESCAE, DESCAE`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)%>%
  select(inter_eff, inter_res, intra)
alphasXNX$treat<-"XNX"

#PNX
load(file = "outputs/modDAtime_PNXoutput.RData")
alphasPNX<-as.data.frame(modDAtimePNX$chains$alphaGibbs)
names<-modDAtimePNX$parameters$alphaTable$`alpha_{to, from}`
colnames(alphasPNX)<-names
alphasPNX<-select(alphasPNX, matches("DESCAE"))
alphasPNX<-rowwise(alphasPNX)%>%
  mutate(inter1=sum(c_across(`ARTSCO, DESCAE`:`CARSCO, DESCAE`)))%>%
  mutate(inter2=sum(c_across(`GENALG, DESCAE`:`RARESPP, DESCAE`)))%>%
  mutate(inter3=sum(c_across(`DESCAE, ARTSCO`:`DESCAE, CARSCO`)))%>%
  mutate(inter4=sum(c_across(`DESCAE, GENALG`:`DESCAE, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`DESCAE, DESCAE`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)%>%
  select(inter_eff, inter_res, intra)
alphasPNX$treat<-"PNX"

#XNW
load(file = "outputs/modDAtime_XNWoutput.RData")
alphasXNW<-as.data.frame(modDAtimeXNW$chains$alphaGibbs)
names<-modDAtimeXNW$parameters$alphaTable$`alpha_{to, from}`
colnames(alphasXNW)<-names
alphasXNW<-select(alphasXNW, matches("DESCAE"))
alphasXNW<-rowwise(alphasXNW)%>%
  mutate(inter1=sum(c_across(`ARTSCO, DESCAE`:`CARSCO, DESCAE`)))%>%
  mutate(inter2=sum(c_across(`GENALG, DESCAE`:`RARESPP, DESCAE`)))%>%
  mutate(inter3=sum(c_across(`DESCAE, ARTSCO`:`DESCAE, CARSCO`)))%>%
  mutate(inter4=sum(c_across(`DESCAE, GENALG`:`DESCAE, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`DESCAE, DESCAE`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)%>%
  select(inter_eff, inter_res, intra)
alphasXNW$treat<-"XNW"

#PXW
load(file = "outputs/modDAtime_PXWoutput.RData")
alphasPXW<-as.data.frame(modDAtimePXW$chains$alphaGibbs)
names<-modDAtimeXXX$parameters$alphaTable$`alpha_{to, from}`
colnames(alphasPXW)<-names
alphasPXW<-select(alphasPXW, matches("DESCAE"))
alphasPXW<-rowwise(alphasPXW)%>%
  mutate(inter1=sum(c_across(`ARTSCO, DESCAE`:`CARSCO, DESCAE`)))%>%
  mutate(inter2=sum(c_across(`GENALG, DESCAE`:`RARESPP, DESCAE`)))%>%
  mutate(inter3=sum(c_across(`DESCAE, ARTSCO`:`DESCAE, CARSCO`)))%>%
  mutate(inter4=sum(c_across(`DESCAE, GENALG`:`DESCAE, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`DESCAE, DESCAE`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)%>%
  select(inter_eff, inter_res, intra)
alphasPXW$treat<-"PXW"

#PNW
load(file = "outputs/modDAtime_PNWoutput.RData")
alphasPNW<-as.data.frame(modDAtimePNW$chains$alphaGibbs)
names<-modDAtimePNW$parameters$alphaTable$`alpha_{to, from}`
colnames(alphasPNW)<-names
alphasPNW<-select(alphasPNW, matches("DESCAE"))
alphasPNW<-rowwise(alphasPNW)%>%
  mutate(inter1=sum(c_across(`ARTSCO, DESCAE`:`CARSCO, DESCAE`)))%>%
  mutate(inter2=sum(c_across(`GENALG, DESCAE`:`RARESPP, DESCAE`)))%>%
  mutate(inter3=sum(c_across(`DESCAE, ARTSCO`:`DESCAE, CARSCO`)))%>%
  mutate(inter4=sum(c_across(`DESCAE, GENALG`:`DESCAE, RARESPP`)))%>%
  mutate(inter_eff=(inter1+inter2)/8, inter_res=(inter3+inter4)/8)%>%
  select(-inter1, -inter2, -inter3, -inter4)%>% mutate(intra=`DESCAE, DESCAE`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)%>%
  select(inter_eff, inter_res, intra)
alphasPNW$treat<-"PNW"


alphasall<-rbind(alphasXXX, alphasXXW, alphasPXX, alphasXNX, alphasPNX,  alphasXNW, alphasPXW, alphasPNW)

ggplot(alphasall, aes(y=intra, fill=treat))+ 
  geom_boxplot()+ylim(-0.25, -1)+
  #geom_hline(aes(yintercept=0), lty=2, color="red")+# xlim (-3, 2)+
  theme_classic()+ ylab("Intraspp competition")+ 
  xlab(" ")+ theme(axis.text.x=element_blank(),
                   axis.ticks.x=element_blank())

ggplot(alphasall, aes(y=inter_res, fill=treat))+ geom_boxplot( )+
  #geom_hline(aes(yintercept=0), lty=2, color="red")+# xlim (-3, 2)+
  theme_classic()+ ylab("Interspecific response competition")+
  xlab(" ")+ theme(axis.text.x=element_blank(),
                   axis.ticks.x=element_blank())

ggplot(alphasall, aes(y=inter_eff, fill=treat))+ geom_boxplot( )+
  #geom_hline(aes(yintercept=0), lty=2, color="red")+# xlim (-3, 2)+
  theme_classic()+ ylab("Interspecific effect competition")+
  xlab(" ")+ theme(axis.text.x=element_blank(),
                   axis.ticks.x=element_blank())

ggplot(alphasall, aes(y=(inter_res+inter_eff)/2, fill=treat))+ geom_boxplot( )+
  #geom_hline(aes(yintercept=0), lty=2, color="red")+# xlim (-3, 2)+
  theme_classic()+ ylab("Interspecific competition")+
  xlab(" ")+ theme(axis.text.x=element_blank(),
                   axis.ticks.x=element_blank())


