#rhos---- 
#use unstandardized rhos so can compare across treatments  
#XXX----
rhos<-as.data.frame(modDAtimeXXX$chains$lgibbs)
#calculate fitness ratios
#rhos<-select(rhos, contains("intercept"))
#ARTSCO=spp1
#DESCAE=spp2
#GEUM=spp3
fitness<-rowwise(rhos)%>%
  mutate(fitness12=log(ARTSCO_intercept/DESCAE_intercept+2), 
         fitness13=log(ARTSCO_intercept/GEUROS_intercept+2), 
         fitness21=log(DESCAE_intercept/ARTSCO_intercept+2), 
         fitness23=log(DESCAE_intercept/GEUROS_intercept+2), 
         fitness31=log(GEUROS_intercept/ARTSCO_intercept+2), 
         fitness32=log(GEUROS_intercept/DESCAE_intercept+2))%>%
  select(contains('fitness'))


ci12<-ci(fitness$fitness12, ci=0.95)
ci12$mu<-mean(fitness$fitness12) 
ci12$spp<-"ARTSCO/DESCAE"

ci13<-ci(fitness$fitness13, ci=0.95)
ci13$mu<-mean(fitness$fitness13) 
ci13$spp<-"ARTSCO/GEUROS"

ci21<-ci(fitness$fitness21, ci=0.95)
ci21$mu<-mean(fitness$fitness21) 
ci21$spp<-"DESCAE/ARTSCO"

ci23<-ci(fitness$fitness23, ci=0.95)
ci23$mu<-mean(fitness$fitness23) 
ci23$spp<-"DESCAE/GEUROS"

ci31<-ci(fitness$fitness31, ci=0.95)
ci31$mu<-mean(fitness$fitness31) 
ci31$spp<-"GEUROS/ARTSCO"

ci32<-ci(fitness$fitness32, ci=0.95)
ci32$mu<-mean(fitness$fitness32) 
ci32$spp<-"GEUROS/DESCAE"

fitness_ciXXX<-rbind(ci12, ci13, ci21, ci23, ci31, ci32)
fitness_ciXXX$treat<-"XXX"

#full posteriors for plotting 
post_fitnessXXX<-pivot_longer(fitness, 
                              cols=everything(),
                              values_to = "fitness_ratio", 
                              names_to = "spp")%>%
  mutate(spp=case_when(spp=="fitness12"~"ARTSCO/DESCAE", 
                       spp=="fitness13"~"ARTSCO/GEUROS", 
                       spp=="fitness21"~"DESCAE/ARTSCO", 
                       spp=="fitness23"~"DESCAE/GEUROS",
                       spp=="fitness31"~"GEUROS/ARTSCO",
                       spp=="fitness32"~"GEUROS/DESCAE"))
post_fitnessXXX$spp<-as.factor(post_fitnessXXX$spp)
post_fitnessXXX$treat<-"XXX"
#remove outliers 
post_fitnessXXX<-filter(post_fitnessXXX, 
                        fitness_ratio<fitness_ciXXX$CI_high&
                          fitness_ratio>fitness_ciXXX$CI_low)

#remake gjam plots with unstandarized values 
rhos<-as.data.frame(modDAtimeXXX$chains$lgibbsUn)
rhos<-pivot_longer(rhos,cols=everything(), 
                   names_to = c("spp", "treat"),
                   names_sep = "_", values_to = "rho")

XXXrhoplot<-ggplot(rhos, aes(x=spp, y=rho, fill=spp))+ geom_boxplot()+ 
  facet_wrap(~treat, scales="free")+ggtitle("Control")+
  geom_hline(yintercept=0, lty=2)+
  theme(legend.position = "none")

#XNX----
rhos<-as.data.frame(modDAtimeXNX$chains$lgibbs)
#calculate fitness ratios 
#rhos<-select(rhos, contains("intercept"))
#ARTSCO=spp1
#DESCAE=spp2
#GEUM=spp3
fitness<-rowwise(rhos)%>%
  mutate(fitness12=ARTSCO_intercept/DESCAE_intercept, 
         fitness13=ARTSCO_intercept/GEUROS_intercept, 
         fitness21=DESCAE_intercept/ARTSCO_intercept, 
         fitness23=DESCAE_intercept/GEUROS_intercept, 
         fitness31=GEUROS_intercept/ARTSCO_intercept, 
         fitness32=GEUROS_intercept/DESCAE_intercept)%>%
  select(contains('fitness'))

ci12<-ci(fitness$fitness12, ci=0.95)
ci12$mu<-mean(fitness$fitness12) 
ci12$spp<-"ARTSCO/DESCAE"

ci13<-ci(fitness$fitness13, ci=0.95)
ci13$mu<-mean(fitness$fitness13) 
ci13$spp<-"ARTSCO/GEUROS"

ci21<-ci(fitness$fitness21, ci=0.95)
ci21$mu<-mean(fitness$fitness21) 
ci21$spp<-"DESCAE/ARTSCO"

ci23<-ci(fitness$fitness23, ci=0.95)
ci23$mu<-mean(fitness$fitness23) 
ci23$spp<-"DESCAE/GEUROS"

ci31<-ci(fitness$fitness31, ci=0.95)
ci31$mu<-mean(fitness$fitness31) 
ci31$spp<-"GEUROS/ARTSCO"

ci32<-ci(fitness$fitness32, ci=0.95)
ci32$mu<-mean(fitness$fitness32) 
ci32$spp<-"GEUROS/DESCAE"

fitness_ciXNX<-rbind(ci12, ci13, ci21, ci23, ci31, ci32)
fitness_ciXNX$treat<-"XNX"


#full posteriors for plotting 
post_fitnessXNX<-pivot_longer(fitness, 
                              cols=everything(),
                              values_to = "fitness_ratio", 
                              names_to = "spp")%>%
  mutate(spp=case_when(spp=="fitness12"~"ARTSCO/DESCAE", 
                       spp=="fitness13"~"ARTSCO/GEUROS", 
                       spp=="fitness21"~"DESCAE/ARTSCO", 
                       spp=="fitness23"~"DESCAE/GEUROS",
                       spp=="fitness31"~"GEUROS/ARTSCO",
                       spp=="fitness32"~"GEUROS/DESCAE"))
post_fitnessXNX$spp<-as.factor(post_fitnessXNX$spp)
post_fitnessXNX$treat<-"XNX"
#remove outliers 
post_fitnessXNX<-filter(post_fitnessXNX, 
                        fitness_ratio<fitness_ciXNX$CI_high&
                          fitness_ratio>fitness_ciXNX$CI_low)

#remake gjam plots with unstandarized values 
rhos<-as.data.frame(modDAtimeXNX$chains$lgibbsUn)
rhos<-pivot_longer(rhos,cols=everything(), 
                   names_to = c("spp", "treat"),
                   names_sep = "_", values_to = "rho")

XNXrhoplot<-ggplot(rhos, aes(x=spp, y=rho, fill=spp))+ geom_boxplot()+ 
  facet_wrap(~treat, scales="free")+ggtitle("N addition")+
  geom_hline(yintercept=0, lty=2)+
  theme(legend.position = "none")

#XXW----
rhos<-as.data.frame(modDAtimeXXW$chains$lgibbs)
#calculate fitness ratios
rhos<-select(rhos, contains("intercept"))
#ARTSCO=spp1
#DESCAE=spp2
#GEUM=spp3
fitness<-rowwise(rhos)%>%
  mutate(fitness12=ARTSCO_intercept/DESCAE_intercept, 
         fitness13=ARTSCO_intercept/GEUROS_intercept, 
         fitness21=DESCAE_intercept/ARTSCO_intercept, 
         fitness23=DESCAE_intercept/GEUROS_intercept, 
         fitness31=GEUROS_intercept/ARTSCO_intercept, 
         fitness32=GEUROS_intercept/DESCAE_intercept)%>%
  select(contains('fitness'))
ci12<-ci(fitness$fitness12, ci=0.95)
ci12$mu<-mean(fitness$fitness12) 
ci12$spp<-"ARTSCO/DESCAE"

ci13<-ci(fitness$fitness13, ci=0.95)
ci13$mu<-mean(fitness$fitness13) 
ci13$spp<-"ARTSCO/GEUROS"

ci21<-ci(fitness$fitness21, ci=0.95)
ci21$mu<-mean(fitness$fitness21) 
ci21$spp<-"DESCAE/ARTSCO"

ci23<-ci(fitness$fitness23, ci=0.95)
ci23$mu<-mean(fitness$fitness23) 
ci23$spp<-"DESCAE/GEUROS"

ci31<-ci(fitness$fitness31, ci=0.95)
ci31$mu<-mean(fitness$fitness31) 
ci31$spp<-"GEUROS/ARTSCO"

ci32<-ci(fitness$fitness32, ci=0.95)
ci32$mu<-mean(fitness$fitness32) 
ci32$spp<-"GEUROS/DESCAE"

fitness_ciXXW<-rbind(ci12, ci13, ci21, ci23, ci31, ci32)
fitness_ciXXW$treat<-"XXW"


#full posteriors for plotting 
post_fitnessXXW<-pivot_longer(fitness, 
                              cols=everything(),
                              values_to = "fitness_ratio", 
                              names_to = "spp")%>%
  mutate(spp=case_when(spp=="fitness12"~"ARTSCO/DESCAE", 
                       spp=="fitness13"~"ARTSCO/GEUROS", 
                       spp=="fitness21"~"DESCAE/ARTSCO", 
                       spp=="fitness23"~"DESCAE/GEUROS",
                       spp=="fitness31"~"GEUROS/ARTSCO",
                       spp=="fitness32"~"GEUROS/DESCAE"))
post_fitnessXXW$spp<-as.factor(post_fitnessXXW$spp)
post_fitnessXXW$treat<-"XXW"
#remove outliers 
post_fitnessXXW<-filter(post_fitnessXXW, 
                        fitness_ratio<fitness_ciXXW$CI_high&
                          fitness_ratio>fitness_ciXXW$CI_low)
#remake gjam plots with unstandarized values 
rhos<-as.data.frame(modDAtimeXXW$chains$lgibbsUn)
rhos<-pivot_longer(rhos,cols=everything(), 
                   names_to = c("spp", "treat"),
                   names_sep = "_", values_to = "rho")

XXWrhoplot<-ggplot(rhos, aes(x=spp, y=rho, fill=spp))+ geom_boxplot()+ 
  facet_wrap(~treat, scales="free")+ggtitle("Warming")+
  geom_hline(yintercept=0, lty=2)+
  theme(legend.position = "none")


#PXX----
rhos<-as.data.frame(modDAtimePXX$chains$lgibbs)
#calculate fitness ratios 
rhos<-select(rhos, contains("intercept"))
#ARTSCO=spp1
#DESCAE=spp2
#GEUM=spp3
fitness<-rowwise(rhos)%>%
  mutate(fitness12=ARTSCO_intercept/DESCAE_intercept, 
         fitness13=ARTSCO_intercept/GEUROS_intercept, 
         fitness21=DESCAE_intercept/ARTSCO_intercept, 
         fitness23=DESCAE_intercept/GEUROS_intercept, 
         fitness31=GEUROS_intercept/ARTSCO_intercept, 
         fitness32=GEUROS_intercept/DESCAE_intercept)%>%
  select(contains('fitness'))
ci12<-ci(fitness$fitness12, ci=0.95)
ci12$mu<-mean(fitness$fitness12) 
ci12$spp<-"ARTSCO/DESCAE"

ci13<-ci(fitness$fitness13, ci=0.95)
ci13$mu<-mean(fitness$fitness13) 
ci13$spp<-"ARTSCO/GEUROS"

ci21<-ci(fitness$fitness21, ci=0.95)
ci21$mu<-mean(fitness$fitness21) 
ci21$spp<-"DESCAE/ARTSCO"

ci23<-ci(fitness$fitness23, ci=0.95)
ci23$mu<-mean(fitness$fitness23) 
ci23$spp<-"DESCAE/GEUROS"

ci31<-ci(fitness$fitness31, ci=0.95)
ci31$mu<-mean(fitness$fitness31) 
ci31$spp<-"GEUROS/ARTSCO"

ci32<-ci(fitness$fitness32, ci=0.95)
ci32$mu<-mean(fitness$fitness32) 
ci32$spp<-"GEUROS/DESCAE"

fitness_ciPXX<-rbind(ci12, ci13, ci21, ci23, ci31, ci32)
fitness_ciPXX$treat<-"PXX"

#full posteriors for plotting 
post_fitnessPXX<-pivot_longer(fitness, 
                              cols=everything(),
                              values_to = "fitness_ratio", 
                              names_to = "spp")%>%
  mutate(spp=case_when(spp=="fitness12"~"ARTSCO/DESCAE", 
                       spp=="fitness13"~"ARTSCO/GEUROS", 
                       spp=="fitness21"~"DESCAE/ARTSCO", 
                       spp=="fitness23"~"DESCAE/GEUROS",
                       spp=="fitness31"~"GEUROS/ARTSCO",
                       spp=="fitness32"~"GEUROS/DESCAE"))
post_fitnessPXX$spp<-as.factor(post_fitnessPXX$spp)
post_fitnessPXX$treat<-"PXX"
#remove outliers 
post_fitnessPXX<-filter(post_fitnessPXX, 
                        fitness_ratio<fitness_ciPXX$CI_high&
                          fitness_ratio>fitness_ciPXX$CI_low)
#remake gjam plots with unstandarized values 
rhos<-as.data.frame(modDAtimePXX$chains$lgibbsUn)
rhos<-pivot_longer(rhos,cols=everything(), 
                   names_to = c("spp", "treat"),
                   names_sep = "_", values_to = "rho")

PXXrhoplot<-ggplot(rhos, aes(x=spp, y=rho, fill=spp))+ geom_boxplot()+ 
  facet_wrap(~treat, scales="free")+ggtitle("Snow addition")+
  geom_hline(yintercept=0, lty=2)+
  theme(legend.position = "none")

#PNX----
rhos<-as.data.frame(modDAtimePNX$chains$lgibbs)
#calculate fitness ratios 
rhos<-select(rhos, contains("intercept"))
#ARTSCO=spp1
#DESCAE=spp2
#GEUM=spp3
fitness<-rowwise(rhos)%>%
  mutate(fitness12=ARTSCO_intercept/DESCAE_intercept, 
         fitness13=ARTSCO_intercept/GEUROS_intercept, 
         fitness21=DESCAE_intercept/ARTSCO_intercept, 
         fitness23=DESCAE_intercept/GEUROS_intercept, 
         fitness31=GEUROS_intercept/ARTSCO_intercept, 
         fitness32=GEUROS_intercept/DESCAE_intercept)%>%
  select(contains('fitness'))
ci12<-ci(fitness$fitness12, ci=0.95)
ci12$mu<-mean(fitness$fitness12) 
ci12$spp<-"ARTSCO/DESCAE"

ci13<-ci(fitness$fitness13, ci=0.95)
ci13$mu<-mean(fitness$fitness13) 
ci13$spp<-"ARTSCO/GEUROS"

ci21<-ci(fitness$fitness21, ci=0.95)
ci21$mu<-mean(fitness$fitness21) 
ci21$spp<-"DESCAE/ARTSCO"

ci23<-ci(fitness$fitness23, ci=0.95)
ci23$mu<-mean(fitness$fitness23) 
ci23$spp<-"DESCAE/GEUROS"

ci31<-ci(fitness$fitness31, ci=0.95)
ci31$mu<-mean(fitness$fitness31) 
ci31$spp<-"GEUROS/ARTSCO"

ci32<-ci(fitness$fitness32, ci=0.95)
ci32$mu<-mean(fitness$fitness32) 
ci32$spp<-"GEUROS/DESCAE"

fitness_ciPNX<-rbind(ci12, ci13, ci21, ci23, ci31, ci32)
fitness_ciPNX$treat<-"PNX"

#full posteriors for plotting 
post_fitnessPNX<-pivot_longer(fitness, 
                              cols=everything(),
                              values_to = "fitness_ratio", 
                              names_to = "spp")%>%
  mutate(spp=case_when(spp=="fitness12"~"ARTSCO/DESCAE", 
                       spp=="fitness13"~"ARTSCO/GEUROS", 
                       spp=="fitness21"~"DESCAE/ARTSCO", 
                       spp=="fitness23"~"DESCAE/GEUROS",
                       spp=="fitness31"~"GEUROS/ARTSCO",
                       spp=="fitness32"~"GEUROS/DESCAE"))
post_fitnessPNX$spp<-as.factor(post_fitnessPNX$spp)
post_fitnessPNX$treat<-"PNX"
#remove outliers 
post_fitnessPNX<-filter(post_fitnessPNX, 
                        fitness_ratio<fitness_ciPNX$CI_high&
                          fitness_ratio>fitness_ciPNX$CI_low)
#remake gjam plots with unstandarized values 
rhos<-as.data.frame(modDAtimePNX$chains$lgibbsUn)
rhos<-pivot_longer(rhos,cols=everything(), 
                   names_to = c("spp", "treat"),
                   names_sep = "_", values_to = "rho")

PNXrhoplot<-ggplot(rhos, aes(x=spp, y=rho, fill=spp))+ geom_boxplot()+ 
  facet_wrap(~treat, scales="free")+ggtitle("Snow + N addition")+
  geom_hline(yintercept=0, lty=2)+
  theme(legend.position = "none")


#PNW----
rhos<-as.data.frame(modDAtimePNW$chains$lgibbs)
#calculate fitness ratios 
rhos<-select(rhos, contains("intercept"))
#ARTSCO=spp1
#DESCAE=spp2
#GEUM=spp3
fitness<-rowwise(rhos)%>%
  mutate(fitness12=ARTSCO_intercept/DESCAE_intercept, 
         fitness13=ARTSCO_intercept/GEUROS_intercept, 
         fitness21=DESCAE_intercept/ARTSCO_intercept, 
         fitness23=DESCAE_intercept/GEUROS_intercept, 
         fitness31=GEUROS_intercept/ARTSCO_intercept, 
         fitness32=GEUROS_intercept/DESCAE_intercept)%>%
  select(contains('fitness'))
ci12<-ci(fitness$fitness12, ci=0.95)
ci12$mu<-mean(fitness$fitness12) 
ci12$spp<-"ARTSCO/DESCAE"

ci13<-ci(fitness$fitness13, ci=0.95)
ci13$mu<-mean(fitness$fitness13) 
ci13$spp<-"ARTSCO/GEUROS"

ci21<-ci(fitness$fitness21, ci=0.95)
ci21$mu<-mean(fitness$fitness21) 
ci21$spp<-"DESCAE/ARTSCO"

ci23<-ci(fitness$fitness23, ci=0.95)
ci23$mu<-mean(fitness$fitness23) 
ci23$spp<-"DESCAE/GEUROS"

ci31<-ci(fitness$fitness31, ci=0.95)
ci31$mu<-mean(fitness$fitness31) 
ci31$spp<-"GEUROS/ARTSCO"

ci32<-ci(fitness$fitness32, ci=0.95)
ci32$mu<-mean(fitness$fitness32) 
ci32$spp<-"GEUROS/DESCAE"

fitness_ciPNW<-rbind(ci12, ci13, ci21, ci23, ci31, ci32)
fitness_ciPNW$treat<-"PNW"

#full posteriors for plotting 
post_fitnessPNW<-pivot_longer(fitness, 
                              cols=everything(),
                              values_to = "fitness_ratio", 
                              names_to = "spp")%>%
  mutate(spp=case_when(spp=="fitness12"~"ARTSCO/DESCAE", 
                       spp=="fitness13"~"ARTSCO/GEUROS", 
                       spp=="fitness21"~"DESCAE/ARTSCO", 
                       spp=="fitness23"~"DESCAE/GEUROS",
                       spp=="fitness31"~"GEUROS/ARTSCO",
                       spp=="fitness32"~"GEUROS/DESCAE"))
post_fitnessPNW$spp<-as.factor(post_fitnessPNW$spp)
post_fitnessPNW$treat<-"PNW"
#remove outliers 
post_fitnessPNW<-filter(post_fitnessPNW, 
                        fitness_ratio<fitness_ciPNW$CI_high&
                          fitness_ratio>fitness_ciPNW$CI_low)
#remake gjam plots with unstandarized values 
rhos<-as.data.frame(modDAtimePNW$chains$lgibbsUn)
rhos<-pivot_longer(rhos,cols=everything(), 
                   names_to = c("spp", "treat"),
                   names_sep = "_", values_to = "rho")

PNWrhoplot<-ggplot(rhos, aes(x=spp, y=rho, fill=spp))+ geom_boxplot()+ 
  facet_wrap(~treat, scales="free")+ggtitle("Warm +Snow + N addition")+
  geom_hline(yintercept=0, lty=2)+
  theme(legend.position = "none")



#combine all---- 
fitness_ci_all<-rbind(fitness_ciXXX, fitness_ciXXW, fitness_ciPXX, fitness_ciXNX)
post_fitness<-rbind(post_fitnessXXX, post_fitnessXNX, 
                    post_fitnessXXW, post_fitnessPXX,
                  post_fitnessPNX, post_fitnessPNW)
#plot all 
ggplot(post_fitness, aes(x=spp, y=log(fitness_ratio+15), fill=treat))+ 
  geom_boxplot()+ylim(2.5, 3)+
  #geom_vline(aes(xintercept=1), lty=2, color="red")
  ylab("Log fitness ratio (r1/r2)")+ theme_classic()

ggplot(post_fitness, aes(x=log(fitness_ratio), fill=treat))+ 
  geom_density(alpha=0.3)+facet_wrap(~spp)+
  geom_vline(xintercept = 0, lty=2)+
  #geom_vline(aes(xintercept=1), lty=2, color="red")
  xlab("Log fitness ratio (r1/r2)")+ theme_classic()

#remake gjam plots with unstandarized values 
rhos<-as.data.frame(modDAtimePXX$chains$lgibbsUn)
rhos<-pivot_longer(rhos,cols=everything(), 
                   names_to = c("spp", "treat"),
                   names_sep = "_", values_to = "rho")

PXXrhoplot<-ggplot(rhos, aes(x=spp, y=rho, fill=spp))+ geom_boxplot()+ 
  facet_wrap(~treat, scales="free")+ggtitle("Snow addition")+
  geom_hline(yintercept=0, lty=2)+
  theme(legend.position = "none")


gridExtra::grid.arrange(XXXrhoplot, XXWrhoplot)
gridExtra::grid.arrange(PXXrhoplot, XNXrhoplot)
 

