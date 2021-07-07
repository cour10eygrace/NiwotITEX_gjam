library(dplyr)
library(tidyr)
library(tidyverse)
library(broom)
library(RColorBrewer)
library(viridisLite)
library(ggplot2)
library(ggpubr)
library(grid)
library(gridGraphics)
library(gridExtra)
library(lme4)
library(lmerTest)
library(vegan)
library(ape)
library(codyn)
library(optimx)

##make distinct color palette  
plotcol<-c("#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", 
           "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", 
           "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D", 
           "#8A7C64", "#599861", "#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", 
           "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", 
           "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D", 
           "#8A7C64", "#599861")

#NWT species counts----
#all years-QA/QC JGS 12/18/20
sppcomp <- read.csv("C:/Users/court/Google Drive/CU Postdoc/LTREB/data/NWT_ITEX_SpComp_data_L1.csv")
sppcomp<-filter(sppcomp, !is.na(year))#remove spaces 
sppcomp<-rename(sppcomp, spp=JGS_code)%>%unite(., plotyear, plot, year, remove=F)

#remove non-vascular plants and Salix glauca transplants 
sppcomp <- subset(sppcomp, spp != "bare" & spp != "litter" & spp != "moss" & spp != "lichen" &
                    spp != "rock" & spp != "FORB_SP1" & spp != "SALGLA")

#Munge spp comp data 
# calculate species hits and relative abundance within each plot x year
spp_abund<-sppcomp %>%group_by(year, plot, spp) %>% 
  summarise(spp_hits=sum(hits))%>%
  group_by(., year, plot, .drop = FALSE) %>%
  mutate(veg_hits=sum(spp_hits))%>%
  mutate(rel_abund=(spp_hits/veg_hits)*100)%>%
  unite(., plotyear, plot, year, remove=F)#%>%

# add treatment codes back in
spp_abund <- left_join(spp_abund, select(sppcomp, plot,block, code,snow, N, temp)) %>%
  distinct(.) %>%
  unite(plotyear, plot, year, remove = F)

#plot raw cover over time 
#break up into abundance groups 
spp_abund<-mutate(spp_abund, group=case_when(spp=="DESCAE"~"DOM",
                                             spp=="ARTSCO"|spp=="GEUROS"|spp=="CARSCO"~"SUBDOM", 
                                             spp=="CALLEP"|spp=="BISBIS"|spp=="TRIPAR"|spp=="GENALG"~"MODERATE", 
                                             TRUE~"RARE"))

ggplot(data=spp_abund, aes(x = year, y = spp_hits, color = group)) +
  geom_point() +
  #geom_line()+
    #geom_smooth(method = "lm", se = FALSE) +
    geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ code) +theme_bw() +  scale_color_manual(values=plotcol)


ggplot(data=spp_abund, aes(x = as.factor(year), y = spp_hits, color = group)) +
  geom_boxplot() +
  #geom_line()+
  #geom_smooth(method = "lm", se = FALSE) +
  #geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ code) +theme_bw() +  scale_color_manual(values=plotcol)

#CODYN change over time 
#wrt 2006 for each time point 
spp_change<-abundance_change(df = spp_abund,
                              species.var = "spp",
                              abundance.var = "spp_hits",
                              replicate.var = "plot",
                              time.var = "year", reference.time = '2006')

spp_change<-left_join(spp_change, select(sppcomp, plot, code))%>%distinct(.)

#break up into abundance groups 
spp_change<-mutate(spp_change, group=case_when(spp=="DESCAE"~"DOM",
                                             spp=="ARTSCO"|spp=="GEUROS"|spp=="CARSCO"~"SUBDOM", 
                                             spp=="CALLEP"|spp=="BISBIS"|spp=="TRIPAR"|spp=="GENALG"~"MODERATE", 
                                             TRUE~"RARE"))

#plot
ggplot(data=spp_change, aes(x =year2, y=change, color=group))+
  #geom_point(aes(x =year2, y=change, color=group)) + theme_classic()+  
  geom_smooth(se=T)+ylab("cover change from year 1")+ xlab("year")+
  facet_wrap(~ code) +theme_bw()+ scale_color_manual(values=plotcol)

ggplot(data=spp_change, aes(x =year2, y=change, color=group))+
  #geom_point(aes(x =year2, y=change, color=group)) + theme_classic()+  
  geom_smooth(method='lm', se=T, formula = y~x)+
  ylab("cover change from year 1")+ xlab("year")+
  facet_wrap(~ code) +theme_bw()+ scale_color_manual(values=plotcol)


#model change over time 
spp_change<-mutate(spp_change, years=year2-2006)
delta_abund<-lm(change~0+ group:years:code,spp_change)
summary(delta_abund)

#singularity with random effect of plot-level the change is calculated at
delta_abundx<-lmer(change~0+ group:years:code +(1|plot),spp_change, 
control = lmerControl(optimizer= "optimx", optCtrl  = list(method="nlminb")))
summary(delta_abundx)

delta_abundx<-lmer(change~0+ group:years:code +(1|year2) ,spp_change, 
control = lmerControl(optimizer= "optimx", optCtrl  = list(method="nlminb")))
summary(delta_abundx)


#pull out slope coefficients and plot
coeffs<-summary(delta_abundx)
coeffs<-as.data.frame(coeffs$coefficients)
coeffs$grouptreat<-row.names(coeffs)

coeffs<-separate(coeffs, grouptreat, c('group', 'x', 'treat'))%>%
  rename(pval=`Pr(>|t|)`)%>%select(-x)

coeffs<-mutate(coeffs, Response=case_when(Estimate>0 & pval<0.05~"Sig",
                                          Estimate<0 & pval<0.05~"Sig", 
                                          TRUE~"NS"))%>%
  #reorder groups by dominance to match gjam output 
  mutate(group = factor(group, levels=c( "groupDOM", "groupSUBDOM", "groupMODERATE", "groupRARE")))

#plot slope coefficients
ggplot(data=coeffs, aes(x =Estimate, y=treat, color=group, shape=Response))+
  geom_point(stat="identity", position="identity", size=2) + theme_classic() +
  geom_vline(xintercept = 0, lty=2)+ xlab("Slope change in abundance/yr")+ 
  scale_color_manual(values=plotcol)+xlim(-1, 2.5)


#plot change over time with model slopes 
coeffs<-mutate(coeffs, 
        group=case_when(group=="groupDOM"~"DOM", 
                        group=="groupSUBDOM"~"SUBDOM", 
                        group=="groupMODERATE"~"MODERATE", 
                        group=="groupRARE"~"RARE"))

coeffs<-mutate(coeffs, 
    code=case_when(treat=="codeXXX"~"XXX",
                   treat=="codeXXW"~"XXW",
                   treat=="codeXNX"~"XNX",
                   treat=="codePXX"~"PXX",
                   treat=="codePXW"~"PXW",
                   treat=="codeXNW"~"XNW",
                   treat=="codePNX"~"PNX",
                   treat=="codePNW"~"PNW"))
                  

spp_changex<-left_join(spp_change, coeffs)        

#plot model predlines 
predslm = predict(delta_abund, interval = "confidence", level = 0.95)
head(predslm)

spp_changex<-cbind(spp_changex, predslm)

#plots
ggplot(data=spp_changex, aes(x =year2, y=change, color=group))+
  geom_line(aes(y=fit))+
  geom_ribbon( aes(ymin = lwr, ymax = upr, color= group, color = NULL), alpha = .15) +
  geom_point(aes(x =year2, y=change, color=group)) + theme_classic()+  
  #geom_jitter(aes(x =year2, y=change, color=group)) + theme_classic()+  
    ylab("cover change from year 1")+ xlab("year")+
  facet_wrap(~ code) +theme_bw()+ scale_color_manual(values=plotcol)

#w free scales on y axis 
ggplot(data=spp_changex, aes(x =year2, y=change, color=group))+
  geom_line(aes(y=fit))+
  geom_ribbon( aes(ymin = lwr, ymax = upr, color= group, color = NULL), alpha = .15) +
  geom_point(aes(x =year2, y=change, color=group)) + theme_classic()+  
  #geom_jitter(aes(x =year2, y=change, color=group)) + theme_classic()+  
  ylab("cover change from year 1")+ xlab("year")+
  facet_wrap(~ code, scales = "free") +theme_bw()+ scale_color_manual(values=plotcol)


#lines only 
ggplot(data=spp_changex, aes(x =year2, y=change, color=group))+
  geom_line(aes(y=fit))+
  geom_ribbon( aes(ymin = lwr, ymax = upr, color= group, color = NULL), alpha = .15) +
  #geom_point(aes(x =year2, y=change, color=group)) + theme_classic()+  
  #geom_jitter(aes(x =year2, y=change, color=group)) + theme_classic()+  
  ylab("cover change from year 1")+ xlab("year")+
  facet_wrap(~ code) +theme_bw()+ scale_color_manual(values=plotcol)

#now plot for treatments with warming only 
ggplot(data=subset(spp_changex,code!="XNX"&code!="PNX"& code!="PXX"),
                   aes(x =year2, y=change, color=group))+
  geom_line(aes(y=fit))+
  geom_ribbon( aes(ymin = lwr, ymax = upr, color= group, color = NULL), alpha = .15) +
  geom_point(aes(x =year2, y=change, color=group)) + theme_classic()+  
  #geom_jitter(aes(x =year2, y=change, color=group)) + theme_classic()+  
  ylab("cover change from year 1")+ xlab("year")+
  facet_wrap(~ code) +theme_bw()+ scale_color_manual(values=plotcol)
