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

#from gjamTimeXXX_dom.R
rare<-subset(sppcomp, code=="XXX")%>%group_by(spp)%>%count()
rare<-mutate(rare, plotyears_perc=n/720)
#top 8 spp are in >85% of plot years, remaining spp are in less than half (max=47%)

#look at total abundance in control plots over time to decide dominance categories 
#calculate average hits per plot 
#remove species with less than 20 total hits (observations) across time period 
rarectl<-subset(sppcomp, code=="XXX")%>%group_by(spp)%>%
  summarise(sum_hits=sum(hits))%>%mutate(avg_hits=sum_hits/90)%>%filter(sum_hits>20)

#plot raw cover over time 
#break up into abundance groups 

spp_abund<-filter(spp_abund, spp %in% rarectl$spp)%>%# filter out rare spp (<20 hits)
  mutate(group=case_when(spp=="DESCAE"~"DOM",
                                             spp=="ARTSCO"|spp=="GEUROS"|spp=="CARSCO"~"SUBDOM", 
                                             spp=="CALLEP"|spp=="BISBIS"|spp=="TRIPAR"|spp=="GENALG"~"MODERATE", 
                                             TRUE~"RARE"))
spp_abundx<-subset(spp_abund,code!="XNX"&code!="PNX"& code!="PXX")
unique(spp_abundx$spp)

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

#Were rare spp lost/gained over time? 
rarespp<<-subset(spp_abund, group=="RARE")

#spp w decent coverage
rarespp<- mutate(rarespp, vrare=if_else(spp_hits>3, 1,0))%>%group_by(spp)%>%mutate(vrare=sum(vrare))
                                        
ggplot(data=subset(rarespp,code!="XNX"&code!="PNX"& code!="PXX"&vrare>0&spp!="JUN_SP"), aes(x = year, y = spp_hits, color = code)) +
  geom_point() +
  #geom_line()+
  #geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ spp) +theme_bw() +  scale_color_manual(values=plotcol)+ylab("hits")


#FIG S2-PLOT RAW HITS OF ALL SPP IN GJAMTIME MODEL 
#all spp 
spp_abund<-mutate(spp_abund,group = factor(group, 
                                           levels=c( "DOM", "SUBDOM", "MODERATE", "RARE")))
ggplot(data=subset(spp_abund,code!="XNX"&code!="PNX"& code!="PXX"), aes(x = year, y = log(spp_hits), color = code)) +
  geom_point() +
  #geom_line()+
  #geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(group~spp) +theme_bw() +  scale_color_manual(values=plotcol)



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
  geom_smooth(se=T)+ylab("cover change from pre-treatment")+ xlab("year")+
  facet_wrap(~ code) +theme_bw()+ scale_color_manual(values=plotcol)

ggplot(data=spp_change, aes(x =year2, y=change, color=group))+
  #geom_point(aes(x =year2, y=change, color=group)) + theme_classic()+  
  geom_smooth(method='lm', se=T, formula = y~x)+
  ylab("cover change from year 1")+ xlab("year")+
  facet_wrap(~ code) +theme_bw()+ scale_color_manual(values=plotcol)


#BY GROUPS 
#CODYN change over time 
#wrt 2006 for each time point 
group_abund<-group_by(spp_abundx, group, plot, year)%>%
  mutate(group_hits=sum(spp_hits))%>%
  select(group, plot, year, group_hits)%>%distinct(.)

group_change<-abundance_change(df = group_abund,
                               species.var = "group",
                               abundance.var = "group_hits",
                               replicate.var = "plot",
                               time.var = "year", reference.time = '2006')

group_change<-left_join(group_change, select(sppcomp, plot, code))%>%distinct(.)


#plot
ggplot(data=group_change, aes(x =year2, y=change, color=group))+
  #geom_point(aes(x =year2, y=change, color=group)) + theme_classic()+  
  geom_smooth(se=T)+ylab("cover change from year 1")+ xlab("year")+
  facet_wrap(~ code) +theme_bw()+ scale_color_manual(values=plotcol)

ggplot(data=group_change, aes(x =year2, y=change, color=group))+
  #geom_point(aes(x =year2, y=change, color=group)) + theme_classic()+  
  geom_smooth(method='lm', se=T, formula = y~x)+
  ylab("cover change from year 1")+ xlab("year")+
  facet_wrap(~ code) +theme_bw()+ scale_color_manual(values=plotcol)


#model change over time 
#spp_change<-mutate(spp_change, years=year2-2006)
group_change<-mutate(group_change, years=year2-2006)

#delta_abund<-lm(change~0+ group:years:code,spp_change)
delta_abund<-lm(change~0+ group:years:code,group_change)
summary(delta_abund)

#singularity with random effect of plot-level the change is calculated at
#group_change<-subset(group_change, code!="XNX"&code!="PNX"& code!="PXX") #runs singular fit but results same
delta_abundx<-lmer(change~0+ group:years:code +(1|plot), group_change, 
control = lmerControl(optimizer= "optimx", optCtrl  = list(method="nlminb")))
summary(delta_abundx)

delta_abundx<-lmer(change~0+ group:years:code +(1|year2) ,group_change, 
control = lmerControl(optimizer= "optimx", optCtrl  = list(method="nlminb")))
summary(delta_abundx)
#https://stats.stackexchange.com/questions/138464/identical-ses-for-all-slopes-in-a-regression-on-a-factor


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

coeffs$SE<-coeffs$`Std. Error`

#plot slope coefficients
ggplot(data=coeffs, aes(x =Estimate, y=treat, color=group, shape=Response))+
  geom_point(stat="identity", position="identity", size=3) + theme_classic() +
  #geom_pointrange(aes(xmin = Estimate-SE, xmax = Estimate+SE))+
  geom_vline(xintercept = 0, lty=2)+ xlab("Slope change in cover")+ 
  scale_color_manual(values=plotcol)+xlim(-2, 2.5)

ggplot(data=subset(coeffs,code!="XNX"&code!="PNX"& code!="PXX"),
       aes(x =Estimate, y=treat, color=group))+
  geom_point(stat="identity", position="identity", size=2) + theme_classic() +
  geom_pointrange(aes(xmin = Estimate-SE, xmax = Estimate+SE))+
  geom_vline(xintercept = 0, lty=2)+ xlab("Slope change in cover")+ 
  scale_color_manual(values=plotcol)+xlim(-2, 2.5)

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
                  

#spp_changex<-left_join(spp_change, coeffs)        
group_changex<-left_join(group_change, coeffs)        

#plot model predlines 
#lm model-delta_abund 
#https://aosmith.rbind.io/2018/11/16/plot-fitted-lines/#plotting-separate-slopes-with-geom_smooth
predslm = predict(delta_abund, interval = "confidence", level = 0.95)
head(predslm)

#spp_changex<-cbind(spp_changex, predslm)
group_changex<-cbind(group_changex, predslm)

#plots
ggplot(data=group_changex, aes(x =year2, y=change, color=group))+
  geom_line(aes(y=fit))+
  geom_ribbon( aes(ymin = lwr, ymax = upr, color= group, color = NULL), alpha = .15) +
  geom_point(aes(x =year2, y=change, color=group)) + theme_classic()+  
  #geom_jitter(aes(x =year2, y=change, color=group)) + theme_classic()+  
    ylab("cover change from year 1")+ xlab("year")+
  facet_wrap(~ code) +theme_bw()+ scale_color_manual(values=plotcol)

#w free scales on y axis 
ggplot(data=group_changex, aes(x =year2, y=change, color=group))+
  geom_line(aes(y=fit))+
  geom_ribbon( aes(ymin = lwr, ymax = upr, color= group, color = NULL), alpha = .15) +
  geom_point(aes(x =year2, y=change, color=group)) + theme_classic()+  
  #geom_jitter(aes(x =year2, y=change, color=group)) + theme_classic()+  
  ylab("cover change from year 1")+ xlab("year")+
  facet_wrap(~ code, scales = "free") +theme_bw()+ scale_color_manual(values=plotcol)


#lines only 
ggplot(data=group_changex, aes(x =year2, y=change, color=group))+
  geom_line(aes(y=fit))+
  geom_ribbon( aes(ymin = lwr, ymax = upr, color= group, color = NULL), alpha = .15) +
  #geom_point(aes(x =year2, y=change, color=group)) + theme_classic()+  
  #geom_jitter(aes(x =year2, y=change, color=group)) + theme_classic()+  
  ylab("cover change from year 1")+ xlab("year")+
  facet_wrap(~ code) +theme_bw()+ scale_color_manual(values=plotcol)

#now plot for treatments with warming only 
ggplot(data=subset(group_changex,code!="XNX"&code!="PNX"& code!="PXX"),
                   aes(x =year2, y=change, color=group))+
  geom_line(aes(y=fit))+
  geom_ribbon( aes(ymin = lwr, ymax = upr, color= group), alpha = .15) +
  geom_point(aes(x =year2, y=change, color=group), alpha=0.5) + theme_classic()+  
  #geom_jitter(aes(x =year2, y=change, color=group)) + theme_classic()+  
  ylab("cover change from year 1")+ xlab("year")+
  facet_wrap(~ code) +theme_bw()+ scale_color_manual(values=plotcol)

ggplot(data=subset(group_changex,code!="XNX"&code!="PNX"& code!="PXX"),
       aes(x =year2, y=change, color=group))+
  geom_line(aes(y=fit))+
  geom_ribbon( aes(ymin = lwr, ymax = upr, color= group), alpha = .15) +
  geom_point(aes(x =year2, y=change, color=group), alpha=0.5) + theme_classic()+  
  #geom_jitter(aes(x =year2, y=change, color=group)) + theme_classic()+  
  ylab("cover change from year 1")+ xlab("year")+
  facet_wrap(~ code,scales="free") +theme_bw()+ scale_color_manual(values=plotcol)


#lmer model w year RE
#delta_abundx
#http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
#make new dataframe
newdat = data.frame(group = group_changex$group,
years = group_changex$years,
code = group_changex$code, 
year2=group_changex$year2, 
change=group_changex$change)

head(newdat)
#predict over new data 
newdat$distance = predict(delta_abundx, newdata = newdat, re.form=NA)
mm <- model.matrix(terms(delta_abundx),newdat)
## or newdat$distance <- mm %*% fixef(fm1)
pvar1 <- diag(mm %*% tcrossprod(vcov(delta_abundx),mm))
random<-VarCorr(delta_abundx)
tvar1 <- pvar1+random$year2  ## must be adapted for more complex models
cmult <- 1.96 ## could use 1.96

newdat <- data.frame(
  newdat
  , plo = newdat$distance-cmult*sqrt(pvar1)
  , phi = newdat$distance+cmult*sqrt(pvar1)
  , tlo = newdat$distance-cmult*sqrt(tvar1)
  , thi = newdat$distance+cmult*sqrt(tvar1)
)


#plots
#with fixed effects uncertainty only 
ggplot(data=newdat, aes(x =year2, y=change, color=group))+
  geom_line(aes(y=distance))+
  geom_ribbon( aes(ymin = plo, ymax =  phi, color= group), alpha = .15) +
  geom_point(aes(x =year2, y=change, color=group), alpha=0.5) + theme_classic()+  
  #geom_jitter(aes(x =year2, y=change, color=group)) + theme_classic()+  
  ylab("cover change from year 1")+ xlab("year")+
  facet_wrap(~ code) +theme_bw()+ scale_color_manual(values=plotcol)

#with fixed & random effects uncertainty 
#WHY DOES THIS LOOK THE SAME AS THE ONE ABOVE?? SCALING ISSUE?
#most of the differences in thi/lo and phi/lo are not for the dominant species so 
#it it hard to see in the plots 
ggplot(data=newdat, aes(x =year2, y=change, color=group))+
  geom_line(aes(y=distance))+
  geom_ribbon(aes(ymin = tlo, ymax = thi, color= group,), alpha = .15) +
  geom_point(aes(x =year2, y=change, color=group), alpha=0.5) + theme_classic()+  
  #geom_jitter(aes(x =year2, y=change, color=group)) + theme_classic()+  
  ylab("cover change from year 1")+ xlab("year")+
  facet_wrap(~ code) +theme_bw()+ scale_color_manual(values=plotcol)

#try with scales="free"
#with fixed effects uncertainty only 
ggplot(data=newdat, aes(x =year2, y=change, color=group))+
  geom_line(aes(y=distance))+
  geom_ribbon( aes(ymin = plo, ymax =  phi, color= group), alpha = .15) +
  geom_point(aes(x =year2, y=change, color=group), alpha=0.5) + theme_classic()+  
  #geom_jitter(aes(x =year2, y=change, color=group)) + theme_classic()+  
  ylab("cover change from year 1")+ xlab("year")+
  facet_wrap(~ code, scales = "free") +theme_bw()+ scale_color_manual(values=plotcol)

#with fixed & random effects uncertainty 
ggplot(data=newdat, aes(x =year2, y=change, color=group))+
  geom_line(aes(y=distance))+
  geom_ribbon(aes(ymin = tlo, ymax = thi, color= group,), alpha = .15) +
  geom_point(aes(x =year2, y=change, color=group), alpha=0.5) + theme_classic()+  
  #geom_jitter(aes(x =year2, y=change, color=group)) + theme_classic()+  
  ylab("cover change from year 1")+ xlab("year")+
  facet_wrap(~ code, scales = "free") +theme_bw()+ scale_color_manual(values=plotcol)

#try without points-can see slight changes
ggplot(data=newdat, aes(x =year2, y=change, color=group))+
  geom_line(aes(y=distance))+
  geom_ribbon( aes(ymin = plo, ymax =  phi, color= group), alpha = .15) +
  #geom_point(aes(x =year2, y=change, color=group), alpha=0.5) + theme_classic()+  
  #geom_jitter(aes(x =year2, y=change, color=group)) + theme_classic()+  
  ylab("cover change from year 1")+ xlab("year")+
  facet_wrap(~ code, scales = "free") +theme_bw()+ scale_color_manual(values=plotcol)

#with fixed & random effects uncertainty 
ggplot(data=newdat, aes(x =year2, y=change, color=group))+
  geom_line(aes(y=distance))+
  geom_ribbon(aes(ymin = tlo, ymax = thi, color= group,), alpha = .15) +
  #geom_point(aes(x =year2, y=change, color=group), alpha=0.5) + theme_classic()+  
  #geom_jitter(aes(x =year2, y=change, color=group)) + theme_classic()+  
  ylab("cover change from year 1")+ xlab("year")+
  facet_wrap(~ code, scales = "free") +theme_bw()+ scale_color_manual(values=plotcol)


###Fig 1
newdat<-mutate(newdat, group = factor(group, 
  levels=c( "DOM", "SUBDOM", "MODERATE", "RARE")))

ggplot(data=subset(newdat, code!="XNX"&code!="PNX"& code!="PXX"), aes(x =year2, y=change, color=group))+
  geom_line(aes(y=distance))+
  geom_ribbon( aes(ymin = plo, ymax =  phi, fill= group), alpha = .25, colour=NA) +
  geom_point(aes(x =year2, y=change, color=group), alpha=0.5) + theme_classic()+  
  #geom_jitter(aes(x =year2, y=change, color=group)) + theme_classic()+  
  ylab("Cover change from pre-treatment")+ xlab("year")+
  facet_wrap(~ code) +theme_bw()+ scale_color_manual(values=plotcol)+ scale_fill_manual(values=plotcol)

