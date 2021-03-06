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
color2<-c("#A98FBA", "#AA865A",  "#74870D", "#1C829D", "#3F4921")

#NWT species counts----
#all years-QA/QC JGS 12/18/20
sppcomp <- read.csv("data/NWT_ITEX_SpComp_data_L1.csv")
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
  unite(., plotyear, plot, year, remove=F)

# add treatment codes back in
spp_abund <- left_join(spp_abund, select(sppcomp, plot,block, code,snow, N, temp)) %>%
  distinct(.) %>%
  unite(plotyear, plot, year, remove = F)

#look at total abundance in control plots over time to decide dominance categories 
#calculate average hits per plot (6 plos * 15 years= 90)
#remove species with less than 20 total hits (observations) across time period 
rarectl<-subset(sppcomp, code=="XXX")%>%group_by(spp)%>% #mutate(sd_hits=sd(hits))%>%
  summarise(sum_hits=sum(hits))%>%mutate(avg_hits=sum_hits/90)%>%filter(sum_hits>20)
std_mean <- function(x) sd(x)/sqrt(length(x))
rarectl2<-subset(sppcomp, code=="XXX")%>%group_by(spp)%>% summarise(sd_hits=sd(hits), se_hits=std_mean(hits))%>%filter(spp %in% rarectl$spp)
rarectl<-left_join(rarectl, rarectl2)  
rarectl<-  mutate(rarectl, group=case_when(spp=="DESCAE"~"DOM",
                                  spp=="ARTSCO"|spp=="GEUROS"|spp=="CARSCO"~"SUBDOM", 
                                  spp=="CALLEP"|spp=="BISBIS"|spp=="TRIPAR"|spp=="GENALG"~"MODERATE", 
                                  TRUE~"RARE"))%>%group_by(group)%>%mutate(group_avg=mean(avg_hits), 
                                                                           group_se=mean(se_hits))

#plot raw cover over time 
#break up into abundance groups 
spp_abund<-filter(spp_abund, spp %in% rarectl$spp)%>%# filter out rare spp (<20 hits)
  mutate(group=case_when(spp=="DESCAE"~"DOM",
                                             spp=="ARTSCO"|spp=="GEUROS"|spp=="CARSCO"~"SUBDOM", 
                                             spp=="CALLEP"|spp=="BISBIS"|spp=="TRIPAR"|spp=="GENALG"~"MODERATE", 
                                             TRUE~"RARE"))
spp_abundx<-subset(spp_abund,code!="XNX"&code!="PNX"& code!="PXX")
unique(spp_abundx$spp) #20 spp

ggplot(data=subset(spp_abund,code!="XNX"&code!="PNX"& code!="PXX"), aes(x = year, y = spp_hits, color = group)) +
  geom_point() +
  #geom_line()+
    #geom_smooth(method = "lm", se = FALSE) +
    geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ code) +theme_bw() +  scale_color_manual(values=plotcol)


#Were rare spp lost/gained over time? 
rarespp<<-subset(spp_abund, group=="RARE")

ggplot(data=subset(rarespp,code!="XNX"&code!="PNX"& code!="PXX"), aes(x = year, y = spp_hits, color = code)) +
  geom_point() +
  #geom_line()+
  geom_smooth(method = "lm", se = FALSE) +
  #geom_smooth( method="loess", se = FALSE) +
  facet_wrap(~ spp, scales="free") +theme_bw() +  scale_color_manual(values=plotcol)+ylab("hits")

#FIG S2-PLOT RAW HITS OF ALL SPP IN GJAMTIME MODEL----
#all spp 
spp_abund<-mutate(spp_abund,group = factor(group, 
                                           levels=c( "DOM", "SUBDOM", "MODERATE", "RARE")))
# plot scales with free y axis 
ggplot(data=subset(spp_abund,code!="XNX"&code!="PNX"& code!="PXX"), aes(x = year, y = log(spp_hits)+1, color = code)) +
  geom_point() +
  #geom_line()+
  #geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(group~spp, scales="free_y") +theme_bw() +  scale_color_manual(values=plotcol)

#CODYN functions----
#BY SPECIES
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
spp_change<-subset(spp_change, code!="XNX"&code!="PNX"& code!="PXX")

#plot
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
  geom_smooth(method='lm', se=T, formula = y~x)+
  ylab("cover change from year 1")+ xlab("year")+
  facet_wrap(~ code) +theme_bw()+ scale_color_manual(values=plotcol)


#model change over time---- 
group_change<-mutate(group_change, years=year2-2006)

delta_abund<-lm(change~0+ group:years:code,group_change)
summary(delta_abund)

#group_change<-subset(group_change, code!="XNX"&code!="PNX"& code!="PXX") 
delta_abundx<-lmer(change~0+ group:years:code +(1|year2) ,group_change, 
control = lmerControl(optimizer= "optimx", optCtrl  = list(method="nlminb")))
summary(delta_abundx)

#Model diagnostics----
plot(delta_abundx)#looks good 
qqnorm(residuals(delta_abundx))#looks good 

#color by years
plot(delta_abundx, resid(., scaled=TRUE) ~ fitted(.), 
     abline = 0,col=group_change$years, xlab="Fitted values",ylab="Standardised residuals")
plot(delta_abundx, resid(., scaled=TRUE) ~ fitted(.), 
     abline = 0,col=group_change$year2, xlab="Fitted values",ylab="Standardised residuals")

mean(group_change$change)
sd(group_change$change)

#residuals by predictor 
#years
ggplot(data.frame(x1=group_change$years,p.resid=residuals(delta_abundx,type="pearson")),
       aes(x=x1,y=p.resid)) +
  geom_point() +
  theme_bw()
#code
ggplot(data.frame(x1=group_change$code,p.resid=residuals(delta_abundx,type="pearson")),
       aes(x=x1,y=p.resid)) +
  geom_point() +
  theme_bw()

#group-uneven for rare 
ggplot(data.frame(x1=group_change$group,p.resid=residuals(delta_abundx,type="pearson")),
       aes(x=x1,y=p.resid)) +
  geom_point() +
  theme_bw()

#year2 (calendar year) 
ggplot(data.frame(x1=group_change$year2,p.resid=residuals(delta_abundx,type="pearson")),
       aes(x=x1,y=p.resid)) +
  geom_point() +
  theme_bw()


#leverage 
lev<-hat(model.matrix(delta_abundx))

#Plot leverage against standardised residuals
plot(resid(delta_abundx,type="pearson")~lev,las=1,ylab="Standardised residuals",xlab="Leverage")


#visualize results----
#pull out slope coefficients and plot
coeffs<-summary(delta_abundx)
coeffs<-as.data.frame(coeffs$coefficients)
coeffs$grouptreat<-row.names(coeffs)
#write.csv(coeffs, "tables/table1.csv")#Table 1

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
#newdat$distance <- mm %*% fixef(delta_abundx)
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
ggplot(data=newdat, aes(x =year2, y=change, color=group))+
  geom_line(aes(y=distance))+
  geom_ribbon(aes(ymin = tlo, ymax = thi, color= group,), alpha = .15) +
  geom_point(aes(x =year2, y=change, color=group), alpha=0.5) + theme_classic()+  
  #geom_jitter(aes(x =year2, y=change, color=group)) + theme_classic()+  
  ylab("cover change from year 1")+ xlab("year")+
  facet_wrap(~ code) +theme_bw()+ scale_color_manual(values=plotcol)


###Fig 1----
newdat<-mutate(newdat, group = factor(group, 
  levels=c( "DOM", "SUBDOM", "MODERATE", "RARE")))

ggplot(data=subset(newdat, code!="XNX"&code!="PNX"& code!="PXX"), aes(x =year2, y=change, color=group))+
  geom_line(aes(y=distance))+
  geom_ribbon( aes(ymin = plo, ymax =  phi, fill= group), alpha = .25, colour=NA) +
  geom_point(aes(x =year2, y=change, color=group), alpha=0.75) + theme_classic()+  
  #geom_jitter(aes(x =year2, y=change, color=group)) + theme_classic()+  
  ylab("Cover change from pre-treatment")+ xlab("year")+ theme_bw()+
    facet_wrap(~ code) +theme(plot.margin=margin(1,0,0,1, 'cm'))+ 
  scale_color_manual(values=color2)+ scale_fill_manual(values=color2)



#Fit GAMM----
library(ggplot2)
library(dplyr)
library(mgcv)
library(tidymv)
#with interactions
#http://r.qcbs.ca/workshop08/book-en/gam-with-interaction-terms.html
group_change<-mutate(group_change, group_code=interaction(group,code))
group_change$year2<-as.factor(group_change$year2)

#linear term only-gamm version of delta_abundx
gammtest0<-mgcv::gam(change ~ 0+  group:years:code + s(year2, bs='re'), 
                     data = group_change, method="REML")
summary(gammtest0)#r-squared 0.47 (same as above)
MuMIn::r.squaredGLMM(delta_abundx)
summary(delta_abundx)

#smooth term only 
gammtest<-mgcv::gam(change ~0+ s(0+ years, by = group_code) + s(year2, bs='re'), 
                    data = group_change, method="REML")
summary(gammtest)#Table S4

AIC(gammtest0, gammtest, delta_abundx) #linear better 

#plot fit lines 
#https://cran.r-project.org/web/packages/tidymv/vignettes/plot-smooths.html
plot_smooths( 
  model = gammtest,
  series = years,
  comparison = group,
  facet_terms = code,
  split = list(group_code = c("group", "code"))
) +
  theme(legend.position = "top")+ theme_classic() + geom_hline(yintercept=0, lty=2, col="black")

