# load packages
library(gjam)
library(devtools)
library(dplyr)
library(repmis)
library(tidyr)
library(corrplot)
library(ggplot2)
library(lme4)
library(lmerTest)
#gjam time supplemental functions
d <- "https://github.com/jimclarkatduke/gjam/blob/master/gjamTimeFunctions.R?raw=True"
source_url(d)
#load environmental Rdata 
load(file="data/Enviro.Rdata") #already mean centered 

#NWT species counts----
#all years-QA/QC JGS 12/18/20
sppcomp <- read.csv("data/NWT_ITEX_SpComp_data_L1.csv")
sppcomp<-filter(sppcomp, !is.na(year))#remove spaces 
sppcomp<-rename(sppcomp, spp=JGS_code)%>%unite(., plotyear, plot, year, remove=F)

#make full enviro matrix for filling NAs later 
plotyears<-select(sppcomp, year, plot, snow, N, temp, block)%>%distinct(.)
enviro$block<-as.numeric(enviro$block)
enviro<-rename(enviro, snow=snow_trt)

enviro_all<-left_join(enviro, plotyears)%>%
  distinct(.) #check only 720 

# calculate species hits and relative abundance within each plot x year
spp_abund<-sppcomp %>%group_by(year, plot, spp) %>% 
  summarise(spp_hits=sum(hits))%>%
  group_by(., year, plot, .drop = FALSE) %>%
  mutate(veg_hits=sum(spp_hits))%>%
  mutate(rel_abund=(spp_hits/veg_hits)*100)%>%
  unite(., plotyear, plot, year, remove=F)#%>%

#calculate total plot hits before removing non-vascular  
plothits<-select(spp_abund%>%ungroup(), veg_hits, plotyear, year)%>%distinct(.)

#remove non-vascular plants and Salix glauca transplants 
sppcomp <- subset(sppcomp, spp != "bare" & spp != "litter" & spp != "moss" & spp != "lichen" &
                    spp != "rock" & spp != "FORB_SP1" & spp != "SALGLA")

#look at distribution of spp coverage over time 
rare<-group_by(sppcomp, spp)%>%count()
rare<-mutate(rare, plotyears_perc=n/720)
#top 8 spp are in >85% of plot years, remaining spp are in less than half (max=47%)

#Munge spp comp data 
# calculate species hits and relative abundance within each plot x year
spp_abund<-sppcomp %>%group_by(year, plot, spp) %>% 
  summarise(spp_hits=sum(hits))%>%
  group_by(., year, plot, .drop = FALSE) %>%
  mutate(veg_hits=sum(spp_hits))%>%
  mutate(rel_abund=(spp_hits/veg_hits)*100)%>%
  unite(., plotyear, plot, year, remove=F)#%>%

# make spp_hits wide and fill in zeroes-censor with total hits in effort 
spp_abundw <- select(spp_abund, -veg_hits, -rel_abund)%>%
  pivot_wider(names_from = spp, values_from = spp_hits) %>%
  mutate_all(funs(replace(., is.na(.), 0))) # replace NAs with zeroes

# group into 4 dominance categories Desc=Dom, Artsco, Geum, Carsco=SubDom, Callep, Tripar, Bisbis, 
# Genalg=moderate, rest=rare (based on hits in XXX plots-check for values )
spp_abundw<-relocate(spp_abundw, CASOCC, .after = TRIPAR)
spp_abundw<-relocate(spp_abundw, STELON, .after = TRIPAR)
spp_abundw<-relocate(spp_abundw, STELON, .after = TRIPAR)
spp_abundw<-rowwise(spp_abundw)%>%mutate(RARE=sum(c_across(STELON:TAROFF)))
spp_abundw<-select(spp_abundw, ARTSCO, BISBIS, CALLEP, CARSCO, DESCAE,
                   GENALG,GEUROS, TRIPAR, RARE)
spp_abundw<-relocate(spp_abundw, GEUROS, .after = ARTSCO)
spp_abundw<-relocate(spp_abundw, CALLEP, .after = TRIPAR)
spp_abundw<-relocate(spp_abundw, BISBIS, .after = TRIPAR)
spp_abundw<-rowwise(spp_abundw)%>%mutate(SUBDOM=sum(c_across(ARTSCO:CARSCO)))
spp_abundw<-rowwise(spp_abundw)%>%mutate(MODERATE=sum(c_across(GENALG:CALLEP)))
spp_abundw<-select(spp_abundw, DESCAE, SUBDOM, MODERATE, RARE, year, plot)%>%rename(DOM=DESCAE)

#looks good 
hist(spp_abundw$DOM)
hist(spp_abundw$SUBDOM)
hist(spp_abundw$MODERATE)
hist(spp_abundw$RARE)

# doesn't like underscores in spp names
names(spp_abundw)

# add treatment codes back in
spp_abundw <- left_join(spp_abundw, select(sppcomp, plot,block, snow, N, temp)) %>%
  distinct(.) %>%
  unite(plotyear, plot, year, remove = F)


#Gjam set up----
#combine spp data with enviro data
xdata <- as.data.frame(select(spp_abundw, plot, block, year, snow, N, temp))
#xdata$block<-as.character(xdata$block)
xdata_all<-full_join(xdata, enviro)


#PNW----
#cumulative N 
xdata<-select(xdata_all, plot, year, depth_cm, NdepCum,avgT)%>%
  rename(Ndep=NdepCum)
#not cumulative N 
#xdata<-select(xdata_all, plot, year, depth_cm, Ndep,avgT)

#pull out ctl values for year 1 
xdata_2006<-filter(xdata, year==2006&plot==4)

#make year start at 1 and replace with XXX 
xdata<- mutate(xdata, year=year-2005)%>%
  mutate(depth_cm=case_when(year==1~xdata_2006$depth_cm, TRUE ~ as.numeric(depth_cm)), 
         Ndep =case_when(year==1~xdata_2006$Ndep, TRUE ~ as.numeric(Ndep)), 
         avgT=case_when(year==1~xdata_2006$avgT, TRUE ~ as.numeric(avgT)))

#subset data for PNW plots
plots<-subset(sppcomp, code=="PNW")  
xdata<-filter(xdata, plot %in% plots$plot)
ydata <- filter(spp_abundw,plot %in% plots$plot)%>%
  select(-plotyear, -N, -snow, -temp, -plot, -block, -year) 
ydata$year <- NULL
ydata$plot <- NULL
ydata <- as.matrix(ydata)

#make effort (edata) based on tot hits per plot year
#plothits<-select(spp_abund%>%ungroup(), veg_hits, plotyear)%>%distinct(.)
plothits<-separate(plothits, col = plotyear,into = "plot", sep = "_", remove=F)#ignore warning 
plothits_PNW<-filter(plothits, plot %in% plots$plot)
vals=plothits_PNW$veg_hits
edata <- matrix(vals, nrow = nrow(xdata), ncol = ncol(ydata))

groups<-xdata$plot
times<-as.integer(xdata$year)

# fill missing times/effort
#uses mean of all years for time 0 (2005) for each plot 
timeCol <- "year"
groupCol <- "plot"
groupVars <- c("groups")
tmp <- gjamFillMissingTimes(xdata, ydata, edata, groupCol, timeCol,
                            FILLMEANS = T, groupVars = groupVars,
                            typeNames = "DA", missingEffort = 1
)
# save model params
xdata <- tmp$xdata
ydata <- tmp$ydata
edata <- tmp$edata
tlist <- tmp$timeList
snames <- colnames(ydata)
colnames(edata)<-snames
effort <- list(columns = 1:ncol(ydata), values = edata)

#fill missing info in xdata for year 0 with ctl 2006 
xdata<-mutate(xdata, depth_cm=case_when(is.na(depth_cm)&year<1~xdata_2006$depth_cm, TRUE ~ as.numeric(depth_cm)), 
              Ndep =case_when(is.na(Ndep)&year<1~xdata_2006$Ndep, TRUE ~ as.numeric(Ndep)), 
              avgT=case_when(is.na(avgT)&year<1~xdata_2006$avgT, TRUE ~ as.numeric(avgT))) #, 
#fill missing info in xdata for other years with actual values  
xdata_fill<-mutate(enviro_all, year=year-2005)%>%rename(depth_cm0=depth_cm, Ndep0=Ndep, avgT0=avgT)%>%
  select(plot, year, depth_cm0, Ndep0, avgT0)
xdata<-mutate(xdata, plot=case_when(groups==1~9, 
                                    groups==2~13, 
                                    groups==3~27, 
                                    groups==4~30, 
                                    groups==5~42, 
                                    groups==6~48))

xdata<-left_join(xdata, xdata_fill)
xdata<-mutate(xdata, depth_cm=ifelse(is.na(depth_cm), depth_cm0, depth_cm), 
              Ndep=ifelse(is.na(Ndep), Ndep0, Ndep), 
              avgT=ifelse(is.na(avgT), avgT0, avgT))%>%select(-contains("0")) 
xdata$plot<-NULL
xdata$year<-NULL
#does not like underscores 
xdata<-rename(xdata, depthcm=depth_cm)

#subset xdata for only Ndep to try to fix singular matrix issue  
#xdata<-select(xdata, -depth_cm, -avgT, -plot, -year)

alphaSign <- matrix(-1, ncol(ydata), ncol(ydata)) # set as competitors
colnames(alphaSign) <- rownames(alphaSign) <- colnames(ydata)

# rhoPrior is a list indicating lo and hi values for the growth rate, which is change per time increment.
# In this example, growth rate rho only includes an intercept, because I included no predictors (Q = 0).
# The density-independent growth rate is given a wide prior values of ±30% per time increment:
rhoPrior  <- list(lo = list(intercept = -1, depthcm = -0.5, 
                            Ndep = -0.5, avgT= -0.5), 
                  hi = list(intercept = 1, depthcm = 0.5, 
                            Ndep = 0.5, avgT= 0.5)) 

priorList <- list(alphaSign = alphaSign,
                  formulaRho = as.formula(~depthcm+Ndep+avgT),
                  rhoPrior = rhoPrior
)

tmp <- gjamTimePrior(xdata, ydata, edata, priorList)  

#reset hi value to positive of negative priors (heterospecific) and zero for conspecific

#re set to -1 and 0 for lo and hi 
lo<- matrix(-1, ncol(ydata), ncol(ydata)) 
tmp$alphaPrior$lo<-lo
hi<- matrix(0, ncol(ydata), ncol(ydata)) 
#diag(hi)<- 0
tmp$alphaPrior$hi<-hi

timeList <- mergeList(tlist, tmp)

# run model
modelList <- list(
  typeNames = "DA", ng = 6000, burnin = 1000,
  timeList = timeList, effort = effort
)

modDAtimePNW<- gjam(formula=timeList$formulaRho, xdata = xdata, ydata = ydata, modelList = modelList)

# save output
save(modDAtimePNW,file = "outputs/modDAtime_PNWoutput_dom.RData")
#load(file = "outputs/modDAtime_PNWoutput_dom.RData")

# plot output
specColor <- c(
  "#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7",
  "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD",
  "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D",
  "#8A7C64", "#599861", "#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7",
  "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD",
  "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D",
  "#8A7C64", "#599861"
)

#plotPars1 <- list(specColor=specColor, PLOTALLY=T, GRIDPLOTS=T, CLUSTERPLOTS=T, SAVEPLOTS = F)
plotPars <- list(specColor=specColor, PLOTALLY=T, GRIDPLOTS=T, CLUSTERPLOTS=T, SAVEPLOTS = T, 
                 outFolder = 'plots/modDAtime_PNWplots_dom')
gjamPlot(modDAtimePNW, plotPars)


#posterior_vs_prior(modDAtime)#how to plot this???

#spp alphas
alphaX<-modDAtimePNW$parameters$alphaMu
colnames(alphaX)<-colnames(ydata)
row.names(alphaX)<-colnames(alphaX)  

pdf(file="plots/modDAtime_PNWplots_dom/alpha_plot.pdf")
corrplot(alphaX ,method = "color", tl.cex = 0.8, tl.col="black", addCoef.col = "black",
         number.cex = 0.75, diag =T, main="alphas", is.corr = FALSE, 
         mar = c(2, 2, 2, 2), cl.lim = c(-1, 0))
dev.off()

corr<-modDAtimePNW$parameters$corMu
colnames(corr)<-colnames(ydata)
row.names(corr)<-colnames(corr)  

pdf(file="plots/modDAtime_PNWplots_dom/corr_plot.pdf")
corrplot(corr, method = "color", tl.cex = 0.8, tl.col="black", addCoef.col = "black",
         number.cex = 0.75, diag = F, main="correlations" ,
         mar = c(2, 2, 2, 2))  
dev.off()

#calculate equillibrium abundance 
wstarPNW <- .wrapperEquilAbund(output =   modDAtimePNW, covars = c('depthcm', 'avgT', 'Ndep'), BYGROUP = F,
                               nsim = 100, ngrid = 10, 
                               verbose = T)
save(wstarPNW, file = "outputs/wstar_PNWoutput_dom.RData")
#load(file = "outputs/wstar_PNWoutput_dom.RData")

#plot
outFolder="plots/modDAtime_PNWplots_dom"
wstar=wstarPNW
source("scripts/ploteqabund.R") 
