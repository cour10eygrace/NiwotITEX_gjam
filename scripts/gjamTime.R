# load packages
library(gjam)
library(devtools)
library(dplyr)
library(tidyr)
library(repmis)
library(corrplot)
library(ggplot2)
library(lme4)
library(lmerTest)
#gjam time supplemental functions
d <- "https://github.com/jimclarkatduke/gjam/blob/master/gjamTimeFunctions.R?raw=True"
source_url(d)
#load environmental Rdata 
load(file="data/Enviro.Rdata")

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

# select top 8 spp - sum the remaining coverage as one 'spp' total=9 spp
spp_abundw<-relocate(spp_abundw, CASOCC, .after = TRIPAR)
spp_abundw<-relocate(spp_abundw, STELON, .after = TRIPAR)
spp_abundw<-rowwise(spp_abundw)%>%mutate(RARESPP=sum(c_across(STELON:TAROFF)))
spp_abundw<-select(spp_abundw, ARTSCO, BISBIS, CALLEP, CARSCO, DESCAE,
                   GENALG,GEUROS, TRIPAR, RARESPP)
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


#all treat all years ----

# make datasets for gjam
xdata<-select(xdata_all, plot, year, depth_cm, Ndep,avgT)
#make year start at 1 
#set all treatments in year 1 to XXX values 
xdata_2006<-filter(xdata, year==2006&plot==4)

xdata<- mutate(xdata, year=year-2005)%>%
  mutate(depth_cm=case_when(year==1~xdata_2006$depth_cm, TRUE ~ as.numeric(depth_cm)), 
         Ndep =case_when(year==1~xdata_2006$Ndep, TRUE ~ as.numeric(Ndep)), 
         avgT=case_when(year==1~xdata_2006$avgT, TRUE ~ as.numeric(avgT)))

#subset data for NOT XXX plots
trtplots<-subset(sppcomp, code!="XXX")  
xdata<-filter(xdata, plot %in% trtplots$plot)
ydata <- filter(spp_abundw,plot %in% trtplots$plot)%>%
  select(-plotyear, -N, -snow, -temp, -plot, -block, -year) 
ydata$year <- NULL
ydata$plot <- NULL
ydata <- as.matrix(ydata)

#make effort (edata) based on tot hits per plot year
plothits<-select(spp_abund%>%ungroup(), veg_hits, plotyear)%>%distinct(.)
plothits<-separate(plothits, col = plotyear,into = c("plot","year"), sep = "_", remove=F)#ignore warning 
plothits_trt<-filter(plothits, plot %in% trtplots$plot)
vals=plothits_trt$veg_hits
edata <- matrix(vals, nrow = nrow(xdata), ncol = ncol(ydata))

#set effort to 100 hits
#edata <- matrix(100, nrow = nrow(xdata), ncol = ncol(ydata))

groups<-xdata$plot
times<-as.integer(xdata$year)
#xdata<-rename(xdata, groups=plot, times=year)

#set intercept in xdata 
#xdata$int<-1
#xdata<-select(xdata, plot, year, int, depth_cm, Ndep, avgT)

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
xdata$plot<-NULL
xdata$year<-NULL
xdata<-rename(xdata, depthcm=depth_cm)

# set priors
alphaSign <- matrix(-1, ncol(ydata), ncol(ydata)) # set as competitors
colnames(alphaSign) <- rownames(alphaSign) <- colnames(ydata)

# rhoPrior is a list indicating lo and hi values for the growth rate, which is change per time increment.
# In this example, growth rate rho only includes an intercept, because I included no predictors (Q = 0).
# The density-independent growth rate is given a wide prior values of ±30% per time increment:
# The density-independent growth rate is given a wide prior values of ±30% per time increment:
rhoPrior  <- list(lo = list(intercept = -0.5, depthcm = -0.5, 
                            Ndep = -0.5, avgT= -0.5), 
                  hi = list(intercept = 0.5, depthcm = 0.5, 
                            Ndep = 0.5, avgT= 0.5)) 

#rhoPrior <- list(lo = list(intercept = -0.3), hi = list(intercept = 0.3))
#rhoPrior <- list(
#  lo = list( intercept=-Inf, snow = -Inf, N = -Inf, temp= -Inf),
#  hi = list( intercept=Inf, snow = Inf, N=Inf, temp=Inf)
#)  

priorList <- list(alphaSign = alphaSign,
                  formulaRho = as.formula(~depthcm+Ndep+avgT),
                  rhoPrior = rhoPrior
)

tmp <- gjamTimePrior(xdata, ydata, edata, priorList)  

#re set to -1 and 0 for lo and hi-max estimates  
lo<- matrix(-1, ncol(ydata), ncol(ydata)) 
tmp$alphaPrior$lo<-lo
hi<- matrix(0, ncol(ydata), ncol(ydata)) 
#diag(hi)<- 0
tmp$alphaPrior$hi<-hi
timeList <- mergeList(tlist, tmp)

# run model
modelList <- list(
  typeNames = "DA", ng = 1000, burnin = 300,
  timeList = timeList, effort = effort
)
modDAtime_trt <- gjam(formula=timeList$formulaRho, xdata = xdata, ydata = ydata, modelList = modelList)

# save output
save(modDAtime_trt, file = "outputs/modDAtime_trtoutput.RData")
#load(file = "outputs/modDAtime_trtoutput.RData")

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
                 outFolder = 'plots/modDAtime_trtplots')
gjamPlot(modDAtime_trt, plotPars)

#spp alphas
alphaX<-modDAtime_trt$parameters$alphaMu
colnames(alphaX)<-colnames(ydata)
row.names(alphaX)<-colnames(alphaX)  

pdf(file="plots/modDAtime_trtplots/alpha_plot.pdf")
corrplot(alphaX ,method = "color", tl.cex = 0.8, tl.col="black", addCoef.col = "black",
         number.cex = 0.75, diag =T, main="alphas",  is.corr = F,
         mar = c(2, 2, 2, 2), cl.lim = c(-1, 0))
dev.off()

corr<-modDAtime_trt$parameters$corMu
colnames(corr)<-colnames(ydata)
row.names(corr)<-colnames(corr)  

pdf(file="plots/modDAtime_trtplots/corr_plot.pdf")
corrplot(corr, method = "color", tl.cex = 0.8, tl.col="black", addCoef.col = "black",
         number.cex = 0.75, diag = F, main="correlations" ,
         mar = c(2, 2, 2, 2))  
dev.off()

#early trt----
spp_abundw_early<-subset(spp_abundw, year<2011)

# make datasets for gjam
xdata<-filter(xdata_all, year<2011)%>%select(plot, year, depth_cm, Ndep,avgT)
#make year start at 1 
#set all treatments in year 1 to XXX values 
xdata_2006<-filter(xdata, year==2006&plot==4)

xdata<- mutate(xdata, year=year-2005)%>%
  mutate(depth_cm=case_when(year==1~xdata_2006$depth_cm, TRUE ~ as.numeric(depth_cm)), 
         Ndep =case_when(year==1~xdata_2006$Ndep, TRUE ~ as.numeric(Ndep)), 
         avgT=case_when(year==1~xdata_2006$avgT, TRUE ~ as.numeric(avgT)))

#subset data for NOT XXX plots
trtplots<-subset(sppcomp, code!="XXX"|code)  
xdata<-filter(xdata, plot %in% trtplots$plot)
ydata <- filter(spp_abundw_early,plot %in% trtplots$plot)%>%
  select(-plotyear, -N, -snow, -temp, -plot, -block, -year) 
ydata$year <- NULL
ydata$plot <- NULL
ydata <- as.matrix(ydata)

#make effort (edata) based on tot hits per plot year
plothits<-select(spp_abund%>%ungroup(), veg_hits, plotyear)%>%distinct(.)
plothits<-separate(plothits, col = plotyear,into = c("plot","year"), sep = "_", remove=F)#ignore warning 
plothits_trt<-filter(plothits, plot %in% trtplots$plot)
plothits_earlytrt<-subset(plothits_trt, year <2011)
vals=plothits_earlytrt$veg_hits
edata <- matrix(vals, nrow = nrow(xdata), ncol = ncol(ydata))

#set effort to 100 hits
#edata <- matrix(100, nrow = nrow(xdata), ncol = ncol(ydata))

groups<-xdata$plot
times<-as.integer(xdata$year)
#xdata<-rename(xdata, groups=plot, times=year)

#set intercept in xdata 
#xdata$int<-1
#xdata<-select(xdata, plot, year, int, depth_cm, Ndep, avgT)

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
xdata$plot<-NULL
xdata$year<-NULL
xdata<-rename(xdata, depthcm=depth_cm)

# set priors
alphaSign <- matrix(-1, ncol(ydata), ncol(ydata)) # set as competitors
colnames(alphaSign) <- rownames(alphaSign) <- colnames(ydata)

# rhoPrior is a list indicating lo and hi values for the growth rate, which is change per time increment.
# In this example, growth rate rho only includes an intercept, because I included no predictors (Q = 0).
# The density-independent growth rate is given a wide prior values of ±30% per time increment:
# The density-independent growth rate is given a wide prior values of ±30% per time increment:
rhoPrior  <- list(lo = list(intercept = -0.5, depthcm = -0.5, 
                            Ndep = -0.5, avgT= -0.5), 
                  hi = list(intercept = 0.5, depthcm = 0.5, 
                            Ndep = 0.5, avgT= 0.5)) 

#rhoPrior <- list(lo = list(intercept = -0.3), hi = list(intercept = 0.3))
#rhoPrior <- list(
#  lo = list( intercept=-Inf, snow = -Inf, N = -Inf, temp= -Inf),
#  hi = list( intercept=Inf, snow = Inf, N=Inf, temp=Inf)
#)  

priorList <- list(alphaSign = alphaSign,
                  formulaRho = as.formula(~depthcm+Ndep+avgT),
                  rhoPrior = rhoPrior
)

tmp <- gjamTimePrior(xdata, ydata, edata, priorList)  

#re set to -1 and 0 for lo and hi-max estimates  
lo<- matrix(-1, ncol(ydata), ncol(ydata)) 
tmp$alphaPrior$lo<-lo
hi<- matrix(0, ncol(ydata), ncol(ydata)) 
#diag(hi)<- 0
tmp$alphaPrior$hi<-hi
timeList <- mergeList(tlist, tmp)

# run model
modelList <- list(
  typeNames = "DA", ng = 1000, burnin = 300,
  timeList = timeList, effort = effort
)
modDAtime_earlytrt <- gjam(formula=timeList$formulaRho, xdata = xdata, ydata = ydata, modelList = modelList)

# save output
save(modDAtime_earlytrt, file = "outputs/modDAtime_earlytrtoutput.RData")
#load(file = "outputs/modDAtime_earlytrtoutput.RData")

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
                 outFolder = 'plots/modDAtime_earlytrtplots')
gjamPlot(modDAtime_earlytrt, plotPars)

#spp alphas
alphaX<-modDAtime_earlytrt$parameters$alphaMu
colnames(alphaX)<-colnames(ydata)
row.names(alphaX)<-colnames(alphaX)  

pdf(file="plots/modDAtime_earlytrtplots/alpha_plot.pdf")
corrplot(alphaX ,method = "color", tl.cex = 0.8, tl.col="black", addCoef.col = "black",
         number.cex = 0.75, diag =T, main="alphas",  is.corr = F,
         mar = c(2, 2, 2, 2), cl.lim = c(-1, 0))
dev.off()

corr<-modDAtime_earlytrt$parameters$corMu
colnames(corr)<-colnames(ydata)
row.names(corr)<-colnames(corr)  

pdf(file="plots/modDAtime_earlytrtplots/corr_plot.pdf")
corrplot(corr, method = "color", tl.cex = 0.8, tl.col="black", addCoef.col = "black",
         number.cex = 0.75, diag = F, main="correlations" ,
         mar = c(2, 2, 2, 2))  
dev.off()

#middle trt----
spp_abundw_mid<-subset(spp_abundw, year>2010&year<2016)

# make data for gjam
xdata<-filter(xdata_all, year>2010&year<2016)%>%select(plot, year, depth_cm, Ndep,avgT)
##make year start at 1 
xdata<- mutate(xdata, year=year-2010)
#subset data for NOT XXX plots
#trtplots<-subset(sppcomp, code!="XXX")  
xdata<-filter(xdata, plot %in% trtplots$plot)

ydata <- filter(spp_abundw_mid,plot %in% trtplots$plot)%>%
  select(-plotyear, -N, -snow, -temp, -plot, -block, -year) 
ydata$year <- NULL
ydata$plot <- NULL
ydata <- as.matrix(ydata)

#make effort (edata) based on tot hits per plot year
#plothits<-select(spp_abund%>%ungroup(), veg_hits, plotyear, year)%>%distinct(.)
#plothits_trt<-filter(plothits, plot %in% trtplots$plot)
plothits_trtmid<-subset(plothits_trt, year>2010&year<2016)
vals=plothits_trtmid$veg_hits
edata <- matrix(vals, nrow = nrow(xdata), ncol = ncol(ydata))
#edata <- matrix(100, nrow = nrow(xdata), ncol = ncol(ydata))#if want to use all as 100 hits  

groups<-xdata$plot
times<-as.integer(xdata$year)
#set intercept in xdata 
#xdata$int<-1
#xdata<-select(xdata, plot, year, int, snow, N, temp)

# fill missing times/effort
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

#fill missing info in xdata for year 0 with year prior (2010)
#fill remaining missing info with actual values 
xdata_2010<-filter(xdata_all, year==2010)%>%mutate(year=year-2010)%>%rename(depth_cm0=depth_cm, Ndep0=Ndep, avgT0=avgT)
xdata_fill<-mutate(enviro_all, year=year-2005)%>%rename(depth_cm0=depth_cm, Ndep0=Ndep, avgT0=avgT)#%>%
#select(plot, year, depth_cm0, Ndep0, avgT0)
xdata_fill<-rbind(xdata_fill, xdata_2010)%>%select(-snow, -N, -temp, -block)
xdata<-mutate(xdata, plot=case_when(groups==1~1, groups==2~2, groups==3~3, groups==4~5, groups==5~7,  
                                    groups==6~8,groups==7~9,groups==8~10,groups==9~11,groups==10~12,groups==11~13,groups==12~14,groups==13~15, 
                                    groups==14~16,groups==15~17,groups==16~18,groups==17~19,groups==18~20,groups==19~21,groups==20~23,groups==21~25,
                                    groups==22~26, groups==23~27,groups==24~28,groups==25~29,groups==26~30,groups==27~31,groups==28~32,groups==29~34,
                                    groups==30~35,groups==31~36,groups==32~37,groups==33~38,groups==34~40,groups==35~41,groups==36~42,groups==37~43,
                                    groups==38~44,groups==39~45,groups==40~46,groups==41~47,groups==42~48))
xdata<-left_join(xdata, xdata_fill)
xdata<-mutate(xdata, depth_cm=ifelse(is.na(depth_cm), depth_cm0, depth_cm), 
              Ndep=ifelse(is.na(Ndep), Ndep0, Ndep), 
              avgT=ifelse(is.na(avgT), avgT0, avgT))%>%select(-contains("0")) 

xdata$plot<-NULL
xdata$year<-NULL
xdata<-rename(xdata, depthcm=depth_cm)

# set priors
alphaSign <- matrix(-1, ncol(ydata), ncol(ydata)) # set as competitors
colnames(alphaSign) <- rownames(alphaSign) <- colnames(ydata)

# rhoPrior is a list indicating lo and hi values for the growth rate, which is change per time increment.
# In this example, growth rate rho only includes an intercept, because I included no predictors (Q = 0).
# The density-independent growth rate is given a wide prior values of ±30% per time increment:
# The density-independent growth rate is given a wide prior values of ±30% per time increment:
rhoPrior  <- list(lo = list(intercept = -0.5, depthcm = -0.5, 
                            Ndep = -0.5, avgT= -0.5), 
                  hi = list(intercept = 0.5, depthcm = 0.5, 
                            Ndep = 0.5, avgT= 0.5)) 

#rhoPrior <- list(lo = list(intercept = -0.3), hi = list(intercept = 0.3))
#rhoPrior <- list(
#  lo = list( intercept=-Inf, snow = -Inf, N = -Inf, temp= -Inf),
#  hi = list( intercept=Inf, snow = Inf, N=Inf, temp=Inf)
#)  

priorList <- list(alphaSign = alphaSign,
                  formulaRho = as.formula(~depthcm+Ndep+avgT),
                  rhoPrior = rhoPrior
)

tmp <- gjamTimePrior(xdata, ydata, edata, priorList)  

#re set to -1 and 0 for lo and hi-max estimates  
lo<- matrix(-1, ncol(ydata), ncol(ydata)) 
tmp$alphaPrior$lo<-lo
hi<- matrix(0, ncol(ydata), ncol(ydata)) 
#diag(hi)<- 0
tmp$alphaPrior$hi<-hi

timeList <- mergeList(tlist, tmp)

# run model
modelList <- list(
  typeNames = "DA", ng = 1000, burnin = 300,
  timeList = timeList, effort = effort
)
modDAtime_midtrt <- gjam(formula=as.formula(~depthcm+Ndep+avgT), xdata = xdata, ydata = ydata, modelList = modelList)

# save output
save(modDAtime_midtrt, file = "outputs/modDAtime_midtrtoutput.RData")
#load(file = "outputs/modDAtime_midtrtoutput.RData")

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
                 outFolder = 'plots/modDAtime_midtrtplots')
gjamPlot(modDAtime_midtrt, plotPars)

#spp alphas
alphaX<-modDAtime_midtrt$parameters$alphaMu
colnames(alphaX)<-colnames(ydata)
row.names(alphaX)<-colnames(alphaX)  

pdf(file="plots/modDAtime_midtrtplots/alpha_plot.pdf")
corrplot(alphaX ,method = "color", tl.cex = 0.8, tl.col="black", addCoef.col = "black",
         number.cex = 0.75, diag =T, main="alphas",  is.corr = F,
         mar = c(2, 2, 2, 2), cl.lim = c(-1, 0))
dev.off()

corr<-modDAtime_midtrt$parameters$corMu
colnames(corr)<-colnames(ydata)
row.names(corr)<-colnames(corr)  

pdf(file="plots/modDAtime_midtrtplots/corr_plot.pdf")
corrplot(corr, method = "color", tl.cex = 0.8, tl.col="black", addCoef.col = "black",
         number.cex = 0.75, diag = F, main="correlations" ,
         mar = c(2, 2, 2, 2))  
dev.off()

#late trt----
spp_abundw_late<-subset(spp_abundw, year>2015)

# make data for gjam
xdata<-filter(xdata_all, year>2015)%>%select(plot, year, depth_cm, Ndep,avgT)
xdata<- mutate(xdata, year=year-2015)

#subset data for NOT XXX plots
#trtplots<-subset(sppcomp, code!="XXX")  
xdata<-filter(xdata, plot %in% trtplots$plot)
ydata <- filter(spp_abundw_late,plot %in% trtplots$plot)%>%
  select(-plotyear, -N, -snow, -temp, -plot, -block, -year) 
ydata$year <- NULL
ydata$plot <- NULL
ydata <- as.matrix(ydata)

#make effort (edata) based on tot hits per plot year
#plothits<-select(spp_abund%>%ungroup(), veg_hits, plotyear, year)%>%distinct(.)
#plothits_trt<-filter(plothits, plot %in% trtplots$plot)
plothits_trtlate<-subset(plothits_trt, year>2015)
vals=plothits_trtlate$veg_hits
edata <- matrix(vals, nrow = nrow(xdata), ncol = ncol(ydata))
#edata <- matrix(100, nrow = nrow(xdata), ncol = ncol(ydata))#if want to use all as 100 hits  

groups<-xdata$plot
times<-as.integer(xdata$year)

# fill missing times/effort
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

#fill missing info in xdata for year 0 with year prior (2015)
#fill remaining missing info with actual values 
xdata_2015<-filter(xdata_all, year==2015)%>%mutate(year=year-2015)%>%rename(depth_cm0=depth_cm, Ndep0=Ndep, avgT0=avgT)
xdata_fill<-mutate(enviro_all, year=year-2005)%>%rename(depth_cm0=depth_cm, Ndep0=Ndep, avgT0=avgT)#%>%
# select(plot, year, depth_cm0, Ndep0, avgT0)
xdata_fill<-rbind(xdata_fill, xdata_2015)%>%select(-snow, -N, -temp, -block)
xdata<-mutate(xdata, plot=case_when(groups==1~1, groups==2~2, groups==3~3, groups==4~5, groups==5~7,  
                                    groups==6~8,groups==7~9,groups==8~10,groups==9~11,groups==10~12,groups==11~13,groups==12~14,groups==13~15, 
                                    groups==14~16,groups==15~17,groups==16~18,groups==17~19,groups==18~20,groups==19~21,groups==20~23,groups==21~25,
                                    groups==22~26, groups==23~27,groups==24~28,groups==25~29,groups==26~30,groups==27~31,groups==28~32,groups==29~34,
                                    groups==30~35,groups==31~36,groups==32~37,groups==33~38,groups==34~40,groups==35~41,groups==36~42,groups==37~43,
                                    groups==38~44,groups==39~45,groups==40~46,groups==41~47,groups==42~48))
xdata<-left_join(xdata, xdata_fill)
xdata<-mutate(xdata, depth_cm=ifelse(is.na(depth_cm), depth_cm0, depth_cm), 
              Ndep=ifelse(is.na(Ndep), Ndep0, Ndep), 
              avgT=ifelse(is.na(avgT), avgT0, avgT))%>%select(-contains("0")) 

xdata$plot<-NULL
xdata$year<-NULL
xdata<-rename(xdata, depthcm=depth_cm)

# set priors
alphaSign <- matrix(-1, ncol(ydata), ncol(ydata)) # set as competitors
colnames(alphaSign) <- rownames(alphaSign) <- colnames(ydata)


# rhoPrior is a list indicating lo and hi values for the growth rate, which is change per time increment.
# In this example, growth rate rho only includes an intercept, because I included no predictors (Q = 0).
# The density-independent growth rate is given a wide prior values of ±30% per time increment:
# The density-independent growth rate is given a wide prior values of ±30% per time increment:
rhoPrior  <- list(lo = list(intercept = -0.5, depthcm = -0.5, 
                            Ndep = -0.5, avgT= -0.5), 
                  hi = list(intercept = 0.5, depthcm = 0.5, 
                            Ndep = 0.5, avgT= 0.5)) 

#rhoPrior <- list(lo = list(intercept = -0.3), hi = list(intercept = 0.3))
#rhoPrior <- list(
#  lo = list( intercept=-Inf, snow = -Inf, N = -Inf, temp= -Inf),
#  hi = list( intercept=Inf, snow = Inf, N=Inf, temp=Inf)
#)  

priorList <- list(alphaSign = alphaSign,
                  formulaRho = as.formula(~depthcm+Ndep+avgT),
                  rhoPrior = rhoPrior
)

tmp <- gjamTimePrior(xdata, ydata, edata, priorList)  

#re set to -1 and 0 for lo and hi-max estimates  
lo<- matrix(-1, ncol(ydata), ncol(ydata)) 
tmp$alphaPrior$lo<-lo
hi<- matrix(0, ncol(ydata), ncol(ydata)) 
#diag(hi)<- 0
tmp$alphaPrior$hi<-hi

timeList <- mergeList(tlist, tmp)

# run model
modelList <- list(
  typeNames = "DA", ng = 1000, burnin = 300,
  timeList = timeList, effort = effort
)
modDAtime_latetrt <- gjam(formula=as.formula(~depthcm+Ndep+avgT), xdata = xdata, ydata = ydata, modelList = modelList)

# save output
save(modDAtime_latetrt, file = "outputs/modDAtime_latetrtoutput.RData")
#load(file = "outputs/modDAtime_latetrtoutput.RData")

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
                 outFolder = 'plots/modDAtime_latetrtplots')
gjamPlot(modDAtime_latetrt, plotPars)


#spp alphas
alphaX<-modDAtime_latetrt$parameters$alphaMu
colnames(alphaX)<-colnames(ydata)
row.names(alphaX)<-colnames(alphaX)  

pdf(file="plots/modDAtime_latetrtplots/alpha_plot.pdf")
corrplot(alphaX ,method = "color", tl.cex = 0.8, tl.col="black", addCoef.col = "black",
         number.cex = 0.75, diag =T, main="alphas",  is.corr = F,
         mar = c(2, 2, 2, 2), cl.lim = c(-1, 0)) 
dev.off()

corr<-modDAtime_latetrt$parameters$corMu
colnames(corr)<-colnames(ydata)
row.names(corr)<-colnames(corr)  

pdf(file="plots/modDAtime_latetrtplots/corr_plot.pdf")
corrplot(corr, method = "color", tl.cex = 0.8, tl.col="black", addCoef.col = "black",
         number.cex = 0.75, diag = F, main="correlations" ,
         mar = c(2, 2, 2, 2))  
dev.off()


