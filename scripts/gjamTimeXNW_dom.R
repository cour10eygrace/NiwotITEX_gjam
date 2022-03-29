rm(list=ls()) #clear the console 
#pull in gjamTime inputs spp counts, enviro data 
source('scripts/gjamTime_setup.R')

#run gjamTime by treatment 
#XNW----
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

#subset data for XNW plots
plots<-subset(sppcomp, code=="XNW")  
xdata<-filter(xdata, plot %in% plots$plot)
ydata <- filter(spp_abundw,plot %in% plots$plot)%>%
  select(-plotyear, -N, -snow, -temp, -plot, -block, -year) 
ydata$year <- NULL
ydata$plot <- NULL
ydata <- as.matrix(ydata)

#make effort (edata) based on tot hits per plot year
#plothits<-select(spp_abund%>%ungroup(), veg_hits, plotyear)%>%distinct(.)
plothits<-separate(plothits, col = plotyear,into = "plot", sep = "_", remove=F)#ignore warning 
plothits_XNW<-filter(plothits, plot %in% plots$plot)
vals=plothits_XNW$veg_hits
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
xdata<-mutate(xdata, plot=case_when(groups==1~5, 
                                    groups==2~7, 
                                    groups==3~17, 
                                    groups==4~20, 
                                    groups==5~35, 
                                    groups==6~47))

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

#set as positive for two values estimated weakly negative- Mod, Dom & Dom, Subdom (Table S2) 
#tmp$alphaPrior$hi[3,1]<-1
#tmp$alphaPrior$hi[1,2]<-1
#tmp$alphaPrior$lo[3,1]<-0
#tmp$alphaPrior$lo[1,2]<-0

timeList <- mergeList(tlist, tmp)

# run model
modelList <- list(
  typeNames = "DA", ng = 10000, burnin =2000,
  timeList = timeList, effort = effort
)

modDAtimeXNW<- gjam(formula=timeList$formulaRho, xdata = xdata, ydata = ydata, modelList = modelList)

# save output
save(modDAtimeXNW, file = "outputs/modDAtime_XNWoutput_dom.RData")
#load(file = "outputs/modDAtime_XNWoutput_dom.RData")

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
                 outFolder = 'plots/modDAtime_XNWplots_dom')
gjamPlot(modDAtimeXNW, plotPars)

#posterior_vs_prior(modDAtime)#how to plot this???

#spp alphas
alphaX<-modDAtimeXNW$parameters$alphaMu
colnames(alphaX)<-colnames(ydata)
row.names(alphaX)<-colnames(alphaX)  

pdf(file="plots/modDAtime_XNWplots_dom/alpha_plot.pdf")
corrplot(alphaX ,method = "color", tl.cex = 0.8, tl.col="black", addCoef.col = "black",
         number.cex = 0.75, diag =T, main="alphas", is.corr = FALSE, 
         mar = c(2, 2, 2, 2), cl.lim = c(-1, 0))
dev.off()

corr<-modDAtimeXNW$parameters$corMu
colnames(corr)<-colnames(ydata)
row.names(corr)<-colnames(corr)  

pdf(file="plots/modDAtime_XNWplots_dom/corr_plot.pdf")
corrplot(corr, method = "color", tl.cex = 0.8, tl.col="black", addCoef.col = "black",
         number.cex = 0.75, diag = F, main="correlations" ,
         mar = c(2, 2, 2, 2))  
dev.off()


#calculate equillibrium abundance 
wstarXNW <- .wrapperEquilAbund(output =   modDAtimeXNW, covars = c('depthcm', 'avgT', 'Ndep'), BYGROUP = F,
                               nsim = 100, ngrid=10, 
                               verbose = T)
save(wstarXNW, file = "outputs/wstar_XNWoutput_dom.RData")
#load(file = "outputs/wstar_XNWoutput_dom.RData")

#plot 
outFolder="plots/modDAtime_XNWplots_dom"
wstar=wstarXNW
source("scripts/ploteqabund.R")
