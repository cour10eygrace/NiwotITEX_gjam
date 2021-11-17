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
#rare<-group_by(sppcomp, spp)%>%count()
#rare<-mutate(rare, plotyears_perc=n/720)
#top 8 spp are in >85% of plot years, remaining spp are in less than half (max=47%)

#look at total abundance in control plots over time 
#calculate average hits per plot 
rarectl<-subset(sppcomp, code=="XXX")%>%group_by(spp)%>%
  summarise(sum_hits=sum(hits))%>%mutate(avg_hits=sum_hits/90)%>%filter(sum_hits>20)

#dominance categories 
#removed anything with less than 20 total hits or 0.25 average hits in control plots across all years 
#rare species 50-100 hits  (0-1 average)
#mod 100-400 (1-5 avg)  hits, subdom 400-1600 hits (5-20 avg), dom 1600+ hits (20+ avg)
#AVERAGE PER PLOT 

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
#spp_abundw<-relocate(spp_abundw, CASOCC, .after = TRIPAR)
#spp_abundw<-relocate(spp_abundw, STELON, .after = TRIPAR)
#spp_abundw<-relocate(spp_abundw, STELON, .after = TRIPAR)
#spp_abundw<-rowwise(spp_abundw)%>%mutate(RARE=sum(c_across(STELON:TAROFF)))
spp_abundw<-select(spp_abundw, DESCAE, ARTSCO, GEUROS, CARSCO,GENALG, BISBIS, CALLEP, TRIPAR,
                   MERLAN, CASOCC, BISVIV, CHIJAM, ERISIM, FESBRA, LEWPYG, LLOSER, LUZSPI, POTDIV, RHOINT, STELON)
#spp_abundw<-relocate(spp_abundw, GEUROS, .after = ARTSCO)
#spp_abundw<-relocate(spp_abundw, CALLEP, .after = TRIPAR)
#spp_abundw<-relocate(spp_abundw, BISBIS, .after = TRIPAR)
spp_abundw<-rowwise(spp_abundw)%>%mutate(SUBDOM=sum(c_across(ARTSCO:CARSCO)))
spp_abundw<-rowwise(spp_abundw)%>%mutate(MODERATE=sum(c_across(GENALG:CALLEP)))
#spp_abundw<-rowwise(spp_abundw)%>%mutate(RARE=sum(c_across(MERLAN:BISVIV))) #50 hits
spp_abundw<-rowwise(spp_abundw)%>%mutate(RARE=sum(c_across(MERLAN:STELON))) #20 hits

spp_abundw<-select(spp_abundw, DESCAE, SUBDOM, MODERATE, RARE, year, plot)%>%rename(DOM=DESCAE)

# add treatment codes back in
spp_abundw <- left_join(spp_abundw, select(sppcomp, plot,block, snow, N, temp)) %>%
  distinct(.) %>%
  unite(plotyear, plot, year, remove = F)


#Gjam set up----
#combine spp data with enviro data
xdata <- as.data.frame(select(spp_abundw, plot, block, year, snow, N, temp))
#xdata$block<-as.character(xdata$block)
xdata_all<-full_join(xdata, enviro)

