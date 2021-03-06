library(dplyr)
library(tidyr)
library(ggplot2)
#enviro data----
#only need to run the rest if changing/updating environmental Rdata 
#N->Nat'l atmospheric deposition program seasonal averages 
#http://nadp.slh.wisc.edu/data/sites/siteDetails.aspx?net=NTN&id=CO02

#air temp->NWT Saddle data loggers daily from EDI 
#https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-nwt.405.4
#ftp://niwotlter.colorado.edu/pub/raw_climate_data/ 

#snow->ITEX data from JGS, Saddle data from EDI 
#https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-nwt.31.17

#Nitrogen----
#Munge N data 
Ndep<-read.csv("data/raw_env/NTN-CO02-s.csv") 

#use Summer (Jun-Aug) Ndep values from NADP as baseline and add constants from Farrer 2014 GCB 
#Nitrogen was added as osmocote slow release fertilizer at a rate of 28 g N/m2/yr from 2006 to 2010 
#and reduced to 10 g N/m2/yr in 2011-2012. Due to incomplete release of the fertilizer pellets and high surface
#water flow at the beginning of the growing season, we estimate that the actual N made available in each plot
#was 14 and 5 g N/m2/yr, respectively. #convert from NADP kg/ha to g/m2

Ndep<-subset(Ndep,seas=="Summer"& year>2005)%>%select(year, seas, NH4, NO3)%>%mutate(totN=(NH4+NO3)/10)%>%
  mutate(Nadd=case_when(year<2011~totN+14, 
                        year>2010~totN+5))
#estimate 2020 as low Ndep year due to COVID
Ndep<-arrange(Ndep, totN)#arrange by total N ascending 
Ndep[15,1]<-2020
Ndep[15,2]<-"Summer"
Ndep[15,5]<-((Ndep[1,5]+Ndep[2,5])/2)#use average of two lowest values (2010, 2014)   
Ndep[15,6]<-Ndep[15,5]+5 #add 5 for N treatment 
Ndep<-rename(Ndep, X=totN, N=Nadd)%>%pivot_longer(cols=c("X", "N"),names_to = "N", values_to = "Ndep")%>%
  select(year, N, Ndep)

#allow cumulative build up 10% from each previous year accumulates plus new addition
#for N addition plots only 
Ndep<-mutate(Ndep, time=year-2006)%>%
  mutate(NdepCum1=case_when(N=="N"&time<5~time*(Ndep*0.1), TRUE~0))%>%
  mutate(NdepCum2=case_when(N=="N"&time>4~sum(NdepCum1)+time*(Ndep*0.1), TRUE~0))%>%
  mutate(NdepCum= Ndep + NdepCum1+NdepCum2)%>%
  select(year, N, Ndep, NdepCum)

plot(Ndep$NdepCum~Ndep$year)#looks good-saturating for N addition plots 

ggplot(data=Ndep, aes(x=year, y=NdepCum, color=N))+ geom_point() +geom_line()+
  ylab("N deposition (g/m2)")+ theme_bw()

#update for 2006 with pre-treatment value 
Ndep_plot<- mutate(Ndep, 
    Ndep=case_when(year==2006~0.1606,TRUE~Ndep))%>%
  mutate(NdepCum=case_when(year==2006~0.1606,TRUE~NdepCum))

ggplot(data=Ndep_plot, aes(x=year, y=NdepCum, color=N))+ geom_point() +geom_line()+
  ylab("N deposition (g/m2)")+ theme_bw()

ggplot(data=snowdat, aes(x=year, y=depth_cm, fill=month))+ 
  geom_boxplot() #april is measured in 8 years and doesn't have outliers 

#Temperature----
#Munge temp data 
temp<-read.csv("data/raw_env/sdlcr23x-cr1000.daily.ml.data.csv")
temp2<-read.csv("data/raw_env/sdl_cr1000_tenmin.csv")#2020 data from ftp

#use June-August temp 
temp<-separate(temp, col = date, into = c("month", "day","year"), sep = "/", remove = F)%>%
  filter(month=='6'|month=='7'|month=='8')
temp$year<-as.numeric(temp$year)
#fill in 2019 data from new sensor 
temp<-filter(temp, year>2005)%>%mutate(airtemp_avg=ifelse(is.na(airtemp_avg), airtemp_hmp2_avg, airtemp_avg)) 
#fill missing values in 2010, 2011 with average for summer of that year 
#take averages across years
tempavg<-select(temp, year, airtemp_avg)%>%group_by(year)%>%summarise(Tmean=mean(airtemp_avg, na.rm=T))
tempavg_2010<-filter(tempavg, year==2010)
tempavg_2011<-filter(tempavg, year==2011)
temp<-mutate(temp, airtemp_avg=case_when(is.na(airtemp_avg)&year==2010~tempavg_2010$Tmean, 
                                         is.na(airtemp_avg)&year==2011~tempavg_2011$Tmean, TRUE~airtemp_avg))%>%
  group_by(year)%>%mutate(avgT=mean(airtemp_avg))%>%select(year, avgT)%>%distinct(.)
temp<-as.data.frame(temp)

#organize 2020 data
temp2<-separate(temp2, col = TIMESTAMP, into = c("month", "day","yr"), sep = "/", remove = F)%>%
  separate(col=yr, into=c("year","time"), sep=" ")%>% filter(month=='6'|month=='7'|month=='8')
hist(temp2$AirTC_HMP2_Avg)#looks good
temp2<-mutate(temp2, avgT=mean(AirTC_HMP2_Avg))%>%select(year, avgT)%>%
  distinct(.)
temp2<-as.data.frame(temp2)
temp<-rbind(temp, temp2) #combine temp and temp 2020
temp$W<-temp$avgT+1 #warming chambers increase 1 deg C
temp<-rename(temp, X=avgT)%>%pivot_longer(cols=c("X", "W"),names_to = "temp", values_to = "avgT")

ggplot(temp, aes(x=year, y=avgT, color=temp)) + geom_point(shape=15, size=2)+
  theme_bw()

#update for 2006 with pre-treatment value 
temp_plot<- mutate(temp, 
  avgT=case_when(year==2006~10.239163,TRUE~avgT))
temp_plot$year2<-as.numeric(as.character(temp_plot$year))

ggplot(temp_plot, aes(x=year2, y=avgT, color=temp)) + 
  geom_point(shape=16, size=2, position=position_dodge(width=0.5))+
  theme_bw()+ geom_line(lty=2) + xlim(2005, 2021)

#Snow----
#Munge Snow data 
snowdat<-read.csv("data/raw_env/NWT_ITEX_snowdepth_CC.csv")
sadsnow<-read.csv("data/raw_env/saddsnow.dw.data.csv")

#Remove unclear measurements and NAs  
#999 means snow too hard to probe/unsure of ground vs. hard layer
#>150 means greater than height of snow pole-ignore NA warnings 
snowdat<-mutate(snowdat, depth_cm=case_when(depth_cm==">150"~-1, 
                                            depth_cm=="999"~-1, 
                                            TRUE~as.numeric(depth_cm)))%>%filter(depth_cm>=0)%>%
  separate(col = date, into = c("month", "day","year"), sep = "/", remove = F)

#plot
ggplot(data=snowdat, aes(x=year, y=depth_cm, fill=month))+ 
  geom_boxplot() #april is measured in 8 years and doesn't have outliers 

ggplot(data=subset(snowdat, month=="4"), aes(x=year, y=depth_cm, fill=snow_trt))+ 
  geom_boxplot() #most years P>X, 2012 only P measured 

#lmsnow<-lm(depth_cm~snow_trt*year, snowdat) #P>X estimate=50.5 cm 
#coef<-lmsnow$coefficients

#use saddle snow data for infilling missing snow years
#take average snow depth across all saddle plots in each year (March-May)
sadsnow2<-separate(sadsnow, col = date, into = c("month", "day","year"), sep = "/", remove = F)%>%
  filter(month=="4"|month=="5")%>% group_by(year)%>%
  mutate(avg_depth=mean(mean_depth, na.rm=T))%>%
  select( year, month, avg_depth)%>%distinct(.)%>%mutate(ct=n_distinct(avg_depth))


#combine with ITEX snow data
#use April/May avg for all years except 2020 use May only bc April not measured 
snow_all<-left_join(sadsnow2, snowdat)%>%filter(year>2005)%>%
  mutate(keep=case_when(month<5&year<2020~1, 
                        month>4&year>2019~1, TRUE~0))%>%filter(keep>0)
#see how many snow measurements in each year 
check<-group_by(snow_all, year, block, snow_trt)%>%count()%>%group_by(year)%>%
  mutate(tot=sum(n))

#average itex snow years with multiple measurements in a plot 
snow_all<-group_by(snow_all, year, block, snow_trt)%>%mutate(depth_cm=mean(depth_cm))

snow_all<-select(snow_all, -day, -date, -notes, -snowfence)%>%distinct(.)

check<-group_by(snow_all, year, block, snow_trt)%>%count()%>%group_by(year)%>%
  mutate(tot=sum(n))

#full plot x years x treat df 
sppcomp <- read.csv("data/NWT_ITEX_SpComp_data_L1.csv")
sppcomp<-filter(sppcomp, !is.na(year))#remove spaces 
sppcomp<-rename(sppcomp, spp=JGS_code)%>%unite(., plotyear, plot, year, remove=F)
plotyears<-select(sppcomp, year, plot, snow, N, temp, block)%>%distinct(.)

snowyears<-select(plotyears, year, block, snow)%>%distinct(.)#full years
snowyears<-subset(snowyears, year<2009|year>2016)#missing years
snow_miss<-filter(snow_all, is.na(LTER_site))%>%mutate(year=as.integer(year))%>%select(-block)
snow_miss$block<-NULL
#expand missing data 
snow_miss<-full_join(snow_miss, snowyears)
snow_miss<-relocate(snow_miss, block, .before="snow_trt")
snow_miss$snow_trt<-NULL
snow_miss<-rename(snow_miss, snow_trt=snow)%>%relocate(snow_trt, .before=depth_cm)
snow_miss<-as.data.frame(snow_miss)
#remove missing data 
snow_all<-filter(snow_all, !is.na(LTER_site))
snow_all<-as.data.frame(snow_all)

#rbind
snow_allX<-rbind(snow_all, snow_miss)

#ITEX for infilling off saddle 
snow_allX$block<-as.factor(snow_allX$block)
snowmod<-lm(depth_cm~avg_depth +snow_trt + as.factor(block), snow_allX)
summary(snowmod)#r2=0.75
hist(snow_all$depth_cm)

#infill missing snow info with correct equations 
coef<-coef(snowmod)
snow_allXX<-mutate(snow_allX, 
                   depth_cm2=case_when(snow_trt=='X' &block==1~ (coef[1]+coef[2]*avg_depth+coef[3]), 
                                       snow_trt=='P' &block==1~ (coef[1]+coef[2]*avg_depth),
                                       snow_trt=='X' &block==2~ (coef[1]+coef[2]*avg_depth+coef[3]+coef[4]), 
                                       snow_trt=='P' &block==2~ (coef[1]+coef[2]*avg_depth+coef[4]), 
                                       snow_trt=='X' &block==3~ (coef[1]+coef[2]*avg_depth+coef[3]+coef[5]),
                                       snow_trt=='P' &block==3~ (coef[1]+coef[2]*avg_depth+coef[5])))
#plot obs vs pred 
plot(snow_allXX$depth_cm2~snow_allXX$depth_cm)
#pearson 
cor.test(snow_allXX$depth_cm2, snow_allXX$depth_cm) #87.4

#combine all 
snow_allXX<-mutate(snow_allXX, depth_cm=ifelse(is.na(depth_cm), depth_cm2, depth_cm))                    

#add infilling info 
snow_allXX<-select(snow_allXX, -keep)%>%mutate(infill=ifelse(depth_cm==depth_cm2, 1, 0))


#plot 
ggplot(data=snow_allXX, aes(x=as.factor(year), y=depth_cm, fill=snow_trt))+ 
  geom_boxplot()+ theme_bw()#looks OK

#update for 2006 with pre-treatment value 
snow_plot<- mutate(snow_allXX, 
depth_cm=case_when(year==2006&snow_trt=="P"&block ==1~	45.94156,
                   year==2006&snow_trt=="P"&block ==2~	50.10823,
                   year==2006&snow_trt=="P"&block ==3~	44.27490,
                    TRUE~depth_cm))
ggplot(data=snow_plot, aes(x=as.factor(year), y=depth_cm, fill=snow_trt))+ 
  geom_boxplot()+ theme_bw()#looks OK

#compare snow infilling 
library(mice)

#infill with mice 
snow_allX<-arrange(snow_allX, year, month, snow_trt, block)

snow_all_imp<-mice(snow_allX, m=10,  method = "pmm", maxit = 50, seed = 75)

#infill missing with means of 10 imputed values 
imp_data<-as.data.frame(snow_all_imp$imp$depth_cm)
imp_data$imp_depth_cm<-rowMeans(imp_data)
imp_data$ID<-row.names(imp_data)
imp_data<-select(imp_data, ID, imp_depth_cm)
snow_allX$ID<-row.names(snow_allX)
snow_allY<-left_join(snow_allX, imp_data, by="ID")%>%
  mutate(depth_cm=if_else(is.na(depth_cm), imp_depth_cm, depth_cm))

#pull out model coefficients to estimate known values for comparison 
fit<-with(data = snow_all_imp, exp=lm(depth_cm~ snow_trt + as.factor(block)))
coef2<-summary(pool(fit))
pool.r.squared(fit) #r2 =0.1 

coef2<-coef2$estimate

snow_allY<-mutate(snow_allY, 
                  imp_depth_cm=case_when(is.na(imp_depth_cm)& snow_trt=='X' &block==1~ (coef2[1]+coef2[2]), 
                                         is.na(imp_depth_cm)& snow_trt=='P' &block==1~ (coef2[1]),
                                         is.na(imp_depth_cm)&snow_trt=='X' &block==2~ (coef2[1]+coef2[2]+coef2[3]), 
                                         is.na(imp_depth_cm)& snow_trt=='P' &block==2~ (coef2[1]+coef2[3]), 
                                         is.na(imp_depth_cm)& snow_trt=='X' &block==3~ (coef2[1]+coef2[2]+coef2[4]),
                                         is.na(imp_depth_cm)&snow_trt=='P' &block==3~ (coef2[1]+coef2[4])))
#  mutate(imp_depth_cm=if_else(is.na(imp_depth_cm), depth_cm, imp_depth_cm)) #refill back in original imputed
snowmod2<-lm(depth_cm~imp_depth_cm +snow_trt + as.factor(block), snow_allY)
summary(snowmod2)#r2=0.33
#pearson 
cor.test(snow_allY$depth_cm, snow_allY$imp_depth_cm)

#combine all enviro data 
str(Ndep)
str(snow_allXX)
str(temp)
temp$year<-as.numeric(temp$year)
snow_allXX$year<-as.numeric(snow_allX$year)
snow_allXX<-select(snow_allXX, -depth_cm2, -avg_depth, -ct, -local_site, -LTER_site, -infill)
enviro<-left_join(snow_allXX, Ndep)
enviro<-left_join(enviro, temp)%>%distinct(.)

save(enviro, file="data/Enviro.Rdata")


