library(bayestestR)
library(dplyr)
library(tidyr)
library(brms)
library(ggplot2)

#XXX----
load(file = "outputs/wstar_XXXoutput_dom.RData")

#pull out of lists 
wstarmu<-as.data.frame(wstarXXX$ccMu)%>%
  pivot_longer(. , cols=everything(),names_to="group", 
                values_to="mu")
wstarsd<-as.data.frame(wstarXXX$ccSd)%>%
  pivot_longer(. , cols=everything(),names_to="group", 
               values_to="sd")

env<-as.data.frame(wstarXXX$x)

#combine into one df
wstarxxx<-cbind(wstarmu, select(wstarsd, -group))
wstarxxx<-arrange(wstarxxx,group)%>% 
  mutate(group = factor(group, levels=c( "DOM", "SUBDOM", "MODERATE", "RARE")))
wstarxxx<-cbind(wstarxxx, env)
#wstarxxx$value<-wstarxxx$mu+wstarxxx$sd
  
#plots
#plot raw data-
ggplot(wstarxxx, aes(x=depthcm, y=mu, col=group)) + geom_point(alpha=0.2)+ 
   geom_smooth(se = T, lty=2)+
#  geom_ribbon(aes(ymin=mu-(sd), ymax=mu+(sd)), fill="lightgray", color="lightgray", alpha=.2) +
  facet_wrap(~group, scales ="free")+ theme_classic()+
  theme(strip.background = element_blank(), legend.position = 'none',
    strip.text.x = element_blank())+ ylab("Steady State abundance") + xlab("Snow depth (std devs)")
  
e<-ggplot(wstarxxx, aes(x=Ndep, y=mu, col=group)) + geom_point(alpha=0.2)+ 
  geom_smooth(se = T, lty=2)+
  #  geom_ribbon(aes(ymin=mu-(sd), ymax=mu+(sd)), fill="lightgray", color="lightgray", alpha=.2) +
  facet_wrap(~group, scales ="free")+ theme_classic()+scale_color_manual(values=color2)+
  theme(strip.background = element_blank(), legend.position = 'none',
        strip.text.x = element_blank())+ ylab("Steady State abundance") + xlab("N deposition (std devs)")+
  ggtitle("Control")


ggplot(wstarxxx, aes(x=avgT, y=mu, col=group)) + geom_point(alpha=0.2)+ 
  geom_smooth(se = T, lty=2)+
  #  geom_ribbon(aes(ymin=mu-(sd), ymax=mu+(sd)), fill="lightgray", color="lightgray", alpha=.2) +
  facet_wrap(~group, scales ="free")+ theme_classic()+
  theme(strip.background = element_blank(), legend.position = 'none',
        strip.text.x = element_blank())+ ylab("Steady State abundance") + xlab("Temperature (std devs)")

#XXW----
load(file = "outputs/wstar_XXWoutput_dom.RData")

#pull out of lists 
wstarmu<-as.data.frame(wstarXXW$ccMu)%>%
  pivot_longer(. , cols=everything(),names_to="group", 
               values_to="mu")
wstarsd<-as.data.frame(wstarXXW$ccSd)%>%
  pivot_longer(. , cols=everything(),names_to="group", 
               values_to="sd")
env<-as.data.frame(wstarXXW$x)

#combine into one df
wstarxxw<-cbind(wstarmu, select(wstarsd, -group))
wstarxxw<-arrange(wstarxxw,group)%>% 
  mutate(group = factor(group, levels=c( "DOM", "SUBDOM", "MODERATE", "RARE")))
wstarxxw<-cbind(wstarxxw, env)
#wstarxxw$value<-wstarxxw$mu+wstarxxw$sd

#plots
#plot raw data-
a<-ggplot(wstarxxw, aes(x=Ndep, y=mu, col=group)) + geom_point(alpha=0.25)+ 
  geom_smooth(se = T, lty=2)+
  #  geom_ribbon(aes(ymin=mu-(sd), ymax=mu+(sd)), fill="lightgray", color="lightgray", alpha=.2) +
  facet_wrap(~group, scales ="free")+ theme_classic()+scale_color_manual(values=color2)+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())+ ylab("Steady State abundance") + xlab("N deposition (std devs)")+
  ggtitle("Warming")
  

#XNW----
load(file = "outputs/wstar_XNWoutput_dom.RData")

#pull out of lists 
wstarmu<-as.data.frame(wstarXNW$ccMu)%>%
  pivot_longer(. , cols=everything(),names_to="group", 
               values_to="mu")
wstarsd<-as.data.frame(wstarXNW$ccSd)%>%
  pivot_longer(. , cols=everything(),names_to="group", 
               values_to="sd")
env<-as.data.frame(wstarXNW$x)

#combine into one df
wstarxnw<-cbind(wstarmu, select(wstarsd, -group))
wstarxnw<-arrange(wstarxnw,group)%>% 
  mutate(group = factor(group, levels=c( "DOM", "SUBDOM", "MODERATE", "RARE")))
wstarxnw<-cbind(wstarxnw, env)

#plots
#plot raw data-
b<-ggplot(wstarxnw, aes(x=Ndep, y=mu, col=group)) + geom_point(alpha=0.25)+ 
  geom_smooth(se = T, lty=2)+
  #  geom_ribbon(aes(ymin=mu-(sd), ymax=mu+(sd)), fill="lightgray", color="lightgray", alpha=.2) +
  facet_wrap(~group, scales ="free")+ theme_classic()+ scale_color_manual(values=color2)+
  theme(strip.background = element_blank(),
      strip.text.x = element_blank())+  ylab(" ") +xlab("N deposition (std devs)")+
  ggtitle("N + Warming")

ggplot(wstarxnw, aes(x=avgT, y=mu, col=group)) + geom_point(alpha=0.1)+ 
  geom_smooth(se = T, lty=2)+
  #  geom_ribbon(aes(ymin=mu-(sd), ymax=mu+(sd)), fill="lightgray", color="lightgray", alpha=.2) +
  facet_wrap(~group, scales ="free")+ theme_classic()+ scale_color_manual(values=color2)+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())+   ylab("Steady State abundance") + xlab("Temperature (std devs)")

#Fig 4
ggpubr::ggarrange(a,b, common.legend = TRUE,  ncol = 2, nrow = 1)


#PXW----
load(file = "outputs/wstar_PXWoutput_dom.RData")

#pull out of lists 
wstarmu<-as.data.frame(wstarPXW$ccMu)%>%
  pivot_longer(. , cols=everything(),names_to="group", 
               values_to="mu")
wstarsd<-as.data.frame(wstarPXW$ccSd)%>%
  pivot_longer(. , cols=everything(),names_to="group", 
               values_to="sd")
env<-as.data.frame(wstarPXW$x)

#combine into one df
wstarpxw<-cbind(wstarmu, select(wstarsd, -group))
wstarpxw<-arrange(wstarpxw,group)%>% 
  mutate(group = factor(group, levels=c( "DOM", "SUBDOM", "MODERATE", "RARE")))
wstarpxw<-cbind(wstarpxw, env)

#plots
#plot raw data-
c<-ggplot(wstarpxw, aes(x=Ndep, y=mu, col=group)) + geom_point(alpha=0.25)+ 
  geom_smooth(se = T, lty=2)+
  #  geom_ribbon(aes(ymin=mu-(sd), ymax=mu+(sd)), fill="lightgray", color="lightgray", alpha=.2) +
  facet_wrap(~group, scales ="free")+ theme_classic()+ scale_color_manual(values=color2)+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())+  ylab(" ") + xlab(" ")+
  ggtitle("Snow + Warming") 

ggplot(wstarpxw, aes(x=depthcm, y=mu, col=group)) + geom_point(alpha=0.25)+ 
  geom_smooth(se = T, lty=2)+
  #  geom_ribbon(aes(ymin=mu-(sd), ymax=mu+(sd)), fill="lightgray", color="lightgray", alpha=.2) +
  facet_wrap(~group, scales ="free")+ theme_classic()+ scale_color_manual(values=color2)+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())+   ylab("Steady State abundance") + xlab("Snow depth (std devs) ")

ggplot(wstarpxw, aes(x=avgT, y=mu, col=group)) + geom_point(alpha=0.25)+ 
  geom_smooth(se = T, lty=2)+
  #  geom_ribbon(aes(ymin=mu-(sd), ymax=mu+(sd)), fill="lightgray", color="lightgray", alpha=.2) +
  facet_wrap(~group, scales ="free")+ theme_classic()+ scale_color_manual(values=color2)+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())+   ylab("Steady State abundance") + xlab("Temperature (std devs)")



#PNW----
load(file = "outputs/wstar_PNWoutput_dom.RData")
#pull out of lists 
wstarmu<-as.data.frame(wstarPNW$ccMu)%>%
  pivot_longer(. , cols=everything(),names_to="group", 
               values_to="mu")
wstarsd<-as.data.frame(wstarPNW$ccSd)%>%
  pivot_longer(. , cols=everything(),names_to="group", 
               values_to="sd")
env<-as.data.frame(wstarPNW$x)

#combine into one df
wstarpnw<-cbind(wstarmu, select(wstarsd, -group))
wstarpnw<-arrange(wstarpnw,group)%>% 
  mutate(group = factor(group, levels=c( "DOM", "SUBDOM", "MODERATE", "RARE")))
wstarpnw<-cbind(wstarpnw, env)

d<-ggplot(wstarpnw, aes(x=Ndep, y=mu, col=group)) + geom_point(alpha=0.25)+ 
  geom_smooth(se = T, lty=2)+
  #  geom_ribbon(aes(ymin=mu-(sd), ymax=mu+(sd)), fill="lightgray", color="lightgray", alpha=.2) +
  facet_wrap(~group, scales ="free")+ theme_classic()+ scale_color_manual(values=color2)+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())+  ylab(" Steady State abundance") + xlab(" ")+
  ggtitle("Snow + N+ Warming") 

#Fig S9
ggpubr::ggarrange(d, c, b, a, e, common.legend = TRUE,  legend= "none", ncol = 3, nrow = 2)

#plots
#plot raw data-don't know how to get sds in there 
ggplot(wstarpnw, aes(x=depthcm, y=mu, col=spp)) +geom_smooth()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")

ggplot(wstarpnw, aes(x=Ndep, y=mu, col=spp)) +geom_smooth()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")

ggplot(wstarpnw, aes(x=avgT, y=mu, col=spp)) +geom_smooth()+
  #geom_ribbon(aes(ymin=mu-(sd/10), ymax=mu+(sd/10)), fill="lightgray", color="lightgray", alpha=.8) +
  facet_wrap(~spp, scales ="free")


