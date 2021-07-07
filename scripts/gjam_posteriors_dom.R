library(dplyr)
library(bayestestR)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(corrplot)

#rhos---- 
#XXX
load(file = "outputs/modDAtime_XXXoutput_dom.RData")
rhosXXX<-as.data.frame(modDAtimeXXX$chains$lgibbs)
rhosXXX$treat<-"XXX"

#XXW
load(file = "outputs/modDAtime_XXWoutput_dom.RData")
rhosXXW<-as.data.frame(modDAtimeXXW$chains$lgibbs)
rhosXXW$treat<-"XXW"

#PXX
load(file = "outputs/modDAtime_PXXoutput_dom.RData")
rhosPXX<-as.data.frame(modDAtimePXX$chains$lgibbs)
rhosPXX$treat<-"PXX"

#XNX
load(file = "outputs/modDAtime_XNXoutput_dom.RData")
rhosXNX<-as.data.frame(modDAtimeXNX$chains$lgibbs)
rhosXNX$treat<-"XNX"

#PNX
load(file = "outputs/modDAtime_PNXoutput_dom.RData")
rhosPNX<-as.data.frame(modDAtimePNX$chains$lgibbs)
rhosPNX$treat<-"PNX"

#XNW
load(file = "outputs/modDAtime_XNWoutput_dom.RData")
rhosXNW<-as.data.frame(modDAtimeXNW$chains$lgibbs)
rhosXNW$treat<-"XNW"

#PXW
load(file = "outputs/modDAtime_PXWoutput_dom.RData")
rhosPXW<-as.data.frame(modDAtimePXW$chains$lgibbs)
rhosPXW$treat<-"PXW"

#PNW
load(file = "outputs/modDAtime_PNWoutput_dom.RData")
rhosPNW<-as.data.frame(modDAtimePNW$chains$lgibbs)
rhosPNW$treat<-"PNW"

#plot all 
rhosall<-rbind(rhosXXX, rhosXXW, rhosPXX, rhosXNX, rhosPNX,  rhosXNW, rhosPXW, rhosPNW)
ggplot(rhosall, aes(y=DOM_depthcm, fill=treat))+ geom_boxplot( )+
  geom_hline(aes(yintercept=0), lty=2, color="red")+# xlim (-3, 2)+
  theme_classic()+ ylab("Effect of snow depth")+ 
  xlab(" ")+ theme(axis.text.x=element_blank(),
                   axis.ticks.x=element_blank())+ ggtitle("Dominant species")

ggplot(rhosall, aes(y=DOM_Ndep, fill=treat))+ geom_boxplot( )+
  geom_hline(aes(yintercept=0), lty=2, color="red")+# xlim (-3, 2)+
  theme_classic()+ ylab("Effect of N deposition")+
  xlab(" ")+ theme(axis.text.x=element_blank(),
                   axis.ticks.x=element_blank()) + ggtitle("Dominant species")

ggplot(rhosall, aes(y=DOM_avgT, fill=treat))+ geom_boxplot( )+
  geom_hline(aes(yintercept=0), lty=2, color="red")+# xlim (-3, 2)+
  theme_classic()+ ylab("Effect of temperature")+
  xlab(" ")+ theme(axis.text.x=element_blank(),
                   axis.ticks.x=element_blank()) +ggtitle("Dominant species")

ggplot(rhosall, aes(y=SUBDOM_depthcm, fill=treat))+ geom_boxplot( )+
  geom_hline(aes(yintercept=0), lty=2, color="red")+# xlim (-3, 2)+
  theme_classic()+ ylab("Effect of snow depth")+ 
  xlab(" ")+ theme(axis.text.x=element_blank(),
                   axis.ticks.x=element_blank()) +ggtitle("Subominant species")

ggplot(rhosall, aes(y=SUBDOM_Ndep, fill=treat))+ geom_boxplot( )+
  geom_hline(aes(yintercept=0), lty=2, color="red")+# xlim (-3, 2)+
  theme_classic()+ ylab("Effect of N deposition")+
  xlab(" ")+ theme(axis.text.x=element_blank(),
                   axis.ticks.x=element_blank()) +ggtitle("Subominant species")

ggplot(rhosall, aes(y=SUBDOM_avgT, fill=treat))+ geom_boxplot( )+
  geom_hline(aes(yintercept=0), lty=2, color="red")+# xlim (-3, 2)+
  theme_classic()+ ylab("Effect of temperature")+
  xlab(" ")+ theme(axis.text.x=element_blank(),
                   axis.ticks.x=element_blank()) +ggtitle("Subominant species")

ggplot(rhosall, aes(y=MODERATE_depthcm, fill=treat))+ geom_boxplot( )+
  geom_hline(aes(yintercept=0), lty=2, color="red")+# xlim (-3, 2)+
  theme_classic()+ ylab("Effect of snow depth")+ 
  xlab(" ")+ theme(axis.text.x=element_blank(),
                   axis.ticks.x=element_blank()) +ggtitle("Moderate species")

ggplot(rhosall, aes(y=MODERATE_Ndep, fill=treat))+ geom_boxplot( )+
  geom_hline(aes(yintercept=0), lty=2, color="red")+# xlim (-3, 2)+
  theme_classic()+ ylab("Effect of N deposition")+
  xlab(" ")+ theme(axis.text.x=element_blank(),
                   axis.ticks.x=element_blank())+ggtitle("Moderate species")

ggplot(rhosall, aes(y=MODERATE_avgT, fill=treat))+ geom_boxplot( )+
  geom_hline(aes(yintercept=0), lty=2, color="red")+# xlim (-3, 2)+
  theme_classic()+ ylab("Effect of temperature")+
  xlab(" ")+ theme(axis.text.x=element_blank(),
                   axis.ticks.x=element_blank()) +ggtitle("Moderate species")

ggplot(rhosall, aes(y=RARE_depthcm, fill=treat))+ geom_boxplot( )+
  geom_hline(aes(yintercept=0), lty=2, color="red")+# xlim (-3, 2)+
  theme_classic()+ ylab("Effect of snow depth")+ 
  xlab(" ")+ theme(axis.text.x=element_blank(),
                   axis.ticks.x=element_blank()) +ggtitle("Rare species")

ggplot(rhosall, aes(y=RARE_Ndep, fill=treat))+ geom_boxplot( )+
  geom_hline(aes(yintercept=0), lty=2, color="red")+# xlim (-3, 2)+
  theme_classic()+ ylab("Effect of N deposition")+
  xlab(" ")+ theme(axis.text.x=element_blank(),
                   axis.ticks.x=element_blank()) +ggtitle("Rare species")

ggplot(rhosall, aes(y=RARE_avgT, fill=treat))+ geom_boxplot( )+
  geom_hline(aes(yintercept=0), lty=2, color="red")+# xlim (-3, 2)+
  theme_classic()+ ylab("Effect of temperature")+
  xlab(" ")+ theme(axis.text.x=element_blank(),
                   axis.ticks.x=element_blank()) +ggtitle("Rare species")

#alphas----
#XXX vs XNX
alphasXXX<-as.data.frame(modDAtimeXXX$parameters$alphaMu)

alphasXNX<-as.data.frame(modDAtimeXNX$parameters$alphaMu)

delta_alpha_XNX_XXX<-as.matrix(alphasXNX-alphasXXX)

pdf(file="plots/modDAtime_XNXplots_dom/delta_alpha_plot.pdf")
corrplot(delta_alpha_XNX_XXX ,method = "color", 
         tl.cex = 0.8, tl.col="black", addCoef.col = "black",
         number.cex = 0.75, diag =T, main="delta alphas XNX-XXX", is.corr = FALSE, 
         mar = c(2, 2, 2, 2), cl.lim = c(-0.5, 0.5))
dev.off()
#decreases competition between moderate and dominant, increases competition between dominant and 
#subdominant
#no effect on intraspecific comp

#XXX vs XXW
alphasXXW<-as.data.frame(modDAtimeXXW$parameters$alphaMu)

delta_alpha_XXW_XXX<-as.matrix(alphasXXW-alphasXXX)

pdf(file="plots/modDAtime_XXWplots_dom/delta_alpha_plot.pdf")
corrplot(delta_alpha_XXW_XXX ,method = "color", 
         tl.cex = 0.8, tl.col="black", addCoef.col = "black",
         number.cex = 0.75, diag =T, main="delta alphas XXW-XXX", is.corr = FALSE, 
         mar = c(2, 2, 2, 2), cl.lim = c(-0.5, 0.5))
dev.off()

#decreases competition between moderate and dominant, increases competition between subdominant and 
#dominant
#no effect on intraspecific comp

#XXX vs PXX
alphasPXX<-as.data.frame(modDAtimePXX$parameters$alphaMu)

delta_alpha_PXX_XXX<-as.matrix(alphasPXX-alphasXXX)

pdf(file="plots/modDAtime_PXXplots_dom/delta_alpha_plot.pdf")
corrplot(delta_alpha_PXX_XXX ,method = "color", 
         tl.cex = 0.8, tl.col="black", addCoef.col = "black",
         number.cex = 0.75, diag =T, main="delta alphas PXX-XXX", is.corr = FALSE, 
         mar = c(2, 2, 2, 2), cl.lim = c(-0.5, 0.5))
dev.off()

#decreases competition of moderate on dominant, but increases competition of dominant on moderate 
#slightly weakens intraspecific comp for all (delta <0.2)

#XXX vs XNW
alphasXNW<-as.data.frame(modDAtimeXNW$parameters$alphaMu)

delta_alpha_XNW_XXX<-as.matrix(alphasXNW-alphasXXX)

pdf(file="plots/modDAtime_XNWplots_dom/delta_alpha_plot.pdf")
corrplot(delta_alpha_XNW_XXX ,method = "color", 
         tl.cex = 0.8, tl.col="black", addCoef.col = "black",
         number.cex = 0.75, diag =T, main="delta alphas XNW-XXX", is.corr = FALSE, 
         mar = c(2, 2, 2, 2), cl.lim = c(-0.5, 0.5))
dev.off()
#decreases commpetition between moderate and dominant both directions, decreases competition between 
#rare and subdominant 

#XXX vs PNX
alphasPNX<-as.data.frame(modDAtimePNX$parameters$alphaMu)

delta_alpha_PNX_XXX<-as.matrix(alphasPNX-alphasXXX)

pdf(file="plots/modDAtime_PNXplots_dom/delta_alpha_plot.pdf")
corrplot(delta_alpha_PNX_XXX ,method = "color", 
         tl.cex = 0.8, tl.col="black", addCoef.col = "black",
         number.cex = 0.75, diag =T, main="delta alphas PNX-XXX", is.corr = FALSE, 
         mar = c(2, 2, 2, 2), cl.lim = c(-0.5, 0.5))
dev.off()
#decreases competition between moderate and dominant, increases competition between dominant and 
#subdominant-same as XNX 
#slightly decreses competition between rare and dominant 

#XXX vs PXW
alphasPXW<-as.data.frame(modDAtimePXW$parameters$alphaMu)

delta_alpha_PXW_XXX<-as.matrix(alphasPXW-alphasXXX)

pdf(file="plots/modDAtime_PXWplots_dom/delta_alpha_plot.pdf")
corrplot(delta_alpha_PXW_XXX ,method = "color", 
         tl.cex = 0.8, tl.col="black", addCoef.col = "black",
         number.cex = 0.75, diag =T, main="delta alphas PXW-XXX", is.corr = FALSE, 
         mar = c(2, 2, 2, 2), cl.lim = c(-0.5, 0.5))
dev.off()
#weakens intraspecific comeptition of dominant---decreased negative frequency dependence 
#increases competition between dominant and subdominant 


#XXX vs PNW
alphasPNW<-as.data.frame(modDAtimePNW$parameters$alphaMu)

delta_alpha_PNW_XXX<-as.matrix(alphasPNW-alphasXXX)

pdf(file="plots/modDAtime_PNWplots_dom/delta_alpha_plot.pdf")
corrplot(delta_alpha_PNW_XXX ,method = "color", 
         tl.cex = 0.8, tl.col="black", addCoef.col = "black",
         number.cex = 0.75, diag =T, main="delta alphas PNW-XXX", is.corr = FALSE, 
         mar = c(2, 2, 2, 2), cl.lim = c(-0.5, 0.5))
dev.off()
#increases competition between subdominant and moderate both directions 
#decreases comeptition between moderate and dominant 

#BROAD PATTERNS 
#global change affected interspecific competition more than intraspecific. Self regulation was high 
# in ambient conditions and mostly stayed high- exception was with snow addition, PXX slightly weakened 
#and PXW strongly weakened negative density dependence (especially for dominant spp)-this pushed PXX communities
#to be unstable (positive real eigenvalue)
#moderate species also strongly competed with dominant species in ambient conditions which lessened 
#in many global change treatments, especially N addiiton 
#subdominant species did not compete with dominant species in ambient conditions which increased 
#in many global change treatments, especially N addition 

#Competition ratios----
#DOM
#XXX
alphasXXX<-as.data.frame(modDAtimeXXX$chains$alphaGibbs)
names<-modDAtimeXXX$parameters$alphaTable$`alpha_{to, from}`
colnames(alphasXXX)<-names
alphasXXX<-select(alphasXXX, "DOM, DOM", "DOM, SUBDOM","DOM, MODERATE", "DOM, RARE",
                  "SUBDOM, DOM","MODERATE, DOM", "RARE, DOM",)
alphasXXX<-rowwise(alphasXXX)%>%
  mutate(inter_eff=sum(c_across(`DOM, SUBDOM`:`DOM, RARE`)))%>%
  mutate(inter_res=sum(c_across(`SUBDOM, DOM`:`RARE, DOM`)))%>%
  mutate(inter_eff=inter_eff/3, inter_res=inter_res/3)%>%
  mutate(intra=`DOM, DOM`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)
alphasXXX$treat<-"XXX"


#XXW
alphasXXW<-as.data.frame(modDAtimeXXW$chains$alphaGibbs)
names<-modDAtimeXXW$parameters$alphaTable$`alpha_{to, from}`
colnames(alphasXXW)<-names
alphasXXW<-select(alphasXXW, "DOM, DOM", "DOM, SUBDOM","DOM, MODERATE", "DOM, RARE",
                  "SUBDOM, DOM","MODERATE, DOM", "RARE, DOM",)
alphasXXW<-rowwise(alphasXXW)%>%
  mutate(inter_eff=sum(c_across(`DOM, SUBDOM`:`DOM, RARE`)))%>%
  mutate(inter_res=sum(c_across(`SUBDOM, DOM`:`RARE, DOM`)))%>%
  mutate(inter_eff=inter_eff/3, inter_res=inter_res/3)%>%
  mutate(intra=`DOM, DOM`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)
alphasXXW$treat<-"XXW"

#PXX
alphasPXX<-as.data.frame(modDAtimePXX$chains$alphaGibbs)
names<-modDAtimePXX$parameters$alphaTable$`alpha_{to, from}`
colnames(alphasPXX)<-names
alphasPXX<-select(alphasPXX, "DOM, DOM", "DOM, SUBDOM","DOM, MODERATE", "DOM, RARE",
                  "SUBDOM, DOM","MODERATE, DOM", "RARE, DOM",)
alphasPXX<-rowwise(alphasPXX)%>%
  mutate(inter_eff=sum(c_across(`DOM, SUBDOM`:`DOM, RARE`)))%>%
  mutate(inter_res=sum(c_across(`SUBDOM, DOM`:`RARE, DOM`)))%>%
  mutate(inter_eff=inter_eff/3, inter_res=inter_res/3)%>%
  mutate(intra=`DOM, DOM`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)
alphasPXX$treat<-"PXX"

#XNX
alphasXNX<-as.data.frame(modDAtimeXNX$chains$alphaGibbs)
names<-modDAtimeXNX$parameters$alphaTable$`alpha_{to, from}`
colnames(alphasXNX)<-names
alphasXNX<-select(alphasXNX, "DOM, DOM", "DOM, SUBDOM","DOM, MODERATE", "DOM, RARE",
                  "SUBDOM, DOM","MODERATE, DOM", "RARE, DOM",)
alphasXNX<-rowwise(alphasXNX)%>%
  mutate(inter_eff=sum(c_across(`DOM, SUBDOM`:`DOM, RARE`)))%>%
  mutate(inter_res=sum(c_across(`SUBDOM, DOM`:`RARE, DOM`)))%>%
  mutate(inter_eff=inter_eff/3, inter_res=inter_res/3)%>%
  mutate(intra=`DOM, DOM`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)
alphasXNX$treat<-"XNX"

#PNX
alphasPNX<-as.data.frame(modDAtimePNX$chains$alphaGibbs)
names<-modDAtimePNX$parameters$alphaTable$`alpha_{to, from}`
colnames(alphasPNX)<-names
alphasPNX<-select(alphasPNX, "DOM, DOM", "DOM, SUBDOM","DOM, MODERATE", "DOM, RARE",
                  "SUBDOM, DOM","MODERATE, DOM", "RARE, DOM",)
alphasPNX<-rowwise(alphasPNX)%>%
  mutate(inter_eff=sum(c_across(`DOM, SUBDOM`:`DOM, RARE`)))%>%
  mutate(inter_res=sum(c_across(`SUBDOM, DOM`:`RARE, DOM`)))%>%
  mutate(inter_eff=inter_eff/3, inter_res=inter_res/3)%>%
  mutate(intra=`DOM, DOM`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)
alphasPNX$treat<-"PNX"

#XNW
alphasXNW<-as.data.frame(modDAtimeXNW$chains$alphaGibbs)
names<-modDAtimeXNW$parameters$alphaTable$`alpha_{to, from}`
colnames(alphasXNW)<-names
alphasXNW<-select(alphasXNW, "DOM, DOM", "DOM, SUBDOM","DOM, MODERATE", "DOM, RARE",
                  "SUBDOM, DOM","MODERATE, DOM", "RARE, DOM",)
alphasXNW<-rowwise(alphasXNW)%>%
  mutate(inter_eff=sum(c_across(`DOM, SUBDOM`:`DOM, RARE`)))%>%
  mutate(inter_res=sum(c_across(`SUBDOM, DOM`:`RARE, DOM`)))%>%
  mutate(inter_eff=inter_eff/3, inter_res=inter_res/3)%>%
  mutate(intra=`DOM, DOM`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)
alphasXNW$treat<-"XNW"

#PXW
alphasPXW<-as.data.frame(modDAtimePXW$chains$alphaGibbs)
names<-modDAtimePXW$parameters$alphaTable$`alpha_{to, from}`
colnames(alphasPXW)<-names
alphasPXW<-select(alphasPXW, "DOM, DOM", "DOM, SUBDOM","DOM, MODERATE", "DOM, RARE",
                  "SUBDOM, DOM","MODERATE, DOM", "RARE, DOM",)
alphasPXW<-rowwise(alphasPXW)%>%
  mutate(inter_eff=sum(c_across(`DOM, SUBDOM`:`DOM, RARE`)))%>%
  mutate(inter_res=sum(c_across(`SUBDOM, DOM`:`RARE, DOM`)))%>%
  mutate(inter_eff=inter_eff/3, inter_res=inter_res/3)%>%
  mutate(intra=`DOM, DOM`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)
alphasPXW$treat<-"PXW"

#PNW
alphasPNW<-as.data.frame(modDAtimePNW$chains$alphaGibbs)
names<-modDAtimePNW$parameters$alphaTable$`alpha_{to, from}`
colnames(alphasPNW)<-names
alphasPNW<-select(alphasPNW, "DOM, DOM", "DOM, SUBDOM","DOM, MODERATE", "DOM, RARE",
                  "SUBDOM, DOM","MODERATE, DOM", "RARE, DOM",)
alphasPNW<-rowwise(alphasPNW)%>%
  mutate(inter_eff=sum(c_across(`DOM, SUBDOM`:`DOM, RARE`)))%>%
  mutate(inter_res=sum(c_across(`SUBDOM, DOM`:`RARE, DOM`)))%>%
  mutate(inter_eff=inter_eff/3, inter_res=inter_res/3)%>%
  mutate(intra=`DOM, DOM`)%>%
  mutate(intra_inter_eff=intra/inter_eff, intra_inter_res=intra/inter_res)
alphasPNW$treat<-"PNW"


alphasall<-rbind(alphasXXX, alphasXXW, alphasPXX, alphasXNX, alphasPNX,  alphasXNW, alphasPXW, alphasPNW)

ggplot(alphasall, aes(y=intra, fill=treat))+ 
  geom_boxplot()+#ylim(-0.25, -1)+
  #geom_hline(aes(yintercept=0), lty=2, color="red")+# xlim (-3, 2)+
  theme_classic()+ ylab("Intraspp competition")+ 
  xlab(" ")+ theme(axis.text.x=element_blank(),
                   axis.ticks.x=element_blank())+ggtitle("Dominant species")

ggplot(alphasall, aes(y=inter_res, fill=treat))+ geom_boxplot( )+
  #geom_hline(aes(yintercept=0), lty=2, color="red")+# xlim (-3, 2)+
  theme_classic()+ ylab("Interspecific response competition")+
  xlab(" ")+ theme(axis.text.x=element_blank(),
                   axis.ticks.x=element_blank()) +ggtitle("Dominant species")

ggplot(alphasall, aes(y=inter_eff, fill=treat))+ geom_boxplot( )+
  #geom_hline(aes(yintercept=0), lty=2, color="red")+# xlim (-3, 2)+
  theme_classic()+ ylab("Interspecific effect competition")+
  xlab(" ")+ theme(axis.text.x=element_blank(),
                   axis.ticks.x=element_blank()) +ggtitle("Dominant species")

ggplot(alphasall, aes(y=log(intra_inter_eff), fill=treat))+ geom_boxplot( )+
  #geom_hline(aes(yintercept=0), lty=2, color="red")+# xlim (-3, 2)+
  theme_classic()+ ylab("Interspecific competition")+
  xlab(" ")+ theme(axis.text.x=element_blank(),
                   axis.ticks.x=element_blank()) +ggtitle("Dominant species")

ggplot(alphasall, aes(y=log(intra_inter_res), fill=treat))+ geom_boxplot( )+
  #geom_hline(aes(yintercept=0), lty=2, color="red")+# xlim (-3, 2)+
  theme_classic()+ ylab("Interspecific competition")+
  xlab(" ")+ theme(axis.text.x=element_blank(),
                   axis.ticks.x=element_blank()) +ggtitle("Dominant species")

