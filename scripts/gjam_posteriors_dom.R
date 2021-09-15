library(dplyr)
library(bayestestR)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(corrplot)
library(bayestestR)

#rhos---- 
#XXX
load(file = "outputs/modDAtime_XXXoutput_dom.RData")
rhosXXX<-as.data.frame(modDAtimeXXX$parameters$rhoStandXTable)
rhosXXX<-separate(rhosXXX, 'rho_{to, from}', c("group", "enviro"))                       
rhosXXX$treat<-"XXX"


#XXW
load(file = "outputs/modDAtime_XXWoutput_dom.RData")
rhosXXW<-as.data.frame(modDAtimeXXW$parameters$rhoStandXTable)
rhosXXW<-separate(rhosXXW, 'rho_{to, from}', c("group", "enviro"))                       
rhosXXW$treat<-"XXW"

#PXX
#load(file = "outputs/modDAtime_PXXoutput_dom.RData")
#rhosPXX<-as.data.frame(modDAtimePXX$chains$lgibbs)
#rhosPXX$treat<-"PXX"

#XNX
#load(file = "outputs/modDAtime_XNXoutput_dom.RData")
#rhosXNX<-as.data.frame(modDAtimeXNX$chains$lgibbs)
#rhosXNX$treat<-"XNX"

#PNX
#load(file = "outputs/modDAtime_PNXoutput_dom.RData")
#rhosPNX<-as.data.frame(modDAtimePNX$chains$lgibbs)
#rhosPNX$treat<-"PNX"

#XNW
load(file = "outputs/modDAtime_XNWoutput_dom.RData")
rhosXNW<-as.data.frame(modDAtimeXNW$parameters$rhoStandXTable)
rhosXNW<-separate(rhosXNW, 'rho_{to, from}', c("group", "enviro"))                       
rhosXNW$treat<-"XNW"

#PXW
load(file = "outputs/modDAtime_PXWoutput_dom.RData")
rhosPXW<-as.data.frame(modDAtimePXW$parameters$rhoStandXTable)
rhosPXW<-separate(rhosPXW, 'rho_{to, from}', c("group", "enviro"))                       
rhosPXW$treat<-"PXW"

#PNW
load(file = "outputs/modDAtime_PNWoutput_dom.RData")
rhosPNW<-as.data.frame(modDAtimePNW$parameters$rhoStandXTable)
rhosPNW<-separate(rhosPNW, 'rho_{to, from}', c("group", "enviro"))                       
rhosPNW$treat<-"PNW"

#plot all 
rhosall<-rbind(rhosXXX, rhosXXW,rhosXNW, rhosPXW, rhosPNW)
rhosall<-subset(rhosall, enviro!="intercept")
specColor <- c(
  "#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7",
  "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD",
  "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D",
  "#8A7C64", "#599861", "#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7",
  "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD",
  "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D",
  "#8A7C64", "#599861"
)

#reorder groups by dominance to match gjam output 
rhosall<-mutate(rhosall,group = factor(group, 
      levels=c( "DOM", "SUBDOM", "MODERATE", "RARE")))

#Fig 2
a<- ggplot(subset(rhosall,treat=="XXX"), aes(y=Estimate, x=group, color=group))+ 
  geom_point( )+
  geom_errorbar(aes(ymin=CI_025, ymax=CI_975), width=.2, position="dodge")+
  #geom_errorbar(aes(ymin=Estimate-(1.96*SE), ymax=Estimate+(1.96*SE)), width=.2, position="dodge")+
  facet_wrap(~enviro, scales='free')+ theme_bw()+ylab("Rho coefficients")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  ggtitle("Control")+xlab(" ")+ scale_color_manual(values = specColor)+
  geom_hline(yintercept =0, color='black', lty=2)

b<-ggplot(subset(rhosall,treat=="XXW"), aes(y=Estimate, x=group, color=group))+ 
  geom_point( )+
  geom_errorbar(aes(ymin=CI_025, ymax=CI_975), width=.2, position="dodge")+
  #geom_errorbar(aes(ymin=Estimate-(1.96*SE), ymax=Estimate+(1.96*SE)), width=.2, position="dodge")+
  facet_wrap(~enviro, scales='free')+ theme_bw()+ylab(" ")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  ggtitle("Warming")+xlab(" ")+scale_color_manual(values = specColor)+
  geom_hline(yintercept =0, color='black', lty=2)

c<-ggplot(subset(rhosall,treat=="XNW"), aes(y=Estimate, x=group, color=group))+ 
  geom_point( )+
  geom_errorbar(aes(ymin=CI_025, ymax=CI_975), width=.2, position="dodge")+
  #geom_errorbar(aes(ymin=Estimate-(1.96*SE), ymax=Estimate+(1.96*SE)), width=.2, position="dodge")+
  facet_wrap(~enviro, scales='free')+ theme_bw()+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  ggtitle("N + warming")+ xlab(" ")+ ylab("Rho coefficients")+scale_color_manual(values = specColor)+
  geom_hline(yintercept =0, color='black', lty=2)

d<-ggplot(subset(rhosall,treat=="PXW"), aes(y=Estimate, x=group, color=group))+ 
  geom_point( )+
  #geom_errorbar(aes(ymin=CI_025, ymax=CI_975), width=.2, position="dodge")+
  geom_errorbar(aes(ymin=Estimate-(1.96*SE), ymax=Estimate+(1.96*SE)), width=.2, position="dodge")+
  facet_wrap(~enviro, scales='free')+ theme_bw()+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  ggtitle("Snow + warming")+ xlab(" ")+ ylab(" ")+scale_color_manual(values = specColor)+
  geom_hline(yintercept =0, color='black', lty=2)

e<-ggplot(subset(rhosall,treat=="PNW"), aes(y=Estimate, x=group, color=group))+ 
  geom_point( )+
  #geom_errorbar(aes(ymin=CI_025, ymax=CI_975), width=.2, position="dodge")+
  geom_errorbar(aes(ymin=Estimate-(1.96*SE), ymax=Estimate+(1.96*SE)), width=.2, position="dodge")+
  facet_wrap(~enviro, scales='free')+ theme_bw()+ylab("Rho coefficients")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  ggtitle("Snow + N + warming")+ xlab(" ")+scale_color_manual(values = specColor)+
  geom_hline(yintercept =0, color='black', lty=2)

ggpubr::ggarrange(e,d, c, b, a, common.legend = TRUE,  ncol = 2, nrow = 3)


#alphas----
#XXX vs XNX
#alphasXXX<-as.data.frame(modDAtimeXXX$parameters$alphaMu)

#alphasXNX<-as.data.frame(modDAtimeXNX$parameters$alphaMu)

#delta_alpha_XNX_XXX<-as.matrix(alphasXNX-alphasXXX)

#pdf(file="plots/modDAtime_XNXplots_dom/delta_alpha_plot.pdf")
#corrplot(delta_alpha_XNX_XXX ,method = "color", 
#         tl.cex = 0.8, tl.col="black", addCoef.col = "black",
#         number.cex = 0.75, diag =T, main="delta alphas XNX-XXX", is.corr = FALSE, 
 #        mar = c(2, 2, 2, 2), cl.lim = c(-0.5, 0.5))
#dev.off()
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
#alphasPXX<-as.data.frame(modDAtimePXX$parameters$alphaMu)#

#delta_alpha_PXX_XXX<-as.matrix(alphasPXX-alphasXXX)

#pdf(file="plots/modDAtime_PXXplots_dom/delta_alpha_plot.pdf")
#corrplot(delta_alpha_PXX_XXX ,method = "color", 
#         tl.cex = 0.8, tl.col="black", addCoef.col = "black",
#         number.cex = 0.75, diag =T, main="delta alphas PXX-XXX", is.corr = FALSE, 
#         mar = c(2, 2, 2, 2), cl.lim = c(-0.5, 0.5))
#dev.off()

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
#alphasPNX<-as.data.frame(modDAtimePNX$parameters$alphaMu)

#delta_alpha_PNX_XXX<-as.matrix(alphasPNX-alphasXXX)

#pdf(file="plots/modDAtime_PNXplots_dom/delta_alpha_plot.pdf")
#corrplot(delta_alpha_PNX_XXX ,method = "color", 
#         tl.cex = 0.8, tl.col="black", addCoef.col = "black",
#         number.cex = 0.75, diag =T, main="delta alphas PNX-XXX", is.corr = FALSE, 
#         mar = c(2, 2, 2, 2), cl.lim = c(-0.5, 0.5))
#dev.off()
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


#Supp mat----
#alphas -Table S1

alphasXXX<-modDAtimeXXX$parameters$alphaTable%>%
  mutate(treat="XXX")
alphasXXW<-modDAtimeXXW$parameters$alphaTable%>%
  mutate(treat="XXW")
alphasXNW<-modDAtimeXNW$parameters$alphaTable%>%
  mutate(treat="XNW")
alphasPXW<-modDAtimePXW$parameters$alphaTable%>%
  mutate(treat="PXW")
alphasPNW<-modDAtimePNW$parameters$alphaTable%>%
  mutate(treat="PNW")
alphas_table<-rbind(alphasXXX,alphasXXW, alphasXNW, alphasPXW, alphasPNW)
write.csv(alphas_table, "supp_table_alphas.csv")

#Fig S6
#supplementary density plots 
densXXX<-as.data.frame(modDAtimeXXX$prediction$ypredMu)
densXXX$plotyear<-row.names(densXXX)
densXXW<-as.data.frame(modDAtimeXXW$prediction$ypredMu)
densXXW$plotyear<-row.names(densXXW)
densPXW<-as.data.frame(modDAtimePXW$prediction$ypredMu)
densPXW$plotyear<-row.names(densPXW)
densXNW<-as.data.frame(modDAtimeXNW$prediction$ypredMu)
densXNW$plotyear<-row.names(densXNW)
densPNW<-as.data.frame(modDAtimePNW$prediction$ypredMu)
densPNW$plotyear<-row.names(densPNW)

densXXX$treat<-"XXX"
densXXW$treat<-"XXW"
densPXW$treat<-"PXW"
densXNW$treat<-"XNW"
densPNW$treat<-"PNW"

dens<-rbind(densXXX, densXXW, densXNW, densPXW, densPNW)
dens<-separate(dens, plotyear, into = c("plot", "year"), sep = "-")
dens<-pivot_longer(dens, c("DOM", "SUBDOM", "MODERATE", "RARE"), 
                   names_to="group",values_to="ypredMu" )
dens<-mutate(dens,group = factor(group, 
                              levels=c( "DOM", "SUBDOM", "MODERATE", "RARE")))

#all years
ggplot(dens, aes(x=ypredMu, fill=group))+ geom_density(alpha=0.5)+
  facet_wrap(~treat)+ theme_bw()+scale_fill_manual(values = specColor)

#calculate final/initial ypred ratios 
densfinal<-filter(dens, year==15)%>%group_by(group, treat)%>%
  summarise(ypredMuF=mean(ypredMu))
densinit<-filter(dens, year==1)%>%group_by(group, treat)%>%
  summarise(ypredMu=mean(ypredMu))
densX<-left_join(densinit, densfinal)%>%mutate(ratiopred=ypredmuF/ypredMu)


#OLD----
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

