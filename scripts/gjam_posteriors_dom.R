library(dplyr)
library(bayestestR)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(corrplot)
library(bayestestR)

#rhos---- 
#XXX
#load(file = "outputs/modDAtime_XXXoutput_dom.RData")
rhosXXX<-as.data.frame(modDAtimeXXX$parameters$rhoStandXTable)
rhosXXX<-separate(rhosXXX, 'rho_{to, from}', c("group", "enviro"))                       
rhosXXX$treat<-"XXX"


#XXW
#load(file = "outputs/modDAtime_XXWoutput_dom.RData")
rhosXXW<-as.data.frame(modDAtimeXXW$parameters$rhoStandXTable)
rhosXXW<-separate(rhosXXW, 'rho_{to, from}', c("group", "enviro"))                       
rhosXXW$treat<-"XXW"

#XNW
#load(file = "outputs/modDAtime_XNWoutput_dom.RData")
rhosXNW<-as.data.frame(modDAtimeXNW$parameters$rhoStandXTable)
rhosXNW<-separate(rhosXNW, 'rho_{to, from}', c("group", "enviro"))                       
rhosXNW$treat<-"XNW"

#PXW
#load(file = "outputs/modDAtime_PXWoutput_dom.RData")
rhosPXW<-as.data.frame(modDAtimePXW$parameters$rhoStandXTable)
rhosPXW<-separate(rhosPXW, 'rho_{to, from}', c("group", "enviro"))                       
rhosPXW$treat<-"PXW"

#PNW
#load(file = "outputs/modDAtime_PNWoutput_dom.RData")
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
color2<-c("#A98FBA", "#AA865A",  "#74870D", "#1C829D", "#3F4921")

#reorder groups by dominance to match gjam output 
rhosall<-mutate(rhosall,group = factor(group, 
      levels=c( "DOM", "SUBDOM", "MODERATE", "RARE")))
#table S1
#write.csv(rhosall, "tables/supp_table_rhos.csv") 

#Fig 2 

a<- ggplot(subset(rhosall,group=="DOM"), aes(y=Estimate, x=treat, color=treat))+ 
  geom_point( )+
  geom_pointrange(aes(ymin=CI_025, ymax=CI_975), width=.2, position="dodge")+
  #geom_errorbar(aes(ymin=Estimate-(1.96*SE), ymax=Estimate+(1.96*SE)), width=.2, position="dodge")+
  facet_wrap(~enviro)+ ylab("Rho coefficients")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  ggtitle("Dominant")+xlab(" ")+ scale_color_manual(values = color2)+
  geom_hline(yintercept =0, color='black', lty=2)+ theme_bw()+
  theme(strip.background = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())


b<-ggplot(subset(rhosall,group=="SUBDOM"), aes(y=Estimate, x=treat, color=treat))+ 
  geom_point( )+
  geom_pointrange(aes(ymin=CI_025, ymax=CI_975), width=.2, position="dodge")+
  #geom_errorbar(aes(ymin=Estimate-(1.96*SE), ymax=Estimate+(1.96*SE)), width=.2, position="dodge")+
  facet_wrap(~enviro)+ylab("Rho coefficients")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  ggtitle("Subdominant")+xlab(" ")+ scale_color_manual(values = color2)+
  geom_hline(yintercept =0, color='black', lty=2) + theme_bw()+
  theme(strip.background = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

c<-ggplot(subset(rhosall,group=="MODERATE"), aes(y=Estimate, x=treat, color=treat))+ 
  geom_point( )+
  geom_pointrange(aes(ymin=CI_025, ymax=CI_975), position="dodge")+
  #geom_errorbar(aes(ymin=Estimate-(1.96*SE), ymax=Estimate+(1.96*SE)), width=.2, position="dodge")+
  facet_wrap(~enviro)+ylab("Rho coefficients")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  ggtitle("Moderate")+xlab(" ")+ scale_color_manual(values = color2)+
  geom_hline(yintercept =0, color='black', lty=2) + theme_bw()+
  theme(strip.background = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())


d<-ggplot(subset(rhosall,group=="RARE"), aes(y=Estimate, x=treat, color=treat))+ 
  geom_point( )+
  geom_pointrange(aes(ymin=CI_025, ymax=CI_975), width=.2, position="dodge")+
  #geom_errorbar(aes(ymin=Estimate-(1.96*SE), ymax=Estimate+(1.96*SE)), width=.2, position="dodge")+
  facet_wrap(~enviro)+ylab("Rho coefficients")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  ggtitle("Rare")+xlab(" ")+ scale_color_manual(values = color2)+
  geom_hline(yintercept =0, color='black', lty=2)+ theme_bw()+
  theme(strip.background = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())


#Fig 2 FINAL 
ggpubr::ggarrange(a, b, c, common.legend = TRUE,  ncol =1, nrow = 3)+
  theme(plot.margin = margin(1,0.25,0.25,1, "cm")) 

#Fig S7-rare spp 
d




#alphas----
#Fig S8 plots 
#XXX vs XNX
alphasXXX<-as.data.frame(modDAtimeXXX$parameters$alphaMu)

#XXX vs XXW (Fig S8d)
alphasXXW<-as.data.frame(modDAtimeXXW$parameters$alphaMu)

delta_alpha_XXW_XXX<-as.matrix(alphasXXW-alphasXXX)

#pdf(file="plots/modDAtime_XXWplots_dom/delta_alpha_plot.pdf")
corrplot(delta_alpha_XXW_XXX ,method = 'circle', 
         tl.cex = 0.8, tl.col="black", addCoef.col = "black",
         number.cex = 0.75, diag =T, main="delta alphas XXW-XXX", is.corr = FALSE, 
         mar = c(2, 2, 2, 2), cl.lim = c(-0.5, 0.5))
#dev.off()


#XXX vs XNW (Fig S8c)
alphasXNW<-as.data.frame(modDAtimeXNW$parameters$alphaMu)

delta_alpha_XNW_XXX<-as.matrix(alphasXNW-alphasXXX)

#pdf(file="plots/modDAtime_XNWplots_dom/delta_alpha_plot.pdf")
corrplot(delta_alpha_XNW_XXX ,method = "circle", 
         tl.cex = 0.8, tl.col="black", addCoef.col = "black",
         number.cex = 0.75, diag =T, main="delta alphas XNW-XXX", is.corr = FALSE, 
         mar = c(2, 2, 2, 2), cl.lim = c(-0.5, 0.5))
#dev.off()

#XXX vs PXW (Fig S8b)
alphasPXW<-as.data.frame(modDAtimePXW$parameters$alphaMu)

delta_alpha_PXW_XXX<-as.matrix(alphasPXW-alphasXXX)

#pdf(file="plots/modDAtime_PXWplots_dom/delta_alpha_plot.pdf")
corrplot(delta_alpha_PXW_XXX ,method = "circle", 
         tl.cex = 0.8, tl.col="black", addCoef.col = "black",
         number.cex = 0.75, diag =T, main="delta alphas PXW-XXX", is.corr = FALSE, 
         mar = c(2, 2, 2, 2), cl.lim = c(-0.5, 0.5))
#dev.off()

#XXX vs PNW (Fig S8a)
alphasPNW<-as.data.frame(modDAtimePNW$parameters$alphaMu)

delta_alpha_PNW_XXX<-as.matrix(alphasPNW-alphasXXX)

#pdf(file="plots/modDAtime_PNWplots_dom/delta_alpha_plot.pdf")
corrplot(delta_alpha_PNW_XXX ,method = "circle", 
         tl.cex = 0.8, tl.col="black", addCoef.col = "black",
         number.cex = 0.75, diag =T, main="delta alphas PNW-XXX", is.corr = FALSE, 
         mar = c(2, 2, 2, 2), cl.lim = c(-0.5, 0.5))
#dev.off()


#calculations for delta alpha bar plots (Fig 3)
#XXW
calcxxw<-matrix(NA, 4,3)
row.names(calcxxw)<-row.names(delta_alpha_XXW_XXX)
calcxxw<-as.data.frame(calcxxw)
calcxxw<-rename(calcxxw,Intra=V1, InterRes=V2, InterEff=V3)

calcxxw$Intra<-diag(delta_alpha_XXW_XXX)
calcxxw$InterRes<-(rowSums(delta_alpha_XXW_XXX)-calcxxw$Intra)
calcxxw$InterEff<-(colSums(delta_alpha_XXW_XXX)-calcxxw$Intra)
calcxxw$Net<-calcxxw$Intra+calcxxw$InterRes
calcxxw$treat<-"XXW"
calcxxw$group<-row.names(calcxxw)

#XNW
calcxnw<-matrix(NA, 4,3)
row.names(calcxnw)<-row.names(delta_alpha_XNW_XXX)
calcxnw<-as.data.frame(calcxnw)
calcxnw<-rename(calcxnw,Intra=V1, InterRes=V2, InterEff=V3)

calcxnw$Intra<-diag(delta_alpha_XNW_XXX)
calcxnw$InterRes<-(rowSums(delta_alpha_XNW_XXX)-calcxnw$Intra)
calcxnw$InterEff<-(colSums(delta_alpha_XNW_XXX)-calcxnw$Intra)
calcxnw$Net<-calcxnw$Intra+calcxnw$InterRes
calcxnw$treat<-"XNW"
calcxnw$group<-row.names(calcxnw)

#PXW
calcpxw<-matrix(NA, 4,3)
row.names(calcpxw)<-row.names(delta_alpha_PXW_XXX)
calcpxw<-as.data.frame(calcpxw)
calcpxw<-rename(calcpxw,Intra=V1, InterRes=V2, InterEff=V3)

calcpxw$Intra<-diag(delta_alpha_PXW_XXX)
calcpxw$InterRes<-(rowSums(delta_alpha_PXW_XXX)-calcpxw$Intra)
calcpxw$InterEff<-(colSums(delta_alpha_PXW_XXX)-calcpxw$Intra)
calcpxw$Net<-calcpxw$Intra+calcpxw$InterRes
calcpxw$treat<-"PXW"
calcpxw$group<-row.names(calcpxw)

#PNW
calcpnw<-matrix(NA, 4,3)
row.names(calcpnw)<-row.names(delta_alpha_PNW_XXX)
calcpnw<-as.data.frame(calcpnw)
calcpnw<-rename(calcpnw,Intra=V1, InterRes=V2, InterEff=V3)

calcpnw$Intra<-diag(delta_alpha_PNW_XXX)
calcpnw$InterRes<-(rowSums(delta_alpha_PNW_XXX)-calcpnw$Intra)
calcpnw$InterEff<-(colSums(delta_alpha_PNW_XXX)-calcpnw$Intra)
calcpnw$Net<-calcpnw$Intra+calcpnw$InterRes
calcpnw$treat<-"PNW"
calcpnw$group<-row.names(calcpnw)

calc<-rbind(calcxxw, calcxnw, calcpxw, calcpnw)
calc<-pivot_longer(calc, cols = c("Intra", "InterRes", "InterEff", "Net"),names_to = "comp", values_to = "value")

calc<-mutate(calc,group = factor(group, 
                              levels=c( "DOM", "SUBDOM", "MODERATE", "RARE")))%>%
  mutate(treat=factor(treat, 
                 levels=c( "XXW", "XNW", "PXW", "PNW")))

#Fig 3
ggplot(subset(calc, comp!="InterEff"), aes(x = comp, y = value, fill = treat)) +
  geom_bar(stat = "identity",position="dodge", col = "black",alpha=0.5, width=0.7)+
  geom_hline(yintercept = 0, lty=2)+
  coord_flip() +   facet_wrap(~group)+   #   theme(aspect.ratio = 1/1.25)+
  ylab("Change in competition from control")+ xlab(" ")+
  scale_fill_manual(values = c("#1C829D", "#74870D", "#AA865A", "#A98FBA"))+  #keep order from Fig 2
  theme_classic()+  theme(plot.margin = margin(1,0,0.5,1, "cm")) +
  theme(strip.background = element_blank())


#alphas -Table S2
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
#write.csv(alphas_table, "tables/supp_table_alphas.csv")


