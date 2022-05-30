# NiwotITEX_gjam
Plant community analyses from long term experimental manipulation plots at Niwot Ridge LTER 

Repo structure as follows 
Data<-Raw environmental (snow depth, N deposition, air temp) and plant abundance (plot cover) data.

Scripts
1. enviro_data.R<- pulls in raw environmental data (raw_env folder) in data folder, processes and saves as data object Enviro.Rdata
2. abundance_change.R <-pulls in raw plant cover data and runs codyn functions and linear mixed effects models (generates Table 1 and Figure 1)
3. gjamTime_setup.R<-combined processed enviro data with plant cover data (NWT_ITEX_SpComp_data_L1.csv) and sets up data obejcts needed to run gjamTime. 
4. gjamTime_XXX_dom.R<-sources setup script (#2 above), formats data and runs gjamTime model and steady state abundance simulations. Separate script for each treatment named accordingly (XXX-CTL, XXW-warming, XNW-N + warming, PXW- snow + warming, PNW-snow + N + warming). 
5. 
