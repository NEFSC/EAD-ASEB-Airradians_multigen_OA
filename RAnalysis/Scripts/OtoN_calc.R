# Purpose: Bay Scallop Project - O:N ratio calculation
# use the resp rate from LoLinr and from start-to-end (simplified) to estimate O:N - output master file

# Written by: Sam J Gurr (last edit 2/17/2022)

# LOAD PACKAGES ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
library(devtools) # devtools::install_github # use devtools to instlal github link
library(LoLinR) # install_github('colin-olito/LoLinR') # install LoLinR from github
library(dplyr)
library(lubridate)
library(rMR) 
library(dplyr)
library(stringr)
library(ggplot2)

# SET WORKING DIRECTORY :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis")


# LOAD DATA ::::::::::::::::::::::::::::::::::::::::::::::::::::
RR_start.end  <- read.csv(file="Output/Respiration/RR_start_end_master.csv", header=T)  %>% dplyr::select(-X) 
RR_size.ref   <- read.csv(file="Data/Physiology/Respiration/Reference_resp_size.csv", header=T) 
# note: this data file has Start.End_RR_mgO2hr - already accounting for the blank start end O2 consumption!
ER            <- read.csv(file="Output/ExcretionRates/ExcretionRates_master.csv", header=T) %>% dplyr::select(-X)




#  edit data  ::::::::::::::::::::::::::::::::::::::
# note: start end Oxygen consumption needs to be filtered for dates/measurments used for excretion
unique(ER$Date) # 20211026 20220202 20220301 20220922 20221026 - also ONLY Liligo measurements (large indivs) have excretion data (and biodep!)
unique(RR_start.end$Date) # "10/26/2021" "10/26/2022" "11/16/2022" "2/2/2022"   "3/1/2022"   "9/14/2021"  "9/22/2022"  "9/30/2021"
unique(RR_start.end_2$Date) 

RR_start.end_2 %>% dplyr::filter(Date %in% '20221026')
unique(RR_start.end_2$Date)

RR_start.end_2 <- RR_start.end %>% 
                      dplyr::mutate(Date = paste("20",(format(as.Date(Date, "%m/%d/%Y"), "%y%m%d")), sep ='')) %>% # change format of the date to the format in Excretion_data
                      dplyr::filter(Date %in% c('20211026', '20220202', '20220301', '20220922', '20221026')) %>% 
                      dplyr::filter(filetype %in% 'LoLigo_data') %>% 
                      dplyr::mutate(Replicate = gsub(".*_","",Chamber_tank)) %>% 
                      dplyr::select(-(c(filetype, Length_um, Dry_Tissue_weight_mg, Whole_Dry_weight_mg))) # do not need it anymore!

nrow(RR_start.end_2)
nrow(ER)

merge(RR_start.end_2 , ER, by = c('Date','Chamber_tank','Number', 'Run', 'pH'))
View(RR_start.end_2)
View(ER)
# Respiration plot (start end rates!) 

RR_startend_boxplot <- RR_start.end_2 %>% 
                            #dplyr::filter(!ExcretionRate_umol_mL_hr_TDWbfactor > 30) %>% # two outliers?
                            ggplot(aes(x = factor(Date), 
                                      # y = Start.End_RR_umolhr, 
                                       y = Start.End_RR_umolhr_biovolcalc,
                                       fill = pCO2)) +
                            geom_boxplot(alpha = 0.5, # color hue
                                         width=0.6, # boxplot width
                                         outlier.size=0, # make outliers small
                                         position = position_dodge(preserve = "single")) + 
                            geom_point(pch = 19, 
                                       position = position_jitterdodge(0.01), 
                                       size=1) +
                            scale_fill_manual(values=c("forestgreen","orange")) +
                            theme_classic() + 
                            ggtitle("F1 Scallops: Respiration rate (start end; umol O2 hr)") +
                            theme(legend.position="none",
                                  axis.title.y=element_text(size=7),
                                  axis.title.x=element_text(size=7),
                                  axis.text.x=element_text(size=7)) +
                            #ylim(0, 0.2) +
                            stat_summary(fun.y=mean, 
                                         geom = "errorbar", 
                                         aes(ymax = ..y.., ymin = ..y..), 
                                         width = 0.6, 
                                         size=0.4, 
                                         linetype = "dashed", 
                                         position = position_dodge(preserve = "single"))  +
                            facet_wrap(~Date, scales = "free")

# Excretion plot

ER_boxplot <- ER %>% 
                            #dplyr::filter(!ExcretionRate_umol_mL_hr_TDWbfactor > 30) %>% # two outliers?
                            ggplot(aes(x = factor(Date), 
                                       y = ExcretionRate_umol_mL_hr, 
                                       fill = pCO2)) +
                            geom_boxplot(alpha = 0.5, # color hue
                                         width=0.6, # boxplot width
                                         outlier.size=0, # make outliers small
                                         position = position_dodge(preserve = "single")) + 
                            geom_point(pch = 19, 
                                       position = position_jitterdodge(0.01), 
                                       size=1) +
                            scale_fill_manual(values=c("forestgreen","orange")) +
                            theme_classic() + 
                            ggtitle("F1 Scallops: Excretion rate (umol O2 hr)") +
                            theme(legend.position="none",
                                  axis.title.y=element_text(size=7),
                                  axis.title.x=element_text(size=7),
                                  axis.text.x=element_text(size=7)) +
                            #ylim(0, 0.2) +
                            stat_summary(fun.y=mean, 
                                         geom = "errorbar", 
                                         aes(ymax = ..y.., ymin = ..y..), 
                                         width = 0.6, 
                                         size=0.4, 
                                         linetype = "dashed", 
                                         position = position_dodge(preserve = "single"))  +
                            facet_wrap(~Date, scales = "free")

ER_boxplot


O_N_Master <- merge(RR_start.end_2,ER)  %>% 
  dplyr::mutate(ExcretionRate_umol_L_hr_TDWbfactor =  ExcretionRate_umol_mL_hr*( (meanTDW/(as.numeric(Dry_Tissue_weight)))^0.822) ) %>% 
  dplyr::mutate(RR_umol_L_hr_TDWbfactor =  Start.End_RR_umolhr*( (meanTDW/(as.numeric(Dry_Tissue_weight)))^0.822) ) %>% 
  dplyr::mutate(O_N =Start.End_RR_umolhr/ ExcretionRate_umol_mL_hr)
  
nrow(O_N_Master) # 75
unique(O_N_Master$Date)
meanTDW <- mean(O_N_Master$Dry_Tissue_weight) # 0.284344
O_N_Master_bfactTDW %>% dplyr::filter(Date %in% '20221026')




O_N_facetted <- O_N_Master %>% 
  #dplyr::filter(!ExcretionRate_umol_mL_hr_TDWbfactor > 30) %>% # two outliers?
  ggplot(aes(x = factor(Date), 
             y = O_N, 
             fill = pCO2)) +
  geom_boxplot(alpha = 0.5, # color hue
               width=0.6, # boxplot width
               outlier.size=0, # make outliers small
               position = position_dodge(preserve = "single")) + 
  geom_point(pch = 19, 
             position = position_jitterdodge(0.01), 
             size=1) +
  scale_fill_manual(values=c("forestgreen","orange")) +
  theme_classic() + 
  ggtitle("O:N, F1 Scallops") +
  theme(legend.position="none",
        axis.title.y=element_text(size=7),
        axis.title.x=element_text(size=7),
        axis.text.x=element_text(size=7)) +
  #ylim(0, 0.2) +
  stat_summary(fun.y=mean, 
               geom = "errorbar", 
               aes(ymax = ..y.., ymin = ..y..), 
               width = 0.6, 
               size=0.4, 
               linetype = "dashed", 
               position = position_dodge(preserve = "single"))  +
  facet_wrap(~Date, scales = "free")


O_N  <- O_N_Master_bfactTDW %>% 
  #dplyr::filter(!ExcretionRate_umol_mL_hr_TDWbfactor > 30) %>% # two outliers?
  ggplot(aes(x = factor(Date), 
             y = O_N, 
             fill = pCO2)) +
  geom_boxplot(alpha = 0.5, # color hue
               width=0.6, # boxplot width
               outlier.size=0, # make outliers small
               position = position_dodge(preserve = "single")) + 
  geom_point(pch = 19, 
             position = position_jitterdodge(0.01), 
             size=1) +
  scale_fill_manual(values=c("forestgreen","orange")) +
  theme_classic() + 
  ggtitle("O:N, F1 Scallops") +
  theme(legend.position="none",
        axis.title.y=element_text(size=7),
        axis.title.x=element_text(size=7),
        axis.text.x=element_text(size=7)) +
  #ylim(0, 0.2) +
  stat_summary(fun.y=mean, 
               geom = "errorbar", 
               aes(ymax = ..y.., ymin = ..y..), 
               width = 0.6, 
               size=0.4, 
               linetype = "dashed", 
               position = position_dodge(preserve = "single"))
library(ggpubr)
ggarrange(O_N_facetted, O_N, ncol = 1, nrow = 2)

Excretion_rate <- Excretion_master %>% 
  dplyr::filter(!ExcretionRate_umol_mL_hr_TDWbfactor > 30) %>% # two outliers?
  ggplot(aes(x = factor(Date), 
             y = ExcretionRate_umol_mL_hr_TDWbfactor, 
             fill = pCO2)) +
  geom_boxplot(alpha = 0.5, # color hue
               width=0.6, # boxplot width
               outlier.size=0, # make outliers small
               position = position_dodge(preserve = "single")) + 
  geom_point(pch = 19, 
             position = position_jitterdodge(0.01), 
             size=1) +
  scale_fill_manual(values=c("forestgreen","orange")) +
  theme_classic() + 
  theme(legend.position="none",
        axis.title.y=element_text(size=7),
        axis.title.x=element_text(size=7),
        axis.text.x=element_text(size=7)) +
  #ylim(0, 0.2) +
  stat_summary(fun.y=mean, 
               geom = "errorbar", 
               aes(ymax = ..y.., ymin = ..y..), 
               width = 0.6, 
               size=0.4, 
               linetype = "dashed", 
               position = position_dodge(preserve = "single")) # no facet
