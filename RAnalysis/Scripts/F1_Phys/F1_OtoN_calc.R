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
# Shannon advised that the start to end was better, but then reversed this and said the rates were best
# IIIIII will go with the rates...

# Start to end resp - review the start to end R script if you want more detail on this, okay!
RR_start.end  <- read.csv(file="Output/Respiration/RR_start_end_master.csv", header=T)  %>% dplyr::select(-X) 
RR_size.ref   <- read.csv(file="Data/Physiology/Respiration/metadata/Reference_resp_size.csv", header=T) 
# Respiration rate master - uses the LoLin R package, again review my days of  work to write a tutorial 
# and guide you how this was done.
SMR_F1        <- read.csv(file="Output/Respiration/F1/F1_RR_calc_master.csv", header=T)  %>% dplyr::select(-X) 
SMR_F2        <- read.csv(file="Output/Respiration/F2/F2_RR_calc_master.csv", header=T)  %>% dplyr::select(-X) 
# note: this data file has Start.End_RR_mgO2hr - already accounting for the blank start end O2 consumption!
ER_F1         <- read.csv(file="Output/ExcretionRates/F1/F1_ExcretionRates_master.csv", header=T) %>% dplyr::select(-X) %>% 
                          dplyr::mutate(Age = case_when(Date == "20211026" ~  92,
                                                        Date == "20220202" ~  191,
                                                        Date == "20220301" ~  218,
                                                        Date == "20220922" ~  423,
                                                        Date == "20221026" ~  457)) # %>% #edit this!
ER_F2         <- read.csv(file="Output/ExcretionRates/F2/F2_ExcretionRates_master.csv", header=T) %>% dplyr::select(-X) %>% 
                        dplyr::mutate(Age = case_when(Date == "20221116" ~  111,
                                                      Date == "20230131" ~  169,
                                                      Date == "20230223" ~  192,
                                                      Date == "20230327" ~  224))

# NOTE: we have TDW correction for ER because this is the gold stanard, we calculated a unique b factor 
# for both ER and SMR using data from ALL individuals meausred and corrected to the mean TDW, 
# we report the shell length b factor fhr SMR when reported alone BCUS it is represented by ALL datapoints, 
# condiering that were are many that did not have a TDW associated with the measurement. 
# FOR EVERY ER MEASUREMENT WE HAVE A SMR MEASUREMENT - WE ALSO HAVE TDW FOR ALL! 

# look here to see what i mean!
# how many datapoints do we have for ER
nrow(ER_F1) # 78 total
nrow(ER_F2) # 82 total
# now for SMr 
(nrow(SMR_F1)) # 136 - see what I mean! 
unique(ER_F1$Date) # 20211026 20220202 20220301 20220922 20221026
unique(ER_F2$Date) # 20221116 20230131 20230223 20230327
# we need to clean up SMR datasets because the Date format will not allow us to merge with ER
SMR_F1_sub <- SMR_F1 %>% dplyr::mutate(Date = paste("20",(format(as.Date(Date, "%m/%d/%Y"), "%y%m%d")), sep ='')) %>% # change format of the date to the format in Excretion_data
                         dplyr::filter(Date %in% c('20211026', '20220202', '20220301', '20220922', '20221026')) %>% 
                         dplyr::filter(filetype %in% 'LoLigo_data') %>% # ommits all microplate SDR dish data, in which ER MEASUREMENTS WERE NOT COMPLETED!!!!
                         dplyr::mutate(Replicate = gsub(".*_","",Chamber_tank)) %>% 
                         dplyr::select(-(c(filetype, Length_um, pCO2,Dry_Tissue_weight, whole_Dry_weight))) %>%  # do not need it anymore!
                          dplyr::select(c('Date','pH','Replicate','Number', 'Run','Length_mm',
                                          'resp_umol_hr'))
nrow(SMR_F1_sub) # 77 - we have one value less than ER due to a bad datapoint


SMR_F2_sub <- SMR_F2 %>% dplyr::mutate(Date = paste("20",(format(as.Date(Date, "%m/%d/%Y"), "%y%m%d")), sep ='')) %>% # change format of the date to the format in Excretion_data
                          dplyr::filter(Date %in% c('20221116', '20230131', '20230223', '20230327')) %>% 
                          dplyr::mutate(Replicate = gsub(".*_","",Chamber_tank)) %>% 
                          dplyr::select(-(c(Dry_Tissue_weight, whole_Dry_weight))) %>%  # do not need it anymore!
                          dplyr::select(c('Date','pH','Replicate','Number', 'Run', 'Length_um',
                                         'resp_umol_hr'))
nrow(SMR_F2_sub) # 84 



# now when we merge, ER is our limiting factor 
F1_ON_Master<- merge(ER_F1,SMR_F1_sub, by=c('Date','pH',  'Replicate','Number', 'Run')) %>% 
                  dplyr::select(-c(Run,Biovolume_g_in_sw,Dry_Shell_weight,whole_Dry_weight,log10_VER,log10_TDW))
nrow(F1_ON_Master) # 74 aligned - 4 less than ER master file 


F2_ON_Master <- merge(ER_F2,SMR_F2_sub, by=c('Date','pH','Replicate','Number', 'Length_um')) %>% 
                  dplyr::select(-c(Run.x,Run.y,Biovolume_g_in_sw,Dry_Shell_weight,whole_Dry_weight,log10_VER,log10_TDW))
nrow(F2_ON_Master) # 82 aligned - exact with the master file


# F1 Plots ::::::::::::::::::::::::::
library(forcats)
F1_ON_Master <- F1_ON_Master %>% 
                  dplyr::mutate(O_N =(resp_umol_hr)/ # for umol of oxygen, curretnly as O2, convert to O 
                                  ExcretionRate_umol_hr) %>% 
                  dplyr::filter(!O_N >200) %>% # outlier omit
                  dplyr::mutate(pCO2 = factor(pCO2, levels=c("500 μatm", "800 μatm")))

# View(F1_ON_Master %>% dplyr::select(Date,pH, Replicate,resp_umol_hr, ExcretionRate_umol_hr, O_N))

write.csv(F1_ON_Master,
          "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/OxygenNitrogen_ratio/F1/F1_ON_master.csv")


F1_ON_MasterMEANS <- F1_ON_Master %>% # mean by tank replicate 
                        dplyr::select(c(Age, pCO2,  Replicate, O_N)) %>% # one extreme outlier value!
                        Rmisc::summarySE(measurevar="O_N", 
                                  groupvars=c("Age", "pCO2",  "Replicate"))

write.csv(F1_ON_MasterMEANS,
          "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/OxygenNitrogen_ratio/F1/F1_ON_master_means.csv")


F1_Boxplot <- F1_ON_MasterMEANS %>% 
                        #dplyr::filter(!(Date %in% '20220301')) %>% # two outliers?
                      ggplot(aes(x=as.factor(Age), 
                                 y=O_N, 
                                 color=as.factor(pCO2))) +
                          geom_boxplot(alpha = 0.5, # color hue
                                       width=0.6, # boxplot width
                                       outlier.size=0, # make outliers small
                                       position = position_dodge(preserve = "single")) + 
                          geom_point(pch = 19, 
                                     position = position_jitterdodge(0.01), 
                                     size=1) +
                          scale_color_manual(values=c("forestgreen","orange")) +
                          theme_classic() + 
                          ggtitle("O:N, F1 Scallops (rep av'd)") +
                          theme(legend.position="none",
                                axis.title.y=element_text(size=7),
                                axis.title.x=element_text(size=7),
                                axis.text.x=element_text(size=7)) +
                          scale_y_continuous(name ="O:N (umol TDW b factor)",expand = c(0, 0), limits = c(0, NA)) +
                          stat_summary(fun.y=mean, 
                                       geom = "errorbar", 
                                       aes(ymax = ..y.., ymin = ..y..), 
                                       width = 0.6, 
                                       size=0.4, 
                                       linetype = "dashed", 
                                       position = position_dodge(preserve = "single")) 
F1_Boxplot

F1_ON_MasterMEANSMEANS <- F1_ON_MasterMEANS %>% # mean by tank replicate 
                          dplyr::select(c(Age, pCO2, O_N)) %>% # one extreme outlier value!
                          Rmisc::summarySE(measurevar="O_N", 
                                    groupvars=c("Age", "pCO2"))

F1_ON_Plot <- F1_ON_MasterMEANSMEANS %>% 
                      ggplot(aes(x=as.factor(Age), 
                                 y=O_N, 
                                 color=as.factor(pCO2))) +
                      geom_point(position=position_dodge(.5))+ 
                      scale_color_manual(values=c("forestgreen",
                                                  "darkorange2"))+
                      geom_errorbar(aes(ymin=O_N-se, 
                                        ymax=O_N+se), width=.2,
                                    position=position_dodge(.5))+
                      theme_classic() +  
                      xlab("Age (dpf)") + 
                      ggtitle("O:N, F1 Scallops (mean +- SE; rep av'd)") +
                      theme_classic() +
                      theme(legend.position="none",
                            panel.grid.major = element_blank(), 
                            panel.grid.minor = element_blank())+ 
                      geom_line(stat = "identity", size=1.0)+
                      scale_y_continuous(name ="O:N (umol TDW b factor)",expand = c(0, 0), limits = c(0, NA)) +
                      theme(text = element_text(size=10))
# F1_ON_Plot

library(ggpubr)
ggarrange(F1_Boxplot, F1_ON_Plot, ncol = 1)

pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/OxygenNitrogen_ratio/F1/F1_ON_TDWbfactor.pdf"), width = 8, height = 8)
ggarrange(F1_Boxplot, F1_ON_Plot, ncol = 1)
dev.off()

# F2 Plots :::::::::::::::::::::::::

F2_ON_Master <- F2_ON_Master %>% 
                    dplyr::mutate(O_N =(resp_umol_hr)/ # for umol of oxygen, curretnly as O2, convert to O 
                                    ExcretionRate_umol_hr) %>% 
                    dplyr::mutate(pCO2 = factor(pCO2, levels=c("500 μatm", "800 μatm", "1200 μatm")))

write.csv(F2_ON_Master,
          "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/OxygenNitrogen_ratio/F2/F2_ON_master.csv")


F2_ON_MasterMEANS <- F2_ON_Master %>% # mean by tank replicate 
                      dplyr::filter(!O_N>200) %>% 
                      dplyr::select(c(Date, pCO2,  Replicate, O_N)) %>% # one extreme outlier value!
                      Rmisc::summarySE(measurevar="O_N", 
                                groupvars=c("Date", "pCO2",  "Replicate"))

write.csv(F2_ON_MasterMEANS,
          "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/OxygenNitrogen_ratio/F2/F2_ON_master_means.csv")


F2_Boxplot <- F2_ON_MasterMEANS %>% 
                  # dplyr::filter(!(Date %in% '20230131' & O_N >100)) %>% # two outliers?
                  ggplot(aes(x=as.factor(Date), 
                             y=O_N, 
                             color=as.factor(pCO2))) +
                  geom_boxplot(alpha = 0.5, # color hue
                               width=0.6, # boxplot width
                               outlier.size=0, # make outliers small
                               position = position_dodge(preserve = "single")) + 
                  geom_point(pch = 19, 
                             position = position_jitterdodge(0.01), 
                             size=1) +
                  scale_color_manual(values=c("forestgreen","orange", "purple")) +
                  theme_classic() + 
                  ggtitle("O:N, F2 Scallops (rep av'd)") +
                  theme(legend.position="none",
                        axis.title.y=element_text(size=7),
                        axis.title.x=element_text(size=7),
                        axis.text.x=element_text(size=7)) +
                  scale_y_continuous(name ="O:N (umol hr)",expand = c(0, 0), limits = c(0, NA)) +
                  stat_summary(fun.y=mean, 
                               geom = "errorbar", 
                               aes(ymax = ..y.., ymin = ..y..), 
                               width = 0.6, 
                               size=0.4, 
                               linetype = "dashed", 
                               position = position_dodge(preserve = "single")) 
F2_Boxplot

F2_ON_MasterMEANSMEANS <- F2_ON_MasterMEANS %>% # mean by tank replicate 
                            # dplyr::filter(!(Date %in% '20230131' & O_N >100)) %>% # two outliers?
                            dplyr::select(c(Date, pCO2, O_N)) %>% # one extreme outlier value!
                            Rmisc::summarySE(measurevar="O_N", 
                                      groupvars=c("Date", "pCO2"))

F2_ON_Plot <- F2_ON_MasterMEANSMEANS %>% 
                            ggplot(aes(x=as.factor(Date), 
                                       y=O_N, 
                                       color=as.factor(pCO2))) +
                            geom_point(position=position_dodge(.5))+ 
                            scale_color_manual(values=c("forestgreen",
                                                        "darkorange2",
                                                        "purple"))+
                            geom_errorbar(aes(ymin=O_N-se, 
                                              ymax=O_N+se), width=.2,
                                          position=position_dodge(.5))+
                            theme_classic() +  
                            xlab("Age (dpf)") + 
                            ggtitle("O:N, F2 Scallops (umol hr)") +
                            theme_classic() +
                            theme(legend.position="none",
                                  panel.grid.major = element_blank(), 
                                  panel.grid.minor = element_blank())+ 
                            geom_line(stat = "identity", size=1.0)+
                            scale_y_continuous(name ="O:N (umol hr)",expand = c(0, 0), limits = c(0, NA)) +
                            theme(text = element_text(size=10))
F2_ON_Plot


ggarrange(F2_Boxplot, F2_ON_Plot, ncol = 1)

pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/OxygenNitrogen_ratio/F2/F2_ON_TDWbfactor.pdf"), width = 8, height = 8)
ggarrange(F2_Boxplot, F2_ON_Plot, ncol = 1)
dev.off()
