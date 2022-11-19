# ---
# title: "Dry weights and condition"
# author: "Samuel Gurr"
# date: "11/18/2022"
# ---

# LOAD PACKAGES
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(tidyverse)
library(car)
library(nlme)
library(Rmisc)
library(lmerTest)
library(lme4)
library(pander)
library(performance)
library(Rmisc)
library(reshape2)
library(ggpubr)

# SET WORKING DIRECTORY 
setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis") # personal computer


# LOAD DATA & cater to this script 

# F1s
F1_dryweights <- read.csv(file="Data/Physiology/Dry_weights/F1/cumulative_raw/F1_dry_weights_raw.csv", header=T,stringsAsFactors=FALSE, fileEncoding="latin1") %>% # master data file
  dplyr::mutate(pCO2 = case_when(pH == 8 ~ "500 μatm", 
                                 pH == 7.5 ~ "800 μatm")) %>% 
  dplyr::mutate(Age = case_when(Date_sampled == "10/26/2021" ~  92,  # ran unique Date to call these sampling dates, the lengths files, spawn was 7.27.2021
                                Date_sampled == "12/2/2021" ~ 129,
                                Date_sampled == "2/2/2022" ~ 191,
                                Date_sampled == "2/28/2022" ~ 217,
                                Date_sampled == "3/2/2022" ~ 219,
                                Date_sampled == "3/28/2022" ~ 245))

# F2s
F2_dryweights <- read.csv(file="Data/Physiology/Dry_weights/F2/cumulative_raw/F2_dry_weights_raw.csv", header=T,stringsAsFactors=FALSE, fileEncoding="latin1") %>%  # master data file 
  dplyr::mutate(pCO2 = case_when(pH == 8 ~ "500 μatm",  
                                 pH == 7.5 ~ "800 μatm", 
                                 pH == 7 ~ "1200 uatm")) %>% 
  dplyr::mutate(Age = case_when(Date_sampled == "11/16/2022" ~  92)) # add more dates as we collect data, spawn was 8.16.2022


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# F1, Summary plots ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# (1) DRY SHELL WEIGHT

F1_dry_shell <- F1_dryweights %>% 
  dplyr::select(c('Age','pH','pH_Replicate', 'pCO2', 'Shell_length_mm','Dry_Shell_weight_g')) %>% 
  dplyr::filter(!Shell_length_mm == 'MEASURE THIS') %>%  # check this row, omit for now
  dplyr::filter(!Dry_Shell_weight_g < 0) %>%  # one sample with a negative value here,.. omit!
  na.omit() %>% 
  dplyr::mutate(Dry_Shell_weight_g = as.numeric(Dry_Shell_weight_g)) %>% 
  dplyr::mutate(Shell_length_mm = as.numeric(Shell_length_mm)) %>% 
  dplyr::mutate(Dry_Shell_weight_g_LENGTHnormalized = Dry_Shell_weight_g/Shell_length_mm)

# not normalized
F1_dry_shell_Summ <-summarySE(F1_dry_shell, measurevar="Dry_Shell_weight_g", groupvars=c("Age", "pCO2"))

DryShellWeight_Plot <- F1_dry_shell_Summ %>% 
  ggplot(aes(x=as.factor(Age), y=Dry_Shell_weight_g, color=as.factor(pCO2))) +
  geom_point(position=position_dodge(.5))+ 
  scale_color_manual(values=c("forestgreen","darkorange2"))+
  geom_errorbar(aes(ymin=Dry_Shell_weight_g-se, ymax=Dry_Shell_weight_g+se), width=.2,
                position=position_dodge(.5))+
  theme_classic() +  
  xlab("Age (dpf)") +
 #xlab("Age (d)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ 
  scale_y_continuous(name ="Dry shell weight (g)")+
  geom_line(stat = "identity", size=1.0)+
  theme(text = element_text(size=15))


# normaliZed to shell length

F1_dry_shell_Summ_norm <-summarySE(F1_dry_shell, measurevar="Dry_Shell_weight_g_LENGTHnormalized", groupvars=c("Age", "pCO2"))

DryShellWeight_Plot_Lengthnorm <- F1_dry_shell_Summ_norm %>% 
  ggplot(aes(x=as.factor(Age), y=Dry_Shell_weight_g_LENGTHnormalized, color=as.factor(pCO2))) +
  geom_point(position=position_dodge(.5))+ 
  scale_color_manual(values=c("forestgreen","darkorange2"))+
  geom_errorbar(aes(ymin=Dry_Shell_weight_g_LENGTHnormalized-se, ymax=Dry_Shell_weight_g_LENGTHnormalized+se), width=.2,
                position=position_dodge(.5))+
  theme_classic() +  
  xlab("Age (dpf)") +
  #xlab("Age (d)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ 
  scale_y_continuous(name ="Dry shell weight (g, length normalized)")+
  geom_line(stat = "identity", size=1.0)+
  theme(text = element_text(size=15))


# (2) DRY TISSUE WEIGHT

F1_dry_tissue <- F1_dryweights %>% 
  dplyr::select(c('Age','pH','pH_Replicate', 'pCO2', 'Shell_length_mm','Dry_Tissue_weight_g')) %>% 
  dplyr::filter(!Shell_length_mm == 'MEASURE THIS') %>%  # check this row, omit for now
  dplyr::filter(!Dry_Tissue_weight_g < 0) %>%  # one sample with a negative value here,.. omit!
  na.omit() %>% 
  dplyr::mutate(Dry_Tissue_weight_g = as.numeric(Dry_Tissue_weight_g)) %>% 
  dplyr::mutate(Shell_length_mm = as.numeric(Shell_length_mm)) %>% 
  dplyr::mutate(Dry_Tissue_weight_g_LENGTHnormalized = Dry_Tissue_weight_g/Shell_length_mm)

# not normalized

F1_dry_tissue_Summ <-summarySE(F1_dry_tissue, measurevar="Dry_Tissue_weight_g", groupvars=c("Age", "pCO2"))

DryTissueWeight_Plot <- F1_dry_tissue_Summ %>% 
  ggplot(aes(x=as.factor(Age), y=Dry_Tissue_weight_g, color=as.factor(pCO2))) +
  geom_point(position=position_dodge(.5))+ 
  scale_color_manual(values=c("forestgreen","darkorange2"))+
  geom_errorbar(aes(ymin=Dry_Tissue_weight_g-se, ymax=Dry_Tissue_weight_g+se), width=.2,
                position=position_dodge(.5))+
  theme_classic() +  
  xlab("Age (dpf)") +
  #xlab("Age (d)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ 
  scale_y_continuous(name ="Dry tissue weight (g)")+
  geom_line(stat = "identity", size=1.0)+
  theme(text = element_text(size=15))


# normaliZed to shell length

F1_dry_tissue_Summ_norm <-summarySE(F1_dry_tissue, measurevar="Dry_Tissue_weight_g_LENGTHnormalized", groupvars=c("Age", "pCO2"))

DryTissueWeight_Plot_Lengthnorm <- F1_dry_tissue_Summ_norm %>% 
  ggplot(aes(x=as.factor(Age), y=Dry_Tissue_weight_g_LENGTHnormalized, color=as.factor(pCO2))) +
  geom_point(position=position_dodge(.5))+ 
  scale_color_manual(values=c("forestgreen","darkorange2"))+
  geom_errorbar(aes(ymin=Dry_Tissue_weight_g_LENGTHnormalized-se, ymax=Dry_Tissue_weight_g_LENGTHnormalized+se), width=.2,
                position=position_dodge(.5))+
  theme_classic() +  
  xlab("Age (dpf)") +
  #xlab("Age (d)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ 
  scale_y_continuous(name ="Dry tissue weight (g, length normalized)")+
  geom_line(stat = "identity", size=1.0)+
  theme(text = element_text(size=15))


# cumulative plot dry tissue and shell
ggarrange(DryShellWeight_Plot, DryShellWeight_Plot_Lengthnorm, DryTissueWeight_Plot, DryTissueWeight_Plot_Lengthnorm,nrow = 2, ncol = 2)


# Export to pdf
pdf(paste0(filename = "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/DryWeights/F1/DryShell_DryTissue.pdf"), width = 12, height = 10)
ggarrange(DryShellWeight_Plot, DryShellWeight_Plot_Lengthnorm, DryTissueWeight_Plot, DryTissueWeight_Plot_Lengthnorm,nrow = 2, ncol = 2)
dev.off()


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# F2, Summary plots ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::




# (1) DRY SHELL WEIGHT

F2_dry_shell <- F2_dryweights %>% 
  dplyr::select(c('Age','pH','pH_Replicate', 'pCO2', 'Shell_length_mm','Dry_Shell_weight_g')) %>% 
  dplyr::filter(!Shell_length_mm == 'MEASURE THIS') %>%  # check this row, omit for now
  dplyr::filter(!Dry_Shell_weight_g < 0) %>%  # one sample with a negative value here,.. omit!
  na.omit() %>% 
  dplyr::mutate(Dry_Shell_weight_g = as.numeric(Dry_Shell_weight_g)) %>% 
  dplyr::mutate(Shell_length_mm = as.numeric(Shell_length_mm)) %>% 
  dplyr::mutate(Dry_Shell_weight_g_LENGTHnormalized = Dry_Shell_weight_g/Shell_length_mm)

# not normalized
F2_dry_shell_Summ <-summarySE(F2_dry_shell, measurevar="Dry_Shell_weight_g", groupvars=c("Age", "pCO2"))

F2DryShellWeight_Plot <- F2_dry_shell_Summ %>% 
  mutate(pCO2 = fct_relevel(pCO2, "500 μatm", "800 μatm", "1200 uatm")) %>% 
  ggplot(aes(x=as.factor(Age), y=Dry_Shell_weight_g, color=as.factor(pCO2))) +
  geom_point(position=position_dodge(.5))+ 
  scale_color_manual(values=c("forestgreen","darkorange2", "purple"))+
  geom_errorbar(aes(ymin=Dry_Shell_weight_g-se, ymax=Dry_Shell_weight_g+se), width=.2,
                position=position_dodge(.5))+
  theme_classic() +  
  xlab("Age (dpf)") +
  #xlab("Age (d)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ 
  scale_y_continuous(name ="Dry shell weight (g)")+
  geom_line(stat = "identity", size=1.0)+
  theme(text = element_text(size=15))


# normaliZed to shell length

F2_dry_shell_Summ_norm <-summarySE(F2_dry_shell, measurevar="Dry_Shell_weight_g_LENGTHnormalized", groupvars=c("Age", "pCO2"))

F2DryShellWeight_Plot_Lengthnorm <- F2_dry_shell_Summ_norm %>% 
  mutate(pCO2 = fct_relevel(pCO2, "500 μatm", "800 μatm", "1200 uatm")) %>% 
  ggplot(aes(x=as.factor(Age), y=Dry_Shell_weight_g_LENGTHnormalized, color=as.factor(pCO2))) +
  geom_point(position=position_dodge(.5))+ 
  scale_color_manual(values=c("forestgreen","darkorange2", "purple"))+
  geom_errorbar(aes(ymin=Dry_Shell_weight_g_LENGTHnormalized-se, ymax=Dry_Shell_weight_g_LENGTHnormalized+se), width=.2,
                position=position_dodge(.5))+
  theme_classic() +  
  xlab("Age (dpf)") +
  #xlab("Age (d)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ 
  scale_y_continuous(name ="Dry shell weight (g, length normalized)")+
  geom_line(stat = "identity", size=1.0)+
  theme(text = element_text(size=15))


# (2) DRY TISSUE WEIGHT

F2_dry_tissue <- F2_dryweights %>% 
  dplyr::select(c('Age','pH','pH_Replicate', 'pCO2', 'Shell_length_mm','Dry_Tissue_weight_g')) %>% 
  dplyr::filter(!Shell_length_mm == 'MEASURE THIS') %>%  # check this row, omit for now
  dplyr::filter(!Dry_Tissue_weight_g < 0) %>%  # one sample with a negative value here,.. omit!
  na.omit() %>% 
  dplyr::mutate(Dry_Tissue_weight_g = as.numeric(Dry_Tissue_weight_g)) %>% 
  dplyr::mutate(Shell_length_mm = as.numeric(Shell_length_mm)) %>% 
  dplyr::mutate(Dry_Tissue_weight_g_LENGTHnormalized = Dry_Tissue_weight_g/Shell_length_mm)

# not normalized

F2_dry_tissue_Summ <-summarySE(F2_dry_tissue, measurevar="Dry_Tissue_weight_g", groupvars=c("Age", "pCO2"))

F2DryTissueWeight_Plot <- F2_dry_tissue_Summ %>% 
  mutate(pCO2 = fct_relevel(pCO2, "500 μatm", "800 μatm", "1200 uatm")) %>% 
  ggplot(aes(x=as.factor(Age), y=Dry_Tissue_weight_g, color=as.factor(pCO2))) +
  geom_point(position=position_dodge(.5))+ 
  scale_color_manual(values=c("forestgreen","darkorange2", "purple"))+
  geom_errorbar(aes(ymin=Dry_Tissue_weight_g-se, ymax=Dry_Tissue_weight_g+se), width=.2,
                position=position_dodge(.5))+
  theme_classic() +  
  xlab("Age (dpf)") +
  #xlab("Age (d)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ 
  scale_y_continuous(name ="Dry tissue weight (g)")+
  geom_line(stat = "identity", size=1.0)+
  theme(text = element_text(size=15))


# normaliZed to shell length

F2_dry_tissue_Summ_norm <-summarySE(F2_dry_tissue, measurevar="Dry_Tissue_weight_g_LENGTHnormalized", groupvars=c("Age", "pCO2"))

F2DryTissueWeight_Plot_Lengthnorm <- F2_dry_tissue_Summ_norm %>% 
  mutate(pCO2 = fct_relevel(pCO2, "500 μatm", "800 μatm", "1200 uatm")) %>% 
  ggplot(aes(x=as.factor(Age), y=Dry_Tissue_weight_g_LENGTHnormalized, color=as.factor(pCO2))) +
  geom_point(position=position_dodge(.5))+ 
  scale_color_manual(values=c("forestgreen","darkorange2", "purple"))+
  geom_errorbar(aes(ymin=Dry_Tissue_weight_g_LENGTHnormalized-se, ymax=Dry_Tissue_weight_g_LENGTHnormalized+se), width=.2,
                position=position_dodge(.5))+
  theme_classic() +  
  xlab("Age (dpf)") +
  #xlab("Age (d)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ 
  scale_y_continuous(name ="Dry tissue weight (g, length normalized)")+
  geom_line(stat = "identity", size=1.0)+
  theme(text = element_text(size=15))


# cumulative plot dry tissue and shell
ggarrange(F2DryShellWeight_Plot, F2DryShellWeight_Plot_Lengthnorm, F2DryTissueWeight_Plot, F2DryTissueWeight_Plot_Lengthnorm,nrow = 2, ncol = 2)

# Export to pdf
pdf(paste0(filename = "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/DryWeights/F2/DryShell_DryTissue.pdf"), width = 12, height = 10)
ggarrange(F2DryShellWeight_Plot, F2DryShellWeight_Plot_Lengthnorm, F2DryTissueWeight_Plot, F2DryTissueWeight_Plot_Lengthnorm,nrow = 2, ncol = 2)
dev.off()
