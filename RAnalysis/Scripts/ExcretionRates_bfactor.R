# ---
# title: "ExcretionRates_bfactor"
# author: "Samuel Gurr"
# date: "3/19/2023"
# output: pdf_document
# ---


#install.packages("pander")
library(dplyr)
library(ggplot2)
library(nlme)
library(lme4)
library(car)
library(ggpmisc)

# SET WORKING DIRECTORY 
setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis") # personal computer
setwd("C:/Users/samuel.gurr/Documents/Github_repositories/EAD-ASEB-Airradians_multigen_OA/RAnalysis") # personal computer



# LOAD DATA & cater to this script 

# Extretion data 
ER_F1_raw <- read.csv(file="Data/Physiology/Excretion_rates/F1/cumultative_raw/F1_Excretion_master.csv", 
                      header=T,stringsAsFactors=FALSE, fileEncoding="latin1") %>% # master data file
                      dplyr::mutate(Generation = 'F1')
ER_F2_raw <- read.csv(file="Data/Physiology/Excretion_rates/F2/cumultative_raw/F2_Excretion_master.csv", 
                      header=T,stringsAsFactors=FALSE, fileEncoding="latin1") %>% # master data file
                      dplyr::mutate(Generation = 'F2')

# how many datapoints do we have 
nrow(ER_F1_raw) # 82
unique(ER_F1_raw$Date) # 20220202 20220301 20211026 20220922 20221026
nrow(ER_F2_raw) # 84 
unique(ER_F2_raw$Date) # 20221116 20230131 20230223 20230327
82+84 # 166 total

# bind together these datasets
Excretion_data <- rbind(ER_F1_raw, ER_F2_raw)
nrow(Excretion_data) # 166

# lets look into these data 
# we have 1-3 measurments per replicate tank
Excretion_count <- as.data.frame(Excretion_data %>% 
                        dplyr::select(c(Date, pH, Replicate)) %>% 
                        dplyr::group_by(Date, pH, Replicate) %>% 
                        dplyr::summarise(n=n())) %>% 
                        dplyr::mutate(Date = as.factor(Date))
Excretion_count


# Size data 
Size_data <- read.csv(file="Data/Physiology/Respiration/Metadata/Reference_resp_size.csv", header=T) 

Size_data_filt <- Size_data %>% 
                          dplyr::mutate(Date = paste("20",
                                                     (format(as.Date(Date, "%m/%d/%Y"), "%y%m%d")), 
                                                     sep ='')) %>% # change format of the date to the format in Excretion_data
                          dplyr::select(-c(Food, Shell_tin_weight, tin_plus_shell, Tissue_tin_.weight, 
                                           tin_plus_tissue, Plate, Vessel_well_volume, Notes)) %>%  # get rid of unneeded column(s)
                          dplyr::filter(Date %in% unique(Excretion_data$Date)) %>% # 20220202 20220301 20211026 20220922 20221026- call these dates in te size dat
                          dplyr::filter(!Instrument %in% 'SDR_24channel') %>% # dates occasionally have resp for F2s with SDr, call the Loligo system for the correct animals
                          dplyr::select(-Instrument) # now we can omit Instrument column
nrow(Size_data_filt) # 166

# lets look into these data 
# we have 1-3 measurments per replicate tank
Size_count <- as.data.frame(Size_data_filt %>% 
                    dplyr::select(c(Date, pH, Replicate)) %>% 
                    dplyr::group_by(Date, pH, Replicate) %>% 
                    dplyr::summarise(n=n())) %>% dplyr::mutate(Date = as.factor(Date))
Size_count


# sanity check
# see whether the datasets differ in any way
setdiff(Size_count, Excretion_count) # nmo output means data are exactly the same!


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# EDIT AND MERG DATA  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

list(unique(Excretion_data$Date)) # 20220202 20220301 20211026 20220922 20221026 20221116 20230131 20230223 20230327- call these dates in the size data and only Loligo RR data (large animals measured excretion!)
list(unique(Size_count$Date)) # 20211026 20220202 20220301 20220922 20221026 20221116 20230131 20230223 20230327- call these dates in the size data and only Loligo RR data (large animals measured excretion!)

nrow(Excretion_data) # 166
nrow(Size_data_filt) # 166


# View(Size_data_2)
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# GET B FACTOR FOR ALL AVAILABLE INDIVIDUALS WITH TDW AND MO2 ::::::::::::::::::::::::::::::

Excretion_data_OM  <- merge(Excretion_data, Size_data_filt) %>% 
                          dplyr::filter(!Dry_Tissue_weight %in% '<add here>') %>% 
                          dplyr::filter(!ExcretionRate_umol_hr < 0) %>% # 6 excretion < 0 omit (20211026 7.5C, 20220202 7.5C, 20220202 8.0C)
                          dplyr::mutate(pCO2 = case_when(pH == 8.0 ~ "500 μatm", 
                                                         pH == 7.5 ~ "800 μatm",
                                                         pH == 7 ~ "1200 μatm"),
                                        Length_mm = Length_um/1000)
nrow(Excretion_data_OM) # 160 - with the dry tissue weights omiited
# View(Excretion_data_OM)
# View(Excretion_data)
# View(Size_data_2)
# Excretion_data_OM$Dry_Tissue_weight
# View(Excretion_data_OM)

Excretion_data_OM$pCO2 <- fct_relevel(Excretion_data_OM$pCO2, c('500 μatm', '800 μatm', '1200 μatm'))

Excretion_data_OM      <- Excretion_data_OM %>% filter(!is.na(Excretion_data_OM$ExcretionRate_umol_hr)) 
nrow(Excretion_data_OM) # 160


# Separate into F1 and F2 files
Excretion_data_F1 <- Excretion_data_OM %>%  
                            dplyr::filter(Generation %in% 'F1') 

Excretion_data_F2 <- Excretion_data_OM %>% 
                            dplyr::filter(Generation %in% 'F2')






# F1 and F2 

Excretion_data_OM$log10_VER     <- log10(as.numeric(Excretion_data_OM$ExcretionRate_umol_hr)) # assign resp value
Excretion_data_OM$log10_TDW     <- log10(as.numeric(Excretion_data_OM$Dry_Tissue_weight)) # assign length value 
Excretion_data_OM$log10_Length  <- log10(as.numeric(Excretion_data_OM$Length_mm)) # assign length value 

# TDW: LOW and MODERATE pCO2 b factor plots by treatment 
TDW_ER_b.factor_LowVMod <- Excretion_data_OM %>% 
                              dplyr::filter(!pCO2 %in% '1200 μatm') %>% # omit high pCO2 from F2 samples
                              dplyr::mutate(Gen_pCO2 = paste0(Generation,'_',pCO2)) %>% 
                              ggplot(aes(x=log10_TDW, 
                                         y=log10_VER, 
                                         color = Gen_pCO2,
                                         shape = Gen_pCO2)) +
                              geom_point(size = 2) +
                              ggpmisc::stat_ma_line(method = "SMA") + # model 2 regression Standard major axis!
                              ggpmisc::stat_ma_eq(ggpmisc::use_label(c("eq", "n", "R2"))) +  
                              scale_color_manual(values=c("forestgreen","darkorange","forestgreen","darkorange")) +
                              scale_shape_manual(values=c(19, 19, 1,1)) +
                              theme(panel.grid.major = element_blank(), 
                                    panel.grid.minor = element_blank())+ 
                              scale_x_continuous(name ="log10_TDW; in mm") +
                              scale_y_continuous(name ="log10_VER; ER in umol L-1 hr-1)") +
                              theme_classic() +
                              theme(legend.position="none",
                                    element_line(linewidth = 0.5, color = 'black'),
                                    axis.title.y=element_text(size=12),
                                    axis.text.x=element_text(size=(12)),
                                    axis.text.y=element_text(size=(12))) + # legend.position="none",
                              ggtitle("Allometric scaling: log10_VER = log10_a + (b.factor * log10_TDW)") +
                              scale_linetype_discrete(name="Gen x pCO2", 
                                                      breaks=c("F1_500 uatm", 
                                                               "F1_800 uatm", 
                                                               "F2_500 uatm", 
                                                               "F2_800 uatm"), 
                                                      labels = c("F1 x low pCO2", 
                                                                 "F1 x moderate pCO2", 
                                                                 "F2 x low pCO2",
                                                                 "F2 x moderate pCO2"))
TDW_ER_b.factor_LowVMod_facetted  <- TDW_ER_b.factor_LowVMod + facet_wrap(~Generation)

pdf(paste0(filename = "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/allometric_scaling/F1_F2_ER_bFactor_TDW_LowvMod.pdf"), 
    width = 5, height = 10)
print(ggpubr::ggarrange(TDW_ER_b.factor_LowVMod,
                        TDW_ER_b.factor_LowVMod_facetted, nrow = 2, ncol = 1)) # print the model diagnostics
dev.off() 


# TDW: LOW and HIGH pCO2 b factor plots by treatment 
TDW_ER_b.factor_LowVMHigh <- Excretion_data_OM %>% 
                      dplyr::filter(!Generation %in% 'F1') %>%  # no High OA treatment - separate paper
                      dplyr::filter(!pCO2 %in% '800 μatm') %>% # omit high pCO2 from F2 samples
                      dplyr::mutate(Gen_pCO2 = paste0(Generation,'_',pCO2)) %>% 
                      ggplot(aes(x=log10_TDW, 
                                 y=log10_VER, 
                                 color = Gen_pCO2,
                                 shape = Gen_pCO2)) +
                      geom_point(size = 2) +
                      ggpmisc::stat_ma_line(method = "SMA") + # model 2 regression Standard major axis!
                      ggpmisc::stat_ma_eq(ggpmisc::use_label(c("eq", "n", "R2"))) +  
                      scale_color_manual(values=c("forestgreen","purple","forestgreen","purple")) +
                      scale_shape_manual(values=c(19, 19, 1,1)) +
                      theme(panel.grid.major = element_blank(), 
                            panel.grid.minor = element_blank())+ 
                      scale_x_continuous(name ="log10_TDW; in mm") +
                      scale_y_continuous(name ="log10_VER; ER in umol L-1 hr-1)") +
                      theme_classic() +
                      theme(legend.position="none",
                            element_line(linewidth = 0.5, color = 'black'),
                            axis.title.y=element_text(size=12),
                            axis.text.x=element_text(size=(12)),
                            axis.text.y=element_text(size=(12))) + # legend.position="none",
                      ggtitle("Allometric scaling: log10_VER = log10_a + (b.factor * log10_TDW)") +
                      scale_linetype_discrete(name="Gen x pCO2", 
                                              breaks=c("F1_500 uatm", 
                                                       "F1_1200 uatm", 
                                                       "F2_500 uatm", 
                                                       "F2_1200 uatm"), 
                                              labels = c("F1 x low pCO2", 
                                                         "F1 x high pCO2", 
                                                         "F2 x low pCO2",
                                                         "F2 x high pCO2"))

pdf(paste0(filename = "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/allometric_scaling/F2_ER_bFactor_TDW_LowvHigh.pdf"), 
    width = 5, height = 5)
print(TDW_ER_b.factor_LowVMHigh) # print the model diagnostics
dev.off() 


# Length: LOW and MODERATE pCO2 b factor plots by treatment 
Length_ER_b.factor_LowVMod <- Excretion_data_OM %>% 
                      dplyr::filter(!pCO2 %in% '1200 μatm') %>% # omit high pCO2 from F2 samples
                      dplyr::mutate(Gen_pCO2 = paste0(Generation,'_',pCO2)) %>% 
                      ggplot(aes(x=log10_Length, 
                                 y=log10_VER, 
                                 color = Gen_pCO2,
                                 shape = Gen_pCO2)) +
                      geom_point(size = 2) +
                      ggpmisc::stat_ma_line(method = "SMA") + # model 2 regression Standard major axis!
                      ggpmisc::stat_ma_eq(ggpmisc::use_label(c("eq", "n", "R2"))) +  
                      scale_color_manual(values=c("forestgreen","darkorange","forestgreen","darkorange")) +
                      scale_shape_manual(values=c(19, 19, 1,1)) +
                      theme(panel.grid.major = element_blank(), 
                            panel.grid.minor = element_blank())+ 
                      scale_x_continuous(name ="log10_Length; in mm") +
                      scale_y_continuous(name ="log10_VER; ER in umol L-1 hr-1)") +
                      theme_classic() +
                      theme(legend.position="none",
                            element_line(linewidth = 0.5, color = 'black'),
                            axis.title.y=element_text(size=12),
                            axis.text.x=element_text(size=(12)),
                            axis.text.y=element_text(size=(12))) + # legend.position="none",
                      ggtitle("Allometric scaling: log10_VER = log10_a + (b.factor * log10_Length)") +
                      scale_linetype_discrete(name="Gen x pCO2", 
                                              breaks=c("F1_500 uatm", 
                                                       "F1_800 uatm", 
                                                       "F2_500 uatm", 
                                                       "F2_800 uatm"), 
                                              labels = c("F1 x low pCO2", 
                                                         "F1 x moderate pCO2", 
                                                         "F2 x low pCO2",
                                                         "F2 x moderate pCO2")) 
Length_ER_b.factor_LowVMod_facetted <- Length_ER_b.factor_LowVMod + facet_wrap(~Generation)

pdf(paste0(filename = "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/allometric_scaling/F1_F2_ER_bFactor_Length_LowvMod.pdf"), 
    width = 5, height = 10)
print(ggpubr::ggarrange(Length_ER_b.factor_LowVMod,
                        Length_ER_b.factor_LowVMod_facetted, nrow = 2, ncol = 1)) # print the model diagnostics
dev.off() 


# Length: LOW and HIGH pCO2 b factor plots by treatment 
Length_ER_b.factor_LowVMHigh <- Excretion_data_OM %>% 
  dplyr::filter(!Generation %in% 'F1') %>%  # no High OA treatment - separate paper
  dplyr::filter(!pCO2 %in% '800 μatm') %>% # omit high pCO2 from F2 samples
  dplyr::mutate(Gen_pCO2 = paste0(Generation,'_',pCO2)) %>% 
  ggplot(aes(x=log10_Length, 
             y=log10_VER, 
             color = Gen_pCO2,
             shape = Gen_pCO2)) +
  geom_point(size = 2) +
  ggpmisc::stat_ma_line(method = "SMA") + # model 2 regression Standard major axis!
  ggpmisc::stat_ma_eq(ggpmisc::use_label(c("eq", "n", "R2"))) +  
  scale_color_manual(values=c("forestgreen","purple","forestgreen","purple")) +
  scale_shape_manual(values=c(19, 19, 1,1)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+ 
  scale_x_continuous(name ="log10_Length; in mm") +
  scale_y_continuous(name ="log10_VER; ER in umol L-1 hr-1)") +
  theme_classic() +
  theme(legend.position="none",
        element_line(linewidth = 0.5, color = 'black'),
        axis.title.y=element_text(size=12),
        axis.text.x=element_text(size=(12)),
        axis.text.y=element_text(size=(12))) + # legend.position="none",
  ggtitle("Allometric scaling: log10_VER = log10_a + (b.factor * log10_TDW)") +
  scale_linetype_discrete(name="Gen x pCO2", 
                          breaks=c("F1_500 uatm", 
                                   "F1_1200 uatm", 
                                   "F2_500 uatm", 
                                   "F2_1200 uatm"), 
                          labels = c("F1 x low pCO2", 
                                     "F1 x high pCO2", 
                                     "F2 x low pCO2",
                                     "F2 x high pCO2"))

pdf(paste0(filename = "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/allometric_scaling/F2_ER_bFactor_Length_LowvHigh.pdf"), 
    width = 5, height = 5)
print(Length_ER_b.factor_LowVMHigh) # print the model diagnostics
dev.off() 







# F1
# Log Log datasets 
Excretion_data_F1$log10_VER     <- log10(as.numeric(Excretion_data_F1$ExcretionRate_umol_hr)) # assign resp value
Excretion_data_F1$log10_TDW     <- log10(as.numeric(Excretion_data_F1$Dry_Tissue_weight)) # assign length value 
Excretion_data_F1$log10_Length  <- log10(as.numeric(Excretion_data_F1$Length_mm)) # assign length value 

mean(Excretion_data_F1$log10_Length) #1.343729

# run plot for b factor 

nrow(Excretion_data_F1) # 78


# all
ER_b.factorLENGTH_PLOT <- Excretion_data_F1 %>% # 2.94 F2 = 0.73, n = 82
                              ggplot(aes(x=log10_Length, y=log10_VER)) +
                              geom_point() +
                              ggpmisc::stat_ma_line(method = "SMA") + # model 2 regression Standard major axis!
                              ggpmisc::stat_ma_eq(use_label(c("eq", "n", "R2"))) +
                              theme(panel.grid.major = element_blank(), 
                                    panel.grid.minor = element_blank())+ 
                              scale_x_continuous(name ="log10_Length; in mm") +
                              scale_y_continuous(name ="log10_VER; RR in umol L-1 hr-1)") +
                              theme_classic() +
                              theme(legend.position="none",axis.title.y=element_text(size=7)) +
                              ggtitle("Excretion Rate scaling: log10_VER = log10_a + 
                                                                  (b.factor * log10_Length)") 


ER_b.factorTDW_PLOT <- Excretion_data_F1 %>% # 1.29, n = 78, R2 = 0.81
                              ggplot(aes(x=log10_TDW, y=log10_VER)) +
                              geom_point() +
                              ggpmisc::stat_ma_line(method = "SMA") + # model 2 regression Standard major axis!
                              ggpmisc::stat_ma_eq(use_label(c("eq", "n", "R2"))) +
                              theme(panel.grid.major = element_blank(), 
                                    panel.grid.minor = element_blank())+ 
                              scale_x_continuous(name ="log10_TDW; in mm") +
                              scale_y_continuous(name ="log10_VER; RR in umol L-1 hr-1)") +
                              theme_classic() +
                              theme(legend.position="none",axis.title.y=element_text(size=7)) +
                              ggtitle("Excretion Rate scaling: log10_VER = log10_a + 
                                                                  (b.factor * log10_TDW)") 


# by treatment
ER_b.factorLENGTH_PLOT_pCO2 <- Excretion_data_F1 %>% 
                                  ggplot(aes(x=log10_Length, y=log10_VER)) +
                                  geom_point() +
                                  ggpmisc::stat_ma_line(method = "SMA") + # model 2 regression Standard major axis!
                                  ggpmisc::stat_ma_eq(use_label(c("eq", "n", "R2"))) +
                                  theme(panel.grid.major = element_blank(), 
                                        panel.grid.minor = element_blank())+ 
                                  scale_x_continuous(name ="log10_Length; in mm") +
                                  scale_y_continuous(name ="log10_VER; RR in umol L-1 hr-1)") +
                                  theme_classic() +
                                  theme(legend.position="none",axis.title.y=element_text(size=7)) +
                                  ggtitle("Excretion Rate scaling: log10_VER = log10_a +  (b.factor * log10_Length)") +
                                  facet_wrap(~pCO2)

ER_b.factorTDW_PLOT_pCO2 <- Excretion_data_F1 %>% # 1.26 and 1.32
                                  ggplot(aes(x=log10_TDW, y=log10_VER)) +
                                  geom_point() +
                                  ggpmisc::stat_ma_line(method = "SMA") + # model 2 regression Standard major axis!
                                  ggpmisc::stat_ma_eq(use_label(c("eq", "n", "R2"))) +
                                  theme(panel.grid.major = element_blank(), 
                                        panel.grid.minor = element_blank())+ 
                                  scale_x_continuous(name ="log10_TDW; in mm") +
                                  scale_y_continuous(name ="log10_VER; RR in umol L-1 hr-1)") +
                                  theme_classic() +
                                  theme(legend.position="none",axis.title.y=element_text(size=7)) +
                                  ggtitle("Excretion Rate scaling: log10_VER = log10_a +  (b.factor * log10_TDW)") +
                                  facet_wrap(~pCO2)





# F1 Low: 4.32
# F1 Moderate 4.57



# F1 

# assign bfactors by treatment
F1_bLength.low <-  4.32 #mean data by rep tank # 6.99 - with all data
F1_bLength.mod <-  4.57 #mean data by rep tank # 6.57 - with all data

F1_bTDW.low <-  1.26 #mean data by rep tank # 6.99 - with all data
F1_bTDW.mod <-  1.32 #mean data by rep tank # 6.57 - with all data


# call the mean lengths of animals measured
F1_meanLength   <- mean(Excretion_data_F1$Length_mm) # 26.37436 mm
F1_meanTDW      <- mean(Excretion_data_F1$Dry_Tissue_weight) # 0.4684321 mm

#View(RR_formatted_F1s$TDW_um)
# ERnorm = ER × (SHmean / SHindiv)b = µmol L-1 O2 mm-1 hr-1

Excretion_F1_calculated <- Excretion_data_F1 %>% 
  
  dplyr::mutate(
    ExcretionRate_umol_hr_bFactorNormLength.MEAN = 
      case_when(
        pH  == 8.0 ~ (ExcretionRate_umol_hr)*((F1_meanLength/Length_mm)^F1_bLength.low),
        pH  == 7.5 ~ (ExcretionRate_umol_hr)*((F1_meanLength/Length_mm)^F1_bLength.mod)
      ),
    ExcretionRate_umol_hr_bFactorNormTDW.MEAN = 
      case_when(
        pH  == 8.0 ~ (ExcretionRate_umol_hr)*((F1_meanTDW/Dry_Tissue_weight)^F1_bTDW.low),
        pH  == 7.5 ~ (ExcretionRate_umol_hr)*((F1_meanTDW/Dry_Tissue_weight)^F1_bTDW.mod)
      )
    ) %>% 
    dplyr::select(c(Generation,
                    Date,
                    pH,
                    pCO2,
                    Replicate,
                    Number,
                    Run,
                    Length_um,
                    Length_mm,
                    Dry_Tissue_weight,
                    ExcretionRate_umol_hr,
                    ExcretionRate_umol_hr_bFactorNormLength.MEAN,
                    ExcretionRate_umol_hr_bFactorNormTDW.MEAN))
#View(RR_formatted_F1smaster)
write.csv(Excretion_F1_calculated, "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/F1/F1_ExcretionRates_master.csv")
write.csv(Excretion_F1_calculated, "C:/Users/samuel.gurr/Documents/Github_repositories/EAD-ASEB-Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/F1/F1_ExcretionRates_master.csv")










#F2
# Log Log datasets 
Excretion_data_F2$log10_VER     <- log10(as.numeric(Excretion_data_F2$ExcretionRate_umol_hr)) # assign resp value
Excretion_data_F2$log10_TDW     <- log10(as.numeric(Excretion_data_F2$Dry_Tissue_weight)) # assign length value 
Excretion_data_F2$log10_Length  <- log10(as.numeric(Excretion_data_F2$Length_mm)) # assign length value 

mean(Excretion_data_F2$log10_Length) #1.453697

# run plot for b factor 

nrow(Excretion_data_F2) # 82


# all
ER_b.factorLENGTH_PLOT <- Excretion_data_F2 %>% 
                            ggplot(aes(x=log10_Length, y=log10_VER)) +
                            geom_point() +
                            ggpmisc::stat_ma_line(method = "SMA") + # model 2 regression Standard major axis!
                            ggpmisc::stat_ma_eq(use_label(c("eq", "n", "R2"))) +
                            theme(panel.grid.major = element_blank(), 
                                  panel.grid.minor = element_blank())+ 
                            scale_x_continuous(name ="log10_Length; in mm") +
                            scale_y_continuous(name ="log10_VER; RR in umol L-1 hr-1)") +
                            theme_classic() +
                            theme(legend.position="none",axis.title.y=element_text(size=7)) +
                            ggtitle("Excretion Rate scaling: log10_VER = log10_a + (b.factor * log10_Length)") 

ER_b.factorTDW_PLOT <- Excretion_data_F2 %>% # 0.754
                              ggplot(aes(x=log10_TDW, y=log10_VER)) +
                              geom_point() +
                              ggpmisc::stat_ma_line(method = "SMA") + # model 2 regression Standard major axis!
                              ggpmisc::stat_ma_eq(use_label(c("eq", "n", "R2"))) +
                              theme(panel.grid.major = element_blank(), 
                                    panel.grid.minor = element_blank())+ 
                              scale_x_continuous(name ="log10_TDW; in mm") +
                              scale_y_continuous(name ="log10_VER; RR in umol L-1 hr-1)") +
                              theme_classic() +
                              theme(legend.position="none",axis.title.y=element_text(size=7)) +
                              ggtitle("Excretion Rate scaling: log10_VER = log10_a + (b.factor * log10_TDW)") 


# by treatment
ER_b.factorLENGTH_PLOT_pCO2 <- Excretion_data_F2 %>% 
                            ggplot(aes(x=log10_Length, y=log10_VER)) +
                            geom_point() +
                            ggpmisc::stat_ma_line(method = "SMA") + # model 2 regression Standard major axis!
                            ggpmisc::stat_ma_eq(use_label(c("eq", "n", "R2"))) +
                            theme(panel.grid.major = element_blank(), 
                                  panel.grid.minor = element_blank())+ 
                            scale_x_continuous(name ="log10_Length; in mm") +
                            scale_y_continuous(name ="log10_VER; RR in umol L-1 hr-1)") +
                            theme_classic() +
                            theme(legend.position="none",axis.title.y=element_text(size=7)) +
                            ggtitle("Excretion Rate scaling: log10_VER = log10_a +  (b.factor * log10_Length)") +
                            facet_wrap(~pCO2)

ER_b.factorTDW_PLOT_pCO2 <- Excretion_data_F2 %>%  #0.413 0.725 0.587
                                  ggplot(aes(x=log10_TDW, y=log10_VER)) +
                                  geom_point() +
                                  ggpmisc::stat_ma_line(method = "SMA") + # model 2 regression Standard major axis!
                                  ggpmisc::stat_ma_eq(use_label(c("eq", "n", "R2"))) +
                                  theme(panel.grid.major = element_blank(), 
                                        panel.grid.minor = element_blank())+ 
                                  scale_x_continuous(name ="log10_TDW; in mm") +
                                  scale_y_continuous(name ="log10_VER; RR in umol L-1 hr-1)") +
                                  theme_classic() +
                                  theme(legend.position="none",axis.title.y=element_text(size=7)) +
                                  ggtitle("Excretion Rate scaling: log10_VER = log10_a +  (b.factor * log10_TDW)") +
                                  facet_wrap(~pCO2)



# OUTPUT PLOTS 
pdf(paste0(filename = "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/allometric_scaling/F2_ExcretionScaling_bFactor_Length.pdf"), 
    width = 8, height = 16)
print(ggarrange(ER_b.factorLENGTH_PLOT,
                ER_b.factorLENGTH_PLOT_pCO2, nrow = 2, ncol = 1)) # print the model diagnostics
dev.off() 

# F2 Low: 3.24
# F2 Moderate 2.55
# F2 High 2.91


pdf(paste0(filename = "C:/Users/samuel.gurr/Documents/Github_repositories/EAD-ASEB-Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/allometric_scaling/F2_ExcretionScaling_bFactor_TDW.pdf"), 
    width = 8, height = 16)
print(ggarrange(ER_b.factorTDW_PLOT,
                ER_b.factorTDW_PLOT_pCO2, nrow = 2, ncol = 1)) # print the model diagnostics
dev.off() 










# F2 

# assign bfactors by treatment
F2_bLength.low  <-  3.24 #mean data by rep tank # 6.99 - with all data
F2_bLength.mod  <-  2.55 #mean data by rep tank # 6.57 - with all data
F2_bLength.high <-  2.91 #mean data by rep tank # 6.57 - with all data


F2_bTDW.low  <-  0.906 #mean data by rep tank # 6.99 - with all data
F2_bTDW.mod  <-  0.725 #mean data by rep tank # 6.57 - with all data
F2_bTDW.high <-  0.587 #mean data by rep tank # 6.57 - with all data



# call the mean lengths of animals measured
F2_meanLength_all   <- mean(Excretion_data_F2$Length_mm) # 30.67378 mm
F2_meanLength_LvM   <- mean(
                            (Excretion_data_F2 %>% filter(!pCO2 %in% '1200 μatm'))$Length_mm
                            )# 30.55455 mm
F2_meanLength_LvH   <-  mean(
                            (Excretion_data_F2 %>% filter(!pCO2 %in% '800 μatm'))$Length_mm
                            )# 31.01759 mm


F2_meanTDW_all   <- mean(Excretion_data_F2$Dry_Tissue_weight) # 0.3775268 g
F2_meanTDW_LvM   <- mean(
                           (Excretion_data_F2 %>% filter(!pCO2 %in% '1200 μatm'))$Dry_Tissue_weight
                           )# 0.3638818 g
F2_meanTDW_LvH   <-  mean(
                            (Excretion_data_F2 %>% filter(!pCO2 %in% '800 μatm'))$Dry_Tissue_weight
                          )# 0.3943907 g

#View(RR_formatted_F2s$TDW_um)
# ERnorm = ER × (SHmean / SHindiv)b = µmol L-1 O2 mm-1 hr-1

Excretion_F2_calculated <- Excretion_data_F2 %>% 
  
                            dplyr::mutate(
                              
                              
                              # LEngth b factor for all, lvm aND LVh
                              ExcretionRate_umol_hr_bFactorNormLength.MEAN_all = 
                                case_when(
                                  pH  == 8.0 ~ (ExcretionRate_umol_hr)*((F2_meanLength_all/Length_mm)^F2_bLength.low),
                                  pH  == 7.5 ~ (ExcretionRate_umol_hr)*((F2_meanLength_all/Length_mm)^F2_bLength.mod),
                                  pH  == 7.0 ~ (ExcretionRate_umol_hr)*((F2_meanLength_all/Length_mm)^F2_bLength.high)
                                ),
                              
                              ExcretionRate_umol_hr_bFactorNormLength.MEAN_LvM = # same except new mean Length
                                case_when(
                                  pH  == 8.0 ~ (ExcretionRate_umol_hr)*((F2_meanLength_LvM/Length_mm)^F2_bLength.low),
                                  pH  == 7.5 ~ (ExcretionRate_umol_hr)*((F2_meanLength_LvM/Length_mm)^F2_bLength.mod),
                                  pH  == 7.0 ~ NA
                                ),                              
                              
                              ExcretionRate_umol_hr_bFactorNormLength.MEAN_LvH =  # same except new mean Length
                                case_when(
                                  pH  == 8.0 ~ (ExcretionRate_umol_hr)*((F2_meanLength_LvH/Length_mm)^F2_bLength.low),
                                  pH  == 7.5 ~ NA,
                                  pH  == 7.0 ~ (ExcretionRate_umol_hr)*((F2_meanLength_LvH/Length_mm)^F2_bLength.high)
                                ),
                              
                              
                              
                              # TDW b factor for all, lvm aND LVh
                              ExcretionRate_umol_hr_bFactorNormTDW.MEAN_all = 
                                case_when(
                                  pH  == 8.0 ~ (ExcretionRate_umol_hr)*((F2_meanLength/Length_mm)^F2_bLength.low),
                                  pH  == 7.5 ~ (ExcretionRate_umol_hr)*((F2_meanLength/Length_mm)^F2_bLength.mod),
                                  pH  == 7.0 ~ (ExcretionRate_umol_hr)*((F2_meanLength/Length_mm)^F2_bLength.high)
                                ),
                              
                              ExcretionRate_umol_hr_bFactorNormTDW.MEAN_LvM =  # same except new mean TDW
                                case_when(
                                  pH  == 8.0 ~ (ExcretionRate_umol_hr)*((F2_meanTDW_LvM/Length_mm)^F2_bLength.low),
                                  pH  == 7.5 ~ (ExcretionRate_umol_hr)*((F2_meanTDW_LvM/Length_mm)^F2_bLength.mod),
                                  pH  == 7.0 ~ NA
                                ),                                   
                              
                              ExcretionRate_umol_hr_bFactorNormTDW.MEAN_LvH = # same except new mean TDW
                                case_when(
                                  pH  == 8.0 ~ (ExcretionRate_umol_hr)*((F2_meanTDW_LvH/Length_mm)^F2_bLength.low),
                                  pH  == 7.5 ~ NA,
                                  pH  == 7.0 ~ (ExcretionRate_umol_hr)*((F2_meanTDW_LvH/Length_mm)^F2_bLength.high)
                                )                                   
                              
                              ) %>% 
                          
                            dplyr::select(c(Generation,
                                            Date,
                                            pH,
                                            pCO2,
                                            Replicate,
                                            Number,
                                            Run,
                                            Length_um,
                                            Length_mm,
                                            Dry_Tissue_weight,
                                            ExcretionRate_umol_hr,
                                            ExcretionRate_umol_hr_bFactorNormLength.MEAN_all,
                                            ExcretionRate_umol_hr_bFactorNormLength.MEAN_LvM,
                                            ExcretionRate_umol_hr_bFactorNormLength.MEAN_LvH,
                                            ExcretionRate_umol_hr_bFactorNormTDW.MEAN_all,
                                            ExcretionRate_umol_hr_bFactorNormTDW.MEAN_LvM,
                                            ExcretionRate_umol_hr_bFactorNormTDW.MEAN_LvH
                                            ))
#View(RR_formatted_F2smaster)
write.csv(Excretion_F2_calculated, "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/F2/F2_ExcretionRates_master.csv")
write.csv(Excretion_F2_calculated, "C:/Users/samuel.gurr/Documents/Github_repositories/EAD-ASEB-Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/F2/F2_ExcretionRates_master.csv")











































# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# NORMALIZED BASED ON B FACTOR 1.06 (ABOVE)::::::::::::::::::::::::::::::
# unique(Excretion_master$Date)
# Excretion_master <- Excretion_data_OM %>% # merge size and excretion datadata
#                       dplyr::filter(!ExcretionRate_umol_hr < 0) %>% # 3 excretion < 0 omit (20211026 7.5C, 20220202 7.5C, 20220202 8.0C)
#                       dplyr::mutate(ExcretionRate_umol_hr_TDWbfactor =  
#                                       ExcretionRate_umol_hr*
#                                       ( (1/(as.numeric(Dry_Tissue_weight)))^0.979) ) %>% # correct ExcretionRate_umol_mL_hr for gram of Tissue Dry WEight
#                       dplyr::mutate(pCO2 = case_when(pH == 8.0 ~ "500 μatm", 
#                                                      pH == 7.5 ~ "800 μatm", 
#                                                      pH == 7.0 ~ "1200 μatm"))
# 
# 
# F1_Excretion_master_bfactor<- Excretion_master %>% 
#                           dplyr::filter(!Date %in% c('20221116','20230131','20230223','20230327')) 
# nrow(F1_Excretion_master_bfactor) # 78
# F2_Excretion_master_bfactor <- Excretion_master %>% 
#                           dplyr::filter(Date %in% c('20221116', '20230131','20230223','20230327')) 
# nrow(F2_Excretion_master_bfactor) # 82
# 
# # WRITE CSV OF THE MASTER FILE
# write.csv(F1_Excretion_master_bfactor, "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/F1/F1_ExcretionRates_master.csv")
# write.csv(F2_Excretion_master_bfactor, "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/F2/F2_ExcretionRates_master.csv")
# 
