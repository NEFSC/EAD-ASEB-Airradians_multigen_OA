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

# SET WORKING DIRECTORY 
setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis") # personal computer



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
                          dplyr::filter(!Dry_Tissue_weight %in% '<add here>')
nrow(Excretion_data_OM) # 166 - with the dry tissue weights omiited
# View(Excretion_data_OM)
# View(Excretion_data)
# View(Size_data_2)
# Excretion_data_OM$Dry_Tissue_weight
# View(Excretion_data_OM)
Excretion_data_OM <- Excretion_data_OM %>% # merge size and excretion data
                          dplyr::filter(!ExcretionRate_umol_hr < 0) %>% # 3 excretion < 0 omit (20211026 7.5C, 20220202 7.5C, 20220202 8.0C)
                          dplyr::mutate(pCO2 = case_when(pH == 8.0 ~ "500 μatm", 
                                                         pH == 7.5 ~ "800 μatm",
                                                         pH == 7 ~ "1200 μatm"))

Excretion_data_OM$pCO2 <- fct_relevel(Excretion_data_OM$pCO2, c('500 μatm', '800 μatm', '1200 μatm'))

Excretion_data_OM            <- Excretion_data_OM %>% filter(!is.na(Excretion_data_OM$ExcretionRate_umol_hr)) 
Excretion_data_OM$log10_VER  <- log10(as.numeric(Excretion_data_OM$ExcretionRate_umol_hr)) # assign resp value
Excretion_data_OM$log10_TDW  <- log10(as.numeric(Excretion_data_OM$Dry_Tissue_weight)) # assign length value 

# run plot for b factor 
nrow(Excretion_data_OM)
ER_b.factor_PLOT_ALL <- Excretion_data_OM %>% 
                        ggplot(aes(x=log10_TDW, y=log10_VER)) +
                        geom_point() +
                        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ 
                        scale_x_continuous(name ="log10_BodyMass; TDW in g") +
                        scale_y_continuous(name ="log10_ER; ER in umol L-1 hr-1)") +
                        theme_classic() +
                        theme(legend.position="none",axis.title.y=element_text(size=7)) +
                        ggtitle("Excretion rate scaling: log10_MO2 = log10_a + (b.factor * log10_BodyMass)") +
                        geom_smooth(method = lm, color = 'red') +
                        ggpmisc::stat_poly_eq(parse=T, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), label.x.npc = "left")
ER_b.factor_PLOT_ALL
# b factor == 0.979 for TDW


ER_b.factor_PLOT_pCO2 <-Excretion_data_OM %>% 
                        ggplot(aes(x=log10_TDW, y=log10_VER, color = pCO2)) +
                        geom_point() +
                        scale_color_manual(values=c("darkgreen","orange", "purple")) +
                        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ 
                        scale_x_continuous(name ="log10_BodyMass; TDW in g") +
                        scale_y_continuous(name ="log10_ER; ER in umol L-1 hr-1)") +
                        theme_classic() +
                        theme(legend.position="none",axis.title.y=element_text(size=7)) +
                        ggtitle("Excretion rate scaling: log10_MO2 = log10_a + (b.factor * log10_BodyMass)") +
                        geom_smooth(method = lm, color = 'red') +
                        ggpmisc::stat_poly_eq(parse=T, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), label.x.npc = "left") + 
                        facet_wrap(~pCO2)
ER_b.factor_PLOT_pCO2


ER_b.factor_PLOT_Gen <-Excretion_data_OM %>% 
                        ggplot(aes(x=log10_TDW, y=log10_VER)) +
                        geom_point() +
                        # scale_color_manual(values=c("darkgreen","orange", "purple")) +
                        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ 
                        scale_x_continuous(name ="log10_BodyMass; TDW in g") +
                        scale_y_continuous(name ="log10_ER; ER in umol L-1 hr-1)") +
                        theme_classic() +
                        theme(legend.position="none",axis.title.y=element_text(size=7)) +
                        ggtitle("Excretion rate scaling: log10_MO2 = log10_a + (b.factor * log10_BodyMass)") +
                        geom_smooth(method = lm, color = 'red') +
                        ggpmisc::stat_poly_eq(parse=T, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), label.x.npc = "left") + 
                        facet_wrap(~Generation)
ER_b.factor_PLOT_Gen



ER_b.factor_PLOT_GenpCO2 <-Excretion_data_OM %>% 
                            ggplot(aes(x=log10_TDW, y=log10_VER, color = pCO2)) +
                            geom_point() +
                            scale_color_manual(values=c("darkgreen","orange", "purple")) +
                            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ 
                            scale_x_continuous(name ="log10_BodyMass; TDW in g") +
                            scale_y_continuous(name ="log10_ER; ER in umol L-1 hr-1)") +
                            theme_classic() +
                            theme(legend.position="none",axis.title.y=element_text(size=7)) +
                            ggtitle("Excretion rate scaling: log10_MO2 = log10_a + (b.factor * log10_BodyMass)") +
                            geom_smooth(method = lm, color = 'red') +
                            ggpmisc::stat_poly_eq(parse=T, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), label.x.npc = "left") + 
                            facet_wrap(~Generation*pCO2)
ER_b.factor_PLOT_GenpCO2


pdf(paste0(filename = "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/allometric_scaling/ExcretionScaling_bFactor_TDW.pdf"), 
                        width = 8, height = 12)
print(ggarrange(ER_b.factor_PLOT_ALL, ER_b.factor_PLOT_pCO2, 
                ER_b.factor_PLOT_Gen, ER_b.factor_PLOT_GenpCO2, nrow = 4, ncol = 1)) # print the model diagnostics 
dev.off()







# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# NORMALIZED BASED ON B FACTOR 1.06 (ABOVE)::::::::::::::::::::::::::::::
unique(Excretion_master$Date)
Excretion_master <- Excretion_data_OM %>% # merge size and excretion datadata
                      dplyr::filter(!ExcretionRate_umol_hr < 0) %>% # 3 excretion < 0 omit (20211026 7.5C, 20220202 7.5C, 20220202 8.0C)
                      dplyr::mutate(ExcretionRate_umol_hr_TDWbfactor =  
                                      ExcretionRate_umol_hr*
                                      ( (1/(as.numeric(Dry_Tissue_weight)))^0.979) ) %>% # correct ExcretionRate_umol_mL_hr for gram of Tissue Dry WEight
                      dplyr::mutate(pCO2 = case_when(pH == 8.0 ~ "500 μatm", 
                                                     pH == 7.5 ~ "800 μatm", 
                                                     pH == 7.0 ~ "1200 μatm"))


F1_Excretion_master_bfactor<- Excretion_master %>% 
                          dplyr::filter(!Date %in% c('20221116','20230131','20230223','20230327')) 
nrow(F1_Excretion_master_bfactor) # 78
F2_Excretion_master_bfactor <- Excretion_master %>% 
                          dplyr::filter(Date %in% c('20221116', '20230131','20230223','20230327')) 
nrow(F2_Excretion_master_bfactor) # 82

# WRITE CSV OF THE MASTER FILE
write.csv(F1_Excretion_master_bfactor, "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/F1/F1_ExcretionRates_master.csv")
write.csv(F2_Excretion_master_bfactor, "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/F2/F2_ExcretionRates_master.csv")

