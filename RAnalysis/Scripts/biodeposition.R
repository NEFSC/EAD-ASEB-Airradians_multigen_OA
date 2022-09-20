

## Load libraries

library(ggplot2)
library(dplyr)
library(reshape2)

## set working directory

setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis") # Work computer
# setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis") # Work computer

## load data 

biodep <- read.csv(file="Data/Biodeposition/Raw_masterdata_biodeposition.csv", header = TRUE) 

## Key for calculated metrics below:
# IER == Inorganic Egestion Rate: PIM of feces/feces collection time
# OER == Organic Egestion Rate: POM of feces/feces collection time
# IRR == Inorganic Rejection Rate: PIM of pseudofeces/pseudofeces collection time
# ORR == Organic Rejection Rate: POM of pseudofeces/pseudofeces collection time
# RR  == Rejection Rate: ORR+IRR
# p   == Fraction of Organic Rejected: ORR/RR (organic fraction of the pseudofeces)
# f   == POM available: Average POM of the water
# SE  == Selection Efficiency: 1-(p/f) (organic content of pseudofeces/organic content of the water)
# IFR == Inorganic Filtration Rate: IER + IRR (PIM feces + PIM pseudofeces; i.e. total inorganic matter filtered/collection time)
# CR  == Cleanrance Rate: IFR/PIM of the water
# FR  == Filtration Rate: CR * TPM of the water
# %RR == RR/FR (amount rejected/total amount filtered)
# TIR == Total Ingestion Rate: FR - RR
# OFR == Organic FIltration Rate: CR * POM of the water
# ORI == Organic INgestion Rate: OFR-ORR
# i   == Fraction of Organic Matter ingested: OIR/TIR (i.e. fraction of ingested material that was organic)
# AR  == Assimilation Rate: OIR-OER (rate of POM filtration - rate of POM rejection - rate of POM egestion)
# AE  == Assimilation Efficiency: AR/OIR


# Calculations

## Particulate matter (total and particulate inorganic/organic matter)
biodep2 <- biodep %>% 
  
  # Total particulate matter
  dplyr::mutate(TPM_mgL = case_when(vol_filtered_L > 0 ~ ((dry_filter_weight_mg - initial_filter_weight_mg) / vol_filtered_L) ,
                                    is.na(vol_filtered_L) ~ (dry_filter_weight_mg - initial_filter_weight_mg) )) %>% 
  
  # Particulate Inorganic matter - NOTE: mg/L for the water samples, BUT just mg for the feces and pseuodofeces (although column reads mg_L)
  dplyr::mutate(PIM_mgL = case_when(vol_filtered_L > 0 ~ ((ash_filter_weight_mg - initial_filter_weight_mg) / vol_filtered_L) ,
                                    is.na(vol_filtered_L) ~ (ash_filter_weight_mg - initial_filter_weight_mg) )) %>% 
  
  # Particulate Organic matter - NOTE: mg/L for the water samples, BUT just mg for the feces and pseuodofeces (although column reads mg_L)
  dplyr::mutate(POM_mgL = case_when(vol_filtered_L > 0 ~ ((dry_filter_weight_mg - ash_filter_weight_mg) / vol_filtered_L) ,
                                    is.na(vol_filtered_L) ~ (dry_filter_weight_mg - ash_filter_weight_mg) )) %>% 
  
  # % of Total as Particulate Inorganic matter 
  dplyr::mutate(Perc_INORG = ((PIM_mgL/TPM_mgL)*100) ) %>% 
  
  
  # % of Total as Particulate Organic matter 
  dplyr::mutate(Perc_ORG = ((POM_mgL/TPM_mgL)*100) ) %>% 
 
# EGESTION RATE DATA (just for feces samples)
 
   # Egestion rate (mg / hour) 
  dplyr::mutate(ER_mghr = case_when(sample_type =='feces' ~ (TPM_mgL / inclubation_time_hours)) ) %>% # although column reads mgL, feces and pseudofeces are in units of just mg
  
  # Organic Egestion Rate (OER) POM of feces/feces collection time 
  dplyr::mutate(OER_mghr = case_when(sample_type =='feces' ~ (POM_mgL / inclubation_time_hours)) ) %>% # although column reads mgL, feces and pseudofeces are in units of just mg
 
   # Inorganic Egestion Rate (OER) PIM of feces/feces collection time 
  dplyr::mutate(IER_mghr = case_when(sample_type =='feces' ~ (PIM_mgL / inclubation_time_hours)) ) %>% # although column reads mgL, feces and pseudofeces are in units of just mg   
  
# REJECTION RATE DATA (just for pseudofeces samples)
 
   # Rejection rate (mg / hour) 
  dplyr::mutate(RR_mghr = case_when(sample_type =='pseudofeces' ~ (TPM_mgL / inclubation_time_hours)) ) %>% # although column reads mgL, feces and pseudofeces are in units of just mg
 
   # Organic Rejection Rate (ORR) POM of pseudofeces/pseudofeces collection time 
  dplyr::mutate(ORR_mghr = case_when(sample_type =='pseudofeces' ~ (POM_mgL / inclubation_time_hours)) ) %>% # although column reads mgL, feces and pseudofeces are in units of just mg
 
   # Inorganic Rejection Rate (IRR) PIM of pseudofeces/pseudofeces collection time 
  dplyr::mutate(IRR_mghr = case_when(sample_type =='pseudofeces' ~ (PIM_mgL / inclubation_time_hours)) )  # although column reads mgL, feces and pseudofeces are in units of just mg   
  


# blanks - as 'water_Input' - call to correct in biodep2 below...
WaterSamples_dat     <- biodep2 %>%  
                        dplyr::select(c('Date', 'sample_type', 'treatment', 'water_sample_time', 'TPM_mgL',  'PIM_mgL',  'POM_mgL', 'Perc_INORG', 'Perc_ORG')) %>% 
                        dplyr::filter(sample_type %in% 'water_Blank') %>% 
                        dplyr::group_by(Date,treatment)
                       # slice(-1) # removes the first timestamp by group 
# mean for these blanks here...
WaterSamples_dat_AVE <- WaterSamples_dat %>% 
                        dplyr::select(-c('sample_type', 'water_sample_time')) %>% # %>%  # omit to average across all group_by columns
                        dplyr::group_by(Date,treatment) %>%
                        dplyr::mutate(TPM_mgL = as.numeric(TPM_mgL)) %>% 
                        dplyr::summarise(across(everything(), list(mean)))
  

# CLearnace rate as Inorganic filtration rate / Particulate inorganic matter of the water



BioSamples <- biodep2 %>% # now to calculate more in depth metrics - requiring contingency statements to the blank water sample values for PIM, POM, etc.. 
                dplyr::select(!c('water_sample_time', 'vol_filtered_L')) %>% 
                dplyr::filter(sample_type %in% c('feces', 'pseudofeces')) 
# look at this data - there are cases when column ONLY exists when a feces or pseudo feces sample.. I have a work around, it isnt clean but it will get everything together

BioSamples_feces   <- BioSamples %>%  
                        dplyr::filter(sample_type %in% 'feces') %>%  # create data frame with just feces (Ejection rate constituents!)
                        dplyr::select(c(Date, treatment, animal_number, tank_ID, # select  all OVERLAPPING identifiers to properly merge
                                        ER_mghr, OER_mghr, IER_mghr)) # select all unique values (Ejection rate constituents) to add


BioSamples_merged  <- merge( (BioSamples %>%  filter(!sample_type %in% 'feces')%>% dplyr::select(!c(sample_type, ER_mghr, OER_mghr, IER_mghr))),
                              BioSamples_feces, by = c('Date', 'treatment', 'animal_number', 'tank_ID'))

# SPECIES STANDARDIZATION COEFFICIENT 
sp_COEF <- 0.62

biodep3 <- BioSamples_merged %>% 
  # IER, OER, IRR, ORR corected for species coefficient and animal dry weight
    dplyr::mutate(IER_correct = IER_mghr*(0.1/animal_dry_weight_mg)^sp_COEF) %>% 
    dplyr::mutate(IRR_correct = IRR_mghr*(0.1/animal_dry_weight_mg)^sp_COEF) %>%                
    dplyr::mutate(OER_correct = OER_mghr*(0.1/animal_dry_weight_mg)^sp_COEF) %>% 
    dplyr::mutate(ORR_correct = ORR_mghr*(0.1/animal_dry_weight_mg)^sp_COEF) %>% 
  # Clearance rate 
    dplyr::mutate(CR = case_when(
       treatment == 7.5 ~ (IRR_mghr + IER_mghr) / WaterSamples_dat_AVE$PIM_mgL_1[1],
       treatment == 8.0 ~ (IRR_mghr + IER_mghr) / WaterSamples_dat_AVE$PIM_mgL_1[2]
       )) %>% 
  # Filtration rate 
    dplyr::mutate(FR = case_when(
      treatment == 7.5 ~ CR * WaterSamples_dat_AVE$TPM_mgL_1[1],
      treatment == 8.0 ~ CR * WaterSamples_dat_AVE$TPM_mgL_1[2]
    )) %>% 
  
    dplyr::mutate(RR_correct = ORR_correct + IRR_correct) %>% 
  
    dplyr::mutate(p = ORR_correct / RR_correct ) %>% 
  
    dplyr::mutate(f =  case_when(
      treatment == 7.5 ~ WaterSamples_dat_AVE$Perc_ORG_1[1] / 100,
      treatment == 8.0 ~ WaterSamples_dat_AVE$Perc_ORG_1[2] / 100
    )) %>% 
  
    dplyr::mutate(SE = 1 - (p / f)) %>% 
  
    dplyr::mutate(IFR = IER_correct + IRR_correct) %>% 
  
    dplyr::mutate(CR_correct = case_when(
      treatment == 7.5 ~ IFR /  WaterSamples_dat_AVE$PIM_mgL_1[1],
      treatment == 8.0 ~ IFR /  WaterSamples_dat_AVE$PIM_mgL_1[2]
    )) %>% 
  
    dplyr::mutate(FR_correct = case_when(
      treatment == 7.5 ~ CR_correct * WaterSamples_dat_AVE$TPM_mgL_1[1],
      treatment == 8.0 ~ CR_correct *  WaterSamples_dat_AVE$TPM_mgL_1[2]
    )) %>% 
  
    dplyr::mutate(RR_Percent = (RR_correct/FR_correct)*100) %>% 
  
    dplyr::mutate(TIR = FR_correct -
                    RR_correct) %>% 
  
    dplyr::mutate(OFR = case_when(
      treatment == 7.5 ~ CR_correct * WaterSamples_dat_AVE$POM_mgL_1[1],
      treatment == 8.0 ~ CR_correct *  WaterSamples_dat_AVE$POM_mgL_1[2]
    )) %>% 
  
    dplyr::mutate(OIR = OFR - ORR_correct) %>% 
  
    dplyr::mutate(i = OIR / TIR) %>% 
  
    dplyr::mutate(AR = OIR - OER_correct) %>% 
  
    dplyr::mutate(AE = AR / OIR)
                                
View(biodep3)    

