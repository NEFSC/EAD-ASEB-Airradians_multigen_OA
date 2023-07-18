

## Load libraries

library(ggplot2)
library(dplyr)

# note! as of 12/19/22 it was discussed that we need to use the 'blanks' NOT the input for our blank POM  values! 


## set working directory

setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/") # Work computer
# setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis") # Work computer

## load data 

biodep_f1 <- read.csv(file="Data/Physiology/Biodeposition/F1/cumulative_raw/Raw_masterdata_biodeposition_F1.csv", header = TRUE) 
biodep_f2 <- read.csv(file="Data/Physiology/Biodeposition/F2/cumulative_raw/Raw_masterdata_biodeposition_F2.csv", header = TRUE) 

biodep    <- rbind(biodep_f1, biodep_f2)
nrow(biodep) == nrow(biodep_f1) + nrow(biodep_f2) # TRUE

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
# CR  == Clearance Rate: IFR/PIM of the water
# FR  == Filtration Rate: CR * TPM of the water
# %RR == RR/FR (amount rejected/total amount filtered)
# TIR == Total Ingestion Rate: FR - RR
# OFR == Organic Filtration Rate: CR * POM of the water
# OIR == Organic Ingestion Rate: OFR-ORR
# i   == Fraction of Organic Matter ingested: OIR/TIR (i.e. fraction of ingested material that was organic)
# AR  == Assimilation Rate: OIR-OER (rate of POM filtration - rate of POM rejection - rate of POM egestion)
# AE  == Assimilation Efficiency: AR/OIR


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Calculations PART1 :::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# 'biodep2' == the first steps, calculating the POM, TPM, PIM, for feces and pseudofeces (not corrected for b factor yet!)

# A few data edits before we proceed (1) remove NAs (2) examine for low pseodofeces samples


# (1) NAs: there is a single Na for ash filter weight - remove this
nrow(biodep) # 336 rows
biodep_f2$ash_filter_weight_mg # one instance of an NA
subset(biodep, !is.na(ash_filter_weight_mg))$ash_filter_weight_mg # now the NA is gone,use this call in the start of the pipeline below
biodep <- subset(biodep, !is.na(ash_filter_weight_mg)) # now the NA is gone,use this call in the start of the pipeline below
nrow(biodep) # 296 rows (removed just a single row - GOOD!)
biodep$ash_filter_weight_mg <- as.numeric(biodep$ash_filter_weight_mg)

# (2) pseudofeces below detection limit: we took notes of samples that had low to no pseudofeces - these will need to be accounted for in our rejection rate calculations 
biodep %>% # use the subset biodep (with the single NA removed)
  dplyr::filter(sample_type %in% 'pseudofeces') %>% # call pseudofeces samples
  #dplyr::filter(sample_type %in% 'feces') %>% # call pseudofeces samples
  dplyr::mutate(TPM_mg =(dry_filter_weight_mg - initial_filter_weight_mg)) %>% 
  dplyr::mutate(PIM_mg =(ash_filter_weight_mg - initial_filter_weight_mg)) %>%
  dplyr::mutate(POM_mg =(dry_filter_weight_mg - ash_filter_weight_mg)) %>% 
  dplyr::arrange(TPM_mg)  # new column on the  ash weight of the pseudofeces filters
# lowest ash weights are  TPM = 0.301; POM = 0.195; PIM = 0.106

# View(biodep2)
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
  
# View(biodep2)



# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Blanks as 'water_Blank' ::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# -------------

# why?
# these values grouped by Date and treatment will be called to corect for calculated values downstream in this script

# blanks - as 'water_Blank' - call to correct in biodep2 below...
WaterSamples_blank     <- biodep2 %>%  
  dplyr::select(c('Date', 'sample_type', 'treatment', 'water_sample_time', 'TPM_mgL',  'PIM_mgL',  'POM_mgL', 'Perc_INORG', 'Perc_ORG')) %>% 
  dplyr::filter(sample_type %in% 'water_Blank') %>% 
  
  dplyr::filter(!(Date %in% '20230201' & treatment %in% 8 & water_sample_time %in% '10:25')) %>%  # omit sample with obvious filter errors (noted in the worksheet!)
  dplyr::filter(!(Date %in% '20230201' & treatment %in% 7.5 & water_sample_time %in% '10:25')) %>%  # omit sample with obvious filter errors (noted in the worksheet!)
  dplyr::filter(!(Date %in% '20230201' & treatment %in% 7 & water_sample_time %in% '9:20')) %>%  # abnormally low % org - omitted (noted in the worksheet!)
  
  dplyr::filter(!(Date %in% '20230224' & treatment %in% 8 & water_sample_time %in% c('9:50','11:10'))) %>%  # omit sample with obvious filter errors (noted in the worksheet!)
  dplyr::filter(!(Date %in% '20230224' & treatment %in% 7.5 & water_sample_time %in% '11:10')) %>%  # omit sample with obvious filter errors (noted in the worksheet!)
  
  dplyr::group_by(Date,treatment)
 # View(WaterSamples_blank)
# slice(-1) # removes the first timestamp by group 
# mean for these blanks here...
WaterSamples_blank_AVE <- WaterSamples_blank %>% 
  dplyr::select(-c('sample_type', 'water_sample_time')) %>% # %>%  # omit to average across all group_by columns
  dplyr::group_by(Date,treatment) %>%
  dplyr::mutate(TPM_mgL = as.numeric(TPM_mgL)) %>% 
  dplyr::summarise(across(everything(), list(mean)))
# WaterSamples_blank_AVE #view your blanks! 


# 'water_Input' - call to correct in biodep2 below...
WaterSamples_input     <- biodep2 %>%  
  dplyr::select(c('Date', 'sample_type', 'treatment', 'water_sample_time', 'TPM_mgL',  'PIM_mgL',  'POM_mgL', 'Perc_INORG', 'Perc_ORG')) %>% 
  dplyr::filter(sample_type %in% 'water_Input') %>% 
  dplyr::filter(!(Date %in% '20221027' & treatment %in% 7.5 & TPM_mgL > 5)) %>%  # omit the time points 9:30 - 10:10am showing abnormally high particulate  
  dplyr::group_by(Date,treatment)
# View(WaterSamples_input)
# slice(-1) # removes the first timestamp by group 
# mean for these blanks here...

WaterSamples_input_AVE <- WaterSamples_input %>% 
  dplyr::select(-c('sample_type', 'water_sample_time')) %>% # %>%  # omit to average across all group_by columns
  dplyr::group_by(Date,treatment) %>%
  dplyr::mutate(TPM_mgL = as.numeric(TPM_mgL)) %>% 
  dplyr::summarise(across(everything(), list(mean)))
# WaterSamples_input_AVE #view your blanks!


water_samples_master <- rbind( (as.data.frame(WaterSamples_input_AVE %>% dplyr::mutate(Type = 'input'))),
                               (as.data.frame(WaterSamples_blank_AVE %>% dplyr::mutate(Type = 'blank'))) )

# WRITE CSV OF THE MASTER FILE
write.csv(water_samples_master, "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/Biodeposition_input_blank.csv")


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Calculations  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# now with our feces and pseudofeces values calculated, we want to fix this master sheet..
# due to values uniquely pertaining to these groups (feces and pseudo) 

BioSamples <- biodep2 %>% # 
                dplyr::select(!c('water_sample_time', 'vol_filtered_L')) %>% # omit unneeded columns
                dplyr::filter(sample_type %in% c('feces', 'pseudofeces')) # call the groups
BioSamples

# look at this data - there are cases when column ONLY exists when a feces or pseudo feces sample.. I have a work around, it isnt clean but it will get everything together

BioSamples_feces   <- BioSamples %>%  
                        dplyr::filter(sample_type %in% 'feces') %>%  # create data frame with just feces (Ejection rate constituents!)
                        dplyr::select(c(Date, treatment, animal_number, tank_ID, # select  all OVERLAPPING identifiers to properly merge
                                        ER_mghr, OER_mghr, IER_mghr)) # select all unique values (Ejection rate constituents) to add

BioSamples_merged  <- merge( (BioSamples %>%  filter(!sample_type %in% 'feces')%>% dplyr::select(!c(sample_type, ER_mghr, OER_mghr, IER_mghr))), # omit redundnat columns
                              BioSamples_feces, by = c('Date', 'treatment', 'animal_number')) %>% # merge with the feces dataframe by the unique identifiers
                      dplyr::rename(animal_dry_weight_g = animal_dry_weight_mg) # its not mg its actually g!!!
# View(BioSamples_merged)
# SPECIES STANDARDIZATION COEFFICIENT - change here when we calculate our own for the Bay scallop and potentially under the different OA treatments
sp_COEF <- 0.78 # standardization coefficient - calculated from CR data (review 'bvalue_noncorrectedcr')

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# FOR LOOP  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


dates             <- as.data.frame(unique(biodep$Date)) 
#F1s == 20220302, 20220923, and 20221027
#F2s == 20230201, 20230224
colnames(dates)   <- "Date"
Biodep_Master_F1s <- data.frame() # start dataframe 
Biodep_Master_F2s <- data.frame() # start dataframe 

#meanTDW <- mean(BioSamples_merged$animal_dry_weight_g) # 0.8582318
# NOTE: we are normalizing dry weights to 1.0 grams for IER, IRR, OER, and ORR (view below)

# F1 data!!  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# main diff here are the case_when for the first and second row in blanks_loop (as 7.5 and 8 respectively)
for (i in 1:3) { # only the F1 data for 20220302, 20220923, and 20221027
  date_loop       <- dates[i,] # only the F1 data for 20220302, 20220923, and 20221027
  blanks_loop     <- WaterSamples_blank_AVE %>% filter(Date %in% date_loop) %>% arrange(treatment)# filter blanks, sort as 7.5 then 8 for treatment pH
  waterinput_loop <- WaterSamples_input_AVE%>% filter(Date %in% date_loop) %>% arrange(treatment) # filter blanks, sort as 7.5 then 8 for treatment pH
  data_loop       <- BioSamples_merged %>%
                        dplyr::filter(Date %in% date_loop) %>% 
                      # IER == Inorganic Egestion Rate: PIM of feces/feces collection time
                        dplyr::mutate(IER_correct = IER_mghr*((1/animal_dry_weight_g)^sp_COEF)) %>% # previously 0.1/animal_dry_weight_g
                      # IRR == Inorganic Rejection Rate: PIM of pseudofeces/pseudofeces collection time
                        dplyr::mutate(IRR_correct = IRR_mghr*((1/animal_dry_weight_g)^sp_COEF)) %>%  # previously 0.1/animal_dry_weight_g
                      # OER == Organic Egestion Rate: POM of feces/feces collection time
                        dplyr::mutate(OER_correct = OER_mghr*((1/animal_dry_weight_g)^sp_COEF)) %>% # previously 0.1/animal_dry_weight_g
                      # ORR == Organic Rejection Rate: POM of pseudofeces/pseudofeces collection time
                        dplyr::mutate(ORR_correct = ORR_mghr*((1/animal_dry_weight_g)^sp_COEF)) %>% 
                      # CR  == Cleanrance Rate: IFR/PIM of the water
                        dplyr::mutate(CR = case_when(
                           treatment == 7.5 ~ (IRR_mghr + IER_mghr) / blanks_loop$PIM_mgL_1[1], # changed from waterinput_loop to blanks_loop on 12/19/22
                           treatment == 8.0 ~ (IRR_mghr + IER_mghr) / blanks_loop$PIM_mgL_1[2]  # changed from waterinput_loop to blanks_loop on 12/19/22
                           )) %>% 
                      # FR  == Filtration Rate: CR * TPM of the water
                        dplyr::mutate(FR = case_when(
                          treatment == 7.5 ~ CR * blanks_loop$TPM_mgL_1[1], # changed from waterinput_loop to blanks_loop on 12/19/22
                          treatment == 8.0 ~ CR * blanks_loop$TPM_mgL_1[2]  # changed from waterinput_loop to blanks_loop on 12/19/22
                        )) %>% 
                      # RR  == Rejection Rate: ORR+IRR
                        dplyr::mutate(RR_correct = ORR_correct + IRR_correct) %>% 
                      # p   == Fraction of Organic Rejected: ORR/RR (organic fraction of the pseudofeces)
                        dplyr::mutate(p = ORR_correct / RR_correct ) %>% 
                      # f   == POM available: Average POM of the water
                        dplyr::mutate(f =  case_when(
                          treatment == 7.5 ~ blanks_loop$Perc_ORG_1[1] / 100, # changed from waterinput_loop to blanks_loop on 12/19/22
                          treatment == 8.0 ~ blanks_loop$Perc_ORG_1[2] / 100 # changed from waterinput_loop to blanks_loop on 12/19/22
                        )) %>% 
                      # SE  == Selection Efficiency: 1-(p/f) (organic content of pseudofeces/organic content of the water)
                        dplyr::mutate(SE = 1 - (p / f)) %>% 
                      # IFR == Inorganic Filtration Rate: IER + IRR (PIM feces + PIM pseudofeces; i.e. total inorganic matter filtered/collection time)
                        dplyr::mutate(IFR = IER_correct + IRR_correct) %>% 
                      # CR  == Cleanrance Rate: IFR/PIM of the water
                        dplyr::mutate(CR_correct = case_when(
                          treatment == 7.5 ~ IFR /  blanks_loop$PIM_mgL_1[1], # changed from waterinput_loop to blanks_loop on 12/19/22
                          treatment == 8.0 ~ IFR /  blanks_loop$PIM_mgL_1[2]  # changed from waterinput_loop to blanks_loop on 12/19/22
                        )) %>% 
                      # FR  == Filtration Rate: CR * TPM of the water
                        dplyr::mutate(FR_correct = case_when(
                          treatment == 7.5 ~ CR_correct * blanks_loop$TPM_mgL_1[1], # changed from waterinput_loop to blanks_loop on 12/19/22
                          treatment == 8.0 ~ CR_correct *  blanks_loop$TPM_mgL_1[2] # changed from waterinput_loop to blanks_loop on 12/19/22
                        )) %>% 
                      # %RR == RR/FR (amount rejected/total amount filtered)
                        dplyr::mutate(RR_Percent = (RR_correct/FR_correct)*100) %>% 
                      # TIR == Total Ingestion Rate: FR - RR
                        dplyr::mutate(TIR = FR_correct -
                                        RR_correct) %>% 
                      # OFR == Organic FIltration Rate: CR * POM of the water
                        dplyr::mutate(OFR = case_when(
                          treatment == 7.5 ~ CR_correct * blanks_loop$POM_mgL_1[1], # changed from waterinput_loop to blanks_loop on 12/19/22
                          treatment == 8.0 ~ CR_correct *  blanks_loop$POM_mgL_1[2] # changed from waterinput_loop to blanks_loop on 12/19/22
                        )) %>% 
                      # ORI == Organic INgestion Rate: OFR-ORR
                        dplyr::mutate(OIR = OFR - ORR_correct) %>% 
                      # i   == Fraction of Organic Matter ingested: OIR/TIR (i.e. fraction of ingested material that was organic)
                        dplyr::mutate(i = OIR / TIR) %>% 
                      # AR  == Assimilation Rate: OIR-OER (rate of POM filtration - rate of POM rejection - rate of POM egestion)
                        dplyr::mutate(AR = OIR - OER_correct) %>% 
                      # AE  == Assimilation Efficiency: AR/OIR
                        dplyr::mutate(AE = AR / OIR) %>% 
                      # add column for uatm pCO2 treatment based on pH groups
                        dplyr::mutate(pCO2 = case_when(treatment == 8.0 ~ "500 μatm", treatment == 7.5 ~ "800 μatm"))

  df                <- data.frame(data_loop) # name dataframe for this single row
  Biodep_Master_F1s <- rbind(Biodep_Master_F1s,df) #bind to a cumulative list dataframe
  print(Biodep_Master_F1s) # print to monitor progress
                        
}

View(Biodep_Master_F1s) # look at your master file!

Biodep_Master_F1s_bad_data <- Biodep_Master_F1s %>% dplyr::filter(AE > 1 | AE < -1)

# F2 data!! ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::



# main diff here are the case_when for the first second and third rows in blanks_loop (as 7, 7.5 and 8 respectively)
for (i in 4:5) { # only the F2 data 20230201, 20230224
  date_loop       <- dates[i,] # only the F2 data 20230201, 20230224
  blanks_loop     <- WaterSamples_blank_AVE %>% filter(Date %in% date_loop) %>% arrange(treatment)# filter blanks, sort as 7.5 then 8 for treatment pH
  waterinput_loop <- WaterSamples_input_AVE%>% filter(Date %in% date_loop) %>% arrange(treatment) # filter blanks, sort as 7.5 then 8 for treatment pH
  data_loop       <- BioSamples_merged %>%
    dplyr::filter(Date %in% date_loop) %>% 
    # IER == Inorganic Egestion Rate: PIM of feces/feces collection time
    dplyr::mutate(IER_correct = IER_mghr*((1/animal_dry_weight_g)^sp_COEF)) %>% # previously 0.1/animal_dry_weight_g
    # IRR == Inorganic Rejection Rate: PIM of pseudofeces/pseudofeces collection time
    dplyr::mutate(IRR_correct = IRR_mghr*((1/animal_dry_weight_g)^sp_COEF)) %>%  # previously 0.1/animal_dry_weight_g
    # OER == Organic Egestion Rate: POM of feces/feces collection time
    dplyr::mutate(OER_correct = OER_mghr*((1/animal_dry_weight_g)^sp_COEF)) %>% # previously 0.1/animal_dry_weight_g
    # ORR == Organic Rejection Rate: POM of pseudofeces/pseudofeces collection time
    dplyr::mutate(ORR_correct = ORR_mghr*((1/animal_dry_weight_g)^sp_COEF)) %>% 
    # CR  == Cleanrance Rate: IFR/PIM of the water
    dplyr::mutate(CR = case_when(
      treatment == 7.0   ~ (IRR_mghr + IER_mghr) / blanks_loop$PIM_mgL_1[1], # changed from waterinput_loop to blanks_loop on 12/19/22
      treatment == 7.5 ~ (IRR_mghr + IER_mghr) / blanks_loop$PIM_mgL_1[2], # changed from waterinput_loop to blanks_loop on 12/19/22
      treatment == 8.0   ~ (IRR_mghr + IER_mghr) / blanks_loop$PIM_mgL_1[3]  # changed from waterinput_loop to blanks_loop on 12/19/22
    )) %>% 
    # FR  == Filtration Rate: CR * TPM of the water
    dplyr::mutate(FR = case_when(
      treatment == 7.0   ~ CR * blanks_loop$TPM_mgL_1[1], # changed from waterinput_loop to blanks_loop on 12/19/22
      treatment == 7.5 ~ CR * blanks_loop$TPM_mgL_1[2], # changed from waterinput_loop to blanks_loop on 12/19/22
      treatment == 8.0   ~ CR * blanks_loop$TPM_mgL_1[3]  # changed from waterinput_loop to blanks_loop on 12/19/22
    )) %>% 
    # RR  == Rejection Rate: ORR+IRR
    dplyr::mutate(RR_correct = ORR_correct + IRR_correct) %>% 
    # p   == Fraction of Organic Rejected: ORR/RR (organic fraction of the pseudofeces)
    dplyr::mutate(p = ORR_correct / RR_correct ) %>% 
    # f   == POM available: Average POM of the water
    dplyr::mutate(f =  case_when(
      treatment == 7.0   ~ blanks_loop$Perc_ORG_1[1] / 100, # changed from waterinput_loop to blanks_loop on 12/19/22
      treatment == 7.5 ~ blanks_loop$Perc_ORG_1[2] / 100, # changed from waterinput_loop to blanks_loop on 12/19/22
      treatment == 8.0   ~ blanks_loop$Perc_ORG_1[3] / 100 # changed from waterinput_loop to blanks_loop on 12/19/22
    )) %>% 
    # SE  == Selection Efficiency: 1-(p/f) (organic content of pseudofeces/organic content of the water)
    dplyr::mutate(SE = 1 - (p / f)) %>% 
    # IFR == Inorganic Filtration Rate: IER + IRR (PIM feces + PIM pseudofeces; i.e. total inorganic matter filtered/collection time)
    dplyr::mutate(IFR = IER_correct + IRR_correct) %>% 
    # CR  == Cleanrance Rate: IFR/PIM of the water
    dplyr::mutate(CR_correct = case_when(
      treatment == 7.0   ~ IFR /  blanks_loop$PIM_mgL_1[1], # changed from waterinput_loop to blanks_loop on 12/19/22
      treatment == 7.5 ~ IFR /  blanks_loop$PIM_mgL_1[2], # changed from waterinput_loop to blanks_loop on 12/19/22
      treatment == 8.0   ~ IFR /  blanks_loop$PIM_mgL_1[3]  # changed from waterinput_loop to blanks_loop on 12/19/22
    )) %>% 
    # FR  == Filtration Rate: CR * TPM of the water
    dplyr::mutate(FR_correct = case_when(
      treatment == 7.0   ~ CR_correct * blanks_loop$TPM_mgL_1[1], # changed from waterinput_loop to blanks_loop on 12/19/22
      treatment == 7.5 ~ CR_correct * blanks_loop$TPM_mgL_1[2], # changed from waterinput_loop to blanks_loop on 12/19/22
      treatment == 8.0   ~ CR_correct * blanks_loop$TPM_mgL_1[3] # changed from waterinput_loop to blanks_loop on 12/19/22
    )) %>% 
    # %RR == RR/FR (amount rejected/total amount filtered)
    dplyr::mutate(RR_Percent = (RR_correct/FR_correct)*100) %>% 
    # TIR == Total Ingestion Rate: FR - RR
    dplyr::mutate(TIR = FR_correct -
                    RR_correct) %>% 
    # OFR == Organic FIltration Rate: CR * POM of the water
    dplyr::mutate(OFR = case_when(
      treatment == 7.0   ~ CR_correct * blanks_loop$POM_mgL_1[1], # changed from waterinput_loop to blanks_loop on 12/19/22
      treatment == 7.5 ~ CR_correct * blanks_loop$POM_mgL_1[2], # changed from waterinput_loop to blanks_loop on 12/19/22
      treatment == 8.0   ~ CR_correct * blanks_loop$POM_mgL_1[3] # changed from waterinput_loop to blanks_loop on 12/19/22
    )) %>% 
    # ORI == Organic INgestion Rate: OFR-ORR
    dplyr::mutate(OIR = OFR - ORR_correct) %>% 
    # i   == Fraction of Organic Matter ingested: OIR/TIR (i.e. fraction of ingested material that was organic)
    dplyr::mutate(i = OIR / TIR) %>% 
    # AR  == Assimilation Rate: OIR-OER (rate of POM filtration - rate of POM rejection - rate of POM egestion)
    dplyr::mutate(AR = OIR - OER_correct) %>% 
    # AE  == Assimilation Efficiency: AR/OIR
    dplyr::mutate(AE = AR / OIR) %>% 
    # add column for uatm pCO2 treatment based on pH groups
    dplyr::mutate(pCO2 = case_when(treatment == 8.0 ~ "500 μatm", 
                                   treatment == 7.5 ~ "800 μatm",
                                   treatment == 7.0   ~ "1200 μatm"))
  
  df                <- data.frame(data_loop) # name dataframe for this single row
  Biodep_Master_F2s <- rbind(Biodep_Master_F2s,df) #bind to a cumulative list dataframe
  print(Biodep_Master_F2s) # print to monitor progress
  
}



View(Biodep_Master_F2s)

# Shannon meeting 3/31/2023 - omit values that are NOT between -1 and 1 for AR 
# note, two values for the F2s and no values for the F1s with this criteria - output the 'bad values' for the F2s and ommit in the master
Biodep_Master_F2s_bad_data <- Biodep_Master_F2s %>% dplyr::filter(AE > 1 | AE < -1)
Biodep_Master_F2s_OM       <- Biodep_Master_F2s %>% dplyr::filter(!(AE > 1 | AE < -1))

# WRITE CSV OF THE MASTER FILE
write.csv(Biodep_Master_F1s, "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/Biodeposition_master_F1.csv")
write.csv(Biodep_Master_F2s_OM, "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/Biodeposition_master_F2.csv")
write.csv(Biodep_Master_F2s_bad_data, "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/Biodeposition_outliers_F2.csv")

#write.csv(Biodep_Master, "C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/Biodeposition_master.csv")







# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# STATISTICS::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


library(car)


# F1s!!!! :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


Biodep_Master_F1 <- read.csv("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/Biodeposition_master_F1.csv", header = T)
Biodep_Master_F1 <- na.omit(Biodep_Master) # omit #6 pH7.5 on 20221027

# (1) First, run anova within date for all records (for looped!)
ANOVA_Dates       <- as.data.frame(unique(Biodep_Master_F1$Date)) # call a list to loop in 

AOVdf_total       <- data.frame() # start dataframe, this will be the master output
DF_loop           <- data.frame(matrix(nrow = 1, ncol = 12)) # create dataframe to save during for loop
colnames(DF_loop) <- c('Date', 'Metric', 'model', 'DF.num' , 'DF.denom', 'F_val','P_val', 'SigDif', 'ShapiroWilk', 'ResidNorm', 'Levenes', 'HomogVar') # names for comuns in the for loop
cols_m_loop       <- as.data.frame(c('SE','OIR','FR_correct', 'CR_correct', 'RR_Percent','OIR','AR','AE')) %>% `colnames<-`('biodep_meas')

for (i in 1:nrow(ANOVA_Dates)) {
  
  date_loop     <- as.character(ANOVA_Dates[i,])
  data_loop     <- Biodep_Master_F1 %>% 
                          dplyr::filter(Date %in% date_loop) %>% 
                          dplyr::select(Date, treatment, cols_m_loop$biodep_meas)
  
        for (m in 3:ncol(data_loop)) { # run anova and normality tests for each of these wihtin data i
        # high cholorphyll model
      
        AOVmod              <- aov(lm(data_loop[,m] ~ as.factor(data_loop$treatment)))
        DF_loop$Date        <- date_loop
        DF_loop$Metric      <- colnames(data_loop[m])
        DF_loop$model       <- 'one-way AOV; x ~ treatment'
        DF_loop$DF.num      <- summary(AOVmod)[[1]][["Df"]][1]
        DF_loop$DF.denom    <- summary(AOVmod)[[1]][["Df"]][2]
        DF_loop$F_val       <- summary(AOVmod)[[1]][["F value"]][1]
        DF_loop$P_val       <- summary(AOVmod)[[1]][["Pr(>F)"]][1]
        DF_loop$SigDif      <- if( (summary(AOVmod)[[1]][["Pr(>F)"]][1]) > 0.05) {
          'NO'} else {'YES'}
        DF_loop$ShapiroWilk <- shapiro.test(resid(AOVmod))[[2]]
        DF_loop$ResidNorm   <- if( shapiro.test(resid(AOVmod))[[2]] > 0.05) {
          'YES'} else {'NO'}
        DF_loop$Levenes     <- leveneTest(AOVmod)[[3]][[1]]
        DF_loop$HomogVar    <- if( leveneTest(AOVmod)[[3]][[1]] > 0.05) {
          'YES'} else {'NO'}
        
        # asign loop and cumulative output table
        df          <- data.frame(DF_loop) # name dataframe for this single row
        AOVdf_total <- rbind(AOVdf_total,DF_loop) #bind to a cumulative list dataframe
        print(AOVdf_total) # print to monitor progress
        }
}
View(AOVdf_total) # view all the anova tests within data 
# WRITE CSV OF THE MASTER FILE
write.csv(AOVdf_total, "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/F1_Biodeposition_ANOVA_table.csv")


# F2s!! :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


Biodep_Master_F2 <- read.csv("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/Biodeposition_master_F2.csv", header = T)
Biodep_Master_F2 <- na.omit(Biodep_Master_F2) # 

# (1) First, run anova within date for all records (for looped!)
ANOVA_Dates       <- as.data.frame(unique(Biodep_Master_F2$Date)) # call a list to loop in 

AOVdf_total       <- data.frame() # start dataframe, this will be the master output
DF_loop           <- data.frame(matrix(nrow = 1, ncol = 12)) # create dataframe to save during for loop
colnames(DF_loop) <- c('Date', 'Metric', 'model', 'DF.num' , 'DF.denom', 'F_val','P_val', 'SigDif', 'ShapiroWilk', 'ResidNorm', 'Levenes', 'HomogVar') # names for comuns in the for loop
cols_m_loop       <- as.data.frame(c('SE','OIR','FR_correct', 'CR_correct', 'RR_Percent','OIR','AR','AE')) %>% `colnames<-`('biodep_meas')

for (i in 1:nrow(ANOVA_Dates)) {
  
  date_loop     <- as.character(ANOVA_Dates[i,])
  data_loop     <- Biodep_Master_F2 %>% 
    dplyr::filter(Date %in% date_loop) %>% 
    dplyr::select(Date, treatment, cols_m_loop$biodep_meas)
  
  for (m in 3:ncol(data_loop)) { # run anova and normality tests for each of these wihtin data i
    # high cholorphyll model
    
    AOVmod              <- aov(lm(data_loop[,m] ~ as.factor(data_loop$treatment)))
    DF_loop$Date        <- date_loop
    DF_loop$Metric      <- colnames(data_loop[m])
    DF_loop$model       <- 'one-way AOV; x ~ treatment'
    DF_loop$DF.num      <- summary(AOVmod)[[1]][["Df"]][1]
    DF_loop$DF.denom    <- summary(AOVmod)[[1]][["Df"]][2]
    DF_loop$F_val       <- summary(AOVmod)[[1]][["F value"]][1]
    DF_loop$P_val       <- summary(AOVmod)[[1]][["Pr(>F)"]][1]
    DF_loop$SigDif      <- if( (summary(AOVmod)[[1]][["Pr(>F)"]][1]) > 0.05) {
      'NO'} else {'YES'}
    DF_loop$ShapiroWilk <- shapiro.test(resid(AOVmod))[[2]]
    DF_loop$ResidNorm   <- if( shapiro.test(resid(AOVmod))[[2]] > 0.05) {
      'YES'} else {'NO'}
    DF_loop$Levenes     <- leveneTest(AOVmod)[[3]][[1]]
    DF_loop$HomogVar    <- if( leveneTest(AOVmod)[[3]][[1]] > 0.05) {
      'YES'} else {'NO'}
    
    # asign loop and cumulative output table
    df          <- data.frame(DF_loop) # name dataframe for this single row
    AOVdf_total <- rbind(AOVdf_total,DF_loop) #bind to a cumulative list dataframe
    print(AOVdf_total) # print to monitor progress
  }
}
View(AOVdf_total) # view all the anova tests within data 
# WRITE CSV OF THE MASTER FILE
write.csv(AOVdf_total, "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/F2_Biodeposition_ANOVA_table.csv")



# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# PLOTTING :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# F1S ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

AE_boxplot <- Biodep_Master_F1 %>% 
  dplyr::mutate(Temperature = case_when(Date == '20220302' ~ '16C', 
                                        Date == '20220923' ~ '20C',
                                        Date == '20221027' ~ '13.3C')) %>%  
  ggplot(aes(pCO2 , AE , fill = pCO2)) +
  theme(panel.grid=element_blank()) +
  geom_boxplot(size=0.2, alpha=0.1, aes(fill=pCO2)) +
  scale_fill_manual(values=c("forestgreen","orange")) +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(jitter.width = 0.1)) +
  theme_classic() +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=6)) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, color="black", fill="white") +
  ggtitle("Assimilation Efficiency, F1 Scallops") +
  theme(axis.text.x=element_blank()) +
  facet_wrap(~Temperature)
# AE_boxplot


AR_boxplot <- Biodep_Master_F1 %>% 
  dplyr::mutate(Temperature = case_when(Date == '20220302' ~ '16C', 
                                        Date == '20220923' ~ '20C',
                                        Date == '20221027' ~ '13.3C')) %>%  
  # filter(!AE < 0) %>% 
  ggplot(aes(pCO2 , AR , fill = pCO2)) +
  theme(panel.grid=element_blank()) +
  geom_boxplot(size=0.2, alpha=0.1, aes(fill=pCO2)) +
  scale_fill_manual(values=c("forestgreen","orange")) +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(jitter.width = 0.1)) +
  theme_classic() +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=6)) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, color="black", fill="white") +
  ggtitle("Assimilation Rate, F1 Scallops") +
  theme(axis.text.x=element_blank()) +
  facet_wrap(~Temperature)
# AR_boxplot

OIR_boxplot <- Biodep_Master_F1 %>% 
  dplyr::mutate(Temperature = case_when(Date == '20220302' ~ '16C', 
                                        Date == '20220923' ~ '20C',
                                        Date == '20221027' ~ '13.3C')) %>%  
  #filter(!AE < 0) %>% 
  ggplot(aes(pCO2 , OIR , fill = pCO2)) +
  theme(panel.grid=element_blank()) +
  geom_boxplot(size=0.2, alpha=0.1, aes(fill=pCO2)) +
  scale_fill_manual(values=c("forestgreen","orange")) +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(jitter.width = 0.1)) +
  theme_classic() +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=6)) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, color="black", fill="white") +
  ggtitle("Organic Ingestion Rate, F1 Scallops") +
  theme(axis.text.x=element_blank()) +
  facet_wrap(~Temperature)
# OIR_boxplot


FR_boxplot <- Biodep_Master_F1 %>% 
  dplyr::mutate(Temperature = case_when(Date == '20220302' ~ '16C', 
                                        Date == '20220923' ~ '20C',
                                        Date == '20221027' ~ '13.3C')) %>%  
  #filter(!AE < 0) %>% 
  ggplot(aes(pCO2 , FR , fill = pCO2)) +
  theme(panel.grid=element_blank()) +
  geom_boxplot(size=0.2, alpha=0.1, aes(fill=pCO2)) +
  scale_fill_manual(values=c("forestgreen","orange")) +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(jitter.width = 0.1)) +
  theme_classic() +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=6)) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, color="black", fill="white") +
  ggtitle("Filtration Rate, F1 Scallops") +
  theme(axis.text.x=element_blank()) +
  facet_wrap(~Temperature)
# FR_boxplot

RR_boxplot <- Biodep_Master_F1 %>% 
  dplyr::mutate(Temperature = case_when(Date == '20220302' ~ '16C', 
                                        Date == '20220923' ~ '20C',
                                        Date == '20221027' ~ '13.3C')) %>%  
  #filter(!AE < 0) %>% 
  ggplot(aes(pCO2 , RR_Percent , fill = pCO2)) +
  theme(panel.grid=element_blank()) +
  geom_boxplot(size=0.2, alpha=0.1, aes(fill=pCO2)) +
  scale_fill_manual(values=c("forestgreen","orange")) +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(jitter.width = 0.1)) +
  theme_classic() +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=6)) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, color="black", fill="white") +
  ggtitle("Percent Rejection Rate, F1 Scallops") +
  theme(axis.text.x=element_blank()) +
  facet_wrap(~Temperature)
# RR_boxplot

SE_boxplot <- Biodep_Master_F1 %>% 
  dplyr::mutate(Temperature = case_when(Date == '20220302' ~ '16C', 
                                        Date == '20220923' ~ '20C',
                                        Date == '20221027' ~ '13.3C')) %>%  
  #filter(!AE < 0) %>% 
  ggplot(aes(pCO2 , SE , fill = pCO2)) +
  theme(panel.grid=element_blank()) +
  geom_boxplot(size=0.2, alpha=0.1, aes(fill=pCO2)) +
  scale_fill_manual(values=c("forestgreen","orange")) +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(jitter.width = 0.1)) +
  theme_classic() +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=6)) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, color="black", fill="white") +
  ggtitle("Selection Efficiency, F1 Scallops") +
  theme(axis.text.x=element_blank()) +
  facet_wrap(~Temperature)
# SE_boxplot

# output the plot 
library(ggpubr)
pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/F1_Biodeposition_Boxplots.pdf"), width = 10, height= 8)
ggarrange(SE_boxplot,RR_boxplot, OIR_boxplot, FR_boxplot, AE_boxplot, AR_boxplot)
dev.off()



# F2S ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

Biodep_Master_F2$pCO2 <- factor(Biodep_Master_F2$pCO2, 
                                levels = c("500 μatm", "800 μatm", "1200 μatm"))


AE_boxplot_F2 <- Biodep_Master_F2 %>% 
  ggplot(aes(pCO2 , AE , fill = pCO2)) +
  theme(panel.grid=element_blank()) +
  geom_boxplot(size=0.2, alpha=0.1, aes(fill=pCO2)) +
  scale_fill_manual(values=c("forestgreen","orange", "purple")) +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(jitter.width = 0.1)) +
  theme_classic() +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=6)) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, color="black", fill="white") +
  ggtitle("Assimilation Efficiency, F2 Scallops") +
  theme(axis.text.x=element_blank()) +
  facet_wrap(~Date)
# AE_boxplot


AR_boxplot_F2 <- Biodep_Master_F2 %>% 
  ggplot(aes(pCO2 , AR , fill = pCO2)) +
  theme(panel.grid=element_blank()) +
  geom_boxplot(size=0.2, alpha=0.1, aes(fill=pCO2)) +
  scale_fill_manual(values=c("forestgreen","orange", "purple")) +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(jitter.width = 0.1)) +
  theme_classic() +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=6)) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, color="black", fill="white") +
  ggtitle("Assimilation Rate, F2 Scallops") +
  theme(axis.text.x=element_blank()) +
  facet_wrap(~Date)
# AR_boxplot

OIR_boxplot_F2 <- Biodep_Master_F2 %>% 
  ggplot(aes(pCO2 , OIR , fill = pCO2)) +
  theme(panel.grid=element_blank()) +
  geom_boxplot(size=0.2, alpha=0.1, aes(fill=pCO2)) +
  scale_fill_manual(values=c("forestgreen","orange", "purple")) +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(jitter.width = 0.1)) +
  theme_classic() +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=6)) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, color="black", fill="white") +
  ggtitle("Organic Ingestion Rate, F2 Scallops") +
  theme(axis.text.x=element_blank()) +
  facet_wrap(~Date)
# OIR_boxplot


FR_boxplot_F2 <- Biodep_Master_F2 %>% 
  ggplot(aes(pCO2 , FR , fill = pCO2)) +
  theme(panel.grid=element_blank()) +
  geom_boxplot(size=0.2, alpha=0.1, aes(fill=pCO2)) +
  scale_fill_manual(values=c("forestgreen","orange", "purple")) +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(jitter.width = 0.1)) +
  theme_classic() +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=6)) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, color="black", fill="white") +
  ggtitle("Filtration Rate, F2 Scallops") +
  theme(axis.text.x=element_blank()) +
  facet_wrap(~Date)
# FR_boxplot

RR_boxplot_F2 <- Biodep_Master_F2 %>% 
  ggplot(aes(pCO2 , RR_Percent , fill = pCO2)) +
  theme(panel.grid=element_blank()) +
  geom_boxplot(size=0.2, alpha=0.1, aes(fill=pCO2)) +
  scale_fill_manual(values=c("forestgreen","orange", "purple")) +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(jitter.width = 0.1)) +
  theme_classic() +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=6)) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, color="black", fill="white") +
  ggtitle("Percent Rejection Rate, F2 Scallops") +
  theme(axis.text.x=element_blank()) +
  facet_wrap(~Date)
# RR_boxplot

SE_boxplot_F2 <- Biodep_Master_F2 %>% 
  ggplot(aes(pCO2 , SE , fill = pCO2)) +
  theme(panel.grid=element_blank()) +
  geom_boxplot(size=0.2, alpha=0.1, aes(fill=pCO2)) +
  scale_fill_manual(values=c("forestgreen","orange", "purple")) +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(jitter.width = 0.1)) +
  theme_classic() +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=6)) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, color="black", fill="white") +
  ggtitle("Selection Efficiency, F2 Scallops") +
  theme(axis.text.x=element_blank()) +
  facet_wrap(~Date)
# SE_boxplot

# output the plot 
library(ggpubr)
pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/F2_Biodeposition_Boxplots.pdf"), width = 10, height= 8)
ggarrange(SE_boxplot_F2,
          RR_boxplot_F2, 
          OIR_boxplot_F2, 
          FR_boxplot_F2, 
          AE_boxplot_F2, 
          AR_boxplot_F2)
dev.off()










































CR_correct_boxplot <- Biodep_Master %>% 
  dplyr::mutate(Temperature = case_when(Date == '20220302' ~ '16C', 
                                        Date == '20220923' ~ '20C',
                                        Date == '20221027' ~ '13.3C')) %>%  
  #filter(!AE < 0) %>% 
  ggplot(aes(pCO2 , CR_correct , fill = pCO2)) +
  theme(panel.grid=element_blank()) +
  geom_boxplot(size=0.2, alpha=0.1, aes(fill=pCO2)) +
  scale_fill_manual(values=c("forestgreen","orange")) +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(jitter.width = 0.1)) +
  theme_classic() +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=6)) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, color="black", fill="white") +
  ggtitle("Clearance Rate, F1 Scallops") +
  theme(axis.text.x=element_blank()) +
  facet_wrap(~Temperature)
CR_correct_boxplot
 





#  KAITE'S mas POSTER FIGURE FOR MAS 2022

Biodep_Master_16_20C <- Biodep_Master %>% 
  dplyr::mutate(Temperature = case_when(Date == '20220302' ~ '16C', 
                                        Date == '20220923' ~ '20C',
                                        Date == '20221027' ~ '13.3C')) %>%  
  dplyr::filter(Temperature %in% c('16C', '20C'))



AE_boxplot <- Biodep_Master_16_20C %>% 
  ggplot(aes(pCO2 , AE , fill = pCO2)) +
  theme(panel.grid=element_blank()) +
  geom_boxplot(size=0.2, alpha=0.1, aes(fill=pCO2)) +
  scale_fill_manual(values=c("forestgreen","orange")) +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(jitter.width = 0.1)) +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12)) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, color="black", fill="white") +
  ggtitle("Assimilation Efficiency") +
  theme(axis.text.x=element_blank(),axis.title.x = element_blank()) +
  facet_wrap(~Temperature)
# AE_boxplot

OIR_boxplot <- Biodep_Master_16_20C %>% 
  ggplot(aes(pCO2 , OIR , fill = pCO2)) +
  theme(panel.grid=element_blank()) +
  geom_boxplot(size=0.2, alpha=0.1, aes(fill=pCO2)) +
  scale_fill_manual(values=c("forestgreen","orange")) +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(jitter.width = 0.1)) +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12)) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, color="black", fill="white") +
  ggtitle("Organic Ingestion Rate") +
  theme(axis.text.x=element_blank(),axis.title.x = element_blank(),legend.position = "none") +
  facet_wrap(~Temperature)
# OIR_boxplot


FR_boxplot <- Biodep_Master_16_20C %>% 
  ggplot(aes(pCO2 , FR , fill = pCO2)) +
  theme(panel.grid=element_blank()) +
  geom_boxplot(size=0.2, alpha=0.1, aes(fill=pCO2)) +
  scale_fill_manual(values=c("forestgreen","orange")) +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(jitter.width = 0.1)) +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12)) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, color="black", fill="white") +
  ggtitle("Filtration Rate") +
  theme(axis.text.x=element_blank(),axis.title.x = element_blank(),legend.position = "none") +
  facet_wrap(~Temperature)
# FR_boxplot

RR_boxplot <- Biodep_Master_16_20C %>% 
  ggplot(aes(pCO2 , RR_Percent , fill = pCO2)) +
  theme(panel.grid=element_blank()) +
  geom_boxplot(size=0.2, alpha=0.1, aes(fill=pCO2)) +
  scale_fill_manual(values=c("forestgreen","orange")) +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(jitter.width = 0.1)) +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12)) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, color="black", fill="white") +
  ggtitle("Percent Rejection Rate") +
  theme(axis.text.x=element_blank(),axis.title.x = element_blank(),legend.position = "none") +
  facet_wrap(~Temperature)
# RR_boxplot

SE_boxplot <- Biodep_Master_16_20C %>% 
  ggplot(aes(pCO2 , SE , fill = pCO2)) +
  theme(panel.grid=element_blank()) +
  geom_boxplot(size=0.2, alpha=0.1, aes(fill=pCO2)) +
  scale_fill_manual(values=c("forestgreen","orange")) +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(jitter.width = 0.1)) +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12)) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, color="black", fill="white") +
  ggtitle("Selection Efficiency") +
  theme(axis.text.x=element_blank(),axis.title.x = element_blank(),legend.position = "none") +
  facet_wrap(~Temperature)


pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/Biodeposition_Boxplots_MAS_poster.pdf"), width = 20, height= 4)
ggarrange(SE_boxplot,RR_boxplot, OIR_boxplot, FR_boxplot, nrow = 1, ncol = 5)
dev.off()

pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/AE_Boxplot_MAS_poster.pdf"), width = 5, height= 4)
ggarrange(AE_boxplot, nrow = 1, ncol = 1)
dev.off()