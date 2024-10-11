

## Load libraries

library(ggplot2)
library(dplyr)
library(Rmisc)

# note! as of 12/19/22 it was discussed that we need to use the 'blanks' NOT the input for our blank POM  values! 


## set working directory

setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/") # Work computer
setwd("C:/Users/samuel.gurr/Documents/Github_repositories/EAD-ASEB-Airradians_multigen_OA/RAnalysis") # Work computer

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
nrow(biodep) # 405 rows
nrow(biodep %>% dplyr::filter(ash_filter_weight_mg %in% NA)) #instance of an NA that IS NOT just NA for water_Input - N = 58
subset(biodep, !is.na(ash_filter_weight_mg))$ash_filter_weight_mg # now the NA is gone,use this call in the start of the pipeline below
biodep <- subset(biodep, !is.na(ash_filter_weight_mg)) # now the NA is gone,use this call in the start of the pipeline below
nrow(biodep) # 347 rows  == 405-58 GREAT!
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
  dplyr::filter(!(Date %in% '20230201' & treatment %in% 8 & water_sample_time %in% '9:45')) %>%  # omit sample with obvious error

  dplyr::group_by(Date,treatment)
 # View(WaterSamples_blank)



Blank_plotting <- WaterSamples_blank %>% dplyr::select(!c(sample_type, water_sample_time)) %>% 
                  tidyr::pivot_longer(!c(Date,treatment), 
                                      names_to = "measurement", values_to = "value") %>% 
                  Rmisc::summarySEwithin(measurevar="value", withinvars=c("Date", "treatment","measurement"), idvar="Date")

Perc_TSM <- Blank_plotting %>%  
  dplyr::filter(measurement %in% c('Perc_ORG', 'Perc_INORG')) %>% 
  ggplot(aes(x=treatment, y=value/100, fill=measurement)) +
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=value-se, ymax=value+se)) +
  coord_cartesian(ylim=c(0,1)) +
  scale_fill_manual(values=c("#CCCCCC","#FFFFFF")) +
  # scale_y_continuous(breaks=seq(4:0.25)) +
  theme_classic() +
  ylab("Proportion") +
  ggtitle("Biodeposition: TSM - Percent inorgnaic and organic material") +
  geom_hline(yintercept=38) +
  facet_wrap(~Date)

Total_TSM <-Blank_plotting %>%  
  dplyr::filter(measurement %in% 'TPM_mgL') %>% 
  ggplot(aes(x=treatment, y=value, group=1)) +
  geom_errorbar(width=.1, aes(ymin=value-se, ymax=value+se)) +
  geom_point(shape=21, size=3, fill="white")  +
  theme_classic() +
  ylab("TPM_mgL") +
  ggtitle("Biodeposition: Total particulate material") +
  facet_wrap(~Date)

ggpubr::ggarrange(Perc_TSM, Total_TSM, nrow=2)

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
#F2s == 20230201, 20230224, and 20230328
colnames(dates)   <- "Date"
Biodep_Master_F1s <- data.frame() # start dataframe 
Biodep_Master_F2s <- data.frame() # start dataframe 

#meanTDW <- mean(BioSamples_merged$animal_dry_weight_g) # 0.8582318
# NOTE: we are normalizing dry weights to 1.0 grams for IER, IRR, OER, and ORR (view below)

# F1 data!!  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

F1sp_coef_8  <- 0.877 # b factor CR Low 0.877 
F1sp_coef_75 <- 1.13 # b factor CR Moderate 1.13


# main diff here are the case_when for the first and second row in blanks_loop (as 7.5 and 8 respectively)
for (i in 1:3) { # only the F1 data for 20220302, 20220923, and 20221027
  date_loop       <- dates[i,] # only the F1 data for 20220302, 20220923, and 20221027
  blanks_loop     <- WaterSamples_blank_AVE %>% filter(Date %in% date_loop) %>% arrange(treatment)# filter blanks, sort as 7.5 then 8 for treatment pH
  waterinput_loop <- WaterSamples_input_AVE%>% filter(Date %in% date_loop) %>% arrange(treatment) # filter blanks, sort as 7.5 then 8 for treatment pH
  data_loop       <- BioSamples_merged %>%
                        dplyr::filter(Date %in% date_loop) %>% 
                      #TRETAMENT SPECIFIC B FACTOR IN THE METRIC BELOW
                      # IER == Inorganic Egestion Rate: PIM of feces/feces collection time
                        dplyr::mutate(IER_correct = case_when( # prevviously IER_mghr*((1/animal_dry_weight_g)^sp_COEF)) %>% # previously 0.1/animal_dry_weight_g
                          treatment == 7.5 ~ IER_mghr*((1/animal_dry_weight_g)^F1sp_coef_75), # plotted below based on SMA log(CR) log(TDW)
                          treatment == 8.0 ~ IER_mghr*((1/animal_dry_weight_g)^F1sp_coef_8)  # plotted below based on SMA log(CR) log(TDW)
                        )) %>%
    
                      # IRR == Inorganic Rejection Rate: PIM of pseudofeces/pseudofeces collection time
                        dplyr::mutate(IRR_correct = case_when( # previously IRR_mghr*((1/animal_dry_weight_g)^sp_COEF)) %>%  # previously 0.1/animal_dry_weight_g
                          treatment == 7.5 ~ IRR_mghr*((1/animal_dry_weight_g)^F1sp_coef_75), # plotted below based on SMA log(CR) log(TDW)
                          treatment == 8.0 ~ IRR_mghr*((1/animal_dry_weight_g)^F1sp_coef_8)  # plotted below based on SMA log(CR) log(TDW)                        )) %>% 
                        )) %>% 
    
                        # OER == Organic Egestion Rate: POM of feces/feces collection time
                        dplyr::mutate(OER_correct = case_when( # prevviously OER_mghr*((1/animal_dry_weight_g)^sp_COEF)) %>% # previously 0.1/animal_dry_weight_g
                          treatment == 7.5 ~ OER_mghr*((1/animal_dry_weight_g)^F1sp_coef_75), # plotted below based on SMA log(CR) log(TDW)
                          treatment == 8.0 ~ OER_mghr*((1/animal_dry_weight_g)^F1sp_coef_8)  # plotted below based on SMA log(CR) log(TDW)                        )) %>% 
                        )) %>% 
    
                        # ORR == Organic Rejection Rate: POM of pseudofeces/pseudofeces collection time
                        dplyr::mutate(ORR_correct = case_when( # prevviously ORR_mghr*((1/animal_dry_weight_g)^sp_COEF)) %>% 
                          treatment == 7.5 ~ ORR_mghr*((1/animal_dry_weight_g)^F1sp_coef_75), # plotted below based on SMA log(CR) log(TDW)
                          treatment == 8.0 ~ ORR_mghr*((1/animal_dry_weight_g)^F1sp_coef_8)  # plotted below based on SMA log(CR) log(TDW)                        )) %>% 
                        )) %>% 
                        # DO NOT USE B FACTOR FOR THE REMAING, ALL DONE
    
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
nrow(Biodep_Master_F1s_bad_data) # no data considered to be the result of error!


Biodep_Master_F1s %>% 
  ggplot(aes(x = log(animal_dry_weight_g), y = log(CR))) +
  geom_point() +
  ggpmisc::stat_ma_line(method = "SMA") + # model 2 regression Standard major axis!
  ggpmisc::stat_ma_eq(ggpmisc::use_label(c("eq", "n", "R2"))) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+ 
  scale_x_continuous(name ="log10_TDW; in mm") +
  scale_y_continuous(name ="log10_ClearanceRate)") +
  theme_classic() +
  theme(legend.position="none") +
  ggtitle("F1 Generation: log10_CI = log10_a + (b.factor * log10_TDW)") +
  facet_wrap(~pCO2)

# b factor CR Low 0.877
# b factor CR moderate  1.13

Biodep_Master_F1s %>% 
  ggplot(aes(x = log(animal_dry_weight_g), y = log(CR))) +
  geom_point() +
  ggpmisc::stat_ma_line(method = "SMA") + # model 2 regression Standard major axis!
  ggpmisc::stat_ma_eq(ggpmisc::use_label(c("eq", "n", "R2"))) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+ 
  scale_x_continuous(name ="log10_TDW; in mm") +
  scale_y_continuous(name ="log10_ClearanceRate)") +
  theme_classic() +
  theme(legend.position="none") +
  ggtitle("F1 Generation: log10_CI = log10_a + (b.factor * log10_TDW)")

# b factor CR Low 0.981

# F2 data!! ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
View(BioSamples_merged)
  
F2sp_coef_8   <- 0.301 # b factor CR Low .303
F2sp_coef_75  <- 0.596 # b factor CR moderate  0.596
F2sp_coef_7   <- 0.489 # b factor CR moderate  0.445

# main diff here are the case_when for the first second and third rows in blanks_loop (as 7, 7.5 and 8 respectively)
for (i in 4:6) { # only the F2 data 20230201, 20230224
  date_loop       <- dates[i,] # only the F2 data 20230201, 20230224
  blanks_loop     <- WaterSamples_blank_AVE %>% filter(Date %in% date_loop) %>% arrange(treatment)# filter blanks, sort as 7.5 then 8 for treatment pH
  waterinput_loop <- WaterSamples_input_AVE%>% filter(Date %in% date_loop) %>% arrange(treatment) # filter blanks, sort as 7.5 then 8 for treatment pH
  data_loop       <- BioSamples_merged %>%
    dplyr::filter(Date %in% date_loop) %>% 
    #TRETAMENT SPECIFIC B FACTOR IN THE METRIC BELOW
    # IER == Inorganic Egestion Rate: PIM of feces/feces collection time
    dplyr::mutate(IER_correct = case_when( # prevviously IER_mghr*((1/animal_dry_weight_g)^sp_COEF)) %>% # previously 0.1/animal_dry_weight_g\
      treatment == 7.0 ~ IER_mghr*((1/animal_dry_weight_g)^F2sp_coef_7), # plotted below based on SMA log(CR) log(TDW)
      treatment == 7.5 ~ IER_mghr*((1/animal_dry_weight_g)^F2sp_coef_75), # plotted below based on SMA log(CR) log(TDW)
      treatment == 8.0 ~ IER_mghr*((1/animal_dry_weight_g)^F2sp_coef_8)  # plotted below based on SMA log(CR) log(TDW)
    )) %>%
    
    # IRR == Inorganic Rejection Rate: PIM of pseudofeces/pseudofeces collection time
    dplyr::mutate(IRR_correct = case_when( # previously IRR_mghr*((1/animal_dry_weight_g)^sp_COEF)) %>%  # previously 0.1/animal_dry_weight_g
      treatment == 7.0 ~ IRR_mghr*((1/animal_dry_weight_g)^F2sp_coef_7), # plotted below based on SMA log(CR) log(TDW)
      treatment == 7.5 ~ IRR_mghr*((1/animal_dry_weight_g)^F2sp_coef_75), # plotted below based on SMA log(CR) log(TDW)
      treatment == 8.0 ~ IRR_mghr*((1/animal_dry_weight_g)^F2sp_coef_8)  # plotted below based on SMA log(CR) log(TDW)                        )) %>% 
    )) %>% 
    
    # OER == Organic Egestion Rate: POM of feces/feces collection time
    dplyr::mutate(OER_correct = case_when( # prevviously OER_mghr*((1/animal_dry_weight_g)^sp_COEF)) %>% # previously 0.1/animal_dry_weight_g
      treatment == 7.0 ~ OER_mghr*((1/animal_dry_weight_g)^F2sp_coef_7), # plotted below based on SMA log(CR) log(TDW)
      treatment == 7.5 ~ OER_mghr*((1/animal_dry_weight_g)^F2sp_coef_75), # plotted below based on SMA log(CR) log(TDW)
      treatment == 8.0 ~ OER_mghr*((1/animal_dry_weight_g)^F2sp_coef_8)  # plotted below based on SMA log(CR) log(TDW)                        )) %>% 
    )) %>% 
    
    # ORR == Organic Rejection Rate: POM of pseudofeces/pseudofeces collection time
    dplyr::mutate(ORR_correct = case_when( # prevviously ORR_mghr*((1/animal_dry_weight_g)^sp_COEF)) %>% 
      treatment == 7.0 ~ ORR_mghr*((1/animal_dry_weight_g)^F2sp_coef_7), # plotted below based on SMA log(CR) log(TDW)
      treatment == 7.5 ~ ORR_mghr*((1/animal_dry_weight_g)^F2sp_coef_75), # plotted below based on SMA log(CR) log(TDW)
      treatment == 8.0 ~ ORR_mghr*((1/animal_dry_weight_g)^F2sp_coef_8)  # plotted below based on SMA log(CR) log(TDW)                        )) %>% 
    )) %>% 
    # DO NOT USE B FACTOR FOR THE REMAING, ALL DONE
    
    # CR  == Cleanrance Rate: IFR/PIM of the water
    dplyr::mutate(CR = case_when(
      treatment == 7.0 ~ (IRR_mghr + IER_mghr) / blanks_loop$PIM_mgL_1[1], # changed from waterinput_loop to blanks_loop on 12/19/22
      treatment == 7.5 ~ (IRR_mghr + IER_mghr) / blanks_loop$PIM_mgL_1[2], # changed from waterinput_loop to blanks_loop on 12/19/22
      treatment == 8.0 ~ (IRR_mghr + IER_mghr) / blanks_loop$PIM_mgL_1[3]  # changed from waterinput_loop to blanks_loop on 12/19/22
    )) %>% 
    # FR  == Filtration Rate: CR * TPM of the water
    dplyr::mutate(FR = case_when(
      treatment == 7.0 ~ CR * blanks_loop$TPM_mgL_1[1], # changed from waterinput_loop to blanks_loop on 12/19/22
      treatment == 7.5 ~ CR * blanks_loop$TPM_mgL_1[2], # changed from waterinput_loop to blanks_loop on 12/19/22
      treatment == 8.0 ~ CR * blanks_loop$TPM_mgL_1[3]  # changed from waterinput_loop to blanks_loop on 12/19/22
    )) %>% 
    # RR  == Rejection Rate: ORR+IRR
    dplyr::mutate(RR_correct = ORR_correct + IRR_correct) %>% 
    # p   == Fraction of Organic Rejected: ORR/RR (organic fraction of the pseudofeces)
    dplyr::mutate(p = ORR_correct / RR_correct ) %>% 
    # f   == POM available: Average POM of the water
    dplyr::mutate(f =  case_when(
      treatment == 7.0 ~ blanks_loop$Perc_ORG_1[1] / 100, # changed from waterinput_loop to blanks_loop on 12/19/22
      treatment == 7.5 ~ blanks_loop$Perc_ORG_1[2] / 100, # changed from waterinput_loop to blanks_loop on 12/19/22
      treatment == 8.0 ~ blanks_loop$Perc_ORG_1[3] / 100 # changed from waterinput_loop to blanks_loop on 12/19/22
    )) %>% 
    # SE  == Selection Efficiency: 1-(p/f) (organic content of pseudofeces/organic content of the water)
    dplyr::mutate(SE = 1 - (p / f)) %>% 
    # IFR == Inorganic Filtration Rate: IER + IRR (PIM feces + PIM pseudofeces; i.e. total inorganic matter filtered/collection time)
    dplyr::mutate(IFR = IER_correct + IRR_correct) %>% 
    # CR  == Cleanrance Rate: IFR/PIM of the water
    dplyr::mutate(CR_correct = case_when(
      treatment == 7.0 ~ IFR /  blanks_loop$PIM_mgL_1[1], # changed from waterinput_loop to blanks_loop on 12/19/22
      treatment == 7.5 ~ IFR /  blanks_loop$PIM_mgL_1[2], # changed from waterinput_loop to blanks_loop on 12/19/22
      treatment == 8.0 ~ IFR /  blanks_loop$PIM_mgL_1[3]  # changed from waterinput_loop to blanks_loop on 12/19/22
    )) %>% 
    # FR  == Filtration Rate: CR * TPM of the water
    dplyr::mutate(FR_correct = case_when(
      treatment == 7.0 ~ CR_correct * blanks_loop$TPM_mgL_1[1], # changed from waterinput_loop to blanks_loop on 12/19/22
      treatment == 7.5 ~ CR_correct * blanks_loop$TPM_mgL_1[2], # changed from waterinput_loop to blanks_loop on 12/19/22
      treatment == 8.0 ~ CR_correct * blanks_loop$TPM_mgL_1[3] # changed from waterinput_loop to blanks_loop on 12/19/22
    )) %>% 
    # %RR == RR/FR (amount rejected/total amount filtered)
    dplyr::mutate(RR_Percent = (RR_correct/FR_correct)*100) %>% 
    # TIR == Total Ingestion Rate: FR - RR
    dplyr::mutate(TIR = FR_correct -
                    RR_correct) %>% 
    # OFR == Organic FIltration Rate: CR * POM of the water
    dplyr::mutate(OFR = case_when(
      treatment == 7.0 ~ CR_correct * blanks_loop$POM_mgL_1[1], # changed from waterinput_loop to blanks_loop on 12/19/22
      treatment == 7.5 ~ CR_correct * blanks_loop$POM_mgL_1[2], # changed from waterinput_loop to blanks_loop on 12/19/22
      treatment == 8.0 ~ CR_correct * blanks_loop$POM_mgL_1[3] # changed from waterinput_loop to blanks_loop on 12/19/22
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
                                   treatment == 7.0 ~ "1200 μatm"))
  
  df                <- data.frame(data_loop) # name dataframe for this single row
  Biodep_Master_F2s <- rbind(Biodep_Master_F2s,df) #bind to a cumulative list dataframe
  print(Biodep_Master_F2s) # print to monitor progress
  
}
# View(Biodep_Master_F2s)
Biodep_Master_F2s %>% 
  # dplyr::filter(!pCO2 %in% '1200 μatm') %>% 
  ggplot(aes(x = log(animal_dry_weight_g), y = log(CR))) +
  geom_point() +
  ggpmisc::stat_ma_line(method = "SMA") + # model 2 regression Standard major axis!
  ggpmisc::stat_ma_eq(ggpmisc::use_label(c("eq", "n", "R2"))) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+ 
  scale_x_continuous(name ="log10_TDW; in mm") +
  scale_y_continuous(name ="log10_ClearanceRate)") +
  theme_classic() +
  theme(legend.position="none") +
  ggtitle("F2 Generation: log10_CI = log10_a + (b.factor * log10_TDW)") +
  facet_wrap(~pCO2)

# b factor CR Low .301
# b factor CR moderate  0.596
# b factor CR high  0.489

Biodep_Master_F2s %>% 
  # dplyr::filter(!pCO2 %in% '1200 μatm') %>% 
  ggplot(aes(x = log(animal_dry_weight_g), y = log(CR))) +
  geom_point() +
  ggpmisc::stat_ma_line(method = "SMA") + # model 2 regression Standard major axis!
  ggpmisc::stat_ma_eq(ggpmisc::use_label(c("eq", "n", "R2"))) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+ 
  scale_x_continuous(name ="log10_TDW; in mm") +
  scale_y_continuous(name ="log10_ClearanceRate)") +
  theme_classic() +
  theme(legend.position="none") +
  ggtitle("F2 Generation: log10_CI = log10_a + (b.factor * log10_TDW)") 

# b factor CR Low 0.46

# Bding for TDW master b factor plots 
nrow(rbind(Biodep_Master_F1s, Biodep_Master_F2s)) == sum(nrow(Biodep_Master_F1s), nrow(Biodep_Master_F2s))
Biodep_Master_F1_and_F2s <- rbind(
                                  (Biodep_Master_F1s %>% dplyr::mutate(Gen = 'F1')), 
                                  (Biodep_Master_F2s %>% dplyr::mutate(Gen = 'F2'))
                                )

TDW_CI_b.factor_LowVMod <- Biodep_Master_F1_and_F2s %>% 
                            dplyr::filter(!pCO2 %in% '1200 μatm') %>% 
                            dplyr::mutate(Gen_pCO2 = paste0(Gen,'_',pCO2)) %>% 
                            ggplot(aes(x=log10(as.numeric(animal_dry_weight_g)), 
                                       y=log10(as.numeric(CR)), 
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
                            scale_y_continuous(name ="log10_CI; CI in μmol L-1 hr-1)") +
                            # ylim(-1.5,2) +
                            theme_classic() +
                            theme(legend.position="none",
                                  element_line(linewidth = 0.5, color = 'black'),
                                  axis.title.y=element_text(size=12),
                                  axis.text.x=element_text(size=(12)),
                                  axis.text.y=element_text(size=(12))) + # legend.position="none",
                            ggtitle("Allometric scaling: log10_CI = log10_a + (b.factor * log10_TDW)") 
TDW_CI_b.factor_LowVMod_facetted  <- TDW_CI_b.factor_LowVMod + facet_wrap(~Gen)


pdf(paste0(filename = "C:/Users/samuel.gurr/Documents/Github_repositories/EAD-ASEB-Airradians_multigen_OA/RAnalysis/Output/Biodeposition/allometric_scaling/F1_F2_CI_bFactor_TDW_LowvMod.pdf"), 
    width = 5, height = 10)
print(ggpubr::ggarrange(TDW_CI_b.factor_LowVMod,
                        TDW_CI_b.factor_LowVMod_facetted, nrow = 2, ncol = 1)) # print the model diagnostics
dev.off() 



TDW_CI_b.factor_F2_LowVHigh <- Biodep_Master_F1_and_F2s %>% 
                              dplyr::filter(!pCO2 %in% '800 μatm') %>% 
                              dplyr::filter(!Gen %in% 'F1') %>%
                              dplyr::mutate(Gen_pCO2 = paste0(Gen,'_',pCO2)) %>% 
                              ggplot(aes(x=log10(as.numeric(animal_dry_weight_g)), 
                                         y=log10(as.numeric(CR)), 
                                         color = Gen_pCO2,
                                         shape = Gen_pCO2)) +
                              geom_point(size = 2) +
                              ggpmisc::stat_ma_line(method = "SMA") + # model 2 regression Standard major axis!
                              ggpmisc::stat_ma_eq(ggpmisc::use_label(c("eq", "n", "R2"))) +  
                              scale_color_manual(values=c("purple","forestgreen")) +
                              scale_shape_manual(values=c(19, 19)) +
                              theme(panel.grid.major = element_blank(), 
                                    panel.grid.minor = element_blank())+ 
                              scale_x_continuous(name ="log10_TDW; in mm") +
                              scale_y_continuous(name ="log10_CI; CI in μmol L-1 hr-1)") +
                              # ylim(-1.5,2) +
                              theme_classic() +
                              theme(legend.position="none",
                                    element_line(linewidth = 0.5, color = 'black'),
                                    axis.title.y=element_text(size=12),
                                    axis.text.x=element_text(size=(12)),
                                    axis.text.y=element_text(size=(12))) + # legend.position="none",
                              ggtitle("Allometric scaling: log10_CI = log10_a + (b.factor * log10_TDW)") 



pdf(paste0(filename = "C:/Users/samuel.gurr/Documents/Github_repositories/EAD-ASEB-Airradians_multigen_OA/RAnalysis/Output/Biodeposition/allometric_scaling/F2_CI_bFactor_TDW_LowvHigh.pdf"), 
    width = 5, height = 5)
print(TDW_CI_b.factor_F2_LowVHigh) # print the model diagnostics
dev.off() 


# Shannon meeting 3/31/2023 - omit values that are NOT between -1 and 1 for AR 
# note, two values for the F2s and no values for the F1s with this criteria - output the 'bad values' for the F2s and ommit in the master
Biodep_Master_F2s_bad_data <- Biodep_Master_F2s %>% dplyr::filter(AE > 1 | AE < -1)
nrow(Biodep_Master_F2s_bad_data) # only 2 rows!
Biodep_Master_F2s_OM       <- Biodep_Master_F2s %>% dplyr::filter(!(AE > 1 | AE < -1))
nrow(Biodep_Master_F2s_OM) # 61
# WRITE CSV OF THE MASTER FILE
write.csv(Biodep_Master_F1s, "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/Biodeposition_master_F1.csv")
write.csv(Biodep_Master_F2s_OM, "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/Biodeposition_master_F2.csv")
write.csv(Biodep_Master_F2s_bad_data, "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/Biodeposition_outliers_F2.csv")


write.csv(Biodep_Master_F1s, "C:/Users/samuel.gurr/Documents/Github_repositories/EAD-ASEB-Airradians_multigen_OA/RAnalysis/Output/Biodeposition/F1/F1_Biodeposition_master.csv")
write.csv(Biodep_Master_F2s_OM, "C:/Users/samuel.gurr/Documents/Github_repositories/EAD-ASEB-Airradians_multigen_OA/RAnalysis/Output/Biodeposition/F2/F2_Biodeposition_master.csv")
write.csv(Biodep_Master_F2s_bad_data, "C:/Users/samuel.gurr/Documents/Github_repositories/EAD-ASEB-Airradians_multigen_OA/RAnalysis/Output/Biodeposition/F2/F2_Biodeposition_outliers.csv")

nrow(Biodep_Master_F2s_OM)

#write.csv(Biodep_Master, "C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/Biodeposition_master.csv")
