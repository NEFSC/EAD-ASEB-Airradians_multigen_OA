

## Load libraries

library(ggplot2)
library(dplyr)
library(reshape2)
library(pander)
library(dplyr)
library(kableExtra)
library(data.table)
library(stringr)
library(latex2exp)
library(Rmisc)
library(aggregate)
library(car)


## set working directory

setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis") # Work computer
# setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis") # Work computer

## load data 

biodep <- read.csv(file="Data/Physiology/Biodeposition/F1/cumulative_raw/Raw_masterdata_biodeposition.csv", header = TRUE) 

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





# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Calculations PART1 :::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# 'biodep2' == the first steps, calculating the POM, TPM, PIM, for feces and pseudofeces (not corrected for b factor yet!)




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
  
View(biodep2)



# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Blanks as 'water_Blank' ::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# why?
# these values grouped by Date and treatment will be called to corect for calculated values downstream in this script

# blanks - as 'water_Blank' - call to correct in biodep2 below...
WaterSamples_blank     <- biodep2 %>%  
  dplyr::select(c('Date', 'sample_type', 'treatment', 'water_sample_time', 'TPM_mgL',  'PIM_mgL',  'POM_mgL', 'Perc_INORG', 'Perc_ORG')) %>% 
  dplyr::filter(sample_type %in% 'water_Blank') %>% 
  dplyr::group_by(Date,treatment)
 View(WaterSamples_blank)
# slice(-1) # removes the first timestamp by group 
# mean for these blanks here...
WaterSamples_blank_AVE <- WaterSamples_blank %>% 
  dplyr::select(-c('sample_type', 'water_sample_time')) %>% # %>%  # omit to average across all group_by columns
  dplyr::group_by(Date,treatment) %>%
  dplyr::mutate(TPM_mgL = as.numeric(TPM_mgL)) %>% 
  dplyr::summarise(across(everything(), list(mean)))
WaterSamples_blank_AVE #view your blanks! 


# 'water_Input' - call to correct in biodep2 below...
WaterSamples_input     <- biodep2 %>%  
  dplyr::select(c('Date', 'sample_type', 'treatment', 'water_sample_time', 'TPM_mgL',  'PIM_mgL',  'POM_mgL', 'Perc_INORG', 'Perc_ORG')) %>% 
  dplyr::filter(sample_type %in% 'water_Input') %>% 
  dplyr::filter(!(Date %in% '20221027' & treatment %in% 7.5 & TPM_mgL > 5)) %>%  # omit the time points 9:30 - 10:10am showing abnormally high particulate  
  dplyr::group_by(Date,treatment)
View(WaterSamples_input)
# slice(-1) # removes the first timestamp by group 
# mean for these blanks here...

WaterSamples_input_AVE <- WaterSamples_input %>% 
  dplyr::select(-c('sample_type', 'water_sample_time')) %>% # %>%  # omit to average across all group_by columns
  dplyr::group_by(Date,treatment) %>%
  dplyr::mutate(TPM_mgL = as.numeric(TPM_mgL)) %>% 
  dplyr::summarise(across(everything(), list(mean)))
WaterSamples_input_AVE #view your blanks!


water_samples_master <- rbind( (as.data.frame(WaterSamples_input_AVE %>% dplyr::mutate(Type = 'input'))),
                               (as.data.frame(WaterSamples_blank_AVE %>% dplyr::mutate(Type = 'blank'))) )

# WRITE CSV OF THE MASTER FILE
write.csv(water_samples_master, "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/Biodeposition_input_blank.csv")


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Calculations PART2 :::::::::::::::::::::::::::::::::::::::::::::::::::::::::
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
                              BioSamples_feces, by = c('Date', 'treatment', 'animal_number', 'tank_ID')) # merge with the feces dataframe by the unique identifiers

# SPECIES STANDARDIZATION COEFFICIENT - change here when we calculate our own for the Bay scallop and potentially under the different OA treatments
sp_COEF <- 0.62 # standardization coefficient
sp_COEF <- 0.696 # standardization coefficient - umol )2 consumption and tissue dry weight (review RespRates_analysis)

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# FOR LOOP PREP ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
dates             <- as.data.frame(unique(biodep$Date)) 
colnames(dates)   <- "Date"
Biodep_Master     <- data.frame() # start dataframe 

for (i in 1:nrow(dates)) {
  date_loop       <- dates[i,]
  blanks_loop     <- WaterSamples_blank_AVE %>% filter(Date %in% date_loop) %>% arrange(treatment)# filter blanks, sort as 7.5 then 8 for treatment pH
  waterinput_loop <- WaterSamples_input_AVE %>% filter(Date %in% date_loop) %>% arrange(treatment) # filter blanks, sort as 7.5 then 8 for treatment pH
  data_loop       <- BioSamples_merged %>%
                        dplyr::filter(Date %in% date_loop) %>% 
                      # IER == Inorganic Egestion Rate: PIM of feces/feces collection time
                        dplyr::mutate(IER_correct = IER_mghr*(0.1/animal_dry_weight_mg)^sp_COEF) %>% 
                      # IRR == Inorganic Rejection Rate: PIM of pseudofeces/pseudofeces collection time
                        dplyr::mutate(IRR_correct = IRR_mghr*(0.1/animal_dry_weight_mg)^sp_COEF) %>%  
                      # OER == Organic Egestion Rate: POM of feces/feces collection time
                        dplyr::mutate(OER_correct = OER_mghr*(0.1/animal_dry_weight_mg)^sp_COEF) %>% 
                      # ORR == Organic Rejection Rate: POM of pseudofeces/pseudofeces collection time
                        dplyr::mutate(ORR_correct = ORR_mghr*(0.1/animal_dry_weight_mg)^sp_COEF) %>% 
                      # CR  == Cleanrance Rate: IFR/PIM of the water
                        dplyr::mutate(CR = case_when(
                           treatment == 7.5 ~ (IRR_mghr + IER_mghr) / waterinput_loop$PIM_mgL_1[1],
                           treatment == 8.0 ~ (IRR_mghr + IER_mghr) / waterinput_loop$PIM_mgL_1[2]
                           )) %>% 
                      # FR  == Filtration Rate: CR * TPM of the water
                        dplyr::mutate(FR = case_when(
                          treatment == 7.5 ~ CR * waterinput_loop$TPM_mgL_1[1],
                          treatment == 8.0 ~ CR * waterinput_loop$TPM_mgL_1[2]
                        )) %>% 
                      # RR  == Rejection Rate: ORR+IRR
                        dplyr::mutate(RR_correct = ORR_correct + IRR_correct) %>% 
                      # p   == Fraction of Organic Rejected: ORR/RR (organic fraction of the pseudofeces)
                        dplyr::mutate(p = ORR_correct / RR_correct ) %>% 
                      # f   == POM available: Average POM of the water
                        dplyr::mutate(f =  case_when(
                          treatment == 7.5 ~ waterinput_loop$Perc_ORG_1[1] / 100,
                          treatment == 8.0 ~ waterinput_loop$Perc_ORG_1[2] / 100
                        )) %>% 
                      # SE  == Selection Efficiency: 1-(p/f) (organic content of pseudofeces/organic content of the water)
                        dplyr::mutate(SE = 1 - (p / f)) %>% 
                      # IFR == Inorganic Filtration Rate: IER + IRR (PIM feces + PIM pseudofeces; i.e. total inorganic matter filtered/collection time)
                        dplyr::mutate(IFR = IER_correct + IRR_correct) %>% 
                      # CR  == Cleanrance Rate: IFR/PIM of the water
                        dplyr::mutate(CR_correct = case_when(
                          treatment == 7.5 ~ IFR /  waterinput_loop$PIM_mgL_1[1],
                          treatment == 8.0 ~ IFR /  waterinput_loop$PIM_mgL_1[2]
                        )) %>% 
                      # FR  == Filtration Rate: CR * TPM of the water
                        dplyr::mutate(FR_correct = case_when(
                          treatment == 7.5 ~ CR_correct * waterinput_loop$TPM_mgL_1[1],
                          treatment == 8.0 ~ CR_correct *  waterinput_loop$TPM_mgL_1[2]
                        )) %>% 
                      # %RR == RR/FR (amount rejected/total amount filtered)
                        dplyr::mutate(RR_Percent = (RR_correct/FR_correct)*100) %>% 
                      # TIR == Total Ingestion Rate: FR - RR
                        dplyr::mutate(TIR = FR_correct -
                                        RR_correct) %>% 
                      # OFR == Organic FIltration Rate: CR * POM of the water
                        dplyr::mutate(OFR = case_when(
                          treatment == 7.5 ~ CR_correct * waterinput_loop$POM_mgL_1[1],
                          treatment == 8.0 ~ CR_correct *  waterinput_loop$POM_mgL_1[2]
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
  Biodep_Master     <- rbind(Biodep_Master,df) #bind to a cumulative list dataframe
  print(Biodep_Master) # print to monitor progress
                        
}

View(Biodep_Master) # look at your master file!

# WRITE CSV OF THE MASTER FILE
write.csv(Biodep_Master, "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/Biodeposition_master.csv")
#write.csv(Biodep_Master, "C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/Biodeposition_master.csv")







# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ANALYSIS AND PLOTTING  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
Biodep_Master <- read.csv("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/Biodeposition_master.csv", header = T)
Biodep_Master <- na.omit(Biodep_Master) # omit #6 pH7.5 on 20221027
# View(Biodep_Master)
# (1) First, run anova within date for all records (for looped!)
ANOVA_Dates       <- as.data.frame(unique(Biodep_Master$Date)) # call a list to loop in 
AOVdf_total       <- data.frame() # start dataframe, this will be the master output
DF_loop           <- data.frame(matrix(nrow = 1, ncol = 12)) # create dataframe to save during for loop
colnames(DF_loop) <- c('Date', 'Metric', 'model', 'DF.num' , 'DF.denom', 'F_val','P_val', 'SigDif', 'ShapiroWilk', 'ResidNorm', 'Levenes', 'HomogVar') # names for comuns in the for loop
cols_m_loop       <- as.data.frame(c('SE','OIR','FR_correct', 'CR_correct', 'RR_Percent','OIR','AR','AE')) %>% `colnames<-`('biodep_meas')


for (i in 1:nrow(ANOVA_Dates)) {
  
  date_loop     <- as.character(ANOVA_Dates[i,])
  data_loop     <- Biodep_Master %>% 
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
write.csv(AOVdf_total, "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/Biodeposition_ANOVA_table.csv")
#write.csv(Biodep_Master, "C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/Biodeposition_master.csv")




# plotting ::::::::::::::::::::::::::::::::::;;


AE_boxplot <- Biodep_Master %>% 
  dplyr::mutate(Temperature = case_when(Date == '20220302' ~ '16C', 
                                        Date == '20220923' ~ '20C',
                                        Date == '20221027' ~ '13.3C')) %>%  
  #filter(!AE < 0) %>% 
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


AR_boxplot <- Biodep_Master %>% 
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

OIR_boxplot <- Biodep_Master %>% 
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


FR_boxplot <- Biodep_Master %>% 
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

RR_boxplot <- Biodep_Master %>% 
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

SE_boxplot <- Biodep_Master %>% 
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
pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/Biodeposition_Boxplots.pdf"), width = 10, height= 8)
ggarrange(SE_boxplot,RR_boxplot, OIR_boxplot, FR_boxplot, AE_boxplot, AR_boxplot)
dev.off()

colnames(Biodep_Master)


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
 