# Purpose: Bay Scallop Project - Respiration rate data 
# to complimen the LoLin R analysis, calc O2 consumption from 'start' to 'end' of each trial

# Written by: Sam J Gurr (last edit 12/19/22)
# - used the blank TOM and POM for the biodep corrections 
# - used an excretion data specific b factor (review Excretion script) of 1.13 for ER v. TDE
# - bfactor for RR and biodep was 0.822 from MO2 v. TDW (review Resp b factor script)

# LOAD PACKAGES ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
library(devtools) # devtools::install_github # use devtools to instlal github link
library(LoLinR) # install_github('colin-olito/LoLinR') # install LoLinR from github
library(dplyr)
library(lubridate)
library(rMR) 
library(dplyr)
library(stringr)
library(rlang)
library(ggplot2)
library(ggfortify)
library(DESeq2)
library(devtools)
library(ggbiplot)
library(VennDiagram)# venn diagrams
library(eulerr) #venn diagrams -  check out the R shiny app (http://eulerr.co/) 

# SET WORKING DIRECTORY :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis")
#setwd("C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis")


# F1s :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;
# biodeposition
Biodep <- read.csv("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/Biodeposition_master_F1.csv", header = T) %>% dplyr::select(-X)

#NOTe: biodep does nothave the columns to merge by including the Run, Chemaner_Tank, Replicate, etc. 
# However... we can merge by the equal length and tissue dry weight in the RR dataset below! 
# first we need to reformat a few things here




# Respiration rate (lolinR rates)
RR <- read.csv(file="Output/Respiration/F1/F1_RR_calc_master.csv", header=T) %>% 
  filter(!Food %in% 'unfed') %>% # omit low food trial data
  # filter out the F2 measurements
  filter(!Date %in% c('8/30/2022', '11/16/2022')) %>% # an F2 measurement
  filter(!(Date == '9/22/2022' & filetype =='SDR_data')) %>% 
  # unique(RR_master$Date) # "10/26/2021" "2/2/2022"   "3/1/2022"   "8/30/2022"  "9/14/2021"  "9/22/2022"  "9/30/2021"
  dplyr::select(c(Date, Age,  pH, pCO2, Replicate, Chamber_tank, Run, Number, filetype, Channel, 
                  Length_mm, 
                  Dry_Shell_weight,
                  Dry_Tissue_weight,
                  whole_Dry_weight,
                  Lpc,
                  BLANK.mean_Lpc,
                  resp_blankStand,
                  volume,
                  Biovol_length3 ,
                  measured_volume,
                  calculated_volume,
                  resp_mg_hr,
                  resp_mg_hr_bFactorNormTDW.MEAN,
                  resp_mg_hr_bFactorNormLength.MEAN,
                  resp_umol_hr,
                  resp_umol_hr_bFactorNormTDW.MEAN,
                  resp_umol_hr_bFactorNormLength.MEAN)) %>% 
  dplyr::rename(RRvolume_vessel_mL = volume) %>% 
  dplyr::rename(RRvolume_measuredBiovol_mL = measured_volume) %>% 
  dplyr::rename(RRvolume_calculatedBiovol_mL = calculated_volume) %>% 
  dplyr::rename(RR_mgLmin_rawblankcor = resp_blankStand) %>% 
  dplyr::rename(RR_mgLmin_blankMean = BLANK.mean_Lpc) %>% 
  dplyr::rename(RR_mgLmin_raw = Lpc)




# start end resp
O2consumption_start.end  <- read.csv(file="Output/Respiration/RR_start_end_master.csv", header=T)  %>% 
  dplyr::select(c(Date, pH, Chamber_tank, Run, Number, filetype, Channel, Start.End_RR_umolhr))



# ammonia excretion
ER <- read.csv(file="Output/ExcretionRates/F1/F1_ExcretionRates_master.csv", header=T)  %>% 
  dplyr::select(c(Date, pH, Replicate, Chamber_tank, Run, Number,
                  Length_um,
                  Dry_Tissue_weight,
                  ExcretionRate_ug_mL_hr,
                  ExcretionRate_umol_mL_hr )) %>%
  dplyr::mutate(ExcretionRate_mg_hr = ExcretionRate_ug_mL_hr/1000) %>% 
  dplyr::rename(ExcretionRate_umol_hr = ExcretionRate_umol_mL_hr) %>% 
  dplyr::mutate(Length_mm = as.numeric(Length_um / 1000)) %>% # Length_mm matched biodep and RR 
  dplyr::select(-c(Length_um,ExcretionRate_ug_mL_hr)) # dont need this anymore do we
unique(ER$Date) # 20211026 20220202 20220301 20220922 20221026





# Prep the date for Biodep merge :::::::::::::::::::::::::::::::::::::::::

# NOTE: only 3/1/2022', '9/22/2022', '10/26/2022' - notice that the biodep dates are one day after respiratio, need to change this to merge properl

unique(RR$Date)

# prep the resp data 
RR_prepped <- RR %>% # just call desired dates tat overlap with biodep for merge
  dplyr::mutate(Date = format(strptime(Date, format = "%m/%d/%Y"), "%m/%d/%Y")) %>% # format to mm/dd/yyy as RR dataset
  filter(Date %in% c('03/01/2022', '09/22/2022', '10/26/2022')) # the only dates we need to merge with biodep
RR_prepped$Dry_Tissue_weight <- as.numeric(RR_prepped$Dry_Tissue_weight)
nrow(RR_prepped) # 45


# prep the ER data  - lets match the RR data 
meanTDW <- mean(ER$Dry_Tissue_weight) # 0.4378721
bTDW    <- 1.13 # added 12/19/22 after calculating a TDW ER specific b factor (review ER analysis script!) 

ER_prepped <- ER %>% 
  
  dplyr::mutate(ExcretionRate_mg_hr_bFactorNormTDW.MEAN = 
                  (ExcretionRate_mg_hr)*((meanTDW/Dry_Tissue_weight)^bTDW)) %>% # TDW b factor - mg
  
  dplyr::mutate(ExcretionRate_umol_hr_bFactorNormTDW.MEAN = 
                  (ExcretionRate_umol_hr)*((meanTDW/Dry_Tissue_weight)^bTDW)) %>% # TDW b factor - umol
  
  dplyr::mutate(Date = format(strptime(Date, format = "%Y%m%d"), "%m/%d/%Y")) %>% # format to mm/dd/yyy as RR dataset
  dplyr::filter(Date %in% c('03/01/2022', '09/22/2022', '10/26/2022'))  %>% # the only dates we need to merge with biodep
  unique() # a few duplicates on 3/1/2022 for some strange reason....
nrow(ER_prepped) # 45 

# prep the biodep data - lets match the RR data 
# note the calc biodep is already corrected for 0.822 bfactor
Biodep_prepped <- Biodep %>% # unique(Biodep$Date) # "03/02/2022" "09/23/2022" "10/27/2022" - need to change to reflect RR_prepped (above)
  dplyr::rename(tank_ID = tank_ID.x) %>% 
  dplyr::mutate(Date = format(strptime(Date, format = "%Y%m%d"), "%m/%d/%Y")) %>% # format to mm/dd/yyy as RR dataset
  dplyr::mutate(Replicate = gsub("[^a-zA-Z]", "", tank_ID)) %>% # new replicate column - reflects RR dataset 
  dplyr::rename(Dry_Tissue_weight = animal_dry_weight_g) %>% # change name to match RR
  dplyr::rename(Length_mm = animal_length_mm) %>% # change name to match RR
  dplyr::rename(pH = treatment) %>% # rename to match 
  dplyr::select(-c(tank_ID.y, animal_number, initial_filter_weight_mg, dry_filter_weight_mg,ash_filter_weight_mg, inclubation_time_hours, pCO2)) %>% 
  dplyr::mutate(Date = case_when(Date == "03/02/2022" ~ '03/01/2022',
                                 Date == "09/23/2022" ~ '09/22/2022',
                                 Date == "10/27/2022" ~ '10/26/2022'))
Biodep_prepped$Chamber_tank <- paste0(Biodep_prepped$pH, "_", Biodep_prepped$Replicate)
nrow(Biodep_prepped) # 43 rows                        
                                 


# check for any missing data between these two datsets :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# call unique identifier to find discrepancies 
RR_prepped$uniqueID <- paste(RR_prepped$Date,RR_prepped$Chamber_tank,RR_prepped$Length_mm,RR_prepped$Replicate, sep = '_')
ER_prepped$uniqueID <- paste(ER_prepped$Date,ER_prepped$Chamber_tank,ER_prepped$Length_mm,ER_prepped$Replicate, sep = '_')
Biodep_prepped$uniqueID <- paste(Biodep_prepped$Date,Biodep_prepped$Chamber_tank,Biodep_prepped$Length_mm,Biodep_prepped$Replicate, sep = '_')

# between RR and excretion!!  - excretion has 532 rows and RR has 45 
# obvious that RR has omited values due to resp < blank that were omitted prior
subset(ER_prepped, !(uniqueID %in% RR_prepped$uniqueID)) # no discrepancies - these exactly reflect one another!


# btween RR and biodep!! 
nrow(RR_prepped) == nrow(Biodep_prepped) # FALSE - there are rows not present in biodep that Resp has - lets call a uni que identifier (that we will omit at the merge of course)

subset(RR_prepped, !(uniqueID %in% Biodep_prepped$uniqueID)) # three discrepancies between the RR and the biodep (RR have 3 more!)
# 3/1/2022 CH6 run 1 7.5_B - not present in the biodep file... we measured another 7.5_B whereas this was for only resp,  all is good! 
# 3/1/2022 CH2 run 2 8E - not present in the biodep file... lets review the RR data 
# 9/22/2022 CH7 8_D - there was no biodep *D on this data, animal was not used for biodep just respiration - all is good! 
# 10/26/2022 CH6 7.5_F 



# READY TO MERGE :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;
# note: this should yield 44 rows! 
# merge by a series of unique identifiers - we will use Date, Chamber_tank (contains pH and replicate information), Dry weight , and Length! 

RR_ER_merge <- merge(RR_prepped,ER_prepped)
nrow(RR_ER_merge) # 45 - no rows lost!

MASTER_ALL <- merge(RR_ER_merge, Biodep_prepped) %>% 
  dplyr::select(-uniqueID) %>% 
  dplyr::mutate(Age = case_when(Date == '3/1/2022'  ~ 218,
                                Date == '9/22/2022'  ~ 423,
                                Date == '10/26/2022' ~ 457)) %>%
  dplyr::mutate(Temperature = case_when(Date == '03/01/2022' ~ '16C', 
                                        Date == '09/22/2022' ~ '20C',
                                        Date == '10/26/2022' ~ '13.3C')) %>%  
  dplyr::mutate(pH_Temperature = paste(pCO2,'_',Temperature, sep = '')) %>% 
  dplyr::mutate(Age = as.factor(Age)) %>% dplyr::arrange(Age)
nrow(MASTER_ALL) # 41 rows - as expected!!!!!



# READY TO WRITE CSV ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

write.csv(MASTER_ALL, "Output/F1_RR_ER_Biodep_master.csv")
#write.csv(Biodep_Master, "C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/Biodeposition_master.csv")




# dplyr::mutate(Age = case_when(Date == '9/14/2021'  ~ 50,
#                               Date == '9/30/2021'  ~ 66,
#                               Date == '10/26/2021'  ~ 92, 
#                               Date == '3/1/2022'  ~ 218,
#                               Date == '9/22/2022'  ~ 423,
#                               Date == '10/26/2022' ~ 457)) %>% 
#   dplyr::mutate(Age = as.factor(Age)) %>%
#   dplyr::arrange(Age)



# Run a PCA
MASTER_ALL <- subset(MASTER_ALL, !is.na(MASTER_ALL$AR))
# nrow(MASTER_ALL) # 41 - one row omitted 

list(colnames(MASTER_ALL)) # view list to call below for PCA




MASTER_ALL_1   <- prcomp(MASTER_ALL[,c(5,6,26,31,61:62)], # all numeric (phys + all modules) - PCA 1 = 0.4155  , PCA 2 0.2729   (cumulative 0.6883 )
                      center = TRUE,
                      scale. = TRUE)
summary(MASTER_ALL_1)


MASTER_ALL_1   <- prcomp(MASTER_ALL[,c(26,31,53,63)], 
                         center = TRUE,
                         scale. = TRUE)
summary(MASTER_ALL_1)

MASTER_ALL_1   <- prcomp(MASTER_ALL[,c(26,31)], # only SMR and ER 
                         center = TRUE,
                         scale. = TRUE)
summary(MASTER_ALL_1)


# plot PCA
PCApCO2 <- ggbiplot(MASTER_ALL_1,
                   obs.scale = 1,
                   var.scale = 1,
                   groups = as.factor(MASTER_ALL$pCO2),
                   ellipse = TRUE,
                   circle = TRUE,
                   ellipse.prob = 0.67) +
  scale_color_discrete(name = '') +  theme_classic() +   ggtitle("pCO2") +
  theme(legend.direction = 'horizontal',
        legend.position = 'top')
# PCApCO2


PCA_Temp <- ggbiplot(MASTER_ALL_1,
                    obs.scale = 1,
                    var.scale = 1,
                    groups = as.factor(MASTER_ALL$Temperature),
                    ellipse = TRUE,
                    circle = TRUE,
                    ellipse.prob = 0.67) +
  scale_color_discrete(name = '') +  theme_classic() +   ggtitle("Temperature") +
  theme(legend.direction = 'horizontal',
        legend.position = 'top')
# PCA_Temp


# TEMPERATURE HAS A BIG EFFECT!  
MASTER_20C     <- MASTER_ALL %>%  dplyr::filter(Temperature %in% '20C')
MASTER_20C_PCA <- prcomp(MASTER_20C[,c(26,31,53,63)], 
                         center = TRUE,
                         scale. = TRUE)
PCA_20c_pCO2 <- ggbiplot(MASTER_20C_PCA,
                     obs.scale = 1,
                     var.scale = 1,
                     groups = as.factor(MASTER_20C$pCO2),
                     ellipse = TRUE,
                     circle = TRUE,
                     ellipse.prob = 0.67) +
                scale_color_discrete(name = '') +  
                theme_classic() + 
                ggtitle("pCO2 effet under 20C") +
                theme(legend.direction = 'horizontal',
                      legend.position = 'top')


MASTER_16C     <- MASTER_ALL %>%  dplyr::filter(Temperature %in% '16C')
MASTER_16C_PCA <- prcomp(MASTER_16C[,c(26,31,53,63)], 
                         center = TRUE,
                         scale. = TRUE)
PCA_16c_pCO2 <- ggbiplot(MASTER_16C_PCA,
                         obs.scale = 1,
                         var.scale = 1,
                         groups = as.factor(MASTER_16C$pCO2),
                         ellipse = TRUE,
                         circle = TRUE,
                         ellipse.prob = 0.67) +
  scale_color_discrete(name = '') +  
  theme_classic() + 
  ggtitle("pCO2 effet under 16C") +
  theme(legend.direction = 'horizontal',
        legend.position = 'top')




PCA_pCO2Temp <- ggbiplot(MASTER_ALL_1,
                     obs.scale = 1,
                     var.scale = 1,
                     groups = as.factor(MASTER_ALL$pH_Temperature),
                     ellipse = TRUE,
                     circle = TRUE,
                     ellipse.prob = 0.67) +
  scale_color_discrete(name = '') +  theme_classic() +   ggtitle("pCO2*Temperature") +
  theme(legend.direction = 'horizontal',
        legend.position = 'top')
# PCA_pCO2Temp

library(ggpubr)
ggarrange(PCApCO2, PCA_Temp, PCA_pCO2Temp )










# subset for temp 20C  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


MASTER_301 <- MASTER_ALL %>% dplyr::filter(Date %in% '03/01/2022') # 20C on 9/22/2022
PCA_301    <- prcomp(MASTER_301[,c(22,29,58,55,54,60,53,51)], 
                     center = TRUE,
                     scale. = TRUE)
PCApCO2_301 <- ggbiplot(PCA_301,
                        obs.scale = 1,
                        var.scale = 1,
                        groups = as.factor(MASTER_301$pCO2),
                        ellipse = TRUE,
                        circle = FALSE,
                        ellipse.prob = 0.67) +
  scale_color_discrete(name = '') +  theme_classic() +   ggtitle("3/1/2022 - Temp 16C") +
  theme(legend.direction = 'horizontal',
        legend.position = 'top')







MASTER_922 <- MASTER_ALL %>% dplyr::filter(Date %in% '09/22/2022') # 20C on 9/22/2022
PCA_922    <- prcomp(MASTER_922[,c(22,29,58,55,54,60,53,51)], 
                         center = TRUE,
                         scale. = TRUE)
PCApCO2_922 <- ggbiplot(PCA_922,
                    obs.scale = 1,
                    var.scale = 1,
                    groups = as.factor(MASTER_922$pCO2),
                    ellipse = TRUE,
                    circle = FALSE,
                    ellipse.prob = 0.67) +
  scale_color_discrete(name = '') +  theme_classic() +   ggtitle("9/22/2022 - Temp 20C") +
  theme(legend.direction = 'horizontal',
        legend.position = 'top')






MASTER_1026 <- MASTER_ALL %>% dplyr::filter(Date %in% '10/26/2022') # 20C on 9/22/2022
PCA_1026    <- prcomp(MASTER_1026[,c(22,29,58,55,54,60,53,51)], 
                     center = TRUE,
                     scale. = TRUE)
PCApCO2_1026 <- ggbiplot(PCA_1026,
                        obs.scale = 1,
                        var.scale = 1,
                        groups = as.factor(MASTER_1026$pCO2),
                        ellipse = TRUE,
                        circle = FALSE,
                        ellipse.prob = 0.67) +
  scale_color_discrete(name = '') +  theme_classic() +   ggtitle("10/26/2022 - Temp 13.3C") +
  theme(legend.direction = 'horizontal',
        legend.position = 'top')

# PCA_pCO2Temp

library(ggpubr)
ggarrange(PCApCO2_301, PCApCO2_922, PCApCO2_1026 )


pdf("Output/F1_PCAplot_RR_ER_Biodep.pdf", 
    width = 8, height = 8)
ggarrange(PCApCO2_301, PCApCO2_922, PCApCO2_1026 )
dev.off()
































# F2s :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;
# biodeposition
Biodep <- read.csv("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/Biodeposition_master_F2.csv", header = T) %>% 
  dplyr::select(-c(X, tank_ID.y)) %>% 
  dplyr::rename(tank_ID = tank_ID.x)
  

#NOTe: biodep does nothave the columns to merge by including the Run, Chemaner_Tank, Replicate, etc. 
# However... we can merge by the equal length and tissue dry weight in the RR dataset below! 
# first we need to reformat a few things here




# Respiration rate (lolinR rates)
RR <- read.csv(file="Output/Respiration/F2_RR_calc_master.csv", header=T) %>% 
  dplyr::select(c(Date, Age,  pH, pCO2, Replicate, Chamber_tank, Run, Number, filetype, Channel, 
                  Length_mm, 
                  Dry_Shell_weight,
                  Dry_Tissue_weight,
                  whole_Dry_weight,
                  Lpc,
                  BLANK.mean_Lpc,
                  resp_blankStand,
                  volume,
                  Biovol_length3 ,
                  measured_volume,
                  calculated_volume,
                  resp_mg_hr,
                  resp_mg_hr_bFactorNormTDW.MEAN,
                  resp_mg_hr_bFactorNormLength.MEAN,
                  resp_umol_hr,
                  resp_umol_hr_bFactorNormTDW.MEAN,
                  resp_umol_hr_bFactorNormLength.MEAN)) %>% 
  dplyr::rename(RRvolume_vessel_mL = volume) %>% 
  dplyr::rename(RRvolume_measuredBiovol_mL = measured_volume) %>% 
  dplyr::rename(RRvolume_calculatedBiovol_mL = calculated_volume) %>% 
  dplyr::rename(RR_mgLmin_rawblankcor = resp_blankStand) %>% 
  dplyr::rename(RR_mgLmin_blankMean = BLANK.mean_Lpc) %>% 
  dplyr::rename(RR_mgLmin_raw = Lpc)


unique(RR$Date)

# ammonia excretion
ER <- read.csv(file="Output/ExcretionRates/F2/F2_ExcretionRates_master.csv", header=T)  %>% 
  dplyr::select(c(Date, pH, Replicate, Chamber_tank, Run, Number,
                  Length_um,
                  Dry_Tissue_weight,
                  ExcretionRate_ug_mL_hr,
                  ExcretionRate_umol_mL_hr )) %>%
  dplyr::mutate(ExcretionRate_mg_hr = ExcretionRate_ug_mL_hr/1000) %>% 
  dplyr::rename(ExcretionRate_umol_hr = ExcretionRate_umol_mL_hr) %>% 
  dplyr::mutate(Length_mm = as.numeric(Length_um / 1000)) %>% # Length_mm matched biodep and RR 
  dplyr::select(-c(Length_um,ExcretionRate_ug_mL_hr)) # dont need this anymore do we
unique(ER$Date) # 20211026 20220202 20220301 20220922 20221026





# Prep the date for Biodep merge :::::::::::::::::::::::::::::::::::::::::

# NOTE: only 3/1/2022', '9/22/2022', '10/26/2022' - notice that the biodep dates are one day after respiratio, need to change this to merge properly


# prep the resp data 
RR_prepped <- RR %>% # just call desired dates tat overlap with biodep for merge
  filter(Date %in% c('1/31/2023', '2/23/2023')) # the only dates we need to merge with biodep
RR_prepped$Dry_Tissue_weight <- as.numeric(RR_prepped$Dry_Tissue_weight)
nrow(RR_prepped) # 42


# prep the ER data  - lets match the RR data 
meanTDW <- mean(ER$Dry_Tissue_weight) # 0.3062667
bTDW    <- 1.13 # added 12/19/22 after calculating a TDW ER specific b factor (review ER analysis script!) 

ER_prepped <- ER %>% 
  
  dplyr::mutate(ExcretionRate_mg_hr_bFactorNormTDW.MEAN = 
                  (ExcretionRate_mg_hr)*((meanTDW/Dry_Tissue_weight)^bTDW)) %>% # TDW b factor - mg
  
  dplyr::mutate(ExcretionRate_umol_hr_bFactorNormTDW.MEAN = 
                  (ExcretionRate_umol_hr)*((meanTDW/Dry_Tissue_weight)^bTDW)) %>% # TDW b factor - umol
  
  dplyr::mutate(Date = format(strptime(Date, format = "%Y%m%d"), "%m/%d/%Y")) %>% # format to mm/dd/yyy as RR dataset
  dplyr::mutate(Date = case_when(Date == "01/31/2023" ~ '1/31/2023',
                                 Date == "02/23/2023" ~ '2/23/2023')) %>% 

  unique() # a few duplicates on 3/1/2022 for some strange reason....
nrow(ER_prepped) # 42

# prep the biodep data - lets match the RR data 
# note the calc biodep is already corrected for 0.822 bfactor
Biodep_prepped <- Biodep %>% # unique(Biodep$Date) # "03/02/2022" "09/23/2022" "10/27/2022" - need to change to reflect RR_prepped (above)
  dplyr::mutate(Date = format(strptime(Date, format = "%Y%m%d"), "%m/%d/%Y")) %>% # format to mm/dd/yyy as RR dataset
  dplyr::mutate(Replicate = gsub("[^a-zA-Z]", "", tank_ID)) %>% # new replicate column - reflects RR dataset 
  dplyr::rename(Dry_Tissue_weight = animal_dry_weight_g) %>% # change name to match RR
  dplyr::rename(Length_mm = animal_length_mm) %>% # change name to match RR
  dplyr::rename(pH = treatment) %>% # rename to match 
  dplyr::select(-c(tank_ID, animal_number, initial_filter_weight_mg, dry_filter_weight_mg,ash_filter_weight_mg, inclubation_time_hours, pCO2)) %>% 
  dplyr::mutate(Date = case_when(Date == "02/01/2023" ~ '1/31/2023',
                                 Date == "02/24/2023" ~ '2/23/2023')) %>% 
  dplyr::select(-Length_mm) # lenngth is the exact same as the resp and Excretion data except diff sig figs and some missing
nrow(Biodep_prepped) # 45 rows                        



# check for any missing data between these two datsets :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# call unique identifier to find discrepancies 
RR_prepped$uniqueID <- paste(RR_prepped$Date,RR_prepped$pH,RR_prepped$Dry_Tissue_weight, sep = '_')
ER_prepped$uniqueID <- paste(ER_prepped$Date,ER_prepped$pH,ER_prepped$Dry_Tissue_weight, sep = '_')
Biodep_prepped$uniqueID <- paste(Biodep_prepped$Date,Biodep_prepped$pH,Biodep_prepped$Dry_Tissue_weight, sep = '_')


# between RR and excretion!!  - excretion has 532 rows and RR has 45 
# obvious that RR has omited values due to resp < blank that were omitted prior
subset(ER_prepped, !(uniqueID %in% RR_prepped$uniqueID)) # no discrepancies - these exactly reflect one another!


# btween RR and biodep!! 
nrow(RR_prepped) == nrow(Biodep_prepped) # FALSE - there are rows not present in biodep that Resp has - lets call a uni que identifier (that we will omit at the merge of course)

subset(RR_prepped, !(uniqueID %in% Biodep_prepped$uniqueID)) # three discrepancies between the RR and the biodep (RR have 3 more!)
# 3/1/2022 CH6 run 1 7.5_B - not present in the biodep file... we measured another 7.5_B whereas this was for only resp,  all is good! 
# 3/1/2022 CH2 run 2 8E - not present in the biodep file... lets review the RR data 
# 9/22/2022 CH7 8_D - there was no biodep *D on this data, animal was not used for biodep just respiration - all is good! 
# 10/26/2022 CH6 7.5_F



# READY TO MERGE :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;
# note: this should yield 44 rows! 
# merge by a series of unique identifiers - we will use Date, Chamber_tank (contains pH and replicate information), Dry weight , and Length! 

RR_ER_merge <- merge(RR_prepped,ER_prepped)
nrow(RR_ER_merge) # 42 - no rows lost! - remember we have 1 additional biodep for each treatmnet since we were limited by the loligo system 

MASTER_ALL <- merge(RR_ER_merge, Biodep_prepped, 
                    by = c('Date','uniqueID','Dry_Tissue_weight', 'Replicate', 'pH')) %>% 
  dplyr::select(c(Date, 
                  Age,
                  pCO2,
                  pH,
                  Replicate,
                  Chamber_tank,
                  Length_mm,
                  Dry_Tissue_weight,
                  resp_mg_hr_bFactorNormTDW.MEAN,
                  resp_umol_hr_bFactorNormTDW.MEAN,
                  ExcretionRate_mg_hr_bFactorNormTDW.MEAN,
                  ExcretionRate_umol_hr_bFactorNormTDW.MEAN,
                  IER_correct,
                  IRR_correct,
                  OER_correct,
                  ORR_correct,
                  CR_correct,
                  FR_correct,
                  RR_correct,
                  p,
                  f,
                  i,
                  SE,
                  AR,
                  AE))
nrow(MASTER_ALL) # 42 rows - as expected!!!!!
colnames(MASTER_ALL)
View(MASTER_ALL)
# READY TO WRITE CSV ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

write.csv(MASTER_ALL, "Output/F2_RR_ER_Biodep_master.csv")
RR_ER_merge
#write.csv(Biodep_Master, "C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/Biodeposition_master.csv")

