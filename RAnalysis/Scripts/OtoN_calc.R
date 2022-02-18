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

# SET WORKING DIRECTORY :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis")


# LOAD DATA :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
Resp_Master          <- read.csv(file="Output/Respiration/Calculated_Resp_Master.csv", header=T) 
Excretion_Master     <- read.csv(file="Data/Excretion/Excretion_master.csv", header=T) 
#merge
RespExcretion_Master <- merge(Resp_Master,Excretion_Master) # only 18 values as of 2/2/2022 that have both resp and excretion


# CALCULATE O:N :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
Master_table <- as.data.frame(RespExcretion_Master %>% 
                  dplyr::select(c('Date',  'pH', 'Replicate', 'Dry_Tissue_weight', 'resp_umol_L_hr', 'start.end_resp_umol_L_hr', 'Nitrogen_ug_mL_hr')) %>% 
                  dplyr::rename(c('RESP_umol_L_hr' = 'resp_umol_L_hr', 'RESP.start.end_umol_L_hr' = 'start.end_resp_umol_L_hr', 'NITROGEN_ug_mL_hr' = 'Nitrogen_ug_mL_hr')) %>% 
                  dplyr::mutate('ON_RESP.LoLinR' = ((RESP_umol_L_hr*14)/(NITROGEN_ug_mL_hr)*1000)) %>% 
                  dplyr::mutate('ON_RESP.start.end' = ((RESP.start.end_umol_L_hr*14)/(NITROGEN_ug_mL_hr)*1000)))
View(Master_table)

# Summary (means SE):::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
as.data.frame(Master_table %>%  
  dplyr::filter(!ON_RESP.LoLinR < 0) %>% 
  summarySE(measurevar="ON_RESP.LoLinR", groupvars=c("Date", "pH")))

# Date  pH N ON_RESP.LoLinR       sd       se       ci
# 1 2/2/2022 7.5 8       11.35245 4.320349 1.527474 3.611902
# 2 2/2/2022 8.0 8       14.47146 7.396323 2.614995 6.183481

as.data.frame(Master_table %>%  
                dplyr::filter(!ON_RESP.start.end < 0) %>% 
                summarySE(measurevar="ON_RESP.start.end", groupvars=c("Date", "pH")))

# Date  pH N ON_RESP.start.end       sd        se       ci
# 1 2/2/2022 7.5 8          8.601386 2.821077 0.9974014 2.358479
# 2 2/2/2022 8.0 8         13.940044 5.070313 1.7926263 4.238888