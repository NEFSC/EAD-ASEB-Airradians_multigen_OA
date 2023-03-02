---
title: "RespirationRates_Analysis_F2s"
author: "Samuel Gurr"
date: "2023-02-28"
output: pdf_document
---


```{r setup, include= FALSE, echo = FALSE}

# LOAD PACKAGES :::::::::::::::::::::::::::::::::::::::::::::::::::::::
library(dplyr)
library(ggplot2)
library(forcats)
library(lme4)
library(lmerTest)
library(performance)
library(car)
library(kableExtra)
library(pander)
library(data.table)
library(Rmisc)
library(devtools)
library(ggpubr)
library(SciViews)

# SET WORKING DIRECTORY :::::::::::::::::::::::::::::::::::::::::::::::
knitr::opts_knit$set(root.dir = "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis")
# setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis") # Work computer

# LOAD DATA :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
RR_master <- read.csv(file="Output/Respiration/RR_calc_raw.csv", header=T) %>% 
              filter(!Food %in% 'unfed') %>% # omit low food trial data
              # filter out the F2 measurements
              filter(Date %in% c('8/30/2022', '9/22/2022', '11/16/2022', '1/31/2023', '2/23/2023')) %>% # an F2 measurement
              filter((Date == '9/22/2022' & filetype =='SDR_data')) %>% 
  # unique(RR_master$Date) # "10/26/2021" "2/2/2022"   "3/1/2022"   "8/30/2022"  "9/14/2021"  "9/22/2022"  "9/30/2021"
              dplyr::mutate(Age = case_when(Date == '8/30/2022'  ~ 13,
                                            Date == '9/22/2022'  ~ 36,
                                            Date == '11/16/2022'  ~ 91, 
                                            Date == '1/31/2023' ~ 167,
                                            Date == '2/23/2023'  ~ 190)) %>% 
              dplyr::mutate(Age = as.factor(Age)) %>% 
              dplyr::arrange(Age)
unique(RR_master$Date)
# View(RR_master)


# NOTE: we have TWO resp values 
# (1) 'resp_umol_hr' calculated using  ONLY measured biovolumes to correct vessel volume - contains NAs for the first few resp dates when biovolume was not measured
# (2) 'resp_umol_hr_biovolcalc' - contains all data with length^3 to correct for biovolume - no NAs, all data with a length measurement has a datapoint!
```
