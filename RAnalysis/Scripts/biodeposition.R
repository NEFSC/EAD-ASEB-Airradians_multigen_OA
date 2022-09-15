

## Load libraries

library(ggplot2)
library(dplyr)
library(reshape2)

## set working directory

setwd("C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis") # Work computer
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
biodep2 <- biodep %>% dplyr::mutate(TPM_mgL = case_when(vol_filtered_L > 0 ~ ((dry_filter_weight_mg - initial_filter_weight_mg) / vol_filtered_L) ,
                                                    is.na(vol_filtered_L) ~ (dry_filter_weight_mg - initial_filter_weight_mg) )) %>% 
                      dplyr::mutate(PIM_mgL = case_when(vol_filtered_L > 0 ~ ((ash_filter_weight_mg - initial_filter_weight_mg) / vol_filtered_L) ,
                                                        is.na(vol_filtered_L) ~ (ash_filter_weight_mg - initial_filter_weight_mg) ))


