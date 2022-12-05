# ---
# title: "ExcretionRate"
# author: "Samuel Gurr"
# date: "9/20/2022"
# output: pdf_document
# ---




#install.packages("pander")
library(dplyr)
library(ggplot2)
# SET WORKING DIRECTORY 
setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis") # personal computer
# LOAD DATA & cater to this script 
Excretion_data <- read.csv(file="Data/Physiology/Excretion_rates/F1/cumultative_raw/Excretion_master.csv", header=T,stringsAsFactors=FALSE, fileEncoding="latin1") # master data file
Size_data   <- read.csv(file="Data/Physiology/Respiration/Reference_resp_size.csv", header=T) 




# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# EDIT AND MERG DATA  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

list(unique(Excretion_data$Date)) # 20220202 20220301 20211026 20220922 20221026 - call these dates in the size data and only Loligo RR data (large animals measured excretion!)

Size_data_2 <- Size_data %>% 
  dplyr::mutate(Date = paste("20",(format(as.Date(Date, "%m/%d/%Y"), "%y%m%d")), sep ='')) %>% # change format of the date to the format in Excretion_data
  dplyr::select(-c(Food, Shell_tin_weight, tin_plus_shell, Tissue_tin_.weight, tin_plus_tissue, Plate, Vessel_well_volume, Notes)) %>%  # get rid of unneeded column(s)
  dplyr::filter(Date %in% unique(Excretion_data$Date)) %>% # 20220202 20220301 20211026 20220922 20221026- call these dates in te size dat
  dplyr::filter(!Instrument %in% 'SDR_24channel') %>% # dates occasionally have resp for F2s with SDr, call the Loligo system for the correct animals
  dplyr::select(-Instrument) # now we can omit Instrument column
nrow(Size_data_2) # 82
nrow(Excretion_data)



meanTDW <- mean(as.numeric(Size_data_2$Dry_Tissue_weight)) # 0.4729451


Excretion_master <- merge(Excretion_data, Size_data_2) %>% # merge size and excretion datadata
  dplyr::filter(!ExcretionRate_umol_mL_hr < 0) %>% # 3 excretion < 0 omit (20211026 7.5C, 20220202 7.5C, 20220202 8.0C)
  dplyr::mutate(ExcretionRate_umol_mL_hr_TDWbfactor =  ExcretionRate_umol_mL_hr*( (meanTDW/(as.numeric(Dry_Tissue_weight)))^0.822) ) %>% # correct ExcretionRate_umol_mL_hr for gram of Tissue Dry WEight
  dplyr::mutate(pCO2 = case_when(pH == 8.0 ~ "500 μatm", pH == 7.5 ~ "800 μatm"))


# WRITE CSV OF THE MASTER FILE
write.csv(Excretion_master, "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/ExcretionRates_master.csv")






# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ANALYSIS AND PLOTTING  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::



# 20220202 :::::::::::::::::::::::::::::::



Excretion_master_0202 <- Excretion_master %>% filter(Date %in% '20220202') # call data 

# LME mod -  data 
LMEmod_0202           <-lme(ExcretionRate_umol_mL_hr_gTDW ~ pCO2, random=~1|Chamber_tank, data=Excretion_master_0202) # cahmber tank = = random factor (ii.e. 8_C, 7.5_C, 8_A, etc.)
pander(anova(LMEmod_0202), style='rmarkdown') # anova table of lmer
# |     &nbsp;      | numDF | denDF | F-value |  p-value  |
# |:---------------:|:-----:|:-----:|:-------:|:---------:|
# | **(Intercept)** |   1   |  14   |  102.2  | 8.139e-08 |
# |    **pCO2**     |   1   |  14   |  1.522  |  0.2377   |
shapiro.test(resid(LMEmod_0202)) # 0.183 -  normal
qqnorm(resid(LMEmod_0202)) # 
hist(resid(LMEmod_0202)) # 



# 20220301 :::::::::::::::::::::::::::::::


Excretion_master_0301 <- Excretion_master %>% filter(Date %in% '20220301') # call data 

# LME mod -  data 
LMEmod_0301           <-lme(ExcretionRate_umol_mL_hr_gTDW ~ pCO2, random=~1|Chamber_tank, data=Excretion_master_0301) # cahmber tank = = random factor (ii.e. 8_C, 7.5_C, 8_A, etc.)
pander(anova(LMEmod_0301), style='rmarkdown') # anova table of lmer
# |     &nbsp;      | numDF | denDF | F-value |  p-value  |
# |:---------------:|:-----:|:-----:|:-------:|:---------:|
# | **(Intercept)** |   1   |  14   |  28.79  | 9.953e-05 |
# |    **pCO2**     |   1   |  14   |  2.295  |   0.152   |
shapiro.test(resid(LMEmod_0301)) # 0.0151 - non normal
qqnorm(resid(LMEmod_0301)) # 
hist(resid(LMEmod_0301)) # 

# LME mod -  transformed data 
LMEmod_0301_log           <-lme(log(ExcretionRate_umol_mL_hr_gTDW) ~ pCO2, random=~1|Chamber_tank, data=Excretion_master_0301) # cahmber tank = = random factor (ii.e. 8_C, 7.5_C, 8_A, etc.)
pander(anova(LMEmod_0301_log), style='rmarkdown') # anova table of lmer
# |     &nbsp;      | numDF | denDF | F-value | p-value |
# |:---------------:|:-----:|:-----:|:-------:|:-------:|
# | **(Intercept)** |   1   |  14   | 0.06904 | 0.7966  |
# |    **pCO2**     |   1   |  14   |  1.451  | 0.2483  |
shapiro.test(resid(LMEmod_0301_log)) # 0.3733 - normal
qqnorm(resid(LMEmod_0301_log)) # 
hist(resid(LMEmod_0301_log)) # 



# plotting ::::::::::::::::::::::::::::::::::;;

View(Excretion_master)
Excretion_rate_facetted <- Excretion_master %>% 
                            dplyr::filter(!ExcretionRate_umol_mL_hr_TDWbfactor > 30) %>% # two outliers?
                            ggplot(aes(x = factor(Date), 
                                       y = ExcretionRate_umol_mL_hr_TDWbfactor, 
                                       fill = pCO2)) +
                            geom_boxplot(alpha = 0.5, # color hue
                                         width=0.6, # boxplot width
                                         outlier.size=0, # make outliers small
                                         position = position_dodge(preserve = "single")) + 
                            geom_point(pch = 19, 
                                       position = position_jitterdodge(0.01), 
                                       size=1) +
                            scale_fill_manual(values=c("forestgreen","orange")) +
                            theme_classic() + 
                            ggtitle("Excretion rate, F1 Scallops") +
                            theme(legend.position="none",
                                  axis.title.y=element_text(size=7),
                                  axis.title.x=element_text(size=7),
                                  axis.text.x=element_text(size=7)) +
                            #ylim(0, 0.2) +
                            stat_summary(fun.y=mean, 
                                         geom = "errorbar", 
                                         aes(ymax = ..y.., ymin = ..y..), 
                                         width = 0.6, 
                                         size=0.4, 
                                         linetype = "dashed", 
                                         position = position_dodge(preserve = "single"))  +
                            facet_wrap(~Date, scales = "free")
Excretion_rate <- Excretion_master %>% 
                    dplyr::filter(!ExcretionRate_umol_mL_hr_TDWbfactor > 30) %>% # two outliers?
                    ggplot(aes(x = factor(Date), 
                               y = ExcretionRate_umol_mL_hr_TDWbfactor, 
                               fill = pCO2)) +
                    geom_boxplot(alpha = 0.5, # color hue
                                 width=0.6, # boxplot width
                                 outlier.size=0, # make outliers small
                                 position = position_dodge(preserve = "single")) + 
                    geom_point(pch = 19, 
                               position = position_jitterdodge(0.01), 
                               size=1) +
                    scale_fill_manual(values=c("forestgreen","orange")) +
                    theme_classic() + 
                    theme(legend.position="none",
                          axis.title.y=element_text(size=7),
                          axis.title.x=element_text(size=7),
                          axis.text.x=element_text(size=7)) +
                    #ylim(0, 0.2) +
                    stat_summary(fun.y=mean, 
                                 geom = "errorbar", 
                                 aes(ymax = ..y.., ymin = ..y..), 
                                 width = 0.6, 
                                 size=0.4, 
                                 linetype = "dashed", 
                                 position = position_dodge(preserve = "single")) # no facet




# output the plot 
library(ggpubr)
pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/ER_Boxplots_TDWbfactor.pdf"), width = 7, height= 6)
ggarrange(Excretion_rate_facetted, Excretion_rate, ncol = 1, nrow = 2)
dev.off()

