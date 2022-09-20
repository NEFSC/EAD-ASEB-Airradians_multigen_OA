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
Excretion_data <- read.csv(file="Data/Excretion_rates/Excretion_master.csv", header=T,stringsAsFactors=FALSE, fileEncoding="latin1") # master data file
Size_data   <- read.csv(file="Data/Respiration/Lengths_Condition_resp_clearance.csv", header=T) 




# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# EDIT AND MERG DATA  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

list(unique(Excretion_data$Date)) # 20220202 20220301 - call these dates in te size data (look below)

Size_data_2 <- Size_data %>% 
  dplyr::mutate(Date = paste("20",(format(as.Date(Date, "%m/%d/%Y"), "%y%m%d")), sep ='')) %>% # change format of the date to the format in Excretion_data
  dplyr::select(-c(Food, Center)) %>%  # get rid of unneeded column(s)
  dplyr::filter(Date %in% unique(Excretion_data$Date)) # 20220202 20220301 - call these dates in te size dat

Excretion_master <- merge(Excretion_data, Size_data_2) %>% # merge size and excretion datadata
  dplyr::filter(!ExcretionRate_umol_mL_hr < 0) %>% # omit rates that were less than the blank
  dplyr::mutate(ExcretionRate_umol_mL_hr_gTDW =  ExcretionRate_umol_mL_hr/Dry_Tissue_weight) %>% # correct ExcretionRate_umol_mL_hr for gram of Tissue Dry WEight
  dplyr::mutate(ExcretionRate_ug_mL_hr_gTDW = ExcretionRate_ug_mL_hr/Dry_Tissue_weight) %>% # correct ExcretionRate_umol_mL_hr for gram of Tissue Dry WEight
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


Excretion_rate_boxplot <- Excretion_master %>% 
                            ggplot(aes(pCO2 , ExcretionRate_umol_mL_hr_gTDW , fill = pCO2)) +
                            theme(panel.grid=element_blank()) +
                            geom_boxplot(size=0.2, alpha=0.1, aes(fill=pCO2)) +
                            scale_fill_manual(values=c("white", "grey50")) +
                            geom_point(shape = 21, size = 2, position = position_jitterdodge(jitter.width = 0.1)) +
                            theme_classic() +
                            theme(axis.text=element_text(size=12),
                                  axis.title=element_text(size=10)) +
                            stat_summary(fun.y=mean, geom="point", shape=18, size=4, color="black", fill="white") +
                            ggtitle("Excretion rate, F1 Scallops") +
                            facet_wrap(~Date)
Excretion_rate_boxplot



# output the plot 
pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/Master_ER_Boxplots.pdf"), width = 7, height= 6)
print(Excretion_rate_boxplot)
dev.off()

