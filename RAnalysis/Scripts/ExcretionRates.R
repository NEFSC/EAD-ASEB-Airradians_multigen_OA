# ---
# title: "ExcretionRate"
# author: "Samuel Gurr"
# date: "9/20/2022"
# output: pdf_document
# ---




#install.packages("pander")
library(dplyr)
library(ggplot2)
library(nlme)
library(lme4)
library(car)
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

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# GET B FACTOR FOR ALL AVAILABLE INDIVIDUALS WITH TDW AND MO2 ::::::::::::::::::::::::::::::
ER            <- ER %>% filter(!is.na(ER$ExcretionRate_umol_mL_hr)) 
ER$log10_VER  <- log10(as.numeric(ER$ExcretionRate_umol_mL_hr)) # assign resp value
ER$log10_TDW  <- log10(as.numeric(ER$Dry_Tissue_weight)) # assign length value 
#summary(lm(RR_master_OM$log10_VO2~RR_master_OM$log10_TDW)) # 0.79749 == b factor

# View(ER)

ER_b.factor_PLOT <- ER %>% 
  ggplot(aes(x=log10_TDW, y=log10_VER)) +
  geom_point() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ 
  scale_x_continuous(name ="log10_BodyMass; TDW in g") +
  scale_y_continuous(name ="log10_ER; ER in umol L-1 hr-1)") +
  theme_classic() +
  theme(legend.position="none",axis.title.y=element_text(size=7)) +
  ggtitle("Excretion rate scaling: log10_MO2 = log10_a + (b.factor * log10_BodyMass)") +
  geom_smooth(method = lm, color = 'red') +
  ggpmisc::stat_poly_eq(parse=T, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), label.x.npc = "left")

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# NNORMALIZED BASED ON B FACTOR 0.78 (ABOVE)                  ::::::::::::::::::::::::::::::

meanTDW <- mean(as.numeric(Size_data_2$Dry_Tissue_weight)) # 0.4729451


Excretion_master <- merge(Excretion_data, Size_data_2) %>% # merge size and excretion datadata
  dplyr::filter(!ExcretionRate_umol_mL_hr < 0) %>% # 3 excretion < 0 omit (20211026 7.5C, 20220202 7.5C, 20220202 8.0C)
  dplyr::mutate(ExcretionRate_umol_mL_hr_TDWbfactor =  ExcretionRate_umol_mL_hr*( (meanTDW/(as.numeric(Dry_Tissue_weight)))^0.78) ) %>% # correct ExcretionRate_umol_mL_hr for gram of Tissue Dry WEight
  dplyr::mutate(pCO2 = case_when(pH == 8.0 ~ "500 μatm", pH == 7.5 ~ "800 μatm"))


ER_plot_raw <- Excretion_master %>% 
  #dplyr::filter(!ExcretionRate_umol_mL_hr_TDWbfactor > 30) %>% # two outliers?
  ggplot(aes(x = factor(Date), 
             y = ExcretionRate_umol_mL_hr, 
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
  ggtitle("F1 Scallops: Excretion rate raw (umol O2 hr)") +
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


ER_plot_normalized <- Excretion_master %>% 
  #dplyr::filter(!ExcretionRate_umol_mL_hr_TDWbfactor > 30) %>% # two outliers?
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
  ggtitle("F1 Scallops: Excretion rate TDW normalized (umol O2 hr)") +
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


# WRITE CSV OF THE MASTER FILE
write.csv(Excretion_master, "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/ExcretionRates_master.csv")



# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ANALYSIS AND PLOTTING  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# WRITE CSV OF THE MASTER FILE

ER <- read.csv(file="C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/ExcretionRates_master.csv", header=T)


nrow(ER)
Dry_Tissue_weight







# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ANALYSIS AND PLOTTING  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
library(kable)
library(pander)

unique(Excretion_master$Date)
Excretion_master_211026 <- Excretion_master %>% filter(Date %in% '20211026') # call data 
Excretion_master_0202 <- Excretion_master %>% filter(Date %in% '20220202') # call data 
Excretion_master_0301 <- Excretion_master %>% filter(Date %in% '20220301') # call data 
Excretion_master_0922 <- Excretion_master %>% filter(Date %in% '20220922') # call data 
Excretion_master_221026 <- Excretion_master %>% filter(Date %in% '20221026') # call data 





# 20211026 :::::::::::::::::::::::::::::::
# LME mod -  data 
LMEmod_211026          <-lme(ExcretionRate_umol_mL_hr_TDWbfactor ~ pCO2, random=~1|Chamber_tank, data=Excretion_master_211026) # cahmber tank = = random factor (ii.e. 8_C, 7.5_C, 8_A, etc.)
shapiro.test(resid(LMEmod_211026)) #  0.2434 normal
pander(anova(LMEmod_211026), style='rmarkdown') # anova table of lmer
#   |     &nbsp;      | numDF | denDF | F-value |  p-value  |
#   |:---------------:|:-----:|:-----:|:-------:|:---------:|
#   | **(Intercept)** |   1   |   9   |  41.89  | 0.0001151 |
#   |    **pCO2**     |   1   |   6   | 0.5117  |  0.5013   |
qqnorm(resid(LMEmod_211026)) # 
hist(resid(LMEmod_211026)) # 








# 20220202 :::::::::::::::::::::::::::::::
# LME mod -  data 
LMEmod_0202           <-lme(ExcretionRate_umol_mL_hr_TDWbfactor ~ pCO2, random=~1|Chamber_tank, data=Excretion_master_0202) # cahmber tank = = random factor (ii.e. 8_C, 7.5_C, 8_A, etc.)
pander(anova(LMEmod_0202), style='rmarkdown') # anova table of lmer
#   |     &nbsp;      | numDF | denDF | F-value |  p-value  |
#   |:---------------:|:-----:|:-----:|:-------:|:---------:|
#   | **(Intercept)** |   1   |  14   |  82.37  | 3.062e-07 |
#   |    **pCO2**     |   1   |  14   | 0.8734  |  0.3659   |
shapiro.test(resid(LMEmod_0202)) # 0.06603 -  normal
qqnorm(resid(LMEmod_0202)) # 
hist(resid(LMEmod_0202)) # 



# 20220301 :::::::::::::::::::::::::::::::
# LME mod -  data 
LMEmod_0301           <-lme(ExcretionRate_umol_mL_hr_TDWbfactor ~ pCO2, random=~1|Chamber_tank, data=Excretion_master_0301) # cahmber tank = = random factor (ii.e. 8_C, 7.5_C, 8_A, etc.)
pander(anova(LMEmod_0301), style='rmarkdown') # anova table of lmer
#   |     &nbsp;      | numDF | denDF | F-value |  p-value  |
#   |:---------------:|:-----:|:-----:|:-------:|:---------:|
#   | **(Intercept)** |   1   |  14   |  43.18  | 1.247e-05 |
#   |    **pCO2**     |   1   |  14   |  2.231  |  0.1574   |
shapiro.test(resid(LMEmod_0301)) # 0.002147 - non normal
qqnorm(resid(LMEmod_0301)) # 
hist(resid(LMEmod_0301)) # 

# LME mod -  transformed data 
LMEmod_0301_log           <-lme(log(ExcretionRate_umol_mL_hr_TDWbfactor) ~ pCO2, random=~1|Chamber_tank, data=Excretion_master_0301) # cahmber tank = = random factor (ii.e. 8_C, 7.5_C, 8_A, etc.)
pander(anova(LMEmod_0301_log), style='rmarkdown') # anova table of lmer
#   |     &nbsp;      | numDF | denDF | F-value |  p-value  |
#   |:---------------:|:-----:|:-----:|:-------:|:---------:|
#   | **(Intercept)** |   1   |  14   |  21.52  | 0.0003827 |
#   |    **pCO2**     |   1   |  14   |  1.274  |  0.2781   |
shapiro.test(resid(LMEmod_0301_log)) # 0.05217 - normal
qqnorm(resid(LMEmod_0301_log)) # 
hist(resid(LMEmod_0301_log)) # 






# 20220922 :::::::::::::::::::::::::::::::
# LME mod -  data 
LMEmod_0922          <-lme(ExcretionRate_umol_mL_hr_TDWbfactor ~ pCO2, random=~1|Chamber_tank, data=Excretion_master_0922) # cahmber tank = = random factor (ii.e. 8_C, 7.5_C, 8_A, etc.)
pander(anova(LMEmod_0922), style='rmarkdown') # anova table of lmer
shapiro.test(resid(LMEmod_0922)) # 0.002147 - non normal

LMEmod_0922_T          <-lme(log(ExcretionRate_umol_mL_hr_TDWbfactor) ~ pCO2, random=~1|Chamber_tank, data=Excretion_master_0922) # cahmber tank = = random factor (ii.e. 8_C, 7.5_C, 8_A, etc.)
pander(anova(LMEmod_0922_T), style='rmarkdown') # anova table of lmer
# |     &nbsp;      | numDF | denDF | F-value | p-value  |
# |:---------------:|:-----:|:-----:|:-------:|:--------:|
# | **(Intercept)** |   1   |   6   |  15.67  | 0.007461 |
# |    **pCO2**     |   1   |   6   |  1.945  |  0.2125  |
shapiro.test(resid(LMEmod_0922_T)) # 0.01509 - non normal
norm(resid(LMEmod_0922_T)) # 
hist(resid(LMEmod_0922_T)) # 





# 20221026 :::::::::::::::::::::::::::::::
# LME mod -  data 
LMEmod_221026          <-lme(ExcretionRate_umol_mL_hr_TDWbfactor ~ pCO2, random=~1|Chamber_tank, data=Excretion_master_221026) # cahmber tank = = random factor (ii.e. 8_C, 7.5_C, 8_A, etc.)
LMmod_221026_aov       <-lm(ExcretionRate_umol_mL_hr_TDWbfactor ~ pCO2, data=Excretion_master_221026) # cahmber tank = = random factor (ii.e. 8_C, 7.5_C, 8_A, etc.)


shapiro.test(resid(LMEmod_221026)) #  non normal
shapiro.test(resid(LMmod_221026_aov)) #  non normal

LMEmod_221026_T          <-lme(log(ExcretionRate_umol_mL_hr_TDWbfactor) ~ pCO2, random=~1|Chamber_tank, data=Excretion_master_221026) # cahmber tank = = random factor (ii.e. 8_C, 7.5_C, 8_A, etc.)
LMmod_221026_T          <-lm(log(ExcretionRate_umol_mL_hr_TDWbfactor) ~ pCO2, data=Excretion_master_221026) # cahmber tank = = random factor (ii.e. 8_C, 7.5_C, 8_A, etc.)

shapiro.test(resid(LMEmod_221026_T)) # 0.5897 -  normal
shapiro.test(resid(LMmod_221026_T)) # 0.5897 -  normal
pander(anova(LMmod_221026_T), style='rmarkdown') # anova table of lmer
#   |     &nbsp;      | numDF | denDF | F-value |  p-value  |
#   |:---------------:|:-----:|:-----:|:-------:|:---------:|
#   | **(Intercept)** |   1   |  11   |  19.95  | 0.0009523 |
#   |    **pCO2**     |   1   |  11   | 0.03597 |   0.853   |
qqnorm(resid(LMEmod_221026_T)) # 
hist(resid(LMEmod_221026_T)) # 






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

