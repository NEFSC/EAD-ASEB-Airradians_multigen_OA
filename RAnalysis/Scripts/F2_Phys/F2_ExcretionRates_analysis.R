# ---
# title: "F2_ExcretionRate"
# author: "Samuel Gurr"
# date: "9/20/2022"
# output: pdf_document
# ---


library(dplyr)
library(ggplot2)
library(nlme)
library(lme4)
library(car)
library(kable)
library(pander)
library(ggpubr)
# SET WORKING DIRECTORY 
setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis") # personal computer

# LOAD DATA
F2_ER_master <- read.csv(file="C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/F2/F2_ExcretionRates_master.csv", header=T)
unique(F2_ER_master$Date) # 20230131 20230223 20230327 - all three dataes - yay! corrected for b factor 1.07
# NOTE: view the bfactor norm R script, B factro using TDW for all data in this mutligen study!


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ANALYSIS AND PLOTTING  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# PLOTS
F2_ER_master$pCO2 <- factor(F2_ER_master$pCO2, levels = c('500 μatm','800 μatm','1200 μatm'))
F2_ER_plot_facetted <- F2_ER_master %>% 
                dplyr::filter(!ExcretionRate_umol_mL_hr_TDWbfactor >40) %>% # one extreme outlier value!
                ggplot(aes(x = pCO2, 
                           y = ExcretionRate_umol_mL_hr_TDWbfactor, 
                           fill = pCO2)) +
                geom_boxplot(alpha = 0.5, # color hue
                             width=0.6, # boxplot width
                             outlier.size=0, # make outliers small
                             position = position_dodge(preserve = "single")) + 
                geom_point(pch = 19, 
                           position = position_jitterdodge(0.01), 
                           size=1) +
                scale_fill_manual(values=c("forestgreen","orange", "purple")) +
                theme_classic() + 
                ggtitle("F2 Scallops: Excretion rate TDW b factor norm (umol NH4 hr)") +
                theme(
                    # legend.position="none", # if you want to omit the lengend!
                      axis.title.y=element_text(size=7),
                      axis.title.x=element_blank(),
                      axis.text.x=element_blank()) +
                stat_summary(fun.y=mean, 
                             geom = "errorbar", 
                             aes(ymax = ..y.., ymin = ..y..), 
                             width = 0.6, 
                             size=0.4, 
                             linetype = "dashed", 
                             position = position_dodge(preserve = "single"))  +
                facet_wrap(~Date)
# print(F2_ER_plot_facetted)


F2_ER_plot <- F2_ER_master %>% 
                    dplyr::filter(!ExcretionRate_umol_mL_hr_TDWbfactor >40) %>% # one extreme outlier value!
                    ggplot(aes(x = as.factor(Date), 
                               y = ExcretionRate_umol_mL_hr_TDWbfactor, 
                               fill = pCO2)) +
                    geom_boxplot(alpha = 0.5, # color hue
                                 width=0.6, # boxplot width
                                 outlier.size=0, # make outliers small
                                 position = position_dodge(preserve = "single")) + 
                    geom_point(pch = 19, 
                               position = position_jitterdodge(0.01), 
                               size=1) +
                    scale_fill_manual(values=c("forestgreen","orange", "purple")) +
                    theme_classic() + 
                    theme(
                      # legend.position="none", # if you want to omit the lengend!
                      axis.title.y=element_text(size=7),
                      axis.title.x=element_text(size=7),
                      axis.text.x=element_text(size=7)) +
                    stat_summary(fun.y=mean, 
                                 geom = "errorbar", 
                                 aes(ymax = ..y.., ymin = ..y..), 
                                 width = 0.6, 
                                 size=0.4, 
                                 linetype = "dashed", 
                                 position = position_dodge(preserve = "single")) # no facet

# output the plot 
pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/F2/F2_ER_Boxplor_TDWbfactor.pdf"), width = 7, height= 6)
print(F2_ER_plot_facetted)
dev.off()

# STATS (not done below... need to do this - write the loop instead!)
unique(F2_ER_master$Date) # 20230131 20230223 20230327
F2_ER_master_131 <- F2_ER_master %>% filter(Date %in% '20230131') # call data 
F2_ER_master_223 <- F2_ER_master %>% filter(Date %in% '20230223') # call data 
F2_ER_master_327 <- F2_ER_master %>% filter(Date %in% '20230327') # call data 



# 20230131 :::::::::::::::::::::::::::::::
# LME mod -  data 
LMEmod_131        <-lme(ExcretionRate_umol_mL_hr_TDWbfactor ~ pCO2, random=~1|Chamber_tank, data=F2_ER_master_131) # cahmber tank = = random factor (ii.e. 8_C, 7.5_C, 8_A, etc.)
shapiro.test(resid(LMEmod_131)) #  0.3082 normal
pander(anova(LMEmod_131), style='rmarkdown') # anova table of lmer
# |     &nbsp;      | numDF | denDF | F-value |  p-value  |
# |:---------------:|:-----:|:-----:|:-------:|:---------:|
# | **(Intercept)** |   1   |   9   |  57.35  | 3.421e-05 |
# |    **pCO2**     |   2   |   9   | 0.7474  |  0.5008   |
qqnorm(resid(LMEmod_131)) # 
hist(resid(LMEmod_131)) # 


# 20230223 :::::::::::::::::::::::::::::::
# LME mod -  data 
LMEmod_0223          <-lme(ExcretionRate_umol_mL_hr_TDWbfactor ~ pCO2, random=~1|Chamber_tank, data=F2_ER_master_223) # cahmber tank = = random factor (ii.e. 8_C, 7.5_C, 8_A, etc.)
pander(anova(LMEmod_0223), style='rmarkdown') # anova table of lmer
shapiro.test(resid(LMEmod_0223)) # 7.24e-07 -  non normal

# |     &nbsp;      | numDF | denDF | F-value | p-value |
# |:---------------:|:-----:|:-----:|:-------:|:-------:|
# | **(Intercept)** |   1   |  18   |  6.818  | 0.01768 |
# |    **pCO2**     |   2   |  18   |  1.179  | 0.3304  |
shapiro.test(resid(LMEmod_0223)) # 0.06603 -  normal
qqnorm(resid(LMEmod_0223)) # 
hist(resid(LMEmod_0223)) # 



# 20230327 :::::::::::::::::::::::::::::::
# LME mod -  data 
LMEmod_327           <-lme(ExcretionRate_umol_mL_hr_TDWbfactor ~ pCO2, random=~1|Chamber_tank, data=F2_ER_master_327) # cahmber tank = = random factor (ii.e. 8_C, 7.5_C, 8_A, etc.)
shapiro.test(resid(LMEmod_327)) # 0.7467 - normal
pander(anova(LMEmod_327), style='rmarkdown') # anova table of lmer
# |     &nbsp;      | numDF | denDF | F-value |  p-value  |
# |:---------------:|:-----:|:-----:|:-------:|:---------:|
# | **(Intercept)** |   1   |  15   |  126.5  | 1.045e-08 |
# |    **pCO2**     |   2   |  15   |  1.562  |   0.242   |
qqnorm(resid(LMEmod_327)) # 
hist(resid(LMEmod_327)) # 









