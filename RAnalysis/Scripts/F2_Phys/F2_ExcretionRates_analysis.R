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

# Summarise Percent Deformities for plotting 
F2_ER_MEANS <- F2_ER_master %>% 
                dplyr::select(Date, pCO2, Replicate, ExcretionRate_umol_mL_hr_TDWbfactor) %>% 
                na.omit() %>% 
                dplyr::group_by(Date, pCO2, Replicate, ) %>% 
                dplyr::summarise(mean_ER_TDWbfactor = mean(ExcretionRate_umol_mL_hr_TDWbfactor), 
                                 n           = n(),
                                 sd_ER_TDWbfactor   = sd(ExcretionRate_umol_mL_hr_TDWbfactor),
                                 se_ER_TDWbfactor   = sd_ER_TDWbfactor/(sqrt(n))) %>% 
                dplyr::mutate(pCO2 = factor(pCO2, levels = c('500 μatm','800 μatm','1200 μatm')))

F2_ER_plot <- F2_ER_master %>% 
                    dplyr::filter(!ExcretionRate_umol_mL_hr_TDWbfactor >40) %>% # one extreme outlier value!
                    ggplot(aes(x = as.factor(Date), 
                               y = ExcretionRate_umol_mL_hr_TDWbfactor, 
                               color = pCO2)) +
                    scale_color_manual(values=c("forestgreen","orange", "purple")) + 
                    stat_summary(fun.y="mean", size = 0.8,
                                 position = position_dodge2(width = 1)) +
                    stat_summary(fun.min = function(x) mean(x) - sd(x)/sqrt(length(x)), 
                                 fun.max = function(x) mean(x) + sd(x)/sqrt(length(x)),
                                 geom = 'errorbar', 
                                 position = position_dodge2(width = .5)) +
                    geom_point(pch = 19, 
                               position = position_jitterdodge(0.2), 
                               size=1) +
                    scale_fill_manual(values=c("forestgreen","orange", "purple")) +
                    theme_classic() + 
                    ggtitle("F2 Scallops: Excretion rate TDW b factor norm (umol NH4 hr)") +
                    theme_classic() +
                    theme(panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(), 
                          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                          axis.text=element_text(size=12),
                          legend.position="none") # no facet

# output the plot 
pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/F2/F2_ER_Boxplor_TDWbfactor.pdf"), width = 7, height= 6)
print(F2_ER_plot)
dev.off()

# STATS (not done below... need to do this - write the loop instead!)
unique(F2_ER_master$Date) # 20230131 20230223 20230327
F2_ER_MEANS <- F2_ER_MEANS %>% dplyr::filter(!mean_ER_TDWbfactor > 40)
F2_ER_MEANS_131 <- F2_ER_MEANS %>% filter(Date %in% '20230131') # call data 
F2_ER_MEANS_223 <- F2_ER_MEANS %>% filter(Date %in% '20230223') # call data 
F2_ER_MEANS_327 <- F2_ER_MEANS %>% filter(Date %in% '20230327') # call data 



# 20230131 :::::::::::::::::::::::::::::::
# LME mod -  data 
library(pander)
LMmod_131        <-lm(mean_ER_TDWbfactor ~ pCO2, data=F2_ER_MEANS_131) # cahmber tank = = random factor (ii.e. 8_C, 7.5_C, 8_A, etc.)
shapiro.test(resid(LMmod_131)) #  0.7686 normal
leveneTest(LMmod_131) # 0.1219 - pass
pander(anova(LMmod_131), style='rmarkdown') # anova table of lmer
  # |    &nbsp;     | Df | Sum Sq | Mean Sq | F value | Pr(>F) |
  # |:-------------:|:--:|:------:|:-------:|:-------:|:------:|
  # |   **pCO2**    | 2  | 4.961  |  2.481  | 0.8468  | 0.4603 |
  # | **Residuals** | 9  | 26.37  |  2.93   |   NA    |   NA   |
qqnorm(resid(LMEmod_131)) # 
hist(resid(LMEmod_131)) # 


# 20230223 :::::::::::::::::::::::::::::::
# LME mod -  data 
LMmod_0223 <- lm(mean_ER_TDWbfactor ~ pCO2,data=F2_ER_MEANS_223) # 
shapiro.test(resid(LMmod_0223)) # 0.0498 -  non normal
leveneTest(LMmod_0223) # 0.4887 - pass
KWmod_0223 <- kruskal.test(mean_ER_TDWbfactor ~ pCO2,data=F2_ER_MEANS_223) # 
pander(KWmod_0223, style='rmarkdown') # KW table
#   | Test statistic | df | P value |
#   |:--------------:|:--:|:-------:|
#   |     3.458      | 2  | 0.1775  |
shapiro.test(resid(LMEmod_0223)) # 0.06603 -  normal
qqnorm(resid(LMEmod_0223)) # 
hist(resid(LMEmod_0223)) # 



# 20230327 :::::::::::::::::::::::::::::::
# LME mod -  data 
LMmod_0327 <- lm(mean_ER_TDWbfactor ~ pCO2,data=F2_ER_MEANS_327) # 
shapiro.test(resid(LMmod_0327)) # 0.6742 -   normal
leveneTest(LMmod_0327) # 0.2248 - pass
pander(aov(LMmod_0327), style='rmarkdown') # anovat table
# |    &nbsp;     | Df | Sum Sq | Mean Sq | F value | Pr(>F) |
# |:-------------:|:--:|:------:|:-------:|:-------:|:------:|
# |   **pCO2**    | 2  | 3.588  |  1.794  |  2.185  | 0.1469 |
# | **Residuals** | 15 | 12.31  |  0.821  |   NA    |   NA   |# 








