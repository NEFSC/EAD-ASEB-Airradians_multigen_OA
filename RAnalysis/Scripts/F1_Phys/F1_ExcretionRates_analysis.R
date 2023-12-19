# ---
# title: "F1_ExcretionRate"
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
F1_ER_master <- read.csv(file="C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/F1/F1_ExcretionRates_master.csv", header=T)
unique(F1_ER_master$Date) # 20211026 20220202 20220301 20220922 20221026 - all three dataes - yay! corrected for b factor 1.07
# NOTE: view the bfactor norm R script, B factro using TDW for all data in this mutligen study!


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ANALYSIS AND PLOTTING  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# PLOTS

# Summarise Percent Deformities for plotting 
F1_ER_MEANS <- F1_ER_master %>% 
  dplyr::select(Date, pCO2, Replicate, ExcretionRate_umol_mL_hr_TDWbfactor) %>% 
  na.omit() %>% 
  dplyr::group_by(Date, pCO2, Replicate, ) %>% 
  dplyr::summarise(mean_ER_TDWbfactor = mean(ExcretionRate_umol_mL_hr_TDWbfactor), 
                   n           = n(),
                   sd_ER_TDWbfactor   = sd(ExcretionRate_umol_mL_hr_TDWbfactor),
                   se_ER_TDWbfactor   = sd_ER_TDWbfactor/(sqrt(n))) %>% 
  dplyr::mutate(pCO2 = factor(pCO2, levels = c('500 μatm','800 μatm','1200 μatm')))

F1_ER_plot <- F1_ER_master %>% 
  dplyr::filter(!ExcretionRate_umol_mL_hr_TDWbfactor >40) %>% # one extreme outlier value!
  ggplot(aes(x = as.factor(Date), 
             y = ExcretionRate_umol_mL_hr_TDWbfactor, 
             color = pCO2)) +
  scale_color_manual(values=c("forestgreen","orange")) + 
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
  ggtitle("F1 Scallops: Excretion rate TDW b factor norm (umol NH4 hr)") +
  theme_classic() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text=element_text(size=12),
        legend.position="none") # no facet

# output the plot 
pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/F1/F1_ER_Boxplor_TDWbfactor.pdf"), width = 7, height= 6)
print(F1_ER_plot)
dev.off()

# STATS (not done below... need to do this - write the loop instead!)
unique(F1_ER_master$Date) # 20211026 20220202 20220301 20220922 20221026
F1_ER_MEANS <- F1_ER_MEANS %>% dplyr::filter(!mean_ER_TDWbfactor > 40)
F1_ER_MEANS_211026 <- F1_ER_MEANS %>% filter(Date %in% '20211026') # call data 
F1_ER_MEANS_0202   <- F1_ER_MEANS %>% filter(Date %in% '20220202') # call data 
F1_ER_MEANS_0301   <- F1_ER_MEANS %>% filter(Date %in% '20220301') # call data 
F1_ER_MEANS_0922   <- F1_ER_MEANS %>% filter(Date %in% '20220922') # call data 
F1_ER_MEANS_221026 <- F1_ER_MEANS %>% filter(Date %in% '20221026') # call data 

# 20211026 :::::::::::::::::::::::::::::::
# LME mod -  data 
library(pander)
LMmod_211026 <- lm(mean_ER_TDWbfactor ~ pCO2, data=F1_ER_MEANS_211026) # cahmber tank = = random factor (ii.e. 8_C, 7.5_C, 8_A, etc.)
shapiro.test(resid(LMmod_211026)) #  0.5065 normal
leveneTest(LMmod_211026) # 0.05557 . - pass
pander(anova(LMmod_211026), style='rmarkdown') # anova table of lmer
#   |    &nbsp;     | Df | Sum Sq | Mean Sq | F value | Pr(>F) |
#   |:-------------:|:--:|:------:|:-------:|:-------:|:------:|
#   |   **pCO2**    | 1  | 7.979  |  7.979  |  2.631  | 0.1559 |
#   | **Residuals** | 6  |  18.2  |  3.033  |   NA    |   NA   |


# 20220202 :::::::::::::::::::::::::::::::
# LME mod -  data 
LMmod_0202 <- lm(mean_ER_TDWbfactor ~ pCO2,data=F1_ER_MEANS_0202) # 
shapiro.test(resid(LMmod_0202)) # 0.056 -  normal
leveneTest(LMmod_0202) # 0.8835 - pass
pander(aov(LMmod_0202), style='rmarkdown') # KW table
#   |    &nbsp;     | Df | Sum Sq | Mean Sq | F value | Pr(>F) |
#   |:-------------:|:--:|:------:|:-------:|:-------:|:------:|
#   |   **pCO2**    | 1  | 9.315  |  9.315  |  1.117  | 0.3084 |
#   | **Residuals** | 14 | 116.7  |  8.336  |   NA    |   NA   |


# 20220301 :::::::::::::::::::::::::::::::
# LME mod -  data 
LMmod_0301 <- lm(mean_ER_TDWbfactor ~ pCO2,data=F1_ER_MEANS_0301) # 
shapiro.test(resid(LMmod_0301)) # 0.159 -   normal
leveneTest(LMmod_0301) # 0.8203 - pass
pander(aov(LMmod_0301), style='rmarkdown') # anova table
#   |    &nbsp;     | Df | Sum Sq | Mean Sq | F value | Pr(>F)  |
#   |:-------------:|:--:|:------:|:-------:|:-------:|:-------:|
#   |   **pCO2**    | 1  | 3.181  |  3.181  |  4.987  | 0.04238 |
#   | **Residuals** | 14 |  8.93  | 0.6379  |   NA    |   NA    |



# 20220922 :::::::::::::::::::::::::::::::
# LME mod -  data 
LMmod_0922 <- lm(mean_ER_TDWbfactor ~ pCO2,data=F1_ER_MEANS_0922) # 
shapiro.test(resid(LMmod_0922)) # 0.0155 -  non- normal
leveneTest(LMmod_0922) # 0.3203 - pass
KEmod_0922 <- kruskal.test(mean_ER_TDWbfactor ~ pCO2,data=F1_ER_MEANS_0922) # 
pander(KEmod_0922, style='rmarkdown') # anova table
#   | Test statistic | df | P value |
#   |:--------------:|:--:|:-------:|
#   |       3        | 1  | 0.08326 |


# 20221026 :::::::::::::::::::::::::::::::
# LME mod -  data 
LMmod_221026 <- lm(mean_ER_TDWbfactor ~ pCO2,data=F1_ER_MEANS_221026) # 
shapiro.test(resid(LMmod_221026)) # 0.0006315 -  non- normal
leveneTest(LMmod_221026) # 0.5705 - pass
KEmod_221026 <- kruskal.test(mean_ER_TDWbfactor ~ pCO2,data=F1_ER_MEANS_221026) # 
pander(KEmod_221026, style='rmarkdown') # anova table
#   | Test statistic | df | P value |
#   |:--------------:|:--:|:-------:|
#   |     0.3265     | 1  | 0.5677  |




