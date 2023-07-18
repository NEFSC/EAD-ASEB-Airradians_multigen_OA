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
ggarrange(F2_ER_plot_facetted, F2_ER_plot, ncol = 1, nrow = 2)
dev.off()

# STATS (not done below... need to do this - write the loop instead!)
unique(F2_ER_master$Date)
F2_ER_master_211026 <- F2_ER_master %>% filter(Date %in% '20211026') # call data 
F2_ER_master_0202 <- F2_ER_master %>% filter(Date %in% '20220202') # call data 
F2_ER_master_0301 <- F2_ER_master %>% filter(Date %in% '20220301') # call data 
F2_ER_master_0922 <- F2_ER_master %>% filter(Date %in% '20220922') # call data 
F2_ER_master_221026 <- F2_ER_master %>% filter(Date %in% '20221026') # call data 


# 20211026 :::::::::::::::::::::::::::::::
# LME mod -  data 
LMEmod_211026          <-lme(ExcretionRate_umol_mL_hr_TDWbfactor ~ pCO2, random=~1|Chamber_tank, data=F2_ER_master_211026) # cahmber tank = = random factor (ii.e. 8_C, 7.5_C, 8_A, etc.)
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
LMEmod_0202           <-lme(ExcretionRate_umol_mL_hr_TDWbfactor ~ pCO2, random=~1|Chamber_tank, data=F2_ER_master_0202) # cahmber tank = = random factor (ii.e. 8_C, 7.5_C, 8_A, etc.)
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
LMEmod_0301           <-lme(ExcretionRate_umol_mL_hr_TDWbfactor ~ pCO2, random=~1|Chamber_tank, data=F2_ER_master_0301) # cahmber tank = = random factor (ii.e. 8_C, 7.5_C, 8_A, etc.)
pander(anova(LMEmod_0301), style='rmarkdown') # anova table of lmer
#   |     &nbsp;      | numDF | denDF | F-value |  p-value  |
#   |:---------------:|:-----:|:-----:|:-------:|:---------:|
#   | **(Intercept)** |   1   |  14   |  43.18  | 1.247e-05 |
#   |    **pCO2**     |   1   |  14   |  2.231  |  0.1574   |
shapiro.test(resid(LMEmod_0301)) # 0.002147 - non normal
qqnorm(resid(LMEmod_0301)) # 
hist(resid(LMEmod_0301)) # 

# LME mod -  transformed data 
LMEmod_0301_log           <-lme(log(ExcretionRate_umol_mL_hr_TDWbfactor) ~ pCO2, random=~1|Chamber_tank, data=F2_ER_master_0301) # cahmber tank = = random factor (ii.e. 8_C, 7.5_C, 8_A, etc.)
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
LMEmod_0922          <-lme(ExcretionRate_umol_mL_hr_TDWbfactor ~ pCO2, random=~1|Chamber_tank, data=F2_ER_master_0922) # cahmber tank = = random factor (ii.e. 8_C, 7.5_C, 8_A, etc.)
pander(anova(LMEmod_0922), style='rmarkdown') # anova table of lmer
shapiro.test(resid(LMEmod_0922)) # 0.002147 - non normal

LMEmod_0922_T          <-lme(log(ExcretionRate_umol_mL_hr_TDWbfactor) ~ pCO2, random=~1|Chamber_tank, data=F2_ER_master_0922) # cahmber tank = = random factor (ii.e. 8_C, 7.5_C, 8_A, etc.)
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
LMEmod_221026          <-lme(ExcretionRate_umol_mL_hr_TDWbfactor ~ pCO2, random=~1|Chamber_tank, data=F2_ER_master_221026) # cahmber tank = = random factor (ii.e. 8_C, 7.5_C, 8_A, etc.)
LMmod_221026_aov       <-lm(ExcretionRate_umol_mL_hr_TDWbfactor ~ pCO2, data=F2_ER_master_221026) # cahmber tank = = random factor (ii.e. 8_C, 7.5_C, 8_A, etc.)


shapiro.test(resid(LMEmod_221026)) #  non normal
shapiro.test(resid(LMmod_221026_aov)) #  non normal

LMEmod_221026_T          <-lme(log(ExcretionRate_umol_mL_hr_TDWbfactor) ~ pCO2, random=~1|Chamber_tank, data=F2_ER_master_221026) # cahmber tank = = random factor (ii.e. 8_C, 7.5_C, 8_A, etc.)
LMmod_221026_T          <-lm(log(ExcretionRate_umol_mL_hr_TDWbfactor) ~ pCO2, data=F2_ER_master_221026) # cahmber tank = = random factor (ii.e. 8_C, 7.5_C, 8_A, etc.)

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

View(F2_ER_master)
Excretion_rate_facetted <- F2_ER_master %>% 
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
Excretion_rate <- F2_ER_master %>% 
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

