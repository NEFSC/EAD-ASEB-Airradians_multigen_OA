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
library(Rmisc)
# SET WORKING DIRECTORY 
setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis") # personal computer

# LOAD DATA
F1_ER_master <- read.csv(file="C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/F1/F1_ExcretionRates_master.csv", header=T)

unique(F1_ER_master$Date) # 20211026 20220202 20220301 20220922 20221026 20221116 - all three dataes - yay! corrected for b factor 1.07
# NOTE: view the bfactor norm R script, B factro using TDW for all data in this mutligen study!

F1_ER_master <- F1_ER_master %>% 
                      dplyr::mutate(Age = case_when(Date == "20211026" ~  92,
                                                    Date == "20220202" ~  191,
                                                    Date == "20220301" ~  218,
                                                    Date == "20220922" ~  423,
                                                    Date == "20221026" ~  457)) %>%  # %>% #edit this!
                      dplyr::mutate(pCO2 = factor(pCO2, levels = c('500 μatm','800 μatm','1200 μatm')))


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ANALYSIS AND PLOTTING  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# means by tank replicate - necessary for the ACTUAL data for plotting and statistics!
F1_ER_MEANS <- F1_ER_master %>% 
  # dplyr::filter(!ExcretionRate_umol_hr_Lengthbfactor >40) %>% # one extreme outlier value!
  summarySE(measurevar="ExcretionRate_umol_hr_bFactorNormLength.MEAN", 
            groupvars=c("Date", "Age", "pCO2", "Replicate"))

# (run t tests in lop 
Ttest_Dates       <- as.data.frame(unique(F1_ER_MEANS$Date)) # call a list to loop in 
Ttest_total       <- data.frame() # start dataframe, this will be the master output
DF_loop           <- data.frame(matrix(nrow = 1, ncol = 13)) # create dataframe to save during for loop
colnames(DF_loop) <- c('Date', 'Age', 'Metric', 'model', 
                       'ShapiroWilk', 'ResidNorm', 'Variance', 
                       'HomogVar', 'DF.num' , 'DF.denom', 'Tstat','P_val', 'SigDif') # names for comuns in the for loop
library(purrr)
library(rstatix)

for (i in 1:nrow(Ttest_Dates)) {
  
  date_loop     <- as.character(Ttest_Dates[i,])
  data_loop     <- F1_ER_MEANS %>% 
    dplyr::filter(!pCO2 %in% '1200 μatm') %>% 
    dplyr::filter(Date == date_loop) %>% 
    dplyr::select(Date, Age, pCO2, ExcretionRate_umol_hr_bFactorNormLength.MEAN) %>% 
    na.omit()
  
  DF_loop$Date        <- date_loop
  DF_loop$Age         <- data_loop$Age[1]
  DF_loop$Metric      <- 'ER; LENGTH b factor normalized'
  
  # run assumptions 
  # normality of data 
  normality <- shapiro.test(data_loop$ExcretionRate_umol_hr_bFactorNormLength.MEAN)[[2]]
  # equal variance 
  variance <- var.test(data_loop$ExcretionRate_umol_hr_bFactorNormLength.MEAN~ 
                         as.numeric(as.factor(data_loop$pCO2)))[[3]]
  
  # run all modles
  Ttestmod.eqvar      <- t.test( data_loop$ExcretionRate_umol_hr_bFactorNormLength.MEAN ~ 
                                   (as.factor(data_loop$pCO2)),
                                 var.equal = TRUE)
  
  Ttestmod.noneqvar   <- t.test( data_loop$ExcretionRate_umol_hr_bFactorNormLength.MEAN ~ 
                                   (as.factor(data_loop$pCO2)),
                                 var.equal = FALSE)
  
  Wilcoxmod           <- wilcox.test(data_loop$ExcretionRate_umol_hr_bFactorNormLength.MEAN ~ 
                                       as.numeric(as.factor(data_loop$pCO2)))
  
  # normality tests for the anova model - asign 
  DF_loop$ShapiroWilk <- normality
  
  DF_loop$ResidNorm   <- if( normality > 0.05) {
    'YES'} else {'NO'}
  
  DF_loop$Variance     <- variance
  
  DF_loop$HomogVar    <- if( variance > 0.05) {
    'YES'} else {'NO'}
  
  if(normality > 0.05 & variance > 0.05) {
    DF_loop$model       <- 'Welchs T test, equal variance'
    DF_loop$DF.num      <- map_df(list(Ttestmod.eqvar), tidy)$parameter[[1]]
    DF_loop$DF.denom    <- 'NA'
    DF_loop$Tstat       <- map_df(list(Ttestmod.eqvar), tidy)$statistic[[1]]
    DF_loop$P_val       <- map_df(list(Ttestmod.eqvar), tidy)$p.value[[1]]
    DF_loop$SigDif      <- if( (map_df(list(Ttestmod.eqvar), tidy)$p.value[[1]]) > 0.05) {
      'NO'} else {'YES'}
    
  } else if (normality > 0.05 & variance < 0.05) {
    DF_loop$model       <- 'Welchs T test, non-equal variance'
    DF_loop$DF.num      <- map_df(list(Ttestmod.noneqvar), tidy)$parameter[[1]]
    DF_loop$DF.denom    <- 'NA'
    DF_loop$Tstat       <- map_df(list(Ttestmod.noneqvar), tidy)$statistic[[1]]
    DF_loop$P_val       <- map_df(list(Ttestmod.noneqvar), tidy)$p.value[[1]]
    DF_loop$SigDif      <- if( (map_df(list(Ttestmod.noneqvar), tidy)$p.value[[1]]) > 0.05) {
      'NO'} else {'YES'}
  } else {
    DF_loop$model       <- 'Wilcoxon rank sum exact test'
    DF_loop$DF.num      <- 'NA'
    DF_loop$DF.denom    <- 'NA'
    DF_loop$Tstat       <- map_df(list(Wilcoxmod), tidy)$statistic[[1]]
    DF_loop$P_val       <- map_df(list(Wilcoxmod), tidy)$p.value[[1]]
    DF_loop$SigDif      <- if( (map_df(list(Wilcoxmod), tidy)$p.value[[1]]) > 0.05) {
      'NO'} else {'YES'}            
  }
  # asign loop and cumulative output table
  df          <- data.frame(DF_loop) # name dataframe for this single row
  Ttest_total <- rbind(Ttest_total,DF_loop) #bind to a cumulative list dataframe
  # print(Ttest_total) # print to monitor progress
  
}
write.csv(Ttest_total, "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/F1/F1_ER_LengthbFactor_Ttest.csv")




# PLOTS

# Summarise Percent Deformities for plotting 

# now take the means by Date pCO2  and tank and remove the tank group - means and st error by date and pCO2
F1_ER_MEANS_plotting <- F1_ER_MEANS %>% 
                          # dplyr::filter(!ExcretionRate_umol_hr_TDWbfactor >40) %>% # one extreme outlier value!
                          summarySE(measurevar="ExcretionRate_umol_hr_bFactorNormLength.MEAN", 
                                    groupvars=c("Date", "Age", "pCO2"))


F1_ER_plot <- F1_ER_MEANS_plotting %>% 
                  ggplot(aes(x=as.factor(Age), 
                             y=ExcretionRate_umol_hr_bFactorNormLength.MEAN, 
                             color=as.factor(pCO2))) +
                  geom_point(position=position_dodge(.5))+ 
                  scale_color_manual(values=c("forestgreen",
                                              "darkorange2"))+
                  geom_errorbar(aes(ymin=ExcretionRate_umol_hr_bFactorNormLength.MEAN-se, 
                                    ymax=ExcretionRate_umol_hr_bFactorNormLength.MEAN+se), width=.2,
                                position=position_dodge(.5))+
                  theme_classic() +  
                  xlab("Age (dpf)") + 
                  ggtitle("F1 Scallops: Excretion rate Length b factor norm (umol NH4 hr)") +
                  theme_classic() +
                  theme(legend.position="none",
                        panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank())+ 
                  geom_line(stat = "identity", size=1.0)+
                  scale_y_continuous(name ="ER",expand = c(0, 0), limits = c(0, NA)) +
                  theme(text = element_text(size=10))

# output the plot 
pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/F1/F1_ER_Lengthbfactor.pdf"), width = 4, height = 4)
print(F1_ER_plot)
dev.off()




# Stats
# (1) First, run anova within date for all records (for looped!)
ANOVA_Dates       <- as.data.frame(unique(F1_ER_MEANS$Date)) # call a list to loop in 
AOVdf_total       <- data.frame() # start dataframe, this will be the master output
DF_loop           <- data.frame(matrix(nrow = 1, ncol = 13)) # create dataframe to save during for loop
colnames(DF_loop) <- c('Date', 'Age', 'Metric', 'model', 'ShapiroWilk', 'ResidNorm', 'Levenes', 'HomogVar', 'DF.num' , 'DF.denom', 'F_val','P_val', 'SigDif') # names for comuns in the for loop

for (i in 1:nrow(ANOVA_Dates)) {
  
  date_loop     <- as.character(ANOVA_Dates[i,])
  data_loop     <- F1_ER_MEANS %>% 
    dplyr::filter(Date == date_loop) %>% 
    dplyr::select(Date, Age, pCO2, ExcretionRate_umol_mL_hr_TDWbfactor) %>% 
    na.omit()
  
  AOVmod              <- aov(lm(data_loop$ExcretionRate_umol_mL_hr_TDWbfactor ~ data_loop$pCO2))
  DF_loop$Date        <- date_loop
  DF_loop$Age         <- data_loop$Age[1]
  DF_loop$Metric      <- 'ER; TDW b factor normalized'
  
  # run both modles
  AOVmod              <- aov(lm(data_loop$ExcretionRate_umol_mL_hr_TDWbfactor ~ as.factor(data_loop$pCO2)))
  KWmod               <- kruskal.test(data_loop$ExcretionRate_umol_mL_hr_TDWbfactor  ~ as.factor(data_loop$pCO2))
  
  # normality tests for the anova model - asign 
  DF_loop$ShapiroWilk <- shapiro.test(resid(AOVmod))[[2]]
  DF_loop$ResidNorm   <- if( shapiro.test(resid(AOVmod))[[2]] > 0.05) {
    'YES'} else {'NO'}
  DF_loop$Levenes     <- leveneTest(AOVmod)[[3]][[1]]
  DF_loop$HomogVar    <- if( leveneTest(AOVmod)[[3]][[1]] > 0.05) {
    'YES'} else {'NO'}
  
  if(shapiro.test(resid(AOVmod))[[2]] > 0.05 & leveneTest(AOVmod)[[3]][[1]] > 0.05) {
    DF_loop$model       <- 'one-way AOV; x ~ treatment'
    DF_loop$DF.num      <- summary(AOVmod)[[1]][["Df"]][1]
    DF_loop$DF.denom    <- summary(AOVmod)[[1]][["Df"]][2]
    DF_loop$F_val       <- summary(AOVmod)[[1]][["F value"]][1]
    DF_loop$P_val       <- summary(AOVmod)[[1]][["Pr(>F)"]][1]
    DF_loop$SigDif      <- if( (summary(AOVmod)[[1]][["Pr(>F)"]][1]) > 0.05) {
      'NO'} else {'YES'}
    
  } else {
    DF_loop$model       <- 'kruskal-wallis; x ~ treatment'
    DF_loop$DF.num      <- (KWmod)[[2]][["df"]][1]
    DF_loop$DF.denom    <- NA
    DF_loop$F_val       <- NA
    DF_loop$P_val       <- (KWmod)[[3]]
    DF_loop$SigDif      <- if( ((KWmod)[[3]]) > 0.05) {
      'NO'} else {'YES'}
  }
  
  # asign loop and cumulative output table
  df          <- data.frame(DF_loop) # name dataframe for this single row
  AOVdf_total <- rbind(AOVdf_total,DF_loop) #bind to a cumulative list dataframe
  print(AOVdf_total) # print to monitor progress
  
}
# View(AOVdf_total) # view all the anova tests within data 

# WRITE CSV OF THE MASTER FILE
write.csv(AOVdf_total, "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/F1/F1_ER_TDW_Statistics.csv")
#write.csv(Biodep_Master, "C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/Biodeposition_master.csv")



# Two way model for all data

AllER_TwoWayAnova <- aov(lm(ExcretionRate_umol_mL_hr_TDWbfactor ~ Age*pCO2, data=F1_ER_MEANS))
shapiro.test(resid(AllER_TwoWayAnova)) # 2.373e-06
library(rcompanion)
AllER_SRH <-  scheirerRayHare(ExcretionRate_umol_mL_hr_TDWbfactor ~ Age*pCO2, data=F1_ER_MEANS)
AllER_SRH
#            Df Sum Sq       H p.value
# Age        4 9506.6 31.1692 0.00000
# pCO2       1  314.6  1.0314 0.30984
# Age:pCO2   4  722.4  2.3685 0.66833
# Residuals 50 7344.2 



# STATS (not done below... need to do this - write the loop instead!)
unique(F1_ER_master$Date) # 20211026 20220202 20220301 20220922 20221026
F1_ER_MEANS <- F1_ER_MEANS %>% dplyr::filter(!ExcretionRate_umol_mL_hr_TDWbfactor > 40)
F1_ER_MEANS_211026 <- F1_ER_MEANS %>% filter(Age %in% '20211026') # call data 
F1_ER_MEANS_0202   <- F1_ER_MEANS %>% filter(Age %in% '20220202') # call data 
F1_ER_MEANS_0301   <- F1_ER_MEANS %>% filter(Age %in% '20220301') # call data 
F1_ER_MEANS_0922   <- F1_ER_MEANS %>% filter(Age %in% '20220922') # call data 
F1_ER_MEANS_221026 <- F1_ER_MEANS %>% filter(Date %in% '20221026') # call data 

# 20211026 :::::::::::::::::::::::::::::::
# LME mod -  data 
library(pander)
LMmod_211026 <- lm(ExcretionRate_umol_mL_hr_TDWbfactor ~ pCO2, data=F1_ER_MEANS_211026) # cahmber tank = = random factor (ii.e. 8_C, 7.5_C, 8_A, etc.)
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
LMmod_221026 <- lm(ExcretionRate_umol_mL_hr_TDWbfactor ~ pCO2,data=F1_ER_MEANS_221026) # 
shapiro.test(resid(LMmod_221026)) # pass
leveneTest(LMmod_221026) # - pass
summary(LMmod_221026)





