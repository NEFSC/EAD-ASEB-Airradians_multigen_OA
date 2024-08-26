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
library(Rmisc)
# SET WORKING DIRECTORY 
setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis") # personal computer

# LOAD DATA
F2_ER_master <- read.csv(file="C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/F2/F2_ExcretionRates_master.csv", header=T)
unique(F2_ER_master$Date) # 20230131 20230223 20230327 - all three dataes - yay! corrected for b factor 1.07
# NOTE: view the bfactor norm R script, B factro using TDW for all data in this mutligen study!

F2_ER_master <- F2_ER_master %>% 
                    dplyr::mutate(Age = case_when(Date == "20221116" ~  111,
                                                  Date == "20230131" ~  169,
                                                  Date == "20230223" ~  192,
                                                  Date == "20230327" ~  224)) %>%  # %>% #edit this!
                    dplyr::mutate(pCO2 = factor(pCO2, levels = c('500 μatm','800 μatm','1200 μatm')))

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ANALYSIS AND PLOTTING  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# PLOTS

# now take the means by Date pCO2  and tank and remove the tank group - means and st error by date and pCO2
F2_ER_MEANS <- F2_ER_master %>% 
                  summarySE(measurevar="ExcretionRate_umol_hr_bFactorNormLength.MEAN", 
                            groupvars=c("Date", "Age", "pCO2", "Replicate"))

F2_ER_MEANS_plotting <- F2_ER_MEANS %>% 
                          dplyr::filter(!ExcretionRate_umol_hr_bFactorNormLength.MEAN >3.5) %>% # one extreme outlier value!
                          summarySE(measurevar="ExcretionRate_umol_hr_bFactorNormLength.MEAN", 
                                    groupvars=c("Date", "Age", "pCO2"))


F2_ER_plot <- F2_ER_MEANS_plotting %>% 
                    ggplot(aes(x=as.factor(Age), 
                               y=ExcretionRate_umol_hr_bFactorNormLength.MEAN, 
                               color=as.factor(pCO2))) +
                    geom_point(position=position_dodge(.5))+ 
                    scale_color_manual(values=c("forestgreen",
                                                "darkorange2",
                                                "purple"))+
                    geom_errorbar(aes(ymin=ExcretionRate_umol_hr_bFactorNormLength.MEAN-se, 
                                      ymax=ExcretionRate_umol_hr_bFactorNormLength.MEAN+se), width=.2,
                                  position=position_dodge(.5))+
                    theme_classic() +  
                    xlab("Age (dpf)") + 
                    ggtitle("F2 Scallops: Excretion rate TDW b factor norm (umol NH4 hr)") +
                    theme_classic() +
                    theme(legend.position="none",
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank())+ 
                    geom_line(stat = "identity", size=1.0)+
                    scale_y_continuous(name ="ER",expand = c(0, 0), limits = c(0, NA)) +
                    theme(text = element_text(size=10))


# output the plot 
pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/F2/F2_ER_Lengthbfactor.pdf"), width = 4, height = 4)
print(F2_ER_plot)
dev.off()





F2_ER_MEANS_LvM_plotting <- F2_ER_MEANS %>% 
                              dplyr::filter(!pCO2 %in% '1200 μatm') %>% 
                              summarySE(measurevar="ExcretionRate_umol_hr_bFactorNormLength.MEAN", 
                                        groupvars=c("Date", "Age", "pCO2"))


F2_ER_plot_LvM <- F2_ER_MEANS_plotting_LvM %>% 
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
                          ggtitle("F2 Scallops: Excretion rate Length b factor norm (umol NH4 hr)") +
                          theme_classic() +
                          theme(legend.position="none",
                                panel.grid.major = element_blank(), 
                                panel.grid.minor = element_blank())+ 
                          geom_line(stat = "identity", size=1.0)+
                          scale_y_continuous(name ="ER",expand = c(0, 0), limits = c(0, NA)) +
                          theme(text = element_text(size=10))


# output the plot 
pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/F2/F2_ER_Lengthbfactor_LvM.pdf"), width = 4, height = 4)
print(F2_ER_plot_LvM)
dev.off()




# Stats
# (1) First, run anova within date for all records (for looped!)
ANOVA_Dates       <- as.data.frame(unique(F2_ER_MEANS$Date)) # call a list to loop in 
AOVdf_total       <- data.frame() # start dataframe, this will be the master output
DF_loop           <- data.frame(matrix(nrow = 1, ncol = 13)) # create dataframe to save during for loop
colnames(DF_loop) <- c('Date', 'Age', 'Metric', 'model', 'ShapiroWilk', 'ResidNorm', 'Levenes', 'HomogVar', 'DF.num' , 'DF.denom', 'F_val','P_val', 'SigDif') # names for comuns in the for loop

for (i in 1:nrow(ANOVA_Dates)) {
  
  date_loop     <- as.character(ANOVA_Dates[i,])
  data_loop     <- F2_ER_MEANS %>% 
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
write.csv(AOVdf_total, "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/F2/F2_ER_TDW_Statistics.csv")
#write.csv(Biodep_Master, "C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/Biodeposition_master.csv")



# (2) t.test 
Ttest_Dates       <- as.data.frame(unique(F2_ER_MEANS$Date)) # call a list to loop in 
Ttest_total       <- data.frame() # start dataframe, this will be the master output
DF_loop           <- data.frame(matrix(nrow = 1, ncol = 13)) # create dataframe to save during for loop
colnames(DF_loop) <- c('Date', 'Age', 'Metric', 'model', 
                       'ShapiroWilk', 'ResidNorm', 'Variance', 
                       'HomogVar', 'DF.num' , 'DF.denom', 'Tstat','P_val', 'SigDif') # names for comuns in the for loop
library(purrr)
library(rstatix)
for (i in 1:nrow(Ttest_Dates)) {
  
  date_loop     <- as.character(Ttest_Dates[i,])
  data_loop     <- F2_ER_MEANS %>% 
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

write.csv(Ttest_total, "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/ExcretionRates/F2/F2_ER_LengthbFactor_Ttest.csv")







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








