# Purpose: Bay Scallop Project - O:N ratio calculation
# use the resp rate from LoLinr and from start-to-end (simplified) to estimate O:N - output master file

# Written by: Sam J Gurr (last edit 2/17/2022)

# LOAD PACKAGES ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
library(devtools) # devtools::install_github # use devtools to instlal github link
library(LoLinR) # install_github('colin-olito/LoLinR') # install LoLinR from github
library(dplyr)
library(lubridate)
library(rMR) 
library(dplyr)
library(stringr)
library(ggplot2)

# SET WORKING DIRECTORY :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis")


# LOAD DATA ::::::::::::::::::::::::::::::::::::::::::::::::::::
# Shannon advised that the start to end was better, but then reversed this and said the rates were best
# IIIIII will go with the rates...

# Start to end resp - review the start to end R script if you want more detail on this, okay!
RR_start.end  <- read.csv(file="Output/Respiration/RR_start_end_master.csv", header=T)  %>% dplyr::select(-X) 
RR_size.ref   <- read.csv(file="Data/Physiology/Respiration/metadata/Reference_resp_size.csv", header=T) 
# Respiration rate master - uses the LoLin R package, again review my days of  work to write a tutorial 
# and guide you how this was done.
SMR_F1        <- read.csv(file="Output/Respiration/F1/F1_RR_calc_master.csv", header=T)  %>% dplyr::select(-X) 
# note: this data file has Start.End_RR_mgO2hr - already accounting for the blank start end O2 consumption!
ER_F1         <- read.csv(file="Output/ExcretionRates/F1/F1_ExcretionRates_master.csv", header=T) %>% dplyr::select(-X) %>% 
                          dplyr::mutate(Age = case_when(Date == "20211026" ~  92,
                                                        Date == "20220202" ~  191,
                                                        Date == "20220301" ~  218,
                                                        Date == "20220922" ~  423,
                                                        Date == "20221026" ~  457)) # %>% #edit this!
# NOTE: we have TDW correction for ER because this is the gold stanard, we calculated a unique b factor 
# for both ER and SMR using data from ALL individuals meausred and corrected to the mean TDW, 
# we report the shell length b factor fhr SMR when reported alone BCUS it is represented by ALL datapoints, 
# condiering that were are many that did not have a TDW associated with the measurement. 
# FOR EVERY ER MEASUREMENT WE HAVE A SMR MEASUREMENT - WE ALSO HAVE TDW FOR ALL! 

# look here to see what i mean!
# how many datapoints do we have for ER
nrow(ER_F1) # 78 total

# now for SMr 
(nrow(SMR_F1)) # 136 - see what I mean! 
unique(ER_F1$Date) # 20211026 20220202 20220301 20220922 20221026

# we need to clean up SMR datasets because the Date format will not allow us to merge with ER
SMR_F1_sub <- SMR_F1 %>% dplyr::mutate(Date = paste("20",(format(as.Date(Date, "%m/%d/%Y"), "%y%m%d")), sep ='')) %>% # change format of the date to the format in Excretion_data
                         dplyr::filter(Date %in% c('20211026', '20220202', '20220301', '20220922', '20221026')) %>% 
                         dplyr::filter(filetype %in% 'LoLigo_data') %>% # ommits all microplate SDR dish data, in which ER MEASUREMENTS WERE NOT COMPLETED!!!!
                         dplyr::mutate(Replicate = gsub(".*_","",Chamber_tank)) %>% 
                         dplyr::select(c('Date', 'Age', 'pCO2','Replicate',
                                          'Number', 'Run','Length_mm',
                                          'resp_umol_hr')) %>% 
                         dplyr::mutate(pCO2 = case_when(pCO2 %in% '500 uatm' ~ '500 μatm',
                                                        pCO2 %in% '800 uatm' ~ '800 μatm'))
nrow(SMR_F1_sub) # 77 - we have one value less than ER due to a bad datapoint


ER_F1_sub <- ER_F1 %>% dplyr::select(c('Date', 'Age', 'pCO2','Replicate',
                                       'Number', 'Run','Length_mm',
                                       'ExcretionRate_umol_hr'))
# now when we merge, ER is our limiting factor 
F1_ON_Merge<- merge(ER_F1_sub, SMR_F1_sub, by=c('Date','Age', 'pCO2',  'Replicate','Number', 'Run'))
nrow(F1_ON_Merge) # 74 aligned - 4 less than ER master file 

# F1 Plots ::::::::::::::::::::::::::
library(forcats)
F1_ON_Master <- F1_ON_Merge %>% 
                  # IMPORTANT! out resp_mg_hr  is in O2 - we cover to resp_umol_hr by divideding by 32 (atomic weight of O2! 
                  # so technically our umol is the atomic equivelnts of O2, not O, to get this we mutliply by 2
                  dplyr::mutate(O_N = (resp_umol_hr)*2 / # for umol of oxygen, curretnly as O2, convert to O 
                                  ExcretionRate_umol_hr) %>% 
                  # dplyr::filter(!O_N >200) %>% # outlier omit
                  dplyr::mutate(pCO2 = factor(pCO2, levels=c("500 μatm", "800 μatm")))

# View(F1_ON_Master %>% dplyr::select(Date,pH, Replicate,resp_umol_hr, ExcretionRate_umol_hr, O_N))

write.csv(F1_ON_Master,
          "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/OxygenNitrogen_ratio/F1/F1_ON_master.csv")


F1_ON_MasterMEANS <- F1_ON_Master %>% # mean by tank replicate 
                        dplyr::filter(!O_N > 400) %>% 
                        dplyr::select(c(Date, Age, pCO2,  Replicate, O_N)) %>% # one extreme outlier value!
                        Rmisc::summarySE(measurevar="O_N", 
                                  groupvars=c("Date", "Age", "pCO2",  "Replicate"))

write.csv(F1_ON_MasterMEANS,
          "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/OxygenNitrogen_ratio/F1/F1_ON_master_means.csv")


F1_Boxplot <- F1_ON_MasterMEANS %>% 
                        #dplyr::filter(!(Date %in% '20220301')) %>% # two outliers?
                      ggplot(aes(x=as.factor(Age), 
                                 y=O_N, 
                                 color=as.factor(pCO2))) +
                          geom_boxplot(alpha = 0.5, # color hue
                                       width=0.6, # boxplot width
                                       outlier.size=0, # make outliers small
                                       position = position_dodge(preserve = "single")) + 
                          geom_point(pch = 19, 
                                     position = position_jitterdodge(0.01), 
                                     size=1) +
                          scale_color_manual(values=c("forestgreen","orange")) +
                          theme_classic() + 
                          ggtitle("O:N, F1 Scallops (rep av'd)") +
                          theme(legend.position="none",
                                axis.title.y=element_text(size=7),
                                axis.title.x=element_text(size=7),
                                axis.text.x=element_text(size=7)) +
                          scale_y_continuous(name ="O:N (raw umol)",expand = c(0, 0), limits = c(0, NA)) +
                          stat_summary(fun.y=mean, 
                                       geom = "errorbar", 
                                       aes(ymax = ..y.., ymin = ..y..), 
                                       width = 0.6, 
                                       size=0.4, 
                                       linetype = "dashed", 
                                       position = position_dodge(preserve = "single")) 
F1_Boxplot

F1_ON_MasterMEANSMEANS <- F1_ON_MasterMEANS %>% # mean by tank replicate 
                          dplyr::select(c(Age, pCO2, O_N)) %>% # one extreme outlier value!
                          Rmisc::summarySE(measurevar="O_N", 
                                    groupvars=c("Age", "pCO2"))

F1_ON_Plot <- F1_ON_MasterMEANSMEANS %>% 
                      ggplot(aes(x=as.factor(Age), 
                                 y=O_N, 
                                 color=as.factor(pCO2))) +
                      geom_point(position=position_dodge(.5))+ 
                      scale_color_manual(values=c("forestgreen",
                                                  "darkorange2"))+
                      geom_errorbar(aes(ymin=O_N-se, 
                                        ymax=O_N+se), width=.2,
                                    position=position_dodge(.5))+
                      theme_classic() +  
                      xlab("Age (dpf)") + 
                      ggtitle("O:N, F1 Scallops (mean +- SE; rep av'd)") +
                      theme_classic() +
                      theme(legend.position="none",
                            panel.grid.major = element_blank(), 
                            panel.grid.minor = element_blank())+ 
                      geom_line(stat = "identity", size=1.0)+
                      scale_y_continuous(name ="O:N (umol)",expand = c(0, 0), limits = c(0, NA)) +
                      theme(text = element_text(size=10))
# F1_ON_Plot

library(ggpubr)
ggarrange(F1_Boxplot, F1_ON_Plot, ncol = 1)

pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/OxygenNitrogen_ratio/F1/F1_ON_Mean_SE.pdf"), width = 4, height = 4)
print(F1_ON_Plot)
dev.off()



## statistics

# (1) First, run anova within date for all records (for looped!)
ANOVA_Dates       <- as.data.frame(unique(F1_ON_MasterMEANS$Date)) # call a list to loop in 
AOVdf_total       <- data.frame() # start dataframe, this will be the master output
DF_loop           <- data.frame(matrix(nrow = 1, ncol = 13)) # create dataframe to save during for loop
colnames(DF_loop) <- c('Date', 'Age', 'Metric', 'model', 'ShapiroWilk', 'ResidNorm', 'Levenes', 'HomogVar', 'DF.num' , 'DF.denom', 'F_val','P_val', 'SigDif') # names for comuns in the for loop

for (i in 1:nrow(ANOVA_Dates)) {
  
  date_loop     <- as.character(ANOVA_Dates[i,])
  data_loop     <- F1_ON_MasterMEANS %>% 
    dplyr::filter(Date == date_loop) %>% 
    dplyr::select(Date, Age, pCO2, O_N) %>% 
    na.omit()
  
  AOVmod              <- aov(lm(data_loop$O_N ~ data_loop$pCO2))
  DF_loop$Date        <- date_loop
  DF_loop$Age         <- data_loop$Age[1]
  DF_loop$Metric      <- 'RR; LENGTH b factor normalized'
  
  # run both modles
  AOVmod              <- aov(lm(data_loop$O_N ~ as.factor(data_loop$pCO2)))
  KWmod               <- kruskal.test(data_loop$O_N  ~ as.factor(data_loop$pCO2))
  
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
write.csv(AOVdf_total, "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/OxygenNitrogen_ratio/F1/F1_OtoN_ANOVA.csv")
#write.csv(Biodep_Master, "C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Biodeposition/Biodeposition_master.csv")






# T-tests

Ttest_Dates       <- as.data.frame(unique(F1_ON_MasterMEANS$Date)) # call a list to loop in 
Ttest_total       <- data.frame() # start dataframe, this will be the master output
DF_loop           <- data.frame(matrix(nrow = 1, ncol = 13)) # create dataframe to save during for loop
colnames(DF_loop) <- c('Date', 'Age', 'Metric', 'model', 
                       'ShapiroWilk', 'ResidNorm', 'Variance', 
                       'HomogVar', 'DF.num' , 'DF.denom', 'Tstat','P_val', 'SigDif') # names for comuns in the for loop

for (i in 1:nrow(Ttest_Dates)) {
  
  date_loop     <- as.character(Ttest_Dates[i,])
  data_loop     <- F1_ON_MasterMEANS %>% 
    dplyr::filter(Date == date_loop) %>% 
    dplyr::select(Date, Age, pCO2, O_N) %>% 
    na.omit()
  
  DF_loop$Date        <- date_loop
  DF_loop$Age         <- data_loop$Age[1]
  DF_loop$Metric      <- 'RR; LENGTH b factor normalized'
  
  # run assumptions 
  # normality of data 
  normality <- shapiro.test(data_loop$O_N)[[2]]
  # equal variance 
  variance <- var.test(data_loop$O_N~ 
                         as.numeric(as.factor(data_loop$pCO2)))[[3]]
  
  # run all modles
  Ttestmod.eqvar      <- t.test( data_loop$O_N ~ 
                                   (as.factor(data_loop$pCO2)),
                                 var.equal = TRUE)
  
  Ttestmod.noneqvar   <- t.test( data_loop$O_N ~ 
                                   (as.factor(data_loop$pCO2)),
                                 var.equal = FALSE)
  
  Wilcoxmod           <- wilcox.test(data_loop$O_N ~ 
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
    Wilcoxmod
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
# View(AOVdf_total) # view all the anova tests within data 

write.csv(Ttest_total, "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/OxygenNitrogen_ratio/F1/F1_OtoN_Ttest.csv")
