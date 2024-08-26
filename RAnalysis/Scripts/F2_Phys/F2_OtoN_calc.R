# Purpose: Bay Scallop Project - O:N ratio calculation
# use the resp rate from LoLinr and from start-to-end (simplified) to estimate O:N - output master file

# Written by: Sam J Gurr (last edit 4/19/2024)

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
SMR_F2        <- read.csv(file="Output/Respiration/F2/F2_RR_calc_master.csv", header=T)  %>% dplyr::select(-X) 
# note: this data file has Start.End_RR_mgO2hr - already accounting for the blank start end O2 consumption!
ER_F2         <- read.csv(file="Output/ExcretionRates/F2/F2_ExcretionRates_master.csv", header=T) %>% dplyr::select(-X) %>% 
                        dplyr::mutate(Age = case_when(Date == "20221116" ~  111,
                                                      Date == "20230131" ~  169,
                                                      Date == "20230223" ~  192,
                                                      Date == "20230327" ~  224))

# NOTE: we have TDW correction for ER because this is the gold stanard, we calculated a unique b factor 
# for both ER and SMR using data from ALL individuals meausred and corrected to the mean TDW, 
# we report the shell length b factor fhr SMR when reported alone BCUS it is represented by ALL datapoints, 
# condiering that were are many that did not have a TDW associated with the measurement. 
# FOR EVERY ER MEASUREMENT WE HAVE A SMR MEASUREMENT - WE ALSO HAVE TDW FOR ALL! 

# look here to see what i mean!
# how many datapoints do we have for ER
nrow(ER_F2) # 82 total
# now for SMr 
unique(ER_F2$Date) # 20221116 20230131 20230223 20230327
# we need to clean up SMR datasets because the Date format will not allow us to merge with ER
SMR_F2_sub <- SMR_F2 %>% dplyr::mutate(Date = paste("20",(format(as.Date(Date, "%m/%d/%Y"), "%y%m%d")), sep ='')) %>% # change format of the date to the format in Excretion_data
                          dplyr::filter(Date %in% c('20221116', '20230131', '20230223', '20230327')) %>% 
                          dplyr::mutate(Replicate = gsub(".*_","",Chamber_tank)) %>% 
                          dplyr::select(-(c(Dry_Tissue_weight, whole_Dry_weight))) %>%  # do not need it anymore!
                          dplyr::select(c('Date', 'Age', 'pCO2','Replicate',
                                          'Number', 'Run','Length_mm',
                                          'resp_umol_hr')) %>% 
                          dplyr::filter(!pCO2 %in% '1200 uatm') %>% 
                          dplyr::mutate(pCO2 = case_when(pCO2 %in% '500 uatm' ~ '500 μatm',
                                                         pCO2 %in% '800 uatm' ~ '800 μatm'))
nrow(SMR_F2_sub) # 56

ER_F2_sub <- ER_F2 %>% dplyr::select(c('Date', 'Age', 'pCO2','Replicate',
                                       'Number', 'Run','Length_mm',
                                       'ExcretionRate_umol_hr')) %>% 
                       dplyr::mutate(Age = case_when(
                         Age == 111 ~ 91,
                         Age == 169 ~ 167,
                         Age == 192 ~ 190,
                         Age == 224 ~ 222
                       )) %>% 
                       dplyr::filter(!pCO2 %in% '1200 μatm')

nrow(ER_F2_sub) # 55
# now when we merge, ER is our limiting factor 
F2_ON_Master <- merge(ER_F2_sub,SMR_F2_sub, by=c('Date','Age', 'pCO2', 'Replicate','Number', 'Length_mm')) 
nrow(F2_ON_Master) # 55 aligned - exact with the master file

# F2 Plots :::::::::::::::::::::::::

F2_ON_Master <- F2_ON_Master %>% 
                    dplyr::mutate(O_N =(resp_umol_hr)*2 / # for umol of oxygen, curretnly as O2, convert to O 
                                    ExcretionRate_umol_hr) 

write.csv(F2_ON_Master,
          "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/OxygenNitrogen_ratio/F2/F2_ON_master.csv")


F2_ON_MasterMEANS <- F2_ON_Master %>% # mean by tank replicate 
                      # dplyr::filter(!O_N>200) %>% 
                      dplyr::select(c(Date, Age, pCO2,  Replicate, O_N)) %>% # one extreme outlier value!
                      Rmisc::summarySE(measurevar="O_N", 
                                groupvars=c("Date", "Age", "pCO2",  "Replicate"))

write.csv(F2_ON_MasterMEANS,
          "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/OxygenNitrogen_ratio/F2/F2_ON_master_means.csv")


F2_Boxplot <- F2_ON_MasterMEANS %>% 
                  # dplyr::filter(!(Date %in% '20230131' & O_N >100)) %>% # two outliers?
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
                  scale_color_manual(values=c("forestgreen","orange", "purple")) +
                  theme_classic() + 
                  ggtitle("O:N, F2 Scallops (rep av'd)") +
                  theme(legend.position="none",
                        axis.title.y=element_text(size=7),
                        axis.title.x=element_text(size=7),
                        axis.text.x=element_text(size=7)) +
                  scale_y_continuous(name ="O:N (umol hr)",expand = c(0, 0), limits = c(0, NA)) +
                  stat_summary(fun.y=mean, 
                               geom = "errorbar", 
                               aes(ymax = ..y.., ymin = ..y..), 
                               width = 0.6, 
                               size=0.4, 
                               linetype = "dashed", 
                               position = position_dodge(preserve = "single")) 
F2_Boxplot

F2_ON_MasterMEANSMEANS <- F2_ON_MasterMEANS %>% # mean by tank replicate 
                            # dplyr::filter(!(Date %in% '20230131' & O_N >100)) %>% # two outliers?
                            dplyr::select(c(Date, Age, pCO2, O_N)) %>% # one extreme outlier value!
                            Rmisc::summarySE(measurevar="O_N", 
                                      groupvars=c("Date", "Age", "pCO2"))

F2_ON_Plot <- F2_ON_MasterMEANSMEANS %>% 
                            ggplot(aes(x=as.factor(Age), 
                                       y=O_N, 
                                       color=as.factor(pCO2))) +
                            geom_point(position=position_dodge(.5))+ 
                            scale_color_manual(values=c("forestgreen",
                                                        "darkorange2",
                                                        "purple"))+
                            geom_errorbar(aes(ymin=O_N-se, 
                                              ymax=O_N+se), width=.2,
                                          position=position_dodge(.5))+
                            theme_classic() +  
                            xlab("Age (dpf)") + 
                            ggtitle("O:N, F2 Scallops (umol hr)") +
                            theme_classic() +
                            theme(legend.position="none",
                                  panel.grid.major = element_blank(), 
                                  panel.grid.minor = element_blank())+ 
                            geom_line(stat = "identity", size=1.0)+
                            scale_y_continuous(name ="O:N (umol hr)",expand = c(0, 0), limits = c(0, NA)) +
                            theme(text = element_text(size=10))
F2_ON_Plot


ggarrange(F2_Boxplot, F2_ON_Plot, ncol = 1)

pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/OxygenNitrogen_ratio/F2/F2_ON_Mean_SE.pdf"), width = 4, height = 4)
print(F2_ON_Plot)
dev.off()



# T-tests

Ttest_Dates       <- as.data.frame(unique(F2_ON_MasterMEANS$Date)) # call a list to loop in 
Ttest_total       <- data.frame() # start dataframe, this will be the master output
DF_loop           <- data.frame(matrix(nrow = 1, ncol = 13)) # create dataframe to save during for loop
colnames(DF_loop) <- c('Date', 'Age', 'Metric', 'model', 
                       'ShapiroWilk', 'ResidNorm', 'Variance', 
                       'HomogVar', 'DF.num' , 'DF.denom', 'Tstat','P_val', 'SigDif') # names for comuns in the for loop

for (i in 1:nrow(Ttest_Dates)) {
  
  date_loop     <- as.character(Ttest_Dates[i,])
  data_loop     <- F2_ON_MasterMEANS %>% 
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

write.csv(Ttest_total, "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/OxygenNitrogen_ratio/F2/F2_OtoN_Ttest.csv")
