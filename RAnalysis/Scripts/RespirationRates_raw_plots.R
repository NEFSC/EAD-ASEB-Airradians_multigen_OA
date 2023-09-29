# Purpose: Bay Scallop Project - Respiration rate data 
# measure respiration rate from raw Loligo output data 
# using Lolin.R (Olito etal. 201?) for reproducible and non-bias calculation of respiration rates

# Written by: Sam J Gurr (last edit 11/16/2022)

# LOAD PACKAGES :::::::::::::::::::::::::::::::::::::::::::::::::::::::

library(devtools) # devtools::install_github # use devtools to instlal github link
library(LoLinR) # install_github('colin-olito/LoLinR') # install LoLinR from github
library(dplyr)
library(lubridate)
library(rMR) 
library(ggplot2)
library(stringr)
# SET WORKING DIRECTORY :::::::::::::::::::::::::::::::::::::::::::::::

setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis")
#setwd("C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis") # Work computer

# CHANGE THE FOLLOWING ..THEN CONTROL A + ENTER ::::::::::::::::::::::
path.p    <- "Data/Physiology/Respiration" #the location of all your respirometry files 
# ANALYSIS  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Objective: use LoLinr to run all respiration rates in a non-bias and autonomous fashion
# Ouputs: there will be two main resources from this script: 
#   (1) cumulative spreasheet of all respiration rate value for each Channel on each day
#   (2) a folder of plots from the LoLinR script to a plots folder - this will allow troubleshooting and sanity checks 

# I. call subfolders as dataframe and create a dataframe for the output

# call the subfolder names for the outside loop 'i' (i.e. 20210914)
folder.names           <- basename(list.files(path = path.p, pattern = "202", recursive = FALSE)) #list all csv file names in the folder and subfolders
folder.names.table     <- data.frame(folder.names)

# ABOUT folder.names
# 1      20210914 - contains 
# 2      20210930 - contains 
# 3      20211026 - contains 
# 4      20220202 - contains 
# 5      20220301 - contains 
# 6      20220420 - contains 
# 7      20220422 - contains 
# 8      20220824 - contains 
# 9      20220829 - contains F2 larvae trial runs with the SDR SensorDish (.csv files)
# 10     20220830 - contains F2 larvae and post-set  with the SDR SensorDish (.csv files) (note: cases with >1 animal per channel were pre-set and 1 animal per channel were post-set)
# 11     20220922 - contains F1 adults at ~14 months in age with LoLigo (.txt files) & F2 spat at ~2 months in age with the SDR SenorDish (.csv files)
# 12     20221026 - contains F1 adults at ~15 months in age with LoLigo (.txt files) - also measured biodeposition for these individuals!
# 13     20221116 - contains F2 juveniles at ~4 months old meausred with LoLigo (.txt files) 
# 14     20230131 - contains F2 adults meausred with LoLigo (.txt files) 
# 15     20230223 - contains F2 adults meausred with LoLigo (.txt files) 
# 16     20230316 - contains F2 D-hinge larvae during a full-reciprocal OA challenge -NOTE: this is a later F1 spawn that does NOT reflect eh parentage for the F1s, this is a side epxeriment for a osspring OA challenge, SDR dish only
# 17     20230327 - contains F2 adults meausred with LoLigo (.txt files) 
# 18     20230407 - contains F3 larvae during the full reciprocal OA challenge (parent x offpsring 3 pCO2s), SDR dish only
# 19     20230412 - contains F3 larvae during the full reciprocal OA challenge (parent x offpsring 3 pCO2s), SDR dish only
# 20     20230421 - contains F3 larvae during the full reciprocal OA challenge (parent x offpsring 3 pCO2s), SDR dish only
# 21     20230517 - contains F3 spat during grow out under ONLy matched parentxoffspring pCO2, SDR dish only


# Call the cumulative dataframe that we will write to in the for loop below
df_total             <- data.frame() # start dataframe 
resp.table           <- data.frame(matrix(nrow = 1, ncol = 7)) # create dataframe to save cumunalitively during for loop
colnames(resp.table) <- c('Date', 'Channel', 'Lpc', 'Leq' , 'Lz', 'alpha','Filename') # names for comuns in the for loop


# II. A bunch o' fors and if/elses - commented throughout!

# outside 'i' loop - call each subfolder one at a time for analysis
for(i in 21:nrow(folder.names.table)) { # for every subfolder 'i' ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # NOTE: when calling the raw files we need to accommodate the different formats
  # 20210914 used the 8-channel loligo system with raw output as .txt files with 'raw' in the title - call these using dplyr in the if/else below
  # 20210930 used the 24-channel SDR sensor dish with raw output as .csv files - call these in the if/else statement below 
  # call all txt files labeled 'raw' in each subfolder (i.e. 20210914) and create a table 
  if (folder.names.table[i,] %in% c('20210930','20220420', '20220422','20220824', '20220829', '20220830', '20230316', '20230407', '20230412', '20230421', '20230517')) { # call data when ONLY the 24-channel SDR dish data was used (csv file output) 
    file.names.table    <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',folder.names.table[i,1],sep=''), pattern = "csv$", recursive = TRUE)))) 
  } else if (folder.names.table[i,] %in% c('20211026', '20220922')) { # for day(s)s when BOTH the loligo system (txt files) AND SDR dish (csv files) were used
    file.names.table1    <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',folder.names.table[i,1],sep=''), pattern = "txt$", recursive = TRUE)))) %>%  dplyr::filter(grepl('raw', txt.files))#list all csv file names in the folder and subfolders
    file.names.table2    <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',folder.names.table[i,1],sep=''), pattern = "csv$", recursive = TRUE)))) #%>%  dplyr::filter(grepl('raw', txt.files))#list all csv file names in the folder and subfolders
    file.names.table     <- rbind(file.names.table1, file.names.table2)
  }  else { # all other data that used ONLY the  8-channel loligo system outputting .txt raw files (now 9/14/21,  2/2/22, 11/16/2022, 1/31/2023, 2/23/2023)
    file.names.table    <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',folder.names.table[i,1],sep=''), pattern = "txt$", recursive = TRUE))))  %>%  dplyr::filter(grepl('raw', txt.files))
  }
  
  # inside 'm' loop - call each  raw .txt or raw .csv file file witin the subfolder 'i'
      for(m in 1:nrow(file.names.table)) { # for every raw .txt or csv file 'm' in the subfolder 'i' :::::::::::::::::::::::::::::::::::::
        
            if (gsub(".*_raw.","", file.names.table[m,1]) == "txt") {
              Resp.Data           <- read.delim2(file = paste(path.p,'/',folder.names.table[i,1], '/', file.names.table[m,1], sep=''), header = TRUE,skip = 37, fileEncoding= "windows-1252") #reads in the data files
              
                # for data spanning 2021 to 2022
              if (str_split((Resp.Data$Date..Time..DD.MM.YYYY.HH.MM.SS.[1]), "/", simplify = TRUE)[[3]] == "2021") { # split by / delimiter and call the thirs string - this is the year - if it is 2021 proceed
                Resp.Data$date      <- paste((sub("2021.*", "", Resp.Data$Date..Time..DD.MM.YYYY.HH.MM.SS.)), '2021', sep='') #  date - use 'sub' to call everything before 2021, add back 2021 using paste
                Resp.Data$time_Sec  <- period_to_seconds(hms(substr((strptime(sub(".*2021/", "", Resp.Data$Date..Time..DD.MM.YYYY.HH.MM.SS.), "%I:%M:%S %p")) , 12,19))) # time - use 'sub' to call target time of the raw date time after 'year/' + strptime' convert to 24 hr clock + 'period_to_seconds' converts the hms to seconds  
              } else if (str_split((Resp.Data$Date..Time..DD.MM.YYYY.HH.MM.SS.[1]), "/", simplify = TRUE)[[3]] == "2022")  { # split by / delimiter and call the thirs string - this is the year - if it is 2022 proceed
                Resp.Data$date      <- paste((sub("2022.*", "", Resp.Data$Date..Time..DD.MM.YYYY.HH.MM.SS.)), '2022', sep='') #  date - use 'sub' to call everything before 2022, add back 2022 using paste
                Resp.Data$time_Sec  <- period_to_seconds(hms(substr((strptime(sub(".*2022/", "", Resp.Data$Date..Time..DD.MM.YYYY.HH.MM.SS.), "%I:%M:%S %p")) , 12,19))) # time - use 'sub' to call target time of the raw date time after 'year/' + strptime' convert to 24 hr clock + 'period_to_seconds' converts the hms to seconds  
              } else { # for all instances NOT 2021 or 2023 in the string split raw date - thus all 2023 data in this experiment 
                Resp.Data$date      <- paste((sub("2023.*", "", Resp.Data$Date..Time..DD.MM.YYYY.HH.MM.SS.)), '2023', sep='') #  date - use 'sub' to call everything before 2023, add back 2023 using paste
                Resp.Data$time_Sec  <- period_to_seconds(hms(substr((strptime(sub(".*2023/", "", Resp.Data$Date..Time..DD.MM.YYYY.HH.MM.SS.), "%I:%M:%S %p")) , 12,19))) # time - use 'sub' to call target time of the raw date time after 'year/' + strptime' convert to 24 hr clock + 'period_to_seconds' converts the hms to seconds  
              }
              
              # assign the remaining parameters
              Resp.Data$seconds   <- (Resp.Data$time_Sec - Resp.Data$time_Sec[1])    # secs - calc the sec time series
              Resp.Data$minutes   <- (Resp.Data$time_Sec - Resp.Data$time_Sec[1])/60 # mins - calc the minute time series
              temperature_C       <- as.numeric(Resp.Data$CH1.temp...C.[1])
              barromP_kPa         <- as.numeric(Resp.Data$Barometric.pressure..hPa.[1]) / 10
              salinity.pp.thou    <- as.numeric(Resp.Data$Salinity....[1])
              Resp.Data           <- Resp.Data %>% # use 'dplyr' 
                #dplyr::filter(!Phase %in% 'Flush') %>% # remove the initial rows labeled flush
                dplyr::select(c(date, seconds, minutes, contains(".O2...air.sat"))) # %>%  # call unique column names for the 8 Channels
              # dplyr::filter(minutes < 40)
              colnames(Resp.Data)[c(4:(ncol(Resp.Data)))] <- substr( ( colnames(Resp.Data)[c(4:(ncol(Resp.Data)))] ), 1,3) # clean these column names to make things easier - first 3 characters
            
              
              
              Resp.Data_15sec = Resp.Data[seq(1, nrow(Resp.Data), 15), ]
              
              } else { 
                
               Resp.Data           <- read.table(file = paste(path.p,'/',folder.names.table[i,1], '/', file.names.table[m,1], sep=''), sep = ',', header = TRUE,skip = 51) #reads in the data files
                
              # Resp.Data           <- read.table(file = paste(path.p,'/',folder.names.table[i,1], '/', file.names.table[m,1], sep=''), sep = ',', header = TRUE,skip = 47) #reads in the data files - IMPORTANT! skipping until row 47 works for later files on the F2 and F3 larvae
                
                # add the for loop here if/when the 24 channel Loligo (csv raw data outputs) is used in 2022!!!
                # for data in 2021 and data in 2022 
                if (str_split((Resp.Data$Date..DD.MM.YYYY.[1]), "/", simplify = TRUE)[[3]] == "2021") {
                  Resp.Data$date      <- paste((sub("2021.*", "", Resp.Data$Date..DD.MM.YYYY.)), '2021', sep='') #  date - use 'sub' to call everything before 2021, add back 2021 using paste
                  Resp.Data$time_Sec  <- period_to_seconds(hms(substr((strptime(sub(".*2021/", "", Resp.Data$Time..HH.MM.SS.), "%I:%M:%S %p")) , 12,19))) # time - use 'sub' to call target time of the raw date time after 'year/' + strptime' convert to 24 hr clock + 'period_to_seconds' converts the hms to seconds  
                } else if (str_split((Resp.Data$Date..DD.MM.YYYY.[1]), "/", simplify = TRUE)[[3]] == "2022") {
                  Resp.Data$date      <- paste((sub("2022.*", "", Resp.Data$Date..DD.MM.YYYY.)), '2022', sep='') #  date - use 'sub' to call everything before 2021, add back 2021 using paste
                  Resp.Data$time_Sec  <- period_to_seconds(hms(substr((strptime(sub(".*2022/", "", Resp.Data$Time..HH.MM.SS.), "%I:%M:%S %p")) , 12,19))) # time - use 'sub' to call target time of the raw date time after 'year/' + strptime' convert to 24 hr clock + 'period_to_seconds' converts the hms to seconds  
                } else { # for 2023 data
                  Resp.Data$date      <- paste((sub("2023.*", "", Resp.Data$Date..DD.MM.YYYY.)), '2023', sep='') #  date - use 'sub' to call everything before 2021, add back 2021 using paste
                  Resp.Data$time_Sec  <- period_to_seconds(hms(substr((strptime(sub(".*2023/", "", Resp.Data$Time..HH.MM.SS.), "%I:%M:%S %p")) , 12,19))) # time - use 'sub' to call target time of the raw date time after 'year/' + strptime' convert to 24 hr clock + 'period_to_seconds' converts the hms to seconds  
                }
                
                
                Resp.Data$seconds   <- (Resp.Data$time_Sec - Resp.Data$time_Sec[1])    # secs - calc the sec time series
                Resp.Data$minutes   <- (Resp.Data$time_Sec - Resp.Data$time_Sec[1])/60 # mins - calc the minute time series
                temperature_C       <- as.numeric(Resp.Data$CH1.temp...C.[1])
                barromP_kPa         <- as.numeric(Resp.Data$Barometric.pressure..hPa.[1]) / 10
                salinity.pp.thou    <- as.numeric(Resp.Data$Salinity....[1])
                Resp.Data           <- Resp.Data %>% # use 'dplyr' 
                  dplyr::select(c(date, seconds, minutes, contains("..Oxygen."))) # call unique column names for the 8 Channels
                colnames(Resp.Data)[c(4:(ncol(Resp.Data)))] <- substr( ( colnames(Resp.Data)[c(4:(ncol(Resp.Data)))] ), 1,2)   # clean these column names to make things easier - first 3 characters
                
                Resp.Data_15sec <- Resp.Data # .csvs are from the SDR (Presens) data - these data are already taken every 15 seconds - rename for clarity in the next statements
              }

        
            if (gsub(".*_raw.","", file.names.table[m,1]) == "txt") {
                    
              date.plot  <- folder.names.table[i,1]
              run.plot   <- gsub("_raw.*","", file.names.table[m,1])
              plot_title <- paste(date.plot, run.plot, sep = '_')
              
                    PLOT <- Resp.Data_15sec %>% 
                      dplyr::select(-c('date', 'seconds')) %>%  
                      reshape2::melt(id.vars = "minutes",variable.name = "channel", value.name = "air.sat") %>%
                      dplyr::filter(!air.sat  %in% 'NaN') %>% 
                      dplyr::mutate(mg.L.min =   (DO.unit.convert(as.numeric(air.sat),  # DO in percent air sat to be converted to mgL - uses an R package from loligo rMR
                                                     DO.units.in = "pct", DO.units.out ="mg/L", 
                                                     bar.units.in = "kPa", bar.press = barromP_kPa, bar.units.out = "kpa",
                                                     temp.C = temperature_C, 
                                                     salinity.units = "pp.thou", salinity = salinity.pp.thou))) %>% 
                      ggplot(aes(x = minutes , y = mg.L.min)) +
                      geom_smooth(method = "loess", se=FALSE, color="black", formula = mg.L.min ~ minutes) +
                      theme_classic() +
                      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", plot.title = element_text(size=10))+ 
                      labs(y = expression(RAW_mg~L^{-1}~O[2]%.%min^{-1})) +
                      xlab("minutes") +
                      geom_point() + 
                      ggtitle(plot_title) +
                      facet_wrap(~channel)
                    
                    #pdf(paste0("C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Respiration/plots_raw/",folder.names.table[i,1],"_", sub("_raw.*","",file.names.table[m,1]),"_regression.pdf"), width=10, height=12)
                    pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Respiration/plots_raw/",folder.names.table[i,1],"_", sub("_raw.*","",file.names.table[m,1]),"_regression.pdf"), width=10, height=12)
                    print(PLOT)
                    dev.off()
                    
                } else if (folder.names.table[i,] == '20210930') {
                    
                            date.plot  <- folder.names.table[i,1]
                            run.plot   <- substr( (sub(".*M_","",file.names.table[m,1])), 1,13)
                            plot_title <- paste(date.plot, run.plot, sep = '_')
                            PLOT <- Resp.Data_15sec %>% 
                              dplyr::select(-c('date', 'seconds')) %>%  
                              reshape2::melt(id.vars = "minutes",variable.name = "channel", value.name = "mg.L.min") %>%
                              ggplot(aes(x = minutes , y = mg.L.min)) +
                              geom_smooth(method = "loess", se=FALSE, color="black", formula = mg.L.min ~ minutes) +
                              theme_classic() +
                              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", plot.title = element_text(size=10))+ 
                              labs(y = expression(RAW_mg~L^{-1}~O[2]%.%min^{-1})) +
                              xlab("minutes") +
                              geom_point() + 
                              ggtitle(plot_title) +
                              facet_wrap(~channel)
                            
                            #pdf(paste0("C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Respiration/plots_raw/",folder.names.table[i,1],"_", substr( (sub(".*M_","",file.names.table[m,1])), 1,13),"_regression.pdf"), width=10, height=12)
                            pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Respiration/plots_raw/",folder.names.table[i,1],"_", substr( (sub(".*M_","",file.names.table[m,1])), 1,13),"_regression.pdf"), width=10, height=12)
                            print(PLOT)
                            dev.off()
                        
                           
                      } else { # just for the SDR run on 20211025 .csv file 
                        date.plot  <- folder.names.table[i,1]
                        run.plot   <- substr((sub(".*resp_","",file.names.table[m,1])), 1, 6)
                        plot_title <- paste(date.plot, run.plot, sep = '_')
                        PLOT <- Resp.Data_15sec %>% 
                          dplyr::select(-c('date', 'seconds')) %>%  
                          reshape2::melt(id.vars = "minutes",variable.name = "channel", value.name = "mg.L.min") %>% 
                          ggplot(aes(x = minutes , y = mg.L.min)) +
                          geom_smooth(method = "loess", se=FALSE, color="black", formula = mg.L.min ~ minutes) +
                          theme_classic() +
                          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", plot.title = element_text(size=10))+ 
                          labs(y = expression(RAW_mg~L^{-1}~O[2]%.%min^{-1})) +
                          xlab("minutes") +
                          geom_point() + 
                          ggtitle(plot_title) +
                          facet_wrap(~channel)  
                        
                        #pdf(paste0("C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Respiration/plots_raw/",folder.names.table[i,1],"_", substr((sub(".*resp_","",file.names.table[m,1])), 1, 5),"_regression.pdf"), width=10, height=12) # 20211026_resp_unfed.csv ONLY
                        pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Respiration/plots_raw/",folder.names.table[i,1],"_", substr((sub(".*resp_","",file.names.table[m,1])), 1, 6),"_regression.pdf"), width=10, height=12) # 20211026_resp_unfed.csv ONLY
                        print(PLOT)
                        dev.off()  }
         
      }
}

        
    