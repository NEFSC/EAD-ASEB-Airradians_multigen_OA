# Purpose: Bay Scallop Project - Respiration rate data 
# measure respiration rate from raw Loligo output data 
# using Lolin.R (Olito etal. 201?) for reproducible and non-bias calculation of respiration rates

# Written by: Sam J Gurr (last edit 5/24/2023)

# LOAD PACKAGES :::::::::::::::::::::::::::::::::::::::::::::::::::::::

library(devtools) # devtools::install_github # use devtools to instlal github link
library(LoLinR) # install_github('colin-olito/LoLinR') # install LoLinR from github
library(dplyr)
library(lubridate)
library(rMR) 
library(stringr)

# SET WORKING DIRECTORY :::::::::::::::::::::::::::::::::::::::::::::::

setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis")
#("C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis") # Work computer

# CHANGE THE FOLLOWING ..THEN CONTROL A + ENTER ::::::::::::::::::::::

path.p    <- "Data/Physiology/Respiration" #the location of all your respirometry files 
a         <- 0.4
ouputNAME <- "Output/Respiration/RR_LoLin_raw.csv" 

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
# 21     20230518 - contains F3 spat during grow out under ONLy matched parentxoffspring pCO2, SDR dish only

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
  
  if (folder.names.table[i,] %in% c('20210930','20220420', '20220422','20220824', '20220829', '20220830', '20230316', '20230407', '20230412', '20230421', '20230518')) { # call data when ONLY the 24-channel SDR dish data was used (csv file output) 
    file.names.table    <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',folder.names.table[i,1],sep=''), pattern = "csv$", recursive = TRUE)))) 
   } else if (folder.names.table[i,] %in% c('20211026', '20220922', '20221026')) { # for day(s)s when BOTH the loligo system (txt files) AND SDR dish (csv files) were used
     file.names.table1    <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',folder.names.table[i,1],sep=''), pattern = "txt$", recursive = TRUE)))) %>%  dplyr::filter(grepl('raw', txt.files))#list all csv file names in the folder and subfolders
     file.names.table2    <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',folder.names.table[i,1],sep=''), pattern = "csv$", recursive = TRUE)))) #%>%  dplyr::filter(grepl('raw', txt.files))#list all csv file names in the folder and subfolders
     file.names.table     <- rbind(file.names.table1, file.names.table2)
    }  else { # all other data that used ONLY the  8-channel loligo system outputting .txt raw files (now 9/14/21,  2/2/22, 11/16/2022, 1/31/2023, 2/23/2023)
        file.names.table    <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',folder.names.table[i,1],sep=''), pattern = "txt$", recursive = TRUE)))) %>%  dplyr::filter(grepl('raw', txt.files))#list all csv file names in the folder and subfolders
    }  

  
  # inside 'm' loop - call each  raw .txt or raw .csv file file witin the subfolder 'i'
  for(m in 1:nrow(file.names.table)) { # for every raw .txt or csv file 'm' in the subfolder 'i' :::::::::::::::::::::::::::::::::::::
      
    if (gsub(".*_raw.","", file.names.table[m,]) == "txt") { # call .txt 8-channel LoLigo data 
        Resp.Data <- read.delim2(file = paste(path.p,'/',folder.names.table[i,1], '/', file.names.table[m,1], sep=''), header = TRUE,skip = 37, fileEncoding= "windows-1252") #reads in the data files
          
        # the caveat of a multigenerational dataset - below is a contingency statment for the date to convert the timestamp to seconds 
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
          
      Resp.Data$seconds   <- (Resp.Data$time_Sec - Resp.Data$time_Sec[1])    # secs - calc the sec time series
      Resp.Data$minutes   <- (Resp.Data$time_Sec - Resp.Data$time_Sec[1])/60 # mins - calc the minute time series
      temperature_C       <- as.numeric(Resp.Data$CH1.temp...C.[1])
      barromP_kPa         <- as.numeric(Resp.Data$Barometric.pressure..hPa.[1]) / 10
      salinity.pp.thou    <- as.numeric(Resp.Data$Salinity....[1])
      Resp.Data           <- Resp.Data %>% # use 'dplyr' 
                              dplyr::select(c(date, seconds, minutes, contains(".O2...air.sat"))) # %>%  # call unique column names for the 8 Channels
      # rename columns 
      colnames(Resp.Data)[c(4:(ncol(Resp.Data)))] <- substr( ( colnames(Resp.Data)[c(4:(ncol(Resp.Data)))] ), 1,3) # clean these column names to make things easier - first 3 characters
      # Truncate! EVERY 15 SECONDS (note: these txt files are long with measurements every second,trancating reduces the analysis time dramatically)
      # the loligo recorded values every second, this slows the model dramatically with >2000 values for each Channel, call every 15 seconds to speed this up
      Resp.Data_15sec = Resp.Data[seq(1, nrow(Resp.Data), 15), ] # truncate every 15 seconds
      
     
      } else { # call .csv Presens SensorDish data 
       Resp.Data           <- read.csv(file = paste(path.p,'/',folder.names.table[i,1], '/', file.names.table[m,1], sep=''), header = TRUE,skip = 51) #reads in the data files
              
              # reformat data in 2021 and data in 2022
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
                                dplyr::select(c(date, seconds, minutes, contains("..Oxygen."))) # call unique column names for the 24 channels
      colnames(Resp.Data)[c(4:(ncol(Resp.Data)))] <- substr( ( colnames(Resp.Data)[c(4:(ncol(Resp.Data)))] ), 1,2) 
      
      Resp.Data_15sec     <- Resp.Data # No need to truncate! The SDR SensorDish data is already recoreded every 15 seconds. simply rename for consistancy with the .txt LoLigo call
            
     } # clean these column names to make things easier - first 3 characters
            
    
    
    
    
    
    if (folder.names.table[i,] == '20210930'){
      Resp.Data_15sec = Resp.Data_15sec  %>%  dplyr::filter(minutes < 40) # SDR data is already taken every 15 seconds, truncate for < 40 minutes in runs as the records start to show noise and undesirable data for O2 consumption (ran whole record and observed ALL Lolin plots to make this decision) 
     } else if (folder.names.table[i,] == '20220830' & substr(file.names.table[m,], 5,7) == '799'){
       Resp.Data_15sec = Resp.Data_15sec  %>%  dplyr::filter(minutes >25 & minutes < 120) # plate 1 SDR 799 for 8/30/22 data, ran for ~200 minutes with a dropoff below 5-6 mgL after 120-150 minutes (omit due to hypocia) and noise as the start ( omit before 25 minutes)
     } else if (folder.names.table[i,] == '20220830' & substr(file.names.table[m,], 5,7) == '873'){
       Resp.Data_15sec = Resp.Data_15sec %>%  dplyr::filter(minutes >15)  # 60 minute file and all data is very stable, a bit of noise initialy and omitted here
     } else if (folder.names.table[i,] == '20220922' & (gsub(".*_raw.","", file.names.table[m,]) == "txt")) { # call all runs with the LoLigo system on 20220922
       Resp.Data_15sec = Resp.Data_15sec %>%  dplyr::filter(minutes > 20 & minutes < 50) # call between miutes 20 and 50 of the trials - took time to start in order to close chambers and channels stopped once below defined threshold (80 % a.s.)
     } else if (folder.names.table[i,] == '20220922' & (gsub(".*\\.","", file.names.table[m,]) == "csv")) { # call all runs with the SDR SensorDish system on 20220922 (.csv files)
       Resp.Data_15sec = Resp.Data_15sec %>%  dplyr::filter(minutes > 10 & minutes < 50) # call data after 10 minutes
     } else if (folder.names.table[i,] == '20221026') { 
       Resp.Data_15sec = Resp.Data_15sec %>%  dplyr::filter(minutes > 10 & minutes < 60) # call data after 10 minutes
     } else if (folder.names.table[i,] == '20221116') { 
       Resp.Data_15sec = Resp.Data_15sec %>%  dplyr::filter(minutes < 75) # call data before minute 75 (when most reached 80% a.s. and finished), also the starting data was very clean in these runs!
     } else if (folder.names.table[i,] == '20230131') { 
       Resp.Data_15sec = Resp.Data_15sec %>%  dplyr::filter(minutes < 30) # call data before minute 30 minutes (when most reached 80% a.s. and finished)
     } else if (folder.names.table[i,] == '20230223') { 
       Resp.Data_15sec = Resp.Data_15sec %>%  dplyr::filter(minutes < 40) # call data before minute 30 minutes (when most reached 80% a.s. and finished)
     } else if (folder.names.table[i,] == '20230316') { 
       Resp.Data_15sec = Resp.Data_15sec[seq(1, nrow(Resp.Data_15sec), 4), ] %>% dplyr::filter(minutes > 60 & minutes < 160) # calls data every minute to reduce load time - here we truncate where we have a linear trend and does not dip below hypoxia
     } else if (folder.names.table[i,] == '20230327') { 
       Resp.Data_15sec = Resp.Data_15sec %>% dplyr::filter(minutes > 15 & minutes < 60)  # calls data < 60 minutes to truncate (partially) befire we reoxugenated the system for certain channels
     } else if (folder.names.table[i,] == '20230407') { 
       Resp.Data_15sec = Resp.Data_15sec[seq(1, nrow(Resp.Data_15sec), 4), ] %>% dplyr::filter(minutes > 20 & minutes < 120) # calls data every minute to reduce load time - here we truncate where we have a linear trend and does not dip below hypoxia
     } else if (folder.names.table[i,] == '20230412') { 
       Resp.Data_15sec = Resp.Data_15sec %>% dplyr::filter(minutes < 30)  # note the log for this data started late after the plates wer loaded (1 hour later) - call 30 minutes of the very linear record to get data at highest DO
     } else if (folder.names.table[i,] == '20230421') { 
       Resp.Data_15sec = Resp.Data_15sec[seq(1, nrow(Resp.Data_15sec), 4), ] %>% dplyr::filter(minutes > 20 & minutes < 150) # calls data every minute to reduce load time - here we truncate where we have a linear trend and does not dip below hypoxia
     } else if (folder.names.table[i,] == '20230518') { 
       Resp.Data_15sec = Resp.Data_15sec[seq(1, nrow(Resp.Data_15sec), 4), ] %>% dplyr::filter(minutes > 15 & minutes < 40) # calls data every minute to reduce load time - here we truncate where we have a linear trend and does not dip below hypoxia
     } else { # note this should only call the txt files in 20211026 as there are no .csv files in 20210914
          # Resp.Data_15sec = Resp.Data %>%  dplyr::filter(minutes > 30 & minutes < 90)# for now we will run the whole dataset to see...
          Resp.Data_15sec = Resp.Data_15sec %>%  dplyr::filter(minutes > 30 & minutes < 90)# for now we will run the whole dataset to see...
          Resp.Data_15sec = Resp.Data_15sec %>%  dplyr::filter(minutes > 60)# 20200829 larve data, omit the linital and target the remaining 
          Resp.Data_15sec = Resp.Data_15sec %>%  dplyr::filter(minutes > 60)# 20200829 larve data, omit the linital and target the remaining 
        }
      # clean these column names to make things easier - first 3 characters
            
          
  
              # inside 'j' loop - for each 'raw' txt file 'm', call each O2 sensor/resp chamber 'j' for analysis
              for(j in 4:(ncol(Resp.Data_15sec))){ # for each sensor column 'j' (..starting at column 4) :::::::::::::::::::::::::::::::
              
                Resp_loop    <- (Resp.Data_15sec[,c(3,j)]) %>% 
                                              dplyr::filter(!str_detect(((Resp.Data_15sec[,c(3,j)])[,2]),"NaN")) %>%  # noticed some random rows have 'NaN' - so I will loop the min and Channels to omit Nas before proceeding
                                              dplyr::mutate(minutes = as.numeric(minutes)) #  %>% # convert minutes to numeric
                                             # dplyr::filter(minutes > max(minutes) -60) # call the ___ minutes before the end of the trial (avoid the first data points noisy and due to handling stress no resp rate)


                # Loligo system needs to cnvert %air sat to mg / L whereas SDR dish does not 
                if ( (substr(colnames(Resp.Data_15sec)[j],1,2) == 'CH') ) { # loligo measurements need to be converted to mg/L from %air sat - these columns are written as "CH#" 
                  Resp_loop <- Resp_loop %>%  dplyr::filter(!colnames(Resp_loop)[2] %in% 'NaN') # Lolin recorede NAs are written as 'Nan' - wonts run unless removed!
                  Resp_loop$mgL     <- DO.unit.convert(as.numeric(Resp_loop[,2]),  # DO in percent air sat to be converted to mgL - uses an R package from loligo rMR
                                                     DO.units.in = "pct", DO.units.out ="mg/L", 
                                                     bar.units.in = "kPa", bar.press = barromP_kPa, bar.units.out = "kpa",
                                                     temp.C = temperature_C, 
                                                     salinity.units = "pp.thou", salinity = salinity.pp.thou)
                } else { Resp_loop$mgL <- na.omit(Resp.Data_15sec[j])
                         Resp_loop$mgL <- as.numeric(unlist(Resp_loop$mgL)) # need to unlist and call as numeric to run LoLinR
                         Resp_loop$minutes <- as.numeric(unlist(Resp_loop$minutes)) # need to unlist and call as numeric to run LoLinR
                         Resp_loop %>% dplyr::filter(mgL > (0.8 * max(na.omit(Resp.Data %>% select(colnames(Resp.Data_15sec[j]))))) ) # grab all data over 80% air saturation for the particular column (pre-filtered by time as 'Resp.Data')

                        } # for the SDR dish values that are already in mg/L simply call the column in the loop
                    
                    # now run data!
                     if (nrow(Resp_loop) < 1) { # if column 'j' is NA write NA in the cumulative sheet...
                        resp.table$Date                <- Resp.Data_15sec[1,1]
                        resp.table$Channel             <- colnames(Resp_loop)[2] 
                        resp.table[3:ncol(resp.table)] <- 'NA'
                        df       <- data.frame(resp.table) # name dataframe for this single row
                        df_total <- rbind(df_total,df) #bind to a cumulative list dataframe
                        print(df_total) # print to monitor progress
                        
                        } else { # else run LoLinR for x=mins and y=mg/l O2
                          
                            model <- rankLocReg(
                            xall    = as.numeric(Resp_loop[, 1]), 
                            yall    = as.numeric(Resp_loop[, 3]), # call x as the minute timeseries and y as the mg L-1 O2 
                            alpha   = a,  # alpha was assigned earlier as 0.4 by the authors default suggestions - review Olito et al. and their github page for details
                            method  = "pc", 
                            verbose = TRUE) 
                        
                            sum.table <- summary(model)
                            
                            resp.table$Date       <- Resp.Data_15sec[1,1]
                            resp.table$Channel    <- colnames(Resp_loop)[2] 
                            resp.table$Lpc        <- sum.table$Lcompare[3,6] # Lpc slope - call the column 'b1'
                            resp.table$Leq        <- sum.table$Lcompare[2,6] # Leq slope - call the column 'b1'
                            resp.table$Lz         <- sum.table$Lcompare[1,6] # Lz slope  - call the column 'b1'
                            resp.table$alpha      <- a
                            resp.table$Filename   <- file.names.table[m,1]
                            
                            df       <- data.frame(resp.table) # name dataframe for this single row
                            df_total <- rbind(df_total,df) #bind to a cumulative list dataframe
                            print(df_total) # print to monitor progress
                            }  # end of  else statement (if column 'j' is NA write NA in the cumulative sheet, else run LoLinR for x=mins and y = mg/l O2)
                            # save plots every inside loop and name by date_run_vialposition
                          if (gsub(".*_raw.","", file.names.table[m,1]) == "txt") {
                            pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Respiration/plots_alpha0.4_increm15sec/",folder.names.table[i,1],"_", sub("_raw.*","",file.names.table[m,1]),"_",colnames(Resp_loop)[2],"_regression.pdf"))
                            #pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Respiration/plots_alpha0.4_increm15sec/",folder.names.table[i,1],"_", sub("_raw.*","",file.names.table[m,1]),"_",colnames(Resp_loop)[2],"_regression.pdf"))
                            plot(model)
                            dev.off()
                            } else if (folder.names.table[i,] == '20210930') {
                              #pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_OA/RAnalysis/Output/Respiration/plots_alpha0.4_increm15sec/",folder.names.table[i,1],"_", substr( (sub(".*M_","",file.names.table[m,1])), 1,13),"_",colnames(Resp_loop)[2],"_regression.pdf"))
                              pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Respiration/plots_alpha0.4_increm15sec/",folder.names.table[i,1],"_", substr( (sub(".*M_","",file.names.table[m,1])), 1,13),"_",colnames(Resp_loop)[2],"_regression.pdf"))
                              plot(model)
                              dev.off() } else { # just for the SDR run on 20211025 .csv file 
                                pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Respiration/plots_alpha0.4_increm15sec/",folder.names.table[i,1],"_", substr((sub(".*resp_","",file.names.table[m,1])), 1, 6),"_",colnames(Resp_loop)[2],"_regression.pdf")) # 20211026_resp_unfed.csv ONLY
                                #pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Respiration/plots_alpha0.4_increm15sec/",folder.names.table[i,1],"_", substr((sub(".*resp_","",file.names.table[m,1])), 1, 6),"_",colnames(Resp_loop)[2],"_regression.pdf")) # 20211026_resp_unfed.csv ONLY
                                plot(model)
                                dev.off()
                                }
         } # end of inside for loop 'j' (for each sensor column 'j' [a] isolate mins and CH_ for analysis [b] convert CH_ data to mg/L using 'DO.unit.convert' [c] calc respi rates with LoLin R)
    
     } # end of inside  for loop 'm' (for every 'raw' .txt file 'm' in the subfolder 'i')
  
} # end of outside for loop 'i' (for every subfolder 'i')

# merge with the preexisiting table
# NOTE: raw values here are...
# (1) in units of mg/L min-1 (slope of line mg/l / mins in LoLinR - check the plot outputs to see)
# (2) not normalized for volume chamber 
# (3) not normalized for blank resp rate 
# (4) not normalized for a size/individual metric (i.e. Tissue Dry weight, shell length, etc.)

cumulative_resp_table <- read.csv(file=ouputNAME, header=TRUE) #call the pre existing cumulative table
new_table             <- rbind(cumulative_resp_table, df_total) # bind the new table from the for loop to the pre exisiting table
if (nrow(new_table) == nrow(cumulative_resp_table) + nrow(df_total)) {
  write.table(new_table,ouputNAME,sep=",", row.names=FALSE)  # write out to the path names outputNAME
} else {print("Did not overwrite!")}

# View(new_table) # view if you like!

# AFTER VISUAL INSPECTION OF PLOTS....
# we have the followoing rates that need to be rerun... (check the diagnostic plots and see for yourself!)

# (1) 20210930_Plate_2_Run_2_C5 https://github.com/SamGurr/Airradians_OA/blob/master/RAnalysis/Output/Respiration/plots_alpha0.4_increm15sec/20210930_Plate_2_Run_2_C5_regression.pdf
# - solution = call 'Lz' instead of the default Leq

# (2) 20210930_Plate_2_Run_1_C1 https://github.com/SamGurr/Airradians_OA/blob/master/RAnalysis/Output/Respiration/plots_alpha0.4_increm15sec/20210930_Plate_2_Run_1_C1_regression.pdf
# - solution = call 'Lz' instead of the default Leq
 
# (3) 20210930_Plate_1_Run_2_C1 https://github.com/SamGurr/Airradians_OA/blob/master/RAnalysis/Output/Respiration/plots_alpha0.4_increm15sec/20210930_Plate_1_Run_2_C1_regression.pdf
# - solution = we reran this at the end of the LoLin script  for 0-20 minutes and got an Lpc  -0.0296, insert this 

# (4) 20220202_Run_1_CH1 https://github.com/SamGurr/Airradians_multigen_OA/blob/main/RAnalysis/Output/Respiration/plots_alpha0.4_increm15sec/20220202_Run_1_CH1_regression.pdf
# - solution = call data before minute 70, raw data shows a jump of NAs when the sensor was repositioned (likely lost signal and was adjusted) this is artifacually calculating a higher rate (view the pdf above)

# (5) 20220202_run_2_CH5 https://github.com/SamGurr/Airradians_multigen_OA/blob/main/RAnalysis/Output/Respiration/plots_alpha0.4_increm15sec/20220202_run_2_CH5_regression.pdf
# - solution = call data after minute 80 when the O2 consumption dramatically increases, the original value was both low and extracted from initial data (post handling), the proposed later rate is more representative
# - NOTE: I ran from minute 80 to 120 and found that the rate increased between 80 - 90 minute mark but evened out at very similar rate to that called < minute 70 (0.0049 and 0.0051) thus, keep the original value for this time point (actually low!)

# (6) 20220202_run3_CH2 https://github.com/SamGurr/Airradians_multigen_OA/blob/main/RAnalysis/Output/Respiration/plots_alpha0.4_increm15sec/20220202_run3_CH2_regression.pdf
# - solution = call data after minute 60, the curent call (pdf above) exctracted the initial values, showing a ow rate unrepresentative of the full timeseries

# load the data to run it 

# is the data a csv file? (from LoLin 24-channel SDR dish )
resp_rerun          <- read.csv(file = "Data/Respiration/20220202/run_1_raw.csv", header = TRUE,skip = 51) #%>% 
                        dplyr::select(c("Relative.time..HH.MM.SS.", "C1..Oxygen.")) %>%  #reads in the data files
                        dplyr::mutate(mgL =  C1..Oxygen.) 
resp_rerun$date      <- paste((sub("2021.*", "", resp_rerun$Date..DD.MM.YYYY.)), '2021', sep='') #  date - use 'sub' to call everything before 2021, add back 2021 using paste
resp_rerun$time_Sec  <- period_to_seconds(hms(substr((strptime(sub(".*2021/", "", resp_rerun$Time..HH.MM.SS.), "%I:%M:%S %p")) , 12,19))) # time - use 'sub' to call target time of the raw date time after 'year/' + strptime' convert to 24 hr clock + 'period_to_seconds' converts the hms to seconds  
resp_rerun$seconds   <- (resp_rerun$time_Sec - resp_rerun$time_Sec[1])    # secs - calc the sec time series
resp_rerun$minutes   <- (resp_rerun$time_Sec - resp_rerun$time_Sec[1])/60 # mins - calc the minute time series 
                        
#is the data a txt file? (from Lolin 8 channel
resp_rerun           <- read.delim2(file = "Data/Physiology/Respiration/20220202/run_1_raw.txt", header = TRUE,skip = 37)
resp_rerun           <- read.delim2(file = "Data/Physiology/Respiration/20220202/run_2_raw.txt", header = TRUE,skip = 37)
resp_rerun           <- read.delim2(file = "Data/Physiology/Respiration/20220202/run3_raw.txt", header = TRUE,skip = 37)
resp_rerun           <- read.delim2(file = "Data/Physiology/Respiration/20221026/run_2_raw.txt", header = TRUE,skip = 37, fileEncoding= "windows-1252")
resp_rerun$date      <- paste((sub("2022.*", "", resp_rerun$Date..Time..DD.MM.YYYY.HH.MM.SS.)), '2022', sep='') #  date - use 'sub' to call everything before 2021, add back 2021 using paste
resp_rerun$time_Sec  <- period_to_seconds(hms(substr((strptime(sub(".*2022/", "", resp_rerun$Date..Time..DD.MM.YYYY.HH.MM.SS.), "%I:%M:%S %p")) , 12,19))) # time - use 'sub' to call target time of the raw date time after 'year/' + strptime' convert to 24 hr clock + 'period_to_seconds' converts the hms to seconds  
resp_rerun$seconds   <- (resp_rerun$time_Sec - resp_rerun$time_Sec[1])    # secs - calc the sec time series
resp_rerun$minutes   <- (resp_rerun$time_Sec - resp_rerun$time_Sec[1])/60 # mins - calc the minute time series

#to calculate mg per L from air saturation....
temperature_C        <- as.numeric(resp_rerun$CH1.temp...C.[1])
barromP_kPa          <- as.numeric(resp_rerun$Barometric.pressure..hPa.[1]) / 10
salinity.pp.thou     <- as.numeric(resp_rerun$Salinity....[1])
                       
resp_rerun_LoLin <- resp_rerun[seq(1, nrow(resp_rerun), 15), ]  %>% # data every 15 seconds to decrease the run time
                    dplyr::filter(!colnames(resp_rerun)[2] %in% 'NaN') %>% # Lolin recorede NAs are written as 'Nan' - wonts run unless removed!
                    #dplyr::select(c("minutes", "CH1.O2...air.sat..")) %>% # run 1 ch 1 20220202
                    #dplyr::select(c("minutes", "CH5.O2...air.sat..")) %>% # run 2 ch 5 20220202
                    # dplyr::select(c("minutes", "CH2.O2...air.sat..")) %>% # run 3 ch 2 20220202
                    
                    #dplyr::filter(minutes < 70) %>% # less than minute 70 - the data we want to call
                    #dplyr::filter(minutes > 80 & minutes < 120) %>% # after minute 80
                    #dplyr::filter(minutes > 50 & minutes < 80) %>% # after minute 60
                    
  
                    # dplyr::select(c("minutes", "CH7.O2...air.sat..")) %>% # run 2 ch7 20221026
                    # dplyr::filter(minutes <20) %>% # run 2 ch7 20221026 - before numute 20
  
                    # dplyr::select(c("minutes", "CH4.O2...air.sat..")) %>% # run 2 ch4 20221026
                    # dplyr::filter(minutes <25) %>% # run 2 ch7 20221026 - before numute 20
                    
                    dplyr::select(c("minutes", "CH3.O2...air.sat..")) %>% # run 2 ch3 20221026
                    dplyr::filter(minutes <25) %>% # run 2 ch7 20221026 - before numute 20

  
                    dplyr::mutate(mgL = (DO.unit.convert(as.numeric(CH3.O2...air.sat..),  # DO in percent air sat to be converted to mgL - uses an R package from loligo rMR
                                                         DO.units.in = "pct", DO.units.out ="mg/L", 
                                                         bar.units.in = "kPa", bar.press = barromP_kPa, bar.units.out = "kpa",
                                                         temp.C = temperature_C, 
                                                         salinity.units = "pp.thou", salinity = salinity.pp.thou)))

model <- rankLocReg(
  xall    = as.numeric(resp_rerun_LoLin[, 1]), 
  yall    = as.numeric(resp_rerun_LoLin[, 3]), # call x as the minute timeseries and y as the mg L-1 O2 
  alpha   = 0.4,  # alpha was assigned earlier as 0.4 by the authors default suggestions - review Olito et al. and their github page for details
  method  = "pc", 
  verbose = TRUE) 
plot(model) # Lpc == -0.0296
plot(model) # # run 2 ch7 20221026 Lpc == -0.1012
plot(model) # # run 2 ch4 20221026 Lpc == -0.0900
plot(model) # # run 2 ch3 20221026 Lpc == -0.1063
