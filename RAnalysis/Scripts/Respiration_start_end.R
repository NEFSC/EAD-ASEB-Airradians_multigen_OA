# Purpose: Bay Scallop Project - Respiration rate data 
# to complimen the LoLin R analysis, calc O2 consumption from 'start' to 'end' of each trial

# Written by: Sam J Gurr (last edit 9/15/2021)

# LOAD PACKAGES ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
library(devtools) # devtools::install_github # use devtools to instlal github link
library(LoLinR) # install_github('colin-olito/LoLinR') # install LoLinR from github
library(dplyr)
library(lubridate)
library(rMR) 
library(dplyr)
library(stringr)

# SET WORKING DIRECTORY :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis")
setwd("C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis")

# CHANGE THE FOLLOWING ..THEN CONTROL A + ENTER ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
path.p    <- "Data/Respiration" #the location of all your respirometry files 


# ANALYSIS  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Objective: call the start and end data and simple oxygen consumed over time (to compliment the rate data from LoLinR)
# Ouputs: there will be ONE  main resources from this script: 
#   (1) cumulative spreasheet of all respiration rate value for each Channel on each day

# I. call subfolders as dataframe and create a dataframe for the output

# call the subfolder names for the outside loop 'i' (i.e. 20210914)
folder.names           <- basename(list.files(path = path.p, pattern = "202", recursive = FALSE)) #list all csv file names in the folder and subfolders
folder.names.table     <- data.frame(folder.names)

# Call the cumulative dataframe that we will write to in the for loop below
df_total               <- data.frame() # start dataframe 
resp.table             <- data.frame(matrix(nrow = 1, ncol = 8)) # create dataframe to save cumunalitively during for loop
colnames(resp.table)   <- c('Date', 'Channel', 'start_min', 'end_min' , 'O2_start_mgL', 'O2_end_mgL','Rate_mgO2_hour', 'Filename') # names for comuns in the for loop

# II. A bunch o' fors and if/elses - commented throughout!

# outside 'i' loop - call each subfolder one at a time for analysis
for(i in 1:nrow(folder.names.table)) { # for every subfolder 'i' ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # NOTE: when calling the raw files we need to accommodate the different formats
  # 20210914 used the 8-channel loligo system with raw output as .txt files with 'raw' in the title - call these using dplyr in the if/else below
  # 20210930 used the 24-channel SDR sensor dish with raw output as .csv files - call these in the if/else statement below 
  # call all txt files labeled 'raw' in each subfolder (i.e. 20210914) and create a table 
  if (folder.names.table[i,] == '20210930') { # call 24-channel SDR dish data - current form only calls data from 20211026  
    file.names.table     <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',folder.names.table[i,1],sep=''), pattern = "csv$", recursive = TRUE))))  %>%  dplyr::filter(grepl('RR_', txt.files))
  } else if (folder.names.table[i,] == '20211026') { # for days when both the loligo system (txt files) or SDR dish (csv files) were used
    file.names.table1    <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',folder.names.table[i,1],sep=''), pattern = "txt$", recursive = TRUE)))) %>%  dplyr::filter(grepl('raw', txt.files))#list all csv file names in the folder and subfolders
    file.names.table2    <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',folder.names.table[i,1],sep=''), pattern = "csv$", recursive = TRUE)))) #%>%  dplyr::filter(grepl('raw', txt.files))#list all csv file names in the folder and subfolders
    file.names.table     <- rbind(file.names.table1, file.names.table2)
  }  else { # all other data that used just the  8-channel loligo system outputting .txt raw files (now 9/14/21 and 2/2/22)
    file.names.table    <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',folder.names.table[i,1],sep=''), pattern = "txt$", recursive = TRUE)))) %>%  dplyr::filter(grepl('raw', txt.files))#list all csv file names in the folder and subfolders
  }  

          # inside 'm' loop - call each  raw .txt or raw .csv file file witin the subfolder 'i'
          for(m in 1:nrow(file.names.table)) { # for every raw .txt or csv file 'm' in the subfolder 'i' :::::::::::::::::::::::::::::::::::::
            
            if (gsub(".*_raw.","", file.names.table[m,]) == "txt") {
               Resp.Data           <- read.delim2(file = paste(path.p,'/',folder.names.table[i,1], '/', file.names.table[m,1], sep=''), header = TRUE,skip = 37) #reads in the data files
               
               # for data in 2021 and data in 2022 
               if (str_split((Resp.Data$Date..Time..DD.MM.YYYY.HH.MM.SS.[1]), "/", simplify = TRUE)[[3]] == "2021") {
                    Resp.Data$date      <- paste((sub("2021.*", "", Resp.Data$Date..Time..DD.MM.YYYY.HH.MM.SS.)), '2021', sep='') #  date - use 'sub' to call everything before 2021, add back 2021 using paste
                    Resp.Data$time_Sec  <- period_to_seconds(hms(substr((strptime(sub(".*2021/", "", Resp.Data$Date..Time..DD.MM.YYYY.HH.MM.SS.), "%I:%M:%S %p")) , 12,19))) # time - use 'sub' to call target time of the raw date time after 'year/' + strptime' convert to 24 hr clock + 'period_to_seconds' converts the hms to seconds  
                  } else {
                    Resp.Data$date      <- paste((sub("2022.*", "", Resp.Data$Date..Time..DD.MM.YYYY.HH.MM.SS.)), '2022', sep='') #  date - use 'sub' to call everything before 2021, add back 2021 using paste
                    Resp.Data$time_Sec  <- period_to_seconds(hms(substr((strptime(sub(".*2022/", "", Resp.Data$Date..Time..DD.MM.YYYY.HH.MM.SS.), "%I:%M:%S %p")) , 12,19))) # time - use 'sub' to call target time of the raw date time after 'year/' + strptime' convert to 24 hr clock + 'period_to_seconds' converts the hms to seconds  
                  }
              
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
              
            } else { 
              Resp.Data           <- read.csv(file = paste(path.p,'/',folder.names.table[i,1], '/', file.names.table[m,1], sep=''), header = TRUE,skip = 51) #reads in the data files
              
              # for data in 2021 and data in 2022
              if (str_split((Resp.Data$Date..DD.MM.YYYY.[1]), "/", simplify = TRUE)[[3]] == "2021") {
                Resp.Data$date      <- paste((sub("2021.*", "", Resp.Data$Date..DD.MM.YYYY.)), '2021', sep='') #  date - use 'sub' to call everything before 2021, add back 2021 using paste
                Resp.Data$time_Sec  <- period_to_seconds(hms(substr((strptime(sub(".*2021/", "", Resp.Data$Time..HH.MM.SS.), "%I:%M:%S %p")) , 12,19))) # time - use 'sub' to call target time of the raw date time after 'year/' + strptime' convert to 24 hr clock + 'period_to_seconds' converts the hms to seconds  
              } else {
                Resp.Data$date      <- paste((sub("2022.*", "", Resp.Data$Date..DD.MM.YYYY.)), '2022', sep='') #  date - use 'sub' to call everything before 2021, add back 2021 using paste
                Resp.Data$time_Sec  <- period_to_seconds(hms(substr((strptime(sub(".*2022/", "", Resp.Data$Time..HH.MM.SS.), "%I:%M:%S %p")) , 12,19))) # time - use 'sub' to call target time of the raw date time after 'year/' + strptime' convert to 24 hr clock + 'period_to_seconds' converts the hms to seconds  
              }
              
              Resp.Data$seconds   <- (Resp.Data$time_Sec - Resp.Data$time_Sec[1])    # secs - calc the sec time series
              Resp.Data$minutes   <- (Resp.Data$time_Sec - Resp.Data$time_Sec[1])/60 # mins - calc the minute time series
              temperature_C       <- as.numeric(Resp.Data$CH1.temp...C.[1])
              barromP_kPa         <- as.numeric(Resp.Data$Barometric.pressure..hPa.[1]) / 10
              salinity.pp.thou    <- as.numeric(Resp.Data$Salinity....[1])
              Resp.Data           <- Resp.Data %>% # use 'dplyr' 
              dplyr::select(c(date, seconds, minutes, contains("..Oxygen."))) # call unique column names for the 8 Channels
              colnames(Resp.Data)[c(4:(ncol(Resp.Data)))] <- substr( ( colnames(Resp.Data)[c(4:(ncol(Resp.Data)))] ), 1,2) 
            } # clean these column names to make things easier - first 3 characters
          
                   # inside 'j' loop - for each 'raw' txt file 'm', call each O2 sensor/resp chamber 'j' for analysis
                    for(j in 4:(ncol(Resp.Data))){ # for each sensor column 'j' (..starting at column 4) :::::::::::::::::::::::::::::::
                      
                      Resp_loop  <- na.omit(Resp.Data[,c(3,j)]) # noticed some random rows have 'NaN' - so I will loop the min and Channels to ommit Nas before proceeding
                      
                      # Loligo system needs to cnvert %air sat to mg / L whereas SDR dish does not 
                      if ( (substr(colnames(Resp.Data)[j],1,2) == 'CH') ) { # loligo measurements need to be converted to mg/L from %air sat - these columns are written as "CH#" 

                        Resp_loop <- Resp_loop[!is.na(as.numeric(as.character(Resp_loop[[2]]))),] 
                        Resp_loop$mgL <- DO.unit.convert(as.numeric(Resp_loop[,2]),  # % air sat to be converted to mgL - uses an R package from loligo rMR
                                                                   DO.units.in = "pct", DO.units.out ="mg/L", 
                                                                   bar.units.in = "kPa", bar.press = barromP_kPa, bar.units.out = "kpa",
                                                                   temp.C = temperature_C, 
                                                                   salinity.units = "pp.thou", salinity = salinity.pp.thou)
                      } else {Resp_loop$mgL <- Resp.Data[j]
                        Resp_loop$mgL       <- as.numeric(unlist(Resp_loop$mgL)) # need to unlist and call as numeric
                        Resp_loop$minutes   <- as.numeric(unlist(Resp_loop$minutes)) # need to unlist and call as numeric 
                        Resp_loop           <- Resp_loop[!is.na(as.numeric(as.character(Resp_loop$mgL))),] # omit 'Nan' in mgL column
                      } # for the SDR dish values that are already in mg/L simply call the column in the loop
                            
                              # now run data!
                              if (nrow(Resp_loop) < 1) { # if column 'j' is NA write NA in the cumulative sheet...
                                resp.table$Date                <- Resp.Data[1,1]
                                resp.table$Channel             <- colnames(Resp_loop)[2] 
                                resp.table[3:ncol(resp.table)] <- 'NA'
                                df       <- data.frame(resp.table) # name dataframe for this single row
                                df_total <- rbind(df_total,df) #bind to a cumulative list dataframe
                                print(df_total) # print to monitor progress
                                
                              } else { # else run LoLinR for x=mins and y=mg/l O2
                                
                                resp.table$Date                 <- Resp.Data[1,1]
                                resp.table$Channel              <- colnames(Resp_loop)[2] 
                                resp.table$start_min            <- Resp_loop$minutes[1]
                                resp.table$end_min              <- Resp_loop$minutes[nrow(Resp_loop)]
                                resp.table$O2_start_mgL         <- Resp_loop$mgL[1]
                                resp.table$O2_end_mgL           <- Resp_loop$mgL[nrow(Resp_loop)]
                                resp.table$Rate_mgO2_hour       <- ( (Resp_loop$mgL[1]) - (Resp_loop$mgL[nrow(Resp_loop)]) ) / ((Resp_loop$minutes[nrow(Resp_loop)])/60)
                                resp.table$Filename             <- file.names.table[m,1]
                                
                                df       <- data.frame(resp.table) # name dataframe for this single row
                                df_total <- rbind(df_total,df) #bind to a cumulative list dataframe
                                print(df_total) # print to monitor progress
                              }
                  } # end of inside for loop 'j' (for each sensor column 'j' [a] isolate mins and CH_ for analysis [b] convert CH_ data to mg/L using 'DO.unit.convert' [c] calc respi rates with LoLin R)
        } # end of inside  for loop 'm' (for every 'raw' .txt file 'm' in the subfolder 'i')
} # end of outside for loop 'i' (for every subfolder 'i')


# write the table 
write.table(df_total,"C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Respiration/Cumulative_resp_start_end.csv", row.names=FALSE) 
write.table(df_total,"C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Respiration/Cumulative_resp_start_end.csv", row.names=FALSE) 


