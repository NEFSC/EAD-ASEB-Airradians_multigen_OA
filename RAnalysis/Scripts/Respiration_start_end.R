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
#setwd("C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis")

# CHANGE THE FOLLOWING ..THEN CONTROL A + ENTER ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
path.p    <- "Data/Physiology/Respiration" #the location of all your respirometry files 


# ANALYSIS  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Objective: call the start and end data and simple oxygen consumed over time (to compliment the rate data from LoLinR)
# Ouputs: there will be ONE  main resources from this script: 
#   (1) cumulative spreasheet of all respiration rate value for each Channel on each day

# I. call subfolders as dataframe and create a dataframe for the output

# call the subfolder names for the outside loop 'i' (i.e. 20210914)
folder.names           <- basename(list.files(path = path.p, pattern = "202", recursive = FALSE)) #list all csv file names in the folder and subfolders
folder.names.table     <- data.frame(folder.names) %>% filter(!folder.names %in% c('20220420', '20220422','20220824','20220829')) #from test data or failed spawns

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
  if (folder.names.table[i,] %in% c('20210930', '20220830')) { #'20220824',  call 24-channel SDR dish data - '
    file.names.table     <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',folder.names.table[i,1],sep=''), pattern = "csv$", recursive = TRUE))))  
  } else if (folder.names.table[i,] %in% c('20211026', '20220922')) { # for days when both the loligo system (txt files) or SDR dish (csv files) were used
    file.names.table1    <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',folder.names.table[i,1],sep=''), pattern = "txt$", recursive = TRUE)))) %>%  dplyr::filter(grepl('raw', txt.files))#list all csv file names in the folder and subfolders
    file.names.table2    <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',folder.names.table[i,1],sep=''), pattern = "csv$", recursive = TRUE)))) #%>%  dplyr::filter(grepl('raw', txt.files))#list all csv file names in the folder and subfolders
    file.names.table     <- rbind(file.names.table1, file.names.table2)
  }  else { # all other data that used just the  8-channel loligo system outputting .txt raw files (now 9/14/21 and 2/2/22)
    file.names.table    <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',folder.names.table[i,1],sep=''), pattern = "txt$", recursive = TRUE)))) %>%  dplyr::filter(grepl('raw', txt.files))#list all csv file names in the folder and subfolders
  }  

          # inside 'm' loop - call each  raw .txt or raw .csv file file witin the subfolder 'i'
          for(m in 1:nrow(file.names.table)) { # for every raw .txt or csv file 'm' in the subfolder 'i' :::::::::::::::::::::::::::::::::::::
            
            if (gsub(".*_raw.","", file.names.table[m,]) == "txt") {
               Resp.Data           <- read.delim2(file = paste(path.p,'/',folder.names.table[i,1], '/', file.names.table[m,1], sep=''), header = TRUE,skip = 37, fileEncoding= "windows-1252") #reads in the data files
               
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
              Resp.Data           <- read.csv(file = paste(path.p,'/',folder.names.table[i,1], '/', file.names.table[m,1], sep=''), header = TRUE,skip = 51, fileEncoding= "windows-1252") #reads in the data files
              
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
                      
                      
                      Resp_loop  <- na.omit(Resp.Data[,c(3,j)]) %>% dplyr::filter(!.[[2]] %in% 'NaN')# noticed some random rows have 'NaN' - so I will loop the min and Channels to ommit Nas before proceeding
                      
                      # Loligo system needs to cnvert %air sat to mg / L whereas SDR dish does not 
                      if ( (substr(colnames(Resp.Data)[j],1,2) == 'CH') ) { # loligo measurements need to be converted to mg/L from %air sat - these columns are written as "CH#" 

                        Resp_loop <- Resp_loop[!is.na(as.numeric(as.character(Resp_loop[[2]]))),] 
                        Resp_loop$mgL <- DO.unit.convert(as.numeric(Resp_loop[,2]),  # % air sat to be converted to mgL - uses an R package from loligo rMR
                                                                   DO.units.in = "pct", DO.units.out ="mg/L", 
                                                                   bar.units.in = "kPa", bar.press = barromP_kPa, bar.units.out = "kpa",
                                                                   temp.C = temperature_C, 
                                                                   salinity.units = "pp.thou", salinity = salinity.pp.thou)
                      } else {Resp_loop$mgL <- na.omit(Resp.Data[j])
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
                                
                                
                                
                                
                                  # incase there are jumps of low mgL outlier data mid -trial
                                  if ( (nrow(Resp_loop %>% dplyr::filter(mgL  < (min(mgL))+(sd(mgL)))) /
                                    nrow(Resp_loop)) * 100 > 1 ) { # call statement to omit these values min+sdDev 
                                    Resp_loop <- Resp_loop
                                      } else {
                                        Resp_loop <- Resp_loop %>% dplyr::filter(!mgL  < (min(mgL))+(1/3*sd(mgL))) # omit these data ~ 1 %of rows - View --> rowsOmit = round(nrow(Resp_loop) * 0.01)
                                      }
                                  
                                
                                
                                  sd_mgL    <- sd(Resp_loop$mgL)
                                  mean_mgL  <- mean(Resp_loop$mgL)
                                  
                                  
                                  resp.table$Date                 <- Resp.Data[1,1]
                                  resp.table$Channel              <- colnames(Resp_loop)[2] 
                                  resp.table$Filename             <- file.names.table[m,1]
                                  
                                  if(sd_mgL < 0.1) { # for occasional like blanks that may have sudden drops/errors whereas the entire dataset is very stable (i.e. 9 22 2022 Ch8 run 1)
                                    Resp_loop <- Resp_loop %>% filter(!mgL < (mean_mgL - (sd_mgL*1.5)) )
                                  } else {
                                    Resp_loop <- Resp_loop
                                  }
                                  
                                  if ( Resp_loop$mgL[nrow(Resp_loop)] < (sd_mgL+min(Resp_loop$mgL)) ) { # if the last row of O2 is less than the standard dev of O2 + the min value - meaning the last point os the true O2 minimum
                                    minim_mgL <- Resp_loop$mgL[nrow(Resp_loop)]
                                    resp.table$start_min            <- Resp_loop$minutes[1]
                                    resp.table$end_min              <- Resp_loop$minutes[nrow(Resp_loop)]    # TRUE, call the last time point      
                                    resp.table$O2_start_mgL         <- max(Resp_loop$mgL[1:20])
                                    resp.table$O2_end_mgL           <- minim_mgL 
                                    resp.table$Rate_mgO2_hour       <- (   (max(Resp_loop$mgL[1:20]) - minim_mgL)/ ((resp.table$end_min[[1]] - resp.table$start_min[[1]])/60)   )#( (Resp_loop$mgL[1]) - (Resp_loop$mgL[nrow(Resp_loop)]) ) / (abs(Resp_loop$minutes[nrow(Resp_loop)])/60)
                                  } else { # else the last value is greater than the standard deviation + the minimum O2 value (meaning that the channel was reoxygenated )
                                    minim_mgL <-min(Resp_loop$mgL)
                                    resp.table$O2_start_mgL         <- max(Resp_loop$mgL[1:20])
                                    resp.table$start_min            <- min((Resp_loop %>% filter(mgL == resp.table$O2_start_mgL[[1]]))$minutes)
                                    resp.table$end_min              <- max((Resp_loop %>% filter(mgL == minim_mgL))$minutes)    # TRUE, call the last time point      
                                    resp.table$O2_end_mgL           <- minim_mgL 
                                    resp.table$Rate_mgO2_hour       <-  ( (resp.table$O2_start_mgL[[1]] - resp.table$O2_end_mgL[[1]]) / ((resp.table$end_min[[1]] - resp.table$start_min[[1]])/60)  )#( (Resp_loop$mgL[1]) - (Resp_loop$mgL[nrow(Resp_loop)]) ) / (abs(Resp_loop$minutes[nrow(Resp_loop)])/60)
                                    resp.table$Filename             <- file.names.table[m,1]
                                      }
                                  

                                  df       <- data.frame(resp.table) # name dataframe for this single row
                                  df_total <- rbind(df_total,df) #bind to a cumulative list dataframe
                                  print(df_total) # print to monitor progress
                              }
                  } # end of inside for loop 'j' (for each sensor column 'j' [a] isolate mins and CH_ for analysis [b] convert CH_ data to mg/L using 'DO.unit.convert' [c] calc respi rates with LoLin R)
        } # end of inside  for loop 'm' (for every 'raw' .txt file 'm' in the subfolder 'i')
} # end of outside for loop 'i' (for every subfolder 'i')
#View(df_total)
nrow(df_total) #408
# View(df_total)
df_total <- as.data.frame(df_total)

216 ==212

228 == 232

# blank data errors
# 1) 3/1/2022 run 3
df_total[212,] # 3/1/2022 CH4 1.138935 run3_raw.txt -blank rate too high, change to CH8 (other blank) in same run!
df_total[216,] # 0.1991301 
df_total[216,7] = 0.1991301
# 2) 3/1/2022 run 3
df_total[228,] # 3/1/2022 CH4 1.138935 run3_raw.txt -blank rate too high, change to CH8 (other blank) in same run!
df_total[232,] # 0.1991301 
df_total[228,7] = 0.1991301

0.1991301 == CH4 run 3 3/1/2022 

CH4 run 1 and run3 == CH* run 1 and run 3 
# obvious errors:

### Channel 8 Loligo blank on 9/22/22


# upload ref metadata for RR (treamtent, plate, run, replication, number, etc.)
RefID <- read.csv(file="C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Data/Physiology/Respiration/Reference_resp_ID.csv", header = TRUE, sep = ',') %>% 
  dplyr::filter(!Date %in% c('4/20/2022', '4/22/2022','8/24/2022','8/29/2022'))
nrow(RefID) # 408

# upload ref sizes
Refsize <- read.csv(file="C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Data/Physiology/Respiration/Reference_resp_size.csv", header = TRUE, sep = ',') %>% 
  dplyr::filter(!Date %in% c('4/20/2022', '4/22/2022','8/24/2022','8/29/2022', '8/30/2022')) %>% 
  dplyr::select(c('Date','Run','Plate','pH', 'Replicate', 'Chamber_tank', 'Number', 'Length_um', 'Biovolume_g_in_sw', 'Dry_Tissue_weight', 'whole_Dry_weight','Instrument'))
nrow(Refsize) # 265 does not include the many trials that were run as tests that this resp sheet indlues (df)

# merge df_total with the ref IDs
merged_df_ref <- as.data.frame(merge(df_total,RefID))
nrow(merged_df_ref) # 408 - all in the ref are here in the start end raw data 

# merge the dataframe above (previous merge) with the sizes (note! no blanks here)
merged_withSize <- merge(merged_df_ref,Refsize) %>% 
  dplyr::filter(!Fed_Unfed %in% 'U') %>% 
  dplyr::mutate(filetype = str_sub(Filename, -3,-1)) %>% 
  dplyr::mutate(filetype = factor(ifelse(filetype == "csv", "SDR_data", "LoLigo_data"))) %>%
  dplyr::select(c('Date','pH','Chamber_tank','Channel', 'Run','Number','Length_um', 'Biovolume_g_in_sw', 'Dry_Tissue_weight','whole_Dry_weight', 'Rate_mgO2_hour', 'filetype')) %>% 
  dplyr::rename(Start.End_RR_mgO2hr = Rate_mgO2_hour)  %>% 
  dplyr::rename(Dry_Tissue_weight_mg = Dry_Tissue_weight) %>% 
  dplyr::rename(Whole_Dry_weight_mg  = whole_Dry_weight) %>% 
  dplyr::mutate(volume = 
                case_when(filetype == "LoLigo_data" & Date %in% c('9/14/2021','9/30/2021') ~ 1.7,# fed F1 foodxOA
                          filetype == "SDR_data" & Date %in% c('9/30/2021','10/26/2021')~ 1.7, # small unfed F1s foodxOA 
                          
                          # filetype == "SDR_data" & Date %in% c('9/30/2021','10/26/2021')~ 1.7, # small unfed F1s foodxOA 
                          
                          filetype == "LoLigo_data" & Date %in% c('10/26/2021') & Channel == 'CH1' ~ 23.1, # F1 data
                          filetype == "LoLigo_data" & Date %in% c('10/26/2021') & Channel == 'CH2' ~ 23.05, 
                          filetype == "LoLigo_data" & Date %in% c('10/26/2021') & Channel == 'CH4' ~ 22.55,
                          filetype == "LoLigo_data" & Date %in% c('10/26/2021') & Channel == 'CH5' ~ 23.18,
                          filetype == "LoLigo_data" & Date %in% c('10/26/2021') & Channel == 'CH6' ~ 23.24,
                          filetype == "LoLigo_data" & Date %in% c('10/26/2021') & Channel == 'CH7' ~ 22.63, 
                          filetype == "LoLigo_data" & Date %in% c('10/26/2021') & Channel == 'CH8' ~ 22.95,
                          filetype == "LoLigo_data" & Date %in% c('10/26/2021') & Channel == 'CH3' ~ 23.31,
                          
                          filetype == "LoLigo_data" & Date %in% c('2/2/2022','3/1/2022') & Channel == 'CH1' ~ 68.55323, # F1 data
                          filetype == "LoLigo_data" & Date %in% c('2/2/2022','3/1/2022') & Channel == 'CH2' ~ 68.85583, 
                          filetype == "LoLigo_data" & Date %in% c('2/2/2022','3/1/2022') & Channel == 'CH4' ~ 68.95481,
                          filetype == "LoLigo_data" & Date %in% c('2/2/2022','3/1/2022') & Channel == 'CH5' ~ 68.57288,
                          filetype == "LoLigo_data" & Date %in% c('2/2/2022','3/1/2022') & Channel == 'CH6' ~ 68.01878,
                          filetype == "LoLigo_data" & Date %in% c('2/2/2022','3/1/2022') & Channel == 'CH7' ~ 68.54551, 
                          filetype == "LoLigo_data" & Date %in% c('2/2/2022','3/1/2022') & Channel == 'CH8' ~ 68.53297,
                          filetype == "LoLigo_data" & Date %in% c('2/2/2022','3/1/2022') & Channel == 'CH3' ~ 68.87473,
                          
                          filetype == "LoLigo_data" & Date %in% c('9/22/2022') & Channel == 'CH1' ~ 196.6, # F1 data
                          filetype == "LoLigo_data" & Date %in% c('9/22/2022') & Channel == 'CH2' ~ 200.6, 
                          filetype == "LoLigo_data" & Date %in% c('9/22/2022') & Channel == 'CH3' ~ 201.4, 
                          filetype == "LoLigo_data" & Date %in% c('9/22/2022') & Channel == 'CH4' ~ 200, 
                          filetype == "LoLigo_data" & Date %in% c('9/22/2022') & Channel == 'CH5' ~ 199.2, 
                          filetype == "LoLigo_data" & Date %in% c('9/22/2022') & Channel == 'CH6' ~ 200.4, 
                          filetype == "LoLigo_data" & Date %in% c('9/22/2022') & Channel == 'CH7' ~ 201.3, 
                          filetype == "LoLigo_data" & Date %in% c('9/22/2022') & Channel == 'CH8' ~ 201.7, 
                          
                          filetype == "LoLigo_data" & Date %in% c('10/26/2022') & Channel == 'CH1' ~ (236.70+18), # F1 data
                          filetype == "LoLigo_data" & Date %in% c('10/26/2022') & Channel == 'CH2' ~ (239.88+18), 
                          filetype == "LoLigo_data" & Date %in% c('10/26/2022') & Channel == 'CH3' ~ (228.63+18), 
                          filetype == "LoLigo_data" & Date %in% c('10/26/2022') & Channel == 'CH4' ~ (234.35+18), 
                          filetype == "LoLigo_data" & Date %in% c('10/26/2022') & Channel == 'CH5' ~ (224.31+18), 
                          filetype == "LoLigo_data" & Date %in% c('10/26/2022') & Channel == 'CH6' ~ (229.07+18), 
                          filetype == "LoLigo_data" & Date %in% c('10/26/2022') & Channel == 'CH7' ~ (223.11+18), 
                          filetype == "LoLigo_data" & Date %in% c('10/26/2022') & Channel == 'CH8' ~ (225.59+18), 
                          
                          filetype == "SDR_data" & Date %in% c('8/30/2022') ~ 0.08, # F2 data 
                          filetype == "SDR_data" & Date %in% c('9/22/2022') ~ 1.7,  # F2 data
                          
                          filetype == "LoLigo_data" & Date %in% c('11/16/2022') & Channel == 'CH1' ~ 23.1, # F2 data
                          filetype == "LoLigo_data" & Date %in% c('11/16/2022') & Channel == 'CH2' ~ 23.05,
                          filetype == "LoLigo_data" & Date %in% c('11/16/2022') & Channel == 'CH3' ~ 23.31,
                          filetype == "LoLigo_data" & Date %in% c('11/16/2022') & Channel == 'CH4' ~ 22.55,
                          filetype == "LoLigo_data" & Date %in% c('11/16/2022') & Channel == 'CH5' ~ 23.18,
                          filetype == "LoLigo_data" & Date %in% c('11/16/2022') & Channel == 'CH6' ~ 23.24,
                          filetype == "LoLigo_data" & Date %in% c('11/16/2022') & Channel == 'CH7' ~ 22.63, 
                          filetype == "LoLigo_data" & Date %in% c('11/16/2022') & Channel == 'CH8' ~ 22.95))


# write csv for the start end RAW (as merged_withSize)
write.csv(merged_withSize, "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Respiration/RR_start_end_raw.csv")
unique(merged_withSize$Date)
# View(merged_withSize)

# mean blanks for the start - to - end values ('Rate_mgO2_hour') :::::::::::::::::::::::::::::::::::::::::
blanks.raw   <- merged_df_ref %>% 
  dplyr::mutate(filetype = str_sub(Filename, -3,-1)) %>% 
  dplyr::mutate(filetype = factor(ifelse(filetype == "csv", "SDR_data", "LoLigo_data"))) %>%
  dplyr::filter(Chamber_tank %in% 'blank') %>% 
  dplyr::select(c('Date','pH','Fed_Unfed','Run', 'Rate_mgO2_hour','filetype')) # no plate for SDR

# View(blanks.raw)
blanks_meansStartEnd <- as.data.frame(blanks.raw %>% 
                          dplyr::group_by(Date, pH, Run, filetype) %>% # grouped by date, pH, and instrument - similar among Runs
                          dplyr::filter(!Rate_mgO2_hour < 0) %>% # ommit blank calls that d/n represent oxygen consumption
                          dplyr::summarise(BLANK_Start.End_RR_mgO2hr  = mean(Rate_mgO2_hour),
                                           # BLANK.start.end_sd  = sd(Rate_mgO2_hour),
                                           n = n()) %>% 
                          #dplyr::mutate(Date_formatted =  gsub("-", "", substr( (strptime(Date, "%m/%d/%Y")), 1,10)) ) %>% 
                           dplyr::arrange(Date, Run, pH))
# View(blanks_meansStartEnd)


Start.end_RR_master <- merge(merged_withSize, blanks_meansStartEnd, by=c("Date", "pH", "Run","filetype")) %>% 
  dplyr::select(!('n')) %>% 
  dplyr::mutate(Length_mm = Length_um / 1000) %>% 
  dplyr::mutate(Start.End_RR_mgO2hr_blankcor = as.numeric(Start.End_RR_mgO2hr - BLANK_Start.End_RR_mgO2hr)) %>% 
  dplyr::filter(!Start.End_RR_mgO2hr_blankcor < 0)  %>% # 33 rows were less than the blank
  dplyr::mutate(Biovol_length3 = (0.000198*(Length_mm^3)))  %>%  # calc biovolume based on length^3;  in Seember 2022 we decided to go with the length3 method for extrapolating biovolume for animals that do not have biovolume measurments 
  # CALCULATE THE ACTUAL VOLUME (accounting for measured and calculated biovolumes)
   # (1) 'calculated_volume' employing length^3 biovolume estimate to correct for volume displaces
  dplyr::mutate(calculated_volume = (volume - Biovol_length3)) %>%  # calculated biovolume based on length^3
  dplyr::mutate(Start.End_RR_ugO2hr_biovolcalc =  (Start.End_RR_mgO2hr_blankcor*1000)*(calculated_volume/1000)  ) %>% # per liter
  dplyr::mutate(Start.End_RR_umolhr_biovolcalc =  Start.End_RR_ugO2hr_biovolcalc/32  ) %>% 

  
  # (2) 'measured_volume' employing ONLY the measured values - resulting in many NAs for data where we did not measure it
  dplyr::mutate(measured_volume = (volume - Biovolume_g_in_sw)) %>%  # measured biovolume *not all data has a value for this!
  dplyr::mutate(Start.End_RR_ugO2hr =  (Start.End_RR_mgO2hr_blankcor*1000)*(measured_volume/1000)  ) %>% # per liter
  dplyr::mutate(Start.End_RR_umolhr =  Start.End_RR_ugO2hr/32  ) %>% 
  dplyr::mutate(pCO2 = 
                  case_when(pH == 8.0 ~ "500 μatm", 
                            pH == 7.5 ~ "800 μatm"))


  

View(Start.end_RR_master)

# merged_withSize
write.csv(Start.end_RR_master, "C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Respiration/RR_start_end_master.csv")
