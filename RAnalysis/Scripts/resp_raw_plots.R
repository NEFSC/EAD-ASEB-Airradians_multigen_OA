# Purpose: Bay Scallop Project - Respiration rate data 
# measure respiration rate from raw Loligo output data 
# using Lolin.R (Olito etal. 201?) for reproducible and non-bias calculation of respiration rates

# Written by: Sam J Gurr (last edit 9/15/2021)

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
setwd("C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis") # Work computer

# CHANGE THE FOLLOWING ..THEN CONTROL A + ENTER ::::::::::::::::::::::
path.p    <- "Data/Respiration" #the location of all your respirometry files 
# ANALYSIS  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Objective: use LoLinr to run all respiration rates in a non-bias and autonomous fashion
# Ouputs: there will be two main resources from this script: 
#   (1) cumulative spreasheet of all respiration rate value for each Channel on each day
#   (2) a folder of plots from the LoLinR script to a plots folder - this will allow troubleshooting and sanity checks 

# I. call subfolders as dataframe and create a dataframe for the output

# call the subfolder names for the outside loop 'i' (i.e. 20210914)
folder.names           <- basename(list.files(path = path.p, pattern = "202", recursive = FALSE)) #list all csv file names in the folder and subfolders
folder.names.table     <- data.frame(folder.names)

# Call the cumulative dataframe that we will write to in the for loop below
df_total             <- data.frame() # start dataframe 
resp.table           <- data.frame(matrix(nrow = 1, ncol = 7)) # create dataframe to save cumunalitively during for loop
colnames(resp.table) <- c('Date', 'Channel', 'Lpc', 'Leq' , 'Lz', 'alpha','Filename') # names for comuns in the for loop


# II. A bunch o' fors and if/elses - commented throughout!

# outside 'i' loop - call each subfolder one at a time for analysis
for(i in 1:nrow(folder.names.table)) { # for every subfolder 'i' ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  # NOTE: when calling the raw files we need to accommodate the different formats
  # 20210914 used the 8-channel loligo system with raw output as .txt files with 'raw' in the title - call these using dplyr in the if/else below
  # 20210930 used the 24-channel SDR sensor dish with raw output as .csv files - call these in the if/else statement below 
  # call all txt files labeled 'raw' in each subfolder (i.e. 20210914) and create a table 
  if (folder.names.table[i,] %in% c('20210930','20220420', '20220422','20220824', '20220829', '20220830')) { # call data when ONLY the 24-channel SDR dish data was used (csv file output) 
    file.names.table    <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',folder.names.table[i,1],sep=''), pattern = "csv$", recursive = TRUE)))) 
  } else if (folder.names.table[i,] == '20211026') { # for day(s)s when BOTH the loligo system (txt files) AND SDR dish (csv files) were used
    file.names.table1    <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',folder.names.table[3,1],sep=''), pattern = "txt$", recursive = TRUE)))) %>%  dplyr::filter(grepl('raw', txt.files))#list all csv file names in the folder and subfolders
    file.names.table2    <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',folder.names.table[3,1],sep=''), pattern = "csv$", recursive = TRUE)))) #%>%  dplyr::filter(grepl('raw', txt.files))#list all csv file names in the folder and subfolders
    file.names.table     <- rbind(file.names.table1, file.names.table2)
  }  else { # all other data that used ONLY the  8-channel loligo system outputting .txt raw files (now 9/14/21 and 2/2/22)
    file.names.table    <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',folder.names.table[i,1],sep=''), pattern = "txt$", recursive = TRUE))))  %>%  dplyr::filter(grepl('raw', txt.files))
  }
  
  # inside 'm' loop - call each  raw .txt or raw .csv file file witin the subfolder 'i'
      for(m in 1:nrow(file.names.table)) { # for every raw .txt or csv file 'm' in the subfolder 'i' :::::::::::::::::::::::::::::::::::::
        
            if (gsub(".*_raw.","", file.names.table[m,]) == "txt") {
              Resp.Data           <- read.delim2(file = paste(path.p,'/',folder.names.table[i,1], '/', file.names.table[m,1], sep=''), header = TRUE,skip = 37, fileEncoding= "windows-1252") #reads in the data files
              
                # for data spanning 2021 to 2022
                if (substr(folder.names.table[i,1], 1,4) == '2021') {
                Resp.Data$date      <- paste((sub("2021.*", "", Resp.Data$Date..Time..DD.MM.YYYY.HH.MM.SS.)), '2021', sep='') #  date - use 'sub' to call everything before 2021, add back 2021 using paste
                Resp.Data$time_Sec  <- period_to_seconds(hms(substr((strptime(sub(".*2021/", "", Resp.Data$Date..Time..DD.MM.YYYY.HH.MM.SS.), "%I:%M:%S %p")) , 12,19))) # time - use 'sub' to call target time of the raw date time after 'year/' + strptime' convert to 24 hr clock + 'period_to_seconds' converts the hms to seconds  
                } else { 
                  Resp.Data$date      <- paste((sub("2022.*", "", Resp.Data$Date..Time..DD.MM.YYYY.HH.MM.SS.)), '2022', sep='') #  date - use 'sub' to call everything before 2021, add back 2021 using paste
                  Resp.Data$time_Sec  <- period_to_seconds(hms(substr((strptime(sub(".*2022/", "", Resp.Data$Date..Time..DD.MM.YYYY.HH.MM.SS.), "%I:%M:%S %p")) , 12,19))) # time - use 'sub' to call target time of the raw date time after 'year/' + strptime' convert to 24 hr clock + 'period_to_seconds' converts the hms to seconds  
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
                
                Resp.Data           <- read.csv(file = paste(path.p,'/',folder.names.table[i,1], '/', file.names.table[m,1], sep=''), header = TRUE,skip = 51) #reads in the data files
                
                # add the for loop here if/when the 24 channel Loligo (csv raw data outputs) is used in 2022!!!
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
                        run.plot   <- substr((sub(".*resp_","",file.names.table[m,1])), 1, 5)
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
                        pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Respiration/plots_raw/",folder.names.table[i,1],"_", substr((sub(".*resp_","",file.names.table[m,1])), 1, 5),"_regression.pdf"), width=10, height=12) # 20211026_resp_unfed.csv ONLY
                        print(PLOT)
                        dev.off()  }
              

        
          # if (gsub(".*_raw.","", file.names.table[i,]) == "txt") {
          #   #pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_OA/RAnalysis/Output/Respiration/plots_alpha0.4_increm15sec/",folder.names.table[i,1],"_", sub("_raw.*","",file.names.table[m,1]),"_",colnames(Resp_loop)[2],"_regression.pdf"))
          #   pdf(paste0("C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Respiration/plots_raw/",folder.names.table[i,1],"_", sub("_raw.*","",file.names.table[m,1]),"_regression.pdf"), width=10, height=12)
          #   print(PLOT)
          #   dev.off()
          # } else if (folder.names.table[i,] == '20210930') {
          #   #pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_OA/RAnalysis/Output/Respiration/plots_alpha0.4_increm15sec/",folder.names.table[i,1],"_", substr( (sub(".*M_","",file.names.table[m,1])), 1,13),"_",colnames(Resp_loop)[2],"_regression.pdf"))
          #   pdf(paste0("C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Respiration/plots_raw/",folder.names.table[i,1],"_", substr( (sub(".*M_","",file.names.table[m,1])), 1,13),"_regression.pdf"), width=10, height=12)
          #   print(PLOT)
          #   dev.off() } else { # just for the SDR run on 20211025 .csv file 
          #     #pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_OA/RAnalysis/Output/Respiration/plots_alpha0.4_increm15sec/",folder.names.table[i,1],"_", substr((sub(".*resp_","",file.names.table[m,1])), 1, 5),"_",colnames(Resp_loop)[2],"_regression.pdf")) # 20211026_resp_unfed.csv ONLY
          #     pdf(paste0("C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_multigen_OA/RAnalysis/Output/Respiration/plots_raw/",folder.names.table[i,1],"_", substr((sub(".*resp_","",file.names.table[m,1])), 1, 5),"_regression.pdf"), width=10, height=12) # 20211026_resp_unfed.csv ONLY
          #     print(PLOT)
          #     dev.off()
          #   }
      }
}

        
    