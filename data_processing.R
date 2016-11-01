library(readxl)
library(lubridate)
library(dplyr)
library(wq)
library(ggplot2)
source('T:/AgWQM/DataAnalysis/Hood River/HRTempTrends/fun_tempHelpers.R')

setwd("T:/TMDL_ER/Hood_River/Temperature_trend_analysis")

files <- list.files()

files <- files[!grepl('~',files)]
files <- files[!grepl('docx',files)]

site_codes <- data.frame(SITE = c('ENLC0.1', 'NLC2.3', 'NLC0.1', 
                                  'WNLC2.0', 'WNLC1.7', 'WNLC0.1',
                                  'WNLC5.5', 'WNLC7.5'), 
                         spwn_dates = c("October 15-May 15", "October 15-May 15",
                                        "October 15-May 15", "January 1-May 15",
                                        "October 15-May 15", "October 15-May 15",
                                        "January 1-May 15", "January 1-May 15")
                         )
site_codes$ben_use_des <- 'Salmon and Steelhead Migration Corridors'
site_codes$ZDADM <- suppressMessages(plyr::revalue(site_codes$ben_use_des, c(
  'Bull Trout Spawning and Juvenile Rearing' = 12,
  'Core Cold Water Habitat' = 16,
  'Salmon and Trout Rearing and Migration' = 18,
  'Salmon and Steelhead Migration Corridors' = 20,
  'Redband and Lanhontan Cutthroat Trout' = 20,
  'Cool water species' = NA,
  'No Salmonid Use/Out of State' = NA
)))

for (i in 1:length(files)) {
  setwd("T:/TMDL_ER/Hood_River/Temperature_trend_analysis")
  sheets <- excel_sheets(files[i])
  for (j in 1:length(sheets)) {
    mydata <- read_excel(files[i], sheet = sheets[j], skip = 4, col_names = TRUE)
    mydata <- mydata[,!(is.na(names(mydata)) | names(mydata) == "")]
    if ('DQL' %in% names(mydata)) {
      #mydata <- within(mydata, rm(DQL)) 
      mydata <- mydata[,1:3]
    }
    mydata <- mydata[complete.cases(mydata),]
    if (any(names(mydata) == 'DATE/TIME')) {
      if (class(mydata$`DATE/TIME`) == 'character') {
        mydata$DATETIME <- as.POSIXct(strptime(mydata$`DATE/TIME`, 
                                               format = "%y-%m-%d %H:%M:%S"))
      } else {
        mydata$DATETIME <- mydata$`DATE/TIME`
      }# split_date <- strsplit(mydata[,'DATE/TIME'], " ")
      # mydata$DATE <- unlist(lapply(split_date, function(x){x[[1]]}))
      # mydata$TIME <- unlist(lapply(split_date, function(x){x[[2]]}))
    } else {
      mydata$TIME <- unlist(lapply(strsplit(as.character(mydata$TIME), " "), function(x)x[[2]]))
      mydata$DATETIME <- as.POSIXct(paste(mydata$DATE, mydata$TIME))
    }
    mydata <- mydata[,c('DATETIME','TEMP')]
    if (j == 1) {
      tmp.data <- mydata
    } else {
      tmp.data <- rbind(tmp.data, mydata)
    }
  }
  tmp.data$SITE <- as.character(site_codes[i,'SITE'])
  
  tmp.data <- merge(tmp.data, site_codes, by = 'SITE')
  
  sdadm <- Calculate.sdadm(df = tmp.data, 
                           result_column_name = 'TEMP', 
                           station_column_name = 'SITE', 
                           datetime_column_name = 'DATETIME',
                           datetime_format = "%Y-%m-%d %H:%M:%S")
  results <- temp_sufficiency_analysis(df.all = tmp.data, sdadm = sdadm)
  results_pass <- results[results$result == 'pass',]
  
  setwd("T:/AgWQM/DataAnalysis/Hood River")
  for (k in 1:nrow(results_pass)) {
    png(file = paste(as.character(site_codes[i,'SITE']), "_", 
                     results_pass[k, "month"], "_plot.png", sep = ""),
        width = 11, height = 8.5, units = "in", res = 100)
    Temp_trends_plot(tmp.data, sdadm, results_pass$month[k])
    dev.off()
  }  
}


