library(readxl)
library(lubridate)
library(dplyr)
library(wq)
library(ggplot2)
library(zoo)
library(chron)
library(reshape2)

source('T:/AgWQM/DataAnalysis/Hood River/HRTempTrends/fun_tempHelpers.R')

options(stringsAsFactors = FALSE)

setwd("T:/TMDL_ER/Hood_River/Temperature_trend_analysis")

files <- list.files(pattern = "xlsx")

files <- files[!grepl('~',files)]
files <- files[!grepl('docx',files)]

print_names <- c('ENLC0.1 - Efork Neal Mouth HRWG houry', 
  'NLC2.3 - Neal FirMt HRWG hourly',
  'NLC0.1 - Neal Mouth HWRG CTWSR', 
  'WNLC2.0 - WFork_Neal_abv_Eastside_lateral_HRWG_hourly', 
  'WNLC1.7 - WFork_Neal_blw_Eastside_lateral_HRWG_hourly', 
  'WNLC0.1 - WFork_Neal_mouth_HRWG_hourly',
  'WNLC5.5 - WFork_Neal_RM5.5_HRWG_hourly', 
  'WNLC7.5 - WFork_Neal_USFSboundary_USFS')

site_codes <- data.frame(SITE = c('ENLC0.1', 
                                  'NLC2.3',
                                  'NLC0.1', 
                                  'WNLC2.0', 
                                  'WNLC1.7', 
                                  'WNLC0.1',
                                  'WNLC5.5', 
                                  'WNLC7.5'), 
                         spwn_dates = c("October 15-May 15", "October 15-May 15",
                                        "October 15-May 15", "January 1-May 15",
                                        "October 15-May 15", "October 15-May 15",
                                        "January 1-May 15", "January 1-May 15")
                         )
#site_codes$ben_use_des <- 'Salmon and Trout Rearing and Migration'
site_codes$ben_use_des <- 'Core Cold Water Habitat'
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
      if (as.character(unique(mydata$TIME)) == '1899-12-30') {
        mydata$DATETIME <- mydata$DATE
      } else {
        mydata$TIME <- unlist(lapply(strsplit(as.character(mydata$TIME), " "), function(x)x[[2]]))
        mydata$DATETIME <- as.POSIXct(paste(mydata$DATE, mydata$TIME))
      }

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
  
  sdadm <- merge(sdadm, site_codes, by = 'SITE', all.x = TRUE)
  results <- temp_sufficiency_analysis(df.all = tmp.data, sdadm = sdadm)
  results_pass <- results[results$result == 'pass',]
  if (i == 1) {
    results_all <- results
  } else {
    results_all <- rbind(results_all, results)
  }

  if (nrow(results_pass) > 0) {
     setwd("T:/AgWQM/DataAnalysis/Hood River")
  for (k in 1:nrow(results_pass)) {

    lstPlots <- Temp_trends_plot(tmp.data, sdadm, results_pass$month[k])
    for (jj in 1:length(lstPlots)) {
      png(file = paste(as.character(site_codes[i,'SITE']), "_",
                      results_pass[k, "month"], "_plot_", letters[1:length(lstPlots)][jj], ".png", sep = ""),
                      width = 11, height = 8.5, units = "in", res = 100)
      print(lstPlots[[jj]])
      dev.off()
    }
    png(file = paste(as.character(site_codes[i,'SITE']), "_",
                     results_pass[k, "month"], "_plot_revised_08142017.png", sep = ""),
        width = 11, height = 8.5, units = "in", res = 100)
    mp <- multiplot(lstPlots[[1]], lstPlots[[3]], lstPlots[[2]], lstPlots[[4]], cols = 2) #, title = paste(unique(tmp.data$SITE),
                                                                                           #             results_pass$month[k],
                                                                                            #            sep = " - "))
    dev.off()
  }
  }
  
 # ev <- EvaluateTempWQS(sdadm_df = sdadm, station_column_name = "SITE")
 # if (i == 1) {
 #   ev_all <- attr(ev, "result_summary")
 # } else {
 #   ev_all <- rbind(ev_all, attr(ev, "result_summary"))
 # }
}

nlc_sdadm <- sdadm

write.csv(ev_all, file = 'hood_river_temp_eval.csv', row.names = FALSE)

usfs_sheets <- excel_sheets('T:/TMDL_ER/Hood_River/Temperature_trend_analysis/Different format file/WFork_Neal_USFSboundary_USFS.xlsx')

usfs_daily <- data.frame()
usfs_raw <- data.frame()
for (i in 1:length(usfs_sheets)) {
  if (grepl('DATA', usfs_sheets[i])) {
    mydata <- read_excel('T:/TMDL_ER/Hood_River/Temperature_trend_analysis/Different format file/WFork_Neal_USFSboundary_USFS.xlsx', 
                         sheet = usfs_sheets[i], skip = 2, col_names = TRUE)
    if (nrow(usfs_daily) > 0) {
      usfs_daily <- rbind(usfs_daily, mydata)
    } else {
      usfs_daily <- mydata
    }
  } else if (usfs_sheets[i] < 2004) {
    next
  } else {
    mydata <- read_excel('T:/TMDL_ER/Hood_River/Temperature_trend_analysis/Different format file/WFork_Neal_USFSboundary_USFS.xlsx', 
                         sheet = usfs_sheets[i], skip = 1, col_names = TRUE)
    if (nrow(usfs_raw) > 0) {
      usfs_raw <- rbind(usfs_raw, mydata)
    } else {
      usfs_raw <- mydata
    }
  }
}

names(usfs_raw)[3] <- 'TEMP'
usfs_raw$DATE <- usfs_raw$Date
usfs_daily_all <- usfs_raw %>% 
  group_by(DATE) %>% 
  dplyr::summarise(MAX = max(TEMP)) %>% 
  rbind(usfs_daily[,c('DATE','MAX')])
usfs_daily_all <- usfs_daily_all[!is.na(usfs_daily_all$DATE),]
usfs_daily_all <- usfs_daily_all[order(usfs_daily_all$DATE),]

DATE<-as.character(seq(min(as.Date(usfs_daily_all$DATE)),max(as.Date(usfs_daily_all$DATE)),by=1))
MAX<-rep(NA,by=0,length.out=length(DATE))

dummy <-data.frame(cbind(DATE, MAX))
tmax <- rbind(usfs_daily_all, dummy)
tmax <- tmax[order(tmax$DATE),]
tmax$MAX <- as.numeric(tmax$MAX)
tmax$DATE <- as.Date(tmax$DATE)
## Calculate daily maximums by station
tmax<- tapply(tmax$MAX,list(tmax$DATE),function(x) {ifelse(all(is.na(x)),NA,max(x, na.rm = TRUE))})
sdadm<- rollapply(tmax,7,mean, fill=NA, align="right")
sdadm<- round(sdadm,1)
datevector <-as.chron(rownames(tmax))
sdadm <-data.frame(sdadm)
sdadm <- cbind(datevector,sdadm)
sdadm$date <- as.Date(sdadm$datevector)
sdadm <- sdadm[,c('date','sdadm')]
sdadm$SITE <- "WNLC7.5"

sdadm <- merge(sdadm, site_codes, by = "SITE")

usfs_sdadm <- sdadm

attr(EvaluateTempWQS(sdadm, 'SITE'), "result_summary")

temp_sufficiency_analysis(df.all = NULL, sdadm = sdadm)

png(file = "T:/AgWQM/DataAnalysis/Hood River/WNLC7.5-7_plot.png", width = 11, height = 8.5, units = "in", res = 100)
Temp_trends_plot(tmp.data = NULL, sdadm = sdadm, selectMonth = 7)
dev.off()

png(file = "T:/AgWQM/DataAnalysis/Hood River/WNLC7.5-8_plot.png", width = 11, height = 8.5, units = "in", res = 100)
Temp_trends_plot(tmp.data = NULL, sdadm = sdadm, selectMonth = 8)
dev.off()

png(file = "T:/AgWQM/DataAnalysis/Hood River/WNLC7.5-9_plot.png", width = 11, height = 8.5, units = "in", res = 100)
Temp_trends_plot(tmp.data = NULL, sdadm = sdadm, selectMonth = 9)
dev.off()

together <- merge(usfs_sdadm[,c('date','sdadm')], nlc_sdadm[,c('date','sdadm')], by = 'date', suffixes = c('.usfs','.nlc'), all = TRUE)

t.lowess <- lowess(together$sdadm.usfs, together$sdadm.nlc, delta = 1)



r = together$sdadm.nlc - t.lowess$y

mannKen(r)

t.l.df <- data.frame(date = together$date, r)

seaKen(ts(t.l.df, frequency = 4))
