Calculate.sdadm <- function(df, result_column_name, station_column_name, datetime_column_name, datetime_format) {
  # Description:
  # Calculates seven day average daily maximum
  #
  # This function takes 4 arguments:
  #  df                   = A data frame containing at minimum the columns representing a Station Identifier, Numeric results and a datetime
  #  result_column_name   = A character string specifying the name of the result column in df
  #  station_column_name  = A character string specifying the name of the station column in df
  #  datetime_column_name = A character string specifying the name of the datetime column in df. datetime column should be in format "%m/%d/%Y %H:%M:%S"
  #  datetime_format      = A character string specifying the format of the datetime column. See the format argument of strptime for details
  #
  # Details:
  # Requires installation of libraries chron and reshape
  # 
  # Result column is coerced to class numeric.
  # 
  # Result values above 36 are treated as if they are Farenheit and are modified using the conversion equation from 
  # Farenheit to Celsius.
  # 
  # NA values are removed when taking daily maximums unless a day has no observed data in which case NA will be returned.
  # 
  # Value: 
  # An object of class data frame with columns:
  # date: class Date in format %Y-%m-%d
  # station_column_name: in same format and name as provided
  # SDADM: class numeric representing the calculated seven day average
  
  require(chron)
  require(reshape)
  require(zoo)
  
  tdata <- df[,c(station_column_name, datetime_column_name, result_column_name)]
  
  ## RENAME
  colnames(tdata)[1] <- "id"
  colnames(tdata)[2] <- "datetime"
  colnames(tdata)[3] <- "t"
  
  ## F -> C  Not a perfect solution but not sure how to deal with it otherwise.
  #Ft <- "Temperature  (?F)" # Using the unit text doesn't seem to work on a pc (works on mac). I think the degree symbol is the problem.
  tdata$t <- as.numeric(tdata$t)
  tdata$t_c <- ifelse(tdata$t > 36, round(((tdata$t-32)*5/9),1),tdata$t)
  
  ## Create a vector of daily dates for grouping
  tdata$datetime <- as.POSIXct(strptime(tdata$datetime, format = datetime_format))
  tdata$date <- as.Date(tdata$datetime, format="%m/%d/%Y")
  
  #############################
  # tdata COLUMN NAMES
  # tdata[1] <- "id"
  # tdata[2] <- "datetime"
  # tdata[3] <- "t"
  # tdata[4] <- "t_c"
  # tdata[5] <- "date"
  #############################
  
  ####################################################################################################
  # This section inserts a dummy station "-99" into tdata with a sequence of -99s and NAs for the associated variables.
  # The -99 timeseries starts from the oldest date in the entire dataset and ends at the most recent date.
  # The purpose for the dummy data is to create a continous daily timeseries so 7DADM are not calculated
  # between breaks in days for the same station.
  
  datetime99<-as.character(seq(min(tdata$date),max(tdata$date),by=1))
  date99<- as.Date(seq(min(tdata$date),max(tdata$date),by=1))
  id99<-rep(unique(tdata$id),by=0,length.out=length(datetime99))
  t99<- rep(NA,by=0,length.out=length(datetime99))
  t_c99<-rep(NA,by=0,length.out=length(datetime99))
  
  dummy <-data.frame(cbind(id99, t99, t_c99))
  dummy<- cbind(dummy, datetime99, date99)
  
  colnames(dummy)[1] <- "id"
  colnames(dummy)[2] <- "t"
  colnames(dummy)[3] <- "t_c"
  colnames(dummy)[4] <- "datetime"
  colnames(dummy)[5] <- "date"
  
  dummy$t_c <- as.numeric(dummy$t_c)
  dummy$t <- as.numeric(dummy$t)
  
  tdata <-rbind(tdata,dummy)
  rm(dummy)
  #############################################################################################
  
  ## Calculate daily maximums by station
  tmax<- tapply(tdata$t_c,list(tdata$date,tdata$id),function(x) {ifelse(all(is.na(x)),NA,max(x, na.rm = TRUE))})
  
  ## Calculate 7DADM
  if (length(tmax) < 7) {
    return("Insufficient data to calculate a single 7DADM")
  } else {
    sdadm<- rollapply(tmax,7,mean, fill=NA, align="right")
    sdadm<- round(sdadm,1)
  }
  
  ## Return data to long format and rename station column header
  if (all(is.na(sdadm))) {
    return("Insufficient data to calculate a single 7DADM")
  } else {
    datevector <-as.chron(rownames(tmax))
    sdadm <-data.frame(sdadm)
    sdadm <- cbind(datevector,sdadm)
    colnames(sdadm)[1] <- "date"
    sdadm.melt <- melt.data.frame(sdadm, id.var="date",variable_name = "id")
    colnames(sdadm.melt)[3] <- "sdadm"
    sdadm.melt$id <- gsub("X","",sdadm.melt$id,fixed=TRUE)
    #sdadm.melt$id <- gsub(".","-",sdadm.melt$id,fixed=TRUE)
    colnames(sdadm.melt)[2] <- station_column_name
    sdadm.melt$date <- as.Date(sdadm.melt$date)
    return(sdadm.melt)
  }
}

EvaluateTempWQS <- function(sdadm_df, station_column_name) {
  # Description:
  # Evaluates temperature seven day average daily max values against Oregon's Water Quality Standards for Temperature
  #
  # This function takes 1 argument:
  #  sdadm_df             = A data frame with at minimum 5 columns which must be named
  #                           and formatted as specified in Details below.
  #
  # Details:
  #  Requires plyr and chron
  #
  #  sdadm_df must have columns with name and format as specified:
  #   id          = Class character representing the Station identifier
  #   date        = Class Date representing date of seven day average daily maximum
  #   sdadm       = Class numeric representing the values of the seven day average daily maximum
  #   spwn_dates  = Class character with the start and end dates of the 
  #                           applicable spawning time period. Requires the format 
  #                           "StartMonth Day-EndMonth Day" e.g. ("January 1-May 15") OR
  #                           "No spawning"
  #   ben_use_des = Class character with the beneficial use designation.
  #
  # ben_use_des must be one of:
  #   'Bull Trout Spawning and Juvenile Rearing',
  #   'Core Cold Water Habitat',
  #   'Salmon and Trout Rearing and Migration',
  #   'Salmon and Steelhead Migration Corridors',
  #   'Redband and Lanhontan Cutthroat Trout',
  #   'Cool water species',
  #   'No Salmonid Use/Out of State'
  # 
  # sdadm values are assumed to be in degrees celsius
  # 
  #
  # Value: 
  # An object of class data frame with columns:
  # 
  require(plyr)
  require(chron)
  #test case: 
  #sdadm_df$spwn_dates <- ifelse(sdadm$id %in% c(36837,36838, 36839, 36874),"August 15-May 15",ifelse(sdadm$id %in% c(36849,36850,36854,36857),"January 1-June 15","No spawning"))
  #sdadm_df$ben_use_des <- ifelse(sdadm$id %in% c(36837,36838, 36839, 36874),"Core Cold Water Habitat",ifelse(sdadm$id %in% c(36849,36850,36854,36857),"Salmon and Trout Rearing and Migration","Redband and Lanhontan Cutthroat Trout"))
  
  ## Build the spawning reference data frame based on the spawning dates and benefiicial use specified
  #sdadm_df$spwn_dates <- selectSpawning
  #sdadm_df$ben_use_des <- selectUse
  stations <- unique(sdadm_df[,station_column_name])
  spd <- unique(sdadm_df[,c('SITE','spwn_dates','ben_use_des')])
  spd_list <- strsplit(spd$spwn_dates, split = "-")
  spd_chron <- lapply(spd_list, function(x) {as.chron(x, format = "%B %d")})
  spd_months <- lapply(spd_chron, months)
  spd_days <- lapply(spd_chron, days)
  spd_months_num <- lapply(spd_months, as.numeric)
  spd_days_num <- lapply(spd_days, as.numeric)
  SSTART_MONTH <- unlist(lapply(spd_months_num, function(x) x[1]))
  SEND_MONTH <- unlist(lapply(spd_months_num, function(x) x[2]))
  SSTART_DAY <- unlist(lapply(spd_days_num, function(x) x[1]))
  SEND_DAY <- unlist(lapply(spd_days_num, function(x) x[2]))
  sdata <- cbind(spd, SSTART_MONTH, SSTART_DAY, SEND_MONTH, SEND_DAY)
  sdata$ZDADM <- suppressMessages(revalue(sdata$ben_use_des, c(
    'Bull Trout Spawning and Juvenile Rearing' = 12,
    'Core Cold Water Habitat' = 16,
    'Salmon and Trout Rearing and Migration' = 18,
    'Salmon and Steelhead Migration Corridors' = 20,
    'Redband and Lanhontan Cutthroat Trout' = 20,
    'Cool water species' = NA,
    'No Salmonid Use/Out of State' = NA
  ))
  )
  rm(spd,spd_list,spd_chron,spd_months,spd_days,
     spd_months_num,spd_days_num,SSTART_MONTH,
     SSTART_DAY,SEND_MONTH,SEND_DAY)
  
  ## Grab numeric spawning values
  sdadm_df$sdata <- match(sdadm_df[, station_column_name], 
                          sdata[, station_column_name])
  
  ## finds the current date, and spawning start/end date and formats as a numeric in the form mm.dd
  sdadm_df$cdate <- as.numeric(months(as.chron(sdadm_df$date))) + 
    (as.numeric(days(as.chron(sdadm_df$date))) * .01)
  sdadm_df$sstr <- as.numeric(sdata$SSTART_MONTH[sdadm_df$sdata]) + 
    (as.numeric(sdata$SSTART_DAY[sdadm_df$sdata]) *.01)
  sdadm_df$send <- as.numeric(sdata$SEND_MONTH[sdadm_df$sdata]) + 
    (as.numeric(sdata$SEND_DAY[sdadm_df$sdata]) *.01)
  sdadm_df$bioc <- as.numeric(sdata$ZDADM[sdadm_df$sdata])
  
  ## checks to see if there is an over winter spawning period
  sdadm_df$winter <- ifelse(sdadm_df$send < sdadm_df$sstr, TRUE, FALSE)
  
  ## looks up the summer bio criterion and spawning start end/date and returns TRUE/FALSE if current date is in summer or spawning period
  sdadm_df$bioc <- ifelse(is.na(sdadm_df$winter), sdadm_df$bioc, ifelse(
    sdadm_df$winter == TRUE,
    ifelse(sdadm_df$sstr <= sdadm_df$cdate | sdadm_df$send >= sdadm_df$cdate, 
           13, sdadm_df$bioc),
    ifelse(sdadm_df$sstr <= sdadm_df$cdate & sdadm_df$send >= sdadm_df$cdate, 
           13, sdadm_df$bioc)))
  
  sdadm_df$summer <- ifelse(sdadm_df$bioc == 13, FALSE, TRUE)
  sdadm_df$spawn <- ifelse(sdadm_df$bioc == 13, TRUE, FALSE)
  
  ## Calculate total 7DADM obersvations and # of 7DADM observations that exceed the summer spawning critera in those time periods; and 
  ## number of 7DADM observations that exceed 16 and 18 over the whole time period (not just in the stated periods)
  sdadm_df$exceedsummer <- ifelse(sdadm_df$sdadm >= sdadm_df$bioc & 
                                    sdadm_df$summer == TRUE, 1, 0)
  sdadm_df$exceedspawn <- ifelse(sdadm_df$sdadm >= sdadm_df$bioc & 
                                   sdadm_df$spawn == TRUE, 1, 0)
  sdadm_df$daystot <-ifelse(!is.na(sdadm_df$sdadm), 1, 0)
  
  ## TABULUAR RESULTS
  # daystot <- tapply(sdadm_df$daystot,list(sdadm_df[, station_column_name],
  #                                         sdadm_df$daystot), length)
  # exceedsummer <- tapply(sdadm_df$exceedsummer,
  #                        list(sdadm_df[, station_column_name],
  #                             sdadm_df$exceedsummer), length)
  # exceedspawn <- tapply(sdadm_df$exceedspawn,
  #                       list(sdadm_df[, station_column_name],
  #                            sdadm_df$exceedspawn), length)
  #sdadm_df <- sdadm_df[!is.na(sdadm_df$sdadm),]
  sdadm_df$year <- years(sdadm_df$date)
  sdadm_df$Time_Period <- ifelse(sdadm_df$summer, "Summer", "Spawning")
  sdadm_df$Time_Period <- factor(sdadm_df$Time_Period, levels = c('Summer', 'Spawning'))
  sdadm_df$exceed <- sdadm_df$exceedspawn | sdadm_df$exceedsummer
  sdadm_df_noNA <- sdadm_df[!is.na(sdadm_df$sdadm),]
  sdadm_df_noNA[is.na(sdadm_df_noNA$Time_Period), 'exceed'] <- FALSE
  sdadm_df_noNA[is.na(sdadm_df_noNA$Time_Period), 'Time_Period'] <- 'Summer'
  result_summary <- ddply(sdadm_df_noNA, .(sdadm_df_noNA[, station_column_name], year, Time_Period), 
                          summarise, 
                          Exceedances = sum(exceed),                  
                          #exceedspawn = sum(exceedspawn),
                          #exceedsummer = sum(exceedsummer),
                          Obs = sum(daystot), .drop = FALSE)
  result_summary <- plyr::rename(result_summary, 
                                 c('sdadm_df_noNA[, station_column_name]' = 
                                     station_column_name))
  
  if (any(is.na(result_summary$Time_Period))) {
    result_summary[which(result_summary$Time_Period == 'Total'), 
                   'Obs'] <- result_summary[is.na(result_summary$Time_Period),
                                            'Obs']
    result_summary <- result_summary[!is.na(result_summary$Time_Period),]
  } else {
    result_summary[result_summary$Time_Period == 'Total', 
                   'Exceedances'] <- sum(result_summary$Exceedances)
    
    result_summary[result_summary$Time_Period == 'Total', 
                   'Obs'] <- sum(result_summary$Obs)
  }
  
  attr(sdadm_df, "result_summary") <- result_summary
  
  sdadm_df <- within(sdadm_df, rm(cdate, sstr, send, winter, daystot))
  
  names(sdadm_df)[names(sdadm_df) == 'bioc'] <- 'criteria_value'
  
  return(sdadm_df)
}

temp_sufficiency_analysis <- function(df.all = NULL, sdadm) {
  if (is.null(df.all)) {
    stns <- unique(sdadm$SITE)
  } else {
    stns <- unique(df.all$SITE)
  }
  qc.results.1 <- NULL
  qc.results.2 <- NULL
  qc.results.3 <- NULL
  for (i in 1:length(stns)) {
    if (!is.null(df.all)) {
      tmp <- df.all[df.all$SITE == stns[i], ]
      
      tmp$date <- as.POSIXct(strptime(tmp$DATETIME, format = "%Y-%m-%d %H:%M:%OS"))
      tmp$month <- month(tmp$date)
      tmp$year <- year(tmp$date)
      tmp$day <- day(tmp$date)
      tmp$hour <- hour(tmp$date)
      
      # subset to data to the months of interest
      #tmp <- tmp[tmp$month %in% c(6,7,8,9,10),]
      
      # QC Test #1 -------------------------------------------------------------
      # Must be at least one observation in a minimum of 22 hours during the day
      
      # First determine number of hours collected within each day
      qc.hr <- as.tbl(tmp) %>%
        group_by(SITE, month, year, day) %>%
        summarise(n = length(unique(hour)))
      qc.hr <- as.data.frame(qc.hr)
      
      # Isolate to days with 22 or more hours represented
      qc.hr$n_threshold <- '>= 22 hours'
      qc.hr$result <- ifelse(qc.hr$n >= 22,'pass','fail')
      
      qc.results.1 <- rbind(qc.results.1,qc.hr)
      
      qc.hr.p <- qc.hr[qc.hr$result == 'pass',]
      qc.hr.p$code <- paste(qc.hr.p$SITE, qc.hr.p$year, qc.hr.p$month, qc.hr.p$day)
      tmp$code <- paste(tmp$SITE, tmp$year, tmp$month, tmp$day)
      
      # subset to just days that pass QC test #1
      tmp <- tmp[tmp$code %in% qc.hr.p$code,]
    } else {
      qc.hr <- data.frame(result = 'pass')
      tmp <- sdadm[!is.na(sdadm$sdadm),]
      qc.hr.p <- sdadm[!is.na(sdadm$sdadm),]
      
      tmp$date <- as.POSIXct(strptime(tmp$date, format = "%Y-%m-%d"))
      tmp$month <- month(tmp$date)
      tmp$year <- year(tmp$date)
      tmp$day <- day(tmp$date)

      qc.hr.p$date <- as.POSIXct(strptime(qc.hr.p$date, format = "%Y-%m-%d"))
      qc.hr.p$month <- month(qc.hr.p$date)
      qc.hr.p$year <- year(qc.hr.p$date)
      qc.hr.p$day <- day(qc.hr.p$date)
    }
    
    if (any(qc.hr$result == 'pass')) {
      # QC Test #2 -------------------------------------------------------------
      # No more than one day for each monthly period without observations
      
      qc.dy <- as.data.frame(as.tbl(qc.hr.p) %>% 
                               group_by(SITE, year, month) %>% 
                               summarise(n = n()))
      qc.dy$n_threshold <- ifelse(qc.dy$month %in% c(1,3,5,7,8,10,12), 30, 
                                  ifelse(qc.dy$month %in% c(4,6,9,11), 29,
                                         ifelse(qc.dy$year %in% c(1992, 1996, 
                                                                  2000, 2004,
                                                                  2008, 2012, 
                                                                  2016, 2020), 
                                                28, 27)))
      qc.dy$result <- ifelse(qc.dy$n >= qc.dy$n_threshold,'pass','fail')
      
      # just redoing this so threshold is clear
      qc.dy$n_threshold <- ifelse(qc.dy$month %in% c(1,3,5,7,8,10,12), 
                                  ">= 30 days", 
                                  ifelse(qc.dy$month %in% c(4,6,9,11), 
                                         ">= 29 days",
                                         ifelse(qc.dy$year %in% c(1992, 1996, 
                                                                  2000, 2004,
                                                                  2008, 2012, 
                                                                  2016, 2020), 
                                                ">= 28 days", ">= 27 days")))
      
      qc.results.2 <- rbind(qc.results.2,qc.dy)
      
      qc.dy.p <- qc.dy[qc.dy$result == 'pass',]
      qc.dy.p$code <- paste(qc.dy.p$SITE, qc.dy.p$year, qc.dy.p$month)
      tmp$code <- paste(tmp$SITE, tmp$year, tmp$month)

      # subset to just months that pass QC test #2
      tmp <- tmp[tmp$code %in% qc.dy.p$code,]
      
      if (any(qc.dy$result == 'pass')) {
        # QC Test #3 -------------------------------------------------------------
        # There must be at least eight years of continuous hourly temperature data
        # for each monthly period
        
        qc.yr <- as.data.frame(as.tbl(qc.dy.p) %>% 
                                 group_by(SITE, month) %>% 
                                 summarise(n = n()))
        qc.yr$n_threshold  <- '>= 8 years'
        qc.yr$result <- ifelse(qc.yr$n >= 8,'pass','fail')
        qc.results.3 <- rbind(qc.results.3,qc.yr)
        
        qc.yr.p <- qc.yr[qc.yr$result == 'pass',]
        
        qc.yr.p$code <- paste(qc.yr.p$SITE, qc.yr.p$month)
        tmp$code <- paste(tmp$SITE, tmp$month)
        
        # subset to just stations that pass QC test #3
        tmp <- tmp[tmp$code %in% qc.yr.p$code,]
      }
    }
  }
  
  stns_pass <- unique(qc.results.3[qc.results.3$result == 'pass', "SITE"])
  
  if (length(stns_pass) > 0) {
    attr(stns_pass, "day_test") <- qc.results.1
    attr(stns_pass, "month_test") <- qc.results.2
    attr(stns_pass, "year_test") <- qc.results.3
  }
  
  return(qc.results.3)
}

Temp_trends_plot <- function(tmp.data, sdadm, selectMonth) {
  sdadm$year <- year(sdadm$date)
  sdadm$month <- month(sdadm$date)
  
  print_month <- switch(as.character(selectMonth), 
                        "1" = "January",
                        "2" = "February",
                        "3" = "March",
                        "4" = "April",
                        "5" = "May",
                        "6" = "June", 
                        "7" = "July", 
                        "8" = "August",
                        "9" = "September",
                        "10" = "October",
                        "11" = "November",
                        "12" = "December")
  
  if (!is.null(tmp.data)) {
    tmp.data$year <- year(tmp.data$DATETIME)
    tmp.data$month <- month(tmp.data$DATETIME)
    tmp.data$day <- day(tmp.data$DATETIME)
    tmp.data$hour <- hour(tmp.data$DATETIME)
    
    tmp.data <- tmp.data[tmp.data$month == selectMonth,]
    sdadm$ZDADM <- unique(tmp.data$ZDADM)
  } 
 
  sdadm <- sdadm[sdadm$month == selectMonth,]
  
  #### Average monthly sdadm ####
  #Determine average sdadm by year and calcualte trend
  amean <- tapply(sdadm$sdadm, list(sdadm$year, sdadm$SITE), 
                  function(x) {
                    ifelse(all(is.na(x)), NA, mean(x, na.rm = TRUE))
                  })
  
  tmean <- mannKen(ts(amean))
  if(is.na(tmean$p.value)) tmean$p.value <- 0
  
  if (!is.null(tmp.data)) {
    #### Average monthly daily cumulative degree hours > WQS ####
    #First take the maximum from each hour of data
    dh <- as.tbl(tmp.data) %>%
      group_by(SITE, ZDADM, year, day, hour) %>%
      summarise(Result = max(TEMP))
    
    #Build id for efficient grouping
    dh$code <- paste(dh$SITE, dh$year, dh$day)
    
    #Calculate degree difference for each hourly max
    dh$dd <- as.numeric(dh$Result) - as.numeric(dh$ZDADM)
    
    #Set all negative values to 0
    dh$dd <- ifelse(dh$dd < 0, 0, dh$dd)
    
    #Sum the positive degree differences to derive cumulative degree hours > WQS
    dh_sum <- dh %>% 
      group_by(SITE, ZDADM, year, code) %>% 
      summarise(dh = sum(dd))
    
    #Derive the monthly average of daily degree hours > WQS
    dh_avg <- dh_sum %>%
      group_by(SITE, ZDADM, year) %>%
      summarise(dh_avg = mean(dh))
    
    #Calculate trend on average daily degree hours > WQS
    dha_wide <- cast(dh_avg, year ~ SITE, value = "dh_avg")
    tdha <- mannKen(ts(dha_wide[-1]))
    if(is.na(tdha$p.value)) tdha$p.value <- 0
    
    p2_btm <- floor(range(dh_sum$dh, na.rm = TRUE))[1]
    p2_top <- ceiling(range(dh_sum$dh, na.rm = TRUE))[2]
  }
  
  a <- NULL
  b <- NULL
  c <- NULL
  d <- NULL
  p1 = NULL
  p2 = NULL
  sig <- ""
  
  p1_btm <- 8#floor(range(sdadm$sdadm, na.rm = TRUE))[1]
  p1_top <- ifelse(ceiling(range(sdadm$sdadm, na.rm = TRUE))[2] < 20, 20, 
                   ceiling(range(sdadm$sdadm, na.rm = TRUE))[2])

  #Boxplots of 7DADM
  df <- sdadm
  df <- df[!is.na(df$sdadm),]
  df$year <- factor(df$year, levels = min(df$year):max(df$year))
  zdadm_stn <- as.numeric(unique(sdadm$ZDADM))
  a <- ggplot(data = df, aes(x=year, y=sdadm)) + 
    geom_boxplot() + 
    theme_bw() + 
    theme(axis.title = element_text(size = 14),
          plot.title = element_text(size = 12, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("Year") + 
    scale_y_continuous(breaks = seq(p1_btm, p1_top, by = 2),
                       labels = seq(p1_btm, p1_top, by = 2),
                       lim = c(p1_btm,p1_top)) +
    scale_x_discrete(drop = FALSE) +
    ylab("Temperature (degrees C)") +
    ggtitle(paste("7 Day Average Daily Maximum Temperature")) +#at", #unique(tmp.data$SITE), "in", print_month)) 
    geom_abline(intercept = zdadm_stn, slope = 0, colour = "red", size = 1.01) + 
    annotate("text", label = "Water Quality Criterion", 
             x = 14,#ifelse(min(df$sdadm) > 14, 7.5, 3.5), 
             y = ifelse(zdadm_stn > max(df$sdadm), 
                        zdadm_stn - 0.5, zdadm_stn + 0.5),
             colour = "red", size = 3.5)
  
  
  df <- as.data.frame(amean)
  df$year <- row.names(df)
  df <- melt(df)
  df$year <- as.numeric(df$year)
  df <- df[!is.na(df$value),]
  
  b <- ggplot(data = df, aes(x = year, y = value)) + 
    geom_point(aes(size = 2)) +
    scale_y_continuous(breaks = seq(p1_btm,p1_top, by = 2),
                       labels = seq(p1_btm,p1_top, by = 2),
                       lim = c(p1_btm,p1_top)) +
    scale_x_continuous(breaks = seq(min(df$year),max(df$year),by=1),
                       labels = seq(min(df$year),max(df$year),by=1),
                       lim = c(min(df$year),max(df$year))) +
    xlab("Year") + 
    ggtitle(paste("Average 7 Day Average Daily Maximum Temperature"))+# at", 
                  #unique(tmp.data$SITE), "in", print_month)) +
    ylab("Temperature (degrees C)") +
    guides(size = FALSE) +
    theme_bw() +
    theme(axis.title = element_text(size = 14),
          plot.title = element_text(size = 12, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  if (tmean$p.value < 0.1) {
    sig <- "_sig"
    #Trend plot with points as average 7DADM
    slope <- tmean$sen.slope
    p1 <- tmean$p.value
    x.delta <- as.numeric((max(df$year) - min(df$year)))/2
    SK.min <- median(df$value, na.rm = TRUE) - x.delta*slope
    SK.max <- median(df$value, na.rm = TRUE) + x.delta*slope
    b <- b + geom_segment(x = min(df$year), y = SK.min,
                          xend = max(df$year), yend = SK.max,
                          linetype = 2, size = 1.05)      
  }
  
  b <- b + annotate("text", x = min(df$year) + 6, y = p1_top - 0.5, 
                    label = ifelse(is.null(p1), "No Trend", 
                                   ifelse(p1 < 0.1, 
                                          paste("Significant Trend (p-value",
                                                ifelse(p1 < 0.05, "< 0.05)", 
                                                       "< 0.1)")), "")), 
                    size = 3.5)
  
  if (!is.null(tmp.data)) {
  #Boxplots of daily degree hours > WQS
  df <- dh_sum
  df$year <- factor(df$year, levels = min(df$year):max(df$year))
  c <- ggplot(data = df, aes(x=year, y=dh)) + 
    geom_boxplot() + 
    theme_bw() + 
    theme(axis.title = element_text(size = 14),
          plot.title = element_text(size = 12, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("Year") + 
    scale_y_continuous(breaks = c(seq(p2_btm,p2_top,by=5)),
                       labels = c(seq(p2_btm,p2_top,by=5)),
                       lim = c(p2_btm,p2_top)) +
    scale_x_discrete(drop = FALSE) +
    ggtitle(paste("Daily Degree Hours Above Water Quality Criterion"))+#at", 
                  #unique(tmp.data$SITE), "in", print_month)) +
    ylab("Daily degree hours (degrees C)")
  
  
  df <- dh_avg
  d <- ggplot(data = df, aes(x = year, y = dh_avg)) + 
    geom_point(aes(size = 1.01)) +
    scale_y_continuous(breaks = seq(p2_btm,p2_top, by = 5),
                       labels = seq(p2_btm,p2_top, by = 5))+
                       #lim = c(p2_btm,p2_top)) + #commented this out because it was not plotting the trend line when SK.min was a negative value. 
    scale_x_continuous(breaks = seq(min(df$year),max(df$year),by=1),
                       labels = seq(min(df$year),max(df$year),by=1),
                       lim = c(min(df$year),max(df$year))) +
    xlab("Year") + 
    ggtitle(paste("Average Daily Degree Hours Above Water Quality Criterion"))+#at", 
                  #unique(tmp.data$SITE), "in", print_month)) +
    ylab("Daily degree hours (degrees C)") +
    guides(size = FALSE) +
    theme_bw() +
    theme(axis.title = element_text(size = 14),
          plot.title = element_text(size = 12, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  if (tdha$p.value < 0.1) {
    sig = "_sig"
    #Trend plot with points as average daily degree hours > WQS
    slope <- tdha$sen.slope
    p2 <- tdha$p.value
    x.delta <- as.numeric((max(df$year) - min(df$year)))/2
    SK.min <- median(df$dh_avg) + 3 - x.delta*slope
    SK.max <- median(df$dh_avg) + 3 + x.delta*slope
    d <- d + geom_segment(aes(x = min(df$year), y = SK.min,
                              xend = max(df$year), yend = SK.max),
                          linetype = 2, size = 1.01)
  }
  
  d <- d + annotate("text", x = min(df$year) + 6, y = ifelse(p2_top == 1, p2_top, p2_top - 1.5), 
                    label = ifelse(is.null(p2), "No trend", 
                                   ifelse(p2 < 0.1, 
                                          paste("Significant Trend (p-value", 
                                                ifelse(p2 < 0.05, "< 0.05)", 
                                                       "< 0.1)")), "")), 
                    size = 3.5)
  }
  
  
  title_stn <- unique(sdadm$SITE)
  
  return(list(a, b, c, d))
  
  # if (is.null(c) | is.null(d)) {
  #   mp <- multiplot(a, b, cols = 2, title = paste(title_stn,
  #                                                       selectMonth,
  #                                                       sep = " - "))
  # } else {
  #     mp <- multiplot(a, c, b, d, cols = 2, title = paste(title_stn,
  #                                                     selectMonth,
  #                                                     sep = " - "))
  # }
  # 
  # return(mp)
}

# Multiple plot function
#
#  FROM R COOKBOOK http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL, title=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (numPlots == 4) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(c(0,1,2,0,3,4),
                     nrow = ceiling(numPlots/cols) + 1, ncol = cols)
  } else {
    layout <- matrix(c(0,1,0,2),
                     nrow = ceiling(numPlots/cols) + 1, ncol = cols)
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout), 
                                               heights = unit(c(1, rep(4,ceiling(numPlots/cols))), "null"))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
    
    if (!is.null(title)) {
      grid.text(title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:ncol(layout)))
    }
  }
}