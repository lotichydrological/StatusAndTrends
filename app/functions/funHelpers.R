clipToPlanArea <- function (df.all, agwqma, selected_planArea) {
  #Function to clip stations returned from HUC based query to specific
  #plan area selected since plan areas don't match HUCs exactly.
  #Arguments:
  #df.all = data frame with data returned from function combine
  #agwqma = shapefile of ag plan area polygons
  #selected_planArea = input$select which is the selected plan area from the user defined query
  
  #First pull out stations and set the projection
  all.sp <- df.all[!duplicated(df.all$SD),c(3,1:2,4:17)]
  coordinates(all.sp) = ~DECIMAL_LONG+DECIMAL_LAT
  proj4string(all.sp) <- CRS("+init=epsg:4269")
  #Transform agwqma to same projection as stations
  #agwqma <- spTransform(agwqma, CRS("+proj=longlat +datum=NAD83"))
  #Subset agwqma shapefile to just the plan area selected
  ag_sub <- agwqma[agwqma$PlanName == selected_planArea,]
  #Transform agwqma to same projection as stations
  ag_sub <- spTransform(ag_sub, CRS("+init=epsg:4269"))
  #Extract stations within the selected plan area
  all.sp <- all.sp[ag_sub,]
  #Restrict the data frame to just those results at stations within the plan area
  df.all <- df.all[df.all$Station_ID %in% all.sp@data$Station_ID,]
  
  return(df.all)
}

tabulateResults <- function (df.all) {
  #Summarizes results returned to give number of stations with data
  #and total number of results returned grouped by analyte
  #Arguments:
  #df.all = data frame of data with column names Station_ID, Analyte and Result
  library(dplyr)
  all.totals <- df.all %>% 
    group_by(Analyte) %>% 
    summarize(Stations = length(unique(Station_ID)), Results = length(Result))
  all.totals <- as.data.frame(all.totals)
  return(all.totals)
}

MRLhandling <- function (df.all) {
  #Makes MRL numeric to determine detect/non-detect condition and 
  #sets Result to MRL when condition is non-detect
  #Arguments:
  #df.all = data frame of data with column names MRL, Result
  df.all$MRL <- suppressWarnings(as.numeric(df.all$MRL))
  
  df.all[is.na(df.all$MRL),'MRL'] <- 0
  
  df.all$Detect <- ifelse(df.all$Result < df.all$MRL,0,1)
  df.all[which(df.all$Result < df.all$MRL),'Result'] <- 
    df.all[which(df.all$Result < df.all$MRL),'MRL']   
  
  df.all[which(df.all$Result == 'ND'),'Detect'] <- 0 
  df.all[which(df.all$Result == 'ND'),'Result'] <- 
    df.all[which(df.all$Result == 'ND'),'MRL']
  
  return(df.all)
}

update_fc2ec <- function (df.all) {
  #COnverts fecal coliform values to e. coli and updates comment field
  #to keep track of the conversion
  #Arguments:
  #df.all = data frame of query results with columns Analyte, Result, Comment
  df.all[df.all$Analyte == 'Fecal Coliform','Result'] <- 
    round(fc2ec(df.all[df.all$Analyte == 'Fecal Coliform','Result']))
  df.all[df.all$Analyte == 'Fecal Coliform','Comment'] <- 
    ifelse(is.na(df.all[df.all$Analyte == 'Fecal Coliform','Comment']),
           "Fecal Coliform value converted to E. Coli",
           paste(df.all[df.all$Analyte == 'Fecal Coliform','Comment'],
                 "Fecal Coliform value converted to E. Coli",
                 sep = ", "))
  df.all[df.all$Analyte == 'Fecal Coliform','Analyte'] <- 'E. Coli'
  return(df.all)
}

remove_QAfail <- function (df.all) {
  #Removes data that does not meet QA objectives and keeps track of 
  #data removed from parent data frame
  #Arguments:
  #df.all = data frame of data returned from query with column named Status, Comment
  #Returns:
  #df.all with attribute removal_tracking = data frame with comments added fore
  #reason for removal of data from parent data frame
  
  #Identifies data from DEQ sources with Grades D, E or F 
  df.ex <- df.all[df.all$Status %in% c('D','E','F'),]
  df.all <- df.all[!df.all$Status %in% c('D','E','F'),]
  if (nrow(df.ex) > 0) {
    df.ex$Reason <- "Sample Status equivalent to D, E or F"
  }
  
  #Identifies data from NWIS or STORET with comments indicating sample contamination
  df.ex2 <- df.all[grep('Qualifier=C',df.all$Comment),]
  df.all <- df.all[!grepl('Qualifier=C | [Cc]ontamination | QCS FAILED',
                          df.all$Comment),]
  if (nrow(df.ex2) > 0) {
    df.ex2$Reason <- "Comment indicates sample contamination"
    df.ex <- rbind(df.ex, df.ex2)
    rm(df.ex2)
  }
  
  #Identifies data from NWIS or STORET with comments indicating major QC issue
  df.ex2 <- df.all[grep('Qualifier=Q',df.all$Comment),]
  df.all <- df.all[!grepl('Qualifier=Q',df.all$Comment),]
  if (nrow(df.ex2) > 0) {
    df.ex2$Reason <- "Comment indicates major QC issue"
    df.ex <- rbind(df.ex, df.ex2)
    rm(df.ex2)
  }
  
  #Identifies data where Result was Void, Cancelled or NA for some other reason
  df.ex2 <- df.all[is.na(df.all$Result),]
  df.all <- df.all[!is.na(df.all$Result),]
  if (nrow(df.ex2) > 0) {
    df.ex2$Reason <- "Result is Void, Cancelled or other NA"
    df.ex <- rbind(df.ex, df.ex2)
    rm(df.ex2)
  }
  
  attr(df.all, "removal_tracking") <- df.ex
  
  return(df.all)
}

summarizeByOrg <- function(df.all) {
  #Tabulates results by organization. The intent of this summary is to provide
  #a high level look at whether the organization returned any comments indicating
  #whether they have included any qc info worth looking at.
  #Arguments:
  #df.all = data frame of query results with columns Client, Comment, Result, Station_ID
  #Returns:
  #df.summary = data frame with totals by organization
  for (i in 1:length(unique(df.all$Client))) {
    org.rows <- nrow(df.all[which(df.all$Client == 
                                    unique(df.all$Client)[i]),])
    org.na.rows <- length(df.all[which(df.all$Client == 
                                         unique(df.all$Client)[i]),
                                 'Result'][is.na(df.all[which(
                                   df.all$Client == 
                                     unique(df.all$Client)[i]),
                                   'Result'])])
    org.stations <- length(unique(df.all[df.all$Client == 
                                           unique(df.all$Client)[i],
                                         'Station_ID']))
    org.comments <- ifelse(all(c("" ,NA) %in% unique(
      df.all[df.all$Client == unique(df.all$Client)[i],'Comment'])),0,
      length(unique(df.all[df.all$Client == unique(df.all$Client)[i],
                           'Comment'])))
    new.row <- data.frame("Organization" = unique(df.all$Client)[i],
                          "Observations" = org.rows,
                          "'Unique Stations'" = org.stations,
                          "'NA obs'" = org.na.rows,
                          "'Unique Comments'" = org.comments,
                          check.names = FALSE)
    ifelse(i == 1, df.summary <- new.row, df.summary <- rbind(df.summary, 
                                                              new.row))
    df.summary <- arrange(df.summary,desc(Observations))
  }
  return(df.summary)
}

summarizeByStation <- function(df.all) {
  #Provides totals by analyte by station. The intent of this is to assist in 
  #identifying the stations with continuous data that may have 7DADMs for Temp.
  #Arguments:
  #df.all = data frame of query results
  #Returns:
  #df.totals = data.frame with stations as rows and number of results as values
  #            and analytes as columns
  df.totals <- as.data.frame.matrix(table(df.all$Station_ID, 
                                          df.all$Analyte))
  df.totals <- cbind(rownames(df.totals), df.totals)
  df.totals <- plyr::rename(df.totals, c("rownames(df.totals)" = 'Station_ID'))
  rownames(df.totals) <- 1:nrow(df.totals)
  return(df.totals)
}

generateStnLyrToPlot <- function(df.all, df.totals) {
  #Takes the data returned from the query and the totals by analyte and station
  #and generates a spatial object for plotting with plotGoogleMaps
  #Arguments:
  #df.all = data frame of query results
  #df.totals = data frame of tabulated results by analyte and by station
  #Returns:
  #all.sp = spatial points dataframe
  all.sp <- merge(df.all[,c('Station_ID',
                            'Station_Description',
                            'DECIMAL_LAT',
                            'DECIMAL_LONG')], 
                  df.totals, 
                  by = 'Station_ID', all.x = TRUE)
  all.sp <- all.sp[!duplicated(all.sp$Station_ID),]
  coordinates(all.sp) = ~DECIMAL_LONG+DECIMAL_LAT
  proj4string(all.sp) <- CRS("+init=epsg:4269")
  return(all.sp)
}

extract_303d <- function (df.all, wq_limited, selectedPlanArea) {
  #Extracts 303(d) records for parameters returned in query and clipped to
  #selected plan area
  #Arguments:
  #df.all = data frame returned from clipToPlanArea
  #path_303d = relative path for shiny app to look for fname_303d
  #fname_303d = filename of 303d list shapefile
  
  #Bring in the shpaefile of the wq limited waters from the IR
  wq_limited <- wq_limited[wq_limited$Pollutant %in% 
                             unique(df.all$Analyte) & 
                             wq_limited$PlanName == selectedPlanArea,]
  
  return(wq_limited)
}

pickReviewDf <- function(input_reviewDf, lstSummaryDfs, df.all) {
  reviewDf <- switch(input_reviewDf,
                     "df.org" = (
                       lstSummaryDfs[["df.org"]]
                     ),
                     "df.report" = (
                       lstSummaryDfs[["df.report"]]
                     ),
                     "df.comment" = (
                       out <- as.data.frame(table(df.all[,'Comment'],useNA='always'))
                     ),
                     "df.removal" = ({
                       df.ex <- lstSummaryDfs[["df.removal"]]
                       if(nrow(df.ex) > 0) {
                         out <- plyr::rename(as.data.frame(table(df.ex[,'Reason'])), 
                                       c('Var1' = "'Reason for removal'", 
                                         'Freq' = "'Number of observations removed'"))
                       }
                       else {
                         out <- data.frame("Message" = "All data met QC requirments")
                       }
                       }),
                     "df.station.totals" = (
                       lstSummaryDfs[["df.station.totals"]]
                     ),
                     "df.sub" = ({
                       out <- data.frame(lapply(df.all, 
                                                FUN = function(x) {
                                                  if (all(class(x) == 'character')) {
                                                    x <- factor(x)
                                                  } else {
                                                    x
                                                  }
                                                }
                       )
                       )
                       out$Station_ID <- factor(out$Station_ID)
                       out
                     }),
                     "wq_limited" = (
                       lstSummaryDfs[["wq_limited"]]                                                
                     )      
  )
  return(reviewDf)
}

generate_new_data <- function(df.all, sdadm, selectStation, selectParameter,
                              selectUse = NULL, selectSpawning= NULL) {
  if (selectParameter == 'Temperature') {
    df.sub <- sdadm[sdadm$Station_ID == unique(strsplit(
      selectStation, ' - ')[[1]][1]),]
  } else {
    df.sub <- df.all[df.all$Station_ID == unique(strsplit(
      selectStation, ' - ')[[1]][1]) & df.all$Analyte == selectParameter,]
  }
  return(df.sub)
}

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
    sdadm.melt$id <- gsub(".","-",sdadm.melt$id,fixed=TRUE)
    colnames(sdadm.melt)[2] <- station_column_name
    sdadm.melt$date <- as.Date(sdadm.melt$date)
    return(sdadm.melt)
  }
}

EvaluateTempWQS <- function(sdadm_df, selectUse, selectSpawning, station_column_name) {
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
  sdadm_df$spwn_dates <- selectSpawning
  sdadm_df$ben_use_des <- selectUse
  stations <- unique(sdadm_df[,station_column_name])
  spd <- unique(sdadm_df[,c('Station_ID','spwn_dates','ben_use_des')])
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
  sdadm_df <- sdadm_df[!is.na(sdadm_df$sdadm),]
  
  sdadm_df$Time_Period <- ifelse(sdadm_df$summer, "Summer", "Spawning")
  sdadm_df$Time_Period <- factor(sdadm_df$Time_Period, levels = c('Summer', 'Spawning', 'Total'))
  sdadm_df$exceed <- sdadm_df$exceedspawn | sdadm_df$exceedsummer
  result_summary <- ddply(sdadm_df, .(sdadm_df[, station_column_name], Time_Period), 
                                            summarise, 
                                            Exceedances = sum(exceed),                  
                          #exceedspawn = sum(exceedspawn),
                                            #exceedsummer = sum(exceedsummer),
                                            Obs = sum(daystot), .drop = FALSE)
  result_summary <- plyr::rename(result_summary, 
                                 c('sdadm_df[, station_column_name]' = 
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

EvaluatepHWQS <- function(new_data, ph_crit, PlanName, selectpHCrit = NULL) {
  if (is.null(selectpHCrit)) {
    return("Please indicate the selected pH criteria")
  } else {
    OWRD_basin <- strsplit(selectpHCrit, " - ")[[1]][1]
    crit_selected <- strsplit(selectpHCrit, " - ")[[1]][2]
    ph_crit_min <- ph_crit[ph_crit$ph_standard == crit_selected & 
                             ph_crit$OWRD_basin == OWRD_basin & 
                             ph_crit$plan_name == PlanName, 
                           'ph_low']
    ph_crit_max <- ph_crit[ph_crit$ph_standard == crit_selected &
                             ph_crit$OWRD_basin == OWRD_basin & 
                             ph_crit$plan_name == PlanName, 
                           'ph_high']
    new_data$exceed <- ifelse(new_data[, 'Result'] < ph_crit_min |
                                new_data[, 'Result'] > ph_crit_max, 
                              1, 0)
    new_data$Year <- as.character(years(new_data$Sampled))
    return(new_data)
  }
}

EvaluateEColiWQS <- function(new_data) {
  new_data$exceed <- ifelse(new_data[, 'Result'] > 406, 1, 0)
  ecoli_gm_eval <- gm_mean_30_day(new_data, 
                                  unique(new_data$Analyte), 
                                  unique(new_data$Station_ID))
  ecoli_gm_eval$exceed <- ifelse(ecoli_gm_eval$gm > 126, 1, 0)
  ex_df <- data.frame("Station_ID" = rep(unique(
    new_data$Station_ID),2),
    "Station_Description" = rep(unique(
      new_data$Station_Description), 2),
    "Sample" = c('Single sample', 'Geomean'),
    "Obs" = c(nrow(new_data),
              nrow(ecoli_gm_eval)),
    "Exceedances" = c(sum(new_data$exceed),
                      sum(ecoli_gm_eval$exceed))
  )
  attr(new_data, "ecoli_gm_eval") <- ecoli_gm_eval
  attr(new_data, "ex_df") <- ex_df
  return(new_data)
}

EvaluateEnteroWQS <- function(new_data) {
  new_data$exceed <- ifelse(new_data[, 'Result'] > 158, 1, 0)
  entero_gm_eval <- gm_mean_30_day(new_data, 
                                   unique(new_data$Analyte), 
                                   unique(new_data$Station_ID))
  entero_gm_eval$exceed <- ifelse(entero_gm_eval$gm > 35, 1, 0)
  ex_df <- data.frame("Station_ID" = rep(unique(new_data$Station_ID),2),
                      "Station_Description" = rep(unique(
                        new_data$Station_Description), 2),
                      "Sample" = c('Single sample', 'Geomean'),
                      "Obs" = c(nrow(new_data),
                                nrow(entero_gm_eval)),
                      "Exceedances" = c(sum(new_data$exceed),
                                        sum(entero_gm_eval$exceed))
  )
  attr(new_data, "entero_gm_eval") <- entero_gm_eval
  attr(new_data, "ex_df") <- ex_df
  return(new_data)
}

generate_exceed_df <- function(new_data, parm, selectpHCrit, ph_crit, PlanName, 
                               selectStation, selectUse, selectSpawning,
                               station_column_name = 'Station_ID') {
  exceed_df <- switch(
    parm,
    "pH" = ({
      if (is.null(selectpHCrit)) {
        NULL
      } else {
        new_data <- EvaluatepHWQS(new_data, ph_crit, PlanName, selectpHCrit)
        ddply(new_data, .(Station_ID, Station_Description, Year), #, Month), 
              summarize, Obs = length(exceed), Exceedances = sum(exceed)) 
      }
    }),
    "Temperature" = ({
      if (is.null(selectSpawning)) {
        NULL 
      } else {
        if (any(!is.na(new_data$sdadm))) {
          new_data <- EvaluateTempWQS(new_data, selectUse, selectSpawning, "Station_ID") 
        } else {
          new_data <- "Insufficient data to calculate a single 7DADM"
        }
        
        if (is.data.frame(new_data)) {
          ex_df <- attr(new_data, "result_summary")
        } else {
          ex_df <- data.frame()
        }
      }
    }),
    "E. Coli" = ({
      new_data <- EvaluateEColiWQS(new_data)  
      attr(new_data, "ex_df")
    }),
    "Enterococcus" = ({
      new_data <- EvaluateEnteroWQS(new_data)
      attr(new_data, "ex_df")
    })
  )
  exceed_df$Percent_Exceed <- exceed_df$Exceedances/exceed_df$Obs * 100
  exceed_df
}

#From SO: http://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
gm_mean = function(x, na.rm=TRUE, zero.propagate = TRUE){
  if(any(x < 0, na.rm = TRUE)){
    return(NaN)
  }
  if(zero.propagate){
    if(any(x == 0, na.rm = TRUE)){
      return(0)
    }
    exp(mean(log(x), na.rm = na.rm))
  } else {
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  }
}

fc2ec <- function(fc) {
  ec <- 0.531*fc^1.06
  return(ec)
}

gm_mean_30_day <- function(df, parameter, station) {
  sub <- df[df$Analyte == parameter &
              df$Station_ID == station,]
  
  sub_start <- df[df$Analyte == parameter,]
  
  gm_df <- data.frame()
  for (i in 1:length(unique(sub_start$Station_ID))) {
    sub <- sub_start[sub_start$Station_ID == unique(sub_start$Station_ID)[i],]
    
    sort(sub[,'Sampled'])
    
    sub$day <- as.Date(sub$Sampled, format = "%Y-%m-%d")
    
    if ((as.Date(max(sub$Sampled)) - as.Date(min(sub$Sampled)) < 30)) {
      day <- as.Date((seq(min(sub$Sampled),min(sub$Sampled) + 29*24*60*60,by=86400)), format = "%Y-%m-%d")
    } else {
      day <- as.Date((seq(min(sub$Sampled),max(sub$Sampled),by=86400)), format = "%Y-%m-%d")
    }
    
    Result <- rep(NA, length(day))
    sub_long <- rbind(sub[,c('day','Result')], data.frame(day, Result))
    
    sub_long_max <- aggregate(sub_long, by = list(sub_long$day), FUN = function(x) {if(all(is.na(x))) {
      NA
    } else {
      max(x, na.rm = TRUE)}
    })
    
    sub_long_max$n <- ifelse(is.na(sub_long_max$Result), 0, 1)
    
    sub_long_max$ind <- rownames(sub_long_max)
    
    if (nrow(sub_long_max) > 1) {
      obs_in_30 <- rollapplyr(sub_long_max$n, width = 30, FUN = sum)
      
      #print(paste(any(obs_in_30 >= 5),unique(sub[,'Station_ID'])))
      
      
      
      geo_mean_30 <- rollapplyr(sub_long_max$Result, width = 30, FUN = gm_mean)
      
      gm_df_all <- data.frame("day" = sub_long_max[30:nrow(sub_long_max),'day'],
                              "n" = obs_in_30,
                              "gm" = geo_mean_30,
                              "ind" = rep(NA, length(geo_mean_30)))
      
      for (j in 1:nrow(gm_df_all)) {
        sub_sub <- sub_long_max[(which(sub_long_max$day == gm_df_all[j,'day'])-29):which(sub_long_max$day == gm_df_all[j,'day']),]
        gm_df_all[j,'ind'] <- paste(sub_sub[which(sub_sub$Result != 0),'ind'],collapse = ",")
      }
      
      gm_df_min5 <- gm_df_all[gm_df_all$n >= 5,]
      
      gm_df_5_first <- gm_df_min5[!duplicated(gm_df_min5$ind),]
      
      if (nrow(gm_df_5_first) > 0) {
        gm_df_5_first$id <- unique(sub_start$Station_ID)[i]
        
        gm_df <-  rbind(gm_df, gm_df_5_first)
      }
    } 
  }
  
  if ("ind" %in% names(gm_df)) {
    gm_df <- within(gm_df, rm(ind))
  }
  
  return(gm_df)
}

