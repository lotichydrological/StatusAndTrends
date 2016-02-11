# library(dplyr)
# library(lubridate)
# checkObservations <- function(station, date, value){
#   data_frame(station, date, value) %>%
#     group_by_(~station) %>%
#     summarise(n = n(),
#               is.regular = length(unique(diff(date))) < 2) }
# 
# # Test it out
# dates <- seq(Sys.time() - dweeks(20), Sys.time(), by = '1 hour') 
# df <- expand.grid(station = 1:10, date = dates) 
# df$temp <- runif(nrow(df))
# 
# checkObservations(df$station, df$date, df$temp)
# 
# df2 <- df %>% sample_frac(0.6)
# checkObservations(df2$station, df2$date, df2$temp)



library(shiny)
library(RCurl)
library(XML)
library(dataRetrieval)
library(plyr)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(plotGoogleMaps)
library(DT)
library(wq)
library(chron)
library(reshape)
#library(xlsx)
#library(RODBC)

options(stringsAsFactors = FALSE)

source('app/functions/01_DataQuery.R')
source('app/functions/funClean.R')
source('app/functions/funEvaluateBacteria.R')
source('app/functions/funPlots.R')
source('app/functions/funSeaKen.R')

agwqma <- readOGR(dsn = 'app/data/GIS', layer = 'ODA_AgWQMA', verbose = FALSE)
agwqma <- spTransform(agwqma, CRS("+proj=longlat +datum=NAD83"))
HUClist <- read.csv('app/data/PlanHUC_LU.csv')
ph_crit <- read.csv('app/data/PlanOWRDBasinpH_LU.csv')
parms <- read.csv('app/data/WQP_Table3040_Names.csv', stringsAsFactors = FALSE)
wq_limited <- readOGR(dsn = 'app/data/GIS', layer = 'ORStreamsWaterQuality_2010_WQLimited_V3', verbose = FALSE)

#For app purposes set up input 
input <- list(action_button = c(0))
input$action_button <- 1
input$parms <- c('Temperature')
input$select <- 'North Coast'
input$dates <- c("2000-01-01", "2015-11-16")
input$db <- c("DEQ")
input$selectStation <-  "13442 - Tillamook River at Yellow Fir Road"#"13430 - Hoquarten Slough at Hwy 101 (Tillamook)"
input$selectParameter <- 'Temperature'
input$selectLogScale <- TRUE
input$selectSpawning <- 'No spawning'
input$selectUse <- 'Core Cold Water Habitat'
input$selectpHCrit <- 'North Coast - All other basin waters'
input$selectRange <- (c(as.Date(strptime(input$dates[1], format = "%Y-%m-%d")), 
                                                       as.Date(strptime(input$dates[2], format = "%Y-%m-%d"))))
input$plotTrend <- FALSE

wqpData <- NULL
lasarData <- NULL
elmData <- NULL
nwisData <- NULL
prog <- 0
wqp_message <- ""

if ('Water Quality Portal' %in% input$db) {
  wqpData <- tryCatch(wqpQuery(planArea = input$select,
                               HUClist = HUClist,
                               inParms = input$parms,
                               luParms = parms,
                               startDate = input$dates[1],
                               endDate = input$dates[2]),
                      error = function(err) {err <- geterrmessage()})
  
  if (!is.data.frame(wqpData)) {
    if (wqpData == "No data") {
      df.all <- 'Your query returned no results from the Water Quality Portal.'
      wqp_message <- df.all
    } else {
      df.all <- 'Water Quality Portal is busy. Please try again in a few minutes.'
      wqp_message <- df.all 
    }
  }
  
  nwisData <- tryCatch(nwisQuery(planArea = input$select,
                                 HUClist = HUClist,
                                 inParms = input$parms,
                                 startDate = input$dates[1],
                                 endDate = input$dates[2]),
                       error = function(err) {err <- geterrmessage()})
}

if ('DEQ' %in% input$db) {
  lasarData <- lasarQuery(planArea = input$select,
                          HUClist = HUClist,
                          inParms = input$parms,
                          startDate = input$dates[1],
                          endDate = input$dates[2])
  odbcCloseAll()
  if (nrow(lasarData) == 0) lasarData <- NULL
  
  elmData <- elementQuery(planArea = input$select,
                          HUClist = HUClist,
                          inParms = input$parms,
                          startDate = input$dates[1],
                          endDate = input$dates[2])
  odbcCloseAll()
  if (nrow(elmData) == 0) elmData <- NULL
}


if (wqp_message != 'Water Quality Portal is busy. Please try again in a few minutes.') {
  df.all <- tryCatch(combine(E=elmData,L=lasarData,W=wqpData,N=nwisData),
                     error = function(err) 
                     {err <- geterrmessage()})
  
  if (is.null(df.all)) {
    df.all <- 'Your query returned no data'
  }
  
  if(!is.data.frame(df.all)) {
    output$text1 <- renderText({df.all})
  }
  else {
    all.sp <- df.all[!duplicated(df.all$SD),c(3,1:2,4:17)]
    coordinates(all.sp) = ~DECIMAL_LONG+DECIMAL_LAT
    proj4string(all.sp) <- CRS("+init=epsg:4269")
    ag_sub <- agwqma[agwqma$PlanName == input$select,]
    #HUC_sub <- HUC[ag_sub,]
    ag_sub <- spTransform(ag_sub, CRS("+init=epsg:4269"))
    all.sp <- all.sp[ag_sub,]
    
    df.all <- df.all[df.all$Station_ID %in% all.sp@data$Station_ID,]
    
    wq_limited <- wq_limited[wq_limited$Pollutant %in% 
                               unique(df.all$Analyte),]
    wq_limited <- spTransform(wq_limited, CRS("+init=epsg:4269"))
    wq_limited <- tryCatch(wq_limited[ag_sub,],
                           error = function(err) {
                             data.frame("Message" =
                                          "No 303(d) listed segments for this parameter")})
    if(!is.data.frame(wq_limited)){
      wq_limited <- data.frame(lapply(wq_limited@data, factor)) 
    }
    
    all.totals <- ddply(df.all, 
                        .(Database), 
                        summarize, 
                        n_stations = length(unique(Station_ID)))
    n_samp <- as.data.frame.matrix(table(df.all$Database, df.all$Analyte))
    n_samp$Database <- row.names(n_samp)
    all.totals <- merge(all.totals, n_samp, by = 'Database')
    attr(df.all, "totals") <- all.totals
    
    
    df.all$Result <- clean(df.all$Result)
    df.report <- attr(df.all$Result, 'report')
    df.all[which(df.all$Result == 'ND'),'Detect'] <- 0 
    df.all[which(df.all$Result == 'ND'),'Result'] <- 
      df.all[which(df.all$Result == 'ND'),'MRL']
    
    
    df.ex <- df.all[df.all$Status %in% c('D','E','F'),]
    df.all <- df.all[!df.all$Status %in% c('D','E','F'),]
    if (nrow(df.ex) > 0) {
      df.ex$Reason <- "Sample Status equivalent to D, E or F"
    }
    
    df.all$MRL <- suppressWarnings(as.numeric(df.all$MRL))
    
    df.all[is.na(df.all$MRL),'MRL'] <- 0
    
    df.all$Detect <- ifelse(df.all$Result < df.all$MRL,0,1)
    df.all[which(df.all$Result < df.all$MRL),'Result'] <- 
      df.all[which(df.all$Result < df.all$MRL),'MRL']
    
    df.ex2 <- df.all[grep('Qualifier=C',df.all$Comment),]
    df.all <- df.all[!grepl('Qualifier=C | [Cc]ontamination | QCS FAILED',
                            df.all$Comment),]
    if (nrow(df.ex2) > 0) {
      df.ex2$Reason <- "Comment indicates sample contamination"
      df.ex <- rbind(df.ex, df.ex2)
      rm(df.ex2)
    }
    
    df.ex2 <- df.all[grep('Qualifier=Q',df.all$Comment),]
    df.all <- df.all[!grepl('Qualifier=Q',df.all$Comment),]
    if (nrow(df.ex2) > 0) {
      df.ex2$Reason <- "Comment indicates major QC issue"
      df.ex <- rbind(df.ex, df.ex2)
      rm(df.ex2)
    }
    
    df.ex2 <- df.all[is.na(df.all$Result),]
    df.all <- df.all[!is.na(df.all$Result),]
    if (nrow(df.ex2) > 0) {
      df.ex2$Reason <- "Result is Void, Cancelled or other NA"
      df.ex <- rbind(df.ex, df.ex2)
      rm(df.ex2)
    }
    df.all$Result <- suppressWarnings(as.numeric(df.all$Result))
    
    df.all[df.all$Analyte == 'Fecal Coliform','Result'] <- 
      round(fc2ec(df.all[df.all$Analyte == 'Fecal Coliform','Result']))
    df.all[df.all$Analyte == 'Fecal Coliform','Comment'] <- 
      ifelse(is.na(df.all[df.all$Analyte == 'Fecal Coliform','Comment']),
             "Fecal Coliform value converted to E. Coli",
             paste(df.all[df.all$Analyte == 'Fecal Coliform','Comment'],
                   "Fecal Coliform value converted to E. Coli",
                   sep = ", "))
    df.all[df.all$Analyte == 'Fecal Coliform','Analyte'] <- 'E. Coli'
    
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
    
    #           df.date <- data.frame('Sampled' = seq(
    #             as.POSIXct(strptime(input$dates[1], format = '%Y-%m-%d')),
    #             as.POSIXct(strptime(input$dates[2], format = '%Y-%m-%d')),
    #             by = 60*60*24)
    #           )
    
    df.totals <- as.data.frame.matrix(table(df.all$Station_ID, 
                                            df.all$Analyte))
    df.totals <- cbind(rownames(df.totals), df.totals)
    df.totals <- rename(df.totals, c("rownames(df.totals)" = 'Station_ID'))
    rownames(df.totals) <- 1:nrow(df.totals)
    
    all.sp <- merge(df.all[,c('Station_ID',
                              'Station_Description',
                              'DECIMAL_LAT',
                              'DECIMAL_LONG')], 
                    df.totals, 
                    by = 'Station_ID', all.x = TRUE)
    all.sp <- all.sp[!duplicated(all.sp$Station_ID),]
    coordinates(all.sp) = ~DECIMAL_LONG+DECIMAL_LAT
    proj4string(all.sp) <- CRS("+init=epsg:4269")
    
    output$text1 <- renderText({
      paste("You just submitted", 
            input$select, 
            "Plan Area Query for", 
            paste(input$parms,collapse=", "), 
            "from",
            input$dates[1], 
            "to", 
            input$dates[2])
    })
  }
}


# 
# 
# 
# 
# 
# 
# 

new_data <- Calculate.sdadm(spawning = 
                  input$selectSpawning,
                station = 
                  input$selectStation,
                use = input$selectUse,
                df.all = df.all,
                dates = input$selectRange)

# new_data$year <- years(new_data$Sampled)
# plot(as.Date(new_data$plotSampled), new_data$Result, col = c(3:(2 + length(levels(new_data$year)) - 1),1), pch = 19, xlim = c(as.Date('2000-01-01'),as.Date('2000-12-31')))
# legend('topright',legend = levels(new_data$year), col = c(3:(2 + length(levels(new_data$year)) - 1),1), pch = 19)