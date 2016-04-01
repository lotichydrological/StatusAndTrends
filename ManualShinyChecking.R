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
library(ggplot2)
#library(xlsx)
#library(RODBC)

options(stringsAsFactors = FALSE)

source('app/functions/01_DataQuery.R')
source('app/functions/funClean.R')
source('app/functions/funPlots.R')
source('app/functions/funSeaKen.R')
source('app/functions/funHelpers.R')

agwqma <- readOGR(dsn = 'app/data/GIS', layer = 'ODA_AgWQMA', verbose = FALSE)
#agwqma <- spTransform(agwqma, CRS("+proj=longlat +datum=NAD83"))
HUClist <- read.csv('app/data/PlanHUC_LU.csv')
ph_crit <- read.csv('app/data/PlanOWRDBasinpH_LU.csv')
parms <- read.csv('app/data/WQP_Table3040_Names.csv', stringsAsFactors = FALSE)
wq_limited <- read.csv('app/data/wq_limited_df_temp_bact_ph.csv')
#wq_limited <- readOGR(dsn = 'app/data/GIS', layer = 'ORStreamsWaterQuality_2010_WQLimited_V3', verbose = FALSE)

#For app purposes set up input 
input <- list(action_button = c(0))
input$action_button <- 1
input$parms <- c('Temperature')
input$select <- 'North Coast'
input$dates <- c("2006-01-01", "2015-12-31")
input$db <- c("DEQ")
input$selectStation <-  "33312 - "
input$selectParameter <- 'Temperature'
input$selectLogScale <- TRUE
input$selectSpawning <- 'January 1-June 15'
input$selectUse <- 'Cool water species'
input$selectpHCrit <- 'Powder - All other basin waters'
input$plotTrend <- FALSE

wqpData <- NULL
lasarData <- NULL
elmData <- NULL
nwisData <- NULL
df.all <- NULL
lstSummaryDfs <- list()
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
  
  if (any(c('Temperature', 'pH') %in% input$parms)) {
    nwisData <- tryCatch(nwisQuery(planArea = input$select,
                                   HUClist = HUClist,
                                   inParms = input$parms,
                                   startDate = input$dates[1],
                                   endDate = input$dates[2]),
                         error = function(err) {err <- geterrmessage()})
  }
  
  if (is.null(wqpData) & is.null(nwisData)) {
    wqp_message <- 'Your query returned no results from the Water Quality Portal.'
  } else if (!is.data.frame(wqpData) & !is.null(wqpData)) {
    if (grepl("307", wqpData)) {
      wqp_message <- 'Water Quality Portal is busy. Please try again in a few minutes.'
    }
  }
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
}

#         if (is.null(df.all)) {
#           df.all <- 'Your query returned no data'
#         }

  #### Tabulate Results ####
  #Isolate to only include data at stations in the plan area polygon
  df.all <- clipToPlanArea(df.all, agwqma, input$select)
  
  #Summarize Stations and Number of results by Analyte
  all.totals <- tabulateResults(df.all)
  
  #Summarize results by organization
  lstSummaryDfs[[1]] <- summarizeByOrg(df.all)
  names(lstSummaryDfs)[1] <- "df.org"
  
  #Tabulate number of results at each station
  lstSummaryDfs[[2]] <- summarizeByStation(df.all)
  names(lstSummaryDfs)[2] <- "df.station.totals"
  
  #### Cleaning result field ####
  #Fix non-numeric results in the Result field
  df.all$Result <- clean(df.all$Result)
  lstSummaryDfs[[3]] <- attr(df.all$Result, "report")
  names(lstSummaryDfs)[3] <- "df.report"
  df.all$Result <- suppressWarnings(as.numeric(df.all$Result))
  
  #MRL handling
  df.all <- MRLhandling(df.all)
  
  #Fecal coliform to e. coli conversion
  if ("Fecal Coliform" %in% df.all$Analyte) {
    df.all <- update_fc2ec(df.all)
  }
  
  #### Calculate trends and adnl data for plotting ####
  #Generate sdadm once for temperature plotting/exceedance use
  if (any('Temperature' %in% df.all$Analyte)) {
    sdadm <- Calculate.sdadm(df.all, "Result", "Station_ID", "Sampled",
                             '%Y-%m-%d %H:%M:%S')
  } else {
    sdadm <- NULL
  }
  
  #Run Seasonal Kendall for pH and Bacteria
  if (any(c('pH', 'E. Coli', "Enterococcus") %in% df.all$Analyte)) {
    SeaKen <- run_seaKen(df.all)
  } else {
    SeaKen <- NULL
  }
  
  #### Performing QA Screen ####
  #Check QA info and remove data not meeting QA objectives
  df.all <- remove_QAfail(df.all)
  #Pull out the tracking data frame of data removed
  lstSummaryDfs[[4]] <- attr(df.all, "removal_tracking")
  names(lstSummaryDfs)[4] <- "df.removal"
  
  #### Preparing data for mapping ####
  #Generate layer for mapping
  all.sp <- generateStnLyrToPlot(df.all, lstSummaryDfs[["df.station.totals"]])
  
  #Restrict ag plan areas to select plan area
  ag_sub <- agwqma[agwqma$PlanName == input$select,]
  ag_sub <- spTransform(ag_sub, CRS("+init=epsg:4269"))
  
  #Restrict layer for mapping to just the selected plan area
  all.sp <- all.sp[ag_sub,]
  
  #Extract 303(d) segments in the plan area for parameters
  #returned in the query
  lstSummaryDfs[[5]] <- extract_303d(df.all, wq_limited, input$select)
  names(lstSummaryDfs)[5] <- "wq_limited"

  new_data <- generate_new_data(df.all, sdadm, input$selectStation, input$selectParameter,
                    input$selectUse, input$selectSpawning)
  
  generate_exceed_df(new_data, input$selectParameter, input$selectpHCrit,
                     ph_crit, input$select, input$selectStation)

  sea_ken_table <- SeaKen
  plot_trend <- input$plotTrend
  plot_criteria <- input$selectpHCrit
  plan_area <- input$select
  
  plot.ph(new_data = new_data, 
          sea_ken_table = SeaKen,  
          ph_crit,
          plot_trend = input$plotTrend,
          plot_criteria = input$selectpHCrit,
          plan_area = input$select)