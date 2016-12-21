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

#write.csv(df.all, 'df.all.csv')

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
hucs <- readOGR(dsn = 'app/data/GIS', layer = 'WBD_HU8', verbose = FALSE)
#agwqma <- spTransform(agwqma, CRS("+proj=longlat +datum=NAD83"))
HUClist <- read.csv('app/data/PlanHUC_LU.csv')
ph_crit <- read.csv('app/data/PlanOWRDBasinpH_LU.csv')
ph_crit <- merge(ph_crit, HUClist, by.x = 'plan_name', by.y = 'PlanName', all.x = TRUE)
parms <- read.csv('app/data/WQP_Table3040_Names.csv', stringsAsFactors = FALSE)
wq_limited <- read.csv('app/data/GIS/wq_limited_df_temp_bact_ph_DO3.csv')
#wq_limited <- readOGR(dsn = 'app/data/GIS', layer = 'ORStreamsWaterQuality_2010_WQLimited_V3', verbose = FALSE)

#For app purposes set up input 
input <- list(action_button = c(0))
input$action_button <- 1
input$parms <- c('Dissolved Oxygen')
input$select <- "Lower Willamette"
input$dates <- c("2000-01-01", "2016-09-23")
input$db <- c("Water Quality Portal")
input$selectStation <-  "10332"
input$selectParameter <- 'Dissolved Oxygen'
input$selectLogScale <- FALSE
input$selectSpawning <- 'October 15-May 15'
input$selectUse <- 'Salmon and Trout Rearing and Migration'
input$selectpHCrit <- 'Deschutes - All other basin waters'#'John Day - All other basin waters'
input$plotTrend <- TRUE
input$selectUseDO<-'Cold-Water Aquatic Life'
input$checkSpawning<-TRUE


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
  
  if (any(c('Temperature', 'pH', 'Dissolved Oxygen') %in% input$parms)) {
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

#Perform sufficiency analysis for temperature
temp_stns_pass <- temp_sufficiency_analysis(df.all)

#Run Seasonal Kendall for pH and Bacteria
if (any(c('pH', 'E. Coli', "Enterococcus") %in% df.all$Analyte)) {
  SeaKen <- run_seaKen(df.all)
} else {
  SeaKen <- data.frame()
}
lstSummaryDfs[[4]] <- SeaKen
names(lstSummaryDfs)[4] <- "sea_ken_table"

#Calculate 30 GM for E. Coli and Enterococcus
#WIll get to this later. May need to go in plotting section.

#### Performing QA Screen ####
#Check QA info and remove data not meeting QA objectives
df.all <- remove_QAfail(df.all)
#Pull out the tracking data frame of data removed
lstSummaryDfs[[5]] <- attr(df.all, "removal_tracking")
names(lstSummaryDfs)[5] <- "df.removal"

#### Preparing data for mapping ####
#Generate layer for mapping
all.sp <- generateStnLyrToPlot(df.all, lstSummaryDfs[["df.station.totals"]])

#Restrict ag plan areas to select plan area
if (!grepl("[0-9].", input$select)) {
  ag_sub <- agwqma[agwqma$PlanName == input$select,]
  ag_sub <- spTransform(ag_sub, CRS("+init=epsg:4269"))
  
  #Restrict layer for mapping to just the selected plan area
  all.sp <- all.sp[ag_sub,]
} else {
  huc_sub <- hucs[hucs$HUC_8 == strsplit(input$select, 
                                         split = " - ")[[1]][1],]
  huc_sub <- spTransform(huc_sub, CRS("+init=epsg:4269"))
  
  #Restrict layer for mapping to just the selected plan area
  #all.sp <- all.sp[huc_sub,]
}

#Extract 303(d) segments in the plan area for parameters
#returned in the query
wq_lim_whole <- extract_303d(df.all, wq_limited, input$select)
lstSummaryDfs[[6]] <- wq_lim_whole[,c('Stream_Lak', 'LLID_Strea', 
                                      'Miles', 'Pollutant', 'Season', 
                                      'Assessme_1', 'Criteria', 
                                      'Listing_St')]
lstSummaryDfs[[6]] <- plyr::rename(lstSummaryDfs[[6]], 
                                   c('Stream_Lak' = 'Waterbody',
                                     'LLID_Strea' = 'LLID',
                                     'Assessme_1' = 'Year Assessed',
                                     'Listing_St' = 'Listing Status'))
names(lstSummaryDfs)[6] <- "wq_limited"


  new_data <- generate_new_data(df.all, sdadm, input$selectStation, input$selectParameter,
                    input$selectUse, input$selectSpawning)
  
  if (input$selectParameter %in% c('pH', 'E. Coli', 'Enterococcus')) {
    tmp_df <- new_data
    tmp_df$day <- substr(tmp_df$Sampled, 1, 10)
    tmp_df$code <- paste(tmp_df$Station_ID, tmp_df$Analyte, tmp_df$day)
    sub <- with(tmp_df, resolveMRLs(code, Detect, Result))
    tmp_df_MRL <- tmp_df[sub,]
    tmp_df <- remove.dups(tmp_df_MRL, max)
  } else {
    tmp_df <- new_data
  }
  
  generate_exceed_df(tmp_df, input$selectParameter, input$selectpHCrit,
                     ph_crit, input$select, input$selectStation)

  sea_ken_table <- SeaKen
  plot_trend <- input$plotTrend
  plot_criteria <- input$selectpHCrit
  plan_area <- input$select
  
  new_data$Sampled <- as.POSIXct(strptime(new_data$Sampled, format = "%Y-%m-%d %H:%M:%S"))
  plot.bacteria(new_data = new_data,
                     sea_ken_table = SeaKen,
                     plot_trend = input$plotTrend,
                     plot_log = input$selectLogScale,
                     parm = 'E. Coli')
  
  plot.ph(new_data = new_data, 
          sea_ken_table = SeaKen,  
          ph_crit,
          plot_trend = input$plotTrend,
          plot_criteria = input$selectpHCrit,
          plan_area = input$select)
  
  
  new_data_temp <- generate_temp_data(new_data = new_data, selectSpawning = input$selectSpawning,
                     selectUse = input$selectUse, selectMonth = "January")
  
  Temp_trends_plot(new_data_temp, input$selectStation, input$selectMonth)
  
  
  DO<-df.all%>%
    filter(Station_ID == input$selectStation, Analyte == "Dissolved Oxygen")
  
  plot.DO<-plot.DO(new_data = DO,
                   df.all = df.all,
                   selectUseDO = input$selectUseDO,
                   selectSpawning = input$selectSpawning,
                   analyte_column = 'Analyte',
                   station_id_column = 'Station_ID',
                   station_desc_column = 'Station_Description',
                   datetime_column = 'Sampled',
                   result_column = 'Result',
                   datetime_format = '%Y-%m-%d %H:%M:%S',
                   parm = 'Dissolved Oxygen')
  plot.DO
  
  ##
  DO_test<-df.all%>%
    filter(Analyte == "Dissolved Oxygen", Station_ID == '11321' )
  
  g <- ggplot(data = new_data_all, aes(x = Sampled, y = Result)) +
    geom_point(aes(color = new_data_all$Conc_Exceed)) +
    geom_point(data = BCsat, shape = 8)+
    scale_colour_manual(name = 'Key', values = c('pink', 'black')) +
    scale_fill_manual(name="Meet b/c %DO",values=BCsat) +
    geom_hline(data = d, aes(yintercept = y), linetype = "dashed", color = "red") +
    ggtitle(bquote(atop(.(title)))) +
    theme(legend.position = "top",
          legend.title = element_blank(),
          legend.direction = 'horizontal') +
    xlab(x.lab) +
    ylab(y.lab) +
    xlim(x.lim) +
    ylim(y.lim)
  g
  
  