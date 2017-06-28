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
library(zoo)
library(spatialEco)
library(dplyr)
library(lubridate)
#library(xlsx)
#library(RODBC)

options(stringsAsFactors = FALSE)

source('app/functions/01_DataQuery.R')
source('app/functions/funClean.R')
source('app/functions/funPlots.R')
source('app/functions/funSeaKen.R')
source('app/functions/funHelpers.R')
#source('app/functions/directoryInput.R')

agwqma <- readOGR(dsn = 'app/data/GIS', layer = 'ODA_AgWQMA', verbose = FALSE)
hucs <- readOGR(dsn = 'app/data/GIS', layer = 'WBD_HU8', verbose = FALSE)
#agwqma <- spTransform(agwqma, CRS("+proj=longlat +datum=NAD83"))
HUClist <- read.csv('app/data/PlanHUC_LU.csv')
stations_huc <- read.csv('app/data/station_wbd_12132016.csv')
ph_crit <- read.csv('app/data/PlanOWRDBasinpH_LU.csv')
ph_crit <- merge(ph_crit, HUClist, by.x = 'plan_name', by.y = 'PlanName', all.x = TRUE)
parms <- read.csv('app/data/WQP_Table3040_Names.csv', stringsAsFactors = FALSE)
wq_limited <- read.csv('app/data/GIS/wq_limited_df_temp_bact_ph_DO_2012.csv')

#For app purposes set up input 
input <- list(action_button = c(0))
input$action_button <- 1
input$parms <- c('Bacteria')
input$select <- "Hood River"
input$dates <- c("2016-01-01", "2017-01-01")
input$db <- c('DEQ', 'Water Quality Portal')
input$selectStation <-  "10764 - "
input$selectParameter <- 'Total Phosphorus'
input$selectLogScale <- FALSE
input$selectSpawning <- 'No spawning'#'January 1-May 15'
input$selectUse <- 'Core Cold Water Habitat'
input$selectpHCrit <- 'Willamette - All other basin waters'#'John Day - All other basin waters'
input$plotTrend <- TRUE
input$selectUseDO<-'Cold-Water Aquatic Life'
input$checkSpawning<-TRUE
input$selectWQSTSS<- 0


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
  
  if (any(c('Temperature', 'pH', 'Dissolved Oxygen', 'Total Suspended Solids', 'Total Phosphorus') %in% input$parms)) {
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
                          endDate = input$dates[2],
                          stations_wbd = stations_huc)
  odbcCloseAll()
  if (nrow(lasarData) == 0) lasarData <- NULL
  

  elmData <- elementQuery(planArea = input$select,
                          HUClist = HUClist,
                          inParms = input$parms,
                          startDate = input$dates[1],
                          endDate = input$dates[2],
                          stations_wbd = stations_huc)
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
#temp_stns_pass <- temp_sufficiency_analysis(df.all = df.all)

#Run Seasonal Kendall for pH and Bacteria
if (any(c('pH', 'E. Coli', "Enterococcus", "Dissolved Oxygen", 'Total Suspended Solids', 'Total Phosphorus') %in% df.all$Analyte)) {
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

lstSummaryDfs[[8]] <- Stations_Status(df.all)
names(lstSummaryDfs)[8] <- "Stations_Status"

if(lstSummaryDfs[8] != 'No stations meet Status criteria') {
  lstSummaryDfs[[8]] <- lstSummaryDfs[[8]]
} else {
  as.data.frame('No stations meet Status criteria')
}

lstSummaryDfs[[9]] <- Stations_Trend(df.all)
names(lstSummaryDfs)[9] <- "Stations_Trend"

if(lstSummaryDfs[9] == "No Stations Meet Trend Criteria") {
  lstSummaryDfs[[9]] <- as.data.frame('No Stations Meet Trend Criteria')
} else {
  lstSummaryDfs[[9]] <- lstSummaryDfs[[9]]
}

lstSummaryDfs[[10]] <- All_stns_fit_Criteria(trend = lstSummaryDfs[[9]], 
                                             status = lstSummaryDfs[[8]],
                                             df.all = df.all)
names(lstSummaryDfs)[10] <- "stns"


library(PythonInR)
autodetectPython("C:/Python27/ArcGISx6410.3/python.exe")
pyConnect("C:/Python27/ArcGISx6410.3/python.exe")
# pyIsConnected()
Delineation(stns = lstSummaryDfs[[10]],
                         outPath = "T:/AgWQM/DataAnalysis/Watersheds")


# lstSummaryDfs[[7]] <- Stations_Status(df.all)
# names(lstSummaryDfs)[7] <- "Stations_Status"
# 
# lstSummaryDfs[[8]] <- Stations_Trend(df.all)
# names(lstSummaryDfs)[8] <- "Stations_Trend"
# 
# lstSummaryDfs[[9]] <- All_stns_fit_Criteria(trend = lstSummaryDfs[[8]], status = lstSummaryDfs[[7]], df.all = df.all)
# names(lstSummaryDfs)[9] <- "stns"

#Pull in Stream Cat data for NLCD 2011 land use
# stn_nlcd_df <- landUseAnalysis(all.sp, cats, NLCD2011)
# lstSummaryDfs[[7]] <- data.frame()#stn_nlcd_df
# names(lstSummaryDfs)[[7]] <- 'stn_nlcd_df'

# status<-Stations_Status(df.all)
# trend<-Stations_Trend(df.all)
# stns<-All_stns_fit_Criteria(status, trend)

  new_data <- generate_new_data(df.all, sdadm, input$selectStation, input$selectParameter,
                    input$selectUse, input$selectSpawning)
  
  # plot.Temperature(new_data = new_data,
  #                  all_data = df.all,
  #                  selectUse = input$selectUse,
  #                  selectSpawning = input$selectSpawning)
  # 
  if (input$selectParameter %in% c('pH', 'E. Coli', 'Enterococcus', 'Dissolved Oxygen', 'Total Phosphorus', 'Total Suspended Solids')) {
    tmp_df <- new_data
    tmp_df$day <- substr(tmp_df$Sampled, 1, 10)
    tmp_df$code <- paste(tmp_df$Station_ID, tmp_df$Analyte, tmp_df$day)
    sub <- with(tmp_df, resolveMRLs(code, Detect, Result))
    tmp_df_MRL <- tmp_df[sub,]
    if (input$selectParameter == 'Dissolved Oxygen'){
      tmp_df <- remove.dups(tmp_df, min)
    } else {
      tmp_df <- remove.dups(tmp_df_MRL, max)
    }
  } else {
    tmp_df <- new_data
  }
  # 
  # 
  exc<-generate_exceed_df(new_data = tmp_df,
                          df.all = df.all,
                     parm = input$selectParameter,
                     selectpHCrit = input$selectpHCrit,
                     ph_crit = ph_crit,
                     PlanName =  input$select,
                     selectStation =  input$selectStation,
                     selectSpawning = input$selectSpawning,
                     selectUse = input$selectUse,
                     selectUseDO = input$selectUseDO,
                     selectWQSTSS = input$selectWQSTSS,
                     selectWQSTP = input$selectWQSTP)
  
  
  plot.TSS<-plot.TSS(new_data = new_data,
                     df.all = df.all,
                     selectWQSTSS = 5,
                     sea_ken_table = SeaKen,
                     plot_trend = TRUE,
                     analyte_column = 'Analyte',
                     station_id_column = 'Station_ID',
                     station_desc_column = 'Station_Description',
                     datetime_column = 'Sampled',
                     result_column = 'Result',
                     datetime_format = '%Y-%m-%d %H:%M:%S',
                     parm = 'Total Suspended Solids (mg/l)')
  plot.TSS

  
  
  plot.TP<-plot.TP(new_data = new_data,
                     df.all = df.all,
                     selectWQSTP = input$selectWQSTP,
                     sea_ken_table = SeaKen,
                     plot_trend = input$plotTrend,
                     analyte_column = 'Analyte',
                     station_id_column = 'Station_ID',
                     station_desc_column = 'Station_Description',
                     datetime_column = 'Sampled',
                     result_column = 'Result',
                     datetime_format = '%Y-%m-%d %H:%M:%S',
                     parm = 'Total Phosphorus (mg/l)')
  
  plot.TP
  
  
  # sea_ken_table <- SeaKen
  # plot_trend <- input$plotTrend
  # plot_criteria <- input$selectpHCrit
  # plan_area <- input$select
  # 
  # new_data$Sampled <- as.POSIXct(strptime(new_data$Sampled, format = "%Y-%m-%d %H:%M:%S"))
  plot.bacteria(new_data = new_data,
                     sea_ken_table = SeaKen,
                     plot_trend = input$plotTrend,
                     plot_log = input$selectLogScale,
                     parm = 'E. Coli')
  # 
  # plot.ph(new_data = new_data,
  #         sea_ken_table = SeaKen,
  #         ph_crit,
  #         plot_trend = input$plotTrend,
  #         plot_criteria = input$selectpHCrit,
  #         plan_area = input$select)
  # 
  # 
  # new_data_temp <- generate_temp_data(new_data = new_data, selectSpawning = input$selectSpawning,
  #                    selectUse = input$selectUse, selectMonth = "January")
  # 
  # Temp_trends_plot(new_data_temp, input$selectStation, input$selectMonth)
  # 
  # plot.Temperature(new_data = sdadm,
  #                  all_data = df.all,
  #                  selectUse = input$selectUse,
  #                  selectSpawning = input$selectSpawning,
  #                  station_id_column = 'Station_ID',
  #                  station_desc_column = 'Station_Description',
  #                  datetime_column = 'date',
  #                  datetime_format = '%Y-%m-%d',
  #                  plot_trend = FALSE)
  
  

  # input$selectStation <-  'USGS-421015121471800 - '
  # selectSpawning <- 'No spawning'
  # input$selectSpawning <- selectSpawning
  # selectUseDO<-'Cool-Water Aquatic Life'
  # input$selectUseDO<-selectUseDO
  # 
  # DO<-df.all%>%
  #   filter(Station_ID == input$selectStation, Analyte == "Dissolved Oxygen")
  # 
  DO <- generate_new_data(df.all, 
                    sdadm, 
                    input$selectStation, 
                    input$selectParameter)
  
  DO_exc<-EvaluateDOWQS(new_data = DO, 
                          df.all = df.all, 
                          selectUseDO = input$selectUseDO,
                          selectSpawning = input$selectSpawning,
                          analyte_column = 'Analyte',
                          station_id_column = 'Station_ID',
                          station_desc_column = 'Station_Description',
                          datetime_column = 'Sampled',
                          result_column = 'Result',
                          datetime_format = '%Y-%m-%d %H:%M:%S')
  
  table(DO_exc$exceed)
 
  
  
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
  
  #ggsave("g.png", height = 6, width = 6)
  
  ####prism#############################################
# library(devtools)
# install_github(repo = 'prism', username = 'ropensci')
# library(prism)
#   
# get_prism_monthlys(type = 'ppt', year = 2000:2017, mon = 1:12)
# p<-prism_slice(c(-123.7224,44.44074), ls_prism_data()[,1])
# p+ggtitle('Monthy Precipitation Totals for the South Santiam AgWQ Management Area')
# #ggsave('T:/AgWQM/DataAnalysis/South Santiam/Precipitation.png', height = 7, width = 9)
  # new_data <- ecoli
  # 
  # new_data$exceed <- as.vector(ifelse(new_data[, 'Result'] > 130, 1, 0))
  # ss_ex_df <- new_data %>%
  #   group_by(Station_ID, Station_Description, year) %>%
  #   summarise(Sample = 'Single Sample',
  #             Obs = n(),
  #             Exceedances = sum(exceed))
  # 
  # entero_gm_eval <- gm_mean_90_day(new_data, 
  #                                  unique(new_data$Analyte), 
  #                                  unique(new_data$Station_ID))
  # entero_gm_eval$year <- year(entero_gm_eval$day)
  # if (nrow(entero_gm_eval) > 0) {
  #   entero_gm_eval$exceed <- ifelse(entero_gm_eval$gm > 35, 1, 0)
  #   gm_ex_df <- entero_gm_eval %>% 
  #     group_by(id, year) %>%
  #     summarise(Sample = 'Geometric Mean',
  #               Obs = n(),
  #               Exceedances = sum(exceed)) %>%
  #     rename(Station_ID = id)
  #   
  #   gm_ex_df <- merge(gm_ex_df, 
  #                     unique(new_data[,c('Station_ID', 'Station_Description')]), 
  #                     by = 'Station_ID', 
  #                     all.x = TRUE)
  # } else {
  #   gm_ex_df <- ss_ex_df
  #   gm_ex_df$Obs <- 0
  #   gm_ex_df$Exceedances <- 0
  # }
  # 
  # ex_df <- rbind(as.data.frame(ss_ex_df), as.data.frame(gm_ex_df)) %>% arrange(Station_ID)
  # attr(new_data, "entero_gm_eval") <- entero_gm_eval
  # attr(new_data, "ex_df") <- ex_df
  # 
  # 
  # mydata<-df.all
  # 
  # mydata$year <- as.numeric(format(mydata$Sampled, format = "%Y"))
  # entero <- mydata[mydata$Analyte == 'Enterococcus',]
  # ecoli <- mydata[mydata$Analyte == 'E. Coli',]
  # 
  # ecoli <- EvaluateEColiWQS(ecoli)
  # ecoli_eval <- attr(ecoli, "ex_df")
  # #write.csv(ecoli_eval, 'ecoli_evaluated.csv', row.names = FALSE)
  # 
  # entero <- EvaluateEnteroWQS(entero)
  # entero_eval <- attr(entero, "ex_df")
  # #write.csv(entero_eval, 'entero_evaluated.csv', row.names = FALSE)
  # 
  # trend_summary <- mydata %>% dplyr::group_by(Station_ID, year) %>% 
  #   summarise(n = n()) %>% spread(year, n)
  # 
  # #Outout data summary to csv
  # #write.csv(x = trend_summary, file = 'data_summary.csv', row.names = FALSE)
  # 
  # #Ths is where the seasonal kendall analysis is run
  # results_seaken_ecoli <- run_seaKen(ecoli)
  # results_seaken_entero <- run_seaKen(entero)
  # results_seaken <- rbind(results_seaken_ecoli, results_seaken_entero)
  # stns <- unique(results_seaken$Station_ID)
  # for (i in 1:length(stns)) {
  #   mydata_sub <- mydata[mydata$Station_ID == stns[i],]
  #   
  #   if (!grepl("Not Significant", results_seaken[results_seaken$Station_ID == stns[i],'signif'])) {
  #     trend = TRUE
  #   } else {
  #     trend = FALSE
  #   }
  #   
  #   setwd("C:/Users/MRubens/Desktop/plots_test")
  #   
  #   b <- plot.bacteria(new_data=mydata_sub,
  #                      sea_ken_table=results_seaken ,
  #                      plot_trend = trend,
  #                      plot_log = FALSE,
  #                      parm = unique(mydata_sub$Analyte))
  #   
  #   fname <- paste(stns[i], "trend_plot_untran.jpg", sep = "-")
  #   ggsave(b, file = fname, height = 8, width = 8, device = 'jpeg')
  #   
  #   b <- plot.bacteria(new_data=mydata_sub,
  #                      sea_ken_table=results_seaken ,
  #                      plot_trend = FALSE,
  #                      plot_log = TRUE,
  #                      parm = unique(mydata_sub$Analyte))
  #   
  #   
  #   fname <- paste(stns[i], "trend_plot_log.jpg", sep = "-")
  #   ggsave(b, file = fname, height = 8, width = 8, device = 'jpeg')
  # }
  
  

  