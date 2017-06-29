combine <- function(E = NULL, L = NULL, W = NULL, N = NULL) {
 # E <- elmData
 # L <- lasarData
 # W <- wqpData
 # N <- nwisData

  if (is.data.frame(W)) {
    wqp.map <- c('MonitoringLocationIdentifier' = 'Station_ID',
                 'OrganizationFormalName' = 'Client',
                 'CharacteristicName' = 'Analyte',
                 #'ResultSampleFractionText' = 'Fraction',
                 'ResultMeasureValue' = 'Result',
                 'ResultMeasure.MeasureUnitCode' = 'Unit',
                 'MeasureQualifierCode' = 'Status',
                 'ActivityTypeCode' = 'SampleType',
                 'DetectionQuantitationLimitMeasure.MeasureValue' = 'MRL',
                 #'DetectionQuantitationLimitMeasure.MeasureUnitCode' = 'MRLUnit',
                 'MonitoringLocationName' = 'Station_Description',
                 #'ResultAnalyticalMethod.MethodIdentifier' = 'SpecificMethod',
                 'LatitudeMeasure' = 'DECIMAL_LAT',
                 'LongitudeMeasure' = 'DECIMAL_LONG',
                 'HorizontalCoordinateReferenceSystemDatumName' = 'DATUM',
                 'ResultDetectionConditionText' = 'Detect',
                 'ResultCommentText' = 'Comment',
                 'ResultStatusIdentifier' = 'StatusIdentifier',
                 'HUCEightDigitCode' = 'HUC'
    )
    Wx <- attr(W, 'siteInfo')
    W <- merge(W, Wx[,c('MonitoringLocationIdentifier',
                        "MonitoringLocationName",
                        'LatitudeMeasure',
                        'LongitudeMeasure',
                        'HorizontalCoordinateReferenceSystemDatumName',
                        'HUCEightDigitCode')], 
               by = 'MonitoringLocationIdentifier', 
               all.x = TRUE)
    W$Sampled <- paste(W$ActivityStartDate, W$ActivityStartTime.Time)
    W <- W[,c(names(wqp.map),'Sampled')]
    W <- plyr::rename(W,wqp.map)
    W$Database <- 'Water Quality Portal'
    W$SD <- paste(W$Database, W$Station_ID)
  } else {
    W <- NULL
  }
  
  if (!is.null(N)) {
    siteInfo <- attr(N, 'siteInfo')
    siteInfo <- siteInfo[!duplicated(siteInfo$site_no),]
    N <- merge(N, siteInfo[,c('site_no', 'station_nm', 'dec_lat_va', 
                              'dec_lon_va', 'hucCd', 'srs')],
               by = 'site_no',
               all.x = TRUE)
    name_map <- c('agency_cd' = 'Client',
                  'site_no' = 'Station_ID',
                  'dateTime' = 'Sampled',
                  'station_nm' = 'Station_Description',
                  'dec_lat_va' = 'DECIMAL_LAT',
                  'dec_lon_va' = 'DECIMAL_LONG',
                  'hucCd' = 'HUC',
                  'srs' = 'DATUM')
    N <- N[,c(names(name_map),'Analyte','Result','Unit','Status','SampleType')]
    N <- plyr::rename(N, name_map)
    N <- cbind(N, data.frame(MRL = rep(NA, nrow(N)),
                             Database = rep("NWIS", nrow(N)),
                             Detect = rep(NA, nrow(N)), 
                             Comment = rep(NA, nrow(N)), 
                             StatusIdentifier = rep(NA, nrow(N))))
    N$SD <- paste(N$Database, N$Station_ID)
    N$Sampled <- strftime(N$Sampled, format = '%Y-%m-%d %H:%M:%S')
    N$Station_ID <- paste0(N$Client, "-", N$Station_ID)
  }
  
  if (!is.null(L)) {
    lasar.map <- c('NAME' = 'Client',
                   'PARAMETER_NM' = 'Analyte',
                   #'PARAMETER_PREFIX_1' = 'Fraction',
                   'STATION' = 'Station_ID',
                   'LOCATION_DESCRIPTION' = 'Station_Description',
                   'QA_QC_TYPE' = 'SampleType',
                   'RESULT' = 'Result',
                   'METHOD_REPORTING_LIMIT' = 'MRL',
                   'UNIT' = 'Unit',
                   'STATUS' = 'Status',
                   'SAMPLE_DATE_TIME' = 'Sampled',
                   'AREA_ABBREVIATION' = 'HUC')
    L <- L[,c(names(lasar.map),'DATUM','DECIMAL_LAT','DECIMAL_LONG')]
    L <- plyr::rename(L,lasar.map)
    L$Database <- 'LASAR'
    L$SD <- paste(L$Database, L$Station_ID)
    L <- cbind(L, data.frame(Detect = rep(NA, nrow(L)), 
                             Comment = rep(NA, nrow(L)), 
                             StatusIdentifier = rep(NA, nrow(L))))
  }
  
  if (!is.null(E)) {
    E <- plyr::rename(E, c('Units' = 'Unit', 
                           'DQL' = 'Status', 
                           'SampleQualifiers' = 'StatusIdentifier', 
                           'AnalyteQualifiers' = 'Comment',
                           'HUC8' = 'HUC'))
    E$DATUM <- 'Assumed NAD83'
    E <- E[,c('Client','Analyte','Station_ID','Station_Description',
              'SampleType','Result','MRL','Unit','Status','Sampled','DATUM',
              'DECIMAL_LAT','DECIMAL_LONG','StatusIdentifier','Comment','HUC')]
    E$Database <- 'Element'
    E$SD <- paste(E$Database, E$Station_ID)
    E <- cbind(E, data.frame(Detect = rep(NA, nrow(E))))
  }
  
  df.all <- rbind(E,L,W,N)
  
  if ('Dissolved Oxygen' %in% df.all$Analyte){
    df.all[which(df.all$Unit == '%'), 'Analyte'] <- 'Dissolved oxygen saturation'
    df.all[df.all$Analyte == "Dissolved oxygen (DO)", 'Analyte'] <- 'Dissolved Oxygen'
  }
  
  if('Total Phosphorus' %in% df.all$Analyte) {
    df.all[df.all$Analyte == 'Phosphorus', 'Analyte'] <- 'Total Phosphorus'
    df.all[df.all$Analyte == 'Phosphate, Total as P', 'Analyte'] <- 'Total Phosphorus'
  }
  
  if('Total Suspended Solids' %in% df.all$Analyte){
    df.all[df.all$Analyte == 'Total suspended solids', 'Analyte'] <- 'Total Suspended Solids'
  }
  
  df.all$Analyte <- mapvalues(df.all$Analyte, 
                              from = c('Temperature, water','Escherichia coli',
                                       'Fecal coliform','Enterococci','E. coli', 'Dissolved Oxygen'),
                              to = c('Temperature','E. Coli','Fecal Coliform',
                                     'Enterococcus','E. Coli', 'Dissolved Oxygen'),
                              warn_missing = FALSE)
  
  return(df.all)
}


elementQuery <- function(planArea = NULL, HUClist, inParms, startDate, endDate,
                         stations_wbd = stations_huc) {
  library(RODBC)
  
  options(stringsAsFactors = FALSE)
  
    #For testing
    # library(sp)
    # library(rgdal)
    # library(rgeos)
    # agwqma <- readOGR(dsn = 'AgWQMA_DataRetrieval/data/GIS', layer = 'ODA_AgWQMA', verbose = FALSE)
    # agwqma <- spTransform(agwqma, CRS("+proj=longlat +datum=NAD83"))
    # HUC <- readOGR(dsn = 'AgWQMA_DataRetrieval/data/GIS', layer = 'huc250k_a_or', verbose = FALSE)
    # HUC <- spTransform(HUC, CRS("+proj=longlat +datum=NAD83"))
    # HUClist <- lapply(as.list(agwqma$PlanName),function(x) {HUC[agwqma[agwqma$PlanName == x,],]})
    # names(HUClist) <- agwqma$PlanName
    # 
    # planArea <- 'South Santiam'
    # startDate <- "2000-03-01 00:00:00.000"
    # endDate <- "2017-03-01 00:00:00.000"
    # inParms <- c('Total Phosphorus')
    # input <- data.frame(select = rep(planArea, 3), parms = inParms, dates = c(startDate, endDate, startDate))
    # parms <- read.csv('AgWQMA_DataRetrieval/data/WQP_Table3040_Names.csv', stringsAsFactors = FALSE)
  
  #### Define Geographic Area using myArea from 01_DataQueryUI.R ####
  
  if (is.null(planArea)) {
    myHUCs <- HUClist
  } else if (grepl("[0-9].", planArea)) {
    myHUCs <- strsplit(planArea, split = " - ")[[1]][1]
  } else {
    myHUCs <- HUClist[HUClist$PlanName == planArea,'HUC8']
  }

  elm <- odbcConnect('ELEMENT')
  
  st <- stations_wbd[stations_wbd$HUC8 %in% myHUCs,]
  st <- st[!duplicated(st$STATION_KEY),]
  
  myStations <- paste(st$STATION_KEY,collapse="','")
  
  #### Specify element names for inParms ####
  qryParms <- c()
  if (any(inParms == 'Temperature')) {
    qryParms <- c(qryParms, 'Temperature')
  }
  if (any(inParms == 'pH')) {
    qryParms <- c(qryParms, 'pH')
  }
  if (any(inParms == 'Bacteria')) {
    qryParms <- c(qryParms, c('E. Coli','Fecal Coliform','Enterococcus'))
  }
  if (any(inParms == 'Dissolved Oxygen')) {
    qryParms <- c(qryParms, c('Dissolved Oxygen','Dissolved oxygen saturation'))
  }
  if(any(inParms == 'Total Suspended Solids')) {
    qryParms<- c(qryParms, c('Total Suspended Solids'))
  }
  if(any(inParms == 'Total Phosphorus')) {
    qryParms <- c(qryParms, c('Phosphate, Total as P'))
  }
  qryParms <- paste(qryParms,collapse="','")
  #### Restrict Matrix to surface water ####
  siteType <- c("'River/Stream','Estuary','Ocean','Lake'")
  
  #### Build query ####
  qry <- paste0("SELECT * FROM Repo_Result WHERE Station_ID in ('",
                myStations,"') AND Analyte in ('", 
                qryParms, "') AND Matrix in (", 
                siteType, ") AND Sampled >= '", 
                startDate, "' AND Sampled <= '",
                endDate, "' AND DQL in ('A', 'B', 'C', 'E');")
  
  #### Pass the query ####
  myData <- sqlQuery(elm, qry)
  
  myData <- merge(myData, st[,c('STATION_KEY','HUC8')], 
                  by.x = 'Station_ID', by.y = 'STATION_KEY', all.x = TRUE)
  
  odbcCloseAll()

  return(myData)
}

lasarQuery <- function(planArea = NULL, HUClist, inParms, startDate, endDate, 
                       stations_wbd = stations_huc) {
  library(RODBC)
  
  options(stringsAsFactors = FALSE)
  
#   #For testing
# library(sp)
# library(rgdal)
# library(rgeos)
# agwqma <- readOGR(dsn = 'app/data/GIS', layer = 'ODA_AgWQMA', verbose = FALSE)
# hucs <- readOGR(dsn = 'app/data/GIS', layer = 'WBD_HU8', verbose = FALSE)
# HUClist <- read.csv('app/data/PlanHUC_LU.csv')
# stations_huc <- read.csv('app/data/station_wbd_12132016.csv')
# planArea <- 'South Santiam'
# startDate <- "2000-01-01 00:00:00.000"
# endDate <- "2010-03-01 00:00:00.000"
# inParms <- c('Total Phosphorus')
  
  #### Establish connection to database ####
  channel <- odbcConnect('DEQLEAD-LIMS')
  
  #### Define Geographic Area using myArea from 01_DataQueryUI.R ####
  if (is.null(planArea)) {
    myHUCs <- paste(HUClist, collapse = "','")
  } else if (grepl("[0-9].", planArea)) {
    myHUCs <- strsplit(planArea, split = " - ")[[1]][1]
  } else {
    myHUCs <- paste(HUClist[HUClist$PlanName == planArea,'HUC8'],collapse="','")
  }
  
  # Get all the LASAR stations within each HUC8
  stationlist <- stations_wbd[stations_wbd$HUC8 %in% 
                                HUClist[HUClist$PlanName == planArea,'HUC8'], 'STATION_KEY']
  myStations <- paste(stationlist,collapse="','")
  
  #### Define site types to query ####
  siteType <- "'Surface water', 'Bay/Estuary/Ocean', 'Canal', 'Reservoir', 'Lake', 'Ditch/Pond/Culvert/Drain'"
  
  #### Define data quality level ####
  myDQL <- "'A+', 'A', 'B', 'C', 'E'"
  
  ##### Set parameters ####
  #Get paramters
  AllParms <- sqlFetch(channel,'XLU_LASAR_PARAMETERS')
  
  qryParms <- c()
  #Expand bacteria to include fecal and enterococcus
  if (any(inParms == 'Temperature')) {
    qryParms <- c(qryParms, unique(AllParms[grep('[Tt]emperature',AllParms$PARAMETER_NM),'PARAMETER_NM']))
  }
  if(any(inParms == 'Bacteria')) {
   qryParms <-  c(qryParms, unique(AllParms[grep('E. [Cc]oli|Fecal [Cc]oliform|[Ee]nterococcus',AllParms$PARAMETER_NM),'PARAMETER_NM']))
  }
  if (any(inParms == 'pH')) {
    qryParms <- c(qryParms, 'pH')
  }
  if(any(inParms == 'Dissolved Oxygen')) {
    qryParms <-  c(qryParms, 'Dissolved Oxygen')
  }
  if(any(inParms == "Total Suspended Solids")) {
    qryParms <- c(qryParms, 'Total Suspended Solids')
  }
  if(any(inParms =='Total Phosphorus')) {
    qryParms <- c(qryParms, 'Total Phosphorus', 'Total Total Phosphorus')
  }
  qryParms <- paste(qryParms, collapse = "','")
  
  #### Build the query ####
  
  # qry <- paste0("SELECT DISTINCT
  #               pm1.ABBREVIATION as PARAMETER_PREFIX_1,
  #               pm2.ABBREVIATION as PARAMETER_PREFIX_2,
  #               p.PARAMETER_NM,
  #               pm3.ABBREVIATION as PARAMETER_SUFFIX_1,
  #               pm4.ABBREVIATION as PARAMETER_SUFFIX_2")
  # qry <- gsub('\n','',qry)  

qry <- paste0("SELECT r.RESULT_KEY,
                     r.STATION,
               s.LOCATION_DESCRIPTION,
               s.DECIMAL_LAT,
               s.DECIMAL_LONG,
               s.DATUM,
               a.AREA_ABBREVIATION,
               o.NAME,
               r.SAMPLE_DATE_TIME,
               d.DATA_DESCRIPTION,
               pm1.ABBREVIATION as PARAMETER_PREFIX_1,
               pm2.ABBREVIATION as PARAMETER_PREFIX_2,
               p.PARAMETER_NM,
               pm3.ABBREVIATION as PARAMETER_SUFFIX_1,
               pm4.ABBREVIATION as PARAMETER_SUFFIX_2,
               r.RESULT,
               u.UNIT,
               st.STATUS,
               qa.QA_QC_TYPE,
               sm.SAMPLE_MATRIX_NAME,
               pr.METHOD_DETECTION_LIMIT,
               pr.METHOD_REPORTING_LIMIT
               FROM Result r LEFT JOIN
               STATION s on r.STATION = s.STATION_KEY LEFT JOIN
               (SELECT DISTINCT a.AREA_ABBREVIATION, sa.STATION FROM STATION_AREA sa LEFT JOIN
               XLU_AREA a on sa.XLU_AREA = a.AREA_KEY WHERE AREA_CLASS = 18) a on r.STATION = a.STATION  LEFT JOIN
               XLU_LASAR_DATA d on r.DATA_TYPE = d.LASAR_DATA_KEY LEFT JOIN
               XLU_LASAR_PARAMETERS p on r.XLU_LASAR_PARAMETER = p.XLU_LASAR_PARAMETERS_KEY LEFT JOIN
               PARAMETER_MODIFIER pm1 on p.PARAMETER_PREFIX_1 = pm1.MODIFIER_KEY LEFT JOIN
               PARAMETER_MODIFIER pm2 on p.PARAMETER_PREFIX_2 = pm2.MODIFIER_KEY LEFT JOIN
               PARAMETER_MODIFIER pm3 on p.PARAMETER_SUFFIX_1 = pm3.MODIFIER_KEY LEFT JOIN
               PARAMETER_MODIFIER pm4 on p.PARAMETER_SUFFIX_2 = pm4.MODIFIER_KEY LEFT JOIN
               UNIT u on p.UNIT = u.UNIT_KEY LEFT JOIN
               XLU_STATUS st on r.QA_QC_STATUS = st.XLU_STATUS_KEY LEFT JOIN
               XLU_QA_QC_TYPE qa on r.QA_QC_TYPE = qa.QA_QC_TYPE_KEY LEFT JOIN
               SAMPLE_MATRIX sm on r.SAMPLE_MATRIX = sm.SAMPLE_MATRIX_KEY LEFT JOIN
               PARAMETER_RESULT pr on r.PARAMETER_RESULT = pr.PARAMETER_RESULT_KEY LEFT JOIN
               ORGANIZATION o on r.SAMPLING_ORGANIZATION = o.ORGANIZATION_KEY
               WHERE r.STATION in ('", myStations, "') AND
               r.SAMPLE_DATE_TIME >= '", startDate, "' AND
               r.SAMPLE_DATE_TIME <= '", endDate, "' AND
               st.STATUS in (", myDQL, ") AND
               sm.SAMPLE_MATRIX_NAME in (", siteType, ") AND
               p.PARAMETER_NM in ('", qryParms, "')")
  qry <- gsub('\n','',qry)
  
  #### Pass the query ####
  myData <- sqlQuery(channel, qry, as.is = c(rep(FALSE,8),TRUE,rep(FALSE,13)))
  
  return(myData)
}

wqpQuery <- function(planArea = NULL, HUClist, inParms, luParms, startDate, endDate) {
library(RCurl)
library(XML)
library(dataRetrieval)
library(plyr)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
#library(RODBC)
  
  # planArea <- input$select
  # inParms <- input$parms
  # luParms <- parms
  # startDate <- input$dates[1]
  # endDate <- input$dates[2]
  
  

options(stringsAsFactors = FALSE)

#### Define Geographic Area using myArea from 01_DataQueryUI.R ####
if (is.null(planArea)) {
  myHUCs <- URLencode.PTB(paste(HUClist, collapse = ";"))
} else if (grepl("[0-9].", planArea)) {
  myHUCs <- strsplit(planArea, split = " - ")[[1]][1]
} else {
  myHUCs <- paste(HUClist[HUClist$PlanName == planArea,'HUC8'],collapse=';')
}

#### Define site types to query ####
#Returns list of available domain values for site type
#wqp.siteTypes <- WQP.domain.get('Sitetype')

#Using the wqp.siteTypes enter the values you want to query for in siteType.
#Separate each value with the URL encoded semi-colon '%3B'. For values with commas use the URL encoded value '%2C+'
siteType = 'Estuary;Ocean;Stream;Lake, Reservoir, Impoundment'

#### Get characteristics ####
#The entire list of parameters that match to a criteria
#luParms<-read.csv('app/data/WQP_Table3040_Names.csv', stringsAsFactors = FALSE)
parms <- luParms

#Take the inputs and write them to another vector for editing
myParms <- inParms

#Expand bacteria to include fecal and enterococcus
if(any(inParms == 'Bacteria')) {
  myParms <- c(myParms, c('E. coli','Fecal coliform','Enterococci'))
  myParms <- myParms[-which(myParms == "Bacteria")] 
}


# #Expand DO to match database domain values
# if (any(inParms == 'Dissolved Oxygen')) {
#   myParms <-c(myParms, c('Dissolved oxygen', 'Dissolved oxygen (DO)'))
#   myParms <- myParms[-which(myParms == 'Dissolved Oxygen')]
# } 

#grab just the parameters we want
#characteristics<- URLencode.PTB(paste(myParms))
#characteristics <- URLencode.PTB(paste(parms[parms$DEQ.Table.name %in% myParms,'WQP.Name'],collapse=';'))
characteristics <- paste(parms[parms$DEQ.Table.name %in% myParms,'WQP.Name'],collapse=';')

#### Define sample media to query ####
#wqp.sampleMedia <- WQP.domain.get('Samplemedia')

#Separate each value you want to query with the URL encoded semi-colon '%3B'.
sampleMedia <- 'Water'

# if (any(myParms == "Total Suspended Solids")) {
#   sampleMedia <- 'Sediment'
# }

#### Pass the query to WQP ####
wqp.data <- readWQPdata(#stateCode = myArea,
  #countycode = myArea,
  huc = myHUCs, 
  characteristicName = characteristics, 
  startDate = startDate, 
  endDate = endDate,
  sampleMedia = sampleMedia,
  siteType = siteType)

Wx <- attr(wqp.data, "siteInfo")
# ##REMOVE dissolved P##
if(any('Total Phosphorus' %in% c(myParms))) {
  wqp.data[c("ResultSampleFractionText")][is.na(wqp.data[c("ResultSampleFractionText")])] <- 'Total'
  wqp.data<- wqp.data[wqp.data$ResultSampleFractionText == 'Total' , ]
    if(any(unique(wqp.data$ResultMeasure.MeasureUnitCode == 'ug/l'))){
        wqp.data[which(wqp.data$ResultMeasure.MeasureUnitCode == 'ug/l'), 'ResultMeasureValue'] <- 
              (wqp.data[which(wqp.data$ResultMeasure.MeasureUnitCode == 'ug/l'), 'ResultMeasureValue'])/1000
    }
  wqp.data[which(wqp.data$ResultMeasure.MeasureUnitCode == 'ug/l'), 'ResultMeasure.MeasureUnitCode'] <- 'mg/l'
  wqp.data[which(wqp.data$ResultMeasure.MeasureUnitCode == 'mg/kg as P'), 'ResultMeasure.MeasureUnitCode'] <- 'mg/l'
  wqp.data[which(wqp.data$ResultMeasure.MeasureUnitCode == 'mg/l as P'), 'ResultMeasure.MeasureUnitCode'] <- 'mg/l'
  
 }

attr(wqp.data, "siteInfo") <- Wx

#wqp.stations <- attr(wqp.data, 'siteInfo')

# #Write query output to .csv
# timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
# 
# wqp.data.filename <- paste('./Data/wqpData',timestamp,'.csv',sep='')
# write.csv(wqp.data,wqp.data.filename)
# 
# wqp.stations.filename <- paste('./Data/wqpStations',timestamp,'.csv',sep='')
# write.csv(wqp.stations,wqp.stations.filename)

# if (is.null(wqp.data)) {
#   wqp.data <- "No data"
# }

#### ####
return(wqp.data)
}

URLencode.PTB <- function (URL, reserved = FALSE) 
{
  OK <- "[^-ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvywxyz0-9//_']"
  x <- strsplit(URL, "")[[1L]]
  z <- grep(OK, x)
  if (length(z)) {
    y <- sapply(x[z], function(x) {if(x == ' ') {
      '+'
    } else if (x == '('){
      '('
    } else if (x == ')'){
      ')'
    } else {
      paste0("%", as.character(charToRaw(x)), collapse = "")
    }
    })
    x[z] <- y
  }
  paste(x, collapse = "")
}

nwisQuery <- function(planArea = NULL, HUClist, inParms, startDate, endDate) {
  library(RCurl)
  library(XML)
  library(dataRetrieval)
  library(plyr)
  library(sp)
  library(rgdal)
  library(raster)
  library(rgeos)
  library(data.table)
  #library(RODBC)
  
  options(stringsAsFactors = FALSE)
  
  #### Define Geographic Area using myArea from 01_DataQueryUI.R ####
  if (is.null(planArea)) {
    myHUCs <- paste(HUClist, collapse = ",")
  } else if (grepl("[0-9].", planArea)) {
    myHUCs <- strsplit(planArea, split = " - ")[[1]][1]
  } else {
    myHUCs <- paste(HUClist[HUClist$PlanName == planArea,'HUC8'],collapse=',')
  }
  
  #### Define site types ####
  siteTypeCd <- "LK,ST,ST-CA,ST-DCH,ST-TS,ES"
  
  temp_data_c <- NULL
  tc_sites <- NULL
  temp_data_f <- NULL
  tf_sites <- NULL
  ph_data <- NULL
  DO_data<- NULL
  ph_sites <- NULL
  DO_sites <- NULL
  TSS_sites<-NULL
  TSS_data<-NULL
  TP_sites<-NULL
  TP_data<-NULL
  
  #### Define parameters to query ####
  if ('Temperature' %in% inParms) {
    temp_data_c <- readNWISdata(service = "dv",
                                huc=myHUCs,
                                siteTypeCd=siteTypeCd,
                                startDate=startDate,
                                endDate=endDate,
                                parameterCd='00010')
    if (nrow(temp_data_c) > 0) {
      tc_sites <- attr(temp_data_c, 'siteInfo')
      temp_data_c <- temp_data_c[, c('agency_cd', 'site_no', 'dateTime', 
                                     'X_00010_00001', 'X_00010_00001_cd')]
      temp_data_c$Analyte <- 'Temperature'
      temp_data_c$Unit <- 'C'
      temp_data_c$SampleType <- 'Maximum'
    }
    
    
    temp_data_f <- readNWISdata(service = "dv",
                                huc=myHUCs,
                                siteTypeCd=siteTypeCd,
                                startDate=startDate,
                                endDate=endDate,
                                parameterCd='00011')
    if (nrow(temp_data_f) > 0) {
      tf_sites <- attr(temp_data_f, 'siteInfo')
      temp_data_f <- temp_data_f[, c('agency_cd', 'site_no', 'dateTime', 
                                     'X_00011_00001', 'X_00011_00001_cd')]
      temp_data_f$Analyte <- 'Temperature'
      temp_data_f$Unit <- 'F'
      temp_data_c$SampleType <- 'Maximum'
    }
  }
  
  if('pH' %in% inParms) {
    ph_data <- readNWISdata(service = "iv",
                            huc=myHUCs,
                            siteTypeCd=siteTypeCd,
                            startDate=startDate,
                            endDate=endDate,
                            parameterCd='00400')
    if (nrow(ph_data) > 0) {
      ph_sites <- attr(ph_data, 'siteInfo')
      ph_data <- ph_data[,names(ph_data) != "tz_cd"]
      ph_data$Analyte <- 'pH'
      ph_data$Unit <- 'ph Units'
      ph_data$SampleType <- 'Continuous'
      if (any(grepl('Mid|Lower', names(ph_data)))) {
        ph_data <- ph_data[,-grep('Mid|Lower', names(ph_data))]
      }
    }
  }
  
  if('Dissolved Oxygen' %in% inParms) {
    DO_data <- readNWISdata(service = "iv",
                            huc=myHUCs,
                            siteTypeCd=siteTypeCd,
                            startDate=startDate,
                            endDate=endDate,
                            parameterCd='00300')
    if (nrow(DO_data) > 0) {
      DO_sites <- attr(DO_data, 'siteInfo')
      DO_data <- DO_data[,names(DO_data) != 'tz_cd']
      DO_data$Analyte <- 'Dissolved Oxygen'
      DO_data$Unit <- 'mg/l'
      DO_data$SampleType <- 'Continuous'
      if (any(grepl('Mid|Lower', names(DO_data)))) {
        DO_data <- DO_data[,-grep('Mid|Lower', names(DO_data))]
      }
    }
  }
  
  if('Total Suspended Solids' %in% inParms) {
    parmatercode<-c('00530', '70293', '70299')
    TSS_data <- readNWISdata(service = "iv",
                            huc=myHUCs,
                            siteTypeCd=siteTypeCd,
                            startDate=startDate,
                            endDate=endDate,
                            parameterCd = parmatercode)
    if (nrow(TSS_data) > 0) {
      TSS_sites <- attr(TSS_data, 'siteInfo')
      TSS_data <- TSS_data[,names(TSS_data) != 'tz_cd']
      TSS_data$Analyte <- 'Total Suspended Solids'
      TSS_data$Unit <- 'mg/l'
      TSS_data$SampleType <- 'grab'
      if (any(grepl('Mid|Lower', names(TSS_data)))) {
        TSS_data <- TSS_data[,-grep('Mid|Lower', names(TSS_data))]
      }
    } else {
      TSS_data <- NULL 
    }
  }
  
  if('Total Phosphorus' %in% inParms) {
    parmatercode<-c('99891', '99893')
    TP_data <- readNWISdata(service = "iv",
                             huc=myHUCs,
                             siteTypeCd=siteTypeCd,
                             startDate=startDate,
                             endDate=endDate,
                             parameterCd = parmatercode)
    if (nrow(TP_data) > 0) {
      TP_sites <- attr(TP_data, 'siteInfo')
      TP_data <- TP_data[,names(TP_data) != 'tz_cd']
      TP_data$Analyte <- 'Total Phosphorus'
      TP_data$Unit <- 'mg/l'
      TP_data$SampleType <- 'grab'
      if (any(grepl('Mid|Lower', names(TP_data)))) {
        TP_data <- TP_data[,-grep('Mid|Lower', names(TP_data))]
      }
    } else {
      TP_data <- NULL 
    }
  }
  
  
  df_list <- list(temp_data_c, temp_data_f, ph_data, DO_data, TSS_data)
  df_list_rows <- lapply(df_list, nrow)
  
  if (any(unlist(df_list_rows) > 0)) {
    df_list <- df_list[unlist(sapply(df_list, function(x) dim(x)[1])) > 0]
    df_list <- lapply(df_list, function(x) {
      comment_field <- grep('^X_',grep('_cd$',names(x), 
                                       value = TRUE), 
                            value = TRUE)
      comment_rename <- setNames("Status", comment_field)
      x <- plyr::rename(x, comment_rename)
      result_field <- grep('^X', names(x), value = TRUE)
      result_rename <- setNames("Result", result_field)
      x <- plyr::rename(x, result_rename)
    })
    nwis_data <- as.data.frame(data.table::rbindlist(df_list))
    siteInfo <- rbind(tc_sites, tf_sites, ph_sites, DO_sites)
    attr(nwis_data, "siteInfo") <- siteInfo
  } else {
    nwis_data <- NULL
  }
  return(nwis_data)
}
