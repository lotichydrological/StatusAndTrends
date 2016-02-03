combine <- function(E = NULL, L = NULL, W = NULL, N = NULL) {
#   E <- elmData
#   L <- lasarData
#   W <- wqpData
#   W <- wqp.data
  
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
    W <- rename(W,wqp.map)
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
    N <- N[,c(names(name_map),'Analyte','Result','Unit','Status')]
    N <- rename(N, name_map)
    N <- cbind(N, data.frame(SampleType = rep("Continuous", nrow(N)),
                             MRL = rep(NA, nrow(N)),
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
    L <- rename(L,lasar.map)
    L$Database <- 'LASAR'
    L$SD <- paste(L$Database, L$Station_ID)
    L <- cbind(L, data.frame(Detect = rep(NA, nrow(L)), 
                             Comment = rep(NA, nrow(L)), 
                             StatusIdentifier = rep(NA, nrow(L))))
  }
  
  if (!is.null(E)) {
    E <- rename(E, c('Units' = 'Unit', 
                     'DQL' = 'Status', 
                     'SampleQualifiers' = 'StatusIdentifier', 
                     'AnalyteQualifiers' = 'Comment',
                     'AREA_ABBREVIATION' = 'HUC'))
    E <- E[,c('Client','Analyte','Station_ID','Station_Description',
              'SampleType','Result','MRL','Unit','Status','Sampled','DATUM',
              'DECIMAL_LAT','DECIMAL_LONG','StatusIdentifier','Comment','HUC')]
    E$Database <- 'Element'
    E$SD <- paste(E$Database, E$Station_ID)
    E <- cbind(E, data.frame(Detect = rep(NA, nrow(E))))
  }
    
  df.all <- rbind(E,L,W,N)
  
  df.all$Analyte <- mapvalues(df.all$Analyte, 
                              from = c('Temperature, water','Escherichia coli',
                                       'Fecal coliform','Enterococci','E. coli'),
                              to = c('Temperature','E. Coli','Fecal Coliform',
                                     'Enterococcus','E. Coli'),
                              warn_missing = FALSE)
  
   return(df.all)
}

elementQuery <- function(planArea = NULL, HUClist, inParms, startDate, endDate) {
  library(RODBC)
  
  options(stringsAsFactors = FALSE)
  
  #   #For testing
#     library(sp)
#     library(rgdal)
#     library(rgeos)
#     agwqma <- readOGR(dsn = 'AgWQMA_DataRetrieval/data/GIS', layer = 'ODA_AgWQMA', verbose = FALSE)
#     agwqma <- spTransform(agwqma, CRS("+proj=longlat +datum=NAD83"))
#     HUC <- readOGR(dsn = 'AgWQMA_DataRetrieval/data/GIS', layer = 'huc250k_a_or', verbose = FALSE)
#     HUC <- spTransform(HUC, CRS("+proj=longlat +datum=NAD83"))
#     HUClist <- lapply(as.list(agwqma$PlanName),function(x) {HUC[agwqma[agwqma$PlanName == x,],]})
#     names(HUClist) <- agwqma$PlanName
#     
#     planArea <- 'Inland Rogue'
#     startDate <- "2011-03-01 00:00:00.000"
#     endDate <- "2013-03-01 00:00:00.000"
#     inParms <- c('Temperature')
#     input <- data.frame(select = rep(planArea, 3), parms = inParms, dates = c(startDate, endDate, startDate))
#     parms <- read.csv('AgWQMA_DataRetrieval/data/WQP_Table3040_Names.csv', stringsAsFactors = FALSE)
  
  #### Define Geographic Area using myArea from 01_DataQueryUI.R ####
  if (is.null(planArea)) {
    myHUCs <- HUClist
  } else {
    myHUCs <- HUClist[HUClist$PlanName == planArea,'HUC8']
  }

  las <- odbcConnect('LASAR2_GIS')
  elm <- odbcConnect('ELEMENT')
  
  st <- sqlQuery(las, 'SELECT s.STATION_KEY, 
                                    m.DATUM, 
                       xa.AREA_ABBREVIATION 
                       FROM STATION s 
                            LEFT JOIN XLU_MAP_DATUM m on s.DATUM = m.DATUM_KEY 
                            LEFT JOIN STATION_AREA sa on s.STATION_KEY = sa.STATION 
                            LEFT JOIN XLU_AREA xa on sa.XLU_AREA = xa.AREA_KEY')
  st <- st[st$AREA_ABBREVIATION %in% myHUCs,]
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
  qryParms <- paste(qryParms,collapse="','")
  #### Restrict Matrix to surface water ####
  siteType <- c("'River/Stream','Estuary','Ocean','Lake'")
  
  #### Build query ####
  qry <- paste0("SELECT * FROM Repo_Result WHERE Station_ID in ('",
                myStations,"') AND Analyte in ('", 
                qryParms, "') AND Matrix in (", 
                siteType, ") AND Sampled >= '", 
                startDate, "' AND Sampled <= '",
                endDate, "';")
  
  #### Pass the query ####
  myData <- sqlQuery(elm, qry)
  
  myData <- merge(myData, st[,c('STATION_KEY','DATUM','AREA_ABBREVIATION')], 
                  by.x = 'Station_ID', by.y = 'STATION_KEY', all.x = TRUE)
  
  odbcCloseAll()

  return(myData)
}

lasarQuery <- function(planArea = NULL, HUClist, inParms, startDate, endDate) {
  library(RODBC)
  
  options(stringsAsFactors = FALSE)
  
#   #For testing
#   library(sp)
#   library(rgdal)
#   library(rgeos)
#   agwqma <- readOGR(dsn = 'AgWQMA_DataRetrieval/data/GIS', layer = 'ODA_AgWQMA', verbose = FALSE)
#   agwqma <- spTransform(agwqma, CRS("+proj=longlat +datum=NAD83"))
#   HUC <- readOGR(dsn = 'AgWQMA_DataRetrieval/data/GIS', layer = 'huc250k_a_or', verbose = FALSE)
#   HUC <- spTransform(HUC, CRS("+proj=longlat +datum=NAD83"))
#   HUClist <- lapply(as.list(agwqma$PlanName),function(x) {HUC[agwqma[agwqma$PlanName == x,],]})
#   names(HUClist) <- agwqma$PlanName
#   
#   planArea <- 'Inland Rogue'
#   startDate <- "2010-01-01 00:00:00.000"
#   endDate <- "2010-03-01 00:00:00.000"
#   inParms <- c('Temperature','pH','Bacteria')
  
  #### Establish connection to database ####
  channel <- odbcConnect('LASAR2_GIS')
  
  #### Define Geographic Area using myArea from 01_DataQueryUI.R ####
  if (is.null(planArea)) {
    myHUCs <- paste(HUClist, collapse = "','")
  } else {
    myHUCs <- paste(HUClist[HUClist$PlanName == planArea,'HUC8'],collapse="','")
  }
  
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
  
  qryParms <- paste(qryParms, collapse = "','")
  
  #### Build the query ####
  
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
               WHERE a.AREA_ABBREVIATION in ('", myHUCs, "') AND
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

options(stringsAsFactors = FALSE)

#### Define Geographic Area using myArea from 01_DataQueryUI.R ####
if (is.null(planArea)) {
  myHUCs <- URLencode.PTB(paste(HUClist, collapse = ";"))
} else {
  myHUCs <- URLencode.PTB(paste(HUClist[HUClist$PlanName == planArea,'HUC8'],collapse=';'))
}

#### Define site types to query ####
#Returns list of available domain values for site type
#wqp.siteTypes <- WQP.domain.get('Sitetype')

#Using the wqp.siteTypes enter the values you want to query for in siteType.
#Separate each value with the URL encoded semi-colon '%3B'. For values with commas use the URL encoded value '%2C+'
siteType = URLencode.PTB('Estuary;Ocean;Stream;Lake, Reservoir, Impoundment')

#### Get characteristics ####
#The entire list of parameters that match to a criteria
parms <- luParms
#parms <- read.csv('AgWQMA_DataRetrieval/data/WQP_Table3040_Names.csv', stringsAsFactors = FALSE)

#Expand bacteria to include fecal and enterococcus
if(any(inParms == 'Bacteria')) {
  myParms <- c(inParms, c('E. coli','Fecal coliform','Enterococci'))
  myParms <- myParms[-which(myParms == "Bacteria")]
} else {
  myParms <- inParms
}


#grab just the parameters we want
characteristics <- URLencode.PTB(paste(parms[parms$DEQ.Table.name %in% myParms,'WQP.Name'],collapse=';'))

#### Define sample media to query ####
#wqp.sampleMedia <- WQP.domain.get('Samplemedia')

#Separate each value you want to query with the URL encoded semi-colon '%3B'.
sampleMedia <- 'Water'

#### Pass the query to WQP ####
wqp.data <- readWQPdata(#stateCode = myArea,
  #countycode = myArea,
  huc = myHUCs, 
  characteristicName = characteristics, 
  startDate = startDate, 
  endDate = endDate,
  sampleMedia = sampleMedia,
  siteType = siteType)
#wqp.stations <- attr(wqp.data, 'siteInfo')

# #Write query output to .csv
# timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
# 
# wqp.data.filename <- paste('./Data/wqpData',timestamp,'.csv',sep='')
# write.csv(wqp.data,wqp.data.filename)
# 
# wqp.stations.filename <- paste('./Data/wqpStations',timestamp,'.csv',sep='')
# write.csv(wqp.stations,wqp.stations.filename)

if (is.null(wqp.data)) {
  wqp.data <- "No data"
}

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
  } else {
    myHUCs <- paste(HUClist[HUClist$PlanName == planArea,'HUC8'],collapse=',')
  }
  
  #### Define site types ####
  siteTypeCd <- "LK,ST,ST-CA,ST-DCH,ST-TS,ES"
  
  temp_data_c <- NULL
  temp_data_f <- NULL
  ph_data <- NULL
  #### Define parameters to query ####
  if ('Temperature' %in% inParms) {
    temp_data_c <- readNWISdata(service = "iv",
                                huc=myHUCs,
                                siteTypeCd=siteTypeCd,
                                startDate=startDate,
                                endDate=endDate,
                                parameterCd='00010')
    if (nrow(temp_data_c) > 0) {
      temp_data_c$Analyte <- 'Temperature'
      temp_data_c$Unit <- 'C'
    }
    
    
    temp_data_f <- readNWISdata(service = "iv",
                                huc=myHUCs,
                                siteTypeCd=siteTypeCd,
                                startDate=startDate,
                                endDate=endDate,
                                parameterCd='00011')
    if (nrow(temp_data_f) > 0) {
      temp_data_f$Analyte <- 'Temperature'
      temp_data_f$Unit <- 'F'
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
      ph_data$Analyte <- 'pH'
      ph_data$Unit <- 'ph Units'
    }
  }
  
  
  df_list <- list(temp_data_c, temp_data_f, ph_data)
  df_list_rows <- lapply(df_list, nrow)
  
  if (any(unlist(df_list_rows) > 0)) {
    df_list <- df_list[unlist(sapply(df_list, function(x) dim(x)[1])) > 0]
    df_list <- lapply(df_list, function(x) {
      comment_field <- grep('^X_',grep('_cd$',names(x), 
                                       value = TRUE), 
                            value = TRUE)
      comment_rename <- setNames("Status", comment_field)
      x <- rename(x, comment_rename)
      result_field <- grep('^X', names(x), value = TRUE)
      result_rename <- setNames("Result", result_field)
      x <- rename(x, result_rename)
    })
    site_list <- lapply(df_list, function (x) {
      x_siteInfo <- attr(x, "siteInfo")
    })
    nwis_data <- as.data.frame(data.table::rbindlist(df_list))
    siteInfo <- as.data.frame(data.table::rbindlist(site_list))
    attr(nwis_data, "siteInfo") <- siteInfo
  } else {
    nwis_data <- 'No data'
  }
  return(nwis_data)
}