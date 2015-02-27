elementQuery <- function(planArea, HUClist, inParms, startDate, endDate) {
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
#     startDate <- "2010-01-01 00:00:00.000"
#     endDate <- "2013-03-01 00:00:00.000"
#     inParms <- c('Temperature','pH','Bacteria')
  
  #### Define Geographic Area using myArea from 01_DataQueryUI.R ####
  myHUCs <- HUClist[planArea][[1]]$HUC_8
  
  las <- odbcConnect('LASAR2_GIS')
  elm <- odbcConnect('ELEMENT')
  
  sa <- sqlFetch(las, 'STATION_AREA')
  xa <- sqlFetch(las, 'XLU_AREA')  
  
  myStations <- paste(sa[sa$XLU_AREA %in% xa[xa$AREA_ABBREVIATION %in% myHUCs,'AREA_KEY'],'STATION'],collapse="','")
  
  #### Specify element names for inParms ####
  qryParms <- c()
  if (any(inParms == 'Temperature')) {
    qryParms <- c(qryParms, 'Temperature')
  }
  if (any(inParms == 'pH')) {
    qryParms <- c(qryParms, 'pH')
  }
  if (any(inParms == 'Bacteria')) {
    qryParms <- c(qryParms, c('E. Coli','Fecal Coliform','Total Coliform','Enterococcus'))
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

  odbcCloseAll()

  return(myData)
}

lasarQuery <- function(planArea, HUClist, inParms, startDate, endDate) {
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
  myHUCs <- paste(HUClist[planArea][[1]]$HUC_8,collapse="','")
  
  #### Define site types to query ####
  siteType <- "'Surface water', 'Bay/Estuary/Ocean', 'Canal', 'Reservoir', 'Lake', 'Ditch/Pond/Culvert/Drain'"
  
  #### Define data quality level ####
  myDQL <- "'A+', 'A', 'B'"
  
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
               STATION_AREA sa on r.STATION = sa.STATION LEFT JOIN 
               XLU_AREA a on sa.XLU_AREA = a.AREA_KEY LEFT JOIN
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
  myData <- sqlQuery(channel, qry)
  
  return(myData)
}

wqpQuery <- function(planArea, HUClist, inParms, startDate, endDate) {
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
myHUCs <- paste(HUClist[planArea][[1]]$HUC_8,collapse=';')

#### Define site types to query ####
#Returns list of available domain values for site type
#wqp.siteTypes <- WQP.domain.get('Sitetype')

#Using the wqp.siteTypes enter the values you want to query for in siteType.
#Separate each value with the URL encoded semi-colon '%3B'. For values with commas use the URL encoded value '%2C+'
siteType = 'Estuary;Ocean;Stream;Lake, Reservoir, Impoundment'

#### Get characteristics ####
#The entire list of parameters that match to a criteria
parms <- read.csv('./data/WQP_Table3040_Names.csv', stringsAsFactors = FALSE)

#Expand bacteria to include fecal and enterococcus
if(any(inParms == 'Bacteria')) {
  inParms <- c(inParms, c('E. coli','Fecal coliform','Enterococci'))
  inParms <- inParms[-which(inParms == "Bacteria")]
  }


#grab just the parameters we want
characteristics <- paste(parms[parms$DEQ.Table.name %in% inParms,'WQP.Name'],collapse=';')


#Now for the LASAR Stations they use useCodes 
#xsu <- sqlFetch(channel, "XLU_STATION_USE")
#STATION_USE <- sqlFetch(channel, "STATION_USE")
#xsu[xsu$XLU_STATION_USE_KEY %in% STATION_USE$XLU_STATION_USE,c('XLU_STATION_USE_KEY','DESCRIPTION')]
# - This is a list of the station use codes that are actually used.
# XLU_STATION_USE_KEY DESCRIPTION
# 32  Beaches
# 51  Domestic Supply
# 63	Obs/Monitoring
# 71	Air Quality
# 77	Industrial Waste
# 78	Agricultural Waste
# 79	Ambient Water Quality
# 97	Stream surface water
# 98	Lake surface water
# 103	303d analysis Stations General
# 104	303d analysis Saltwater stations
# 105	Subsurface water (well,groundwater,subsurface treatment water)
# 106	Domestic Wastewater treatment (septic tank)
# 107	303d analysis Cascade Lakes above 3000 ft.
# 109	Landfill monitoring well
# 110	Domestic drinking water well
# 111	Landfill surface water
# 113	Domestic Wastewater treatment (STP)
# 114	Umatilla Army WMD Depot well
# 112	Canal, culvert, ditch, drain
# 119	Estuary
# 120	Stream or River
# 121	Land
# 122	Source
# 123	Other
# 124	Lake or Pond
# 125	Air Monitoring
# 126	Reservoir or Quarry
# 127	Ocean
# 128	Wetland
# 129	Spring
# 130	Groundwater

#Relevant use codes based on above description
useCodes <- c(32, 63, 79, 97, 98, 103, 104, 107, 112, 119, 120, 124, 126, 127, 128, 129)


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

#### ####
return(wqp.data)
}