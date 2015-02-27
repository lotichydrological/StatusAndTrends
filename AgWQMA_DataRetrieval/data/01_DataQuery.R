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