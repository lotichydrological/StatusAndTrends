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
#We want to extract only those stations in the current AgWQMA so let's bring that layer in and match the projection
agwqma <- readOGR(dsn = './GIS', layer = 'ODA_AgWQMA')
path = 'T:/AgWQM/DataAnalysis/StatusAndTrends/GIS/ODA_AgWQMA.shp'
agwqma <- shapefile(x = path, stringsAsFactors = FALSE)

#Transform agwqma to same projection as points
agwqma <- spTransform(agwqma, CRS("+proj=longlat +datum=NAD83"))
agwqma <- spTransform(agwqma, CRS("+init=epsg:2994"))

#Bring in the WBD for 8 digit HUC
HUC <- readOGR(dsn = '//deqhq1/gislibrary/base_data/hydrography/nhd/2008_Watershed_Boundary_Dataset/WBD_HUC_4th/hydrologic_units', layer = 'huc250k_a_or')
path2 = '//deqhq1/gislibrary/base_data/hydrography/nhd/2008_Watershed_Boundary_Dataset/WBD_HUC_4th/hydrologic_units/huc250k_a_or.shp'
HUC <- shapefile(x = path2, stringsAsFactors = FALSE)
HUC <- spTransform(HUC, CRS("+proj=longlat +datum=NAD83"))
HUC <- spTransform(HUC, CRS("+init=epsg:2994"))
HUC_OR <- HUC[agwqma,]

#Generate list of HUCs
agAreas <- as.list(agwqma$PlanName)
HUC.list <- lapply(agAreas, function(x) {HUC[agwqma[agwqma$PlanName == x,],]})
HUC.list <- lapply(as.list(agwqma$PlanName),function(x) {HUC[agwqma[agwqma$PlanName == x,],]})

names(HUC.list) <- agwqma$PlanName
myHUCs <- HUC.list[myArea][[1]]$HUC_8

#### Define site types to query ####
#Returns list of available domain values for site type
wqp.siteTypes <- WQP.domain.get('Sitetype')

#Using the wqp.siteTypes enter the values you want to query for in siteType.
#Separate each value with the URL encoded semi-colon '%3B'. For values with commas use the URL encoded value '%2C+'
siteType = 'Estuary;Ocean;Stream;Lake, Reservoir, Impoundment'