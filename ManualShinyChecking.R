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
wq_limited <- readOGR(dsn = 'app/data/GIS', layer = 'ORStreamsWaterQuality_2010_WQLimited_V3', verbose = FALSE)

#For app purposes set up input 
input <- list(action_button = c(0))
input$action_button <- 1
input$parms <- c('Temperature', 'pH', 'Bacteria')
input$select <- 'Burnt River'
input$dates <- c("2010-01-01", "2015-05-21")
input$db <- c("LASAR", "Element")
input$selectStation <-  "27760 - "
input$selectParameter <- 'Temperature'
input$selectLogScale <- TRUE
input$selectSpawning <- 'January 1-June 15'
input$selectUse <- 'Salmon and Steelhead Migration Corridors'
input$selectpHCrit <- 'Powder - All other basin waters'
input$selectRange <- (c(as.Date(strptime(input$dates[1], format = "%Y-%m-%d")), 
                                                       as.Date(strptime(input$dates[2], format = "%Y-%m-%d"))))
input$plotTrend <- TRUE
# 
# spn_index <- which(test$standard == 1)
# spn_diff <- diff(spn_index)
# 
# if (all(spn_diff == 1)) {
#   spn_1 <- max(spn_index)
#   
#   lines(x = c(test[spn_1 + 1, 'data'],
#               test[nrow(test), 'data']),
#         y = c(unique(test[(spn_1 + 1):nrow(test),'standard']) + 12.5,
#               unique(test[(spn_1 + 1):nrow(test),'standard']) + 12.5))
# } else {
#   spn_1 <- which(spn_diff > 1)
#   spn_2_start <- spn_index[spn_1 + 1]
#   
#   lines(x = c(test[spn_2_start,'data'],
#               test[nrow(test),'data']),
#         y = c(unique(test[spn_2_start:nrow(test),'standard']) + 12.5,
#               unique(test[spn_2_start:nrow(test),'standard']) + 12.5))
#   
#   lines(x = c(test[spn_1 + 1,'data'],
#               test[spn_2_start - 1, 'data']),
#         y = c(unique(test[(spn_1 + 1):(spn_2_start - 1),'standard']) + 12.5,
#               unique(test[(spn_1 + 1):(spn_2_start - 1),'standard']) + 12.5))
# }
# 
# lines(x = c(test[1,'data'],
#             test[spn_1,'data']),
#       y = c(unique(test[1:spn_1,'standard']) +12.5,
#             unique(test[1:spn_1,'standard'])+12.5))
# 
# 
# 
# 
# 
# 
# 
# 

# new_data$year <- years(new_data$Sampled)
# plot(as.Date(new_data$plotSampled), new_data$Result, col = c(3:(2 + length(levels(new_data$year)) - 1),1), pch = 19, xlim = c(as.Date('2000-01-01'),as.Date('2000-12-31')))
# legend('topright',legend = levels(new_data$year), col = c(3:(2 + length(levels(new_data$year)) - 1),1), pch = 19)