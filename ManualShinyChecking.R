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

source('testing/data/01_DataQuery.R')
source('testing/data/funClean.R')

agwqma <- readOGR(dsn = 'testing/data/GIS', layer = 'ODA_AgWQMA', verbose = FALSE)
agwqma <- spTransform(agwqma, CRS("+proj=longlat +datum=NAD83"))
HUClist <- read.csv('testing/data/PlanHUC_LU.csv')
ph_crit <- read.csv('testing/data/PlanOWRDBasinpH_LU.csv')

#For testing purposes set up input 
input <- list(action_button = c(0))
input$action_button <- 1
input$parms <- c('Bacteria')
input$select <- 'Burnt River'
input$dates <- c("2010-01-01", "2015-05-21")
input$db <- c("LASAR")
input$selectStation <-  "11494 - "
input$selectParameter <- 'E. Coli'
input$selectLogScale <- TRUE
input$selectSpawning <- 'No spawning'
input$selectUse <- 'Redband and Lanhontan Cutthroat Trout'
input$selectpHCrit <- 'Powder - All other basin waters'
input$selectRange <- (c(as.Date(strptime(input$dates[1], format = "%Y-%m-%d")), 
                                                       as.Date(strptime(input$dates[2], format = "%Y-%m-%d"))))
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
