library(sp)
# library(rgdal)
# library(raster)
# library(rgeos)

agwqma <- readOGR(dsn = 'app/data/GIS', layer = 'ODA_AgWQMA', verbose = FALSE)
agwqma <- spTransform(agwqma, CRS("+init=epsg:4269"))
wq_limited <- readOGR(dsn = 'app/data/GIS', 
                      layer = 'ORStreamsWaterQuality_2010_WQLimited_V3', 
                      verbose = FALSE)

wq_limited <- wq_limited[wq_limited$Pollutant %in% 
                           c("Temperature", "pH", "Fecal Coliform", "E. Coli",
                             "Enterococcus"),]
wq_limited <- spTransform(wq_limited, CRS("+init=epsg:4269"))

for (i in 1:length(unique(agwqma$PlanName))) {
  ag_sub <- agwqma[agwqma$PlanName == unique(agwqma$PlanName)[i],]
    
  tryCatch(wq_limited_sub <- wq_limited[ag_sub,], error = function(err) {"woops"})
  
  if(!exists("wq_limited_sub")) {
    next
  }
  
  wq_limited_sub_df <- wq_limited_sub@data
  wq_limited_sub_df$PlanName <- unique(agwqma$PlanName)[i]
  
  if (i == 1) {
    wq_limited_df <- wq_limited_sub_df
  } else {
    wq_limited_df <- rbind(wq_limited_df, wq_limited_sub_df)
  }
  
  rm(ag_sub, wq_limited_sub, wq_limited_sub_df)
}

write.csv(wq_limited_df, 'app/data/GIS/wq_limited_df_temp_bact_ph.csv', row.names = FALSE)
