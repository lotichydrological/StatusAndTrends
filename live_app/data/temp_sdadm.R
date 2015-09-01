Calculate.sdadm <- function(spawning, station, use, df.all) {
  #
  spd <- spawning #input$selectSpawning
  spd_list <- strsplit(spd, split = "-")
  spd_chron <- lapply(spd_list, function(x) {as.chron(x, format = "%B %d")})
  spd_months <- lapply(spd_chron, months)
  spd_days <- lapply(spd_chron, days)
  spd_months_num <- as.numeric(spd_months[[1]])
  spd_days_num <- as.numeric(spd_days[[1]])
  SSTART_MONTH <- spd_months_num[1]
  SEND_MONTH <- spd_months_num[2]
  SSTART_DAY <- spd_days_num[1]
  SEND_DAY <- spd_days_num[2]
  sdata <- data.frame('id' = unique(strsplit(station,' - ')[[1]][1]), #input$selectStation
                      'spawndates' = spd,
                      'SSTART_MONTH' = SSTART_MONTH, 
                      'SSTART_DAY' = SSTART_DAY,
                      'SEND_MONTH' = SEND_MONTH,
                      'SEND_DAY' = SEND_DAY)
  sdata$ZDADM <- switch(use, #input$selectUse
                        'Bull Trout Spawning and Juvenile Rearing' = 12,
                        'Core Cold Water Habitat' = 16,
                        'Salmon and Trout Rearing and Migration' = 18,
                        'Salmon and Steelhead Migration Corridors' = 20,
                        'Redband and Lanhontan Cutthroat Trout' = 20,
                        'Cool water species' = NA,
                        'No Salmonid Use/Out of State' = NA
  )
  rm(spd,spd_list,spd_chron,spd_months,spd_days,
     spd_months_num,spd_days_num,SSTART_MONTH,
     SSTART_DAY,SEND_MONTH,SEND_DAY)
  tdata <- df.all[df.all$Station_ID == unique(strsplit(station,' - ')[[1]][1]) & #input$selectStation
                    df.all$Analyte == 'Temperature',c('Station_ID','Station_Description',
                                                      'Sampled','Unit','Result')]
  
  ## RENAME
  colnames(tdata)[1] <- "id"
  colnames(tdata)[2] <- "name"
  colnames(tdata)[3] <- "datetime"
  colnames(tdata)[4] <- "unit"
  colnames(tdata)[5] <- "t"
  
  ## F -> C  Not a perfect solution but not sure how to deal with it otherwise.
  #Ft <- "Temperature  (?F)" # Using the unit text doesn't seem to work on a pc (works on mac). I think the degree symbol is the problem.
  tdata$t_c <- ifelse(tdata$t > 36, round(((tdata$t-32)*5/9),1),tdata$t)
  is.numeric(tdata$t_c)
  
  ## Create a vector of daily dates for grouping
  tdata$date <- as.Date(tdata$datetime, format="%m/%d/%Y")
  
  #inherits(tdata$date, "Date") # checks to see if it is class Date
  
  #############################
  # tdata COLUMN NAMES
  # tdata[1] <- "id"
  # tdata[2] <- "name"
  # tdata[3] <- "lat"
  # tdata[4] <- "long"
  # tdata[5] <- "datetime"
  # tdata[6] <- "unit"
  # tdata[7] <- "t"
  # tdata[8] <- "t_c"
  # tdata[9] <- "date"
  #############################
  
  ####################################################################################################
  # This section inserts a dummy station "-99" into tdata with a sequence of -99s and NAs for the associated variables.
  # The -99 timeseries starts from the oldest date in the entire dataset and ends at the most recent date.
  # The purpose for the dummy data is to create a continous daily timeseries so 7DADM are not calculated
  # between breaks in days for the same station.
  
  datetime99<-as.character(seq(min(tdata$date),max(tdata$date),by=1))
  date99<- as.Date(seq(min(tdata$date),max(tdata$date),by=1))
  id99<-seq(unique(tdata$id),by=0,length.out=length(datetime99))
  name99<-rep(NA,by=1,length.out=length(datetime99))
  unit99<-rep(NA,by=1,length.out=length(datetime99))
  t99<- rep(NA,by=0,length.out=length(datetime99))
  t_c99<-rep(NA,by=0,length.out=length(datetime99))
  zdadm99<-rep(NA,by=1,length.out=length(datetime99))
  spawndates99<-rep(NA,by=1,length.out=length(datetime99))
  
  dummy <-data.frame(cbind(id99,name99,unit99,t99,t_c99))
  dummy<- cbind(dummy,datetime99,date99)
  
  colnames(dummy)[1] <- "id"
  colnames(dummy)[2] <- "name"
  colnames(dummy)[3] <- "unit"
  colnames(dummy)[4] <- "t"
  colnames(dummy)[5] <- "t_c"
  colnames(dummy)[6] <- "datetime"
  colnames(dummy)[7] <- "date"
  
  
  inherits(dummy$date, "Date") # should be FALSE
  is.numeric(dummy$t_c) # should be TRUE
  
  tdata <-rbind(tdata,dummy)
  rm(dummy)
  #############################################################################################
  
  ## Calculate daily maximums by station
  tmax<- tapply(tdata$t_c,list(tdata$date,tdata$id),function(x) {ifelse(all(is.na(x)),NA,max(x, na.rm = TRUE))})
  
  ## Calculate 7DADM
  sdadm<- rollapply(tmax,7,mean, fill=NA, align="right")
  sdadm<- round(sdadm,1)
  
  if (all(is.na(sdadm))) {
    return("Insufficient data to calculate a single 7DADM")
  } else {
    datevector <-as.chron(rownames(tmax))
    sdadm <-data.frame(sdadm)
    sdadm <- cbind(datevector,sdadm)
    colnames(sdadm)[1] <- "date"
    sdadm.melt <- melt.data.frame(sdadm, id.var="date",variable_name = "id")
    colnames(sdadm.melt)[3] <- "sdadm"
    sdadm.melt$id <- gsub("X","",sdadm.melt$id,fixed=TRUE)
    sdadm.melt$id <- gsub(".","-",sdadm.melt$id,fixed=TRUE)
    attr(sdadm.melt, "sdata") <- sdata
    ## finds the current date, and spawning start/end date and formats as a numeric in the form mm.dd
    sdadm.melt$cdate <- as.numeric(months(sdadm.melt$date)) + (as.numeric(days(sdadm.melt$date)) * .01)
    sdadm.melt$sstr <- as.numeric(sdata$SSTART_MONTH[1]) + (as.numeric(sdata$SSTART_DAY[1]) *.01)
    sdadm.melt$send <- as.numeric(sdata$SEND_MONTH[1]) + (as.numeric(sdata$SEND_DAY[1]) *.01)
    sdadm.melt$bioc <- as.numeric(sdata$ZDADM[1])
    
    ## checks to see if there is an over winter spawning period
    sdadm.melt$winter <- ifelse(sdadm.melt$send < sdadm.melt$sstr, TRUE, FALSE)
    
    ## looks up the summer bio criterion and spawning start end/date and returns TRUE/FALSE if current date is in summer or spawning period
    sdadm.melt$bioc <- ifelse(sdadm.melt$winter == TRUE,
                              ifelse(sdadm.melt$sstr <= sdadm.melt$cdate | sdadm.melt$send >= sdadm.melt$cdate, 13, sdadm.melt$bioc),
                              ifelse(sdadm.melt$sstr <= sdadm.melt$cdate & sdadm.melt$send >= sdadm.melt$cdate, 13, sdadm.melt$bioc))
    
    sdadm.melt$summer <- ifelse(sdadm.melt$bioc == 13, FALSE, TRUE)
    sdadm.melt$spawn <- ifelse(sdadm.melt$bioc == 13, TRUE, FALSE)
    
    ## Calculate total 7DADM obersvations and # of 7DADM observations that exceed the summer spawning critera in those time periods; and 
    ## number of 7DADM observations that exceed 16 and 18 over the whole time period (not just in the stated periods)
    sdadm.melt$exceedsummer <- ifelse(sdadm.melt$sdadm >=sdadm.melt$bioc & sdadm.melt$summer == TRUE, TRUE, FALSE)
    sdadm.melt$exceedspawn <- ifelse(sdadm.melt$sdadm >=sdadm.melt$bioc & sdadm.melt$spawn == TRUE, TRUE, FALSE)
    sdadm.melt$daystot <-ifelse(sdadm.melt$sdadm !="NA", TRUE, FALSE)
    return(sdadm.melt)
  }
}
################################################################################################
# FIND THE BIOLOGICAL CRITERION AND SPAWNING PERIOD
# This routine looks up the station ID, grabs the summer bio criterion value, 
# grabs the spawning start/end dates, makes a vector of the bio criteria (bioc), and
# two vectors (summer, spawning) of TRUE/FALSE if the date is in summer or spawing period.
spd <- spawning #input$selectSpawning
spd_list <- strsplit(spd, split = "-")
spd_chron <- lapply(spd_list, function(x) {as.chron(x, format = "%B %d")})
spd_months <- lapply(spd_chron, months)
spd_days <- lapply(spd_chron, days)
spd_months_num <- as.numeric(spd_months[[1]])
spd_days_num <- as.numeric(spd_days[[1]])
SSTART_MONTH <- spd_months_num[1]
SEND_MONTH <- spd_months_num[2]
SSTART_DAY <- spd_days_num[1]
SEND_DAY <- spd_days_num[2]
sdata <- data.frame('id' = unique(strsplit(station,' - ')[[1]][1]), #input$selectStation
                    'spawndates' = spd,
                    'SSTART_MONTH' = SSTART_MONTH, 
                    'SSTART_DAY' = SSTART_DAY,
                    'SEND_MONTH' = SEND_MONTH,
                    'SEND_DAY' = SEND_DAY)
sdata$ZDADM <- switch(use, #input$selectUse
                      'Bull Trout Spawning and Juvenile Rearing' = 12,
                      'Core Cold Water Habitat' = 16,
                      'Salmon and Trout Rearing and Migration' = 18,
                      'Salmon and Steelhead Migration Corridors' = 20,
                      'Redband and Lanhontan Cutthroat Trout' = 20,
                      'Cool water species' = NA,
                      'No Salmonid Use/Out of State' = NA
)
rm(spd,spd_list,spd_chron,spd_months,spd_days,
   spd_months_num,spd_days_num,SSTART_MONTH,
   SSTART_DAY,SEND_MONTH,SEND_DAY)



#sdadm.melt$sdata <- match(sdadm.melt$id,sdata$id)




## Calculate begin/end date by station
datemax <- tapply(as.character(tdata$date),tdata$id,max)
datemin <- tapply(as.character(tdata$date),tdata$id,min)

## TABULUAR RESULTS
daystot <- tapply(sdadm.melt$daystot,list(sdadm.melt$id,sdadm.melt$daystot),length)
exceedsummer <- tapply(sdadm.melt$exceedsummer,list(sdadm.melt$id,sdadm.melt$exceedsummer),length)
exceedspawn <- tapply(sdadm.melt$exceedspawn,list(sdadm.melt$id,sdadm.melt$exceedspawn),length)

## only get TRUE value, dim() checks which column TRUE is in 
## NOTE that if there is no TRUE col it will return the FALSE columns.
## This usually occurs for exceedspawn. Look for unusually high number of exceedances.
## Need to build in a few more lines of code using which(colnames(exceedspawn) == "TRUE") and 
## if it returns a length() = zero it replaces the false col with NAs and grabs those instead.
daystot<- daystot[,dim(daystot)[2]]
exceedsummer <- exceedsummer[,dim(exceedsummer)[2]]
exceedspawn<- exceedspawn[,dim(exceedspawn)[2]]

tdata2 <- cbind(tdata[1],tdata[2],tdata[3],tdata[4],tdata[10], tdata[11])
locdata <- unique(tdata2)
locdata <-locdata[order(locdata[,1]),] 

results <- rbind(datemin,datemax,daystot,exceedsummer, exceedspawn)
results<-t(results)

final <- cbind(locdata,results)

## Write Output
# write.table(final, paste(outpath,tablename,"_final_station_summary.txt",sep=""), sep="\t",row.names = FALSE)
# #write.table(sdadm, paste(outpath,tablename,"_sdadm.txt",sep=""), sep="\t")
# write.table(sdadm.melt, paste(outpath,tablename,"_sdadm_melt.txt",sep=""), sep="\t",row.names = FALSE)

nrow(locdata) -1 # NUMBER of STATIONS (not includeing Dummy)

final

## REMOVE DATA
#rm(dmatrix)
#rm(sdadm)
#rm(sdadm.melt)
#rm(sdata)
#rm(tdata)
#rm(tdata2)
#rm(tmax)

