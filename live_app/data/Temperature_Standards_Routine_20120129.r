###########################################################
# Version 20120129 | Ryan Michie | ODEQ
# This code brings in multi station temperature data 
# from a 2007 Access database formatted using LASAR conventions and calculates the
# 
# -Daily Maximum
# -7day average daily maximum
# -number of days a station exceeds the standard (both summer and spawning)
# -summarizes location and standards information
#                                             
# REQUIREMENTS: 
# LASAR data file must contain all the data columns from LASAR
# A .CSV file summarizing standards info by station
# ALL STATION DATA MUST BE FREE OF these symbols: , # '
###########################################################

## Load required packages
library(chron)
library(RODBC)
library(zoo)
library(reshape)

###########################################################
## Designate path and file name for temperature data 

inpath <- "//DEQHQ1/TMDL/TMDL_WR/MidCoast/Data/Temperature/LASAR/"
#inpath <- "//DEQHQ1/TMDL/TMDL_WR/MidCoast/Data/Temperature/USFS/"
#inpath <- "/Users/rmichie/Desktop/R/"

outpath <- "//DEQHQ1/TMDL/TMDL_WR/MidCoast/Data/Temperature/R/"

#infile <-"LASARresultsTest.csv"
#tablename <- "Test"

#infile <-"LASARResults_Alsea_20120131.accdb"
#tablename <- "Alsea_20120131"

infile <-"LASARResults_Drift_Creek_20120501.accdb"
tablename <- "Drift_Creek_20120501"

#infile <-"LASARResults_SiletzYaquina_20101014.accdb"
#tablename <- "SiletzYaquina_20101014"

#infile <-"LASARResults_Siltcoos_20120131.accdb"
#tablename <- "Siltcoos_20120131"

#infile <-"LASARResults_Siuslaw_20101014.accdb"
#tablename <- "Siuslaw_20101014"

#infile <-"USFS_MidCoast_20101117.accdb"
#tablename <- "USFS_MidCoast_20101117"

###########################################################
## Designate the data source 
## LASAR = 1, USFS = 0

dsource <- 1

###########################################################
## Designate path and file name for station standards information

spath <- "//DEQHQ1/TMDL/TMDL_WR/MidCoast/Data/Temperature/Stations/"
sfile <-"Stations_20120501.csv"
sworkingfile <- paste(spath,sfile,sep = "")
sdata <- read.table(sworkingfile, header=TRUE, sep=",", na.strings="NA" ,check.names=FALSE, stringsAsFactors=FALSE, fill=TRUE)

###########################################################
## READ DATA FROM ACCESS 2007
dworkingfile <- paste(inpath,infile,sep = "")
channel <-odbcConnectAccess2007(dworkingfile)
tdata.raw <- sqlFetch(channel, tablename)
#tdata <- sqlQuery(channel, paste("SELECT * ",tablename,sep=""))
close(channel)

## READ DATA FROM .CSV
#dworkingfile <- paste(inpath,infile,sep = "")
#tdata.raw <- read.table(dworkingfile, header=TRUE, sep=",", na.strings="NA", dec=".", check.names=FALSE,stringsAsFactors=FALSE)
###########################################################

#############################
# LASAR COLUMNS for  tdata.raw
# tdata[5] "c"
# tdata[7] "Station Description"
# tdata[8] "Latitude"
# tdata[9] "Longitude"
# tdata[13] "Sample Date Time"
# tdata[30] "PARAMETER NAME"
# tdata[31] "RESULT"

# CAS NUMBER
# 999990033  - Temperature

# USFS COLUMNS for tdata.raw
# tdata[1] "Station Identifier"
# tdata[5] "Station Description"
# tdata[7] "Latitude"
# tdata[8] "Longitude"
# tdata[10] "Sample Date Time"
# tdata[14] "RESULT"
# tdata[15] "PARAMETER NAME"
#############################

## Narrow Down Data
if(dsource== 1) { # LASAR DATA
  tdata <- cbind(tdata.raw[5], (tdata.raw[7]), tdata.raw[8],tdata.raw[9],tdata.raw[13],tdata.raw[30], tdata.raw[31])
  } else { # USFS DATA
  tdata <- cbind(tdata.raw[1], (tdata.raw[5]), tdata.raw[7],tdata.raw[8],tdata.raw[10],tdata.raw[15], tdata.raw[14])
  }

## RENAME
colnames(tdata)[1] <- "id"
colnames(tdata)[2] <- "name"
colnames(tdata)[3] <- "lat"
colnames(tdata)[4] <- "long"
colnames(tdata)[5] <- "datetime"
colnames(tdata)[6] <- "unit"
colnames(tdata)[7] <- "t"

## CLEAR SOME MEMORY
rm(tdata.raw)

## F -> C  Not a perfect solution but not sure how to deal with it otherwise.
#Ft <- "Temperature  (?F)" # Using the unit text doesn't seem to work on a pc (works on mac). I think the degree symbol is the problem.
tdata$t_c <- ifelse(tdata$t > 36, round(((tdata$t-32)*5/9),1),tdata$t)
is.numeric(tdata$t_c)

## Create a vector of daily dates for grouping
tdata$date <- as.Date(tdata$datetime, format="%m/%d/%Y")
inherits(tdata$date, "Date") # checks to see if it is class Date

## Adds a vector of the summer period bio criterion and the Spawning period
tdata$zdadm <- as.numeric(sdata$ZDADM[match(tdata$id,sdata$station)])
tdata$spawndates <- sdata$SPN_FINAL[match(tdata$id,sdata$station)]

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
date99<- as.character(seq(min(tdata$date),max(tdata$date),by=1))
id99<-seq(-99,by=0,length.out=length(datetime99))
name99<-rep(NA,by=1,length.out=length(datetime99))
lat99<-rep(NA,by=1,length.out=length(datetime99))
long99<-rep(NA,by=1,length.out=length(datetime99))
unit99<-rep(NA,by=1,length.out=length(datetime99))
t99<- seq(-99.9,by=0,length.out=length(datetime99))
t_c99<-seq(-99.9,by=0,length.out=length(datetime99))
zdadm99<-rep(NA,by=1,length.out=length(datetime99))
spawndates99<-rep(NA,by=1,length.out=length(datetime99))

dummy <-data.frame(cbind(id99,name99,lat99,long99,unit99,t99,t_c99,zdadm99,spawndates99))
dummy<- cbind(dummy,datetime99,date99)

colnames(dummy)[1] <- "id"
colnames(dummy)[2] <- "name"
colnames(dummy)[3] <- "lat"
colnames(dummy)[4] <- "long"
colnames(dummy)[5] <- "unit"
colnames(dummy)[6] <- "t"
colnames(dummy)[7] <- "t_c"
colnames(dummy)[8] <- "zdadm"
colnames(dummy)[9] <- "spawndates"
colnames(dummy)[10] <- "datetime"
colnames(dummy)[11] <- "date"


inherits(dummy$date, "Date") # should be FALSE
is.numeric(dummy$t_c) # should be TRUE

tdata <-rbind(tdata,dummy)
rm(dummy)
#############################################################################################

## Calculate daily maximums by station
tmax<- tapply(tdata$t_c,list(tdata$date,tdata$id),max)

## Calculate 7DADM
sdadm<- rollapply(tmax,7,mean, fill=TRUE, align="right")
sdadm<- round(sdadm,1)

################################################################################################
# FIND THE BIOLOGICAL CRITERION AND SPAWNING PERIOD
# This routine looks up the station ID, grabs the summer bio criterion value, 
# grabs the spawning start/end dates, makes a vector of the bio criteria (bioc), and
# two vectors (summer, spawning) of TRUE/FALSE if the date is in summer or spawing period.

datevector <-as.chron(rownames(tmax))
dmatrix <- matrix(datevector)
colnames(dmatrix)[1] <- "date"
sdadm <- cbind(dmatrix,sdadm)
sdadm <-data.frame(sdadm)
sdadm.melt <- melt.data.frame(sdadm, id.var="date",variable_name = "id")
colnames(sdadm.melt)[3] <- "sdadm"
sdadm.melt$id <- gsub("X","",sdadm.melt$id,fixed=TRUE)
sdadm.melt$id <- gsub(".","-",sdadm.melt$id,fixed=TRUE)

sdadm.melt$sdata <- match(sdadm.melt$id,sdata$station)

## finds the current date, and spawning start/end date and formats as a numeric in the form mm.dd
sdadm.melt$cdate <- as.numeric(months(sdadm.melt$date)) + (as.numeric(days(sdadm.melt$date)) * .01)
sdadm.melt$sstr <- as.numeric(sdata$SSTART_MONTH[sdadm.melt$sdata]) + (as.numeric(sdata$SSTART_DAY[sdadm.melt$sdata]) *.01)
sdadm.melt$send <- as.numeric(sdata$SEND_MONTH[sdadm.melt$sdata]) + (as.numeric(sdata$SEND_DAY[sdadm.melt$sdata]) *.01)
sdadm.melt$bioc <- as.numeric(sdata$ZDADM[sdadm.melt$sdata])

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


## Calculate begin/end date by station
datemax <- tapply(as.character(tdata$date),tdata$id,max)
datemin <- tapply(as.character(tdata$date),tdata$id,min)

## PLOTS
## boxplot of monthly sdadm for each station
#monthvector <- months(as.chron(rownames(tmax)))
#boxplot(sdadm[,6]~monthvector,xlab="Month", ylab="7-Day Average Daily Maximum Temperature (C)",ylim=c(0,25)) 
#plot(datevector,sdadm[,4], type="l", ylim=c(0,24))

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
write.table(final, paste(outpath,tablename,"_final_station_summary.txt",sep=""), sep="\t",row.names = FALSE)
#write.table(sdadm, paste(outpath,tablename,"_sdadm.txt",sep=""), sep="\t")
write.table(sdadm.melt, paste(outpath,tablename,"_sdadm_melt.txt",sep=""), sep="\t",row.names = FALSE)

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

