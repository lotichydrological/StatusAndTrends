#### Define geographic area for search ####
#You can query by the whole state, a county or set of counties or by HUC
#Uncomment the section you wish to use and comment out the other two otptions

#Query the whole state of Oregon
#myArea <- 'US%3A41'

#Query by county
# e.g. a single county: Enter "US:41:001" for Baker County in the function URLencode.PTB("US:41:001")
# e.g. for more than one county: Separate codes with a semi-colon. URLencode.PTB("US:41:001;US:41:003")
#wqp.Counties <- WQP.domain.get('county')
#myArea <- 'US:41:001;US:41:003'

#Query by 8-digit HUC
#Refer to http://water.usgs.gov/GIS/regions.html to identify the HUCS
# Separate multiple by semicolons. 17100203;17100204;17100205
myArea <- '17100307;17100308;17100309;17100310;17100311' #Inland Rogue plus a little lower

#### Define site types to query ####
#Returns list of available domain values for site type
wqp.siteTypes <- WQP.domain.get('Sitetype')

#Using the wqp.siteTypes enter the values you want to query for in siteType.
#Separate each value with the URL encoded semi-colon '%3B'. For values with commas use the URL encoded value '%2C+'
siteType = 'Estuary;Ocean;Stream;Lake, Reservoir, Impoundment'

#### Define sample media to query ####
wqp.sampleMedia <- WQP.domain.get('Samplemedia')

#Separate each value you want to query with the URL encoded semi-colon '%3B'.
sampleMedia <- 'Water'

#### Define characteristics to query ####
#First get the list of Characteristic names from the WQP. These names are consistent with EPA's SRS. 
wqp.characteristics <- WQP.domain.get('Characteristicname')

#The entire list of parameters that match to a criteria
parms <- read.csv('WQP_Table3040_Names.csv', stringsAsFactors = FALSE)

#grab just the parameters we want
characteristics <- paste(parms[parms$WQP.Name %in% c('Temperature, water','pH','Escherichia coli','Fecal Coliform',
                                                     'Fecal coliforms', 'Enterococci', 'Enterococcus'),'WQP.Name'],collapse=';')

#### Define start and end date ####
#The expected format is mm-dd-yyyy
startDate <- '01-01-1995'
endDate <- '02-01-2015'


## Setup Steps ##
# This creates a data source name driver for R to use when connecting to the database

# 1. Open Windows Explorer and navigate to: C:\Windows\SysWOW64\
# 2. Find and open an exe named "odbcad32.exe"
# 3. Click the Tab "System DSN"
# 4. Click > ADD
# 5. Select the driver "SQL Server"
# 6. Name: LASAR2 DEV
#    Description: Leave Blank
#    Server: DEQSQL2\DEV
#   Click NEXT
# 7. Choose "With Windows NT authentication..."
#   Click NEXT
# 8. Change the default database to: LASAR2
#   Click NEXT
# 9. Don't change anything here > Click FINISH
# 10. Click "Test Data Source" to verify it works - Click OK
# 11. Click OK on ODBC screen

# Make sure to use the 32 bit version of R if you are using a 64 bit computer

# If you use R studio set the defualt to 32 bit 
# 1. In R studio go to Tools > Options
# 2. For the R version, Click CHANGE
# 3. Select "Choose a specfic version of R"
# 4. Select [32-bit] C:\Program Files\R\R-...
# 5. Click OK, Click OK.
# 6. Close R Studio and restart again for changes to take affect.

###########################################################

## Load required R packages
# install.packages("RODBC") #Delete first hashtag and run if RODBC is not installed
library(RODBC)

## Designate outfile path and name
thedate <-Sys.Date()
outpath <-"D:/LASAR/"
outfile <- paste("LASAR_Query2_",thedate,".csv",sep="")  

## Make a connection to LASAR2 DEV
channel <- odbcConnect("LASAR2_GIS")

## Grab the names of all the tables in the database
TableNames<- sqlTables(channel,errors = FALSE,schema='dbo')

## Grab all the stations in the database
StationsAll <- sqlFetch(channel, "STATION")


## Grab all the Area Geography and associated codes
AreaClass <- sqlFetch(channel, "XLU_AREA_CLASS")
AreaKeys <- sqlFetch(channel, "XLU_AREA")
StationAreas <- sqlFetch(channel,"STATION_AREA")

##Find the area key for Willamette and Umatilla Basins 
HUC5 <- c("1707010301",
          "1707010302", "1707010303", "1707010304", "1707010305", "1707010306", 
          "1707010307", "1707010308", "1707010309", "1707010310", "1707010311", 
          "1707010312", "1707010313", "1709000101", "1709000102", "1709000103", 
          "1709000104", "1709000105", "1709000106", "1709000107", "1709000108", 
          "1709000109", "1709000110", "1709000201", "1709000202", "1709000203", 
          "1709000205", "1709000301", "1709000302", "1709000303", "1709000304", 
          "1709000305", "1709000306", "1709000401", "1709000402", "1709000403", 
          "1709000404", "1709000405", "1709000406", "1709000407", "1709000501", 
          "1709000502", "1709000503", "1709000504", "1709000505", "1709000506", 
          "1709000601", "1709000602", "1709000603", "1709000604", "1709000605", 
          "1709000606", "1709000607", "1709000608", "1709000701", "1709000702", 
          "1709000703", "1709000704", "1709000801", "1709000802", "1709000803", 
          "1709000804", "1709000805", "1709000806", "1709000807", "1709000901", 
          "1709000902", "1709000903", "1709000904", "1709000905", "1709000906", 
          "1709001001", "1709001002", "1709001003", "1709001004", "1709001005", 
          "1709001101", "1709001102", "1709001103", "1709001104", "1709001105", 
          "1709001106", "1709001201", "1709001202", "1709001203")

HUC5 <- as.integer(HUC5) # Need to be as integer because numeric values can have 'junk' extending out to 16 decimal places


# Here's an alternative search string.  This produces 31 more HUC5s; maybe due to missing values from the first example?
HUC5.2<-c("170900.{4}$","170701.{4}$")

HUC5.2.list<-replicate(length(HUC5.2),list()) #empty list for global search
HUC5.2.1<-character(0) #empty character string

for (i in 1:length(HUC5.2)){
HUC5.2.list[[i]]<-as.vector(AreaKeys$AREA_ABBREVIATION[grep(HUC5.2[i],as.character(AreaKeys$AREA_ABBREVIATION))])
HUC5.2.1<-c(HUC5.2.1,HUC5.2.list[[i]])
}

HUC5.2.2<-as.integer(HUC5.2.1)

# Even better, here's an external file where you can enter your search string (.csv file should have been pushed back to GitHub)
HUC5.3<-read.csv("HUC5.csv",colClasses="character")
HUC5.3<-paste0(HUC5.3$HUC5,".{4}$") # Adds in search for all HUC10s

HUC5.3.list<-replicate(length(HUC5.3),list()) #empty list for global search
HUC5.3.1<-character(0) #empty character string

for (i in 1:length(HUC5.3)){
  HUC5.3.list[[i]]<-as.vector(AreaKeys$AREA_ABBREVIATION[grep(HUC5.3[i],as.character(AreaKeys$AREA_ABBREVIATION))])
  HUC5.3.1<-c(HUC5.3.1,HUC5.3.list[[i]])
}

HUC5.3.2<-as.integer(HUC5.3.1)

####

myAreas <- AreaKeys[AreaKeys$AREA_ABBREVIATION %in% HUC5,] # Try HUC5.2.2 and HUC5.3.2 to see that those work
myAreaKeys <- myAreas$AREA_KEY

##Get a list of all the stations in my area
#AreaStations <- subset(StationAreas, XLU_AREA == myAreaKey)
AreaStations <- StationAreas[StationAreas$XLU_AREA %in% myAreaKeys,]
myStations <- as.integer(AreaStations$STATION) # Should be an integer
# 
# myStationsdf <- sqlQuery(access.con, "SELECT * FROM FINALReferenceData_SMiller_may2014 
#                        WHERE agency = 'DEQ'")
# myStations <- myStationsdf$agency_ID
# myStations <- as.character(myStations[1:1025])

##Restrict stations to types of interest
#UsedUses <- XLU_STATION_USE[XLU_STATION_USE$XLU_STATION_USE_KEY %in% unique_use,]
#XLU_STATION_USE - This is a list of the station use codes that are actually used.
# XLU_STATION_USE_KEY DESCRIPTION
# 32  Beaches
# 51	Domestic Supply
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
UseCodes <- c(32, 63, 79, 97, 98, 103, 104, 107, 112, 119, 120, 124, 126, 127, 128, 129)
STATION_USE <- sqlFetch(channel, "STATION_USE")
Stations_with_use <- STATION_USE[STATION_USE$XLU_STATION_USE %in% UseCodes,]
myStations <- myStations[myStations %in% Stations_with_use$STATION]

## Data Types - eg. grab, continuous data
datatypes <- sqlFetch(channel, "XLU_LASAR_DATA")
# LASAR_DATA_KEY     DATA_DESCRIPTION
# 1              0  Grab and Continuous
# 2              1       Grab data only
# 3              2 Continuous data only
myDatatype <- 1

## Grab a list of all the parameters and numerical codes
AllParameters <- sqlFetch(channel, "XLU_LASAR_PARAMETERS")

AllParameters[AllParameters$PARAMETER_NM == 'Temperature',]
## Find the code for Field Temperature (Note the space at the end) in units of Celcius
#Tempcodes <- subset(AllParameters, INDEX_COLUMN == "Field Temperature " & UNIT == 21)

#Potentially useful search of code names:
#DOcodes <- AllParameters[grep("Dissolved Oxygen", AllParameters$INDEX_COLUMN),]
## XLU_LASAR_PARAMETERS_KEY = Index Column
#379 = Percent Saturation Field Dissolved Oxygen [most common for continuous] 
#1847 = Dissolved Oxygen
#2823 = Field Dissolved Oxygen [most common for continuous]
#2906 = Dissolved Oxygen 
#4759 = Percent Saturation Field Dissolved Oxygen 
## XLU_LASAR_PARAMETERS_KEY /  Parameter / UNIT / Unit Code
# 975   / Field Temperature / C / 21
# 2215  / Field Temperature / F / 22
# 11769 / Air Temperature / C / 21

## Set the water quality parameter
#myParams <- c(379, 1847, 2823, 2906, 4759)
myParams.s <- "379, 1847, 2823, 2906, 4759"


### Set the Date/Time Range
myStartdate <- "2000-01-01 00:00:00"
myEnddate <- "2012-12-31 00:00:00"

myQuery <- c()

# for (station in myStations) {
#   for (param in myParams){
#     qry <- paste("SELECT * FROM Result WHERE (Station =",station,") AND (XLU_LASAR_PARAMETER =",param,") AND (SAMPLE_DATE_TIME >='",myStartdate,"') AND (SAMPLE_DATE_TIME <='",myEnddate,"')", sep="")
#     myQuery <- append(myQuery, qry)
#     }
#   }

for (station in myStations) {
  qry <- paste("SELECT * FROM Result WHERE (Station =",station,") AND (XLU_LASAR_PARAMETER IN (",myParams.s,")) AND (SAMPLE_DATE_TIME >='",myStartdate,"') AND (SAMPLE_DATE_TIME <='",myEnddate,"')", sep="")
  myQuery <- append(myQuery, qry)
  }

for (station in myStations) {
  qry <- paste("SELECT * FROM Result WHERE Station =",station, sep="")
  myQuery <- append(myQuery, qry)
}

## Retreive data.
for(i in 1:length(myQuery)) {
  print(myQuery[i])
  data <- sqlQuery(channel,myQuery[i],stringsAsFactors = FALSE, na.strings = "NA")
  ifelse(i==1,mydata <- data, mydata <- rbind(mydata,data))
  rm(data)
  }


myQuery <- paste("SELECT * FROM Result WHERE Station in (", paste(myStations, collapse = ","), ") AND XLU_LASAR_PARAMETER in (", paste(myParams.s, collapse = ","), ")",sep = "")
mydata <- sqlQuery(channel, myQuery)

close(channel)
#plot(mydata$SAMPLE_DATE_TIME, mydata$RESULT) 

#Bring in the parameter name
mydata <- merge(mydata, 
                AllParameters[,c('XLU_LASAR_PARAMETERS_KEY','PARAMETER_NM','INDEX_COLUMN')], 
                by.x = "XLU_LASAR_PARAMETER",
                by.y = 'XLU_LASAR_PARAMETERS_KEY', 
                all.x = TRUE)

check <- sqlQuery(channel, "SELECT r.SAMPLING_ORGANIZATION, COUNT(*) AS 'num' FROM Result r LEFT JOIN 
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
              ORGANIZATION o on r.SAMPLING_ORGANIZATION = o.ORGANIZATION_KEY WHERE r.SAMPLING_ORGANIZATION is NULL GROUP BY r.SAMPLING_ORGANIZATION")
check <- merge(check, sqlFetch(channel, 'ORGANIZATION'), by.x = 'SAMPLING_ORGANIZATION', by.y = 'ORGANIZATION_KEY', all.x = TRUE)

check2 <- sqlQuery(channel, "SELECT r.QA_QC_STATUS, COUNT(*) AS 'num' FROM Result r LEFT JOIN 
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
              ORGANIZATION o on r.SAMPLING_ORGANIZATION = o.ORGANIZATION_KEY WHERE r.SAMPLING_ORGANIZATION = 0 GROUP BY QA_QC_STATUS")
check2 <- merge(check2, sqlFetch(channel, 'XLU_STATUS'), by.x = 'QA_QC_STATUS', by.y = 'XLU_STATUS_KEY', all.x = TRUE)

qry <- paste("SELECT r.RESULT_KEY,
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
                   PARAMETER_RESULT pr on r.PARAMETER_RESULT = pr.PARAMETER_RESULT_KEY
              ORGANIZATION o on r.SAMPLING_ORGANIZATION = o.ORGANIZATION_KEY
              WHERE a.AREA_ABBREVIATION in ('17090012') AND
                    r.SAMPLE_DATE_TIME < '2010-12-31 00:00:00.000' AND
                    r.SAMPLE_DATE_TIME > '2010-10-31 00:00:00.000' AND
                    st.STATUS in ('A+','A','B') AND
                    sm.SAMPLE_MATRIX_NAME in ('Surface water', 'Bay/Estuary/Ocean', 'Canal', 'Reservoir', 'Lake',
                                              'Ditch/Pond/Culvert/Drain')
              ")
mydata <- sqlQuery(channel, qry)

qry2 <- paste("SELECT DISTINCT
             pm1.ABBREVIATION as PARAMETER_PREFIX_1,
             pm2.ABBREVIATION as PARAMETER_PREFIX_2,
             p.PARAMETER_NM,
             pm3.ABBREVIATION as PARAMETER_SUFFIX_1,
             pm4.ABBREVIATION as PARAMETER_SUFFIX_2
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
             ORGANIZATION o on r.SAMPLING_ORGANIZATION = o.ORGANIZATION_KEY
             WHERE 
             st.STATUS in ('A+','A','B') AND
             sm.SAMPLE_MATRIX_NAME in ('Surface water', 'Bay/Estuary/Ocean', 'Canal', 'Reservoir', 'Lake',
             'Ditch/Pond/Culvert/Drain')
             ")
mydata2 <- sqlQuery(channel, qry2)

## Write ouput to file
#write.csv(mydata, paste(outpath,outfile,sep=""))
