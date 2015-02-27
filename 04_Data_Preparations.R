#### This file's purpose is to first compile raw data from each data source, the
#### Water Quality Portal, LASAR and the Call for Data from City of Gresham.
#### Then it standardizes the datasets and applies the decision rules outlined
#### in the Assessment Methodology for compiling and determining applicable criteria.
#### Finally, the comparison to the standard is made and valid sample logic applied.

#### Set up libraries to be used ####
library(plyr)
library(reshape2)
library(stringr)
library(RODBC)
library(foreign)
library(xlsx)

options(stringsAsFactors = FALSE, scipen = 100)

#Pull in criteria determination calculation functions
source('//deqhq1/wqassessment/2012_WQAssessment/ToxicsRedo/TMP-RCode/hardness_eval_functions_Element_Names.R')

#### Pull in the raw data and criteria ####
con <- odbcConnect('WQAssessment')
wqp.data <- sqlFetch(con, 'WQPData_wND_07162014')
wqp.stations <- sqlFetch(con, 'WQPStations_wUpdatedLatLon_06132014')
lasar <- sqlFetch(con, 'LASAR_Toxics_Query_wcriterianame_wAddOns_07082014')
odbcCloseAll()
gresham <- read.csv('//deqhq1/wqassessment/2012_WQAssessment/WQ_2012_Assessment_Raw/Gresham/CFD_Gresham_2012_Analytical_Data.csv')

#Pull in the compiled criteria table used for the Toxics Monitoring prgram
source('//deqhq1/wqassessment/2012_WQAssessment/ToxicsRedo/TMP-RCode/criteria.R')

#The names in the criteria table differ from the names in Tables30/40 and differ from the names in the WQAssessment database
#This file was created to resolve some of those differences
wqp.name.match <- read.csv('WQPNameMatch_05142014.csv',stringsAsFactors=FALSE)
#Because the TMP was interested in a wider range of Pollutants they looked at EPA benchmarks too. We are just interested
#DEQ Standards here
deq.pollutants <- criteria.values.melted.applicable[criteria.values.melted.applicable$variable %in% 
                                                      c('Table 40 Human Health Criteria for Toxic Pollutants - Water + Organism',
                                                        'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only',
                                                        'Table 30 Toxic Substances - Freshwater Acute',
                                                        'Table 30 Toxic Substances - Freshwater Chronic',
                                                        'Table 30 Toxic Substances - Saltwater Acute',
                                                        'Table 30 Toxic Substances - Saltwater Chronic'),]
#Putting these with the name translations helps associate everything more clearly as we move forward
Table3040.applicable <- merge(deq.pollutants, wqp.name.match[,c('Criteria.Name','DEQ.Table.name')], by.x = 'Pollutant', by.y = 'Criteria.Name')
Table3040.applicable <- rename(Table3040.applicable, c('Pollutant' = 'Criteria.Name.full', 'DEQ.Table.name' = 'criterianame'))
#The criteria were populated above for FW and SW designations only. Because the AM on page 60 specifies Estuaries take the most stringent 
#of saltwater and freshwater it is easier to just duplicate everything here and assign a new Matrix category of ES.
T3040.ES <- Table3040.applicable
T3040.ES$Matrix <- 'ES'
T3040.ES$ID <- paste(T3040.ES$criterianame, T3040.ES$Matrix)
#This will remove unnecessary duplicates where the saltwater and freshwater criteria were the same
T3040.ES$x <- apply(T3040.ES[,names(T3040.ES)],1,paste,collapse=',')
T3040.ES <- T3040.ES[!duplicated(T3040.ES$x),]
T3040.ES <- within(T3040.ES, rm(x))
#Beacuse Estuaries aren't used as drinking water sources the Table 40 criteria for Water + Organism doesn't apply except where there is no
#criteria for Organism Only so we isolate those here to add in after we remove the Water + Organism criteria for estuaries
wo.to.keep <- T3040.ES[T3040.ES$criterianame %in% c('Barium','Chlorophenoxy Herbicide (2,4,5,-TP)','2,4-D','Copper','Methoxychlor','Nitrates') & 
                         T3040.ES$variable == 'Table 40 Human Health Criteria for Toxic Pollutants - Water + Organism',]
T3040.ES <- T3040.ES[T3040.ES$variable != 'Table 40 Human Health Criteria for Toxic Pollutants - Water + Organism',]
T3040.ES <- rbind(T3040.ES, wo.to.keep)
#Here we put the Estuary criteria back into the table
Table3040.applicable <- rbind(Table3040.applicable, T3040.ES)
#In order to rbind we make the variable column character instead of a factor
Table3040.applicable$variable <- as.character(Table3040.applicable$variable)
#We have a benchmark we use for Orthophosphate as P
Table3040.applicable <- rbind(Table3040.applicable, c('Criteria.name.full' = 'Phosphate Phosphorus', 'variable' = 'EPA Benchmark', 'value' = 50, 'Matrix' = 'FW', 'ID' = 'Phosphate Phosphorus FW', 'criterianame' = 'Phosphate Phosphorus'))
#For now I will take care of this here because there are permissions issues on the criteria table at the lab
Table3040.applicable <- Table3040.applicable[Table3040.applicable$Criteria.Name.full != 'Chromium, Dissolved',]
Table3040.applicable[Table3040.applicable$Criteria.Name.full == 'Chromium (Hex)','Criteria.Name.full'] <- 'Chromium (Hex), Dissolved'
#For some reason criteria name full for Mercury doesn't indicate that it's for Total recoverable
Table3040.applicable[Table3040.applicable$Criteria.Name.full == 'Mercury','Criteria.Name.full'] <- 'Mercury, Total recoverable'
#Similarly Arsenic isn't handled well at this point either
Table3040.applicable <- Table3040.applicable[Table3040.applicable$Criteria.Name.full != 'Arsenic, Total inorganic',]
#Not sure why Azinphos methyl is persisted as a Criteria.Name.full
Table3040.applicable <- Table3040.applicable[which(Table3040.applicable$Criteria.Name.full != 'Azinphos methyl'),]

#### First the Water Quality Portal data ####
#Remove Bed Sediment and Suspended samples
wqp.data <- wqp.data[!wqp.data$ResultSampleFractionText %in% c('Bed Sediment','Suspended'),]

#Remove samples labeled as Interstitial
wqp.data <- wqp.data[which(wqp.data$ActivityMediaSubdivisionName != 'Interstitial'),]

#Make a date-time column
wqp.data$Sampled <- paste(wqp.data$ActivityStartDate, wqp.data$ActivityStartTimeTime)

#Make a column with criteria name based on earlier mapping 
wqp.data <- merge(wqp.data, unique(wqp.name.match[,c('WQP.Name','DEQ.Table.name')]), by.x = 'CharacteristicName', by.y = 'WQP.Name', all.x = TRUE)
wqp.data <- rename(wqp.data, c('DEQ.Table.name' = 'criterianame'))
wqp.data$criterianame <- ifelse(is.na(wqp.data$criterianame),wqp.data$CharacteristicName,wqp.data$criterianame)

#Pull in site_only so when we build summary info later it will fit that formatting better
wqp.data <- merge(wqp.data, wqp.stations[,c('MonitoringLocationIdentifier','site_only')], by = 'MonitoringLocationIdentifier', all.x = TRUE)
wqp.data$site_only <- gsub('USGS-|11NPSWRD-|NARSTEST-|R10PORTLANDHARBOR-','',wqp.data$site_only)

#we need to put in a detect/nondetect column here so we can accomodate the specific data sources qualifiers
wqp.data[grep('Original Qualifier: U',wqp.data$ResultCommentText),'ResultMeasureValue'] <- 0
wqp.data$ResultMeasureValue <- as.numeric(wqp.data$ResultMeasureValue)
wqp.data$DetectionQuantitationLimitMeasureMeasureValue <- as.numeric(wqp.data$DetectionQuantitationLimitMeasureMeasureValue)
wqp.data$dnd <- ifelse(wqp.data$ResultMeasureValue == 0,
                       0,
                       ifelse(is.na(wqp.data$DetectionQuantitationLimitMeasureMeasureValue),
                              1,
                              ifelse(wqp.data$ResultMeasureValue < wqp.data$DetectionQuantitationLimitMeasureMeasureValue,
                                     0,
                                     1)))

#Let's pull out only those columns we need to make this business work
wqp.data.sub <- wqp.data[,c('site_only','OrganizationFormalName',
                            'Sampled', 'CharacteristicName','ResultSampleFractionText',
                            'ResultMeasureValue','ResultMeasureMeasureUnitCode','MeasureQualifierCode',
                            'ActivityTypeCode','DetectionQuantitationLimitMeasureMeasureValue','DetectionQuantitationLimitMeasureMeasureUnitCode',
                            'MonitoringLocationName', 'criterianame','ResultAnalyticalMethodMethodIdentifier', 'dnd')]

#Now we make the names match the script I built for the Toxics Monitroing Program
wqp.data.sub <- rename(wqp.data.sub, c('site_only' = 'SampleRegID',
                                       'OrganizationFormalName' = 'Agency',
                                       'CharacteristicName' = 'Name',
                                       'ResultSampleFractionText' = 'Fraction',
                                       'ResultMeasureValue' = 'tResult',
                                       'ResultMeasureMeasureUnitCode' = 'Unit',
                                       'MeasureQualifierCode' = 'Status',
                                       'ActivityTypeCode' = 'SampleType',
                                       'DetectionQuantitationLimitMeasureMeasureValue' = 'tMRL',
                                       'DetectionQuantitationLimitMeasureMeasureUnitCode' = 'tMRLUnit',
                                       'MonitoringLocationName' = 'SampleAlias',
                                       'ResultAnalyticalMethodMethodIdentifier' = 'SpecificMethod'))

#These are the column names from the original script I built for the TMP
# str(data.wo.void)
# 'data.frame':  163574 obs. of  15 variables:
#   $ Analyte         : chr  "1-Methylphenanthrene" "1-Methylphenanthrene" "1-Methylphenanthrene" "1-Methylphenanthrene" ...
# $ Project         : chr  "Mid Coast" "South Coast" "Owyhee" "North Coast" ...
# $ SampleRegID     : int  37399 28303 10730 13654 37399 10990 20434 12962 11047 34309 ...
# $ SampleAlias     : chr  "Umpqua River at Discovery Center Docks" "Elk Creek at ODFW Hatchery" "Owyhee River at Rome (Hwy.95)" "Necanicum R at 12th Street approach" ...
# $ Sampled         : chr  "11/19/2013 9:45" "10/28/2013 13:00" "9/23/2013 9:25" "12/2/2013 14:25" ...
# $ SampleType      : chr  "Grab Sample" "Grab Sample" "Grab Sample" "Grab Sample" ...
# $ Matrix          : chr  "Estuary" "River/Stream" "River/Stream" "Estuary" ...
# $ tResult         : num  0 0 0 0 0 0 0 0 0 0 ...
# $ tMRL            : num  5.93 5.89 5.93 5.37 5.36 5.86 6.15 5.42 5.95 5.91 ...
# $ Unit            : chr  "ng/L" "ng/L" "ng/L" "ng/L" ...
# $ SpecificMethod  : chr  "EPA 8270D" "EPA 8270D" "EPA 8270D" "EPA 8270D" ...
# $ Status          : chr  "A" "A" "B" "B" ...
# $ chem.group      : chr  "Combustion By-Products" "Combustion By-Products" "Combustion By-Products" "Combustion By-Products" ...
# $ Detect.nondetect: num  0 0 0 0 0 0 0 0 0 0 ...
# $ code            : chr  "1-Methylphenanthrene 37399 11/19/2013 9:45" "1-Methylphenanthrene 28303 10/28/2013 13:00" "1-Methylphenanthrene 10730 9/23/2013 9:25" "1-Methylphenanthrene 13654 12/2/2013 14:25" ...

#### Now the lasar data ####
#Make a numeric result column
lasar$Result_clean <- as.numeric(lasar$Result_clean)

#Populate the Criteria.Name.full column so we can have a separate column tracking to the criteria name with fraction
lasar.names.match <- read.csv('lasar_names_match.csv',stringsAsFactors = FALSE)
lasar.names.match <- rename(lasar.names.match, c('Pollutant' = 'Criteria.Name.full'))
lasar <- within(lasar, rm(criterianame))
lasar <- merge(lasar, lasar.names.match, by.x = 'NAME', by.y = 'lasar.name', all.x = TRUE)
#This removes unnecessary duplicates
lasar$x <- apply(lasar[,names(lasar)],1,paste,collapse=',')
lasar <- lasar[!duplicated(lasar$x),]
lasar <- within(lasar, rm(x))
#Pulls in the updated criterianame column
lasar <- merge(lasar, unique(Table3040.applicable[,c('Criteria.Name.full','criterianame')]), by = 'Criteria.Name.full', all.x = TRUE)
lasar$criterianame <- ifelse(is.na(lasar$criterianame),ifelse(lasar$NAME == 'Chromium','Chromium VI',lasar$NAME),lasar$criterianame)

#Make recoverable lower case to be consistent 
lasar$ABBREVIATION <- gsub('R','r',lasar$ABBREVIATION)

#Make a date-time field
lasar$Sampled <- paste(as.character(lasar$SAMPLE_DATE), as.character(lasar$SAMPLE_TIME)) #i had this in here because the lasar date and time were coming in as posix but today they come in as character so i'm taking it out: substr(as.character(lasar$SAMPLE_TIME),12,19)

#see how much diff there is with method_detection_limit and method_reporting_limit
#View(lasar[which(lasar$METHOD_DETECTION_LIMIT != lasar$METHOD_REPORTING_LIMIT),])
#we're going with METHOD_REPORTING_LIMIT

#found some hanging bad QA QC Types from lasar data set
lasar <- lasar[!lasar$QA_QC_TYPE %in% c('Equipment Blank - Field', 
                                        'Matrix Spike - Field', 
                                        'Matrix Spike Duplicate - Field', 
                                        'Transfer Blank'),]

#We need to put detect/non-detect in here since it is based on the old Result column from lasar
lasar$Result_clean <- as.numeric(lasar$Result_clean)
lasar$METHOD_REPORTING_LIMIT <- as.numeric(lasar$METHOD_REPORTING_LIMIT)
lasar$dnd <- ifelse(grepl('<',lasar$Result),
                    0,
                    ifelse(is.na(lasar$METHOD_REPORTING_LIMIT),
                           1,
                           ifelse(lasar$Result_clean < lasar$METHOD_REPORTING_LIMIT,
                                  0,
                                  1)))
lasar[grepl('<',lasar$Result),'METHOD_REPORTING_LIMIT'] <- lasar[grepl('<',lasar$Result),'Result_clean']

#Make the lasar names match the script and be consistent with the new wqp names
lasar.new.names <- rename(lasar, c('NAME' = 'Name',
                                   'ABBREVIATION' = 'Fraction',
                                   'STATION_KEY' = 'SampleRegID',
                                   'LOCATION_DESCRIPTION' = 'SampleAlias',
                                   'QA_QC_TYPE' = 'SampleType',
                                   'Result_clean' = 'tResult',
                                   'METHOD_REPORTING_LIMIT' = 'tMRL',
                                   'UNIT' = 'Unit',
                                   'STATUS' = 'Status',
                                   'dnd' = 'dnd'))
lasar.new.names$Agency <- 'ODEQ'
lasar.new.names$tMRLUnit <- lasar.new.names$Unit

#I had to pull method out from the lasar dataset since I didn't know where to get accurate method information related
#to the method_key in the parameter_result table. Until i find out where to get that info I'll populate a column with NAs
lasar.new.names$SpecificMethod <- NA



#This subsets the lasasr dataframe to only have the columns to be used for aggregation
lasar.new.names <- lasar.new.names[,names(wqp.data.sub)]

#### Need to make sure to include the gresham data too ####
#Format the date and time columns
gresham$posix_date <- as.POSIXct(strptime(gresham$SAMPLE_DATE, format = '%m/%d/%Y'))
gresham$posix_time <- as.POSIXct(strptime(gresham$SAMPLE_TIME, format = '%H:%M'))
#make a date-time field
gresham$Sampled <- paste(as.character(gresham$posix_date), substr(as.character(gresham$posix_time),12,19))
#Make a numeric tResult column
gresham$tResult <- ifelse(is.na(as.numeric(gresham$RESULT)),0,as.numeric(gresham$RESULT))
#Determine detect/non-detect status. For gresham this is easy because the NDs were reported as <MRL which were converted to 0 in the last line.
gresham$dnd <- ifelse(gresham$tResult == 0,0,1)
#Make a Fraction column for teh metals
gresham$Fraction <- ifelse(grepl('Dissolved',gresham$PARAMETER),'Dissolved',ifelse(grepl('Total',gresham$PARAMETER),'Total recoverable',''))
#This will make the Names consistent with LASAR
gresh.rename.vector <- c('Alpha-BHC' = 'alpha-BHC', 
                         'Cu-Dissolved' = 'Copper', 
                         'Cu-Total' = 'Copper', 
                         'Endosulfan Sulfate' = 'Endosulfan sulfate',
                         'Hardness' = 'Hardness, carbonate as CaCO3',
                         'Hg-Total' = 'Mercury',
                         'NH3-N' = 'Ammonia as N',
                         'Ni-Dissolved' = 'Nickel',
                         'Ni-Total' = 'Nickel',
                         'NO3-N' = 'Nitrates',
                         'O-PO4' = 'Orthophosphate as P',
                         'Pb-Dissolved' = 'Lead',
                         'Pb-Total' = 'Lead',
                         'Total-P' = 'Phosphorus',
                         'Zn-Dissolved' = 'Zinc',
                         'Zn-Total' = 'Zinc')
gresham$Name <- mapvalues(gresham$PARAMETER, from = names(gresh.rename.vector), to = gresh.rename.vector)
gresham$tMRLUnit <- gresham$UNIT
gresham$Status <- gresham$DQL #These are all NA
#These are parameters we are not analyzing for in this effort
gresham <- gresham[!gresham$Name %in% c('BOD5','E. coli', 'TKN', 'TSS', 'Chlorophyll-a', 'Endrin Ketone'),]
#This will populate the criterianame column
gresham.criteria.vector <- c( "4,4'-DDD" = "DDD 4,4'", 
                              "4,4'-DDE" = "DDE 4,4'", 
                              "4,4'-DDT" = "DDT 4,4'",
                              "Orthophosphate as P" = "Orthophosphate as P", 
                              "Phosphorus" = "Phosphorus Elemental",
                              "alpha-BHC" = "BHC Alpha", 
                              "Endosulfan I" = "Endosulfan Alpha", 
                              "Endosulfan II" = "Endosulfan Beta",
                              "Endosulfan sulfate" = "Endosulfan Sulfate", 
                              "Mercury" = "Mercury (total)")
gresham$criterianame <- mapvalues(gresham$Name, from = names(gresham.criteria.vector), to = gresham.criteria.vector)
#Here we make the names consistent
gresham <- rename(gresham, c('STATION_ID' = 'SampleRegID', 'SITE_DESCRIPTION_LOCATION' = 'SampleAlias', 'SAMPLING_ORGANIZATION' = 'Agency',
                             'METHOD_NAME' = 'SpecificMethod', 'REPORTING_LIMIT' = 'tMRL', 'UNIT' = 'Unit', 'SAMPLE_QA_TYPE' = 'SampleType'))
gresham.sub <- gresham[,names(wqp.data.sub)]

#### Pulling wqp.data and lasar and gresham together now! ####
data.complete <- rbind(lasar.new.names, wqp.data.sub, gresham.sub)
#I had vector allocation issues so I am keeping my workingspace a little lighter
rm(wqp.data, wqp.stations, gresham, lasar)
#This makes a data frame with these column names. If you were to pick this code up this is where
#you could make sure you have these fields and start from here to run with a new dataset.
#   "SampleRegID" = Station Key
#        "Agency" = Sampling Agency
#       "Sampled" = Sample Date time
#          "Name" = Characteristic Name from respective source
#      "Fraction" = Sample fraction
#       "tResult" = Result value
#          "Unit" = Result unit 
#        "Status" = DEQ Data Quality Level
#    "SampleType" = Primary/Duplicate identifier
#          "tMRL" = Minimum reporting limit
#      "tMRLUnit" = Minimum reporting limit unit
#   "SampleAlias" = Station description
#  "criterianame" = Name as it appears in criteria table
#"SpecificMethod" = Analytis method
#           "dnd" = Detect/nondetect indicator (1 = detected, 0 = nondetect)

#makes sure orthophosphate can match to the criteria
data.complete[grep('hospha',data.complete$Name),c('criterianame')] <- 'Phosphate Phosphorus'

#### Cleaning up the complete dataset and making some fields for consistent future processing ####
#result should also be in micrograms since all the criteria are as well
#first there are several improperply labeled units as well as some pH ones labeled with None for unit. pH is inlcuded in this analysis
#for the purposes of calculating pentachlorophenol and ammonia criteria 
data.complete$Unit <- str_trim(data.complete$Unit)
data.complete <- data.complete[!data.complete$Unit %in% c('%','mg/Kg wet','ng/SPMD','ueq/L'),]
data.complete[data.complete$Unit %in% c('mg/L','mg/l','mg/l as N',"mg/l CaCO3"),'tResult'] <- data.complete[data.complete$Unit %in% c('mg/L','mg/l','mg/l as N',"mg/l CaCO3"),'tResult']*1000
data.complete[data.complete$Unit %in% c('mg/L','mg/l','mg/l as N',"mg/l CaCO3"),'Unit'] <- 'µg/L'
data.complete[data.complete$Unit %in% c('ng/L', 'ng/l'),'tResult'] <- data.complete[data.complete$Unit %in% c('ng/L', 'ng/l'),'tResult']/1000
data.complete[data.complete$Unit %in% c('ng/L', 'ng/l'),'Unit'] <- 'µg/L'
data.complete[data.complete$Unit %in% c('pg/L', 'pg/l'),'tResult'] <- data.complete[data.complete$Unit %in% c('pg/L', 'pg/l'),'tResult']/1000000
data.complete[data.complete$Unit %in% c('pg/L', 'pg/l'),'Unit'] <- 'µg/L'
data.complete[is.na(data.complete$Unit),'Unit'] <- 'µg/L'
data.complete[data.complete$Unit == 'ppb','Unit'] <- 'µg/L'
data.complete[data.complete$Unit == 'ug/l','Unit'] <- 'µg/L'

#for the MRL units too
data.complete$tMRLUnit <- str_trim(data.complete$tMRLUnit)
data.complete <- data.complete[!data.complete$tMRLUnit %in% c('%','mg/Kg wet','ng/SPMD','ueq/L'),]
data.complete[data.complete$tMRLUnit %in% c('mg/L','mg/l','mg/l as N',"mg/l CaCO3"),'tMRL'] <- data.complete[data.complete$tMRLUnit %in% c('mg/L','mg/l','mg/l as N',"mg/l CaCO3"),'tMRL']*1000
data.complete[data.complete$tMRLUnit %in% c('mg/L','mg/l','mg/l as N',"mg/l CaCO3"),'tMRLUnit'] <- 'µg/L'
data.complete[data.complete$tMRLUnit %in% c('ng/L', 'ng/l'),'tMRL'] <- data.complete[data.complete$tMRLUnit %in% c('ng/L', 'ng/l'),'tMRL']/1000
data.complete[data.complete$tMRLUnit %in% c('ng/L', 'ng/l'),'tMRLUnit'] <- 'µg/L'
data.complete[data.complete$tMRLUnit %in% c('pg/L', 'pg/l'),'tMRL'] <- data.complete[data.complete$tMRLUnit %in% c('pg/L', 'pg/l'),'tMRL']/1000000
data.complete[data.complete$tMRLUnit %in% c('pg/L', 'pg/l'),'tMRLUnit'] <- 'µg/L'

data.complete[which(data.complete$tMRLUnit == 'ppb'),'tMRLUnit'] <- 'µg/L'
data.complete[which(data.complete$tMRLUnit == 'ug/l'),'tMRLUnit'] <- 'µg/L'

#This is the sample's name.full to be used to compare to the criteria.name.full
total.to.recoverable <- c('Arsenic','Mercury','Copper','Zinc','Nickel','Lead','Selenium',
                          'Chromium','Iron','Barium','Thallium','Manganese','Silver','Antimony',
                          'Cadmium','Cyanide','Chromium(VI)')
dissolved.metals <- c('Arsenic','Cadmium','Chromium','Chromium(VI)','Lead','Nickel','Selenium','Silver','Zinc')
data.complete[data.complete$Name %in% total.to.recoverable & data.complete$Fraction %in% c('Total', 'Recoverable'),'Fraction'] <- 'Total recoverable'
#Assumes total sample fraction for those samples that remain unstandardized
data.complete[data.complete$Name %in% total.to.recoverable & !grepl('Total recoverable|Dissolved', data.complete$Fraction),'Fraction'] <- 'Total recoverable'
#compile Name.full for metals
data.complete$Name.full <- ifelse(data.complete$Name %in% c(total.to.recoverable, dissolved.metals) & 
                                    data.complete$Fraction %in% c('Dissolved', 'Total recoverable'),
                                  paste(data.complete$Name, ", ", data.complete$Fraction, sep = ''),
                                  data.complete$Name)

#Fields for future grouping. I make them numeric to use less memory when grouping which accelerates the processing time.
data.complete$id <- paste(data.complete$SampleRegID, data.complete$Name.full, data.complete$Sampled)
data.complete$id <- as.numeric(as.factor(data.complete$id))
data.complete$day <- substr(data.complete$Sampled,1,10)
data.complete$code <- paste(data.complete$SampleRegID, data.complete$Name.full, data.complete$day)
data.complete$code <- as.numeric(as.factor(data.complete$code))
data.complete$index <- rownames(data.complete)

#### Resolve duplicates ####
#Since we take the maximum by day. It doesn't matter if we resolved field primary and field duplicate samples first
#I am still going to leave this code block here for now though
#resolve field duplicates specified by sample type
# fd <- data.complete[data.complete$SampleType %in% c('Sample - Field Duplicate', 'Quality Control Sample-Field Replicate', 
#                                                     'Quality Control Field Replicate Msr/Obs', 'Quality Control Sample-Other'),]
# fd.fp <- data.complete[data.complete$code %in% fd$code,]
# sample.max <- ddply(fd.fp, .(code), summarise, maximum = max(tResult), rows = length(tResult), index = index[which.max(tResult)])
# fd.fp.max <- fd.fp[fd.fp$index %in% sample.max$index,]
# data.complete.wo.fd.fp.pairs <- data.complete[!data.complete$index %in% fd.fp$index,]
# data.complete.w.resolved.fd <- rbind(data.complete.wo.fd.fp.pairs, fd.fp.max)

#resolve duplicates, taking the max by day
#because there are some with multiple methods and two MRL levels we can't just pick the max
#value unless it was a detection so I made the resolveMRLs function from the hardness_eval_functions_Element_Name.R file
sub <- with(data.complete, resolveMRLs(code, dnd, tResult))
data.complete.wo.dup.MRLs <- data.complete[sub,]
data.complete.wo.dups <- remove.dups(data.complete.wo.dup.MRLs)

#### Select only those stations that mapped to a stream or lake ####
#This section also serves the purpose of adding in the Matrix (or station type)
#If you were to pick this up and not be connected to DEQ servers you should just need 
#a table with a USE_Final determination for each station and a Station/Water type column
#called Matrix to indicate if the samples from the station should be compared to the freshwater
# or the saltwater criteria.
con <- odbcConnect('WQAssessment')
sul2012 <- sqlFetch(con, 'StationUseList')
odbcCloseAll()
sul2012 <- sul2012[sul2012$USE_Final == 1,]
sul2012 <- rename(sul2012, c('Water_Type' = 'Matrix'))
data.complete.w.matrix <- merge(data.complete.wo.dups, sul2012[,c('STATION','Matrix')], by.x = 'SampleRegID', by.y = 'STATION')

#### Grouping parameters to be compared to composite criteria ####
#Some of the criteria apply to Totals and not individual degradates. Here we do that totalling so comparisons can be done.

#First, we will make a Total DDT - 
# 
# This Criteria is never the most stringent and will not be included for analysis
# 
# ddt <- data.complete.wo.dups[data.complete.wo.dups$Name %in% c("4,4`-DDD", "4,4`-DDE", "4,4`-DDT", "p,p'-DDD", "p,p'-DDE", "p,p'-DDT"),]
# ddt$tResult <- ddt$tResult*ddt$dnd
# ddt.casted <- dcast(ddt, Agency + SampleRegID + SampleAlias + Matrix +  
#                       Sampled + SpecificMethod + Fraction + day ~ Name, value.var = 'tResult')
# ddt.casted$'Total DDT' <- rowSums(ddt.casted[,c("4,4`-DDD", "4,4`-DDE", "4,4`-DDT","p,p'-DDD", "p,p'-DDE", "p,p'-DDT")],na.rm=TRUE)
# ddt.casted.sub <- within(ddt.casted, rm("4,4`-DDD", "4,4`-DDE", "4,4`-DDT","p,p'-DDD", "p,p'-DDE", "p,p'-DDT"))
# ddt.melted <- melt(ddt.casted.sub, 
#                    id.vars = c('Agency','SampleRegID','SampleAlias','Sampled','Matrix','SpecificMethod',  
#                                'Fraction','day'),
#                    variable.name = 'Name',
#                    value.name = 'tResult')
# ddt.melted$dnd <- ifelse(ddt.melted$tResult > 0,1,0)
# ddt.melted$Name.full <- ddt.melted$Name
# ddt.melted.addons <- data.frame('tMRL' = rep(0,nrow(ddt.melted)),
#                                 'tMRLUnit' = rep('µg/L',nrow(ddt.melted)),
#                                 'Unit' = rep('µg/L',nrow(ddt.melted)), 
#                                 'Status' = rep('A',nrow(ddt.melted)))
# ddt.melted <- cbind(ddt.melted, ddt.melted.addons)
# ddt.melted$id <- paste(ddt.melted$SampleRegID, ddt.melted$Name.full, ddt.melted$Sampled)
# ddt.melted$day <- substr(ddt.melted$Sampled,1,10)
# ddt.melted$code <- paste(ddt.melted$SampleRegID, ddt.melted$Name.full, ddt.melted$day)
# ddt.melted$index <- as.character(max(as.numeric(data.complete.wo.dups$index)) + as.numeric(rownames(ddt.melted)))
# ddt.melted$criterianame <- 'Total DDT'
# ddt.melted$SampleType <- 'Sample'
# dcwd.ddt <- rbind(data.complete.wo.dups, ddt.melted)

#Now Total Endosulfan
endo <- data.complete.w.matrix[data.complete.w.matrix$Name %in% c("Endosulfan I", "Endosulfan II", ".alpha.-Endosulfan", ".beta.-Endosulfan"),]
endo$tResult <- endo$tResult*endo$dnd
endo.casted <- dcast(endo, Agency + SampleRegID + SampleAlias + Matrix + Fraction +
                       Sampled +  SpecificMethod ~ Name, value.var = 'tResult')
endo.casted$Endosulfan <- rowSums(endo.casted[,c("Endosulfan I", "Endosulfan II", ".alpha.-Endosulfan", ".beta.-Endosulfan")],na.rm=TRUE)
endo.casted.sub <- within(endo.casted, rm("Endosulfan I", "Endosulfan II", ".alpha.-Endosulfan", ".beta.-Endosulfan"))
endo.melted <- melt(endo.casted.sub, id.vars = c('Agency','SampleRegID','SampleAlias','Sampled','Matrix','SpecificMethod','Fraction'),variable.name = 'Name',value.name = 'tResult')#melt
endo.melted$dnd <- ifelse(endo.melted$tResult > 0,1,0)
endo.melted.addons <- data.frame('tMRLUnit' = rep('µg/L',nrow(endo.melted)),
                                 'Unit' = rep('µg/L',nrow(endo.melted)), 
                                 'Status' = rep('A',nrow(endo.melted)))
endo.melted <- cbind(endo.melted, endo.melted.addons)
endo.tMRL <- ddply(endo, .(Agency, SampleRegID, SampleAlias, Matrix, Fraction, Sampled, SpecificMethod), summarize, tMRL = min(tMRL))
endo.melted <- cbind(endo.melted, endo.tMRL$tMRL)
endo.melted <- rename(endo.melted, c('endo.tMRL$tMRL' = 'tMRL'))
endo.melted$Name.full <- endo.melted$Name
endo.melted$id <- paste(endo.melted$SampleRegID, endo.melted$Name.full, endo.melted$Sampled)
endo.melted$day <- substr(endo.melted$Sampled,1,10)
endo.melted$code <- paste(endo.melted$SampleRegID, endo.melted$Name.full, endo.melted$day)
endo.melted$index <- as.character(max(as.numeric(data.complete.w.matrix$index)) + as.numeric(rownames(endo.melted)))
endo.melted$criterianame <- 'Endosulfan'
endo.melted$SampleType <- 'Sample'
dcwd.endo <- rbind(data.complete.w.matrix, endo.melted)

#Now Nitrosamines
nitrosamines <- data.complete.w.matrix[data.complete.w.matrix$Name %in% c("n-Nitrosodiphenylamine", "N-Nitrosodi-n-propylamine", "N-Nitrosodimethylamine", "N-Nitrosodiphenylamine"),]
nitrosamines$tResult <- nitrosamines$tResult*nitrosamines$dnd
nitrosamines.casted <- dcast(nitrosamines, Agency + SampleRegID + SampleAlias + Matrix + Fraction +
                       Sampled +  SpecificMethod ~ Name, value.var = 'tResult')
nitrosamines.casted$Nitrosamines <- rowSums(nitrosamines.casted[,c("n-Nitrosodiphenylamine", "N-Nitrosodi-n-propylamine", "N-Nitrosodimethylamine", "N-Nitrosodiphenylamine")],na.rm=TRUE)
nitrosamines.casted.sub <- within(nitrosamines.casted, rm("n-Nitrosodiphenylamine", "N-Nitrosodi-n-propylamine", "N-Nitrosodimethylamine", "N-Nitrosodiphenylamine"))
nitrosamines.melted <- melt(nitrosamines.casted.sub, id.vars = c('Agency','SampleRegID','SampleAlias','Sampled','Matrix','SpecificMethod','Fraction'),variable.name = 'Name',value.name = 'tResult')#melt
nitrosamines.melted$dnd <- ifelse(nitrosamines.melted$tResult > 0,1,0)
nitrosamines.melted.addons <- data.frame('tMRLUnit' = rep('µg/L',nrow(nitrosamines.melted)),
                                 'Unit' = rep('µg/L',nrow(nitrosamines.melted)), 
                                 'Status' = rep('A',nrow(nitrosamines.melted)))
nitrosamines.melted <- cbind(nitrosamines.melted, nitrosamines.melted.addons)
nitrosamines.tMRL <- ddply(nitrosamines, .(Agency, SampleRegID, SampleAlias, Matrix, Fraction, Sampled, SpecificMethod), summarize, tMRL = min(tMRL))
nitrosamines.melted <- cbind(nitrosamines.melted, nitrosamines.tMRL$tMRL)
nitrosamines.melted <- rename(nitrosamines.melted, c('nitrosamines.tMRL$tMRL' = 'tMRL'))
nitrosamines.melted$Name.full <- nitrosamines.melted$Name
nitrosamines.melted$id <- paste(nitrosamines.melted$SampleRegID, nitrosamines.melted$Name.full, nitrosamines.melted$Sampled)
nitrosamines.melted$day <- substr(nitrosamines.melted$Sampled,1,10)
nitrosamines.melted$code <- paste(nitrosamines.melted$SampleRegID, nitrosamines.melted$Name.full, nitrosamines.melted$day)
nitrosamines.melted$index <- as.character(max(as.numeric(dcwd.endo$index)) + as.numeric(rownames(nitrosamines.melted)))
nitrosamines.melted$criterianame <- 'Nitrosamines'
nitrosamines.melted$SampleType <- 'Sample'
dcwd.endo.nitrosamines <- rbind(dcwd.endo, nitrosamines.melted)

#Now Hexachlorocyclo-hexane-Technical
hch <- data.complete.w.matrix[data.complete.w.matrix$Name %in% c("beta-BHC", "alpha-BHC", ".beta.-Hexachlorocyclohexane", ".alpha.-Hexachlorocyclohexane","Lindane"),]
hch$tResult <- hch$tResult*hch$dnd
hch.casted <- dcast(hch, Agency + SampleRegID + SampleAlias + Matrix + Fraction +
                               Sampled +  SpecificMethod ~ Name, value.var = 'tResult')
hch.casted$'Hexachlorocyclo-hexane-Technical' <- rowSums(hch.casted[,c("beta-BHC", "alpha-BHC", ".beta.-Hexachlorocyclohexane", ".alpha.-Hexachlorocyclohexane","Lindane")],na.rm=TRUE)
hch.casted.sub <- within(hch.casted, rm("beta-BHC", "alpha-BHC", ".beta.-Hexachlorocyclohexane", ".alpha.-Hexachlorocyclohexane","Lindane"))
hch.melted <- melt(hch.casted.sub, id.vars = c('Agency','SampleRegID','SampleAlias','Sampled','Matrix','SpecificMethod','Fraction'),variable.name = 'Name',value.name = 'tResult')#melt
hch.melted$dnd <- ifelse(hch.melted$tResult > 0,1,0)
hch.melted.addons <- data.frame('tMRLUnit' = rep('µg/L',nrow(hch.melted)),
                                         'Unit' = rep('µg/L',nrow(hch.melted)), 
                                         'Status' = rep('A',nrow(hch.melted)))
hch.melted <- cbind(hch.melted, hch.melted.addons)
hch.tMRL <- ddply(hch, .(Agency, SampleRegID, SampleAlias, Matrix, Fraction, Sampled, SpecificMethod), summarize, tMRL = min(tMRL))
hch.melted <- cbind(hch.melted, hch.tMRL$tMRL)
hch.melted <- rename(hch.melted, c('hch.tMRL$tMRL' = 'tMRL'))
hch.melted$Name.full <- hch.melted$Name
hch.melted$id <- paste(hch.melted$SampleRegID, hch.melted$Name.full, hch.melted$Sampled)
hch.melted$day <- substr(hch.melted$Sampled,1,10)
hch.melted$code <- paste(hch.melted$SampleRegID, hch.melted$Name.full, hch.melted$day)
hch.melted$index <- as.character(max(as.numeric(dcwd.endo.nitrosamines$index)) + as.numeric(rownames(hch.melted)))
hch.melted$criterianame <- 'Hexachlorocyclo-hexane-Technical'
hch.melted$SampleType <- 'Sample'
dcwd.endo.nitrosamines.hch <- rbind(dcwd.endo.nitrosamines, hch.melted)

#Now Total Chlordane
chlordane <- dcwd.endo.nitrosamines.hch[dcwd.endo.nitrosamines.hch$Name %in% c("Oxychlordane", "alpha-Chlordane", "cis-Chlordane", 'trans-Chlordane',"gamma-Chlordane+trans-Nonachlor", "trans-Nonachlor", "cis-Nonachlor"),]
chlordane$tResult <- chlordane$tResult*chlordane$dnd
chlordane.casted <- dcast(chlordane, Agency + SampleRegID + SampleAlias + Matrix +
                            Sampled + SpecificMethod +Fraction ~ Name, value.var = 'tResult')
chlordane.casted$Chlordane <- rowSums(chlordane.casted[,c("Oxychlordane", "cis-Chlordane", 'trans-Chlordane',"trans-Nonachlor", "cis-Nonachlor")],na.rm=TRUE)
chlordane.casted.sub <- within(chlordane.casted, rm("Oxychlordane", "cis-Chlordane", 'trans-Chlordane',"trans-Nonachlor", "cis-Nonachlor"))
chlordane.melted <- melt(chlordane.casted.sub, id.vars = c('Agency','SampleRegID','SampleAlias','Sampled','Matrix','SpecificMethod','Fraction'),variable.name = 'Name',value.name = 'tResult')#melt
chlordane.melted$dnd <- ifelse(chlordane.melted$tResult > 0,1,0)
chlordane.melted.addons <- data.frame('tMRLUnit' = rep('µg/L',nrow(chlordane.melted)),
                                      'Unit' = rep('µg/L',nrow(chlordane.melted)), 
                                      'Status' = rep('A',nrow(chlordane.melted)))
chlordane.melted <- cbind(chlordane.melted, chlordane.melted.addons)
chlordane.tMRL <- ddply(chlordane, .(Agency, SampleRegID, SampleAlias, Matrix, Fraction, Sampled, SpecificMethod), summarize, tMRL = min(tMRL))
chlordane.melted <- cbind(chlordane.melted, chlordane.tMRL$tMRL)
chlordane.melted <- rename(chlordane.melted, c('chlordane.tMRL$tMRL' = 'tMRL'))
chlordane.melted$Name.full <- chlordane.melted$Name
chlordane.melted$id <- paste(chlordane.melted$SampleRegID, chlordane.melted$Name.full, chlordane.melted$Sampled)
chlordane.melted$day <- substr(chlordane.melted$Sampled,1,10)
chlordane.melted$code <- paste(chlordane.melted$SampleRegID, chlordane.melted$Name.full, chlordane.melted$day)
chlordane.melted$index <- as.character(max(as.numeric(dcwd.endo.nitrosamines.hch$index)) + as.numeric(rownames(chlordane.melted)))
chlordane.melted$criterianame <- 'Chlordane'
chlordane.melted$SampleType <- 'Sample'
dcwd.endo.nitrosamines.hch.chlord <- rbind(dcwd.endo.nitrosamines.hch, chlordane.melted)

#Now total PCBs
#First the congeners
pcb <- dcwd.endo.nitrosamines.hch.chlord[grep('PCB',dcwd.endo.nitrosamines.hch.chlord$Name),]
pcb$tResult <- pcb$tResult*pcb$dnd
pcb.casted <- dcast(pcb, Agency + SampleRegID + SampleAlias + Matrix + 
                      Sampled + SpecificMethod + Fraction ~ Name, value.var = 'tResult')
pcb.casted$'Polychlorinated Biphenyls (PCBs)' <- rowSums(pcb.casted[,unique(dcwd.endo.nitrosamines.hch.chlord[grep('PCB',dcwd.endo.nitrosamines.hch.chlord$Name),'Name'])],na.rm=TRUE)
pcb.casted.sub <- pcb.casted[,!names(pcb.casted) %in% unique(dcwd.endo.nitrosamines.hch.chlord[grep('PCB',dcwd.endo.nitrosamines.hch.chlord$Name),'Name'])]
pcb.melted <- melt(pcb.casted.sub, id.vars = c('Agency','SampleRegID','SampleAlias','Sampled','Matrix','SpecificMethod','Fraction'),variable.name = 'Name',value.name = 'tResult')#melt
pcb.melted$dnd <- ifelse(pcb.melted$tResult > 0,1,0)
pcb.melted.addons <- data.frame('tMRLUnit' = rep('µg/L',nrow(pcb.melted)),
                                'Unit' = rep('µg/L',nrow(pcb.melted)), 
                                'Status' = rep('A',nrow(pcb.melted)))
pcb.melted <- cbind(pcb.melted, pcb.melted.addons)
pcb.tMRL <- ddply(pcb, .(Agency, SampleRegID, SampleAlias, Matrix, Fraction, Sampled, SpecificMethod), summarize, tMRL = min(tMRL))
pcb.melted <- cbind(pcb.melted, pcb.tMRL$tMRL)
pcb.melted <- rename(pcb.melted, c('pcb.tMRL$tMRL' = 'tMRL'))
pcb.melted$Name.full <- pcb.melted$Name
pcb.melted$id <- paste(pcb.melted$SampleRegID, pcb.melted$Name.full, pcb.melted$Sampled)
pcb.melted$day <- substr(pcb.melted$Sampled,1,10)
pcb.melted$code <- paste(pcb.melted$SampleRegID, pcb.melted$Name.full, pcb.melted$day)
pcb.melted$index <- as.character(max(as.numeric(dcwd.endo.nitrosamines.hch.chlord$index)) + as.numeric(rownames(pcb.melted)))
pcb.melted$criterianame <- 'Polychlorinated Biphenyls (PCBs)'
pcb.melted$SampleType <- 'Sample'
dcwd.endo.nitrosamines.hch.chlord.pcb <- rbind(dcwd.endo.nitrosamines.hch.chlord, pcb.melted)

#Now the Aroclors
aroclor <- dcwd.endo.nitrosamines.hch.chlord.pcb[grep('roclor',dcwd.endo.nitrosamines.hch.chlord.pcb$Name),]
aroclor$tResult <- aroclor$tResult*aroclor$dnd
aroclor.casted <- dcast(aroclor, Agency + SampleRegID + SampleAlias + Matrix + 
                      Sampled + SpecificMethod + Fraction ~ Name, value.var = 'tResult')
aroclor.casted$'Polychlorinated Biphenyls (PCBs)' <- rowSums(aroclor.casted[,unique(dcwd.endo.nitrosamines.hch.chlord.pcb[grep('roclor',dcwd.endo.nitrosamines.hch.chlord.pcb$Name),'Name'])],na.rm=TRUE)
aroclor.casted.sub <- aroclor.casted[,!names(aroclor.casted) %in% unique(dcwd.endo.nitrosamines.hch.chlord.pcb[grep('roclor',dcwd.endo.nitrosamines.hch.chlord.pcb$Name),'Name'])]
aroclor.melted <- melt(aroclor.casted.sub, id.vars = c('Agency','SampleRegID','SampleAlias','Sampled','Matrix','SpecificMethod','Fraction'),variable.name = 'Name',value.name = 'tResult')#melt
aroclor.melted$dnd <- ifelse(aroclor.melted$tResult > 0,1,0)
aroclor.melted.addons <- data.frame('tMRLUnit' = rep('µg/L',nrow(aroclor.melted)),
                                'Unit' = rep('µg/L',nrow(aroclor.melted)), 
                                'Status' = rep('A',nrow(aroclor.melted)))
aroclor.melted <- cbind(aroclor.melted, aroclor.melted.addons)
aroclor.tMRL <- ddply(aroclor, .(Agency, SampleRegID, SampleAlias, Matrix, Fraction, Sampled, SpecificMethod), summarize, tMRL = min(tMRL))
aroclor.melted <- cbind(aroclor.melted, aroclor.tMRL$tMRL)
aroclor.melted <- rename(aroclor.melted, c('aroclor.tMRL$tMRL' = 'tMRL'))
aroclor.melted$Name.full <- aroclor.melted$Name
aroclor.melted$id <- paste(aroclor.melted$SampleRegID, aroclor.melted$Name.full, aroclor.melted$Sampled)
aroclor.melted$day <- substr(aroclor.melted$Sampled,1,10)
aroclor.melted$code <- paste(aroclor.melted$SampleRegID, aroclor.melted$Name.full, aroclor.melted$day)
aroclor.melted$index <- as.character(max(as.numeric(dcwd.endo.nitrosamines.hch.chlord.pcb$index)) + as.numeric(rownames(aroclor.melted)))
aroclor.melted$criterianame <- 'Polychlorinated Biphenyls (PCBs)'
aroclor.melted$SampleType <- 'Sample'
dcwd.endo.nitrosamines.hch.chlord.pcb.aroclor <- rbind(dcwd.endo.nitrosamines.hch.chlord.pcb, aroclor.melted)

#we need to take the calcium and magnesium and calculate hardness where we can
calmag <- dcwd.endo.nitrosamines.hch.chlord.pcb.aroclor[dcwd.endo.nitrosamines.hch.chlord.pcb.aroclor$Name %in% c('Calcium','Magnesium'),]
calmag$tResult <- calmag$tResult*calmag$dnd
calmag.casted <- dcast(calmag, Agency + SampleRegID + SampleAlias + Matrix +
                         Sampled + SpecificMethod +Fraction ~ Name, value.var = 'tResult', fun.aggregate = max, fill = as.numeric(NA))
calmag.casted$'Hardness, carbonate as CaCO3' <- 2.497*(calmag.casted$Calcium) + 4.1189*(calmag.casted$Magnesium)
calmag.casted.sub <- within(calmag.casted, rm("Calcium","Magnesium"))
calmag.melted <- melt(calmag.casted.sub, id.vars = c('Agency','SampleRegID','SampleAlias','Sampled','Matrix','SpecificMethod','Fraction'),variable.name = 'Name',value.name = 'tResult')#melt
calmag.melted$dnd <- ifelse(calmag.melted$tResult > 0,1,0)
calmag.melted.addons <- data.frame('tMRL' = rep(0,nrow(calmag.melted)), 
                                   'tMRLUnit' = rep('µg/L',nrow(calmag.melted)),
                                   'Unit' = rep('µg/L',nrow(calmag.melted)), 
                                   'Status' = rep('A',nrow(calmag.melted)))
calmag.melted <- cbind(calmag.melted, calmag.melted.addons)
calmag.melted[calmag.melted$Fraction %in% c('Total', 'Recoverable'),'Fraction'] <- 'Total recoverable'
calmag.melted$Name.full <- paste(calmag.melted$Name, calmag.melted$Fraction, sep = ', ')
calmag.melted$id <- paste(calmag.melted$SampleRegID, calmag.melted$Name.full, calmag.melted$Sampled)
calmag.melted$day <- substr(calmag.melted$Sampled,1,10)
calmag.melted$code <- paste(calmag.melted$SampleRegID, calmag.melted$Name.full, calmag.melted$day)
calmag.melted$index <- as.character(max(as.numeric(dcwd.endo.nitrosamines.hch.chlord.pcb.aroclor$index)) + as.numeric(rownames(calmag.melted)))
calmag.melted$criterianame <- calmag.melted$Name.full
calmag.melted$SampleType <- 'Sample'
dcwd.w.totals <- rbind(dcwd.endo.nitrosamines.hch.chlord.pcb.aroclor, calmag.melted)

#This resolves some issues with naming and makes sure that we have fraction resolved as well
dcwd.w.totals[dcwd.w.totals$Name.full == "Hardness, carbonate as CaCO3",'Name.full'] <- paste(dcwd.w.totals[dcwd.w.totals$Name.full == "Hardness, carbonate as CaCO3",'Name.full'], dcwd.w.totals[dcwd.w.totals$Name.full == "Hardness, carbonate as CaCO3",'Fraction'], sep = ', ')
dcwd.w.totals[dcwd.w.totals$Name.full == "Hardness, carbonate as CaCO3, Total",'Name.full'] <- "Hardness, carbonate as CaCO3, Total recoverable"
dcwd.w.totals[dcwd.w.totals$Name.full == "Hardness, carbonate as CaCO3, Calculated",'Name.full'] <- "Hardness, carbonate as CaCO3, Total recoverable"
dcwd.w.totals[dcwd.w.totals$Name.full == "Hardness, carbonate as CaCO3, NA" ,'Name.full'] <- "Hardness, carbonate as CaCO3, Total recoverable"
dcwd.w.totals[grep('ardnes',dcwd.w.totals$Name.full),'tResult'] <- dcwd.w.totals[grep('ardnes',dcwd.w.totals$Name.full),'tResult']/1000
dcwd.w.totals[grep('ardnes',dcwd.w.totals$Name.full),'criterianame'] <- dcwd.w.totals[grep('ardnes',dcwd.w.totals$Name.full),'Name.full']
dcwd.w.totals <- dcwd.w.totals[!is.na(dcwd.w.totals$tResult),]

#Since I made the code numeric to improve processing of the duplicate resolution prior to doing the totals
#there is now a discrepancy in that field. To resolve I will regenerate the concatenated form in order to appropriately
#remove duplicates with the following function which operates on the code field
dcwd.w.totals$code <- paste(dcwd.w.totals$SampleRegID, dcwd.w.totals$Name.full, dcwd.w.totals$day)
dcwd.w.totals$code <- as.numeric(as.factor(dcwd.w.totals$code))

#we want the max between endosulfan and its sum of isomers, between chlordane
#and is sum of isomers and between sum of pcb congeners and aroclors.
dcwd.w.totals <- remove.dups(dcwd.w.totals)
rm(list = ls()[grep('aroclor|calmag|chlordane|endo|hch|nitrosamines|pcb|data.complete',ls())])

#### Associate with criteria ####
#Not sure where the best place is for this but we need to fix some of the issues with Arsenic and Dissolved/Total/Total inorganic
#We can assume per Andrea that all of dissolved is in Total inorganic form. That leaves the conversion for anything in Total recoverable.
#The AM section for total coversion is on page 64 - Decided NOT to use this conversion
#dcwd.w.totals[dcwd.w.totals$Name.full %in% c('Arsenic, Total recoverable','Arsenic'),'tResult'] <- dcwd.w.totals[dcwd.w.totals$Name.full %in% c('Arsenic, Total recoverable','Arsenic'),'tResult']*0.76
#dcwd.w.totals[grep('Arsenic',dcwd.w.totals$Name.full),'criterianame'] <- 'Arsenic, Total inorganic'

#We need an ID to match with the criteria and ensure the right matrix is used
dcwd.w.totals$ID <- paste(dcwd.w.totals$criterianame, dcwd.w.totals$Matrix)
Table3040.applicable$ID <- paste(Table3040.applicable$criterianame, Table3040.applicable$Matrix)

#make the tMRL 0 where it is NA for determining valid samples later on
dcwd.w.totals[is.na(dcwd.w.totals$tMRL),'tMRL'] <- 0

#Now that the names are consistent we can match using analyte name and bring in the criteria
dcc <- merge(dcwd.w.totals, Table3040.applicable, by = 'ID', all.x = TRUE)
dcc <- within(dcc, rm(criterianame.y))

#### Hardness criteria calculation ####
#Using the hardness evaluation function loaded above we can calculate the hardness based criteria values
#and bring them into the dataframe with the other criteria values. First, though we remove the hardness 
#metals from the dataframe since the output of the function maintains all the columns of the original dataframe
#Calculate criteria based on hardness
hm <- hardness.crit.calc(dcwd.w.totals)
#Push CFC and CFA for lead to table to be used later for conversions
td.conv <- hm[grepl('Dissolved',hm$Criteria.Name.full) & hm$Name.full != hm$Criteria.Name.full,c('code','CFC','CFA','CFC_SW','CFA_SW')]
#make the column names consistent
hm <- hm[,names(dcc)]
#so that we don't introduce duplication we want to replace those rows where we
#have calculated the criteria (and drop anything that didn't calculate)
dcc.wo.hm <- dcc[!dcc$ID %in% hm$ID,]
dcc.hm <- rbind(dcc.wo.hm, hm)

#### Pentachlorophenol pH dependent criteria calculation ####
#Similarly pentachlorophenol is parameter dependent and is handled the same as hardness
# penta <- pentachlorophenol.crit.calc(dcwd.w.totals)
# penta <- penta[,names(dcc)]
# penta40 <- dcc[dcc$criterianame == 'Pentachlorophenol' & !is.na(dcc$variable),]
# penta.all <- rbind(penta, penta40)
# penta.min <- ddply(penta.all, .(criterianame, SampleRegID, Sampled, Fraction), function(m) {m[which(m$value == min(m$value)),]})

## Turns out Table 40 criteria for penta are ALWAYS lower for this dataset ##

#dcc.wo.penta <- dcc.hm[!dcc.hm$criterianame %in% penta$criterianame,]
#dcc.penta <- rbind(dcc.wo.penta, penta)
#rm(list = ls()[c(grep('penta', ls()))])

#### Ammonia pH and temperature dependent criteria calculation ####
#Ammonia is also parameter dependent and is handled similarly
amm <- ammonia.crit.calc(dcwd.w.totals)
amm <- amm[,names(dcc.hm)]
dcc.wo.amm <- dcc.hm[!dcc.hm$ID %in% amm$ID,]
#We need to clean up the workspace because I was running into problems with memory allocation
rm(list = setdiff(ls(), c('dcc.wo.amm', 'amm', 'td.conv')))
dcc.w.calcs <- rbind(dcc.wo.amm, amm)

#### EVALUATION ####
#regenerate this so it is new as long as any(duplicated(rownames(dcc.w.calcs))) evaluates to false
dcc.w.calcs$index <- rownames(dcc.w.calcs)

#Make sure criteria value is numeric so the appropriate minimum can be taken
dcc.w.calcs$value <- as.numeric(dcc.w.calcs$value)

#Take the minimum criteria
#dplyr can't return whole rows efficiently yet. this is a data table solution retrieved from this 
#thread http://stackoverflow.com/questions/21308436/dplyr-filter-get-rows-with-minimum-of-variable-but-only-the-first-if-multiple
library(data.table)
dt <- as.data.table(dcc.w.calcs)
setkey(dt, code,value)
dt.min <- dt[J(unique(code)), mult="first"]
dcc.min <- as.data.frame(dt.min)
rm(dt, dt.min)

#Convert to appropriate sample fraction when appropriate fraction is NOT available EXCEPT where the sample is ND. 
dcc.min$tResult.old <- dcc.min$tResult
dissolved.metals.criteria <- unique(dcc.min[grep('Dissolved',dcc.min$Criteria.Name.full),c('Criteria.Name.full')])
sub1 <- dcc.min[dcc.min$Criteria.Name.full %in% dissolved.metals.criteria,]
sub2 <- merge(sub1, td.conv, by = 'code', all.x = TRUE)
sub2$x <- apply(sub2[,names(sub2)],1,paste,collapse=',')
sub2 <- sub2[!duplicated(sub2$x),]
sub2 <- within(sub2, rm(x))
sub3 <- sub2[sub2$Criteria.Name.full != sub2$Name.full,]
sub4 <- sub3[sub3$dnd == 1,] 
sub4[sub4$Criteria.Name.full == 'Chromium (Hex), Dissolved',c('CFA')] <- 0.982 
sub4[sub4$Criteria.Name.full == 'Chromium (Hex), Dissolved',c('CFC')] <- 0.962
sub4[sub4$Criteria.Name.full == 'Chromium (Hex), Dissolved',c('CFA_SW','CFC_SW')] <- 0.993
sub4[sub4$Criteria.Name.full == 'Selenium, Dissolved', 'CFA'] <- 0.996
sub4[sub4$Criteria.Name.full == 'Selenium, Dissolved', 'CFC'] <- 0.922
sub4[sub4$Criteria.Name.full == 'Selenium, Dissolved', c('CFC_SW','CFA_SW')] <- 0.998 
sub4$tResult <- ifelse(grepl('Chronic',sub4$variable),
                       ifelse(grepl('Saltwater',sub4$variable),
                              sub4$tResult*sub4$CFC_SW,
                              sub4$tResult*sub4$CFC),
                       ifelse(grepl('Saltwater',sub4$variable),
                              sub4$tResult*sub4$CFA_SW,
                              sub4$tResult*sub4$CFA))

sub4 <- sub4[,names(dcc.min)]
dcc.min.wo.sub4 <- dcc.min[!dcc.min$index %in% sub4$index,]
dcc.min <- rbind(dcc.min.wo.sub4, sub4)

#Make sure tResult and criteria values are numeric
dcc.min$tResult <- as.numeric(dcc.min$tResult)
dcc.min$value <- as.numeric(dcc.min$value)

#evaluate exceedances to the minimum criteria
dcc.min$exceed <- ifelse(dcc.min$criterianame.x == 'Alkalinity',ifelse(dcc.min$tResult >= dcc.min$value,0,1),ifelse(dcc.min$tResult <= dcc.min$value,0,1))

#### Determining which are valid exceedances ####
#Where the MRL is greater than the criteria we can't use that sample to determine attainment or non-attainment
dcc.min$Valid <- ifelse(dcc.min$dnd == 1,1,ifelse(dcc.min$tMRL <= dcc.min$value, ifelse(dcc.min$tMRL == 0, 0, 1), 0))

#Dissolved versus total criteria
dtc <- dcc.min[dcc.min$Criteria.Name.full != dcc.min$Name.full & dcc.min$Fraction == 'Dissolved' & grepl('Total',dcc.min$Criteria.Name.full),]
dtc$Valid <- ifelse(dtc$tResult > dtc$value,ifelse(dtc$Valid == 0,0,1),0)
dcc.min.wo.dtc <- dcc.min[!dcc.min$index %in% dtc$index,]
dcc.min <- rbind(dcc.min.wo.dtc, dtc)

#When we have both Total and Dissolved for a sample we only want to continue with the appropriate fraction for the standard
dcc.min$newcode <- ifelse(is.na(dcc.min$Criteria.Name.full),'',paste(dcc.min$SampleRegID, dcc.min$Sampled, dcc.min$Criteria.Name.full))
td.dup <- dcc.min[dcc.min$newcode %in% dcc.min[duplicated(dcc.min$newcode),'newcode'] & dcc.min$newcode != '',]
td.dup$remove <- ifelse(td.dup$Name.full != td.dup$Criteria.Name.full,1,0)
#There's always a complication and for Chromium, we have both Chrom 6 and Non-specific chrom. We want to take the non-specific chrom when the 
#Chrom 6 MRL is above the criterion in the cases below
cr6 <- names(table(dcc.min[dcc.min$newcode %in% dcc.min[duplicated(dcc.min$newcode),'newcode'] & dcc.min$newcode != '','newcode'])[table(dcc.min[dcc.min$newcode %in% dcc.min[duplicated(dcc.min$newcode),'newcode'] & dcc.min$newcode != '','newcode']) > 2])
td.dup[td.dup$newcode == 'LW2-W011 2006-11-03 10:30:00 Chromium (Hex), Dissolved' & td.dup$Name.full == 'Chromium(VI), Total recoverable','remove'] <- 0
td.dup[td.dup$newcode == 'LW2-W016-2 2004-11-30 14:30:00 Chromium (Hex), Dissolved' & td.dup$Name.full == 'Chromium(VI), Total recoverable','remove'] <- 0
td.dup[td.dup$newcode == 'LW2-W016 2004-11-30 13:35:00 Chromium (Hex), Dissolved' & td.dup$Name.full == 'Chromium(VI), Total recoverable','remove'] <- 0
td.dup[td.dup$newcode == 'LW2-W016 2005-03-15 09:23:00 Chromium (Hex), Dissolved' & td.dup$Name.full == 'Chromium, Dissolved','remove'] <- 0
td.dup[td.dup$newcode == 'LW2-W016 2005-07-18 09:03:00 Chromium (Hex), Dissolved' & td.dup$Name.full == 'Chromium, Dissolved','remove'] <- 0
dcc.min.wo.td.dup <- dcc.min[!dcc.min$index %in% td.dup$index,]
td.keep <- td.dup[td.dup$remove == 0,]
td.keep <- within(td.keep, rm(remove))
dcc.min <- rbind(dcc.min.wo.td.dup, td.keep)
