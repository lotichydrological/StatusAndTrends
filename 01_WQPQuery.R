#Unfortunately, this got pretty messy as I was trying a few different things and learning how to work with the WQP.
#The most reusable portion of this code is up to line 193. After that it was working solely with the specific data set 
#i retrieved in order to add data necessary for calculated criteria

#### Using the water quality portal REST service for Characteristic names ####
library(RCurl)
library(XML)
#library(RODBC)

options(stringsAsFactors = FALSE)

source('wqpquery_functions.R')

#NEED TO REWRITE SO WE DON"T NEED THIS FUNCTION#
#con <- odbcConnect('WQAssessment')

#### Define state to query ####
#you can use the wqp.domain.get if you want outside Oregon but here is the Oregon code
Oregon <- 'US%3A41'

#### Define site types to query ####
#Returns list of available domain values for site type
wqp.siteTypes <- WQP.domain.get('Sitetype')

#Using the wqp.siteTypes enter the values you want to query for in siteType.
#Separate each value with the URL encoded semi-colon '%3B'. For values with commas use the URL encoded value '%2C+'
siteType = 'Estuary%3BOcean%3BStream%3BStream%3BLake%2C+Reservoir%2C+Impoundment'

#### Define sample media to query ####
wqp.sampleMedia <- WQP.domain.get('Samplemedia')

#Separate each value you want to query with the URL encoded semi-colon '%3B'.
sampleMedia <- 'Water'

#### Define characteristics to query ####
#First get the list of Characteristic names from the WQP. These names are consistent with EPA's SRS. 
wqp.characteristics <- WQP.domain.get('Characteristicname')

## Compare to Table 30 names ##
#Pull in the compiled criteria table used for the Toxics Monitoring prgram
source('//deqlead01/wqm/TOXICS_2012/Data/R/criteria.R')

#Select only those parameters that have a Table 30 or Table 40 criterion.
deq.pollutants <- criteria.values.melted.applicable[criteria.values.melted.applicable$variable %in% 
                                                      c('Table 40 Human Health Criteria for Toxic Pollutants - Water + Organism',
                                                        'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only',
                                                        'Table 30 Toxic Substances - Freshwater Acute',
                                                        'Table 30 Toxic Substances - Freshwater Chronic',
                                                        'Table 30 Toxic Substances - Saltwater Acute',
                                                        'Table 30 Toxic Substances - Saltwater Chronic'),]

#look for matching names in the Water Quality Portal
matched <- deq.pollutants[deq.pollutants$Pollutant %in% wqp.characteristics$value,]

#Identify the parameters we need to resolve for naming issues.
not.matched <- deq.pollutants[!deq.pollutants$Pollutant %in% wqp.characteristics$value,]

#output DEQ table pollutants that we do not have a match for
#write.csv(unique(not.matched$Pollutant),'//deqhq1/wqassessment/2012_WQAssessment/ToxicsRedo/WQPNameMatch.csv')

#once the matches have been identified
to.match <- read.csv('//deqhq1/wqassessment/2012_WQAssessment/ToxicsRedo/WQPNameMatch.csv', stringsAsFactors = FALSE)

#for PCBs the standard is a total of all aroclors and congeners and so is a many to one. No comparison is done to individual compounds.
#given that I'll list out all the aroclor and pcb names to include in the wqp query after taking out the blank match.
#to.match[to.match$Criteria.Name != 'Polychlorinated Biphenyls (PCBs)',]
aroclors <- wqp.characteristics[grep('[Aa]roclor',wqp.characteristics$value),'value']
pcbs <- wqp.characteristics[grep('[Pp][Cc][Bb]',wqp.characteristics$value),'value']

#then we put the characterisitic names together
#The last character vector here includes parameters that were missed in the initial parameter identification
#as well as parameters necessary to calculate sample specific criteria.
to.query <- c(to.match$WQP.Name, 
              aroclors, 
              pcbs, 
              matched$Pollutant, 
              c('pH','Temperature, water','Temperature','Hardness, Ca, Mg','Hardness, Ca, Mg as CaCO3',
                'Hardness, Calcium','Hardness, carbonate','Hardness, carbonate as CaCO3','Hardness, magnesium',
                'Total Hardness','Calcium','Magnesium','Calcium as CaCO3','Magnesium as CaCO3','Ammonia', 'Ammonia as N',
                'Chlordane, technical, and/or chlordane metabolites','Oxychlordane','cis-Nonachlor','trans-Nonachlor',
                'Nonachlor','trans-Chlordane','cis-Chlordane','Chlordane, technical'))
to.query <- to.query[!to.query %in% c('Dinitrophenol','','Nitrosamine')]
to.query <- unique(to.query)

#build out table for relate back to criteria names so it's a whole table operation and not based on similar names
#which make it hard to track
to.add <- data.frame('Criteria.Name' = matched$Pollutant, 
                     'WQP.Name' = matched$Pollutant, 
                     'Requires.followup' = rep(0,length(matched$Pollutant)), 
                     'DEQ.Table.name' = matched$Pollutant)
to.add <- to.add[!duplicated(to.add$Criteria.Name),]

wqp.criteria.relate <- rbind(to.match[,c(2:5)], to.add)

#write.csv(wqp.criteria.relate,'//deqhq1/wqassessment/2012_WQAssessment/ToxicsRedo/WQPNameMatch_05142014.csv',row.names = FALSE)

#somehow chlordane isn't getting into the to.query vector now when it originally did at index 204
#this code inserts it back into that specific index
#to.query.start <- to.query[1:203]
#to.query.end <- to.query[204:length(to.query)]
#to.query <- c(to.query.start, 'Chlordane', to.query.end)

#### Define start and end date ####
#The expected format is mm-dd-yyyy
startDate <- '01-01-2000'
endDate <- '12-31-2011'

#### Pass the query using the arguments you defined above ####
#Note that you can also pass different geographical scales but that is not currently built into this
#Please refer to the linked page from the function definition for other available search parameters

#This code was run the first time to set up the tracking table
#success.table <- data.frame('Characteristic' = to.query, 
#                            'Success' = rep('Not Run',length(to.query)), 
#                            'stations' = rep(0, length(to.query)),
#                            'samples' = rep(0,length(to.query)),stringsAsFactors = FALSE)

#This next block pulls in the tracking table to see what's been done up to this point
success.table <- sqlFetch(con, 'WQPQuery_Status')

#Need to add in Ammonia and Ammonia as N to the success table. This parameter has only ph and temperature dependent criteria and so was 
#missed when the criteria value column was coerced to numeric.
success.table <- rbind(success.table, data.frame('Characteristic' = c('Ammonia','Ammonia as N','Chlordane, technical, and/or chlordane metabolites','Oxychlordane','cis-Nonachlor','trans-Nonachlor',
                                                                      'Nonachlor','trans-Chlordane','cis-Chlordane','Chlordane, technical'), 
                                                 'Success' = rep('Not run',10), 
                                                 'stations' = rep(0,10), 
                                                 'samples' = rep(0,10), stringsAsFactors = FALSE))

#we have to pull this back from the databse to add to it now that we have parameters to add.
wqp.stations <- sqlFetch(con, 'WQPStations_05022014')
wqp.stations <- within(wqp.stations, rm(x))

#I set up a loop to pass queries because I was having problems with the query taking way too long to run and when 
#problems came up it meants having to start over. The loop queries each parameter in the list one by one and adds the data
#to the whole data table. The stations are queried first and if there is no data then is.character(tmp.stations) returns false.
#Otherwise, the data query runs. There is a validator script available within the WQP REST framework but I found it easier and faster
#to just see if any stations return. Since we are constantly hitting their server sometimes they reject the request. In this case the loop will
#break and you'll need to wait a few minutes and use the iterator specified in the prompt to plug into the start of the for loop. If the 
#query is successful the data table is added to. The stations are added to a dataframe and duplicates are removed at the end of the loop and
#then can be written to the database after the loop with the success table.
for (i in 259:length(to.query)) {
  tmp.stations <- wqp.station.query(stateCode = Oregon, 
                                    siteType = siteType, 
                                    sampleMedia = sampleMedia, 
                                    characteristicName = to.query[i], 
                                    startDate = startDate, 
                                    endDate = endDate)
  
  if (names(tmp.stations)[1] != 'OrganizationIdentifier') {
    print('The server is not accepting queries at this time. Please wait a bit to resume.')
    print(paste('You can resume with the iterator', i, 'which is parameter', to.query[i]))
    break
  }
  
  if(is.character(tmp.stations)) {
    print(paste('Awww shucks. Parameter',to.query[i],'has no data'))
    success.table[i,'Success'] <- 'Unsuccessful'
  } else {
    tmp.data <- wqp.data.query(stateCode = Oregon, 
                               siteType = siteType, 
                               sampleMedia = sampleMedia, 
                               characteristicName = to.query[i], 
                               startDate = startDate, 
                               endDate = endDate)
    
    if (names(tmp.data)[1] != 'OrganizationIdentifier') {
      print('The server is not accepting queries at this time. Please wait a bit to resume.')
      print(paste('You can resume with the iterator', i, 'which is parameter', to.query[i]))
      break
    }
    
    if(nrow(tmp.data) > 0) {
      if(i == 1) {
        wqp.stations <- tmp.stations
        sqlSave(con, tmp.data, 'WQPData_05022014', varTypes = WQPvarTypes)
      } else {
        names(tmp.stations) <- gsub('\\.','',names(tmp.stations))
        wqp.stations <- rbind(wqp.stations, tmp.stations)
        tmp.data$USGSPCode <- as.character(tmp.data$USGSPCode)
        tmp.data$DetectionQuantitationLimitMeasure.MeasureValue <- as.numeric(tmp.data$DetectionQuantitationLimitMeasure.MeasureValue)
        sqlSave(con, tmp.data, 'WQPData_05022014', append = TRUE, varTypes = WQPvarTypes, rownames = FALSE)
      }
      print(paste('Success for Parameter:',to.query[i]))
      
      success.table[i,'Success'] <- 'Success!'

      
      print(paste('There are', nrow(tmp.stations), 'stations with', nrow(tmp.data), 'samples for',to.query[i]))
      
      print(paste(length(to.query)-i,'parameters left to query')) 
      
    } else {
      print(paste('No data available for', to.query[i]))
      success.table[i,'Success'] <- 'No data returned'
    }
    
    success.table[i,'samples'] <- nrow(tmp.data)
    success.table[i,'stations'] <- nrow(tmp.stations)
   
    rm(tmp.data)
    gc()
  }
  
  wqp.stations$x <- apply(wqp.stations[,names(wqp.stations)],1,paste,collapse=',')
  wqp.stations <- wqp.stations[!duplicated(wqp.stations$x),]
  wqp.stations <- within(wqp.stations, rm(x))
}

#This removes additional duplicate stations that have differences in other fields that don't matter so we only use the ID and the VerticalMeasure
wqp.stations$x <- apply(wqp.stations[,c('MonitoringLocationIdentifier', 'VerticalMeasureMeasureValue')],1,paste, collapse = ',')
wqp.stations.dups.removed <- wqp.stations[!duplicated(wqp.stations$x),]
wqp.stations.dups.removed <- within(wqp.stations.dups.removed, rm(x))

#This is where you can save the updated tables to the database
#sqlSave(con, wqp.stations.dups.removed, 'WQPStations_05022014', rownames = FALSE)
#sqlSave(con, success.table, 'WQPQuery_Status', rownames = FALSE)

#This pulls the data from the database
wqp.data <- sqlFetch(con, 'WQPData_05022014')
#wqp.data[wqp.data$CharacteristicName == '']

#we need to add conductivity to the list of parameters to query
cond.to.add.to.st <- data.frame('Characteristic' = c('Conductivity','Specific conductance'), 
                                'Success' = rep('Not run',2), 
                                'stations' = rep(0,2), 
                                'samples' = rep(0,2))
sqlSave(con, cond.to.add.to.st, 'WQPQuery_Status', append = TRUE, rownames = FALSE)
st <- sqlFetch(con, 'WQPQuery_Status')

#and salinity
sal.to.add.to.st <- data.frame('Characteristic' = c('Salinity'), 
                                'Success' = rep('Not run',1), 
                                'stations' = rep(0,1), 
                                'samples' = rep(0,1))
sqlSave(con, sal.to.add.to.st, 'WQPQuery_Status', append = TRUE, rownames = FALSE)
st <- sqlFetch(con, 'WQPQuery_Status')

#### Query for temperature, conductivity and salinity at ammonia ####
#We only want to query temperature and conductivity (or salinity) for where we have ammonia
#the function i had wasn't really working for some reason so I pasetd the function steps here
#this isn't really returning much. but it takes a long time to query temperature in the whole state
#so it's hard to check if it's working right.
ammonia.stations <- paste(as.character(unique(wqp.data[grep('Ammonia',wqp.data$CharacteristicName),'MonitoringLocationIdentifier'])),collapse=';')
# tmp.stations.ammonia <- wqp.station.query(stateCode = Oregon, 
#                                   siteType = siteType, 
#                                   sampleMedia = sampleMedia, 
#                                   characteristicName = (paste(as.character(st[c(245,246,268,269,270),'Characteristic']),collapse=';')), 
#                                   startDate = startDate, 
#                                   endDate = endDate)
theDataURL.ammonia.calc <- paste('http://www.waterqualitydata.us/Result/search?',
                                        'statecode=', Oregon,
                                        '&siteType=', siteType, 
                                        '&siteid=', URLencode.PTB(ammonia.stations),
                                        '&sampleMedia=', sampleMedia,
                                        '&characteristicName=', URLencode.PTB(paste(as.character(st[c(245,246,268,269,270),'Characteristic']),collapse=';')),
                                        '&startDateLo=', startDate,
                                        '&startDateHi=', endDate,
                                        '&mimeType=csv', sep ='')

tmp.data.ammonia.calc <- getURL(theDataURL.ammonia.calc)
wqp.data.ammonia.calc <- read.csv(textConnection(tmp.data.ammonia.calc), stringsAsFactors = FALSE, as.is = c('ResultMeasureValue'))

#update success table
st[c(245,246,268,269,270),c('Success','stations','samples')] <- data.frame('Success' = c('Success!','No data returned','Success!','No data returned','No data returned'),
                                                                           'stations' = c(1,0,1,0,0),
                                                                           'samples' = c(35,0,39,0,0))

#### Query hardness data ####
#bring in names of crtieria for hardness calcs
source('//deqlead01/wqm/TOXICS_2012/Data/R/hardness_eval_functions_Element_Names.R')

#we don't need Silver because the chronic is always more stringent and is not hardness based
hardness.metals.to.query <- as.character(unique(constants[constants$Name.alone != 'Silver','Name.alone']))

#Using just the character vector from above with the SRS names for hardness in the wqp domain list
#this queries statewide. There were over 600 stations with hardness metals and i think it was too many for the WQP
#to take for station specific querying

#We already have the stations we want this for in our dataset so we only have to run the data query

# tmp.stations.hm <- wqp.station.query(stateCode = Oregon, 
#                                           siteType = siteType, 
#                                           sampleMedia = sampleMedia, 
#                                           characteristicName = paste(c('Hardness, Ca, Mg','Hardness, Ca, Mg as CaCO3',
#                                                                        'Hardness, Calcium','Hardness, carbonate','Hardness, carbonate as CaCO3','Hardness, magnesium',
#                                                                        'Total Hardness','Calcium','Magnesium','Calcium as CaCO3','Magnesium as CaCO3'),collapse=';'), 
#                                           startDate = startDate, 
#                                           endDate = endDate)

tmp.data.hm <- wqp.data.query(stateCode = Oregon, 
                                   siteType = siteType, 
                                   sampleMedia = sampleMedia, 
                                   characteristicName = paste(c('Hardness, Ca, Mg','Hardness, Ca, Mg as CaCO3',
                                                                'Hardness, Calcium','Hardness, carbonate','Hardness, carbonate as CaCO3','Hardness, magnesium',
                                                                'Total Hardness','Calcium','Magnesium','Calcium as CaCO3','Magnesium as CaCO3'),collapse=';'), 
                                   startDate = startDate, 
                                   endDate = endDate)

#Here we can drop the hardness data that is associated with stations where we don't have hardness metals data 
tmp.data.hm.to.keep <- tmp.data.hm[tmp.data.hm$MonitoringLocationIdentifier %in% as.character(unique(wqp.data[wqp.data$CharacteristicName %in% hardness.metals.to.query,'MonitoringLocationIdentifier'])),]

#This just updates the status table
st.update <- ddply(tmp.data.hm.to.keep, .(CharacteristicName), summarise, stations = length(unique(MonitoringLocationIdentifier)), samples = length(ResultMeasureValue))
st.update$success <- 'Success!'

st[as.character(st$Characteristic) %in% c('Hardness, Ca, Mg','Hardness, Ca, Mg as CaCO3',
                                          'Hardness, Calcium','Hardness, carbonate','Hardness, carbonate as CaCO3','Hardness, magnesium',
                                          'Total Hardness','Calcium','Magnesium','Calcium as CaCO3','Magnesium as CaCO3'),'Success'] <- 'No data returned'
st[as.character(st$Characteristic) %in% as.character(st.update$CharacteristicName),c('Success','stations','samples')] <- st.update[as.character(st.update$CharacteristicName) %in% as.character(st$Characteristic),c('success','stations','samples')]

#### Query pH data ####
#First I queried stations and checked against list of current stations.
#All stations returned are in the current station list so all that is necessary is the data query
tmp.stations.ph <- wqp.station.query(stateCode = Oregon, 
                                     siteType = siteType, 
                                     sampleMedia = sampleMedia, 
                                     characteristicName = to.query[244], 
                                     startDate = startDate, 
                                     endDate = endDate)

tmp.data.ph <- wqp.data.query(stateCode = Oregon, 
                           siteType = siteType, 
                           sampleMedia = sampleMedia, 
                           characteristicName = to.query[244], 
                           startDate = startDate, 
                           endDate = endDate)

#Update station table to include stations with ph only
names(tmp.stations.ph) <- gsub('\\.','',names(tmp.stations.ph))
wqp.stations <- rbind(wqp.stations.dups.removed, tmp.stations.ph)
wqp.stations$x <- apply(wqp.stations[,names(wqp.stations)],1,paste,collapse=',')
wqp.stations <- wqp.stations[!duplicated(wqp.stations$x),]
wqp.stations <- within(wqp.stations, rm(x))
wqp.stations$x <- apply(wqp.stations[,c('MonitoringLocationIdentifier', 'VerticalMeasureMeasureValue')],1,paste, collapse = ',')
wqp.stations.dups.removed <- wqp.stations[!duplicated(wqp.stations$x),]
wqp.stations.dups.removed <- within(wqp.stations.dups.removed, rm(x))

#sqlSave(con, wqp.stations.dups.removed, 'WQPStations_05052014', rownames = FALSE)

#update success table
st[st$Characteristic == 'pH','Success'] <- 'Success!'
st[st$Characteristic == 'pH','stations'] <- nrow(tmp.stations.ph)
st[st$Characteristic == 'pH','samples'] <- nrow(tmp.data.ph)

#save st to database
sqlSave(con, st, 'WQPQueryStatus_05052014', rownames = FALSE)

#### combine calculation data and add to data table ####
tmp.data.to.add <- rbind(wqp.data.ammonia.calc, tmp.data.hm.to.keep, tmp.data.ph)
#sqlSave(con, tmp.data.to.add, 'WQPData_05022014', append = TRUE, varTypes = WQPvarTypes, rownames = FALSE)
