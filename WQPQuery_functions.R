#### Using the water quality portal REST service for Characteristic names ####
library(RCurl)
library(XML)

#### Define domain value retrieval function ####
#This function takes a string as the value argument and retrieves domains in XML format and converts them to dataframes in R
#for the domains for the available search terms go to http://www.waterqualitydata.us/webservices_documentation.jsp#Domain
#As of 4/8/2014 the available parameter names include: countrycode, statecode, countycode, Sitetype, Organization, Samplemedia,
#Characteristictype, Characteristicname, providers
WQP.domain.get <- function(value) {
  theURL <- paste('http://www.waterqualitydata.us/Codes/', value, '?mimeType=xml', sep = '')
  tmp <- getURL(theURL)
  tmp.top <- xmlRoot(xmlTreeParse(tmp))
  tmp.df <- data.frame(value = rep("",length(tmp.top)),desc = rep("",length(tmp.top)),providers = rep("",length(tmp.top)), stringsAsFactors = FALSE)
  for (i in 1:length(tmp.top)) {
    tmp.df$value[i] <- xmlGetAttr(tmp.top[[i]],'value')
    tmp.df$desc[i] <- xmlGetAttr(tmp.top[[i]],'desc')
    tmp.df$providers[i] <- xmlGetAttr(tmp.top[[i]],'providers')  
  }
  return(tmp.df)
}

#### URL encoding function ####
#NOTE: This function has been modified to be consistent with WQP REST service encoding
URLencode.PTB <- function (URL, reserved = FALSE) 
{
  OK <- "[^-ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvywxyz0-9//_']"
  x <- strsplit(URL, "")[[1L]]
  z <- grep(OK, x)
  if (length(z)) {
    y <- sapply(x[z], function(x) {if(x == ' ') {
      '+'
    } else if (x == '('){
      '('
    } else if (x == ')'){
      ')'
    } else {
      paste0("%", as.character(charToRaw(x)), collapse = "")
    }
    })
    x[z] <- y
  }
  paste(x, collapse = "")
}

wqp.verification <- function(URL) {
  h <- basicHeaderGatherer()
  zz <- getURI(URL, headerfunction = h$update)
  
  if(as.numeric(h$value()["Total-Site-Count"]) == 0){
    return.val = FALSE
  } else {
    return.val = TRUE
  }
}

#### Water Quality Portal Station Query ####
#All arguments are required to be from the WQP Domain values accessible using the WQP.domain.get function
#This function will return a dataframe of stations whose data match the criteria specified.
wqp.station.query <- function(stateCode, siteType, sampleMedia, characteristicName, startDate, endDate) {#siteid,
  theStationURL <- paste('http://www.waterqualitydata.us/Station/search?',
                         'statecode=', Oregon,
                         '&siteType=', siteType, 
                         #'&siteid=', siteid,
                         '&sampleMedia=', sampleMedia,
                         '&characteristicName=', URLencode.PTB(characteristicName),
                         '&startDateLo=', startDate,
                         '&startDateHi=', endDate,
                         '&mimeType=csv', sep ='')
  
 # if(wqp.verification(theStationURL)) {
    tmp.stations <- getURL(theStationURL)
    if (tmp.stations != "") {
      wqp.stations <- read.csv(textConnection(tmp.stations), stringsAsFactors = FALSE)
    }
     
 # } else {
 #   print("There is a problem with your query. No results are returned")
 # }
}

#### Water Quality Portal Data Query ####
#All arguments are required to be from the WQP Domain values accessible using the WQP.domain.get function
#This function will return a dataframe of data which match the criteria specified.
wqp.data.query <- function(stateCode, siteType, sampleMedia, characteristicName, startDate, endDate) { #siteid,
  theDataURL <- paste('http://www.waterqualitydata.us/Result/search?',
                      'statecode=', Oregon,
                      '&siteType=', siteType, 
                      #'&siteid=', siteid,
                      '&sampleMedia=', sampleMedia,
                      '&characteristicName=', URLencode.PTB(characteristicName),
                      '&startDateLo=', startDate,
                      '&startDateHi=', endDate,
                      '&mimeType=csv', sep ='')
  
  tmp.data <- getURL(theDataURL)
  wqp.data <- read.csv(textConnection(tmp.data), stringsAsFactors = FALSE, as.is = c('ResultMeasureValue'))
  }

wqp.data.query.by.station <- function(stateCode, siteType, sampleMedia, characteristicName, startDate, endDate, siteid) { 
  theDataURL <- paste('http://www.waterqualitydata.us/Result/search?',
                      'statecode=', Oregon,
                      '&siteType=', siteType, 
                      '&siteid=', siteid,
                      '&sampleMedia=', sampleMedia,
                      '&characteristicName=', characteristicName,
                      '&startDateLo=', startDate,
                      '&startDateHi=', endDate,
                      '&mimeType=csv', sep ='')
  
  tmp.data <- getURL(theDataURL)
  wqp.data <- read.csv(textConnection(tmp.data), stringsAsFactors = FALSE, as.is = c('ResultMeasureValue'))
}

WQPvarTypes <- c('OrganizationIdentifier'= 'varchar(255)',
                 'OrganizationFormalName'= 'varchar(255)',
                 'ActivityIdentifier'= 'varchar(255)',
                 'ActivityTypeCode'= 'varchar(255)',
                 'ActivityMediaName'= 'varchar(255)',
                 'ActivityMediaSubdivisionName'= 'varchar(255)',
                 'ActivityStartDate'= 'varchar(255)',
                 'ActivityStartTime.Time'= 'varchar(255)',
                 'ActivityStartTime.TimeZoneCode'= 'varchar(255)',
                 'ActivityEndDate'= 'varchar(255)',
                 'ActivityEndTime.Time'= 'varchar(255)',
                 'ActivityEndTime.TimeZoneCode'= 'varchar(255)',
                 'ActivityDepthHeightMeasure.MeasureValue'= 'float',
                 'ActivityDepthHeightMeasure.MeasureUnitCode'= 'varchar(255)',
                 'ActivityDepthAltitudeReferencePointText'= 'varchar(255)',
                 'ActivityTopDepthHeightMeasure.MeasureValue'= 'float',
                 'ActivityTopDepthHeightMeasure.MeasureUnitCode'= 'varchar(255)',
                 'ActivityBottomDepthHeightMeasure.MeasureValue'= 'float',
                 'ActivityBottomDepthHeightMeasure.MeasureUnitCode'= 'varchar(255)',
                 'ProjectIdentifier'= 'varchar(255)',
                 'ActivityConductingOrganizationText'= 'varchar(255)',
                 'MonitoringLocationIdentifier'= 'varchar(255)',
                 'ActivityCommentText'= 'varchar(255)',
                 'SampleAquifer'= 'varchar(255)',
                 'HydrologicCondition'= 'varchar(255)',
                 'HydrologicEvent'= 'varchar(255)',
                 'SampleCollectionMethod.MethodIdentifier'= 'varchar(255)',
                 'SampleCollectionMethod.MethodIdentifierContext'= 'varchar(255)',
                 'SampleCollectionMethod.MethodName'= 'varchar(255)',
                 'SampleCollectionEquipmentName'= 'varchar(255)',
                 'ResultDetectionConditionText'= 'varchar(255)',
                 'CharacteristicName'= 'varchar(255)',
                 'ResultSampleFractionText'= 'varchar(255)',
                 'ResultMeasureValue'= 'float',
                 'ResultMeasure.MeasureUnitCode'= 'varchar(255)',
                 'MeasureQualifierCode'= 'varchar(255)',
                 'ResultStatusIdentifier'= 'varchar(255)',
                 'StatisticalBaseCode'= 'varchar(255)',
                 'ResultValueTypeName'= 'varchar(255)',
                 'ResultWeightBasisText'= 'varchar(255)',
                 'ResultTimeBasisText'= 'varchar(255)',
                 'ResultTemperatureBasisText'= 'varchar(255)',
                 'ResultParticleSizeBasisText'= 'varchar(255)',
                 'PrecisionValue'= 'varchar(255)',
                 'ResultCommentText'= 'varchar(255)',
                 'USGSPCode'= 'varchar(255)',
                 'ResultDepthHeightMeasure.MeasureValue'= 'float',
                 'ResultDepthHeightMeasure.MeasureUnitCode'= 'varchar(255)',
                 'ResultDepthAltitudeReferencePointText'= 'varchar(255)',
                 'SubjectTaxonomicName'= 'varchar(255)',
                 'SampleTissueAnatomyName'= 'varchar(255)',
                 'ResultAnalyticalMethod.MethodIdentifier'= 'varchar(255)',
                 'ResultAnalyticalMethod.MethodIdentifierContext'= 'varchar(255)',
                 'ResultAnalyticalMethod.MethodName'= 'varchar(255)',
                 'MethodDescriptionText'= 'varchar(255)',
                 'LaboratoryName'= 'varchar(255)',
                 'AnalysisStartDate'= 'varchar(255)',
                 'ResultLaboratoryCommentText'= 'varchar(255)',
                 'DetectionQuantitationLimitTypeName'= 'varchar(255)',
                 'DetectionQuantitationLimitMeasure.MeasureValue'= 'float',
                 'DetectionQuantitationLimitMeasure.MeasureUnitCode'= 'varchar(255)',
                 'PreparationStartDate' = 'varchar(255)')



