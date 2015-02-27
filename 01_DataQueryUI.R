#### Define geographic area for search ####
#Specify the Plan Area: One of uni  agwqma$PlanName:
# [1] "Burnt River"                                  "Clackamas"                                    "Coos-Coquille"                               
# [4] "Crooked River"                                "Curry County"                                 "Goose and Summer Lakes"                      
# [7] "Greater Harney Basin"                         "Hells Canyon"                                 "Hood River"                                  
# [10] "Inland Rogue"                                 "Klamath Headwaters"                           "Lost River"                                  
# [13] "Lower Deschutes"                              "Lower John Day"                               "Lower Snake Asotin"                          
# [16] "Lower Willamette"                             "Malheur River"                                "Mid Coast"                                   
# [19] "Middle Deschutes"                             "Middle John Day"                              "Middle Willamette"                           
# [22] "Molalla-Pudding French Prairie North Santiam" "North and Middle Forks John Day"              "North Coast"                                 
# [25] "Owyhee"                                       "Powder-Brownlee"                              "Sandy Basin"                                 
# [28] "South Santiam"                                "Southern Willamette Valley"                   "Thousand Virgin"                             
# [31] "Tualatin River Subbasin"                      "Umatilla Basin"                               "Umpqua River"                                
# [34] "Upper Deschutes"                              "Upper Grande Ronde"                           "Upper Mainstem and South Fork John Day River"
# [37] "Upper Willamette-Siuslaw"                     "Walla Walla"                                  "Wallowa"                                     
# [40] "Willow Creek"                                 "Yamhill"   
myArea <- 'Inland Rogue' 

#### Define characteristics to query ####
# #First get the list of Characteristic names from the WQP. These names are consistent with EPA's SRS. 
# wqp.characteristics <- WQP.domain.get('Characteristicname')

#The entire list of parameters that match to a criteria
parms <- read.csv('WQP_Table3040_Names.csv', stringsAsFactors = FALSE)

#grab just the parameters we want
characteristics <- paste(parms[parms$WQP.Name %in% c('Temperature, water','pH','Escherichia coli','Fecal Coliform',
                                                     'Fecal coliforms', 'Enterococci', 'Enterococcus'),'WQP.Name'],collapse=';')

#### Define start and end date ####
#The expected format is mm-dd-yyyy
startDate <- '01-01-1995'
endDate <- '02-01-2015'

#generate dfs
source('./01_DataQuery.R')