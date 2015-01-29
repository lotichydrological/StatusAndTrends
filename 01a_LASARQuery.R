library(RODBC)
library(xlsx)
library(plyr)
library(stringr)
library(reshape2)
library(foreign)

options(stringsAsFactors = FALSE, scipen = 100)

get.cases <- function(chk.values) {
  ## Checks for non-numeric values in the vector "chk.values", which should
  ## be a character vector. A data.frame is returned with the non-numeric
  ## values (cases) and the number of occurrences for each case. If there
  ## are olnly numeric values in the input vectore, the entries in the 
  ## data.frame returned are "No non-numeric values found" for the case
  ## and NA for the count
  ## Created by Kevin Brannan
  ## Version 1.0.0.09.20.2012
  tmp.cases <- chk.values[grep("[^0-9.]",chk.values)][!duplicated(chk.values[grep("[^0-9.]",chk.values)])]
  if(length(tmp.cases) > 0){
    tmp.cases.report <- data.frame(Case = tmp.cases,Count=as.numeric(NA))
    for(ii in 1:length(tmp.cases)){
      tmp.cases.report$Count[ii] <- length(grep(tmp.cases.report$Case[ii],chk.values))
    }
  } else{
    tmp.cases.report <- data.frame("No non-numeric values found",NA)
    names(tmp.cases.report) <- c("Case","Count")
  }
  return(tmp.cases.report)
}

sub.cases <- function(data.in,sub.table){
  ## Replaces non-numeric values of data.in with the correspoinding elements
  ## in sub.table. The sub.table dataframe should be generated using the 
  ## get.cases function
  ## Created by Kevin Brannan
  ## Version 1.0.0.09.20.2012
  for(ii in 1:length(sub.table$Sub)){
    sub.index <- data.in == sub.table$Case[ii]  #grep(sub.table$Case[ii],data.in, fixed = TRUE)
    print(paste("Number of sub for ", sub.table$Case[ii], " is ",sub.table$Sub[ii],sep=""))
    if(length(sub.index)> 0){
      data.in[data.in == sub.table$Case[ii]] <- as.character(sub.table$Sub[ii])
      rm(sub.index)
    }
  }
  return(data.in)
}


#set up to query LASAR from deqlead-lims
con <- odbcConnect('LASAR')

#Pull in the compiled criteria table used for the Toxics Monitoring prgram
source('//deqhq1/wqassessment/2012_WQassessment/ToxicsRedo/TMP-Rcode/criteria.R')

#build out query
#we need to get lasar names
#First we pull the LASAR table with every parameter name in it
all.parameters <- sqlFetch(con, 'PARAMETER')
#Then we pull in those pollutants that are on Table 30 or Table 40
deq.pollutants <- criteria.values.melted.applicable[criteria.values.melted.applicable$variable %in% 
                                                      c('Table 40 Human Health Criteria for Toxic Pollutants - Water + Organism',
                                                        'Table 40 Human Health Criteria for Toxic Pollutants - Organism Only',
                                                        'Table 30 Toxic Substances - Freshwater Acute',
                                                        'Table 30 Toxic Substances - Freshwater Chronic',
                                                        'Table 30 Toxic Substances - Saltwater Acute',
                                                        'Table 30 Toxic Substances - Saltwater Chronic'),]
#Several of the pollutants have multiple criteria but we only care about the names not the individual criteria here
deq.pollutants <- deq.pollutants[!duplicated(deq.pollutants$Pollutant),]
#Here we make a dataframe with two identical columns. One will preserve the LASAR name and the other will be used 
#to show us which names we already match exactly
lasar.names <- data.frame('Pollutant' = all.parameters[all.parameters$NAME %in% deq.pollutants$Pollutant,'NAME'], 
                          'lasar.name' = all.parameters[all.parameters$NAME %in% deq.pollutants$Pollutant,'NAME'])
#This combines the criteria table with the names from LASAR. The rows that don't have a match are the ones we need to find lasar 
#name matches for
lasar.names.match <- merge(data.frame('Pollutant' = deq.pollutants[,'Pollutant']), lasar.names, by = 'Pollutant', all.x = TRUE)
#Some of the metals didn't match because we are carrying fraction in the criteria table name. This just fills them in accordingly
lasar.names.match[grep('Dissolved',lasar.names.match$Pollutant),'lasar.name'] <- gsub(', Dissolved', "", lasar.names.match[grep('Dissolved',lasar.names.match$Pollutant),'Pollutant'])
lasar.names.match[grep('Total recoverable',lasar.names.match$Pollutant),'lasar.name'] <- gsub(', Total recoverable', "", lasar.names.match[grep(', Total recoverable',lasar.names.match$Pollutant),'Pollutant'])

#This was some review code
#View(arrange(all.parameters[grep('ichlorobromo', all.parameters$NAME),],NAME))
#all.parameters[which(all.parameters$CAS_NUMBER == 541731),]
#lasar.names.match[is.na(lasar.names.match$lasar.name),]

#We are going to fill in the NA's with the deq criteria names and then use this rename vector to fill in the lasar names
rename.vector <- c("Alkalinity" = "Alkalinity as Calcium Carbonate",
                   "Arsenic, Total inorganic" = "Arsenic",
                   "Azinphos methyl" = "Azinphos Methyl",
                   "Chloronaphthalene 2" = "2-Chloronaphthalene",
                   "Chlorodibromomethane" = "Dibromochloromethane",
                   "Chloroethyl Ether bis 2"= "Bis(2-chloroethyl)ether",
                   "Chloroisopropyl Ether bis 2" = "Bis(2-chloroisopropyl) ether",
                   "Chloromethyl ether, bis" = "bis(Chloromethyl)ether",
                   "Chlorophenol 2" = "2-Chlorophenol",
                   "Chlorophenoxy Herbicide (2,4,5,-TP)" = "Silvex",
                   "Chromium (Hex)" = 'Chromium, hexavalent',
                   "Di-n-butyl Phthalate" = "di-n-Butylphthalate",
                   "Dichlorobenzene(m) 1,3" = "1,3-Dichlorobenzene",
                   "Dichlorobenzene(o) 1,2" = "1,2-Dichlorobenzene",
                   "Dichlorobenzene(p) 1,4" = "1,4-Dichlorobenzene",
                   "Dichlorobenzidine 3,3'" = "3,3`-Dichlorobenzidine",
                   "Dichlorobromomethane" = "Bromodichloromethane",
                   "Dichloroethane 1,2" = "1,2-Dichloroethane",
                   "Dichloroethylene 1,1" = "1,1-Dichloroethylene",
                   "Dichloroethylene trans 1,2" = "trans-1,2-Dichloroethylene",
                   "Dichlorophenol 2,4" = "2,4-Dichlorophenol",
                   "Dichloropropane 1,2" = "1,2-Dichloropropane",
                   "Dichloropropene 1,3" = "1,3-Dichloropropene",
                   "Dimethylphenol 2,4" = "2,4-Dimethylphenol",
                   "Dinitrophenol 2,4" = "2,4-Dinitrophenol",
                   "Dinitrophenols" = "Dinitrophenol",
                   "Dioxin (2,3,7,8-TCDD)" = "2,3,7,8-TCDD",
                   "Diphenylhydrazine 1,2" = "1,2-Diphenylhydrazine",
                   "Endosulfan" = "5-Norbornene-2, 3-dimethanol, 1,4,5,6,7,7-hexachloro cyclic sulfite",
                   "Endosulfan Sulfate" = "Endosulfan sulfate",
                   "Heptachlor Epoxide" = "Heptachlor epoxide",
                   "Hexachlorocyclo-hexane-Technical" = "HCH",
                   "Methyl Bromide" = "Bromomethane",
                   "Methyl-4,6-dinitrophenol 2" = "2-Methyl-4,6-dinitrophenol",
                   "Nitrates" = "Nitrate/nitrite as N",
                   "Nitrosamines"  = "Nitrosamine, NOS",
                   "Nitrosodi-n-propylamine, N" = "n-Nitroso-di-n-propylamine",
                   "Nitrosodibutylamine, N" = "n-Nitroso-di-n-butylamine",
                   "Nitrosodiethylamine, N" = "n-Nitrosodiethylamine",
                   "Nitrosodimethylamine, N" = "n-Nitrosodimethylamine",
                   "Nitrosodiphenylamine, N" = "n-Nitrosodiphenylamine",
                   "Nitrosopyrrolidine, N" = "n-Nitrosopyrrolidine",
                   "Phosphorus Elemental" = "Phosphorus",
                   "Sulfide Hydrogen Sulfide" = "Hydrogen Sulfide",
                   "Tetrachlorobenzene, 1,2,4,5-" = "1,2,4,5-Tetrachlorobenzene",
                   "Tetrachloroethane 1,1,2,2" = "1,1,2,2-Tetrachloroethane",
                   "Toxaphene" = "Toxaphene (technical)",
                   "Trichlorobenzene 1,2,4" = "1,2,4-Trichlorobenzene",
                   "Trichloroethane 1,1,2" = "1,1,2-Trichloroethane",
                   "Trichlorophenol 2,4,6" = "2,4,6-Trichlorophenol",
                   "Trichlorophenol, 2, 4, 5-" = "2,4,5-Trichlorophenol")
#Here we fill in the NA's with the deq table names
lasar.names.match$lasar.name <- ifelse(is.na(lasar.names.match$lasar.name),lasar.names.match$Pollutant,lasar.names.match$lasar.name)
#Here we convert all those names to what they were in lasar
lasar.names.match$lasar.name <- mapvalues(lasar.names.match$lasar.name, from = names(rename.vector), to = rename.vector)
#It's a one to many relationship from deq criteria table to lasar names so this adds some of those in
to.add <- data.frame('Pollutant' = c('Alkalinity', 'Arsenic, Total inorganic', 'Arsenic, Total inorganic', "Azinphos methyl", 
                                     "Azinphos methyl", "Azinphos methyl","Chloroethyl Ether bis 2","Chlorophenoxy Herbicide (2,4,5,-TP)",
                                     'Chromium, Dissolved',"Dichlorobenzidine 3,3'","Dichloroethane 1,2", "Dichloroethylene 1,1",
                                     "Dichloroethylene trans 1,2", "Dichloropropene 1,3","Methyl-4,6-dinitrophenol 2","Nitrates",
                                     "Nitrates","Nitrosodibutylamine, N","Nitrosodimethylamine, N","Nitrosopyrrolidine, N",
                                     "Phosphorus Elemental"),
                     'lasar.name' = c('Carbonate Alkalinity as Calcium Carbonate', 'Arsenic, ion (As3+)', 'Arsenic, ion (As5+)',
                                      'Azinophos Methyl','Guthion','Guthion (Azinphosmethyl)','Bis(2-Chloroethyl) ether','2,4,5-TP (Silvex)',
                                      'Chromium, trivalent','1,1`-Biphenyl-4,4`-diamine, 3,3`-dichloro-',"Ethylene dichloride", 
                                      "1,1-Dichloroethene", "trans-1,2-Dichloroethene", 'Propene, 1,3-dichloro-','4,6-Dinitro-2-methylphenol',
                                      "Nitrate", "Nitrate as N",'1-Butanamine, N-butyl-N-nitroso-',"Methanamine, N-methyl-N-nitroso",
                                      "Pyrrole, tetrahydro-N-nitroso-", "Total Phosphorus"))
lasar.names.match <- rbind(lasar.names.match, to.add)

#This df is useful for matching to criteria names later
#write.csv(lasar.names.match, 'lasar_names_match.csv', row.names = FALSE)

#This finds all the aroclors and PCBs that aren't in the lasar.names.match df because the criteria is for Total PCBs or Aroclors
aroclors <- all.parameters[grep('[Aa]roclor',all.parameters$NAME),'NAME']
pcbs <- all.parameters[grep('[Pp][Cc][Bb]',all.parameters$NAME),'NAME']

#This concatenates the elements of the vector into a single string that can be inserted into the LASAR query
#A couple parameters were added after the fact since they were missed in the first couple attempts
parameters <- paste("'",paste(c(aroclors, 
                                pcbs, 
                                unique(lasar.names.match$lasar.name),
                                'Ammonia as N',
                                'Orthophosphate as P'),collapse="','"),"'",sep="")

#Here we build the text of the query. I tested this out using SQL Server Management Studio. The advantages of this query is that it does
#all the lookup for you via the table joins instead of having to pull each table into R separately. The disadvantage is that the table structures
#in LASAR are remarkably disorganized and inconsistently populated and finding inconsistencies is difficult. Had we pulled each individual table
#into R we may have discovered that the QA_QC_TYPE in the Parameter_Result table was not consistently populated and resulted in several rows
#being excluded due to the nature of the inner join. Instead, the QA_QC_TYPE is consistently populated in the Sample table. This wasn't discovered
#until we were in the review stages of the Assessment_Report_Summary. This one issue isn't such a big deal to fix but it really calls into question
#if there are other issues that we are missing.
query <- paste("SELECT pr.[PARAMETER_KEY],
               pr.[Result],
               p.[NAME],
               pm.[ABBREVIATION],
               pm.ABBREVIATION + p.[NAME] as 'Name.full',
               s.[SAMPLE_DATE],
               s.[SAMPLE_TIME],
               s.[STATION_KEY],
               sn.[LOCATION_DESCRIPTION],
               sm.SAMPLE_MATRIX,
               pr.METHOD_DETECTION_LIMIT,
               pr.METHOD_REPORTING_LIMIT,
               q.QA_QC_TYPE,
               st.STATUS,
               u.UNIT,
               ss.SUBPROJECT_NAME
              FROM [LASAR].[dbo].[PARAMETER_RESULT] pr JOIN [LASAR].[dbo].[PARAMETER] p on 
               pr.PARAMETER_KEY = p.PARAMETER_KEY JOIN
               [LASAR].[dbo].[PARAMETER_MODIFIER] pm on 
               pr.PARAMETER_PREFIX_1 = pm.MODIFIER_KEY JOIN
               [LASAR].[dbo].[SAMPLE] s on
               pr.SAMPLE_KEY = s.SAMPLE_KEY JOIN
               [LASAR].dbo.XLU_QA_QC_TYPE q on
               q.QA_QC_TYPE_KEY = pr.QA_QC_TYPE JOIN
               [LASAR].dbo.XLU_STATUS st on
               st.XLU_STATUS_KEY = pr.QA_QC_STATUS JOIN
               [LASAR].dbo.UNIT u on
               u.UNIT_KEY = pr.UNIT_KEY JOIN 
               [LASAR].dbo.STATION sn on
               sn.STATION_KEY = s.STATION_KEY JOIN 
               [LASAR].dbo.SAMPLING_SUBPROJECT ss on 
               ss.SAMPLING_SUBPROJECT_KEY = s.SAMPLING_SUBPROJECT_KEY JOIN
               [LASAR].[dbo].[SAMPLE_MATRIX] sm on 
               sm.SAMPLE_MATRIX_KEY = pr.SAMPLE_MATRIX_KEY
              WHERE s.SAMPLE_DATE > '2000-01-01 00:00:00.000' and 
               s.SAMPLE_DATE < '2011-12-31 00:00:00.000' and
               s.STATION_KEY != '10000' and 
               st.STATUS in ('A','A+','B') and 
               sm.SAMPLE_MATRIX in ('Surface water', 'Bay/Estuary/Ocean', 'Canal', 'Reservoir', 'Lake',
               'Ditch/Pond/Culvert/Drain') and
               p.Name in (", parameters, ") 
              Order by s.STATION_KEY, s.SAMPLE_DATE;")
#Here we pass the query to the SQL database
lasar.pr <- sqlQuery(con, query)

#The result column is remarkably messy and the following steps work on cleaning it up
#first we resolve leading and trailing whitespace so we have less unique cases to handle
lasar$Result <- str_trim(lasar$Result)
#This identifies non-numeric values in the Result column
report <- get.cases(lasar$Result)

#This creates the sub column for the sub.cases function to operate from. It takes the Case from the report and separates 
#the items based on spaces. The first line here makes anything with a '<' equivalent to the value attached to it. The second line
#then just makes everything numeric. This assumes that there isn't a space between the < and the number. If there is, then the Sub will
#will be set to 0. That's what the following lines do with individual cases that weren't handled well with this loop. Further, the embedded
#ifelse in the report$Sub portion of the loop will only return ND when the first line isn't run. That is the first line removes the < from
#all the cases so the second line will never pick that up to return ND. This should probably turned into a function with the option of making
#a numeric Result_clean with MRLs for subs when reported with a < or returning a character Result_clean with NDs for subs when reported with a <.
#This really depends on the analysis you intend to do. For the Integrated report we want values in the result column so we have a value to compare
#to the criteria and we determine validity of the comparison using the MRL. This works fine when the MRL matches the value reported next to the
#< symbol but the custom with the old lab lims was that when the sample precluded the attainment of the method MRL, the method MRL would be 
#included with the report and the actual MRL would be placed in the Result column with a < symbol. Since we preserve the original result 
#we are able to track if the sample is a detect or non-detect and handle accordingly from that but it would be nice if we put it into this process
#in a cleaner manner.
lst.split <- strsplit(as.character(report$Case), split = ' ')
for (i in 1:length(lst.split)){
  lst.split[[i]][1] <- ifelse(substr(lst.split[[i]][1],1,1) == '<',substr(lst.split[[i]][1],2,nchar(lst.split[[i]][1])),lst.split[[i]][1])
  report$Sub[i] <- ifelse(is.na(as.numeric(str_trim(lst.split[[i]][1]))), 
                          ifelse(substr(str_trim(lst.split[[i]][1]),1,1) == '<','ND',NA),
                          as.numeric(lst.split[[i]][1]))
}
#These are the cases that weren't handled well from the processing loop above.
report[report$Case == '< 0.5','Sub'] <- 0.5
report[report$Case == '< 10 Est','Sub'] <- 10
report[report$Case == '< 10','Sub'] <- 10
report[report$Case == '<0.001est','Sub'] <- 0.001
report[report$Case == '<0.003est','Sub'] <- 0.003
report[report$Case == '<0.002est','Sub'] <- 0.002
report[report$Case == '<0.004est','Sub'] <- 0.004
report[report$Case == '>0.6','Sub'] <- 0.6  
report[report$Case == '>1','Sub'] <- 1
report[report$Case == '<1..5','Sub'] <- 1.5
report[report$Case == '< 6000','Sub'] <- 6000
report[report$Case == '< 19','Sub'] <- 19
report[report$Case == '< 18','Sub'] <- 18
report[report$Case == '< 19est','Sub'] <- 19
report[report$Case == '< 20','Sub'] <- 20
report[report$Case == '<0.005(LOD)','Sub'] <- 0.005
report[report$Case == '0.006J','Sub'] <- 0.006
report[report$Case == '<20est','Sub'] <- 20
report[report$Case == '<40est','Sub'] <- 40
report[report$Case == '<80est','Sub'] <- 80
#Here we use the sub.cases function to create the Result_clean column
Result_clean <- sub.cases(lasar$Result, report) 
lasar <- cbind(lasar, Result_clean)
#This removes all the cancelled and voided samples
lasar <- lasar[!is.na(lasar$Result_clean),]

#This pulls out stations to be pushed to station locate efforts
lasar.stations <- unique(lasar[,c('STATION_KEY','LOCATION_DESCRIPTION')])

#put criteria names in lasar df
lasar$criteria.name <- mapvalues(lasar$NAME, from = lasar.names.match$lasar.name, to  = lasar.names.match$Pollutant)

# Here is code to save the lasar dataset into the WQAssessment database so it is available for future processing
# wq <- odbcConnect('WQAssessment')
# lasar.to.save <- lasar
# lasar.to.save$SAMPLE_DATE <- as.character(lasar.to.save$SAMPLE_DATE)
# lasar.to.save$SAMPLE_TIME <- substr(as.character(lasar.to.save$SAMPLE_TIME),12,19)
# sqlSave(wq, lasar.to.save, tablename = 'LASAR_Toxics_Query_wCriteriaNames_06132014', rownames = FALSE)
# rm(lasar.to.save)
