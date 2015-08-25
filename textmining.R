library(doBy)
library(tm)
library(plyr)

options(stringsAsFactors = FALSE)

wqp <- read.csv('//deqhq1/tmdl/agwqm/dataanalysis/northcoast_pilot/north coast_temperature_20150723_0859_wqp.csv')
wqp <- within(wqp, rm(X))
el <- read.csv('//deqhq1/tmdl/agwqm/dataanalysis/northcoast_pilot/north coast_temperature_20150722_1421_Elm_lasar.csv')
all <- rbind(wqp, el)

load(file="//deqhq1/tmdl/agwqm/dataanalysis/statusandtrends/QCdf.Rdata")

str(QCdf)

raw.data <- all

chr.names <- names(raw.data)

unique(raw.data[,chr.names[1]])

unique(raw.data[,chr.names[2]])

unique(raw.data[,chr.names[3]])

unique(raw.data[,chr.names[4]])

unique(raw.data[,chr.names[5]])

unique(raw.data[,chr.names[6]])

unique(raw.data[,chr.names[7]])


## quick function
unique.n <- function(chr.vec) length(unique(chr.vec))
unique.n(raw.data[,chr.names[7]])

## make "OrganizationFormalName" and "CharacteristicName" factors
df.facts <- cbind(data.frame(OrganizationFormalName=factor(x=raw.data[,1],levels=unique(raw.data[,1])),
                       CharacteristicName=factor(x=raw.data[,2],levels=unique(raw.data[,2]))
                       ),
                  raw.data[,3:7],stringsAsFactors=FALSE)
str(df.facts)

## quick summry
df.sum00 <- summaryBy(list(c(chr.names[3:7]),c(chr.names[1:2])),data=df.facts,FUN=unique.n)
df.sum01 <- summaryBy(list(c(chr.names[3:7]),c(chr.names[2])),data=df.facts,FUN=unique.n)
df.sum02 <- summaryBy(list(c(chr.names[3:7]),c(chr.names[1])),data=df.facts,FUN=unique.n)

## term freq
junk <- PlainTextDocument(unique(all[,'Comment']))
more.junk <- termFreq(junk)
str(more.junk)
str(more.junk[1])
attr(more.junk,"dimnames")
df.junk <- data.frame(term=attr(more.junk,"dimnames"),count=more.junk,stringsAsFactors=FALSE)

df.junk <- df.junk[order(df.junk$count, decreasing=TRUE),]


# summary by sampling organization
for (i in 1:length(unique(all$Client))) {
  org.rows <- nrow(all[which(all$Client == unique(all$Client)[i]),])
  org.stations <- length(unique(all[all$Client == unique(all$Client)[i],'Station_ID']))
  org.status.id <- ifelse(all(c("",NA) %in% unique(all[all$Client == unique(all$Client)[i],'StatusIdentifier'])),0,
                          length(unique(all[all$Client == unique(all$Client)[i],'StatusIdentifier'])))
  org.comments <- ifelse(all(c("",NA) %in% unique(all[all$Client == unique(all$Client)[i],'Comment'])),0,
                         length(unique(all[all$Client == unique(all$Client)[i],'Comment'])))
  new.row <- data.frame("Organization" = unique(all$Client)[i],
               "N_Observations" = org.rows,
               "N_Unique_Stations" = org.stations,
               "N_Unique_Status_Identifiers" = org.status.id,
               "N_Unique_Comments" = org.comments)
  ifelse(i == 1, df.summary <- new.row, df.summary <- rbind(df.summary, new.row))
}
arrange(df.summary, desc(N.Observations))
