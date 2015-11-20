library(stringr)
library(plyr)
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
      if (tmp.cases.report$Case[ii] == "$$$") tmp.cases.report$Case[ii] <- "\\$$$"
      tmp.cases.report$Count[ii] <- length(grep(tmp.cases.report$Case[ii],chk.values))
      if (tmp.cases.report$Case[ii] == "\\$$$") tmp.cases.report$Case[ii] <- "$$$"
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
    #print(paste("Number of sub for ", sub.table$Case[ii], " is ",sub.table$Sub[ii],sep=""))
    if(length(sub.index)> 0){
      data.in[data.in == sub.table$Case[ii]] <- as.character(sub.table$Sub[ii])
      rm(sub.index)
    }
  }
  return(data.in)
}

clean <- function(result) {
  result <- str_trim(result)
  report <- get.cases(result)
  
  lst.split <- strsplit(as.character(report$Case), split = ' ')
  for (i in 1:length(lst.split)){
    lst.split[[i]][1] <- str_trim(gsub('[Ee]st|>','',lst.split[[i]][1]))
    lst.split[[i]][1] <- ifelse(substr(lst.split[[i]][1],1,1) == '<',substr(lst.split[[i]][1],2,nchar(lst.split[[i]][1])),lst.split[[i]][1])
    report$Sub[i] <- suppressWarnings(ifelse(is.na(as.numeric(str_trim(lst.split[[i]][1]))), 
                            ifelse(substr(str_trim(lst.split[[i]][1]),1,1) == '<' | str_trim(lst.split[[i]][1]) == 'ND','ND',NA),
                            as.numeric(lst.split[[i]][1])))
  }
  
  Result_clean <- sub.cases(result, report) #just use the report object created in step 02_LASAR_clean.R
  attr(Result_clean, "report") <- report
  #mydata <- cbind(mydata, Result_clean)
  return(Result_clean)
}


evaluate <- function(df, result, parm.name) {
  if (any(df[,parm.name] == 'pH')) {
    df[df[,parm.name] == 'pH','digress'] <- ifelse(df[df[,parm.name] == 'pH',result] < 6.5 | df[df[,parm.name] == 'pH',result] > 8.5, 1, 0)
  } 
  return(df[,'digress'])
}

plot.points <- function(station, parm, SeaKen.table){ ####create a function called "plot.points"
 tmp.data <- df.all[df.all$Station_ID == station & df.all$Analyte == parm,]
 if(length(tmp.data$Result)>0) {  #### in order to proceed, the subset must have more than 0 data points
   tmp.data$Sampled <- as.POSIXct(strptime(tmp.data$Sampled, format = '%Y-%m-%d'))  
   x.min <- min(tmp.data$Sampled) #min of subset date
    x.max <- max(tmp.data$Sampled) #max of subset date
    x.lim <- c(x.min, x.max) ####define the data domain for graph
    y.min <- if(floor(min(df.all$Result))<=0 ){ #min of data for graph& log.scale=="y"
      1 #set minimum y value for log scale to one
    }else{
      floor(min(df.all$Result))
    }
    y.max <- ceiling(max(df.all$Result)) #max of data for graph
    y.lim <- c(y.min,y.max) ####define the data range
    #river.mile <- spawning.period.table[spawning.period.table$STATION == station,'RM']
    title <- paste0(min(tmp.data$Station_Description), ", ID = ", min(tmp.data$Station_ID)) #, " , river mile = ",river.mile
    x.lab <- "month"
    y.lab <- parm#paste0(parameter.subset.name, " _",am.or.pm, "_")
    ####definitions for drawing Seasonal Kendall slope line
    y.median <- median(tmp.data$Result)
    slope <- as.numeric(SeaKen.table[SeaKen.table$Station_ID == station,'slope'] )
    p.value <- as.numeric(SeaKen.table[SeaKen.table$Station_ID == station,'pvalue'] )
    p.value.label <- SeaKen.table[SeaKen.table$Station_ID == station,'signif'] 
    x.delta <- as.numeric((x.max-x.min)/2)####average date
    SK.min <- y.median-x.delta*slope/365.25#minimum y value for line
    SK.max <- y.median+x.delta*slope/365.25#maximum y value for line
    sub.text <- paste0("p value = " ,round(p.value, digits=3),", ",  p.value.label, ", slope = ", round(slope, digits=2), ", n = ", nrow(tmp.data))
    ####definitions for plotting numeric criteria by date from spawning.period.table.
    #date.spawn.Start <- spawning.period.table[spawning.period.table$STATION == station,'spwnStart']
    #date.spawn.End <- spawning.period.table[spawning.period.table$STATION == station,'spwnEnd']
    
    #numeric.spawn <- spawning.period.table[spawning.period.table$STATION == station,'DO.criterion.spawn']
    #numeric.nonspawn <- spawning.period.table[spawning.period.table$STATION == station,'DO.criterion.nonspawn']
    
    ####plot the timeseries
    #file.name.ts <- paste0(station,"_timeseries",parameter.graph.name,".png")
    #png(filename=file.name.ts ,width = 700, height = 400) ####create a .png with the station name in the filepath specified above
    par(xpd=NA,oma=c(0,0,4,0), mar=c(5.1,4.1,3.1,2.1)) 
    plot(tmp.data$Sampled, tmp.data$Result, xlim=x.lim, ylim=y.lim, xlab="", ylab=y.lab, bty="L") ####plot the points , log=log.scale  
    title(main=title, cex.main=1.2, outer=TRUE)
    mtext(text=sub.text, side=3,cex=1.0, outer=TRUE)
    exceeds.points <- tmp.data[tmp.data$digress == 1,]
    points(exceeds.points$Sampled, exceeds.points$Result, col="red", pch=20) ####plot the exceedances
    if(p.value.label !="Not Significant"){
      lines(x=c(x.min, x.max), y=c(SK.min, SK.max), col="red", lwd=2)#draw Seasonal Kendall slope line using median concentration at average date
    }
    lines(x=c(x.min, x.max), y=c(6.5, 6.5), lty=2)#draw WQS 
    lines(x=c(x.min, x.max), y=c(8.5, 8.5), lty=3)#draw WQS 
    legend(x=par("usr")[1],y=par("usr")[3], legend=c("Maximum criterion", "Minimum criterion", "Seasonal Kendall trend"), 
           lty=c(2,3,1), col=c("black","black","red"), lwd=c(1,1,2), xjust=-0.01, yjust=-8., box.lty=0, cex=1.0, horiz=TRUE)
    # dev.copy(png,paste0(outpath.plot.points, station, parameter.graph.name, "timeseries.png"),width = 700, height = 400) ####create a .png with the station name in the filepath specified above
    # dev.off() ####write the .png
  } else {
    "No data available to plot"
  }
}


Calculate.sdadm <- function(spawning, station, use, df.all, dates) {
  #
  if (!is.character(spawning)) {
    return("Select a spawning date")
  }
  spd <- as.character(spawning) #input$selectSpawning
  if (spd == 'No spawning') {
    SSTART_MONTH <- NA
    SSTART_DAY <- NA
    SEND_MONTH <- NA
    SEND_DAY <- NA
  } else {
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
  }
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
                        'No Salmonid Use/Out of State' = NA)
  if (spd == "No spawning") {
    rm(spd,SSTART_MONTH,
       SSTART_DAY,SEND_MONTH,SEND_DAY)
  } else {
    rm(spd,spd_list,spd_chron,spd_months,spd_days,
       spd_months_num,spd_days_num,SSTART_MONTH,
       SSTART_DAY,SEND_MONTH,SEND_DAY)
  }
  
      tdata <- df.all[df.all$Station_ID == unique(strsplit(station,' - ')[[1]][1]) & #input$selectStation
                        df.all$Analyte == 'Temperature' & 
                        as.POSIXct(df.all$Sampled) >= as.POSIXct(strptime(dates[1], format = "%Y-%m-%d")) &
                        as.POSIXct(df.all$Sampled) <= as.POSIXct(strptime(dates[2], format = "%Y-%m-%d"))
                      ,c('Station_ID','Station_Description',
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
  #tdata$date <- as.Date(tdata$datetime, format="%m/%d/%Y")
  tdata$date <- as.POSIXct(strptime(tdata$datetime, format = "%Y-%m-%d"))
  
  #inherits(tdata$date, "Date") # checks to see if it is class Date
  
  #############################
  # tdata COLUMN NAMES
  # tdata[1] <- "id"
  # tdata[2] <- "name"
  # tdata[3] <- "datetime"
  # tdata[4] <- "unit"
  # tdata[5] <- "t"
  # tdata[6] <- "t_c"
  # tdata[7] <- "date"
  #############################
  
  ####################################################################################################
  # This section inserts a dummy station "-99" into tdata with a sequence of -99s and NAs for the associated variables.
  # The -99 timeseries starts from the oldest date in the entire dataset and ends at the most recent date.
  # The purpose for the dummy data is to create a continous daily timeseries so 7DADM are not calculated
  # between breaks in days for the same station.
  
  datetime99<-as.character(seq(dates[1],dates[2],by=1))
  date99<- as.POSIXct(strptime(seq(dates[1],dates[2],by=1), format = "%Y-%m-%d"))
  id99<-rep(unique(tdata$id),by=0,length.out=length(datetime99))
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
  
  dummy$t_c <- as.numeric(dummy$t_c)
  dummy$t <- as.numeric(dummy$t)
  
  inherits(dummy$date, "Date") # should be FALSE
  is.numeric(dummy$t_c) # should be TRUE
  
  tdata <-rbind(tdata,dummy)
  rm(dummy)
  #############################################################################################
  
  ## Calculate daily maximums by station
  tmax<- tapply(tdata$t_c,list(tdata$date,tdata$id),function(x) {ifelse(all(is.na(x)),NA,max(x, na.rm = TRUE))})
  
  ## Calculate 7DADM
  if (length(tmax) < 7) {
    return("Insufficient data to calculate a single 7DADM")
  } else {
    sdadm<- rollapply(tmax,7,mean, fill=NA, align="right")
    sdadm<- round(sdadm,1)
  }

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
    #names(sdadm.melt)[names(sdadm.melt) == 'date'] <- 'Sampled'
    attr(sdadm.melt, "sdata") <- sdata
    ## finds the current date, and spawning start/end date and formats as a numeric in the form mm.dd
    sdadm.melt$cdate <- as.numeric(months(sdadm.melt$date)) + (as.numeric(days(sdadm.melt$date)) * .01)
    sdadm.melt$sstr <- as.numeric(sdata$SSTART_MONTH[1]) + (as.numeric(sdata$SSTART_DAY[1]) *.01)
    sdadm.melt$send <- as.numeric(sdata$SEND_MONTH[1]) + (as.numeric(sdata$SEND_DAY[1]) *.01)
    sdadm.melt$bioc <- as.numeric(sdata$ZDADM[1])
    
    ## checks to see if there is an over winter spawning period
    sdadm.melt$winter <- ifelse(sdadm.melt$send < sdadm.melt$sstr, TRUE, FALSE)
    
    ## looks up the summer bio criterion and spawning start end/date and returns TRUE/FALSE if current date is in summer or spawning period
    sdadm.melt$bioc <- ifelse(is.na(sdadm.melt$winter), sdadm.melt$bioc, ifelse(
                              sdadm.melt$winter == TRUE,
                              ifelse(sdadm.melt$sstr <= sdadm.melt$cdate | sdadm.melt$send >= sdadm.melt$cdate, 13, sdadm.melt$bioc),
                              ifelse(sdadm.melt$sstr <= sdadm.melt$cdate & sdadm.melt$send >= sdadm.melt$cdate, 13, sdadm.melt$bioc)))
    
    sdadm.melt$summer <- ifelse(sdadm.melt$bioc == 13, FALSE, TRUE)
    sdadm.melt$spawn <- ifelse(sdadm.melt$bioc == 13, TRUE, FALSE)
    
    ## Calculate total 7DADM obersvations and # of 7DADM observations that exceed the summer spawning critera in those time periods; and 
    ## number of 7DADM observations that exceed 16 and 18 over the whole time period (not just in the stated periods)
    sdadm.melt$exceedsummer <- ifelse(sdadm.melt$sdadm >=sdadm.melt$bioc & sdadm.melt$summer == TRUE, TRUE, FALSE)
    sdadm.melt$exceedspawn <- ifelse(sdadm.melt$sdadm >=sdadm.melt$bioc & sdadm.melt$spawn == TRUE, TRUE, FALSE)
    #sdadm.melt$daystot <-ifelse(sdadm.melt$sdadm !="NA", TRUE, FALSE)
    return(sdadm.melt)
  }
}
#save to a .csv
#write.csv(df.all, 'C:/users/pbryant/desktop/Biomon_LASAR_Query_2014_12_04.csv')
