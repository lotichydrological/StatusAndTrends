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



#save to a .csv
#write.csv(df.all, 'C:/users/pbryant/desktop/Biomon_LASAR_Query_2014_12_04.csv')
