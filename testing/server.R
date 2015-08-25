#Use this to make it accessible for other people to access
#runApp("app_name",host="0.0.0.0",port=3168)

library(shiny)
library(RCurl)
library(XML)
library(dataRetrieval)
library(plyr)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(plotGoogleMaps)
library(DT)
library(wq)
#library(xlsx)
#library(RODBC)

options(stringsAsFactors = FALSE)

source('data/01_DataQuery.R')
source('data/funClean.R')

agwqma <- readOGR(dsn = './data/GIS', layer = 'ODA_AgWQMA', verbose = FALSE)
#For testing purposes
#agwqma <- readOGR(dsn = 'AgWQMA_DataRetrieval/data/GIS', layer = 'ODA_AgWQMA', verbose = FALSE)

#Transform agwqma to same projection as points
agwqma <- spTransform(agwqma, CRS("+proj=longlat +datum=NAD83"))

#Bring in the HUC list mapped to overlapping ag plan areas
#This process would not produce desired results (only those HUCs wholly contained by the plan area)
#When done through the use of intersection and overlay tools provided in the sp and rgdal packages
#Therefore, a manual identificiation of the desired HUCs for each Ag plan area was completed
HUClist <- read.csv('data/PlanHUC_LU.csv')

#For testing purposes set up input 
# input <- list(action_button = c(0))
# input$action_button <- 1
# input$parms <- c('pH')
# input$select <- 'North Coast'
# input$dates <- c("1990-01-01", "2015-05-21")
# input$db <- c("LASAR")
# input$selectStation <-  "10339 - Willamette River at Canby Ferry"
# input$selectParameter <- 'Temperature'


shinyServer(function(input, output, session) { 
  autoInvalidate <- reactiveTimer(1000, session)
  observe({
    if (input$action_button == 0)
      return()
    isolate({
      if (is.null(input$parms)) {
        output$text1 <- renderText("Please refresh the page and remember to select a parmaeter to query")
      } else {
        withProgress(message = "Processing:", value = 0, {
        
        wL <- FALSE
        lL <- FALSE
        eL <- FALSE
        wqpData <- ""
        lasarData <- ""
        elmData <- ""      
        prog <- 0
        
        if ('Water Quality Portal' %in% input$db) {
          incProgress(1/10, detail = 'Querying the Water Quality Portal')
          prog <- prog + 1/10
          wqpData <- tryCatch(wqpQuery(planArea = input$select,
                                       HUClist = HUClist,
                                       inParms = input$parms,
                                       startDate = input$dates[1],
                                       endDate = input$dates[2]),error = function(err) {err <- geterrmessage()})
          wL <- ifelse(is.data.frame(wqpData),ifelse(nrow(wqpData) > 0,TRUE,FALSE),FALSE)
          if (!is.data.frame(wqpData)) {
            df.all <- 'Water Quality Portal is busy. Please try again in a few minutes.'
          } 
        }
        if ('LASAR' %in% input$db) {
          incProgress(1/10, detail = 'Querying the LASAR database')
          prog <- prog + 1/10
          lasarData <- lasarQuery(planArea = input$select,
                                  HUClist = HUClist,
                                  inParms = input$parms,
                                  startDate = input$dates[1],
                                  endDate = input$dates[2])
          lL <- ifelse(is.data.frame(lasarData),ifelse(nrow(lasarData) > 0,TRUE,FALSE),FALSE) 
          odbcCloseAll()
        }
        if ('Element' %in% input$db) {
          incProgress(1/10, detail = 'Querying the Element database')
          prog <- prog + 1/10
          elmData <- elementQuery(planArea = input$select,
                                  HUClist = HUClist,
                                  inParms = input$parms,
                                  startDate = input$dates[1],
                                  endDate = input$dates[2])
          eL <- ifelse(is.data.frame(elmData),ifelse(nrow(elmData) > 0,TRUE,FALSE),FALSE)
          odbcCloseAll()
        }
        
        incProgress(1/10, detail = 'Combining query results')
        prog <- prog + 1/10
        if(wL) {
          if (lL) {
            if (eL) {
              df.all <- tryCatch(combine(E=elmData,L=lasarData,W=wqpData),error = function(err) {err <- geterrmessage()})
            } else {
              df.all <- tryCatch(combine(L=lasarData,W=wqpData),error = function(err) {err <- geterrmessage()})
            } 
          } else if (eL) {
            df.all <- tryCatch(combine(E=elmData,W=wqpData),error = function(err) {err <- geterrmessage()})
          } else {
            df.all <- tryCatch(combine(W=wqpData),error = function(err) {err <- geterrmessage()})
          }
        } else if (lL) {
          if (eL) {
            df.all <- tryCatch(combine(E=elmData,L=lasarData),error = function(err) {err <- geterrmessage()})
          } else {
            df.all <- tryCatch(combine(L=lasarData),error = function(err) {err <- geterrmessage()})
          }
        } else if (eL) {
          df.all <- tryCatch(combine(E=elmData),error = function(err) {err <- geterrmessage()})
        } else {
          df.all <- 'Your query returned no data'
        }
        
        incProgress(1/10, "Tabulating results")
        prog <- prog + 1/10
        all.sp <- df.all[!duplicated(df.all$SD),c(3,1:2,4:17)]
        coordinates(all.sp) = ~DECIMAL_LONG+DECIMAL_LAT
        proj4string(all.sp) <- CRS("+init=epsg:4269")
        ag_sub <- agwqma[agwqma$PlanName == input$select,]
        #HUC_sub <- HUC[ag_sub,]
        ag_sub <- spTransform(ag_sub, CRS("+init=epsg:4269"))
        all.sp <- all.sp[ag_sub,]
        
        df.all <- df.all[df.all$Station_ID %in% all.sp@data$Station_ID,]
        
        all.totals <- ddply(df.all, .(Database), summarize, n_stations = length(unique(Station_ID)))
        n_samp <- as.data.frame.matrix(table(df.all$Database, df.all$Analyte))
        n_samp$Database <- row.names(n_samp)
        all.totals <- merge(all.totals, n_samp, by = 'Database')
        
        attr(df.all, "totals") <- all.totals
        
        incProgress(1 - prog, "Cleaning result field")
        df.all$Result <- clean(df.all$Result)
        df.report <- attr(df.all$Result, 'report')
        df.all[which(df.all$Result == 'ND'),'Detect'] <- 0 
        df.all[which(df.all$Result == 'ND'),'Result'] <- df.all[which(df.all$Result == 'ND'),'MRL']
        
        
        df.ex <- df.all[df.all$Status %in% c('D','E','F'),]
        df.all <- df.all[!df.all$Status %in% c('D','E','F'),]
        if (nrow(df.ex) > 0) {
          df.ex$Reason <- "Sample Status equivalent to D, E or F"
        }
        
        df.all$MRL <- as.numeric(df.all$MRL)
        
        df.all[is.na(df.all$MRL),'MRL'] <- 0
        
        df.all$Detect <- ifelse(df.all$Result < df.all$MRL,0,1)
        df.all[which(df.all$Result < df.all$MRL),'Result'] <- df.all[which(df.all$Result < df.all$MRL),'MRL']
        
        df.ex2 <- df.all[grep('Qualifier=C',df.all$Comment),]
        df.all <- df.all[!grepl('Qualifier=C | [Cc]ontamination | QCS FAILED',df.all$Comment),]
        if (nrow(df.ex2) > 0) {
          df.ex2$Reason <- "Comment indicates sample contamination"
          df.ex <- rbind(df.ex, df.ex2)
          rm(df.ex2)
        }
        
        df.ex2 <- df.all[grep('Qualifier=Q',df.all$Comment),]
        df.all <- df.all[!grepl('Qualifier=Q',df.all$Comment),]
        if (nrow(df.ex2) > 0) {
          df.ex2$Reason <- "Comment indicates major QC issue"
          df.ex <- rbind(df.ex, df.ex2)
          rm(df.ex2)
        }
        
        df.ex2 <- df.all[is.na(df.all$Result),]
        df.all <- df.all[!is.na(df.all$Result),]
        if (nrow(df.ex2) > 0) {
          df.ex2$Reason <- "Result is Void, Cancelled or other NA"
          df.ex <- rbind(df.ex, df.ex2)
          rm(df.ex2)
        }
        df.all$Result <- as.numeric(df.all$Result)
        
        for (i in 1:length(unique(df.all$Client))) {
          org.rows <- nrow(df.all[which(df.all$Client == unique(df.all$Client)[i]),])
          org.na.rows <- length(df.all[which(df.all$Client == unique(df.all$Client)[i]),
                                       'Result'][is.na(df.all[which(df.all$Client == unique(df.all$Client)[i]),'Result'])])
          org.stations <- length(unique(df.all[df.all$Client == unique(df.all$Client)[i],'Station_ID']))
          org.comments <- ifelse(all(c("",NA) %in% unique(df.all[df.all$Client == unique(df.all$Client)[i],'Comment'])),0,
                                 length(unique(df.all[df.all$Client == unique(df.all$Client)[i],'Comment'])))
          new.row <- data.frame("Organization" = unique(df.all$Client)[i],
                                "Observations" = org.rows,
                                "'Unique Stations'" = org.stations,
                                "'NA obs'" = org.na.rows,
                                "'Unique Comments'" = org.comments,
                                check.names = FALSE)
          ifelse(i == 1, df.summary <- new.row, df.summary <- rbind(df.summary, new.row))
          df.summary <- arrange(df.summary,desc(Observations))
        }
        
        df.all$digress <- evaluate(df.all, "Result", "Analyte")
        
        SeaKen <- data.frame(Station_ID=sort(unique(df.all$Station_ID)),analyte="none",slope="none",pvalue="none",median="none",N="none",stringsAsFactors=FALSE)
        for (p in 1:length(unique(df.all$Analyte))) {
          parm <- unique(df.all$Analyte)[p]
          
          for(ii in 1:length(SeaKen$Station_ID)) { 
            # specifiy current Station_ID
            tmp.one.station <- SeaKen$Station_ID[ii]
            tmp.data.raw <- df.all[df.all$Station_ID == tmp.one.station & df.all$Analyte == parm,]
            SeaKen$analyte[ii] <- parm
            SeaKen$N[ii] <- length(tmp.data.raw$Result)
            if (!nrow(tmp.data.raw) > 1) next
            # Reshape and manipulate data to convert to wqData-class
            tmp.data <- data.frame(date=tmp.data.raw$Sampled,
                                   time="0000",
                                   stn=as.character(tmp.one.station),
                                   depth=1,
                                   variable=parm,
                                   value=as.numeric(tmp.data.raw$Result), 
                                   stringsAsFactors=FALSE)
            
            # Construct an object of class "WqData"
            tmp.wq <- wqData(tmp.data, c(1,3,4), c(5,6), site.order = TRUE, type = "long",time.format = "%Y-%m-%d %H:%M:%S")
            # Create time series from water quality data
            tmp.ts <- tsMake(tmp.wq, focus = parm, layer = c(0, 5)) 
            if (!length(tmp.ts) > 2) next
            if (start(tmp.ts)[1] == end(tmp.ts)[1]) next
            tmp.result <- seaKen(tmp.ts)
            SeaKen$pvalue[ii] <- tmp.result$p.value
            SeaKen$slope[ii] <- tmp.result$sen.slope
            SeaKen$median[ii] <- median(as.numeric(tmp.data.raw$Result),na.rm = FALSE)
            
            rm(list=ls(pattern="tmp.*"))
          }
        }
        
        SeaKen$signif <- ifelse(SeaKen$pvalue<=0.01, "99% Significance Level",
                                ifelse(SeaKen$pvalue<=0.05, "95% Significance Level",
                                       ifelse(SeaKen$pvalue<=0.1, "90% Significance Level",
                                              ifelse(SeaKen$pvalue<=0.2, "80% Significance Level","Not Significant"))))
        }
        )
        

        
        if(!is.data.frame(df.all)) {
          output$text1 <- renderText({df.all})
        }
        else {
          output$text1 <- renderText({
            paste("You just submitted", 
                  input$select, 
                  "Plan Area Query for", 
                  paste(input$parms,collapse=", "), 
                  "from",
                  input$dates[1], 
                  "to", 
                  input$dates[2])
          })
          
          output$isdf <- renderText({
            ifelse(is.data.frame(df.all),"Results returned",0)
          })
          
          output$view <- renderTable({
            attr(df.all, 'totals')
          })
          
          output$downloadData <- downloadHandler(
            filename = function() {
              paste(input$select, "_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv",sep='')
            },
            content = function(file) {
              write.csv(df.all, file, row.names = FALSE)
            }
          )
          
          output$mymap <- renderUI({
            withProgress(message = "Processing:", value = 0, {
              incProgress(1/3, detail = 'Plotting stations')
              prog <- 1/3
            m <- plotGoogleMaps(all.sp, add = TRUE, filename = 'myMap1.html', openMap = FALSE, legend = FALSE, layerName = "Sampling stations", mapTypeId = "ROADMAP")
              incProgress(1/3, detail = "Plotting Ag Area")
              prog <- 2/3
            m <- plotGoogleMaps(ag_sub, previousMap = m, filename = "myMap2.html", 
                                openMap = FALSE, layerName = "Ag Plan Areas", legend = FALSE)
              incProgress(1 - prog, detail = "Rendering plot")
            tags$iframe(
              srcdoc = paste(readLines('myMap2.html'), collapse = '\n'),
              width = "100%",
              height = "600px"
            )
            })
          })
        }
        }
      })
  
      output$display <- DT::renderDataTable({switch(input$ReviewDf,
                                                "df.summary" = (
                                                    df.summary
                                                ),
                                                "df.cleaned" = (
                                                  df.report
                                                ),
                                                "df.Comment" = (
                                                    out <- as.data.frame(table(df.all[,'Comment'],useNA='always'))
                                                ),
                                                "df.removal" = (
                                                  if(nrow(df.ex) > 0) {
                                                      out <- rename(as.data.frame(table(df.ex[,'Reason'])), 
                                                                    c('Var1' = "'Reason for removal'", 'Freq' = "'Number of observations removed'"))
                                                  }
                                                  else {
                                                    out <- data.frame("Message" = "All data met QC requirments")
                                                  }),
                                                "df.sub" = (
                                                    df.all
                                                )
      )},server = TRUE)

      updateSelectInput(session, "selectStation", choices = unique(paste(df.all$Station_ID, df.all$Station_Description, sep = ' - ')))
      output$selectParameter = renderUI({mydata <- unique(df.all[df.all$Station_ID == unique(strsplit(input$selectStation,' - ')[[1]][1]),'Analyte'])
                                         selectInput('selectParameter','Select parameter to evaluate:',mydata)})

      #Update the data to be used for plotting
      DataUse <- reactive(df.all[df.all$Station_ID == unique(strsplit(input$selectStation,' - ')[[1]][1]) &
                          df.all$Analyte == input$selectParameter,])
      
      output$datatable <- renderPlot({new_data <- DataUse()
                                      new_data$Sampled <- as.POSIXct(strptime(new_data$Sampled, format = '%Y-%m-%d'))  
                                      x.min <- min(new_data$Sampled) #min of subset date
                                      x.max <- max(new_data$Sampled) #max of subset date
                                      x.lim <- c(x.min, x.max) ####define the data domain for graph
                                      y.min <- if(floor(min(df.all$Result))<=0 ){ #min of data for graph& log.scale=="y"
                                        1 #set minimum y value for log scale to one
                                      }else{
                                        floor(min(df.all$Result))
                                      }
                                      y.max <- ceiling(max(df.all$Result)) #max of data for graph
                                      y.lim <- c(y.min,y.max) ####define the data range
                                      #river.mile <- spawning.period.table[spawning.period.table$STATION == station,'RM']
                                      title <- paste0(min(new_data$Station_Description), ", ID = ", min(new_data$Station_ID)) #, " , river mile = ",river.mile
                                      x.lab <- "month"
                                      y.lab <- parm#paste0(parameter.subset.name, " _",am.or.pm, "_")
                                      ####definitions for drawing Seasonal Kendall slope line
                                      y.median <- median(new_data$Result)
                                      slope <- as.numeric(SeaKen[SeaKen$Station_ID == unique(new_data$Station_ID),'slope'] )
                                      p.value <- as.numeric(SeaKen[SeaKen$Station_ID == unique(new_data$Station_ID),'pvalue'] )
                                      p.value.label <- SeaKen[SeaKen$Station_ID == unique(new_data$Station_ID),'signif'] 
                                      x.delta <- as.numeric((x.max-x.min)/2)####average date
                                      SK.min <- y.median-x.delta*slope/365.25#minimum y value for line
                                      SK.max <- y.median+x.delta*slope/365.25#maximum y value for line
                                      sub.text <- paste0("p value = " ,round(p.value, digits=3),", ",  
                                                         p.value.label, ", slope = ", round(slope, digits=2), 
                                                         ", n = ", nrow(new_data))
                                      ####definitions for plotting numeric criteria by date from spawning.period.table.
                                      #date.spawn.Start <- spawning.period.table[spawning.period.table$STATION == station,'spwnStart']
                                      #date.spawn.End <- spawning.period.table[spawning.period.table$STATION == station,'spwnEnd']
                                      
                                      #numeric.spawn <- spawning.period.table[spawning.period.table$STATION == station,'DO.criterion.spawn']
                                      #numeric.nonspawn <- spawning.period.table[spawning.period.table$STATION == station,'DO.criterion.nonspawn']
                                      
                                      ####plot the timeseries
                                      #file.name.ts <- paste0(station,"_timeseries",parameter.graph.name,".png")
                                      #png(filename=file.name.ts ,width = 700, height = 400) ####create a .png with the station name in the filepath specified above
                                      par(xpd=NA,oma=c(0,0,4,0), mar=c(5.1,4.1,3.1,2.1)) 
                                      plot(new_data$Sampled, new_data$Result, xlim=x.lim, ylim=y.lim, xlab="", ylab=y.lab, bty="L") ####plot the points , log=log.scale  
                                      title(main=title, cex.main=1.2, outer=TRUE)
                                      mtext(text=sub.text, side=3,cex=1.0, outer=TRUE)
                                      exceeds.points <- new_data[new_data$digress == 1,]
                                      points(exceeds.points$Sampled, exceeds.points$Result, col="red", pch=20) ####plot the exceedances
                                      if(p.value.label !="Not Significant"){
                                        lines(x=c(x.min, x.max), y=c(SK.min, SK.max), col="red", lwd=2)#draw Seasonal Kendall slope line using median concentration at average date
                                      }
                                      lines(x=c(x.min, x.max), y=c(6.5, 6.5), lty=2)#draw WQS 
                                      lines(x=c(x.min, x.max), y=c(8.5, 8.5), lty=3)#draw WQS 
                                      legend(x=par("usr")[1],y=par("usr")[3], legend=c("Maximum criterion", 
                                                                                       "Minimum criterion", 
                                                                                       "Seasonal Kendall trend"), 
                                             lty=c(2,3,1), col=c("black","black","red"), lwd=c(1,1,2), 
                                             xjust=-0.01, yjust=-8., box.lty=0, cex=1.0, horiz=TRUE)
      })
  })
})