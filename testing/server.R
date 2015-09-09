#Use this to make it accessible for other people to access
#runApp("testing",host="0.0.0.0",port=3168)

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
library(chron)
library(reshape)
#library(xlsx)
#library(RODBC)

options(stringsAsFactors = FALSE, warn = -1)

source('data/01_DataQuery.R')
source('data/funClean.R')
source('data/funEvaluateBacteria.R')

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


ph_crit <- read.csv('data/PlanOWRDBasinpH_LU.csv')
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
      } else if (input$dates[1] == input$dates[2]) {
        output$text1 <- renderText("Please refresh the page and select non-identical dates")
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
        
        df.all$MRL <- suppressWarnings(as.numeric(df.all$MRL))
        
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
        df.all$Result <- suppressWarnings(as.numeric(df.all$Result))
        
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
        
        sea_ken_int <- data.frame(Station_ID=sort(unique(df.all$Station_ID)),analyte="none",slope="none",pvalue="none",median="none",N="none",stringsAsFactors=FALSE)
        for (p in 1:length(unique(df.all$Analyte))) {
          parm <- unique(df.all$Analyte)[p]
          
          for(ii in 1:length(sea_ken_int$Station_ID)) { 
            # specifiy current Station_ID
            tmp.one.station <- sea_ken_int$Station_ID[ii]
            tmp.data.raw <- df.all[df.all$Station_ID == tmp.one.station & df.all$Analyte == parm,]
            sea_ken_int$analyte[ii] <- parm
            sea_ken_int$N[ii] <- length(tmp.data.raw$Result)
            if (!nrow(tmp.data.raw) > 1) next
            # Reshape and manipulate data to convert to wqData-class
            tmp.data <- data.frame(date=tmp.data.raw$Sampled,
                                   time="0000",
                                   stn=as.character(tmp.one.station),
                                   depth=1,
                                   variable=parm,
                                   value=suppressWarnings(as.numeric(tmp.data.raw$Result)), 
                                   stringsAsFactors=FALSE)
            
            # Construct an object of class "WqData"
            tmp.wq <- wqData(tmp.data, c(1,3,4), c(5,6), site.order = TRUE, type = "long",time.format = "%Y-%m-%d %H:%M:%S")
            # Create time series from water quality data
            tmp.ts <- suppressWarnings(tsMake(tmp.wq, focus = parm, layer = c(0, 5)) )
            if (!length(tmp.ts) > 2 |
                start(tmp.ts)[1] == end(tmp.ts)[1] | 
                !any(1:frequency(tmp.ts) %in% cycle(tmp.ts)) |
                !all(1:12 %in% cycle(tmp.ts))) next
            tmp.result <- seaKen(tmp.ts)
            sea_ken_int$pvalue[ii] <- tmp.result$p.value
            sea_ken_int$slope[ii] <- tmp.result$sen.slope
            sea_ken_int$median[ii] <- suppressWarnings(median(as.numeric(tmp.data.raw$Result),na.rm = FALSE))
            
            rm(list=ls(pattern="tmp.*"))
          }
          
          ifelse(p == 1, SeaKen <- sea_ken_int, SeaKen <- rbind(SeaKen, sea_ken_int))
        }
        
        SeaKen$signif <- ifelse(SeaKen$pvalue<=0.01, "99% Significance Level",
                                ifelse(SeaKen$pvalue<=0.05, "95% Significance Level",
                                       ifelse(SeaKen$pvalue<=0.1, "90% Significance Level",
                                              ifelse(SeaKen$pvalue<=0.2, "80% Significance Level","Not Significant"))))
        
        df.date <- data.frame('Sampled' = seq(as.POSIXct(strptime(input$dates[1], format = '%Y-%m-%d')),
                                              as.POSIXct(strptime(input$dates[2], format = '%Y-%m-%d')),
                                              by = 60*60*24))
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
      
      output$review_control <- renderUI({
        validate(
          need(is.data.frame(df.all), message = FALSE)
        )
        selectInput(inputId =  "ReviewDf", label = 'Select Review table to view:',
                    choices = list("Summary by organization" = 'df.summary',
                                   "Result values modified" = "df.cleaned",
                                   "Data removal information" = "df.removal",
                                   "Unique comment values" = 'df.Comment',
                                   "Parameter results by station" = 'df.station.results',
                                   "Data in tabular format" = 'df.sub'),
                    selectize = TRUE
        )
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
                                                "df.station.results" = (
                                                  as.data.frame.matrix(table(df.all$Station_ID, df.all$Analyte))
                                                ),
                                                "df.sub" = (
                                                    df.all
                                                )
      )},filter = 'top',server = TRUE)
      
      output$selectStation = renderUI({
        validate(
          need(is.data.frame(df.all), message = FALSE)
        )
        selectInput("selectStation","Select station to evaluate:",
                    choices = unique(paste(df.all$Station_ID, 
                                           df.all$Station_Description, sep = ' - ')),
                    selectize = TRUE)
      })
      #updateSelectInput(session, "selectStation", choices = unique(paste(df.all$Station_ID, df.all$Station_Description, sep = ' - ')))
      output$selectParameter = renderUI({mydata <- unique(df.all[df.all$Station_ID == unique(strsplit(input$selectStation,' - ')[[1]][1]),'Analyte'])
                                         selectInput('selectParameter','Select parameter to evaluate:',mydata)})
      
      output$selectLogScale = renderUI({
        validate(
          need(input$selectParameter == 'E. Coli',message = FALSE)
        )
        checkboxInput("selectLogScale", "Plot data with log scale")
      })
      
      output$selectpHCrit = renderUI({
        validate(
          need(input$selectParameter == 'pH',message = FALSE)
        )
        ph_crit_choices <- paste(ph_crit[ph_crit$plan_name == input$select,c('OWRD_basin')],
                                 ph_crit[ph_crit$plan_name == input$select,c('ph_standard')],
                                 sep = " - ")
        selectInput('selectpHCrit',"Select applicable OWRD Basin specific pH criteria:",
                    choices = ph_crit_choices,
                    selectize = TRUE)
      })
      
      output$selectSpawning = renderUI({
        validate(
          need(input$selectParameter == 'Temperature', message = FALSE)
        )
        selectInput('selectSpawning',"Select applicable spawning time period:",
                    choices = c('No spawning',
                                'January 1-June 15',
                                'January 1-May 15',
                                'August 1-June 15',
                                'August 15-June 15',
                                'August 15-May 15',
                                'September 1-June 15',
                                'September 1-May 15',
                                'September 15-June 15',
                                'September 15-May 15',
                                'October 1-June 15',
                                'October 1-May 15',
                                'October 15-June 15',
                                'October 15-May 15',
                                'October 23-April 15',
                                'November 1-June 15',
                                'November 1-May 1',
                                'November 1-May 15'),
                    selectize = TRUE)
      })
      
      output$selectUse = renderUI({
        validate(
          need(input$selectParameter == 'Temperature', message = FALSE)
        )
        selectInput('selectUse',"Select applicable beneficial fish use:",
                    choices = c('Bull Trout Spawning and Juvenile Rearing',
                                'Core Cold Water Habitat',
                                'Salmon and Trout Rearing and Migration',
                                'Salmon and Steelhead Migration Corridors',
                                'Redband and Lanhontan Cutthroat Trout',
                                'Cool water species',
                                'No Salmonid Use/Out of State'),
                    selectize = TRUE)
      })
      
      output$selectRange <- renderUI({
        sliderInput("selectRange", "Select Date Range for Plot",
                    min = as.Date(strptime(input$dates[1], format = "%Y-%m-%d")),
                    max = as.Date(strptime(input$dates[2], format = "%Y-%m-%d")),
                    value = c(as.Date(strptime(input$dates[1], format = "%Y-%m-%d")), 
                              as.Date(strptime(input$dates[2], format = "%Y-%m-%d")))
        )
      })
      
      #Update the data to be used for plotting
      DataUse <- reactive({switch(input$selectParameter,
                                  "pH" = df.all[df.all$Station_ID == unique(strsplit(input$selectStation,' - ')[[1]][1]) &
                                    df.all$Analyte == input$selectParameter,],
                                  "Temperature" = Calculate.sdadm(spawning = input$selectSpawning,
                                                                  station = input$selectStation,
                                                                  use = input$selectUse,
                                                                  df.all = df.all,
                                                                  dates = input$selectRange),
                                  "E. Coli" = df.all[df.all$Station_ID == unique(strsplit(input$selectStation,' - ')[[1]][1]) &
                                                        df.all$Analyte == input$selectParameter,])
        })
      
        output$ts_plot <- renderPlot({
          switch(input$selectParameter,
               "pH" = ({      new_data <- DataUse()
                 new_data$Sampled <- as.POSIXct(strptime(new_data$Sampled, format = '%Y-%m-%d'))  
                 x.min <- as.POSIXct(strptime(input$selectRange[1], format = '%Y-%m-%d'))#min(new_data$Sampled) #min of subset date
                 x.max <- as.POSIXct(strptime(input$selectRange[2], format = '%Y-%m-%d'))#max(new_data$Sampled) #max of subset date
                 x.lim <- c(x.min, x.max) ####define the data domain for graph
                  y.min <- ifelse(floor(min(new_data$Result))< 4,floor(min(new_data$Result)),4) #{ #min of data for graph& log.scale=="y"
#                    1 #set minimum y value for log scale to one
#                   }else{
#                     4 #floor(min(new_data$Result))
#                   }
                 y.max <- ifelse(ceiling(max(new_data$Result)) > 10,ceiling(max(new_data$Result)),10) #max of data for graph
                 y.lim <- c(y.min,y.max) ####define the data range
                 title <- paste0(min(new_data$Station_Description), ", ID = ", min(new_data$Station_ID)) #, " , river mile = ",river.mile
                 x.lab <- "month"
                 y.lab <- unique(new_data$Analyte)[1]
                 ####definitions for drawing Seasonal Kendall slope line
                 y.median <- median(new_data$Result)
                 slope <- suppressWarnings(as.numeric(SeaKen[SeaKen$Station_ID == unique(new_data$Station_ID),'slope'] ))
                 p.value <- suppressWarnings(as.numeric(SeaKen[SeaKen$Station_ID == unique(new_data$Station_ID),'pvalue'] ))
                 p.value.label <- SeaKen[SeaKen$Station_ID == unique(new_data$Station_ID),'signif'] 
                 x.delta <- as.numeric((x.max-x.min)/2)####average date
                 SK.min <- y.median-x.delta*slope/365.25#minimum y value for line
                 SK.max <- y.median+x.delta*slope/365.25#maximum y value for line
                 sub.text <- paste0("p value = " ,round(p.value, digits=3),", ",  
                                    p.value.label, ", slope = ", round(slope, digits=2), 
                                    ", n = ", nrow(new_data))
                 par(xpd=NA,oma=c(0,0,4,0), mar=c(5.1,4.1,3.1,2.1)) 
                 plot(new_data$Sampled, new_data$Result, xlim=x.lim, ylim=y.lim, xlab="", ylab=y.lab, bty="L") ####plot the points , log=log.scale  
                 title(main=title, cex.main=1.2, outer=TRUE)
                 mtext(text=sub.text, side=3,cex=1.0, outer=TRUE)
                 OWRD_basin <- strsplit(input$selectpHCrit, " - ")[[1]][1]
                 crit_selected <- strsplit(input$selectpHCrit, " - ")[[1]][2]
                 ph_crit_min <- ph_crit[ph_crit$ph_standard == crit_selected & 
                                          ph_crit$OWRD_basin == OWRD_basin & 
                                          ph_crit$plan_name == input$select, 'ph_low']
                 ph_crit_max <- ph_crit[ph_crit$ph_standard == crit_selected &
                                          ph_crit$OWRD_basin == OWRD_basin & 
                                          ph_crit$plan_name == input$select, 'ph_high']
                 exceeds.points <- new_data[new_data$Result < ph_crit_min | new_data$Result > ph_crit_max,]
                 points(exceeds.points$Sampled, exceeds.points$Result, col="red", pch=20) ####plot the exceedances
                 if(p.value.label !="Not Significant"){
                   lines(x=c(x.min, x.max), y=c(SK.min, SK.max), col="red", lwd=2)#draw Seasonal Kendall slope line using median concentration at average date
                 }
                 lines(x=c(x.min, x.max), y=c(ph_crit_min, ph_crit_min), lty=2)#draw WQS 
                 lines(x=c(x.min, x.max), y=c(ph_crit_max, ph_crit_max), lty=3)#draw WQS 
                 legend(x=par("usr")[1],y=par("usr")[3], legend=c("Maximum criterion", 
                                                                  "Minimum criterion", 
                                                                  "Seasonal Kendall trend"), 
                        lty=c(2,3,1), col=c("black","black","red"), lwd=c(1,1,2), 
                        xjust=-0.01, yjust=-8., box.lty=0, cex=1.0, horiz=TRUE)}),
               "Temperature" = ({ new_data <- DataUse()
               validate(
                 need(is.data.frame(new_data), "Insufficient data to calculate a single 7DADM")
               )
                 new_data$Sampled <- as.POSIXct(strptime(new_data$date, format = '%m/%d/%y'))  
                 x.min <- min(new_data$Sampled) #min of subset date
                 x.max <- max(new_data$Sampled) #max of subset date
                 x.lim <- c(x.min, x.max) ####define the data domain for graph
                 y.min <- if(floor(min(new_data$sdadm, na.rm = TRUE))<=0 ){ #min of data for graph& log.scale=="y"
                   1 #set minimum y value for log scale to one
                 }else{
                   10
                   #floor(min(new_data$sdadm, na.rm = TRUE))
                 }
                 y.max <- ceiling(max(new_data$sdadm, na.rm = TRUE)) #max of data for graph
                 y.lim <- c(y.min,y.max) ####define the data range
                 #river.mile <- spawning.period.table[spawning.period.table$STATION == station,'RM']
                 title <- paste0(unique(df.all$Station_Description)[unique(df.all$Station_ID) == new_data[1,"id"]], 
                                 ", ID = ", 
                                 new_data[1,"id"]) #, " , river mile = ",river.mile
                 x.lab <- "month"
                 y.lab <- "Temperature (7DADM)"
                 ####definitions for drawing Seasonal Kendall slope line
                 y.median <- median(new_data$sdadm)
                 slope <- as.numeric(SeaKen[SeaKen$Station_ID == unique(new_data$id),'slope'] )
                 p.value <- as.numeric(SeaKen[SeaKen$Station_ID == unique(new_data$id),'pvalue'] )
                 p.value.label <- SeaKen[SeaKen$Station_ID == unique(new_data$id),'signif'] 
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
                 plot(new_data$Sampled, new_data$sdadm, xlim=x.lim, ylim=y.lim, xlab="", ylab=y.lab, bty="L") ####plot the points , log=log.scale  
                 title(main=title, cex.main=1.2, outer=TRUE)
                 mtext(text=sub.text, side=3,cex=1.0, outer=TRUE)
                 exceeds.points <- new_data[new_data$exceedsummer | new_data$exceedspawn,]
                 points(exceeds.points$Sampled, exceeds.points$sdadm, col="red", pch=20) ####plot the exceedances
                 if(p.value.label !="Not Significant"){
                   lines(x=c(x.min, x.max), y=c(SK.min, SK.max), col="red", lwd=2)#draw Seasonal Kendall slope line using median concentration at average date
                 }
                 ####Draw WQS
                 spn_index <- which(new_data$bioc == 13)
                 spn_diff <- diff(spn_index)
                 
                 if (all(spn_diff == 1)) {
                   if (length(spn_index) > 0) {
                     spn_1 <- max(spn_index)
                     
                     if (spn_1 == nrow(new_data)) {
                       #Plot non-spawn time-period
                       lines(x = c(new_data[1, 'Sampled'],
                                   new_data[spn_index[1] - 1, 'Sampled']),
                             y = c(unique(new_data[1:(spn_index[1] - 1),'bioc']),
                                   unique(new_data[1:(spn_index[1] - 1),'bioc'])), lty = 3)
                     } else {
                       #Plot non-spawn time-period
                       lines(x = c(new_data[spn_1 + 1, 'Sampled'],
                                   new_data[nrow(new_data), 'Sampled']),
                             y = c(unique(new_data[(spn_1 + 1):nrow(new_data),'bioc']),
                                   unique(new_data[(spn_1 + 1):nrow(new_data),'bioc'])), lty = 3)
                     }
                     #Plot spawn time period
                     lines(x = c(new_data[spn_index[1],'Sampled'],
                                 new_data[spn_1,'Sampled']),
                           y = c(unique(new_data[spn_index[1]:spn_1,'bioc']),
                                 unique(new_data[spn_index[1]:spn_1,'bioc'])), lty=2)
                   } else {
                     lines(x = c(new_data[1,'Sampled'],
                                 new_data[nrow(new_data),'Sampled']),
                           y = c(unique(new_data[1:nrow(new_data),'bioc']),
                                 unique(new_data[1:nrow(new_data),'bioc'])), lty = 3)
                   }
                   
                 } else {
                   spn_stop <- spn_index[which(spn_diff > 1)]
                   spn_start <- spn_index[which(spn_diff > 1) + 1]
                   nspn_start <- spn_stop + 1
                   nspn_stop <- spn_start - 1
                   
                   
                   for (i in 1:length(spn_start)) {
                     if (i < length(spn_start)) {
                       #Plot next spawn time period
                       lines(x = c(new_data[spn_start[i],'Sampled'],
                                   new_data[spn_stop[i + 1],'Sampled']),
                             y = c(unique(new_data[spn_start[i]:spn_stop[i + 1],'bioc']),
                                   unique(new_data[spn_start[i]:spn_stop[i + 1],'bioc'])), lty = 2)
                       
                       #Plot non-spawn time period
                       lines(x = c(new_data[nspn_start[i],'Sampled'],
                                   new_data[nspn_stop[i], 'Sampled']),
                             y = c(unique(new_data[nspn_start[i]:nspn_stop[i],'bioc']),
                                   unique(new_data[nspn_start[i]:nspn_stop[i],'bioc'])), lty = 3)
                     } else {
                       #Plot last spawn-time period
                       lines(x = c(new_data[spn_start[i],'Sampled'],
                                   new_data[max(spn_index),'Sampled']),
                             y = c(unique(new_data[spn_start[i]:max(spn_index),'bioc']),
                                   unique(new_data[spn_start[i]:max(spn_index),'bioc'])), lty = 2)
                       
                       #Plot non-spawn time period
                       lines(x = c(new_data[nspn_start[i],'Sampled'],
                                   new_data[nspn_stop[i], 'Sampled']),
                             y = c(unique(new_data[nspn_start[i]:nspn_stop[i],'bioc']),
                                   unique(new_data[nspn_start[i]:nspn_stop[i],'bioc'])), lty = 3)
                       
                       #Plot last non-spawn time period
                       if (new_data[nrow(new_data),'bioc'] != 13) {
                         lines(x = c(new_data[max(spn_index) + 1,'Sampled'],
                                     new_data[nrow(new_data), 'Sampled']),
                               y = c(unique(new_data[(max(spn_index) + 1):nrow(new_data),'bioc']),
                                     unique(new_data[(max(spn_index) + 1):nrow(new_data),'bioc'])), lty = 3)
                       }
                     }
                   }
                   
                   #Plot first non-spawn time period TODO: Add functionality to check if start of data is in spawning or non-spawning
                   if (spn_index[1] != 1) {
                     lines(x = c(new_data[1,'Sampled'],
                                 new_data[spn_index[1] - 1, 'Sampled']),
                           y = c(unique(new_data[1:(spn_index[1] - 1), 'bioc']),
                                 unique(new_data[1:(spn_index[1] - 1), 'bioc'])), lty = 3)
                   }
                   
                   #Plot first spawn time period
                   lines(x = c(new_data[spn_index[1],'Sampled'],
                               new_data[spn_stop[1],'Sampled']),
                         y = c(unique(new_data[spn_index[1]:spn_stop[1],'bioc']),
                               unique(new_data[spn_index[1]:spn_stop[1],'bioc'])), lty=2)
                   
                 }
                 
                 
                 
                 
                 legend(x=par("usr")[1],y=par("usr")[3], legend=c("Spawning criterion", 
                                                                  "Non-spawning criterion", 
                                                                  "Seasonal Kendall trend"), 
                        lty=c(2,3,1), col=c("black","black","red"), lwd=c(1,1,2), 
                        xjust=-0.01, yjust=-8., box.lty=0, cex=1.0, horiz=TRUE)
                 }),
              "E. Coli" = ({ new_data <- DataUse()
                ecoli_gm_eval <- Evaluate30dayEcoli(new_data, input$selectParameter, input$selectStation)
                x.min <- as.POSIXct(strptime(input$selectRange[1], format = '%Y-%m-%d'))#min(new_data$Sampled) #min of subset date
                x.max <- as.POSIXct(strptime(input$selectRange[2], format = '%Y-%m-%d'))#max(new_data$Sampled) #max of subset date
                x.lim <- c(x.min, x.max) ####define the data domain for graph
                y.min <- if(floor(min(new_data$Result))<=0 & input$selectLogScale){ #min of data for graph TODO: Add check box for log scale  & log.scale=="y"
                  1 #set minimum y value for log scale to one
                }else{
                  floor(min(new_data$Result))
                }
                y.max <- max(ceiling(max(new_data$Result)),415) #max of data for graph
                y.lim <- c(y.min,y.max) ####define the data range
                title <- paste0(min(new_data$Station_Description), ", ID = ", min(new_data$Station_ID))
                x.lab <- "month"
                y.lab <- "E. Coli"
                ####definitions for drawing Seasonal Kendall slope line
                y.median <- median(new_data$Result)
                slope <- as.numeric(SeaKen[SeaKen$Station_ID == unique(new_data$Station_ID) & SeaKen$analyte == unique(new_data$Analyte),'slope'])
                p.value <- as.numeric(SeaKen[SeaKen$Station_ID== unique(new_data$Station_ID) & SeaKen$analyte == unique(new_data$Analyte),'pvalue'] )
                p.value.label <- SeaKen[SeaKen$Station_ID == unique(new_data$Station_ID) & SeaKen$analyte == unique(new_data$Analyte),'signif'] 
                x.delta <- as.numeric((x.max-x.min)/2)####average date
                SK.min <- y.median-x.delta*slope/365.25#minimum y value for line
                SK.max <- y.median+x.delta*slope/365.25#maximum y value for line
                sub.text <- paste0("p value = " ,round(p.value, digits=3),", ",  p.value.label, ", slope = ", round(slope, digits=2), ", n = ", nrow(new_data))
                ####plot the timeseries
                par(xpd=NA,oma=c(0,0,4,0), mar=c(5.1,4.1,3.1,2.1)) 
                plot(new_data$Sampled, new_data$Result, xlim=x.lim, ylim=y.lim, xlab="", ylab=y.lab, bty="L", log = ifelse(input$selectLogScale,"y","")) ####plot the points , log=log.scale  
                points(as.POSIXct(strptime(ecoli_gm_eval$day, format = "%Y-%m-%d")), ecoli_gm_eval$gm, pch = 2)
                title(main=title, cex.main=1.2, outer=TRUE)
                mtext(text=sub.text, side=3,cex=1.0, outer=TRUE)
                exceeds.points.sampled <- new_data[new_data$Result > 406,]
                points(exceeds.points.sampled$Sampled, exceeds.points.sampled$Result, col="red", pch=20) ####plot the exceedances
                if (nrow(ecoli_gm_eval > 0)) {
                  ecoli_gm_eval$Sampled <- as.POSIXct(ecoli_gm_eval$day)
                  exceeds.points.gm <- ecoli_gm_eval[ecoli_gm_eval$gm > 126,]
                  points(exceeds.points.gm$Sampled, exceeds.points.gm$gm, col = "maroon", pch = 17)
                  }
                if(p.value.label !="Not Significant"){
                  lines(x=c(x.min, x.max), y=c(SK.min, SK.max), col="red", lwd=2)#draw Seasonal Kendall slope line using median concentration at average date
                }
                lines(x=c(x.min, x.max), y=c(406, 406), lty=2)#draw WQS 
                lines(x=c(x.min, x.max), y=c(126, 126), lty=3)#draw WQS 
                legend(x=x.min,y=y.min, legend=c("Maximum criterion", "Geomean criterion", "Seasonal Kendall trend"), 
                       lty=c(2,3,1), col=c("black","black","red"), lwd=c(1,1,2), xjust=-0.01, yjust=-8.3, box.lty=0, cex=1.0, horiz=TRUE)
                legend(x=x.min,y=y.min, legend=c("Single Sample", "Geomean Values"), 
                       pch=c(1,2), col=c("black","black"), xjust=-0.5, yjust=-8.4, box.lty=0, cex=0.9, horiz=TRUE)            
                })
        )
        })
  })
})

options(warn = 0)