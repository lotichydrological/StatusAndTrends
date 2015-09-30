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
source('data/funPlots.R')

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

wq_limited <- readOGR(dsn = './data/GIS', layer = 'ORStreamsWaterQuality_2010_WQLimited_V3', verbose = FALSE)
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
        
        wq_limited <- wq_limited[wq_limited$Pollutant %in% unique(df.all$Analyte),]
        wq_limited <- spTransform(wq_limited, CRS("+init=epsg:4269"))
        wq_limited <- wq_limited[ag_sub,]
        wq_limited <- data.frame(lapply(wq_limited@data, factor))
        
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
        
        df.all[df.all$Analyte == 'Fecal Coliform','Result'] <- round(fc2ec(df.all[df.all$Analyte == 'Fecal Coliform','Result']))
        df.all[df.all$Analyte == 'Fecal Coliform','Comment'] <- ifelse(is.na(df.all[df.all$Analyte == 'Fecal Coliform','Comment']),
                                                                       "Fecal Coliform value converted to E. Coli",
                                                                       paste(df.all[df.all$Analyte == 'Fecal Coliform','Comment'],
                                                                             "Fecal Coliform value converted to E. Coli",
                                                                             sep = ", "))
        df.all[df.all$Analyte == 'Fecal Coliform','Analyte'] <- 'E. Coli'
        
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
        
        df.totals <- as.data.frame.matrix(table(df.all$Station_ID, df.all$Analyte))
        df.totals <- cbind(rownames(df.totals), df.totals)
        df.totals <- rename(df.totals, c("rownames(df.totals)" = 'Station_ID'))
        rownames(df.totals) <- 1:nrow(df.totals)

        
        all.sp <- merge(df.all[,c('Station_ID','Station_Description','DECIMAL_LAT','DECIMAL_LONG')], df.totals, by = 'Station_ID', all.x = TRUE)
        all.sp <- all.sp[!duplicated(all.sp$Station_ID),]
        coordinates(all.sp) = ~DECIMAL_LONG+DECIMAL_LAT
        proj4string(all.sp) <- CRS("+init=epsg:4269")
        
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
              
            m <- plotGoogleMaps(all.sp, 
                              add = TRUE, 
                              filename = 'myMap2.html', 
                              openMap = FALSE, 
                              legend = FALSE, 
                              layerName = "Sampling stations", 
                              mapTypeId = "ROADMAP")
            
              incProgress(prog, detail = "Plotting Ag Area")
              prog <- 2/3
              
            m <- plotGoogleMaps(ag_sub, 
                                previousMap = m, 
                                filename = "myMap2.html", 
                                openMap = FALSE, 
                                layerName = "Ag Plan Areas", 
                                legend = FALSE, 
                                colPalette = "light green")
            
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
                                   "Data in tabular format" = 'df.sub',
                                   "WQ Limited Waters within Ag Area" = 'wq_limited'),
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
                                                                    c('Var1' = "'Reason for removal'", 
                                                                      'Freq' = "'Number of observations removed'"))
                                                  }
                                                  else {
                                                    out <- data.frame("Message" = "All data met QC requirments")
                                                  }),
                                                "df.station.results" = (
                                                  df.totals
                                                ),
                                                "df.sub" = ({
                                                  out <- data.frame(lapply(df.all, 
                                                                    FUN = function(x) {
                                                                      if (all(class(x) == 'character')) {
                                                                         x <- factor(x)
                                                                        } else {
                                                                          x
                                                                        }
                                                                      }
                                                                    )
                                                             )
                                                  out$Station_ID <- factor(out$Station_ID)
                                                  out
                                                }),
                                                "wq_limited" = (
                                                  wq_limited
                                                )
      )},filter = 'top',server = TRUE)
      
      output$selectStation = renderUI({
        validate(
          need(is.data.frame(df.all), message = FALSE)
        )
        selectInput("selectStation","Select station to evaluate:",
                    choices = sort(unique(paste(df.all$Station_ID, 
                                           df.all$Station_Description, sep = ' - '))),
                    selectize = TRUE)
      })
      #updateSelectInput(session, "selectStation", choices = unique(paste(df.all$Station_ID, df.all$Station_Description, sep = ' - ')))
      output$selectParameter = renderUI({mydata <- unique(df.all[df.all$Station_ID == unique(strsplit(input$selectStation,' - ')[[1]][1]),'Analyte'])
                                         selectInput('selectParameter','Select parameter to evaluate:',mydata)})
      
      output$selectLogScale = renderUI({
        validate(
          need(input$selectParameter %in% c('E. Coli','Enterococcus'),message = FALSE)
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
      
      output$fish_use_link <- renderUI({
        validate(
          need(input$selectParameter == 'Temperature', message = FALSE)
        )
        h5(a("Refer to Fish Use and Spawning Use Maps by Basin", 
             href = "http://www.deq.state.or.us/wq/rules/div041tblsfigs.htm#f1",
             target = "_blank"))
      })
      
      output$plotTrend <- renderUI({
        checkboxInput("plotTrend", "Plot Seasonal Kendall trend line (Note: May not be significant)")
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
                                                        df.all$Analyte == input$selectParameter,],
                                  "Enterococcus" = df.all[df.all$Station_ID == unique(strsplit(input$selectStation,' - ')[[1]][1]) &
                                                            df.all$Analyte == input$selectParameter,])
        })
      
        output$ts_plot <- renderPlot({
          switch(input$selectParameter,
               "pH" = ({      new_data <- DataUse()
                  plot.ph(new_data, 
                          SeaKen, 
                          ph_crit, 
                          x_min = input$selectRange[1],
                          x_max = input$selectRange[2],
                          plot_trend = input$plotTrend,
                          plot_criteria = input$selectpHCrit)
                 }),
               "Temperature" = ({ new_data <- DataUse()
               validate(
                 need(is.data.frame(new_data), "Insufficient data to calculate a single 7DADM")
               )
                plot.Temperature(new_data, 
                                 df.all, 
                                 SeaKen, 
                                 plot_trend = input$plotTrend)
                 }),
              "E. Coli" = ({ new_data <- DataUse()
                ecoli_gm_eval <- gm_mean_30_day(new_data, unique(new_data$Analyte), unique(new_data$Station_ID))
                plot.ecoli(new_data, 
                           SeaKen, 
                           ecoli_gm_eval,
                           plot_trend = input$plotTrend,
                           plot_log = input$selectLogScale,
                           x_min = input$selectRange[1],
                           x_max = input$selectRange[2])
                }),
                "Enterococcus" = ({ new_data <- DataUse()
                  entero_gm_eval <- gm_mean_30_day(new_data, unique(new_data$Analyte), unique(new_data$Station_ID))
                  plot.entero(new_data, 
                             SeaKen, 
                             entero_gm_eval,
                             plot_trend = input$plotTrend,
                             plot_log = input$selectLogScale,
                             x_min = input$selectRange[1],
                             x_max = input$selectRange[2])
                })
          )
        })
  })
})

options(warn = 0)