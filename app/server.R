#Use this to make it accessible for other people to access
#runApp("app",host="0.0.0.0",port=3168)

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
library(ggplot2)
#library(xlsx)
#library(RODBC)

suppressWarnings(rm(list = ls()))

options(stringsAsFactors = FALSE, warn = -1, shiny.reactlog = FALSE)

source('functions/01_DataQuery.R')
source('functions/funClean.R')
source('functions/funPlots.R')
source('functions/funSeaKen.R')
source('functions/funHelpers.R')

agwqma <- readOGR(dsn = './data/GIS', layer = 'ODA_AgWQMA', verbose = FALSE)

#Bring in the HUC list mapped to overlapping ag plan areas
#This process would not produce desired results (only those HUCs wholly contained by the plan area)
#When done through the use of intersection and overlay tools provided in the sp and rgdal packages
#Therefore, a manual identificiation of the desired HUCs for each Ag plan area was completed
HUClist <- read.csv('data/PlanHUC_LU.csv')

#Table of ph criteria for lookup
ph_crit <- read.csv('data/PlanOWRDBasinpH_LU.csv')

#The entire list of WQP parameters that match to a criteria
parms <- read.csv('data/WQP_Table3040_Names.csv', stringsAsFactors = FALSE)

#Pre-extracted 303(d) with PlanName added
wq_limited <- read.csv('data/wq_limited_df_temp_bact_ph.csv')

shinyServer(function(input, output, session) { 
  ###################################
  ###################################
  ####### Data Query Tab ###########
  ###################################
  ###################################
  
  #First check for all required inputs and provide a place for printing the text
  #since the result of the button isn't directly a single table or chart
  input_check_text <- eventReactive(input$action_button, {
    if (input$select == "") {
      "Please select a Plan Area"
    } else if (is.null(input$parms)) {
      "Please select a parmaeter to query"
    } else if (input$dates[1] == input$dates[2]) {
      "Please select non-identical dates"
    } else if (is.null(input$db)) {
      "Please select a database to query"
    } else {
      HTML(paste0("You just submitted ", 
              input$select, 
              " Plan Area Query for ", 
              paste(input$parms,collapse=", "), 
              " from",
              "<br/>",
              input$dates[1], 
              " to ", 
              input$dates[2]))
    }
  })
  
  output$text1 <- renderUI(input_check_text())
  
  #Then, if all the inputs check out, run the query
  observeEvent(input$action_button, {
    if (input$select != "" & !is.null(input$parms) & 
        input$dates[1] != input$dates[2] & !is.null(input$db)) {
    withProgress(message = "Processing:", value = 0, {
      #### Query the databases ####
      wqpData <- NULL
      lasarData <- NULL
      elmData <- NULL
      nwisData <- NULL
      df.all <- NULL
      lstSummaryDfs <- list()
      prog <- 0
      wqp_message <- ""
      
      if ('Water Quality Portal' %in% input$db) {
        incProgress(1/10, detail = 'Querying the Water Quality Portal')
        prog <- prog + 1/10
        wqpData <- tryCatch(wqpQuery(planArea = input$select,
                                       HUClist = HUClist,
                                       inParms = input$parms,
                                       luParms = parms,
                                       startDate = input$dates[1],
                                       endDate = input$dates[2]),
                              error = function(err) {err <- geterrmessage()})
          
        if (any(c('Temperature', 'pH') %in% input$parms)) {
          incProgress(1/10, detail = 'Querying NWIS continuous data')
          prog <- prog + 1/10
          nwisData <- tryCatch(nwisQuery(planArea = input$select,
                                         HUClist = HUClist,
                                         inParms = input$parms,
                                         startDate = input$dates[1],
                                         endDate = input$dates[2]),
                               error = function(err) {err <- geterrmessage()})
        }
        
        if (is.null(wqpData) & is.null(nwisData)) {
            wqp_message <- 'Your query returned no results from the Water Quality Portal.'
          } else if (!is.data.frame(wqpData) & !is.null(wqpData)) {
            if (grepl("307", wqpData)) {
            wqp_message <- 'Water Quality Portal is busy. Please try again in a few minutes.'
          }
        }
        }
        
        if ('DEQ' %in% input$db) {
          incProgress(1/10, detail = 'Querying the LASAR database')
          prog <- prog + 1/10
          
          lasarData <- lasarQuery(planArea = input$select,
                                  HUClist = HUClist,
                                  inParms = input$parms,
                                  startDate = input$dates[1],
                                  endDate = input$dates[2])
          odbcCloseAll()
          if (nrow(lasarData) == 0) lasarData <- NULL
          
          
          incProgress(1/10, detail = 'Querying the Element database')
          prog <- prog + 1/10
          
          elmData <- elementQuery(planArea = input$select,
                                  HUClist = HUClist,
                                  inParms = input$parms,
                                  startDate = input$dates[1],
                                  endDate = input$dates[2])
          odbcCloseAll()
          if (nrow(elmData) == 0) elmData <- NULL
        }
        
        
        if (wqp_message != 'Water Quality Portal is busy. Please try again in a few minutes.') {
          incProgress(1/10, detail = 'Combining query results')
          prog <- prog + 1/10
          
          df.all <- tryCatch(combine(E=elmData,L=lasarData,W=wqpData,N=nwisData),
                             error = function(err) 
                             {err <- geterrmessage()})
        }

        if(is.null(df.all)) {
          output$text2 <- renderUI(HTML("<b> Your query returned no data </b>"))
        } else {
          if (wqp_message != "") {
            output$text2 <- renderText(wqp_message)
          } else {
            output$text2 <- renderText("")
          }
          #### Tabulate Results ####
          incProgress(1/10, detail = "Tabulating results")
          prog <- prog + 1/10
          
          #Isolate to only include data at stations in the plan area polygon
          df.all <- clipToPlanArea(df.all, agwqma, input$select)
          
          #Summarize Stations and Number of results by Analyte
          all.totals <- tabulateResults(df.all)
          
          #Summarize results by organization
          lstSummaryDfs[[1]] <- summarizeByOrg(df.all)
          names(lstSummaryDfs)[1] <- "df.org"
          
          #Tabulate number of results at each station
          lstSummaryDfs[[2]] <- summarizeByStation(df.all)
          names(lstSummaryDfs)[2] <- "df.station.totals"
          
          #### Cleaning result field ####
          incProgress(1/10, detail = "Cleaning result field")
          prog <- prog + 1/10
          #Fix non-numeric results in the Result field
          df.all$Result <- clean(df.all$Result)
          lstSummaryDfs[[3]] <- attr(df.all$Result, "report")
          names(lstSummaryDfs)[3] <- "df.report"
          df.all$Result <- suppressWarnings(as.numeric(df.all$Result))
          
          #MRL handling
          df.all <- MRLhandling(df.all)
          
          #Fecal coliform to e. coli conversion
          if ("Fecal Coliform" %in% df.all$Analyte) {
            df.all <- update_fc2ec(df.all)
          }

          #### Calculate trends and adnl data for plotting ####
          incProgress(1/10, detail = "Calculating 7DADM and Trends")
          prog <- prog + 1/10
          #Generate sdadm once for temperature plotting/exceedance use
          if (any('Temperature' %in% df.all$Analyte)) {
            sdadm <- Calculate.sdadm(df.all, "Result", "Station_ID", "Sampled",
                                     '%Y-%m-%d %H:%M:%S')
          } else {
            sdadm <- NULL
          }
          
          #Run Seasonal Kendall for pH and Bacteria
          if (any(c('pH', 'E. Coli', "Enterococcus") %in% df.all$Analyte)) {
            SeaKen <- run_seaKen(df.all)
          } else {
            SeaKen <- NULL
          }
          
          #Calculate 30 GM for E. Coli and Enterococcus
          #WIll get to this later. May need to go in plotting section.
          
          #### Performing QA Screen ####
          incProgress(1/10, detail = "Performing QA Screen")
          prog <- prog + 1/10
          #Check QA info and remove data not meeting QA objectives
          df.all <- remove_QAfail(df.all)
          #Pull out the tracking data frame of data removed
          lstSummaryDfs[[4]] <- attr(df.all, "removal_tracking")
          names(lstSummaryDfs)[4] <- "df.removal"
          
          #### Preparing data for mapping ####
          incProgress(1/10, detail = "Preparing data for mapping")
          prog <- prog + 1/10
          #Generate layer for mapping
          all.sp <- generateStnLyrToPlot(df.all, lstSummaryDfs[["df.station.totals"]])
          
          #Restrict ag plan areas to select plan area
          ag_sub <- agwqma[agwqma$PlanName == input$select,]
          ag_sub <- spTransform(ag_sub, CRS("+init=epsg:4269"))
          
          #Restrict layer for mapping to just the selected plan area
          all.sp <- all.sp[ag_sub,]
          
          incProgress(1/10, detail = "Extracting 303(d) listed segments")
          prog <- prog + 1/10
          #Extract 303(d) segments in the plan area for parameters
          #returned in the query
          lstSummaryDfs[[5]] <- extract_303d(df.all, wq_limited, input$select)
          names(lstSummaryDfs)[5] <- "wq_limited"
        }
      })
      
      #This outputs the table of results returned by parameter
      output$all_totals<- renderTable({
        validate(
          need(try(is.data.frame(df.all)), message = FALSE)
        )
        all.totals
        })
      
      #This creates the button for downloading the raw formatted data
      output$downloadData <- renderUI({
        validate(
          need(is.data.frame(df.all), message = FALSE)
        )
        downloadButton("dlDataHandler", "Download the data")
      })
      
      output$dlDataHandler <- downloadHandler(
        filename = function() {
          paste(input$select, "_", format(Sys.time(), "%Y%m%d_%H%M"), 
                ".csv",sep='')
        },
        content = function(file) {
          write.csv(df.all, file, row.names = FALSE)
        }
      )
      
      #This adds the View Map action button
      output$action_button_map <- renderUI({
        validate(
          need(is.data.frame(df.all), message = FALSE)
        )
        actionButton(inputId = 'action_button_map',label = 'View map')
      })
      
      #This builds the map view
      observeEvent(input$action_button_map, {
        output$mymap <- renderUI({
          req(ag_sub)
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
      })
      
      ###################################
      ###################################
      ####### Data Review Tab ###########
      ###################################
      ###################################
      
      #The control is built here
      output$review_control <- renderUI({
        validate(
          need(is.data.frame(df.all), message = FALSE)
        )
        selectInput(inputId =  "ReviewDf", 
                    label = 'Select Review table to view:',
                    choices = list("Parameter results by station" = 
                                     'df.station.totals',
                                   "Data in tabular format" = 'df.sub',
                                   "WQ Limited Waters within Ag Area" = 
                                     'wq_limited',
                                   "QA - Summary by organization" = 'df.org',
                                   "QA - Result values modified" = "df.report",
                                   "QA - Data removal information" = "df.removal",
                                   "QA - Unique comment values" = 'df.comment'),
                                   
                    selectize = TRUE
        )
      })
      
      #The table is built here
      output$display <- DT::renderDataTable({
        validate(
          need(input$ReviewDf != "", message = FALSE)
        )
        pickReviewDf(input_reviewDf = input$ReviewDf, lstSummaryDfs, df.all)
        }, options = list(processing = FALSE), filter = 'top') 

      ###################################
      ###################################
      #### Plot Status and Trend Tab ####
      ###################################
      ###################################
      
      #First build the controls
      output$selectStation = renderUI({
        validate(
          need(is.data.frame(df.all), message = FALSE)
        )
        selectInput("selectStation","Select station to evaluate:",
                    choices = c("Choose one"="",sort(
                      unique(paste(df.all$Station_ID, 
                                   df.all$Station_Description, 
                                   sep = ' - ')))),
                    selectize = TRUE,
                    selected = NULL)
      })
      
      output$selectParameter = renderUI({
        validate(
          need(input$selectStation != "", message = FALSE)
        )
        mydata <- unique(df.all[df.all$Station_ID == unique(
          strsplit(input$selectStation,' - ')[[1]][1]),'Analyte'])
        selectInput('selectParameter','Select parameter to evaluate:',
                    c("Choose one"="",mydata))
      })
      
      output$selectLogScale = renderUI({
        validate(
          need(input$selectParameter %in% c('E. Coli','Enterococcus'),
               message = FALSE)
        )
        checkboxInput("selectLogScale", "Plot data with log scale")
      })
      
      output$selectpHCrit = renderUI({
        validate(
          need(input$selectParameter == 'pH',message = FALSE)
        )
        ph_crit_choices <- paste(ph_crit[ph_crit$plan_name == 
                                           input$select,c('OWRD_basin')],
                                 ph_crit[ph_crit$plan_name == 
                                           input$select,c('ph_standard')],
                                 sep = " - ")
        selectInput('selectpHCrit',
                    "Select applicable OWRD Basin specific pH criteria:",
                    choices = ph_crit_choices,
                    selectize = TRUE)
      })
      
      output$plotTrend <- renderUI({
        validate(
          need(input$selectParameter %in% c('pH', 'E. Coli', 'Enterococcus'), 
               message = FALSE)
        )
        checkboxInput("plotTrend", 
                      "Plot Seasonal Kendall trend line 
                            (Note: May not be significant)")
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
      
      output$fish_use_link <- renderUI({
        validate(
          need(input$selectParameter == 'Temperature', message = FALSE)
        )
        h5(a("Refer to Fish Use and Spawning Use Maps by Basin", 
             href = "http://www.deq.state.or.us/wq/rules/div041tblsfigs.htm#f1",
             target = "_blank"))
      })
      
      #######################
      #### Data Handling ####
      #######################
      
      #Next create the reactive data frame based on the inputs
      DataUse <- reactive({
        generate_new_data(df.all, 
                          sdadm, 
                          input$selectStation, 
                          input$selectParameter)
      })
      
      #This builds the exceedance table
      output$exceed_df <- DT::renderDataTable({
        validate(
          need(input$selectStation != '', message = FALSE)
        )
        validate(
          need(input$selectParameter != "", message = FALSE)
        )
        if (input$selectParameter != '') {
          if (input$selectParameter == 'Temperature') {
            validate(
              need(!is.null(input$selectUse), message = FALSE)
            )
          } else if (input$selectParameter == 'pH') {
            validate(
              need(!is.null(input$selectpHCrit), message = FALSE)
            )
          } else if (input$selectParameter == 'E. Coli' |
                     input$selectParameter == 'Enterococcus') {
            validate(
              need(!is.null(input$selectLogScale), message = FALSE)
            )
          }
        }
        generate_exceed_df(DataUse(), 
                           input$selectParameter, 
                           input$selectpHCrit,
                           ph_crit, 
                           PlanName = input$select, 
                           input$selectStation, 
                           input$selectUse, 
                           input$selectSpawning)
        })
      
      # #Make the plot interactive
      #First set up the ranges object
      ranges <- reactiveValues(x = NULL, y = NULL)
      
      #Next make sure it resets when Station and Parameter change
      observeEvent(input$selectStation, {
        ranges$x <- NULL
        ranges$y <- NULL
      })
      
      observeEvent(input$selectParameter, {
        ranges$x <- NULL
        ranges$y <- NULL
      })
      
      #Then add in the observation that creates the range based on the brush
      observeEvent(input$plot1_dblclick, {
        brush <- input$plot1_brush
        if (!is.null(brush)) {
          ranges$x <- c(as.POSIXct(brush$xmin, origin = "1970-01-01"),
                        as.POSIXct(brush$xmax, origin = "1970-01-01"))
          ranges$y <- c(brush$ymin, brush$ymax)
        } else {
          ranges$x <- NULL
          ranges$y <- NULL
        }
      })
      
      #Reactive function to build plot based on input$selectParameter
      plotInput <- reactive({
        df <- DataUse()
        switch(EXPR = input$selectParameter,
               "pH" = ({
                 if (nrow(df) > 2) {
                   g <- plot.ph(new_data = df, 
                                sea_ken_table = SeaKen,  
                                ph_crit,
                                plot_trend = input$plotTrend,
                                plot_criteria = input$selectpHCrit,
                                plan_area = input$select)
                 } else {
                   g <- ggplot(data.frame()) + geom_point() + 
                     annotate("text", label = "Insufficient data for plotting", 
                              x = 1, y = 1)
                 }
                 
                 g <- g + coord_cartesian(xlim = ranges$x, ylim = ranges$y)
               }),
               "Temperature" = ({
                 if (any(!is.na(df$sdadm))) {
                   g <- plot.Temperature(new_data = df, 
                                         all_data = df.all,
                                         selectUse = input$selectUse,
                                         selectSpawning = input$selectSpawning)
                 } else {
                   g <- ggplot(data.frame()) + geom_point() + 
                     annotate("text", label = "Insufficient data to calculate a single 7DADM", 
                              x = 1, y = 1)
                 }
                 
                 g <- g + coord_cartesian(xlim = ranges$x, ylim = ranges$y)
               }),
               "E. Coli" = ({
                 if (nrow(df) > 2) {
                   g <- plot.bacteria(new_data = df,
                                      sea_ken_table = SeaKen,
                                      plot_trend = input$plotTrend,
                                      plot_log = input$selectLogScale,
                                      parm = 'E. Coli')
                 } else {
                   g <- ggplot(data.frame()) + geom_point() + 
                     annotate("text", label = "Insufficient data for plotting", 
                              x = 1, y = 1)
                 }
                 
                 if (input$selectLogScale) {
                   g <- g + coord_trans(ytrans = "log10", limx = ranges$x, limy = ranges$y)
                 } else {
                   g <- g + coord_cartesian(xlim = ranges$x, ylim = ranges$y)
                 }
               }),
               "Enterococcus" = ({
                 if (nrow(df) > 2) {
                   g <- plot.bacteria(new_data = df,
                                      sea_ken_table = SeaKen,
                                      plot_trend = input$plotTrend,
                                      plot_log = input$selectLogScale,
                                      parm = 'Enterococcus')
                 } else {
                   g <- ggplot(data.frame()) + geom_point() + 
                     annotate("text", label = "Insufficient data for plotting", 
                              x = 1, y = 1)
                 }
                 
                 if (input$selectLogScale) {
                   g <- g + coord_trans(ytrans = "log10", limx = ranges$x, limy = ranges$y)
                 } else {
                   g <- g + coord_cartesian(xlim = ranges$x, ylim = ranges$y)
                 }
               })
        )
        #g <- g + scale_x_datetime(breaks = "1 day") #TODO: Make date labeling better

        g
      })
      
      #Call the plot rendering function making sure it doesn't try to generate the
      #plot prematurely by adding in the cascading validations
      output$ts_plot <- renderPlot({
        validate(
          need(input$selectStation != '', message = FALSE)
        )
        validate(
          need(input$selectParameter != "", message = FALSE)
        )
        if (input$selectParameter != '') {
          if (input$selectParameter == 'Temperature') {
            validate(
              need(!is.null(input$selectUse), message = FALSE)
            )
          } else if (input$selectParameter == 'pH') {
            validate(
              need(!is.null(input$selectpHCrit), message = FALSE)
            )
          } else if (input$selectParameter == 'E. Coli' | 
                     input$selectParameter == 'Enterococcus' ) {
            validate(
              need(!is.null(input$plotTrend), message = FALSE)
            )
          }
        }
        plotInput()
      })
      
      output$downloadPlot <- downloadHandler(filename = function () {
        paste(
          strsplit(input$selectStation, split = " - ")[[1]][1], "-",
          input$selectParameter, "-",
          ifelse(is.null(ranges$x[1]), min(DataUse()$date), ranges$x[1]), "-",
          ifelse(is.null(ranges$x[2]), min(DataUse()$date), ranges$x[2]),
          "-timeseries.png", sep = "")},
        content = function(file) {
          ggsave(plotInput(), file = file)
        })
    }
  })
})

options(warn = 0)