library(shiny)
#Use this to make it accessible for other people to access
#runApp("app",host="0.0.0.0",port=3168)
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
library(reshape2)
library(ggplot2)
library(zoo)
library(spatialEco)
library(dplyr)
library(lubridate)
library(leaflet)
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
hucs <- readOGR(dsn = './data/GIS', layer = 'WBD_HU8', verbose = FALSE)
#Bring in the HUC list mapped to overlapping ag plan areas
#This process would not produce desired results (only those HUCs wholly contained by the plan area)
#When done through the use of intersection and overlay tools provided in the sp and rgdal packages
#Therefore, a manual identificiation of the desired HUCs for each Ag plan area was completed
HUClist <- read.csv('data/PlanHUC_LU.csv')

#LASAR Stations don't all have HUC8 values in the Area Abbreviation table so we Ryan
#did the GIS to get them based on the LASAR database lat/lon for each station
stations_huc <- read.csv('data/station_wbd_12132016.csv')

#Table of ph criteria for lookup
ph_crit <- read.csv('data/PlanOWRDBasinpH_LU.csv')
ph_crit <- merge(ph_crit, HUClist, by.x = 'plan_name', by.y = 'PlanName', all.x = TRUE)

#The entire list of WQP parameters that match to a criteria
parms <- read.csv('data/WQP_Table3040_Names.csv', stringsAsFactors = FALSE)

#Pre-extracted 303(d) with PlanName added
wq_limited <- read.csv('data/GIS/wq_limited_df_temp_bact_ph_DO_2012.csv')

# Need to bring in the NLCD and OR catchments for getting associated StreamCat Land Use summary
# load('data/NLCD2011_OR.Rdata')
# load('data/OR_cats.Rdata')

shinyServer(function(input, output, session) { 
  ###################################
  ###################################
  ####### Data Query Tab ###########
  ###################################
  ###################################
  
  #
  observe({
    if (input$query_area == '8-digit HUC') {
      area_choices <- c("Choose one" = "", sort(unique(paste(hucs$HUC_8, "-", 
                                                             hucs$HU_8_NAME))))
    } else {
      area_choices <- c("Choose one" = "", sort(agwqma$PlanName))
    }
    
    updateSelectInput(session, "select", choices = area_choices)
  })
  
  #First check for all required inputs and provide a place for printing the text
  #since the result of the button isn't directly a single table or chart
  input_check_text <- eventReactive(input$action_button, {
    if (input$select == "") {
      "Please select a Geographic Area"
    } else if (is.null(input$parms)) {
      "Please select a parmaeter to query"
    } else if (input$dates[1] == input$dates[2]) {
      "Please select non-identical dates"
    } else if (is.null(input$db)) {
      "Please select a database to query"
    } else {
      HTML(paste0("You just submitted ", 
                  input$select, 
                  " Geographic Area Query for ", 
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
          
          if (any(c('Temperature', 'pH', 'Dissolved Oxygen', 'Total Suspended Solids', 'Total Phosphorus') %in% input$parms)) {
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
                                  endDate = input$dates[2],
                                  stations_wbd = stations_huc)
          odbcCloseAll()
          if (nrow(lasarData) == 0) lasarData <- NULL
          
          
          incProgress(1/10, detail = 'Querying the Element database')
          prog <- prog + 1/10
          
          elmData <- elementQuery(planArea = input$select,
                                  HUClist = HUClist,
                                  inParms = input$parms,
                                  startDate = input$dates[1],
                                  endDate = input$dates[2],
                                  stations_wbd = stations_huc)
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
          #Perform sufficiency analysis for temperature
          if ('Temperature' %in% input$parms) {
            temp_stns_pass <- temp_sufficiency_analysis(df.all)
          } else {
            temp_stns_pass <- data.frame()
            attr(temp_stns_pass, "year_test") <- data.frame(Station_ID="none")
          }
          
          #Generate sdadm once for temperature plotting/exceedance use
          if (any('Temperature' %in% df.all$Analyte)) {
            sdadm <- Calculate.sdadm(df.all, "Result", "Station_ID", "Sampled",
                                     '%Y-%m-%d %H:%M:%S')
          } else {
            sdadm <- NULL
          }
          
          #Run Seasonal Kendall for pH and Bacteria
          if (any(c('pH', 'E. Coli', "Enterococcus", 'Dissolved Oxygen', 'Total Suspended Solids', 'Total Phosphorus') %in% df.all$Analyte)) {
            SeaKen <- run_seaKen(df.all)
          } else {
            SeaKen <- data.frame()
          }
          
          lstSummaryDfs[[4]] <- SeaKen
          names(lstSummaryDfs)[4] <- "sea_ken_table"
          
          #Calculate 30 GM for E. Coli and Enterococcus
          #WIll get to this later. May need to go in plotting section.
          
          #### Performing QA Screen ####
          incProgress(1/10, detail = "Performing QA Screen")
          prog <- prog + 1/10
          #Check QA info and remove data not meeting QA objectives
          df.all <- remove_QAfail(df.all)
          #Pull out the tracking data frame of data removed
          lstSummaryDfs[[5]] <- attr(df.all, "removal_tracking")
          names(lstSummaryDfs)[5] <- "df.removal"
          
          #### Preparing data for mapping ####
          incProgress(1/10, detail = "Preparing data for mapping")
          prog <- prog + 1/10
          #Generate layer for mapping
          all.sp <- generateStnLyrToPlot(df.all, lstSummaryDfs[["df.station.totals"]])
          
          #Restrict ag plan areas to select plan area
          if (!grepl("[0-9].", input$select)) {
            ag_sub <- agwqma[agwqma$PlanName == input$select,]
            ag_sub <- spTransform(ag_sub, CRS("+init=epsg:4269"))
            
            #Restrict layer for mapping to just the selected plan area
            all.sp <- all.sp[ag_sub,]
          } else {
            huc_sub <- hucs[hucs$HUC_8 == strsplit(input$select, 
                                                   split = " - ")[[1]][1],]
            huc_sub <- spTransform(huc_sub, CRS("+init=epsg:4269"))
            
            #Restrict layer for mapping to just the selected plan area
            #all.sp <- all.sp[huc_sub,]
          }
          
          incProgress(1/10, detail = "Extracting 303(d) listed segments")
          prog <- prog + 1/10
          #Extract 303(d) segments in the plan area for parameters
          #returned in the query
          wq_lim_whole <- extract_303d(df.all, wq_limited, input$select)
          lstSummaryDfs[[6]] <- wq_lim_whole[,c('Stream_Lak', 'LLID_Strea',
                                                'Miles', 'Pollutant', 'Season',
                                                'Assessme_1', 'Criteria',
                                                'Listing_St')]
          lstSummaryDfs[[6]] <- plyr::rename(lstSummaryDfs[[6]],
                                             c('Stream_Lak' = 'Waterbody',
                                               'LLID_Strea' = 'LLID',
                                               'Assessme_1' = 'Year Assessed',
                                               'Listing_St' = 'Listing Status'))
          names(lstSummaryDfs)[6] <- "wq_limited"
          
          incProgress(1/10, detail = "Performing Land Use Analysis")
          prog <- prog + 1/10
          #Pull in Stream Cat data for NLCD 2011 land use
          #these two lines cannot be commented if you want the land use analysis to run
          # stn_nlcd_df <- landUseAnalysis(all.sp, cats, NLCD2011)
          # lstSummaryDfs[[7]] <-stn_nlcd_df
          #This line has to be commented out if you want the land use analysis to run
          
          lstSummaryDfs[[7]] <- data.frame()
          names(lstSummaryDfs)[[7]] <- 'stn_nlcd_df'
          
          lstSummaryDfs[[8]] <- Stations_Status(df.all)
          names(lstSummaryDfs)[8] <- "Stations_Status"
          
          if(lstSummaryDfs[8] != 'No stations meet Status criteria') {
            lstSummaryDfs[[8]] <- lstSummaryDfs[[8]]
          } else {
            as.data.frame('No stations meet Status criteria')
          }
          
          lstSummaryDfs[[9]] <- Stations_Trend(df.all)
          names(lstSummaryDfs)[9] <- "Stations_Trend"
          
          if(nrow(lstSummaryDfs[[9]]) == 0) {
            lstSummaryDfs[[9]] <- as.data.frame('No stations meet criteria to assess trends')
          } else {
            lstSummaryDfs[[9]] <- lstSummaryDfs[[9]]
          }
          
          lstSummaryDfs[[10]] <- All_stns_fit_Criteria(trend = lstSummaryDfs[[9]], 
                                                       status = lstSummaryDfs[[8]],
                                                       df.all = df.all)
          names(lstSummaryDfs)[10] <- "stns"
          
          
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
        output$mymap <- renderLeaflet({
          basinMap <- leaflet(options = leafletOptions(maxZoom = 18)) %>% 
            addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
            addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
            addWMSTiles(GetURL("USGSTopo"), 
                        attribution = paste0("<a href='https://www.usgs.gov/'>",
                                             "U.S. Geological Survey</a> | ",
                                             "<a href='https://www.usgs.gov/laws/policies_notices.html'>",
                                             "Policies</a>"),
                        group = "USGS Topo", layers = "0") %>%
            addWMSTiles(GetURL("USGSHydroCached"), 
                        group = "Hydrography", 
                        options = WMSTileOptions(format = "image/png", 
                                                 transparent = TRUE),
                        layers = "0") %>%
            hideGroup("Hydropgraphy") %>%
            addMarkers(data = all.sp, 
                       lng = all.sp@coords[,1], 
                       lat = all.sp@coords[,2], 
                       popup = paste("<style> div.leaflet-popup-content {width:auto !important;}</style>",
                                     lapply(rownames(all.sp@data), 
                                      function(row) {
                                        htmlTable::htmlTable(all.sp@data[row,],
                                                             header = c('Stn_ID', 'Stn_Name',
                                                                        names(all.sp@data)[-c(1,2)]),
                                                             rnames = FALSE)
                                        })),
                       group = 'Stations',
                       clusterOptions = markerClusterOptions())
          if (grepl("[0-9].", input$select)) {
            basinMap <- basinMap %>% addPolygons(data = huc_sub, 
                                                 stroke = FALSE, 
                                                 fillOpacity = 0.05, 
                                                 smoothFactor = 0.5, 
                                                 fillColor = topo.colors(13, alpha = NULL), 
                                                 popup = huc_sub@data$HU_8_NAME,
                                                 group = "Area")
          } else {
            basinMap <- basinMap %>% addPolygons(data = ag_sub, 
                                                 stroke = FALSE, 
                                                 fillOpacity = 0.05, 
                                                 smoothFactor = 0.5, 
                                                 fillColor = topo.colors(13, alpha = NULL), 
                                                 popup = ag_sub@data$PlanName,
                                                 group = "Area")
          }
          basinMap <- basinMap %>% addLayersControl(
            baseGroups = c('Terrain', 'Satellite', "USGS Topo"),
            overlayGroups = c('Stations', 'Area', 'Hydrography'),
            options = layersControlOptions(collapsed = FALSE)
          )
          basinMap
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
                                   "WQ Limited Waters within Geographic Area" = 
                                     'wq_limited',
                                   'Land Use Breakdown' = 'stn_nlcd_df',
                                   'Stations that Meet Status' = 'Stations_Status',
                                   'Stations that Meet Trend' = 'Stations_Trend',
                                   'All Stations that Meet Criteria' = 'stns',
                                   'Summary of All Stations by Year' = NULL,
                                   "Seasonal Kendall Results" = 'sea_ken_table',
                                   "QA - Summary by organization" = 'df.org',
                                   "QA - Result values modified" = "df.report",
                                   "QA - Data removal information" = "df.removal",
                                   "QA - Unique comment values" = 'df.comment'),
                    
                    selectize = TRUE
        )
      })
    
      #The table is built here
      tbl_disp_input <- reactive({
        validate(
          need(input$ReviewDf != "", message = FALSE)
        )
        tbl_disp <- pickReviewDf(input_reviewDf = input$ReviewDf, 
                                 lstSummaryDfs, df.all)
      })
      
      output$display <- DT::renderDataTable({
        tbl_disp <- tbl_disp_input()
        if ("Sampled" %in% names(tbl_disp)) {
          tbl_disp <- datatable(tbl_disp, 
                                filter = "top", 
                                selection = "none") %>% 
            formatDate("Sampled", 'toLocaleString')
        }
        tbl_disp            
      }, options = list(processing = FALSE), 
      filter = 'top', selection = "single", server = FALSE) 
      
      output$wq_lim_link <- renderUI({
        validate(
          need(input$ReviewDf == 'wq_limited', message = FALSE)
        )
        
        s = input$display_rows_selected
        
        if (length(s)) {
          link <- paste0('http://www.deq.state.or.us/wq/assessment/rpt2012/results.asp?txtLlid=',
                         wq_lim_whole[s, "LLID_Strea"], "&cboParameters=",
                         wq_lim_whole[s, "Pollutant_"], "&Status=10")
          h5(a("Click here to view on the WQ Assessment Web Page",
               href = link,
               target = "_blank"))
        }
      })
      
      output$Note_text <- renderUI({
        validate(
          need(input$ReviewDf == 'wq_limited', message = FALSE)
        )
        
        s = input$display_rows_selected
        
        if (length(s)) {
          h5("Make sure to click 'New Search' before you leave that page in order for new selections from this page to update as expected.")
        }
        
      })
      
      
      # #This creates the button for downloading the raw formatted data
      output$dlReviewTab <- downloadHandler(
        filename = function() {
          paste(input$select, "_", input$ReviewDf, ".csv",sep='')
        },
        content = function(file) {
          write.csv(tbl_disp_input(), file, row.names = FALSE)
        }
      )
      
      
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
      
      
      ##TSS INPUT ALLOCATION VALUE
      output$value <- renderUI({ 
        validate(
          need(input$selectParameter %in% c('Total Suspended Solids'),
               message = FALSE)
        )
         numericInput("selectWQSTSS", "TSS Allocation:", 0, min = 0, max = 100)
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
                                           input$select | ph_crit$HUC8 == 
                                           strsplit(input$select, 
                                                    split = " - ")[[1]][1],
                                         c('OWRD_basin')],
                                 ph_crit[ph_crit$plan_name == 
                                           input$select | ph_crit$HUC8 == 
                                           strsplit(input$select, 
                                                    split = " - ")[[1]][1],
                                         c('ph_standard')],
                                 sep = " - ")
        selectInput('selectpHCrit',
                    "Select applicable OWRD Basin specific pH criteria:",
                    choices = ph_crit_choices,
                    selectize = TRUE,
                    selected = NULL)
      })
      
      output$plotTrend <- renderUI({
        validate(
          need(input$selectParameter %in% c('pH', 'E. Coli', 'Enterococcus', 'Total Suspended Solids', 'Dissolved Oxygen'), 
               message = FALSE)
        )
        checkboxInput("plotTrend", 
                      "Plot Seasonal Kendall trend line 
                      (Note: May not be significant)")
      })
      
      output$selectSpawning = renderUI({
        validate(
          need(input$selectParameter %in% c('Temperature', 'Dissolved Oxygen'), message = FALSE)
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

      output$selectMonth <- renderUI({
        validate(
          need(input$selectParameter == 'Temperature', message = FALSE)
        )
        validate(
          need(unique(
            strsplit(input$selectStation,
                     ' - ')[[1]][1]) %in% attr(temp_stns_pass, 
                                               'year_test')$Station_ID,
            message = FALSE)
        )
        
        selectInput('selectMonth', label = "Select month for trend analysis",
                    choices = month.name[
                      attr(temp_stns_pass, "year_test")[
                        attr(temp_stns_pass, "year_test")$Station_ID == unique(
                          strsplit(input$selectStation,
                                   ' - ')[[1]][1]) & 
                          attr(temp_stns_pass, "year_test")$result == 'pass', 'month']],
                    selectize = TRUE)
      })
      
      output$selectUseDO = renderUI({
        validate(
          need(input$selectParameter == 'Dissolved Oxygen', message = FALSE)
        )
        selectInput('selectUseDO',"Select Aquatic fish use:",
                    choices = c('Cold-Water Aquatic Life',
                                'Cool-Water Aquatic Life',
                                'Warm-Water Aquatic Life',
                                'Estuarine Waters'),
                    selectize = TRUE)
      })
      
      output$fish_use_link <- renderUI({
        validate(
          need(input$selectParameter %in% c('Temperature','Dissolved Oxygen'), 
               message = FALSE)
        )
        h5(a("Refer to Spawning Use Maps by Basin", 
             href = "https://www.oregon.gov/deq/Regulations/Pages/Administrative-Rules.aspx",
             target = "_blank"))
      })
      
      # output$checkSpawning <- renderUI({
      #   validate(
      #     need(input$selectParameter %in% c('Dissolved Oxygen'), 
      #          message = FALSE)
      #   )
      #   checkboxInput(inputId = "checkSpawning",
      #                 label = "Spawning",
      #                 value = FALSE)
      # })
      
      output$fish_use_link_DO <- renderUI({
        validate(
          need(input$selectParameter == 'Dissolved Oxygen', message = FALSE)
        )
        h5(a("Refer to Memo for Aquatic Life Designations", 
             href = "http://www.deq.state.or.us/wq/standards/docs/MemoDOCriteria20100608.pdf",
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
              need(!is.null(input$selectpHCrit) & input$selectpHCrit != "", 
                   message = FALSE)
            )
          } else if (input$selectParameter == 'E. Coli' |
                     input$selectParameter == 'Enterococcus') {
            validate(
              need(!is.null(input$selectLogScale), message = FALSE)
            )
          } else if (input$selectParameter == 'Dissolved Oxygen') {
            validate(
              need(!is.null(input$selectUseDO), message = FALSE)
            )
          } else if (input$selectParameter == "Total Suspended Solids") {
            validate(
              need(!is.null(input$selectWQSTSS), message = FALSE)
            )
          }
        }
        
        if (input$selectParameter %in% c('pH', 'E. Coli', 'Enterococcus', 'Total Suspended Solids', 'Dissolved Oxygen')) {
          tmp_df <- DataUse()
          tmp_df$day <- substr(tmp_df$Sampled, 1, 10)
          tmp_df$code <- paste(tmp_df$Station_ID, tmp_df$Analyte, tmp_df$day)
          sub <- with(tmp_df, resolveMRLs(code, Detect, Result))
          tmp_df_MRL <- tmp_df[sub,]
          if (input$selectParameter == 'Dissolved Oxygen'){
            tmp_df <- remove.dups(tmp_df, min)
          } else {
            tmp_df <- remove.dups(tmp_df_MRL, max)
          }
        } else {
          tmp_df <- DataUse()
        }
        
        generate_exceed_df(new_data = tmp_df,
                           df.all = df.all,
                           parm = input$selectParameter,
                           selectpHCrit = input$selectpHCrit,
                           ph_crit = ph_crit,
                           PlanName =  input$select,
                           selectStation =  input$selectStation,
                           selectSpawning = input$selectSpawning,
                           selectUse = input$selectUse,
                           selectUseDO = input$selectUseDO,
                           selectWQSTSS = input$selectWQSTSS)
      })
      
      # This handles the temp trend analysis
      DataUse_temp <- reactive({
        generate_temp_data(DataUse(),
                           input$selectSpawning,
                           input$selectUse,
                           input$selectMonth)
      })
      
      #This generate the temp trend plot
      output$temp_trend_plot <- renderPlot({
        validate(
          need(input$selectStation != '', message = FALSE)
        )
        validate(
          need(input$selectParameter == "Temperature", message = FALSE)
        )
        validate(
          need(input$selectMonth != "", message = FALSE)
        )
        validate(
          need(!is.null(input$selectUse), message = FALSE)
        )
        Temp_trends_plot(DataUse_temp(), input$selectStation, input$selectMonth)
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
                 df$Sampled <- as.POSIXct(strptime(df$Sampled, format = "%Y-%m-%d %H:%M:%S"))
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
                 df$Sampled <- as.POSIXct(strptime(df$Sampled, format = "%Y-%m-%d %H:%M:%S"))
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
                   g <- g + coord_trans(y = "log10", limx = ranges$x, limy = ranges$y)
                 } else {
                   g <- g + coord_cartesian(xlim = ranges$x, ylim = ranges$y)
                 }
               }),
               "Enterococcus" = ({
                 df$Sampled <- as.POSIXct(strptime(df$Sampled, format = "%Y-%m-%d %H:%M:%S"))
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
                   g <- g + coord_trans(y = "log10", limx = ranges$x, limy = ranges$y)
                 } else {
                   g <- g + coord_cartesian(xlim = ranges$x, ylim = ranges$y)
                 }
               }),
               "Dissolved Oxygen" = ({
                 df$Sampled <- as.POSIXct(strptime(df$Sampled, format = "%Y-%m-%d %H:%M:%S"))
                 if (nrow(df) > 2) {
                   g <- plot.DO(new_data = df,
                                df.all = df.all,
                                sea_ken_table = SeaKen,
                                plot_trend = input$plotTrend,
                                selectUseDO = input$selectUseDO,
                                selectSpawning = input$selectSpawning,
                                analyte_column = 'Analyte',
                                station_id_column = 'Station_ID',
                                station_desc_column = 'Station_Description',
                                datetime_column = 'Sampled',
                                result_column = 'Result',
                                datetime_format = '%Y-%m-%d %H:%M:%S',
                                parm = 'Dissolved Oxygen')
                 } else {
                   g <- ggplot(data.frame()) + geom_point() + 
                     annotate("text", label = "Insufficient data for plotting", 
                              x = 1, y = 1)
                 }
                 g <- g + coord_cartesian(xlim = ranges$x, ylim = ranges$y)
               }),
               "Total Suspended Solids" = ({
                 df$Sampled <- as.POSIXct(strptime(df$Sampled, format = "%Y-%m-%d %H:%M:%S"))
                 if (nrow(df) > 2) {
                   g <- plot.TSS(new_data = df,
                                 df.all = df.all,
                                 selectWQSTSS = input$selectWQSTSS,
                                 sea_ken_table = SeaKen,
                                 plot_trend = input$plotTrend,
                                 analyte_column = 'Analyte',
                                 station_id_column = 'Station_ID',
                                 station_desc_column = 'Station_Description',
                                 datetime_column = 'Sampled',
                                 result_column = 'Result',
                                 datetime_format = '%Y-%m-%d %H:%M:%S',
                                 parm = 'Total Suspended Solids (mg/l)')
                 } else {
                   g <- ggplot(data.frame()) + geom_point() + 
                     annotate("text", label = "Insufficient data for plotting", 
                              x = 1, y = 1)
                 }
                 g <- g + coord_cartesian(xlim = ranges$x, ylim = ranges$y)
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
              need(!is.null(input$selectpHCrit) & input$selectpHCrit != "", 
                   message = FALSE)
            )
          } else if (input$selectParameter == 'E. Coli' | 
                     input$selectParameter == 'Enterococcus' ) {
            validate(
              need(!is.null(input$selectLogScale), message = FALSE)
            )
          } else if (input$selectParameter == 'Dissolved Oxygen') {
            validate(
              need(!is.null(input$selectUseDO), message = FALSE)
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
          ggsave(plotInput(), file = file, height = 8, width = 8)
        })
      }
  })
  })

options(warn = 0)
