source('data/01_DataQuery.R')

agwqma <- readOGR(dsn = './data/GIS', layer = 'ODA_AgWQMA', verbose = FALSE)
# #path = 'T:/AgWQM/DataAnalysis/StatusAndTrends/GIS/ODA_AgWQMA.shp'
# #agwqma <- shapefile(x = path, stringsAsFactors = FALSE)
#For testing purposes
#agwqma <- readOGR(dsn = 'AgWQMA_DataRetrieval/data/GIS', layer = 'ODA_AgWQMA', verbose = FALSE)

#Transform agwqma to same projection as points
agwqma <- spTransform(agwqma, CRS("+proj=longlat +datum=NAD83"))
#agwqma <- spTransform(agwqma, CRS("+init=epsg:2994"))

#Bring in the WBD for 8 digit HUC
HUC <- readOGR(dsn = './data/GIS', layer = 'WBD_HU8', verbose = FALSE)
#path2 = '//deqhq1/gislibrary/base_data/hydrography/nhd/2008_Watershed_Boundary_Dataset/WBD_HUC_4th/hydrologic_units/WBD_HU8.shp'
#HUC <- shapefile(x = path2, stringsAsFactors = FALSE)
#For testing purposes
#HUC <- readOGR(dsn = 'AgWQMA_DataRetrieval/data/GIS', layer = 'WBD_HU8', verbose = FALSE)
HUC <- spTransform(HUC, CRS("+proj=longlat +datum=NAD83"))
#HUC <- spTransform(HUC, CRS("+init=epsg:2994"))
#HUC_OR <- HUC[agwqma,]

#Generate list of HUCs
# agAreas <- as.list(agwqma$PlanName)
# HUC.list <- lapply(agAreas, function(x) {HUC[agwqma[agwqma$PlanName == x,],]})
HUClist <- lapply(as.list(agwqma$PlanName),function(x) {HUC[agwqma[agwqma$PlanName == x,],]})

names(HUClist) <- agwqma$PlanName

#For testing purposes set up input 
# input <- list(action_button = c(0))
# input$action_button <- 1
# input$parms <- c('Bacteria')
# input$select <- 'North Coast'
# input$dates <- c("2005-01-01", "2015-04-22")
# input$db <- c("Water Quality Portal", "Element", "LASAR")

shinyServer(function(input, output) { 
  observe({
    if (input$action_button == 0)
      return()
    isolate({
      if (is.null(input$parms)) {
        output$text1 <- renderText("Please refresh the page and remember to select a parmaeter to query")
      } else {
        wL <- FALSE
        lL <- FALSE
        eL <- FALSE
        wqpData <- ""
        lasarData <- ""
        elmData <- ""      
        
        if ('Water Quality Portal' %in% input$db) {
          wqpData <- tryCatch(wqpQuery(planArea = input$select,
                                       HUClist = HUClist,
                                       inParms = input$parms,
                                       startDate = input$dates[1],
                                       endDate = input$dates[2]),error = function(err) {err <- geterrmessage()})
          wL <- ifelse(is.data.frame(wqpData),ifelse(nrow(wqpData) > 0,TRUE,FALSE),FALSE)
          if (!is.data.frame(wqpData)) {
            all <- 'Water Quality Portal is busy. Please try again in a few minutes.'
          } 
        }
        if ('LASAR' %in% input$db) {
          lasarData <- lasarQuery(planArea = input$select,
                                  HUClist = HUClist,
                                  inParms = input$parms,
                                  startDate = input$dates[1],
                                  endDate = input$dates[2])
          lL <- ifelse(is.data.frame(lasarData),ifelse(nrow(lasarData) > 0,TRUE,FALSE),FALSE) 
        }
        if ('Element' %in% input$db) {
          elmData <- elementQuery(planArea = input$select,
                                  HUClist = HUClist,
                                  inParms = input$parms,
                                  startDate = input$dates[1],
                                  endDate = input$dates[2])
          eL <- ifelse(is.data.frame(elmData),ifelse(nrow(elmData) > 0,TRUE,FALSE),FALSE)
        }
        
        if(wL) {
          if (lL) {
            if (eL) {
              all <- tryCatch(combine(E=elmData,L=lasarData,W=wqpData),error = function(err) {err <- geterrmessage()})
            } else {
              all <- tryCatch(combine(L=lasarData,W=wqpData),error = function(err) {err <- geterrmessage()})
            } 
          } else if (eL) {
            all <- tryCatch(combine(E=elmData,W=wqpData),error = function(err) {err <- geterrmessage()})
          } else {
            all <- tryCatch(combine(W=wqpData),error = function(err) {err <- geterrmessage()})
          }
        } else if (lL) {
          if (eL) {
            all <- tryCatch(combine(E=elmData,L=lasarData),error = function(err) {err <- geterrmessage()})
          } else {
            all <- tryCatch(combine(L=lasarData),error = function(err) {err <- geterrmessage()})
          }
        } else if (eL) {
          all <- tryCatch(combine(E=elmData),error = function(err) {err <- geterrmessage()})
        } else {
          all <- 'Your query returned no data'
        }
        
        
        if(!is.data.frame(all)) {
          output$text1 <- renderText({all})
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
            ifelse(is.data.frame(all),"Download the data in .csv format using the button below",0)
          })
          
          output$view <- renderTable({
            attr(all, 'totals')
          })
          
          output$downloadData <- downloadHandler(
            filename = function() {
              paste(input$select, "_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv",sep='')
            },
            content = function(file) {
              write.csv(all, file, row.names = FALSE)
            }
          )
        }
      }
      

      
    })

})
})