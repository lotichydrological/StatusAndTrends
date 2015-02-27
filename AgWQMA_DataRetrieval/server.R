source('data/01_DataQuery.R')

agwqma <- readOGR(dsn = './data/GIS', layer = 'ODA_AgWQMA', verbose = FALSE)
# #path = 'T:/AgWQM/DataAnalysis/StatusAndTrends/GIS/ODA_AgWQMA.shp'
# #agwqma <- shapefile(x = path, stringsAsFactors = FALSE)

#Transform agwqma to same projection as points
agwqma <- spTransform(agwqma, CRS("+proj=longlat +datum=NAD83"))
#agwqma <- spTransform(agwqma, CRS("+init=epsg:2994"))

#Bring in the WBD for 8 digit HUC
HUC <- readOGR(dsn = './data/GIS', layer = 'huc250k_a_or', verbose = FALSE)
#path2 = '//deqhq1/gislibrary/base_data/hydrography/nhd/2008_Watershed_Boundary_Dataset/WBD_HUC_4th/hydrologic_units/huc250k_a_or.shp'
#HUC <- shapefile(x = path2, stringsAsFactors = FALSE)
HUC <- spTransform(HUC, CRS("+proj=longlat +datum=NAD83"))
#HUC <- spTransform(HUC, CRS("+init=epsg:2994"))
#HUC_OR <- HUC[agwqma,]

#Generate list of HUCs
# agAreas <- as.list(agwqma$PlanName)
# HUC.list <- lapply(agAreas, function(x) {HUC[agwqma[agwqma$PlanName == x,],]})
HUClist <- lapply(as.list(agwqma$PlanName),function(x) {HUC[agwqma[agwqma$PlanName == x,],]})

names(HUClist) <- agwqma$PlanName

shinyServer(function(input, output) { 
  observe({
    if (input$action_button == 0)
      return()
    isolate({
#       wqpData <- wqpQuery(planArea = input$select,
#                HUClist = HUClist,
#                inParms = input$parms,
#                startDate = input$dates[1],
#                endDate = input$dates[2]) 
#       lasarData <- lasarQuery(planArea = input$select,
#                               HUClist = HUClist,
#                               inParms = input$parms,
#                               startDate = input$dates[1],
#                               endDate = input$dates[2])
      elmData <- elementQuery(planArea = input$select,
                              HUClist = HUClist,
                              inParms = input$parms,
                              startDate = input$dates[1],
                              endDate = input$dates[2])
      #elementData <- elementQuery()
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
      output$view <- renderDataTable({
          elmData  
      })
    })
})
}
)