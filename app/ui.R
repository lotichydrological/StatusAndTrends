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
#library(xlsx)
#library(RODBC)

options(stringsAsFactors = FALSE)

#### Define Geographic Area using myArea from 01_DataQueryUI.R ####
#We want to extract only those stations in the current AgWQMA so let's bring that layer in and match the projection
agwqma <- readOGR(dsn = './data/GIS', layer = 'ODA_AgWQMA', verbose = FALSE)

shinyUI(fluidPage(
  
  titlePanel("Oregon Water Quality Status and Trend Beta Version 2.1"),
  
  mainPanel(
    HTML("<script> if (!window.chrome) { alert('For full functionality you will need to load this link in Google Chrome');} </script>"),
    tabsetPanel(
      tabPanel("Data Query", fluidRow(
        column(4, 
               radioButtons("query_area", label = h3("Geographic Area Type"),
                                  choices = c('8-digit HUC', 'ODA Agricultural Plan Area'))
        ),
        column(3,
               checkboxGroupInput("parms",label = h3("Parameter(s) to Query"),
                                  choices = c('Temperature','pH','Bacteria'),
                                  selected = 1)
        ),
        column(5,
               dateRangeInput("dates",label = h3("Start and End Dates")))
      ),
      
      fluidRow(
        column(4,
               selectInput("select",label = h3('Geographic Area'),           
                           choices = c("Choose one" = "")
                           )                       
               ),
        column(3,
               checkboxGroupInput('db', label = h3('Database(s) to Query'),
                                  c('Water Quality Portal','DEQ'),
                                  selected = 1)
        ),
        column(5,
               h3("Run Query"),
               actionButton(inputId = "action_button",label = 'Submit')
               # checkboxGroupInput('grade', label = h3("Data Quality Level (for DEQ data)"),
               #                    choices = c('A', 'B', 'C', 'E'),
               #                    selected = c('A', 'B', 'C', 'E'),
               #                    inline = TRUE)
               )
      ),
      
      # fluidRow(
      #   column(4),
      #   column(3, h3("Run Query"),
      #          actionButton(inputId = "action_button",label = 'Submit')
      # )
      # ),
      
      fluidRow(
        column(12,
               h3(" "),
               htmlOutput("text1"),
               h3(" "),
               htmlOutput("text2")
        )
      ),
      
      fluidRow(
        column(6,
               tableOutput('all_totals')),
        column(3, 
#                conditionalPanel(condition = "output.text2 == ''",
#                                 "Click here to download the data",
#                                 downloadButton('downloadData','Download')
                                uiOutput('downloadData')
               # )
               ),
        column(3,
               uiOutput("action_button_map")
               )
      )
      
      ,

      uiOutput("mymap")
    ),
    tabPanel("Review Data", 
             fluidRow(column(3,
                             uiOutput('review_control'),
                             uiOutput('wq_lim_link'),
                             uiOutput('Note_text')
             ),
             column(9,
                    DT::dataTableOutput("display")
             )
             )
             ),
    tabPanel("Plot Status and Trend", fluidRow(
      column(3,
             uiOutput('selectStation'),
             br(),
             uiOutput('selectParameter'),
             br(),
             uiOutput('selectLogScale'),
             uiOutput('plotTrend'),
             uiOutput('selectpHCrit'),
             uiOutput('selectSpawning'),
             br(),
             uiOutput('selectUse'),
             br(),
             uiOutput('selectRange'),
             br(),
             uiOutput('fish_use_link')
            ),
      column(9,
             conditionalPanel(condition = "input.selectParameter",
                              renderText("ts_plot_text")),
             conditionalPanel(condition = "input.selectParameter",
                              plotOutput('ts_plot', dblclick = "plot1_dblclick",
                                        brush = brushOpts(
                                          id = "plot1_brush",
                                          resetOnNew = TRUE
                                        )
                              )),
                              #plotOutput("ts_plot"))
             conditionalPanel(
               condition = "input.selectParameter",
               downloadButton(outputId = "downloadPlot", label = "Save plot")
               ),
             br(),
             conditionalPanel(condition = "input.selectParameter",
                              DT::dataTableOutput("exceed_df"))
             )
             )
      )
      )
   )
  )
)