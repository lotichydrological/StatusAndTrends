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
  
  titlePanel("Oregon Ag Water Quality Biennial Review"),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Data Query", fluidRow(
        column(3, 
               selectInput("select",label = h3('Select Plan Area'),
                           #choices = list())                  
                           choices = sort(agwqma$PlanName))
        ),
        column(3,
               checkboxGroupInput("parms",label = h3("Select Paramters to Query"),
                                  choices = c('Temperature','pH','Bacteria'),
                                  selected=1)
        ),
        column(3,
               dateRangeInput("dates",label = h3("Select the Start and End Dates")))
      ),
      
      fluidRow(
        column(3,
               checkboxGroupInput('db','Select Database(s) to Query:',
                                  c('Water Quality Portal','LASAR','Element'),select = c('Water Quality Portal','LASAR','Element'))),
        column(3,
               h3("Run Query"),
               actionButton(inputId = "action_button",label = 'Submit'))
      ),
      
      fluidRow(
        column(3),
        column(3,
               conditionalPanel(
                 condition = "input.action_button == 1",
                 "Please be patient. This will take awhile."
               ))),
      
      fluidRow(
        column(3),
        column(3,
               h3(" "),
               verbatimTextOutput("text1")
        )
      ),
      
      fluidRow(
        column(3, 
               verbatimTextOutput("isdf"),
               tableOutput('view')),
        column(3, 
               conditionalPanel(condition = "output.isdf=='Results returned'",
                                "Click here to download the data",
                                downloadButton('downloadData','Download')
               )
               ),
        column(3,
               conditionalPanel(
                 condition = "output.isdf=='Results returned'",
                 'Click here to view map',
                 actionButton(inputId = 'action_button2',label = 'View map')
               )
      )
      
 
      ),

      conditionalPanel(condition = "input.action_button2 == 1",
                       uiOutput("mymap"))
    ),
    tabPanel("Review Data", fluidRow(column(3,
                                            conditionalPanel(condition = "output.isdf == 'Results returned"),
                                            selectInput("ReviewDf",'Select Review table to view:',choices = list("Summary by organization" = 'df.summary',
                                                                                                       "Result values modified" = "df.cleaned",
                                                                                                       "Data removal information" = "df.removal",
                                                                                                       "Unique comment values" = 'df.Comment',
                                                                                                       "Data in tabular format" = 'df.sub'),selectize = TRUE
                                                        )),
                                     column(9,
                                            conditionalPanel(condition = "output.isdf == 'Results returned'",
                                               dataTableOutput("display")
                                             ))
    )
  ),
  tabPanel("Plot Status and Trend", fluidRow(
    column(3,
           conditionalPanel(condition = "output.isdf = 'Results returned'"),
           selectInput("selectStation","Select station to evaluate:",
                       "",selectize = TRUE),
    br(),
#         selectInput("selectParameter",'Select parameter to evaluate:',
#                     "",selectize=TRUE)    
        uiOutput('selectParameter')
        ),
column(9,
       plotOutput("datatable"))
  ))
 )
)
)
)