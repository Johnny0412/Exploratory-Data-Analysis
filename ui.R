library(shiny)
library(shinyjs)
library(DT)
library(shinycssloaders)
library(corrgram)
library(ggplot2)
library(RColorBrewer)

shinyUI(
    fluidPage(
        useShinyjs(),
        titlePanel("Exploratory Data Analysis"),
        tabsetPanel(
            tabPanel("Data Overview",
                     
                     tabsetPanel(
                         tabPanel("Summary",
                                  verbatimTextOutput(outputId = "DataSummary")
                         ),
                         tabPanel("Data Structure",
                                  verbatimTextOutput(outputId = "DataStructure")
                         ),
                         tabPanel("Raw Data",
                                  DT::dataTableOutput(outputId = "RawData")
                         ),
                         tabPanel("Data Missingness",
                                  h3("Missingness of Data", align = "center"),
                                  withSpinner(
                                      plotOutput(outputId = "Missing")
                                  ),
                                  checkboxInput(inputId = "sort", label = "Sort", value = TRUE),
                                  checkboxInput(inputId = "cluster", label = "Cluster", value = FALSE)
                                  
                         )
                     )
            ),
            
            tabPanel("Numeric Features",
                     
                     tabsetPanel(
                         tabPanel("Rising Value",
                                  hr(),
                                  selectInput(inputId = "NumVariable", label = "Select a numeric variable", 
                                              choices = list("Y", "ID", "survey1", "survey2", "survey3", "survey4", "survey5", "survey6", "survey7", "survey8", "survey9", "survey10", "survey11", 
                                                             "survey12", "survey13", "survey14", "survey15", "survey16", "survey17", "survey18", "survey19", "survey20", "survey21", "survey22", 
                                                             "survey23", "survey24", "survey25", "survey26", "survey27", "survey28", "survey29", "survey30"), selected= "Y"),
                                  h3("Rising value chart", align = "center"),
                                  plotOutput(outputId = "RiseValue"),
                                  checkboxInput(inputId = "center", label = "Center", value = FALSE),
                                  checkboxInput(inputId = "scale", label = "Scale", value = FALSE)
                                  
                         ),
                         tabPanel("Homogeneity Plot",
                                  hr(),
                                  h3("Homogeneity Plot", align = "center"),
                                  plotOutput(outputId = "Homogeneity"),
                                  checkboxInput(inputId = "center2", label = "Center", value = TRUE),
                                  checkboxInput(inputId = "scale2", label = "Scale", value = TRUE)
                         ),
                         tabPanel("Histogram",
                                  selectInput(inputId = "NumVariable2", label = "Select a numeric variable", 
                                              choices = list("Y", "ID", "survey1", "survey2", "survey3", "survey4", "survey5", "survey6", "survey7", "survey8", "survey9", "survey10", "survey11", 
                                                             "survey12", "survey13", "survey14", "survey15", "survey16", "survey17", "survey18", "survey19", "survey20", "survey21", "survey22", 
                                                             "survey23", "survey24", "survey25", "survey26", "survey27", "survey28", "survey29", "survey30"), selected= "Y"),
                                  sliderInput(inputId = "bin", label = "Number of bins", min = 50, max = 400, step = 50, value = 50),
                                  h3("Distribution", align = "center"),
                                  plotOutput(outputId = "Histogram")
                         ),
                         tabPanel("Box-plots",
                                  withSpinner(
                                      plotOutput(outputId = "BoxPlot")
                                  ),
                                  checkboxInput(inputId = "standardise", label = "Show standardized", value = FALSE),
                                  checkboxInput(inputId = "outliers", label = "Show outliers", value = FALSE),
                                  sliderInput(inputId = "range", label = "IQR Multiplier", min = 0, max = 20, step = 1, value = 5)
                         ),
                         tabPanel("Correlation",
                                  withSpinner(
                                      plotOutput(outputId = "Correlation")
                                  ),
                                  checkboxInput(inputId = "abs", label = "Uses absolute correlation", value = TRUE),
                                  selectInput(inputId = "CorrMeth", label = "Correlation method", choices = c("pearson","spearman","kendall"), selected = "pearson"),
                                  selectInput(inputId = "Group", label = "Grouping method", choices = list("none" = FALSE,"OLO" = "OLO","GW" = "GW","HC" = "HC"), selected = "OLO")
                         )
                     )
            ),

        
            tabPanel("Categorical Features",
                     tabsetPanel(
                         tabPanel("Pie Chart",
                                  hr(),
                                  selectInput(inputId = "CatVariable", label = "Select a categorical variable", 
                                              choices = list("Supervisor", "Priority", "Price", "Speed", "Duration", "Temp", "Location", "Agreed", "State", "Class", "Surface"), selected= "Y"),
                                  verbatimTextOutput(outputId = "table"),
                                  withSpinner(
                                      plotOutput(outputId = "Pie")
                                  )
                         ),
                         tabPanel("Mosaic",
                                  selectizeInput(inputId = "CatVariables", label = "Show variables:", 
                                                 choices = list("Supervisor", "Priority", "Price", "Speed", "Duration", "Temp", "Location", "Agreed", "State", "Class", "Surface"), 
                                                 multiple = TRUE, selected = list("Surface", "Class", "Location", "State")),
                                  withSpinner(
                                      plotOutput(outputId = "Mosaic")
                                  )
                         )
                     )
            ),
            
            tabPanel("Mixed Features",
                     tabsetPanel(
                         tabPanel("Table Plot",

                                 h3("Table Plot", align = "center"),
                                 withSpinner(
                                     plotOutput(outputId = "tableplot")
                                 )
                         ),
                         tabPanel("Pairs Plot",
                                  selectInput(inputId = "CatVariable2", label = "Colour by", 
                                              choices = list("NULL", "Supervisor", "Priority", "Price", "Speed", "Duration", "Temp", "Location", "Agreed", "State", "Class", "Surface"), selected= "NULL"),
                                  
                                  column(3, selectInput(inputId = "Pair1", label = "Variable 1", 
                                              choices = list("Y", "ID", "Supervisor", "Priority", "Price", "Speed", "Duration", "Temp", "Location", "Agreed", "State", "Class", "Surface", "survey1", "survey2", "survey3", "survey4", "survey5", "survey6", "survey7", "survey8", "survey9", "survey10", "survey11", 
                                                             "survey12", "survey13", "survey14", "survey15", "survey16", "survey17", "survey18", "survey19", "survey20", "survey21", "survey22", 
                                                             "survey23", "survey24", "survey25", "survey26", "survey27", "survey28", "survey29", "survey30"), selected= "survey1")),
                                  column(3, selectInput(inputId = "Pair2", label = "Variable 2", 
                                              choices = list("Y", "ID", "Supervisor", "Priority", "Price", "Speed", "Duration", "Temp", "Location", "Agreed", "State", "Class", "Surface", "survey1", "survey2", "survey3", "survey4", "survey5", "survey6", "survey7", "survey8", "survey9", "survey10", "survey11", 
                                                             "survey12", "survey13", "survey14", "survey15", "survey16", "survey17", "survey18", "survey19", "survey20", "survey21", "survey22", 
                                                             "survey23", "survey24", "survey25", "survey26", "survey27", "survey28", "survey29", "survey30"), selected= "survey2")),
                                  column(3, selectInput(inputId = "Pair3", label = "Variable 3", 
                                              choices = list("Y", "ID", "Supervisor", "Priority", "Price", "Speed", "Duration", "Temp", "Location", "Agreed", "State", "Class", "Surface", "survey1", "survey2", "survey3", "survey4", "survey5", "survey6", "survey7", "survey8", "survey9", "survey10", "survey11", 
                                                             "survey12", "survey13", "survey14", "survey15", "survey16", "survey17", "survey18", "survey19", "survey20", "survey21", "survey22", 
                                                             "survey23", "survey24", "survey25", "survey26", "survey27", "survey28", "survey29", "survey30"), selected= "survey5")),
                                  column(3, selectInput(inputId = "Pair4", label = "Variable 4", 
                                              choices = list("Y", "ID", "Supervisor", "Priority", "Price", "Speed", "Duration", "Temp", "Location", "Agreed", "State", "Class", "Surface", "survey1", "survey2", "survey3", "survey4", "survey5", "survey6", "survey7", "survey8", "survey9", "survey10", "survey11", 
                                                             "survey12", "survey13", "survey14", "survey15", "survey16", "survey17", "survey18", "survey19", "survey20", "survey21", "survey22", 
                                                             "survey23", "survey24", "survey25", "survey26", "survey27", "survey28", "survey29", "survey30"), selected= "survey6")),
                                  h3("Pairs Plot", align = "center"),
                                  withSpinner(
                                      plotOutput(outputId = "MixedPairs")
                                  )
                                  
                                  
                         )
                     )
            )
            
            
        )
    )
)
