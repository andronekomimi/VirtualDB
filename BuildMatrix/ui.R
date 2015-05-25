# BUILDMATRIX ui.R
library(shiny)
library(shinyFiles)
shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
    tags$link(rel = "icon", type = "image/x-icon", href = "logo.png")
  ),
  
  headerPanel("Virtual DB"),
  
  sidebarLayout(
   NULL,
    
    mainPanel(
      fluidRow(
        column(width = 12,
               wellPanel(
      h3("How to build your DB"),
      helpText("The following steps will allow you to build your virtual database :",
               p("* Select the CSV file (with header)"),
               p("* Select the searchable fields and define their types"),
               p("* GO !"),
               "Once it's done, use the custom query interface to search in your 
               virtual db")
      ))),
      fluidRow(
        column(width = 12,
               wellPanel(
                 h3("1. Choose CSV File"),
                 shinyFilesButton('files', 'Choose file', 
                                  'Please select a file', FALSE),
                 textInput(inputId = "path", label = ""), 
                 tags$br()
               ))),
      fluidRow(
        column(width = 6,
               wellPanel(  
                 checkboxGroupInput("inCheckboxGroup", label = h3("2. Select fields"), 
                                    choices = "Waiting for your CSV file" ),
                 br(),
                 br(),
                 actionButton(inputId = "sendSelectedCols", label = "OK"),
                 br(),
                 br(),
                 span(textOutput("text1"), style = "color:blue")
               )),
        column(width = 6,
               wellPanel(  
                 h3("3. Define fields type"),
                 br(),
                 uiOutput("selectedCols"),
                 actionButton(inputId = "sendColsType", label = "OK"),
                 br(),
                 br(),
                 span(textOutput("text2"), style = "color:blue"),
                 br(),
                 br(),
                 uiOutput("defineFactorLevels"),
                 actionButton(inputId = "sendSelectedFactors", label = "OK"),
                 br(),
                 br(),
                 downloadButton(outputId = "saveParam", 
                                label = strong("Save parameters")),
                 span(textOutput("text3"), style = "color:blue")
               ))
        
      ),
      fluidRow(
        column(width = 12,
               wellPanel( 
                 h3("4. You can now query your virtual database ",
                   a("here.", href = "../QueryMatrix/")
                 )
               )
        )
      ),
      width = "100%"
    )
  )
))