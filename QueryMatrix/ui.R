#QUERYMATRIX ui.R
library(shiny)
library(shinyFiles)
library(shinyBS)

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
                 h3("How to query the VDB"),
                 helpText("The following steps will allow you to query your virtual database :",
                          p("* Select the parameters file"),
                          p("* Select the CSV file (with header)"),
                          p("* Set up your filters"),
                          p("* GO !"))
               ))),
      fluidRow(
        column(width = 12,
               wellPanel( 
                 fileInput('file1', label = h3('1. Load the parameters file'),
                           accept=c('text/txt','.txt'))
               ))),
      fluidRow(
        column(width = 12,
               wellPanel(
                 h3('2. Choose the data file'),
                 shinyFilesButton('files', 'Choose file', 
                                  'Please select a file', FALSE),
                 textInput(inputId = "path", label = "")
               ))
      ),
      fluidRow(
        column(width = 12,
               wellPanel( 
                 h3('3. Build your query'),
                 br(),
                 bsCollapse(id = "parameters", open = c("Primary parameters","Advanced parameters"), multiple = TRUE,
                             bsCollapsePanel("Primary parameters",uiOutput("primaryParamList"), style = "default"),
                             bsCollapsePanel("Advanced parameters", uiOutput("advancedParamList"), style = "default")
                 ),
                 br(),
                 actionButton(inputId = "sendQuery", label = strong("Query")),
                 br(),
                 br(),
                 p("Want to add/remove some searchable field ? You can make a 
                   new parameter file ",
                   a("here.", href = "../BuildMatrix/")
                 )
               ))),
      fluidRow(
        column(width = 12,
               wellPanel( 
                 h3('4. Results'),
                 br(),
                 conditionalPanel(condition = "input.sendQuery > 0 && !output.text1",
                                  list(
                                    img(src = "page_loader_2.gif",
                                        filetype = "image/gif",
                                        alt = "Please wait... I'm processing your query")
                                  )
                 ),
                 span(textOutput("text1"), style = "color:blue"), br(),
                 tableOutput("view"),
                 dataTableOutput('tbl'),
                 uiOutput("expand_save")
               ))),
      width = "100%"
      
    )
  )
))