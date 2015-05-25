# QUERYMATRIX server.R

source("helpers.R")

current_query <- 0

shinyServer(function(input, output, session) {
  autoInvalidate <- reactiveTimer(100, session)
  
  getParameters <- reactive({
    inFile <- input$file1
    if(!is.null(inFile)) {getParamFromFile(inFile$datapath)}
    
  })
  
  queryMatrix <- reactive({
    
    res = list()
    print("queryMatrix is called")
    if(input$sendQuery > 0 ){
      # put a lock file
      
      inFile1 <- input$file1
      inFile2 <- input$path
      
      if(is.null(inFile1) || !file.exists(inFile2)) {
        res = data.frame(Error = c("Need to fill the two first step before 
                                   querying the matrice"), 
                         stringsAsFactors = FALSE)
      }
      else
      {
        lockfile <- paste0("locks/", session$clientData$singletons,".lock")
        file.create(lockfile)
        q = buildQuery(paramList = getParametersValue())
        res = runQuery(dataset = inFile2, query = q)
        if(file.exists(lockfile)) {file.remove(lockfile)}
      }
      
      current_query = input$sendQuery
      
      res
    }
    
  })
  
  
  getParametersValue <- reactive({
    s <- list()
    
    params_names = c()
    params_values = c()
    params_types = c()
    
    if(input$sendQuery > 0)
    {
      params = getParameters()
      
      for(i in seq(1, nrow(params))) {
        
        params_name = as.character(params[i,]$name)
        params_names = c(params_names, params_name)
        params_type = as.character(subset(params, params$name == params_name)$type)
        params_types = c(params_types, params_type)
        params_value = character()
        
        if(params_type == "Numeric") {
          # get min
          var_name <- paste0("input$",params_name, "_val_min")
          assign(var_name, paste0("input$",params_name, "_val_min"))
          param_value_min = eval(parse(text=var_name))
          
          var_name <- paste0("input$",params_name, "_val_max")
          assign(var_name, paste0("input$",params_name, "_val_max"))
          param_value_max = eval(parse(text=var_name))
          
          params_value = paste0(param_value_min,":",param_value_max)
        }
        else
        {
          if(params_type == "Probability") {
            var_name <- paste0("input$",params_name, "_val")
            assign(var_name, paste0("input$",params_name, "_val"))
            params_value = eval(parse(text=var_name))
            params_value = paste(params_value,collapse = ":")
          }
          else
          {
            var_name <- paste0("input$",params_name, "_val")
            assign(var_name, paste0("input$",params_name, "_val"))
            params_value = eval(parse(text=var_name))
          }
        }
        
        params_values <- c(params_values, params_value)
        
      }
      
      s = list(name = params_names, type = params_types, value = params_values)
      s
    }
    
  })
  
  getPrimaryParamWidgetList <- reactive({
    widgets = list()
    params = getParameters()
    
    params = subset(params, params$category == FALSE)
    advanced_params = subset(params, params$category == TRUE)
    
    for(i in seq(1, nrow(params))) {
      
      params_name = as.character(params[i,]$name)
      params_type = as.character(subset(params, params$name == params_name)$type)
      params_level = strsplit(as.character(params[i,]$level),split = ",", 
                              fixed = TRUE)[[1]]
      
      var_name <- paste0(params_name, "_val")
      assign(var_name, paste0(params_name, "_val"))
      
      widget = switch(params_type,
                      "String" = textInput(inputId = var_name, 
                                           label = params_name,
                                           value = ""),
                      "Numeric" =  fluidRow(
                        column(width = 6,
                               wellPanel(numericInput(inputId = paste0(var_name,"_min"), 
                                                      label = paste("min",params_name),
                                                      value = NA))),
                        column(width = 6,
                               wellPanel(numericInput(inputId = paste0(var_name,"_max"), 
                                                      label = paste("max",params_name),
                                                      value = NA)))),
                      "Probability" = sliderInput(inputId = var_name, 
                                                  label = params_name,
                                                  step = 0.001, min = 0, max =1, 
                                                  value = c(0,1)),
                      "Logical" = selectInput(inputId = var_name, 
                                              label = params_name,
                                              choices = c("TRUE" = 1,
                                                          "FALSE" = 0,
                                                          "NA" = -1),
                                              selected = 0),
                      "Factor" = selectInput(inputId = var_name, 
                                             label = params_name,
                                             choices = c("",params_level),
                                             selected = 0)
      )
      
      widgets = list(widgets, widget)
      
    }
    
    widgets
  })
  
  getAdvancedWidgetList <- reactive({
    widgets = list()
    params = getParameters()
    
    params = subset(params, params$category == TRUE)
    
    for(i in seq(1, nrow(params))) {
      
      params_name = as.character(params[i,]$name)
      params_type = as.character(subset(params, params$name == params_name)$type)
      params_level = strsplit(as.character(params[i,]$level),split = ",", 
                              fixed = TRUE)[[1]]
      
      var_name <- paste0(params_name, "_val")
      assign(var_name, paste0(params_name, "_val"))
      
      widget = switch(params_type,
                      "String" = textInput(inputId = var_name, 
                                           label = params_name,
                                           value = ""),
                      "Numeric" =  fluidRow(
                        column(width = 6,
                               wellPanel(numericInput(inputId = paste0(var_name,"_min"), 
                                                      label = paste("min",params_name),
                                                      value = NA))),
                        column(width = 6,
                               wellPanel(numericInput(inputId = paste0(var_name,"_max"), 
                                                      label = paste("max",params_name),
                                                      value = NA)))),
                      "Probability" = sliderInput(inputId = var_name, 
                                                  label = params_name,
                                                  step = 0.001, min = 0, max =1, 
                                                  value = c(0,1)),
                      "Logical" = selectInput(inputId = var_name, 
                                              label = params_name,
                                              choices = c("TRUE" = 1,
                                                          "FALSE" = 0,
                                                          "NA" = -1),
                                              selected = 0),
                      "Factor" = selectInput(inputId = var_name, 
                                             label = params_name,
                                             choices = c("",params_level),
                                             selected = 0)
      )
      
      widgets = list(widgets, widget)
      
    }
    
    widgets
  })
  
  observe({
    output$primaryParamList <- renderUI({
      if(!is.null(input$file1)){getPrimaryParamWidgetList()}
    })
  })
  
  observe({
    output$advancedParamList <- renderUI({
      if(!is.null(input$file1)){getAdvancedWidgetList()}
    })
  })
  
  
  output$tbl <- renderDataTable(
    if(input$sendQuery > 0)
    {
      res = queryMatrix()
      
      if("Error" %in% names(res))
      {
        res
      }
      else
      {
        if(is.null(input$expand) || input$expand == "No")
        {
          subset(res, select = getParametersValue()$name)
        }
        else
        {
          res
        }
      }
    }
    ,
    options = list(pageLength = 10),
    callback = "function(table) {
      table.on('click.dt', 'tr', function() {
        table.$('tr.selected').removeClass('selected');
        $(this).toggleClass('selected');
        Shiny.onInputChange('rows',
                             table.rows('.selected').data().toArray());
      });
    }",
  )
  
  output$text1 <- renderText({
    if(input$sendQuery > 0)
    {
      res = queryMatrix()
      if(nrow(res) > 0) {
        if(!("Error" %in% names(res)))
        {
          paste0(nrow(res)," result(s)")
        }
        else
        {
          "Error"
        }
        
      }
      else
      {
        paste0("No results")
      }
    }
  })
  
  outputOptions(output, "text1", suspendWhenHidden=FALSE)
  
  output$expand_save <- renderUI({
    expand_status()
  })
  
  
  expand_status <- reactive ({
    res = queryMatrix()
    if(input$sendQuery > 0 && !("Error" %in% names(res))) {
      list(selectInput(inputId = "expand", label = "Show all columns",
                       selected = "No", choices = c("No", "Yes")),
           downloadButton(outputId = "saveResults", 
                          label = strong("Save results"))
      )
    }
  })
  
  observe({
    roots = c(wd='/')
    shinyFileChoose(input, 'files', session=session, roots=roots,
                    filetypes=c('','csv','txt'))
    pfile = parseFilePaths(roots, input$files)
    updateTextInput(session, "path",  value = pfile$datapath)
    
  })
  
  output$saveResults <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste("results.csv")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- ","
      
      # Write to a file specified by the 'file' argument
      write.table( queryMatrix(),file, sep = sep,
                   row.names = FALSE)
    }
  )
})