# BUILDMATRIX server.R

library(shinyFiles)
source("helpers.R")

selectedCols = c()
colsType = c()
widgets = list()
factors = list()

shinyServer(function(input, output, session) {
  
  getCols <- reactive({
    getColumns(input$path)
  })
  
  getColsType <- reactive({
    s <- c()
    
    if(input$sendColsType > 0)
    {
      cols = getSelectedCols()
      var_names = c()
      
      for(i in seq(1, length(cols))) {
        c = gsub(cols[i], pattern = " ", replacement = "")
        var_name <- paste0(c, "_type")
        assign(var_name, paste(c, "_type"))
        var_names = c(var_names, var_name)
      }
      
      for(var in var_names) {
        var_name <- paste0("input$",var)
        s <- c(s, eval(parse(text=var_name)))
      }
      
      names(s) = cols
      s
    }
  })
  
  getColsCategory <- reactive({
    s <- c()
    
    if(input$sendColsType > 0)
    {
      cols = getSelectedCols()
      var_names = c()
      
      for(i in seq(1, length(cols))) {
        c = gsub(cols[i], pattern = " ", replacement = "")
        var_name <- paste0(c, "_cat")
        assign(var_name, paste(c, "_cat"))
        var_names = c(var_names, var_name)
      }
      
      for(var in var_names) {
        var_name <- paste0("input$",var)
        s <- c(s, eval(parse(text=var_name)))
      }
      
      names(s) = cols
      print(s)
      s
    }
  })
  
  getFactorLevels <- reactive({
    s <- c()
    
    if(input$sendSelectedFactors > 0)
    {
      cols = getSelectedCols()
      var_names = c()
      
      for(i in seq(1, length(cols))) {
        c = gsub(cols[i], pattern = " ", replacement = "")
        var_name <- paste0(c, "_factor")
        assign(var_name, paste(c, "_factor"))
        var_names = c(var_names, var_name)
      }
      
      for(var in var_names) {
        var_name <- paste0("input$",var)
        factor_value = eval(parse(text=var_name))
        if(is.null(factor_value)){
          s <- c(s, "")
        }
        else
        {
          s <- c(s, factor_value)
        }
        
      }
      
      names(s) = cols
      
      print(getColsCategory())
      s
    }
    
  })
  
  printColsType <- reactive({
    s <- c()
    
    if(input$sendColsType > 0)
    {
      colsType = getColsType()
      col_names = names(colsType)
      for(col_name in col_names) {
        s <- c(s,paste0(col_name, ":",colsType[[col_name]]))
        
      }
      s
    }
  })
  
  getSelectedCols <- reactive({
    
    if(input$sendSelectedCols > 0)
    {
      selectedCols <- input$inCheckboxGroup
    }
    
    selectedCols
  })
  
  getWidgetList <- reactive({
    
    cols = getSelectedCols()
    for(i in seq(1, length(cols))) {
      var_name <- paste0(cols[i], "_type")
      assign(var_name, paste0(cols[i], "_type"))
      var_name2 <- paste0(cols[i], "_cat")
      assign(var_name2, paste0(cols[i], "_cat"))
      widgets = list(widgets, list(var_name = selectInput(inputId = var_name, 
                                                     label =  cols[i],
                                                     choices = c("String", "Numeric",
                                                                 "Factor","Logical",
                                                                 "Probability"
                                                     )),
                              var_name2 = checkboxInput(inputId = var_name2, label = "Advanced parameters", value = FALSE),
                              hr())
      )
    }
    
    #     ok_button = actionButton(inputId = "sendColsType", label = "OK")
    #     widgets = list(widgets, ok_button)
    
    widgets
  })
  
  getFactorWidgetList <- reactive({
    
    params_type = getColsType()
    params_name = names(params_type)
    
    widget = helpText("Please define the factors of the following parameters :")
    
    factors = list(factors, widget)
    
    for(i in seq(1, length(params_name))) {
      
      var_name <- paste0(params_name[i], "_factor")
      assign(var_name, paste0(params_name[i], "_factor"))
      
      widget = switch(params_type[i],
                      "Factor" = textInput(inputId = var_name, 
                                           label = params_name[i],
                                           value = "levels separated by a comma")
      )
      
      factors = list(factors, widget)
      
    }
    #     ok_button = actionButton(inputId = "sendSelectedFactors", label = "OK")
    #     factors = list(factors, ok_button)
    
    factors
    
  })
  
  observe({
    if(file.exists(input$path)) {
      updateCheckboxGroupInput(session, "inCheckboxGroup", choices = getCols())
    }
    
  })
  
  output$text1 <- renderText({
    paste("You have selected : ", paste(getSelectedCols(), collapse = ","))
  })
  
  output$selectedCols <- renderUI({
    if(length(getSelectedCols() > 0)){getWidgetList()}
  })
  
  output$defineFactorLevels <- renderUI({
    if("Factor" %in% getColsType()){getFactorWidgetList()}
  })
  
  output$text2 <- renderText({
    paste("You have selected : ", paste(printColsType(), collapse = " ; "))
  })
  
  output$text3 <- renderText({
    paste("", getFactorLevels())
  })
  
  buildParametersDF <- reactive({
    name = getSelectedCols()
    type = getColsType()
    level = getFactorLevels()
    category = getColsCategory()
    
    data.frame(name,type,level,category,stringsAsFactors = FALSE)
  })
  
  
  output$saveParam <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste("param.txt")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- ";"
      
      # Write to a file specified by the 'file' argument
      write.table(buildParametersDF(),file, sep = sep,
                  row.names = FALSE)
    }
  )
  
  observe({
    roots = c(wd='/')
    shinyFileChoose(input, 'files', session=session, roots=roots,
                    filetypes=c('','csv','txt'))
    pfile = parseFilePaths(roots, input$files)
    updateTextInput(session, "path",  value = pfile$datapath)
  })
  
  
  
})
