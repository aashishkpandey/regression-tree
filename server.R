###########################################################
#         Regression Tree App (server)              #
###########################################################
library(shiny)
library(rpart)

shinyServer(function(input, output,session) {

  #++_____________++
  ### Data import:
  Dataset <- reactive({
    if (is.null(input$file)) {
      # User has not uploaded a file yet
      return(data.frame())
    }
    Dataset <- read.csv(input$file$datapath ,header=TRUE)
    # row.names(Dataset) = Dataset[,1]; Dataset= Dataset[,2:ncol(Dataset)]
    #Dataset = t(Dataset)
    return(Dataset)
  })
  
  #++_____________++
  # Select variables:
  output$varselect <- renderUI({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    # Variable selection:
    selectInput("Attr", label = h4("Select Response Variable"), 
                choices = colnames(Dataset()))
  })
  
  #------------------------------------------------#
  fit.rt = reactive({
  if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
  x = setdiff(colnames(Dataset()), input$Attr)
  y = input$Attr
  # formula1 = 
  
  fit.rt <- rpart(as.formula(paste(y, paste( x, collapse = ' + '), sep=" ~ ")), 
                  # method="anova",   # use "class" for classification trees
                data=Dataset())
    })
  
  #------------------------------------------------#
  output$results = renderPrint({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    printcp(fit.rt()) # display the results
    # formula.mod()
  })
  
  output$summary = renderPrint({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    summary(fit.rt()) # detailed summary of splits  
  })
  
  output$imp = renderPrint({
    round(fit.rt()$variable.importance/sum(fit.rt()$variable.importance),2)  
  })
  
  output$plot1 = renderPlot({
    plotcp(fit.rt()) # visualize cross-validation results   
  })
  
  output$plot2 = renderPlot({
    
    ## create additional plots 
    # par(mfrow = c(1, 2)) # two plots on one page 
    # rsq.rpart(fit.rt()) # visualize cross-validation results 
    
    # plot tree 
    # plot(fit.rt(), 
    #      uniform = TRUE, 
    #      main = "Regression Tree for Mileage ")
    # text(fit.rt(), use.n = TRUE, all = TRUE, cex = .8)
    
    # create attractive postcript plot of tree 
    title1 = paste("Regression Tree for", input$Attr)
    post(fit.rt(), 
         # file = "tree2.ps", 
         filename = "",   # will print to console
         title = title1) 
    
  })
  
})