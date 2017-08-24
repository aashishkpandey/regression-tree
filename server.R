###########################################################
#         Regression Tree App (server)              #
###########################################################
library(shiny)
library(rpart)
library(pastecs)
library(dplyr)
library(Hmisc)

shinyServer(function(input, output,session) {
  
  #------------------------------------------------#
  
  readdata <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      readdata <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
      return(readdata)
    }
  })
  
  # Select variables:
  output$yvarselect <- renderUI({
    if (identical(readdata(), '') || identical(readdata(),data.frame())) return(NULL)
    
    selectInput("yAttr", "Select Y variable",
                colnames(readdata()), colnames(readdata())[1])
    
  })
  
  output$xvarselect <- renderUI({
    if (identical(readdata(), '') || identical(readdata(),data.frame())) return(NULL)
    
    checkboxGroupInput("xAttr", "Select X variables",
                       setdiff(colnames(readdata()),input$yAttr), setdiff(colnames(readdata()),input$yAttr))
    
  })
  
  Dataset = reactive({
    mydata = readdata()[,c(input$yAttr,input$xAttr)]
    return(mydata)
    
    })
  #------------------------------------------------#
  
  out = reactive({
    data = Dataset()
    Dimensions = dim(data)
    Head = head(data)
    Tail = tail(data)
    Class = NULL
    for (i in 1:ncol(data)){
      c1 = class(data[,i])
      Class = c(Class, c1)
    }
    
    nu = which(Class %in% c("numeric","integer"))
    fa = which(Class %in% c("factor","character"))
    nu.data = data[,nu] 
    fa.data = data[,fa] 
    Summary = list(Numeric.data = round(stat.desc(nu.data) ,4), factor.data = describe(fa.data))
    
    a = seq(from = 0, to=200,by = 4)
    j = length(which(a < ncol(nu.data)))
    out = list(Dimensions = Dimensions,Summary =Summary ,Tail=Tail,fa.data,nu.data,a,j)
    return(out)
  })
  
  output$summarydata = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {
      out()[1:2]
    }
  })
  
  #------------------------------------------------#
  fit.rt = reactive({
  if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
  x = setdiff(colnames(Dataset()), input$Attr)
  y = input$yAttr
  # formula1 = 
  
  fit.rt <- rpart(as.formula(paste(y, paste( x, collapse = ' + '), sep=" ~ ")),
                  cp = input$cp,
                  # method="anova",   # use "class" for classification trees
                data=Dataset())
    })
  
  
  #------------------------------------------------#
  output$results = renderPrint({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    printcp(fit.rt()) # display the results
    # formula.mod()
  })
  
  
  #------------------------------------------------#
  output$summary = renderPrint({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    summary(fit.rt()) # detailed summary of splits  
  })
  
  
  #------------------------------------------------#
  output$imp = renderPrint({
    round(fit.rt()$variable.importance/sum(fit.rt()$variable.importance),2)  
  })
  
  
  #------------------------------------------------#
  output$plot1 = renderPlot({
    plotcp(fit.rt()) # visualize cross-validation results   
  })
  
  
  #------------------------------------------------#
  output$plot2 = renderPlot({
    
    title1 = paste("Regression Tree for", input$yAttr)
    
    fit.rt1 = fit.rt()
    fit.rt1$frame$yval = as.numeric(rownames(fit.rt()$frame))
    
    # create attractive postcript plot of tree 
    post(fit.rt1, 
         # file = "tree2.ps", 
         filename = "",   # will print to console
         use.n = FALSE,
         compress = TRUE,
         title = title1) 
    
  })
  
  output$plot3 = renderPlot({
    
    title1 = paste("Decision Tree for", input$yAttr)
    
  post(fit.rt(), 
       # file = "tree2.ps", 
       filename = "",   # will print to console
       use.n = TRUE,
       compress = TRUE,
       title = title1) 
  })
  
  
  #------------------------------------------------#
  nodes1 =  reactive({
    
  tree_nodes = as.data.frame(fit.rt()$where)
  colnames(tree_nodes) <- "node_number"
  # tree_nodes %>% head()
    
  a0 = as.numeric(rownames(fit.rt()$frame)); a0
  a1 = seq(1:nrow(fit.rt()$frame)); a1 
  a2 = as.vector(fit.rt()$where)
  node_num = a2
  for (i1 in 1:nrow(tree_nodes)){
    node_num[i1] = a0[a2[i1]]
  }
  
  tree_nodes1 <- fit.rt()$where %>% as.data.frame() %>% cbind(node_num) %>% dplyr::select("node_num")
  tree_nodes1
  
  })

  output$nodesout = renderPrint({
    head(nodes1(),15)
  })
  
  
  randomforest = reactive({
    
    trainSet <- Dataset()
    x = setdiff(colnames(Dataset()), input$Attr)
    y = input$yAttr
    
    set.seed(23)
    t = Sys.time()
    forest.surive <- randomForest(as.formula(paste(y, paste( x, collapse = ' + '), sep=" ~ ")), 
                                  data = trainSet, 
                                  mtry = 4, 
                                  importance = TRUE, 
                                  ntree = 5000)    
    
  })
  
  output$rfimp = renderPrint({
    importance(randomforest(), type=1)  
  })
  
  
  #Following evaluates to 33.02. Checking training set includes same rows. 
  # mean(mTitanicAll[trainSet, "fare"])
  # 
  # set.seed(23)
  
  # survive.Predict <- predict(forest.surive, type = "class",
  #                            newdata = mTitanicAll[-trainSet,])
  
  
  
  # a0 = round(survive.Predict)
  # tab = table(a0, mTitanicAll[-trainSet, "survived"])
  # (tab[1] + tab[4]) / sum(tab)  #80.86% whereas simple d-tree gave 82.29%
  
  # alternatively and more flexibly (since 48% could also be "1" and not "0")
  # a01 = sapply(survive.Predict, function(x) ifelse(x >= 0.48, 1, 0))
  # tab = table(a01, mTitanicAll[-trainSet, "survived"])
  # (tab[1] + tab[4]) / sum(tab)  # 81.33% now, up from 80.86%
  
  #------------------------------------------------#
  output$downloadData <- downloadHandler(
    filename = function() { "beer data.csv" },
    content = function(file) {
      write.csv(read.csv("data/beer data.csv"), file, row.names=F, col.names=F)
    }
  )
  
  })