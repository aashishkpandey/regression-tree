###########################################################
#           Regression Tree App (ui)                #
###########################################################
library("shiny")

shinyUI(
  fluidPage(
    
    titlePanel("Regression Tree"),
    
    sidebarLayout(
      
      sidebarPanel(
        # Upload data:
        h4(p(" Data Input")),
        fileInput("file", "Upload data in csv"),
        # h4(p("Select Response Variable")),
        sliderInput('cp','Complexity Parameter',0,1,0.01),
        htmlOutput("yvarselect"),
        htmlOutput("xvarselect")
        
      ),   # end of sidebar panel
      
    mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Overview",
                             h4(p("How to use this shiny application")),
                             p("This shiny application require one data input from the user. To do so, click on the Browse (in left side-bar panel) and upload the csv data input file.
                           Note that this application can read only csv file (comma delimited file), so if you don't have csv input data file, first convert your data in csv format 
                           and then proceed. Make sure you have top row as variable names.",align="justify"),
                             p("Once csv file is uploaded successfully, variables in the data file will reflect in left-side Data Selection Panel. Now you can select 
                            dependent variable (Y Variable) from drop-down menu. By default all other remaining variables will be selected as explanatory variables (X variables). 
                              If you want to drop any variable from explanatory variables, just uncheck that variable and it will be dropped from the model.
                            ",align="justify"),
                             br(),
                             h4(p("Download Sample Input File")),
                             br(),
                             downloadButton('downloadData', 'Download Example file'),
                             br(),
                             br(),
                             p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
                             img(src = "example1.png") #, height = 280, width = 400
                             ),
                    tabPanel("Data Summary",verbatimTextOutput('summarydata')),
                    tabPanel("Model Output",
                             h4('Results'),
                             verbatimTextOutput("results"),
                             h4('Variable importance'),
                             verbatimTextOutput('imp'),
                            h4('Detailed summary of splits'),
                            verbatimTextOutput("summary")),
                    tabPanel("Plot Output",
                             h4('Visualize cross-validation results'),
                             plotOutput("plot1",height = 600, width = 850),
                             h4('Regression Tree'),
                             plotOutput("plot2",height = 600, width = 850),
                             verbatimTextOutput('nodes'))
                             
        ) # end of tabsetPanel
      )# end of main panel
    ) # end of sidebarLayout
  )  # end if fluidPage
) # end of UI



