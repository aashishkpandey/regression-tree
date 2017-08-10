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
        htmlOutput("varselect")
        
      ),   # end of sidebar panel
      
    mainPanel(
        tabsetPanel(type = "tabs",
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
                             plotOutput("plot2",height = 600, width = 850))
                             
        ) # end of tabsetPanel
      )# end of main panel
    ) # end of sidebarLayout
  )  # end if fluidPage
) # end of UI



