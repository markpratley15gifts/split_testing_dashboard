
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  title = 'split test',

  # Application title
  titlePanel(
             h1("Split testing dashboard", align = "center")
            
             ),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(

    
        
      selectInput("select", label = h3("What do you want to do?"), 
                           choices = list("Analyse a current test" = 1, "Plan a new test" = 2),
                           selected = 1),
      uiOutput("testControls"),
      uiOutput("testOver"),
      uiOutput("testRunning"),
      uiOutput("testReamaining"),
      uiOutput("testSig"),
      uiOutput("convPlan"),
      uiOutput("trafficPlan"),
      uiOutput("testSigPlan"),
      uiOutput("propsPlan"),
      uiOutput("daysPlan"),
      uiOutput("minUp"),
      uiOutput("minUpPlan"),
      uiOutput("minPowerPlan"),
      numericInput('null_hypoth', label = ("Null hypotheis (%)"),
                   value = 0),
      selectInput('tails', label = ("Two tails?"),
                  choices = list("1" = 1, "2" = 2),
                  selected = 2),
      uiOutput("range"),
      uiOutput("loadcsv"),
      uiOutput("testCsv"),
      uiOutput("testCsvText")

    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      textOutput("text1"),
      tableOutput("table"),
      tableOutput("tableCsv"),
      
      conditionalPanel(
        condition = 'input.select == 1 &&  input.csv == 2 && output.fileUploaded == true',
        plotOutput("plotCsv")
      ),
  
      conditionalPanel(
        condition = 'input.select == 1 && input.testOver == 2 && input.csv == 1',

        plotOutput("plotSim")
      ),

      conditionalPanel(
        condition = '(input.select == 1 && input.testOver == 2 && input.csv == 2 && output.fileUploaded == true)',

        plotOutput("plotSimCsv")
    ),

      conditionalPanel(
        condition = 'input.select == 2',

        plotOutput("testPlan"),
        tableOutput("powerPlan")
        
      )
      
    )
  )
))

