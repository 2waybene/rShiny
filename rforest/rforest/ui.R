library(shiny)
shinyUI(fluidPage(
  titlePanel("File Input"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file","Upload the file"), # fileinput() function is used to get the file upload contorl option
      
      tags$hr(),
      h5(helpText("Select the read.table parameters below")),
      checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
      checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
      br(),
      radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
      textInput("text1", label = ("Name") 
                ),
      textInput("text2", label = ("Sex") 
               ),
      textInput("text3", label = ("Age") 
                ),
      textInput("text4", label = ("Tel") 
                ),
      textInput("text5", label = ("Address") 
                ),
      textInput("text6", label = ("Sample Number") 
      ),
      textInput("text7", label = ("Sampling Date") 
      ),
      textInput("text8", label = ("Sampling Hospital") 
      ),
      textInput("text9", label = ("Sampling Physician") 
      ),
      textInput("text10", label = ("OCRI Test Date") 
      ),
      textInput("text11", label = ("Technician") 
      )
    ),
    mainPanel(
      uiOutput("tb")
      
      # use below code if you want the tabset programming in the main panel. If so, then tabset will appear when the app loads for the first time.
      #       tabsetPanel(tabPanel("Summary", verbatimTextOutput("sum")),
      #                   tabPanel("Data", tableOutput("table")))
    )
    
  )
))