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
      radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')
      
    ),
    
    mainPanel(
      uiOutput("tb")
      
      # use below code if you want the tabset programming in the main panel. If so, then tabset will appear when the app loads for the first time.
      #       tabsetPanel(tabPanel("Summary", verbatimTextOutput("sum")),
      #                   tabPanel("Data", tableOutput("table")))
    )
    
  ),
  fluidRow(
    column(3, textInput("text1", label = ("Name") 
    )),
    column(3, textInput("text2", label = ("Sex") 
    )),
    column(3, textInput("text3", label = ("Age") 
    )),
    column(3, textInput("text4", label = ("Tel") 
    ))
  ),
  fluidRow(
    
    column(3, textInput("text5", label = ("Address") 
    )),
    column(3, textInput("text6", label = ("Sample Number") 
    )),
    column(3, textInput("text7", label = ("Sampling Date") 
    )),
    column(3, textInput("text8", label = ("Sampling Hospital") 
    ))
  ),
  fluidRow(
    
    column(3, textInput("text9", label = ("Sampling Physician") 
    )),
    column(3, textInput("text10", label = ("OCRI Test Date") 
    )),
    column(3, textInput("text11", label = ("Technician") 
    ))
  )
))