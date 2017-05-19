library(shiny)
source("transform1.R")
rfFit <- readRDS("data/data.Rda")
# use the below options code if you wish to increase the file input limit, in this example file input limit is increased from 5MB to 9MB
# options(shiny.maxRequestSize = 9*1024^2)

shinyServer(function(input,output){
  
  # This reactive function will take the inputs from UI.R and use them for read.table() to read the data from the file. It returns the dataset in the form of a dataframe.
  # file$datapath -> gives the path of the file
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    
  })
  
  # this reactive output contains the summary of the dataset and display the summary in table format
  output$filedf <- renderPlot({
    if(is.null(data())){return ()}
    plotmatrix2(data())
  })
  
  # this reactive output contains the summary of the dataset and display the summary in table format
  output$sum <- renderTable({
    if(is.null(data())){return ()}
    summary(data())
    
  })
  
  # This reactive output contains the dataset and display the dataset in table format
  output$table <- renderTable({
    if(is.null(data())){return ()}
    data()
  })
  
  output$transform <- renderTable({
    if(is.null(data())){return ()}
    transform1(data())
  })
  
  output$predy <- renderText({
    if(is.null(data())){return ()}
    paste("There is a", fabaf(data()), "% chance the patient has cancer.")
  })
  output$doubleplot <- renderPlot({
    if(is.null(data())){return ()}
    ploty1(data())
  })
  output$name <- renderText({
    if(is.null(data())){return ()}
    paste("Name:", input$text1)
  })
  output$sex <- renderText({
    if(is.null(data())){return ()}
    paste("Sex:", input$text2)
  })
  output$age <- renderText({
    if(is.null(data())){return ()}
    paste("Age:", input$text3)
  })
  output$tel <- renderText({
    if(is.null(data())){return ()}
    paste("Tel:", input$text4)
  })
  output$add <- renderText({
    if(is.null(data())){return ()}
    paste("Address:", input$text5)
  })
  output$samp1 <- renderText({
    if(is.null(data())){return ()}
    paste("Sample No:", input$text6)
  })
  output$samp2 <- renderText({
    if(is.null(data())){return ()}
    paste("Sampling date:", input$text7)
  })
  output$samp3 <- renderText({
    if(is.null(data())){return ()}
    paste("Sampling hospital:", input$text8)
  })
  output$samp4 <- renderText({
    if(is.null(data())){return ()}
    paste("Sampling physician:", input$text9)
  })
  output$samp5 <- renderText({
    if(is.null(data())){return ()}
    paste("OCRI test date:", input$text10)
  })
  output$tech <- renderText({
    if(is.null(data())){return ()}
    paste("Technician:", input$text11)
  })
  output$title1 <- renderText({
    if(is.null(data())){return ()}
    paste("Description")
  })
  output$title2 <- renderText({
    if(is.null(data())){return ()}
    paste("Clinical Information")
  })
  output$title3 <- renderText({
    if(is.null(data())){return ()}
    paste("Interpretation")
  })
  output$title4 <- renderText({
    if(is.null(data())){return ()}
    paste("Clinical Study")
  })
  output$ocri1 <- renderText({
    if(is.null(data())){return ()}
    paste("OCRI:")
  })
  output$ocri2 <- renderPlot({
    if(is.null(data())){return ()}
    ploty2(data())
  })
  output$description <- renderText({
    if(is.null(data())){return ()}
    paste("OCRI indicates the probability of oral cancer in a specific sample, 
                        and it may predict cancer risk in the future. With a range from 0 to 1, OCRI at 0 indicates the lowest probability and OCRI at 1 the highest probability.")
  })
  output$description1 <- renderText({
    if(is.null(data())){return ()}
    paste("*OCRI>0.5 is labeled as red, suggesting a high risk of cancer. OCRI<0.5 is labeled as blue, suggesting a low risk of cancer.")
  })
  output$description2 <- renderText({
    if(is.null(data())){return ()}
    paste("Reference: Liu Y, et al. Quantitative Prediction of Oral Cancer Risk in Patients with Oral Leukoplakia. ONCOTARGET 2017.")
  })
  output$description3 <- renderText({
    if(is.null(data())){return ()}
    paste("The value needs to be interpreted in accordance with clinical information. It is generated based on DNA values of exfoliative cells from oral mucosa.")
  })
  output$description4 <- renderText({
    if(is.null(data())){return ()}
    paste("We collected DNA ploidy data of exfoliative cells, histopathology data and clinical data from 120 normal subjects, 110 oral leukoplakia and 134 oral squamous cell carcinoma. OCRI was calculated using data reconstruction and machine learning techniques (sensitivity=1.00, specificity>0.99).")
  })
  output$description5 <- renderText({
    if(is.null(data())){return ()}
    paste("WARNING!")
  })
  output$description6 <- renderText({
    if(is.null(data())){return ()}
    paste("Although OCRI is developed with histopathology as the golden standard, its accuracy is still subject to variations due to sampling, staining and data collection. If OCRI>0.5, it is recommended to repeat the test and consult a pathologist.")
  })
  output$description7 <- renderText({
    if(is.null(data())){return ()}
    paste("FOR RESEARCH USE ALONE, NOT INTENDED FOR CLINICAL USE")
  })
  output$description8 <- renderText({
    if(is.null(data())){return ()}
    paste("Oral Cancer Risk Index (OCRI) Test Report")
  })
  output$description9 <- renderText({
    if(is.null(data())){return ()}
    paste("Upload CSV file of Patient Data with a column DNA_Index")
  })
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
    if(is.null(data()))
      verbatimTextOutput("description9")
    else
      tabsetPanel(tabPanel("Data", tableOutput("table")),tabPanel("Summary", tableOutput("sum")),
                  tabPanel("Transform", tableOutput("transform")),tabPanel("Plot", plotOutput("filedf")),
                  tabPanel("Prediction", textOutput("predy")),tabPanel("Test", verbatimTextOutput("description8"), br(),
                                                                       verbatimTextOutput("title2"),
                                                                       textOutput("name"),textOutput("sex"),
                                                                       textOutput("age"),
                                                                       textOutput("tel"),
                                                                       textOutput("add"),
                                                                       textOutput("samp1"),
                                                                       textOutput("samp2"),
                                                                       textOutput("samp3"),
                                                                       textOutput("samp4"),
                                                                       textOutput("samp5"), br(),
                                                                       verbatimTextOutput("title1"),textOutput("description"), br(),
                                                                       verbatimTextOutput("title3"),textOutput("ocri1"), 
                                                                       plotOutput("ocri2"), textOutput("description3"), br(),
                                                                       verbatimTextOutput("title4"),plotOutput("doubleplot"),
                                                                       textOutput("description4"),
                                                                       textOutput("description1"), br(), textOutput("description2"),
                                                                       br(), textOutput("description5"), textOutput("description6"),
                                                                       br(), textOutput("tech"),
                                                                       br(), verbatimTextOutput("description7")))
                  
  })
})