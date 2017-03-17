# 06-navlist.R

library(shiny)

shinyUI <- navbarPage(title = "MotionDetecter",
                      
      # First tab displaying example results
      
      tabPanel(title = "Sample data",
               helpText("This program computes the anglar velocity of experimental mice. 
               It takes three-point coordinates, and computes the head turing
                        angle. It also reports whether the angle is clockwise -- POSITIVE
                        or, counter-clockwise -- NEGATIVEThe example shown was from a set of velocity data. Please make a
                        selection from the bottom window to view the results."),
               br(), br(),
      
               selectInput("var", "Choose a plot:", 
                           choices = c("Angle scatter plot", "Angle density"),
                           selected = "Angle scatter plot"),
               
               br(), br(),
               
               plotOutput("plot")
    ),
    
    tabPanel(title = "Test points",

             fluidRow(
               
               column(3, 
                      textInput("Point1", 
                                "First Point:x,y", 
                                "950.7687,620.35077")),
               
               column(3, 
                      textInput("Point2", 
                                "Second Point:x,y", 
                                "952.1389,625.4861")),
               
               column(3, 
                      textInput("Point3", 
                                "Third Point:x,y", 
                                "953.59875,629.67285")),
               
               #column(3, textInput("Point1", "First Point:x,y", "50.7687,20.35077")),
               #column(3, textInput("Point2", "Second Point:x,y", "52.1389,25.4861")),
               #column(3, textInput("Point3", "Third Point:x,y", "53.59875,29.67285")),
               
               column(3, 
                      actionButton(inputId="PointsPlot", 
                                   "Plot points",
                                   icon = icon("refresh")))
               
             ),
             
             fluidRow(
                plotOutput("points")
                #tableOutput("points")
             )
         ),
    
    
    tabPanel(title = "User input",
             fluidRow(
               helpText("Please choose a velocity file (.csv format) to compute the anglar speed.")
             ),
             br(), 
             
             fluidRow( 
                fileInput("file1", "Choose CSV File",
                         accept = c(
                         "text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")
                ),
             #),
      
             #fluidRow(
             checkboxInput("header", "Header", TRUE),
             #),
             
             #fluidRow(
               column(3, 
                tableOutput("view"))

            ),
            
            fluidRow(

              actionButton(inputId = "go", 
                           "Compute Angle")
            ),
            fluidRow(
              column(3,
              tableOutput("Results")),
              column(9,
                     plotOutput("density"))
            )
            #fluidRow(
            #  tableOutput("Results")
            #)
    )

)

