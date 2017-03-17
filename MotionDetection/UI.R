# 06-navlist.R

library(shiny)

shinyUI <- navbarPage(title = "MotionDetecter",
                      
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
               column(4, textInput("caption", "First Point", " ")),
               column(4, textInput("caption", "Second Point", " ")),
               column(4, textInput("caption", "Third Point", " "))
             ),
             fluidRow(
                plotOutput("hist")
               )
             
            
         ),
    tabPanel(title = "User input",
      plotOutput("chisq"),
      actionButton("upload", "Resample")
    )

)

