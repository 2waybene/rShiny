library(shiny)

## This is a working ui page


# Define UI for random distribution application 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("MotionDetection"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
      helpText("This program computes the anglar velocity of experimental mice. 
               It takes three-point coordinates, and computes the head turing
               angle. It also reports whether the angle is clockwise -- POSITIVE
               or, counter-clockwise -- NEGATIVE"),
      br(),
      helpText("The example shown was from a set of velocity data. Please make a
            selection from the bottom window to view the results."),

      br(),
      selectInput("var", "Choose a plot:", 
                  choices = c("Angle scatter plot", "Angle density"),
                  selected = "Angle scatter plot")

    ),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Plot", plotOutput("plot")), 
                  tabPanel("Summary", verbatimTextOutput("summary")
                  ) 
             )
    )
  )
))