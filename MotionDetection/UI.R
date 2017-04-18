
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
               column(3, 
                      actionButton(inputId="PointsPlot", 
                                   "Plot points",
                                   icon = icon("refresh")))
             ),
             fluidRow(
                plotOutput("points")
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
             checkboxInput("header", "Header", TRUE),
               column(3, 
                tableOutput("view"))
            ),
            
            fluidRow(
              actionButton(inputId = "go", 
                           "Compute Angle")
            ),
            singleton(tags$head(HTML(
              '
                <script type="text/javascript">
                 $(document).ready(function() {
                 // disable download at startup. data_file is the id of the downloadButton
                 $("#data_file").attr("disabled", "true").attr("onclick", "return false;");

                Shiny.addCustomMessageHandler("download_ready", function(message) {
                    $("#data_file").removeAttr("disabled").removeAttr("onclick").html(
                     "<i class=\\"fa fa-download\\"></i>Download (file size: " + message.fileSize + ")");
                });
             })
           </script>
            '
            ))),
            
            fluidRow(
              column(5,
              tableOutput("Results"))
            ),
            fluidRow(
              column(5,
                     helpText("Download will be available once the processing is completed."))
            ),
            fluidRow(
              column(2, 
                     downloadButton("data_file"))
            )
            
    )

)

