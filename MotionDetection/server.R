library(shiny)
library(plotrix)
source("functionCollections.R")
dt <- read.csv("data/velocity.csv")



#===========================
## computing the angles
##==========================
all.results <- c(list())
angles <- c()
signs <- c()
for (k in 1: (length(dt$x) -2 ))
{
  temp.result <- aoflip(dt$x[k],dt$x[k+1],dt$x[k+2],dt$y[k],dt$y[k+1],dt$y[k+2])
  if (temp.result >= 0) {
    temp.sign = "Pos"
  }else{
    temp.sign = "Neg"
    temp.result = -temp.result
  }
  temp.list <- list (Angle = temp.result, Direction = temp.sign)
  all.results[[k]] <- temp.list
  angles[k] = temp.result
  signs[k] = temp.sign
}  
result <- list (A = angles, S = signs)

##=======================================
##  Done with helper functions run
##======================================

##  Now doing the shinyserver codes here...

# Define server logic for random distribution application
shinyServer(function(input, output, session) {
  

  choiceInput <- reactive({
        switch(input$var,
                   "Angle scatter plot" = "Scatter", 
                   "Angle density"      = "Density")
  })
  
  resultFile <- NULL
  
  # Generate a plot of the data. Also uses the inputs to build
  # the plot label. Note that the dependencies on both the inputs
  # and the data reactive expression are both tracked, and
  # all expressions are called in the sequence implied by the
  # dependency graph
  output$plot <- renderPlot({
    
    choices <- choiceInput()
    if (choices == "Scatter"){
      plot(result$A, pch = 19,
         xlab = "Movement detection order", ylab = "Angle -- degree [0 - 180]",
         col = as.factor(result$S), cex = .6)
      legend("top", c("Neg", "Pos"), lty = 1:2, col =as.factor(result$S) ,  adj = c(0, 0.6))
    }else if (choices == "Density"){
      plot(density(abs(result$A[which(result$S == "Neg")])), main = "Distribution of angles", 
           #  xlim = c(0,180),
           xlab = "Angle -- degree [0 - 180]")
      lines(density(abs(result$A[which(result$S == "Pos")])), col = "red", lty = 2)
      legend(160, 0.033, c("Neg", "Pos"), lty = 1:2, col =as.factor(result$S) ,  adj = c(0, 0.6))
    }else{}
  })
  
  
  #Second tab when plotting points are selected!!
  
  pointValues<- eventReactive(input$PointsPlot, {
    point1 <- input$Point1
    nums1 <- strsplit (point1, ",")
    x1 <- as.numeric(nums1[[1]][1])
    y1 <- as.numeric(nums1[[1]][2])
    
    point2 <- input$Point2
    nums2 <- strsplit (point2, ",")
    x2 <- as.numeric(nums2[[1]][1])
    y2 <- as.numeric(nums2[[1]][2])
    
    point3 <- input$Point3
    nums3 <- strsplit (point3, ",")
    x3 <- as.numeric(nums3[[1]][1])
    y3 <- as.numeric(nums3[[1]][2])
    x <- c(x1,x2,x3)
    y <- c(y1,y2,y3)
    list("xval" = x, "yval"=y)
  })
  

    output$points <- renderPlot({   
 
        points.in <- pointValues()
        x = points.in$xval
        y = points.in$yval


      temp.angle <- aoflip(x[1],x[2],x[3],y[1],y[2],y[3])

      myREVaxis(x,y,yside=2, xside = 1,  main = isolate("Three points plots"))     
      
      segments(x[1],-y[1], x[2],-y[2])
      segments(x[2],-y[2], x[3],-y[3], col=3)
      m <- (y[2]-y[1])/(x[2]-x[1])
      b <- y[1] - m*x[1]
      y3.prime <- m*x[3] + b

      if (temp.angle > 0 ) {
        segments(x[2],-y[2], x[3],-y3.prime, col="red",lty=3)
        leg <- c ("The direction is clock-wise")
        leg.col <- "red"
      }
      else{
        segments(x[2],-y[2], x[3],-y3.prime, col="blue",lty=3)
        leg <- c ("The direction is counter clock-wise")
        leg.col <- "blue"
      }
      legend ("topright", legend = leg, text.col = leg.col)
    })


    #Third tab, allow user to enter dataset
    datasetInput <- reactive({
        inFile <- input$file1
        if (is.null(inFile))
          return(NULL)
        read.csv(inFile$datapath, header = input$header)
     })
      
    output$view <- renderTable({  
      head(datasetInput(), n = 10)
    })
    
  #  index = 0
    data <- eventReactive(input$go, {
      resultFile  <- computeAngles(datasetInput())
      
    })
    
    output$Results <-  renderTable({  
      head(data(),  n =  10)
    })
    observe({
      if (dim(data())[1] > 1) {
        # notify the browser that the data is ready to download
        session$sendCustomMessage("download_ready", list(fileSize=floor(runif(1) * 10000)))
      }
    })
    output$data_file <- downloadHandler(
      filename = function() {
        paste('MotionDetection-', Sys.Date(), '.csv', sep='')
      },
      content = function(file) {
        write.csv(data(), file, row.names = FALSE)
      }
    )
  
})