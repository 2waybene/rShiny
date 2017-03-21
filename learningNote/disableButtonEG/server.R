library(shiny)

fakeDataProcessing <- function(duration) {
  # does nothing but sleep for "duration" seconds while
  # pretending some background task is going on...
  Sys.sleep(duration)
}

shinyServer(function(input, output, session) {
  
  observe({
    if (input$start_proc > 0) {
      fakeDataProcessing(5)
      # notify the browser that the data is ready to download
      session$sendCustomMessage("download_ready", list(fileSize=floor(runif(1) * 10000)))
    }
  })
  
  output$data_file <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(data.frame(x=runif(5), y=rnorm(5)), file)
    }
  )
})
