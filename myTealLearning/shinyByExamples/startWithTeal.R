library(teal)

app <- init(
  data = teal_data(iris = iris),
  modules = list(
    module(
      label = "iris histogram",
      server = function(input, output, session, data) {
        updateSelectInput(session = session,
                          inputId =  "var",
                          choices = names(data()[["iris"]])[1:4])
        
        output$hist <- renderPlot({
          req(input$var)
          hist(x = data()[["iris"]][[input$var]])
        })
      },
      ui = function(id) {
        ns <- NS(id)
        list(
          selectInput(inputId = ns("var"),
                      label =  "Column name",
                      choices = NULL),
          plotOutput(outputId = ns("hist"))
        )
      }
    )
  )
)

shinyApp(app$ui, app$server)