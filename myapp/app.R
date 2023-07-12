library(shinycustomloader)
library(shiny)

ui <- fluidPage(

  #titlePanel("Hello Shiny!"),
  #includeHTML("hea.html"),

  div(style="display:inline-block; vertical-align: right; padding-right: 3px; width:250px; background-position:right; position:absolute;right:19em",
      sliderInput(inputId = "bins", label = "Vari", min = 0, max = 100, step = 10, value = 20)),#

  div(style=" width:250px; background-position:right; position:absolute;right:1em",
      sliderInput(inputId = "var1", label = "Var", min = 0, max = 100, step = 10, value = 20)),#

  # Main panel for displaying outputs ----
  mainPanel(

    # Output: Histogram ----
    withLoader(plotOutput(outputId = "distPlot"),type = "image", loader = "")

  )
)

server = function(input, output){
  output$distPlot <- renderPlot({

    Sys.sleep(3.5)

    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    hist(x, breaks = bins, col = "#007bc2", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")

  })
}
