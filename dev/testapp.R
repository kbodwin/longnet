# Simple example
library(shiny)
library(shinyWidgets)

ui <- fluidPage(
  pickerInput(inputId = "somevalue",
              label = "A label",
              choices = c("a", "b", "c"),
              options = list(`actions-box` = TRUE),
              multiple = T),
  verbatimTextOutput("value")
)
server <- function(input, output) {
  output$value <- renderPrint({ input$somevalue })
}
shinyApp(ui, server)
