library(shiny)

ui <- fluidPage(
  uiOutput("stdout"),
  actionButton("do", "Click Me")
)

server <- function(input, output, session) {
  output$stdout <- renderUI({
    tags$p("HELLO")
  })
  observeEvent(input$do, {
    output$stdout <- renderUI({
      tags$p("WORLD")
    })
  })
}

shinyApp(ui, server)
