library(shiny)
library(ggtips)

fluidPage(
  title = "ggtips :: example app",
  titlePanel("Example: iris"),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      selectInput(
        inputId = "x_aes",
        label = "X variable",
        choices = irisVariables,
        selected = irisVariables[[1]]
      ),
      selectInput(
        inputId = "y_aes",
        label = "Y variable",
        choices = irisVariables,
        selected = irisVariables[[3]]
      )
    ),
    mainPanel = mainPanel(
      uiOutput(outputId = "myPlot") # container for the plot
    )
  )
)
