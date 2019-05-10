library(ggplot2)

# Convenience function for plotting the Iris dataset
plotIris <- function(x_aes, y_aes) {
  ggplot(data = iris, mapping = aes_string(x = x_aes, y = y_aes)) +
    geom_point(mapping = aes(colour = Species))
}

customContentFunction <- function(x) {
  species <- as.character(x$Species)
  symbol <- switch(
    species,
    setosa = "\U0001f33a",
    virginica = "\U0001f338",
    versicolor = "\U0001f33c"
  )
  paste(symbol, species, symbol)
}

function(input, output) {

    # Re-register plot renderer on every input change
    output[["myPlot"]] <- renderWithTooltips(
      plot = plotIris(x_aes = input[["x_aes"]], y_aes = input[["y_aes"]]),
      varDict = {
        # Retrieve aesthetic names and labels from input controls
        x_aes <- input[["x_aes"]]
        x_label <- names(irisVariables[irisVariables == x_aes])
        y_aes <- input[["y_aes"]]
        y_label <- names(irisVariables[irisVariables == y_aes])

        # Create variable dictionary
        # format: list(<variable> = <label>)
        varDict <- structure(
          list(x_label, y_label),
          names = c(x_aes, y_aes)
        )
      },
      callback = customContentFunction,
      width = 8,
      height = 5,
      point.size = 20
    )
}
