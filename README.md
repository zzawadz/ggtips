## ggtips - interactive tooltips for ggplots

Our package provides a set of functions to easily enhance your ggplots with eye-pleasing tooltips, and render them in your Shiny apps.

### Installation

To install ggtips, use devtools:

```
    devtools::install_github("Roche/ggtips")
```

### Usage:

The main function - `renderWithTooltips()` - is a wrapper around Shiny's `renderUI()`. To use it in your app, define an `uiOutput` element in the UI section (this will be the container for your plot), and substiture `renderPlot()` in your server logic with `renderWithTooltips()`, providing a list of variables you want to see in the tooltips:

```
    output[["myPlot"]] <- renderWithTooltips(
      plot = ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length)) + geom_point(aes(colour = Species)),
      varDict = list(Sepal.Width = "Width", Sepal.Length = "Length", Species = "Species")
    )
```

See our demo application (type `ggtips::demo()`) for a more concrete example. (You can build a Dockerized version: `sudo docker build -t ggtips -f inst/Dockerfile .`)
