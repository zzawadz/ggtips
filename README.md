## ggtips - interactive tooltips for ggplots

Our package provides a set of functions to enhance your ggplots with eye-pleasing tooltips easily, and render them in your Shiny apps.

### Preview

![plot with tooltips](inst/example/ggtips.png?raw=true)


[Demo shiny app](https://jcubic.shinyapps.io/ggtips/)

### Installation

To install ggtips, use devtools:

```
    devtools::install_github("Roche/ggtips")
```

### Usage:

The main function - `renderWithTooltips()` - works similar to Shiny's `renderUI()`. To use it in your app, define an `uiOutput` element in the UI section (this will be the container for your plot), and substitute `renderPlot()` in your server logic with `renderWithTooltips()`, providing a list of variables you want to see in the tooltips:

```
    output[["myPlot"]] <- renderWithTooltips(
      plot = ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length)) + geom_point(aes(colour = Species)),
      varDict = list(Sepal.Width = "Width", Sepal.Length = "Length", Species = "Species")
    )
```

`renderWithTooltips` function accepts also optional `customGrob` parameter. It allows to pass modificated grob object and as a result output tooltips coordinates will fits its geometries. This is useful if you want to add any extra geometry to your plot, e.g. bar with information or warning.

See our demo application (type `ggtips::demo()`) for a more concrete example. (You can build a Dockerized version: `sudo docker build -t ggtips -f inst/Dockerfile .`)

After building the docker image (which can take a while) use the command:

```
    docker run -p 3838:3838 ggtips
```

The docker image starts in interactive mode (which means you can use ctrl+c to stop it) and exposes the web browser at port 3838. 

Depending on your Docker setup, the exposed application may be available under: 

[http://localhost:3838](http://localhost:3838) 

or may be exposed through the locally running *docker-machine*, which is the typical
case with the Windows operating system that have no Hyper-V enabled. You can find more information about it on [Hyper-V on Windows](https://docs.microsoft.com/en-us/virtualization/hyper-v-on-windows/reference/hyper-v-requirements).

If your Docker setup is using *docker-machine*, you can find it's IP using command `docker-machine ip`, then use the `http://{docker machine IP}:3838` address to connect to demo session. 

See [Docker setup tutorial](https://docs.docker.com/get-started) to learn more about configuring Docker and [Docker Machine Documentation](https://docs.docker.com/machine/get-started/) to learn about *docker-machine* itself.


## License

Copyright 2019 Genentech, Inc.

Released under Genentech Open License