context("Testing functions that produce tooltip data")
library(ggplot2)


# Prepare input data ------------------------------------------------------

prepareTestPlot <- function(df, xLimit = NULL) {
  testPlot <- ggplot(
    data = df,
    mapping = aes(x = Sepal.Width, y = Sepal.Length)
  ) + 
    geom_point(mapping = aes(colour = Petal.Length)) +
    facet_wrap(~ Species, scales = "fixed")
  
  if (!is.null(xLimit)) {
    testPlot <- testPlot + 
      coord_cartesian(
        xlim = c(0, xLimit),
        expand = TRUE
      )
  }
  
  list(
    plot = testPlot,
    grob = ggplot_build(testPlot)
  )
}

testPlot <- prepareTestPlot(iris)
limitedPlot <- prepareTestPlot(iris, xLimit = 3)


# Test routines -----------------------------------------------------------

test_that("getTooltips()", {
  varDict <- list(Species = "Species", Sepal.Length = "Sepal Length")
  gt <- gridExtra::grid.arrange(testPlot$plot)[[1]][[1]]

  tooltips <- ggtips:::getTooltips(
    plot = testPlot$plot,
    varDict = varDict,
    plotScales = NULL,
    g = gt,
    callback = NULL,
    addAttributes = TRUE
  )
  expect_is(tooltips, "list")
  expect_length(tooltips, 1L)
  points <- tooltips$points
  expect_is(points, "list")
  expect_length(points, 1L)
  tt <- points[[1]]
  expect_is(tt, "data.frame")
  expect_named(tt, c("tooltip", "coordX", "coordY"))
  expect_equal(nrow(tt), nrow(iris))
  
  gt <- gridExtra::grid.arrange(limitedPlot$plot)[[1]][[1]]
  
  tooltips <- ggtips:::getTooltips(
    plot = limitedPlot$plot,
    varDict = varDict,
    plotScales = NULL,
    g = gt,
    callback = NULL,
    addAttributes = TRUE
  )
  expect_is(tooltips, "list")
  expect_length(tooltips, 1L)
  points <- tooltips$points
  expect_is(points, "list")
  expect_length(points, 1L)
  tt <- points[[1]]
  expect_is(tt, "data.frame")
  expect_named(tt, c("tooltip", "coordX", "coordY"))
  xRange <- limitedPlot$grob$layout$panel_ranges[[1]]$x.range
  nPoints <- nrow(iris[iris$Sepal.Width >= xRange[1] & iris$Sepal.Width <= xRange[2], ])
  expect_equal(nrow(tt), nPoints)
})
