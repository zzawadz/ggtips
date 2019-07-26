context("Testing functions that produce tooltip data")
library(ggplot2)


# Prepare input data ------------------------------------------------------

testPlot <- ggplot(
  data = iris,
  mapping = aes(x = Sepal.Width, y = Sepal.Length)
) +
  geom_point(mapping = aes(colour = Petal.Length)) +
  facet_wrap(~ Species)

testGrob <- ggplot_build(testPlot)


# Test routines -----------------------------------------------------------

test_that("getTooltips()", {
  varDict <- list(Species = "Species", Sepal.Length = "Sepal Length")
  gt <- gridExtra::grid.arrange(testPlot)[[1]][[1]]

  tooltips <- ggtips:::getTooltips(
    plot = testPlot,
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
})
