context("Testing content-handling routines")
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

test_that("layers", {
  aesth <- ggtips:::getLayerAesthetics(testPlot)
  expect_gt(length(aesth), 0)
  expect_equal(aesth[[1]], list(x = "Sepal.Width", y = "Sepal.Length"))

  expect_equal(ggtips:::getLayerGeom(testPlot$layers[[1]]), "points")
})

test_that("getTooltipData()", {
  varDict <- list(Species = "Species", Sepal.Length = "Sepal Length")
  tooltipData <- ggtips:::getTooltipData(
    plot = testPlot,
    built = testGrob,
    varDict = varDict,
    plotScales = NULL,
    callback = NULL
  )
  expect_is(tooltipData, "list")
  expect_length(tooltipData, 1L)
  tt <- tooltipData[[1]]
  expect_is(tt, "data.frame")
  expect_named(tt, as.character(varDict))
  expect_equal(nrow(tt), nrow(iris))
})
