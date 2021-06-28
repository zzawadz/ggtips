context("Testing content-handling routines")
library(ggplot2)

# Prepare input data ------------------------------------------------------
prepareTestPlot <- function(df, xLimit = NULL, dummy.aes = FALSE) {
  mapping <- if (dummy.aes) {
    aes(x = Sepal.Width, y = Sepal.Length, dummy.aes = Species)
  } else {
    aes(x = Sepal.Width, y = Sepal.Length)
  }
  testPlot <- ggplot(
    data = df,
    mapping = mapping
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
    testPlot = testPlot,
    testGrob = ggplot_build(testPlot)
  )
}

fullDataPlot <- prepareTestPlot(iris)

df <- iris
df[c(1, 143), "Sepal.Width"] <- NA
df[c(64, 143), "Sepal.Length"] <- NA
missingDataPlot <- prepareTestPlot(df)

# Test routines -----------------------------------------------------------

test_that("layers", {
  aesth <- ggtips:::getLayerAesthetics(fullDataPlot$testPlot)
  expect_gt(length(aesth), 0)
  expect_equal(aesth[[1]], list(x = "Sepal.Width", y = "Sepal.Length", colour = "Petal.Length"))
  
  expect_equal(ggtips:::getLayerGeom(fullDataPlot$testPlot$layers[[1]]), "points")
})

test_that("getTooltipData()", {
  varDict <- list(Species = "Species", Sepal.Length = "Sepal Length")
  fullTooltipData <- ggtips:::getTooltipData(
    plot = fullDataPlot$testPlot,
    built = fullDataPlot$testGrob,
    varDict = varDict,
    plotScales = NULL,
    callback = NULL
  )
  expect_is(fullTooltipData, "list")
  expect_length(fullTooltipData, 1L)
  tt <- fullTooltipData[[1]]
  expect_is(tt, "data.frame")
  expect_named(tt, as.character(varDict))
  expect_equal(nrow(tt), nrow(iris))
  
  missingTooltipData <- ggtips:::getTooltipData(
    plot = missingDataPlot$testPlot,
    built = missingDataPlot$testGrob,
    varDict = varDict,
    plotScales = NULL,
    callback = NULL
  )
  expect_is(missingTooltipData, "list")
  expect_length(missingTooltipData, 1L)
  tt <- missingTooltipData[[1]]
  expect_is(tt, "data.frame")
  expect_named(tt, as.character(varDict))
  expect_equal(nrow(tt), 147) # 3 rows with NAs
  
  limitedPlot <- prepareTestPlot(iris, xLimit = 3, dummy.aes = TRUE)
  limitedTooltipData <- ggtips:::getTooltipData(
    plot = limitedPlot$testPlot,
    built = limitedPlot$testGrob,
    varDict = varDict,
    plotScales = NULL,
    callback = NULL
  )
  expect_is(limitedTooltipData, "list")
  expect_length(limitedTooltipData, 1L)
  tt <- limitedTooltipData[[1]]
  expect_is(tt, "data.frame")
  expect_named(tt, as.character(varDict))
  expect_equal(nrow(tt), 94) # Number of points within range with xlim = 3
})

test_that("Custom case", {
  data <- mpg
  data[["class"]] <- factor(data[["class"]])
  data[124, "cyl"] <- NA
  
  p <- ggplot(
    data = data,
    mapping = aes(x = displ, y = cty, colour = cyl)
  ) + 
    geom_point() + 
    geom_line() + 
    geom_hline(yintercept = 20) + 
    facet_wrap(~ class)
  
  varDict <- list(
    displ = "Display",
    cty = "Cty",
    class = "Class"
  )
  
  data <- ggtips::getSvgAndTooltipdata(
    plot = p,
    varDict = varDict,
    width = 300,
    height = 400,
    plotScales = list(x = "identity", y = "identity"),
    addAttributes = TRUE,
    callback = function(x) paste(x$class, x$cty)
  )
  
  expect_named(data, c("svg", "data"))
  expect_named(data[["data"]], "points")
  expect_length(data[["data"]][["points"]], 1)
  
  df <- data[["data"]][["points"]][[1]]
  expect_is(df, "data.frame")
  expect_true(nrow(df) == 234)
  expect_equal(
    as.character(df$tooltip[123]), 
    "<ul><li>pickup 11</li><li>Display: 5.9</li><li>Cty: 11</li><li>Class: pickup</li></ul>"
  )
  expect_equal(
    as.character(df$tooltip[210]),
    "<ul><li>suv 11</li><li>Display: 5.4</li><li>Cty: 11</li><li>Class: suv</li></ul>"
  )
})
