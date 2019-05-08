context("Testing main (exported) functions")
library(ggplot2)


# Prepare input data ------------------------------------------------------

testPlot <- ggplot(
  data = iris,
  mapping = aes(x = Sepal.Width, y = Sepal.Length)
) +
  geom_point(mapping = aes(colour = Petal.Length)) +
  facet_wrap(~ Species)

varDict <- list(Species = "Species", Sepal.Length = "Sepal Length")


# Test routines -----------------------------------------------------------

test_that("renderWithTooltips()", {
  handler <- renderWithTooltips(plot = testPlot, varDict = varDict)
  expect_message(
    try(handler(), silent = TRUE),
    "Saving .* image"
  )
})
