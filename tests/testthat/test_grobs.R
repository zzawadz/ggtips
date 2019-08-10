context("Testing grid operations")
library(ggplot2)


# Prepare input data ------------------------------------------------------

# ggplot2 3.x shifts grobs on its plots one col right and one row down
is_ggplot_2.x <- packageVersion("ggplot2")$major == 2L
colShift <- `if`(is_ggplot_2.x, 0L, 1L)

testPlot <- ggplot(
  data = iris,
  mapping = aes(x = Sepal.Width, y = Sepal.Length)
) +
  geom_point(mapping = aes(colour = Petal.Length)) +
  facet_wrap(~ Species)

gt <- gridExtra::grid.arrange(testPlot)[[1]][[1]]


# Test routines -----------------------------------------------------------

test_that("assignLayoutNamesToPanels()", {
  expect_equal(
    ggtips:::assignLayoutNamesToPanels(gt),
    c("panel-1-1", "panel-2-1", "panel-3-1")
  )
})

test_that("grobs and geometries", {
  grob <- ggtips:::nameToGrob(gt, "panel-1-1")
  expect_is(grob, "grob")
  expect_gt(length(grob$children), 0)
  expect_true(all(sapply(grob$children, is, "grob")))

  geoms <- ggtips:::getGeomsFromGrob(grob)
  expect_gt(length(geoms), 0)
  expect_true(all(sapply(geoms, is, "grob")))

  points <- ggtips:::filterGeoms(geoms, "points")
  expect_gt(length(points), 0)
  expect_is(points[[1]], "points")
})

test_that("getGrobCol()", {
  expect_equal(ggtips:::getGrobCol(gt, "panel-1-1"), 4L + colShift)
  expect_equal(ggtips:::getGrobCol(gt, "strip-t-2-1"), 8L + colShift)
  expect_equal(ggtips:::getGrobCol(gt, "axis-b-3-1"), 12L + colShift)
  expect_error(ggtips:::getGrobCol(gt, "i_dont_exist"))
})

test_that("getGrobRow()", {
  expect_equal(ggtips:::getGrobRow(gt, "ylab-l"), 7L + colShift)
  expect_equal(ggtips:::getGrobRow(gt, "title"), 2L + colShift)
  expect_equal(ggtips:::getGrobRow(gt, "guide-box"), 7L + colShift)
  expect_error(ggtips:::getGrobRow(gt, "i_dont_exist"))
})

test_that("getGrobSize()", {
  size <- ggtips:::getGrobSize(gt, "panel-1-1")
  expect_is(size$width, "numeric")
  expect_gt(size$width, 0)
  expect_is(size$height, "numeric")
  expect_gt(size$height, 0)
  expect_error(ggtips:::getGrobSize(gt, "i_dont_exist"))
})

test_that("isNullUnit()", {
  expect_false(ggtips:::isNullUnit(unit(1, "npc")))
  expect_true(ggtips:::isNullUnit(unit(1, "null")))
})

test_that("columns and rows", {
  expect_gt(ggtips:::gridColWidth(gt, 1L), 0)
  expect_equal(ggtips:::gridColWidth(gt, 5L + colShift), 0)
  expect_error(ggtips:::gridColWidth(gt, 100L))
  expect_gt(ggtips:::gridRowHeight(gt, 1L), 0)
  expect_equal(ggtips:::gridRowHeight(gt, 2L), 0)
  expect_error(ggtips:::gridRowHeight(gt, 100L))
})
