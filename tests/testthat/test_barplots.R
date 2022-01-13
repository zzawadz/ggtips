library(ggplot2)
library(ggtips)

#### helpers: ----
testGetTooltip <- function(p, varDict) {
  gt <- gridExtra::grid.arrange(p)[[1]][[1]]

  ggtips:::getTooltips(
    plot = p,
    varDict = varDict,
    g = gt,
    plotScales = NULL,
    callback = NULL
  )
}

sortDataFrame <- function(x) {
  factors <- which(sapply(x, is.factor))
  x[, factors] <- lapply(factors, function(i) as.character(x[, i]))
  x[do.call(order, as.list(x)), ]
}

testGetTooltipData <- function(p, varDict) {
  b <- ggplot2::ggplot_build(p)
  d <- ggtips:::getTooltipData(
    plot = p,
    built = b,
    varDict = varDict,
    plotScales = NULL,
    callback = NULL
  )
  d <- d[[1]]
  sortDataFrame(d)
}

#### common (shared across all tests): ----
factor_cols <- c("am", "gear", "cyl", "carb", "vs")
d <- mtcars
d[, factor_cols] <-
  lapply(factor_cols, function(i)
    as.factor(d[, i]))
varDict <- list(am = "Auto/Manual", cyl = "Cylinders", count = "Value")

#### TESTS ----
###### no facets ----
test_that("geom_bar, geom_col, data pre-aggregated are handled properly - no factes", {
  local_edition(3)
  p1 <- ggplot(data = d,
               aes(x = am, fill = cyl)) + geom_bar()

  d_counts <- d
  d_counts$count <- 1
  d_counts <- aggregate(count ~ am + cyl, data = d_counts, length)
  p2 <- ggplot(data <- d_counts,
               aes(x = am, y = count, fill = cyl)) + geom_bar(stat = "identity")

  p3 <- ggplot(data = d_counts,
               aes(x = am, y = count, fill = cyl)) + geom_col()

  tts <- lapply(list(p1, p2, p3), function(p) {
    tt <- testGetTooltip(p, varDict)
    expect_type(tt, "list")
    expect_named(tt, c("rect"))
    expect_named(tt[["rect"]], c("data", "colors"))
    expect_length(tt[["rect"]][["data"]], 3)
    expect_equal(class(tt[["rect"]][["data"]]), "data.frame")
    expect_equal(nrow(tt[["rect"]][["data"]]), 6)
    expect_named(tt[["rect"]][["data"]], c("tooltip", "x", "y"))
    tt_data <- tt[["rect"]][["data"]]
    tt_data[order(tt_data$tooltip), ]
  })
  expect_equal(tts[[1]], tts[[2]], ignore_attr = TRUE)
  expect_equal(tts[[3]], tts[[2]], ignore_attr = TRUE)

  tts_data <- lapply(list(p1, p2, p3), function(p) testGetTooltipData(p, varDict))
  expect_snapshot(tts_data[[1]])
  expect_snapshot(tts_data[[2]])
  expect_snapshot(tts_data[[3]])
})

###### facets ----
test_that("geom_bar, geom_col, data pre-aggregated are handled properly - factes", {
  local_edition(3)
  p1 <- ggplot(data = d,
               aes(x = am, fill = cyl)) +
    geom_bar() +
    facet_wrap("gear")

  d_counts <- d
  d_counts$count <- 1
  d_counts <-
    aggregate(count ~ am + cyl + gear, data = d_counts, length)
  p2 <- ggplot(data <- d_counts,
               aes(x = am, y = count, fill = cyl)) +
    geom_bar(stat = "identity") +
    facet_wrap("gear")

  p3 <- ggplot(data = d_counts,
               aes(x = am, y = count, fill = cyl)) +
    geom_col() +
    facet_wrap("gear")

  tts <- lapply(list(p1, p2, p3), function(p) {
    tt <- testGetTooltip(p, varDict)
    expect_type(tt, "list")
    expect_named(tt, c("rect"))
    expect_named(tt[["rect"]], c("data", "colors"))
    expect_length(tt[["rect"]][["data"]], 3)
    expect_equal(class(tt[["rect"]][["data"]]), "data.frame")
    expect_equal(nrow(tt[["rect"]][["data"]]), 10)
    expect_named(tt[["rect"]][["data"]], c("tooltip", "x", "y"))
    tt_data <- tt[["rect"]][["data"]]
    tt_data[order(tt_data$tooltip), ]
  })
  expect_equal(tts[[1]], tts[[2]], ignore_attr = TRUE)
  expect_equal(tts[[1]], tts[[3]], ignore_attr = TRUE)

  tts_data <- lapply(list(p1, p2, p3), function(p) testGetTooltipData(p, varDict))

  expect_snapshot(tts_data[[1]])
  expect_snapshot(tts_data[[2]])
  expect_snapshot(tts_data[[3]])
})

test_that("geom_bar, geom_col, data pre-aggregated are handled properly - grid", {
  local_edition(3)
  p1 <- ggplot(data = d,
               aes(x = am, fill = cyl)) +
    geom_bar() +
    facet_grid(rows = vars(gear), cols = vars(cyl))

  d_counts <- d
  d_counts$count <- 1
  d_counts <-
    aggregate(count ~ am + cyl + gear, data = d_counts, length)
  p2 <- ggplot(data <- d_counts,
               aes(x = am, y = count, fill = cyl)) +
    geom_bar(stat = "identity") +
    facet_grid(rows = vars(gear), cols = vars(cyl))

  p3 <- ggplot(data = d_counts,
               aes(x = am, y = count, fill = cyl)) +
    geom_col() +
    facet_grid(rows = vars(gear), cols = vars(cyl))

  tts <- lapply(list(p1, p2, p3), function(p) {
    tt <- testGetTooltip(p, varDict)
    expect_type(tt, "list")
    expect_named(tt, c("rect"))
    expect_named(tt[["rect"]], c("data", "colors"))
    expect_length(tt[["rect"]][["data"]], 3)
    expect_equal(class(tt[["rect"]][["data"]]), "data.frame")
    expect_equal(nrow(tt[["rect"]][["data"]]), 10)
    expect_named(tt[["rect"]][["data"]], c("tooltip", "x", "y"))
    tt_data <- tt[["rect"]][["data"]]
    tt_data[order(tt_data$tooltip), ]
  })
  expect_equal(tts[[1]], tts[[2]], ignore_attr = TRUE)
  expect_equal(tts[[1]], tts[[3]], ignore_attr = TRUE)

  tts_data <- lapply(list(p1, p2, p3), function(p) testGetTooltipData(p, varDict))

  expect_snapshot(tts_data[[1]])
  expect_snapshot(tts_data[[2]])
  expect_snapshot(tts_data[[3]])
})
####
###### missing data ----
test_that("missing data is handled properly - random cells", {
  local_edition(3)
  d_miss_rand <- readRDS(file = system.file(
    file.path("testdata", "missings_random_cells.rds"),
    package = "ggtips"
  ))
  p <- ggplot(
    data = d_miss_rand,
    aes(x = am, fill = cyl)
  ) + geom_bar()
  tt <- testGetTooltipData(p, varDict)
  expect_snapshot(tt)
})

test_that("missing data is handled properly - whole group all variables", {
  local_edition(3)
  d_miss_cyl <- readRDS(system.file(
    file.path("testdata", "missings_cyl4.rds"),
    package = "ggtips"
  ))
  p <- ggplot(
    data = d_miss_cyl,
    aes(x = am, fill = cyl)
  ) + geom_bar()
  tt <- testGetTooltipData(p, varDict)
  expect_snapshot(tt)
})

test_that("missing data is handled properly - whole group single variable", {
  local_edition(3)
  d_miss_gear4_am <- readRDS(system.file(
    file.path("testdata", "missings_gear4_am.rds"),
    package = "ggtips"
  ))
  p <- ggplot(
    data = d_miss_gear4_am,
    aes(x = am, fill = cyl)
  ) + geom_bar()
  tt <- testGetTooltipData(p, varDict)
  expect_snapshot(tt)
})

###### missing data with facets ----
test_that("missing data is handled properly - random cells 2", {
  local_edition(3)
  d_miss_rand <- readRDS(file = system.file(
    file.path("testdata", "missings_random_cells.rds"),
    package = "ggtips"
  ))
  p <- ggplot(
    data = d_miss_rand,
    aes(x = am, fill = cyl)
  ) + geom_bar() + facet_wrap("gear")
  tt <- testGetTooltipData(p, varDict)
  expect_snapshot(tt)
})

test_that("missing data is handled properly - random cells 3", {
  local_edition(3)
  d_miss_cyl <- readRDS(system.file(
    file.path("testdata", "missings_cyl4.rds"),
    package = "ggtips"
  ))
  p <- ggplot(
    data = d_miss_cyl,
    aes(x = am, fill = cyl)
  ) + geom_bar() + facet_wrap("gear")
  tt <- testGetTooltipData(p, varDict)
  expect_snapshot(tt)
})

###### position dodge ----
test_that("position dodge - no missing data", {
  local_edition(3)
  d <- readRDS(system.file(
    file.path("testdata", "height_synthetic_data.rds"),
    package = "ggtips"
  ))
  p <- ggplot(
    data = d,
    aes(x = age_class, fill = sex, y = height)
  ) + geom_bar(stat = "identity", position = "dodge")
  varDict <- list(age_class = "Age Class",
                  height = "Mean Height",
                  sex = "Sex")
  tt <- testGetTooltipData(p, varDict)
  expect_snapshot(tt)
})

test_that("position dodge - no missing data, incl. faceting", {
  local_edition(3)
  d <- readRDS(system.file(
    file.path("testdata", "height_synthetic_data.rds"),
    package = "ggtips"
  ))
  p0 <- ggplot(
    data = d,
    aes(x = age_class, fill = sex, y = height)
  ) +
    geom_bar(stat = "identity", position = "dodge")
  p1 <- p0 + facet_wrap("sex")
  p2 <- p0 + facet_wrap("age_class")

  varDict <- list(age_class = "Age Class",
                  height = "Mean Height",
                  sex = "Sex")
  lapply(list(p0, p1, p2), function(p) {
    t <- testGetTooltipData(p, varDict)
    expect_snapshot(t)
  })
})

###### tests for only one bar
test_that("One bar with ggplot default fill is handled properly", {
  local_edition(3)
  p0 <- ggplot(data = data.frame(category = factor("X")), aes(x = category))
  p1 <- p0 + geom_bar()
  p2 <- p0 + geom_bar(fill = "#a0b0f0")
  varDict <- list(category = "Category")
  tts <- lapply(list(p1, p2), function(p) {
    tt <- testGetTooltip(p, varDict)
    expect_true(is.list(tt$rect$colors))
    expect_length(tt$rect$colors, 1)
    expect_length(unlist(tt$rect$colors), 1)
    tt_data <- testGetTooltipData(p, varDict)
    expect_equal(tt_data, "X")
    tt
  })
  expect_equal(tolower(tts[[2]]$rect$colors[[1]]), "#a0b0f0")
})

###### ~ missing data ----
test_that("missing data is handled properly - random cells - position dodge", {
  local_edition(3)
  d_miss_rand <- readRDS(file = system.file(
    file.path("testdata", "missings_random_cells.rds"),
    package = "ggtips"
  ))
  p <- ggplot(
    data = d_miss_rand,
    aes(x = am, fill = cyl)
  ) + geom_bar(position = "dodge")
  tt <- testGetTooltipData(p, varDict)
  expect_snapshot(tt)
})

test_that("missing data is handled properly - whole group all variables - position dodge", {
  local_edition(3)
  d_miss_cyl <- readRDS(system.file(
    file.path("testdata", "missings_cyl4.rds"),
    package = "ggtips"
  ))
  p <- ggplot(
    data = d_miss_cyl,
    aes(x = am, fill = cyl)
  ) + geom_bar(position = "dodge")
  tt <- testGetTooltipData(p, varDict)
  expect_snapshot(tt)
})

test_that("missing data is handled properly - whole group single variable - position dodge", {
  local_edition(3)
  d_miss_gear4_am <- readRDS(system.file(
    file.path("testdata", "missings_gear4_am.rds"),
    package = "ggtips"
  ))
  p <- ggplot(
    data = d_miss_gear4_am,
    aes(x = am, fill = cyl)
  ) + geom_bar(position = "dodge")
  tt <- testGetTooltipData(p, varDict)
  expect_snapshot(tt)
})

