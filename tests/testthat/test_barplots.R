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
  x[do.call(order, as.list(x)), ]
}

testGetTooltipData <- function(p, varDict) {
  b <- ggplot2::ggplot_build(p)
  d <- getTooltipData(
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
  expect_equivalent(tts[[1]], tts[[2]])
  expect_equivalent(tts[[1]], tts[[3]])

  tts_data <- lapply(list(p1, p2, p3), function(p) testGetTooltipData(p, varDict))
  expected_output <- data.frame(
    "Auto/Manual" = as.character(rep(c(0, 1), each = 3)),
    "Cylinders" = as.character(rep(c(4, 6, 8), 2)),
    "Value" = c(3, 4, 12, 8, 3, 2)
  )

  expect_equivalent(tts_data[[1]], expected_output)
  expect_equivalent(tts_data[[2]], expected_output)
  expect_equivalent(tts_data[[3]], expected_output)
})

###### facets ----
test_that("geom_bar, geom_col, data pre-aggregated are handled properly - factes", {
  varDict <- list(am = "Auto/Manual", cyl = "Cylinders", gear = "Gear", count = "Value")
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
  expect_equivalent(tts[[1]], tts[[2]])
  expect_equivalent(tts[[1]], tts[[3]])

  tts_data <- lapply(list(p1, p2, p3), function(p) testGetTooltipData(p, varDict))
  expected_output <- readRDS(system.file(
    file.path("testdata", "am_cyl_by_gear_facets.rds"),
    package = "ggtips"
  ))

  expect_equivalent(tts_data[[1]], expected_output)
  expect_equivalent(tts_data[[2]], expected_output)
  expect_equivalent(tts_data[[3]], expected_output)
})

test_that("geom_bar, geom_col, data pre-aggregated are handled properly - grid", {
  varDict <- list(am = "Auto/Manual", cyl = "Cylinders", gear = "Gear", count = "Value")
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
  expect_equivalent(tts[[1]], tts[[2]])
  expect_equivalent(tts[[1]], tts[[3]])

  tts_data <- lapply(list(p1, p2, p3), function(p) testGetTooltipData(p, varDict))
  expected_output <- readRDS(system.file(
    file.path("testdata", "am_cyl_by_gear_facets.rds"),
    package = "ggtips"
  ))

  expect_equivalent(tts_data[[1]], expected_output)
  expect_equivalent(tts_data[[2]], expected_output)
  expect_equivalent(tts_data[[3]], expected_output)
})
####
###### missing data ----
test_that("missing data is handled properly - random cells", {
    d_miss_rand <- readRDS(file = system.file(
      file.path("testdata", "missings_random_cells.rds"),
      package = "ggtips"
    ))
    p <- ggplot(
      data = d_miss_rand,
      aes(x = am, fill = cyl)
    ) + geom_bar()
    tt <- testGetTooltipData(p, varDict)
    expected_output <- readRDS(system.file(
      file.path("testdata", "am_cyl_missings.rds"),
      package = "ggtips"
    ))
    expect_equivalent(tt, expected_output)
})

test_that("missing data is handled properly - whole group all variables", {
    d_miss_cyl <- readRDS(system.file(
      file.path("testdata", "missings_cyl4.rds"),
      package = "ggtips"
    ))
    p <- ggplot(
      data = d_miss_cyl,
      aes(x = am, fill = cyl)
    ) + geom_bar()
    tt <- testGetTooltipData(p, varDict)
    expected_output <- readRDS(system.file(
      file.path("testdata", "am_cyl_missings2.rds"),
      package = "ggtips"
    ))
    expect_equivalent(tt, expected_output)
})

test_that("missing data is handled properly - whole group single variable", {
    d_miss_gear4_am <- readRDS(system.file(
      file.path("testdata", "missings_gear4_am.rds"),
      package = "ggtips"
    ))
    p <- ggplot(
      data = d_miss_gear4_am,
      aes(x = am, fill = cyl)
    ) + geom_bar()
    tt <- testGetTooltipData(p, varDict)
    expected_output <- readRDS(system.file(
      file.path("testdata", "am_cyl_missings3.rds"),
      package = "ggtips"
    ))
    expect_equivalent(tt, expected_output)
})

###### missing data with facets ----
test_that("missing data is handled properly - random cells", {
  d_miss_rand <- readRDS(file = system.file(
    file.path("testdata", "missings_random_cells.rds"),
    package = "ggtips"
  ))
  p <- ggplot(
    data = d_miss_rand,
    aes(x = am, fill = cyl)
  ) + geom_bar() + facet_wrap("gear")
  tt <- testGetTooltipData(p, varDict)
  expected_output <- readRDS(system.file(
    file.path("testdata", "am_cyl_missings4.rds"),
    package = "ggtips"
  ))
  expect_equivalent(tt, expected_output)
})

test_that("missing data is handled properly - random cells", {
  d_miss_cyl <- readRDS(system.file(
    file.path("testdata", "missings_cyl4.rds"),
    package = "ggtips"
  ))
  p <- ggplot(
    data = d_miss_cyl,
    aes(x = am, fill = cyl)
  ) + geom_bar() + facet_wrap("gear")
  tt <- testGetTooltipData(p, varDict)
  expected_output <- readRDS(system.file(
    file.path("testdata", "am_cyl_missings5.rds"),
    package = "ggtips"
  ))
  expect_equivalent(tt, expected_output)
})

###### position dodge ----
test_that("position dodge - no missing data", {
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
  expected_output <- readRDS(system.file(
    file.path("testdata", "dodge1.rds"),
    package = "ggtips"
  ))
  expect_equivalent(tt, expected_output)
})

test_that("position dodge - no missing data, incl. faceting", {
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
  tt <- lapply(list(p0, p1, p2), function(p) testGetTooltipData(p, varDict))
  expected_output <- readRDS(system.file(
    file.path("testdata", "dodge1.rds"),
    package = "ggtips"
  ))
  lapply(tt, function(t) {
    expect_equivalent(t, expected_output)
  })
})

###### tests for only one bar
test_that("One bar with ggplot default fill is handled properly", {
  p0 <- ggplot(data = data.frame(category = factor("X")), aes(x = category))
  p1 <- p0 + geom_bar()
  p2 <- p0 + geom_bar(fill = "#a0b0f0")
  varDict <- list(category = "Category")
  tts <- lapply(list(p1, p2), function(p) {
    tt <- testGetTooltip(p, varDict)
    expect_true(is.list(tt$rect$colors))
    expect_length(tt$rect$colors, 1)
    expect_length(unlist(tt$rect$colors), 1)
    expect_equivalent(tt$rect$data$tooltip, "<ul><li>Category: X</li></ul>")
    tt
  })
  expect_equal(tolower(tts[[2]]$rect$colors[[1]]), "#a0b0f0")
})

###### ~ missing data ----
test_that("missing data is handled properly - random cells - position dodge", {
  d_miss_rand <- readRDS(file = system.file(
    file.path("testdata", "missings_random_cells.rds"),
    package = "ggtips"
  ))
  p <- ggplot(
    data = d_miss_rand,
    aes(x = am, fill = cyl)
  ) + geom_bar(position = "dodge")
  tt <- testGetTooltipData(p, varDict)
  expected_output <- readRDS(system.file(
    file.path("testdata", "dodge3.rds"),
    package = "ggtips"
  ))
  expect_equivalent(tt, expected_output)
})

test_that("missing data is handled properly - whole group all variables - position dodge", {
  d_miss_cyl <- readRDS(system.file(
    file.path("testdata", "missings_cyl4.rds"),
    package = "ggtips"
  ))
  p <- ggplot(
    data = d_miss_cyl,
    aes(x = am, fill = cyl)
  ) + geom_bar(position = "dodge")
  tt <- testGetTooltipData(p, varDict)
  expected_output <- readRDS(system.file(
    file.path("testdata", "dodge4.rds"),
    package = "ggtips"
  ))
  expect_equivalent(tt, expected_output)
})

test_that("missing data is handled properly - whole group single variable - position dodge", {
  d_miss_gear4_am <- readRDS(system.file(
    file.path("testdata", "missings_gear4_am.rds"),
    package = "ggtips"
  ))
  p <- ggplot(
    data = d_miss_gear4_am,
    aes(x = am, fill = cyl)
  ) + geom_bar(position = "dodge")
  tt <- testGetTooltipData(p, varDict)
  expected_output <- readRDS(system.file(
    file.path("testdata", "dodge5.rds"),
    package = "ggtips"
  ))
  expect_equivalent(tt, expected_output)
})
