library(ggplot2)
library(ggtips)
library(shiny)

STATS <- c("count", "identity")
FACETS <- c(FALSE, TRUE)
GEOMS <- c("geom_bar", "geom_col")
MULTI <- c(FALSE, TRUE)
MISSING_DATA <- c(FALSE, TRUE)
POSITION <- c("stack", "dodge")

TEST_SCENARIOS <- expand.grid(
  stat = STATS,
  facet = FACETS,
  geom = GEOMS,
  missing_data = MISSING_DATA,
  psoition = POSITION,
  multi = MULTI
)

#### helpers: ----
testgetTooltip <- function(p, varDict) {
  gt <- gridExtra::grid.arrange(p)[[1]][[1]]

  ggtips:::getTooltips(
    plot = p,
    varDict = varDict,
    g = gt,
    plotScales = NULL,
    callback = NULL
  )
}

testWithShiny <- function(p, varDict) {
  ui <- fluidPage(uiOutput("plot"))

  server <- function(input, output, session) {
    output$plot <-
      ggtips::renderWithTooltips(plot = p, varDict = varDict)
  }

  shinyApp(ui, server)
}

# use it by simply calling testWithShiny(p, varDict)



#### common (shared across all tests): ----
factor_cols <- c("am", "gear", "cyl", "carb", "vs")
d <- mtcars
d[, factor_cols] <-
  lapply(factor_cols, function(i)
    as.factor(d[, i]))
varDict = list(am = "Auto/Manual", cyl = "Cylinders", count = "Value")

#### TESTS ----
###### no facets ----
print(TEST_SCENARIOS[1, ])
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
    tt <- testgetTooltip(p, varDict)
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
  expected_output <- c(
    tooltip = c(
      "<ul><li>Auto/Manual: 0</li><li>Cylinders: 4</li><li>Value: 3</li></ul>",
      "<ul><li>Auto/Manual: 0</li><li>Cylinders: 6</li><li>Value: 4</li></ul>",
      "<ul><li>Auto/Manual: 0</li><li>Cylinders: 8</li><li>Value: 12</li></ul>",
      "<ul><li>Auto/Manual: 1</li><li>Cylinders: 4</li><li>Value: 8</li></ul>",
      "<ul><li>Auto/Manual: 1</li><li>Cylinders: 6</li><li>Value: 3</li></ul>",
      "<ul><li>Auto/Manual: 1</li><li>Cylinders: 8</li><li>Value: 2</li></ul>"
    )
  )

  expect_equivalent(tts[[1]]$tooltip, expected_output)
  expect_equivalent(tts[[2]]$tooltip, expected_output)
  expect_equivalent(tts[[3]]$tooltip, expected_output)
})

###### facets ----
test_that("geom_bar, geom_col, data pre-aggregated are handled properly - factes", {
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
    tt <- testgetTooltip(p, varDict)
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

  expected_output <- c(
      tooltip = c(
        "<ul><li>Auto/Manual: 0</li><li>Cylinders: 4</li><li>Value: 1</li></ul>",
        "<ul><li>Auto/Manual: 0</li><li>Cylinders: 4</li><li>Value: 2</li></ul>",
        "<ul><li>Auto/Manual: 0</li><li>Cylinders: 6</li><li>Value: 2</li></ul>",
        "<ul><li>Auto/Manual: 0</li><li>Cylinders: 6</li><li>Value: 2</li></ul>",
        "<ul><li>Auto/Manual: 0</li><li>Cylinders: 8</li><li>Value: 12</li></ul>",
        "<ul><li>Auto/Manual: 1</li><li>Cylinders: 4</li><li>Value: 2</li></ul>",
        "<ul><li>Auto/Manual: 1</li><li>Cylinders: 4</li><li>Value: 6</li></ul>",
        "<ul><li>Auto/Manual: 1</li><li>Cylinders: 6</li><li>Value: 1</li></ul>",
        "<ul><li>Auto/Manual: 1</li><li>Cylinders: 6</li><li>Value: 2</li></ul>",
        "<ul><li>Auto/Manual: 1</li><li>Cylinders: 8</li><li>Value: 2</li></ul>"
      )
    )
  expect_equivalent(tts[[1]]$tooltip, expected_output)
  expect_equivalent(tts[[2]]$tooltip, expected_output)
  expect_equivalent(tts[[3]]$tooltip, expected_output)
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
    tt <- testgetTooltip(p, varDict)
    expected_output <- c(
      "<ul><li>Auto/Manual: 0</li><li>Cylinders: 4</li><li>Value: 2</li></ul>",
      "<ul><li>Auto/Manual: 0</li><li>Cylinders: 6</li><li>Value: 4</li></ul>",
      "<ul><li>Auto/Manual: 0</li><li>Cylinders: 8</li><li>Value: 10</li></ul>",
      "<ul><li>Auto/Manual: 0</li><li>Cylinders: NA</li><li>Value: 2</li></ul>",
      "<ul><li>Auto/Manual: 1</li><li>Cylinders: 4</li><li>Value: 5</li></ul>",
      "<ul><li>Auto/Manual: 1</li><li>Cylinders: 6</li><li>Value: 3</li></ul>",
      "<ul><li>Auto/Manual: 1</li><li>Cylinders: 8</li><li>Value: 2</li></ul>",
      "<ul><li>Auto/Manual: 1</li><li>Cylinders: NA</li><li>Value: 1</li></ul>",
      "<ul><li>Auto/Manual: NA</li><li>Cylinders: 4</li><li>Value: 3</li></ul>"
    )
    expect_equivalent(tt$rect$data[order(tt$rect$data$tooltip), "tooltip"], expected_output)
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
    tt <- testgetTooltip(p, varDict)
    expected_output <- c(
      "<ul><li>Auto/Manual: 0</li><li>Cylinders: 6</li><li>Value: 4</li></ul>",
      "<ul><li>Auto/Manual: 0</li><li>Cylinders: 8</li><li>Value: 12</li></ul>",
      "<ul><li>Auto/Manual: 1</li><li>Cylinders: 6</li><li>Value: 3</li></ul>",
      "<ul><li>Auto/Manual: 1</li><li>Cylinders: 8</li><li>Value: 2</li></ul>",
      "<ul><li>Auto/Manual: NA</li><li>Cylinders: NA</li><li>Value: 11</li></ul>"
    )
    expect_equivalent(tt$rect$data[order(tt$rect$data$tooltip), "tooltip"], expected_output)
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
    tt <- testgetTooltip(p, varDict)
    expected_output <- c(
      "<ul><li>Auto/Manual: 0</li><li>Cylinders: 4</li><li>Value: 1</li></ul>",
      "<ul><li>Auto/Manual: 0</li><li>Cylinders: 6</li><li>Value: 2</li></ul>",
      "<ul><li>Auto/Manual: 0</li><li>Cylinders: 8</li><li>Value: 12</li></ul>",
      "<ul><li>Auto/Manual: 1</li><li>Cylinders: 4</li><li>Value: 2</li></ul>",
      "<ul><li>Auto/Manual: 1</li><li>Cylinders: 6</li><li>Value: 1</li></ul>",
      "<ul><li>Auto/Manual: 1</li><li>Cylinders: 8</li><li>Value: 2</li></ul>",
      "<ul><li>Auto/Manual: NA</li><li>Cylinders: 4</li><li>Value: 8</li></ul>",
      "<ul><li>Auto/Manual: NA</li><li>Cylinders: 6</li><li>Value: 4</li></ul>"
    )
    expect_equivalent(tt$rect$data[order(tt$rect$data$tooltip), "tooltip"], expected_output)
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
  tt <- testgetTooltip(p, varDict)
  expected_output <- c(
    "<ul><li>Auto/Manual: 0</li><li>Cylinders: 4</li><li>Value: 1</li></ul>",
    "<ul><li>Auto/Manual: 0</li><li>Cylinders: 4</li><li>Value: 1</li></ul>",
    "<ul><li>Auto/Manual: 0</li><li>Cylinders: 6</li><li>Value: 1</li></ul>",
    "<ul><li>Auto/Manual: 0</li><li>Cylinders: 6</li><li>Value: 1</li></ul>",
    "<ul><li>Auto/Manual: 0</li><li>Cylinders: 6</li><li>Value: 2</li></ul>",
    "<ul><li>Auto/Manual: 0</li><li>Cylinders: 8</li><li>Value: 3</li></ul>",
    "<ul><li>Auto/Manual: 0</li><li>Cylinders: 8</li><li>Value: 7</li></ul>",
    "<ul><li>Auto/Manual: 0</li><li>Cylinders: NA</li><li>Value: 1</li></ul>",
    "<ul><li>Auto/Manual: 0</li><li>Cylinders: NA</li><li>Value: 1</li></ul>",
    "<ul><li>Auto/Manual: 1</li><li>Cylinders: 4</li><li>Value: 5</li></ul>",
    "<ul><li>Auto/Manual: 1</li><li>Cylinders: 6</li><li>Value: 1</li></ul>",
    "<ul><li>Auto/Manual: 1</li><li>Cylinders: 6</li><li>Value: 2</li></ul>",
    "<ul><li>Auto/Manual: 1</li><li>Cylinders: 8</li><li>Value: 2</li></ul>",
    "<ul><li>Auto/Manual: 1</li><li>Cylinders: NA</li><li>Value: 1</li></ul>",
    "<ul><li>Auto/Manual: NA</li><li>Cylinders: 4</li><li>Value: 1</li></ul>",
    "<ul><li>Auto/Manual: NA</li><li>Cylinders: 4</li><li>Value: 1</li></ul>",
    "<ul><li>Auto/Manual: NA</li><li>Cylinders: 4</li><li>Value: 1</li></ul>"
  )
  expect_equivalent(tt$rect$data[order(tt$rect$data$tooltip), "tooltip"], expected_output)
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
  tt <- testgetTooltip(p, varDict)
  expected_output <- c(
    "<ul><li>Auto/Manual: 0</li><li>Cylinders: 6</li><li>Value: 2</li></ul>",
    "<ul><li>Auto/Manual: 0</li><li>Cylinders: 6</li><li>Value: 2</li></ul>",
    "<ul><li>Auto/Manual: 0</li><li>Cylinders: 8</li><li>Value: 12</li></ul>",
    "<ul><li>Auto/Manual: 1</li><li>Cylinders: 6</li><li>Value: 1</li></ul>",
    "<ul><li>Auto/Manual: 1</li><li>Cylinders: 6</li><li>Value: 2</li></ul>",
    "<ul><li>Auto/Manual: 1</li><li>Cylinders: 8</li><li>Value: 2</li></ul>",
    "<ul><li>Auto/Manual: NA</li><li>Cylinders: NA</li><li>Value: 11</li></ul>"
  )
  expect_equivalent(tt$rect$data[order(tt$rect$data$tooltip), "tooltip"], expected_output)
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
  tt <- testgetTooltip(p, varDict)
  expected_output <- c(
    "<ul><li>Age Class: <5</li><li>Mean Height: 0.808</li><li>Sex: M</li></ul>",
    "<ul><li>Age Class: <5</li><li>Mean Height: 0.859</li><li>Sex: F</li></ul>",
    "<ul><li>Age Class: >12</li><li>Mean Height: 1.637</li><li>Sex: F</li></ul>",
    "<ul><li>Age Class: >12</li><li>Mean Height: 1.668</li><li>Sex: M</li></ul>",
    "<ul><li>Age Class: 5-12</li><li>Mean Height: 1.126</li><li>Sex: F</li></ul>",
    "<ul><li>Age Class: 5-12</li><li>Mean Height: 1.239</li><li>Sex: M</li></ul>"
  )
  expect_equivalent(tt$rect$data[order(tt$rect$data$tooltip), "tooltip"], expected_output)
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
  tt <- lapply(list(p0, p1, p2), function(p) testgetTooltip(p, varDict))
  expected_output <- c(
    "<ul><li>Age Class: <5</li><li>Mean Height: 0.808</li><li>Sex: M</li></ul>",
    "<ul><li>Age Class: <5</li><li>Mean Height: 0.859</li><li>Sex: F</li></ul>",
    "<ul><li>Age Class: >12</li><li>Mean Height: 1.637</li><li>Sex: F</li></ul>",
    "<ul><li>Age Class: >12</li><li>Mean Height: 1.668</li><li>Sex: M</li></ul>",
    "<ul><li>Age Class: 5-12</li><li>Mean Height: 1.126</li><li>Sex: F</li></ul>",
    "<ul><li>Age Class: 5-12</li><li>Mean Height: 1.239</li><li>Sex: M</li></ul>"
  )
  lapply(tt, function(t) {
    expect_equivalent(t$rect$data[order(t$rect$data$tooltip), "tooltip"], expected_output)
  })
})

###### tests for only one bar
test_that("One bar with ggplot default fill is handled properly", {
  p0 <- ggplot(data = data.frame(category = factor("X")), aes(x = category))
  p1 <- p0 + geom_bar()
  p2 <- p0 + geom_bar(fill = "#a0b0f0")
  varDict <- list(category = "Category")
  tts <- lapply(list(p1, p2), function(p) {
    tt <- testgetTooltip(p, varDict)
    expect_true(is.list(tt$rect$colors))
    expect_length(tt$rect$colors, 1)
    expect_length(unlist(tt$rect$colors), 1)
    expect_equivalent(tt$rect$data$tooltip, "<ul><li>Category: X</li></ul>")
    tt
  })
  expect_equal(tolower(tts[[2]]$rect$colors[[1]]), "#a0b0f0")
})


TEST_SCENARIOS
# tests for unmapFactor
# origin <- d
# g <- ggplot_build(p)
# g$data
