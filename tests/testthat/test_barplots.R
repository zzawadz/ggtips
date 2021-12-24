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
  expected_output <- structure(list(
    tooltip = c(
      "<ul><li>Auto/Manual: 0</li><li>Cylinders: 4</li><li>Value: 3</li></ul>",
      "<ul><li>Auto/Manual: 0</li><li>Cylinders: 6</li><li>Value: 4</li></ul>",
      "<ul><li>Auto/Manual: 0</li><li>Cylinders: 8</li><li>Value: 12</li></ul>",
      "<ul><li>Auto/Manual: 1</li><li>Cylinders: 4</li><li>Value: 8</li></ul>",
      "<ul><li>Auto/Manual: 1</li><li>Cylinders: 6</li><li>Value: 3</li></ul>",
      "<ul><li>Auto/Manual: 1</li><li>Cylinders: 8</li><li>Value: 2</li></ul>"
    ),
    x = c(
      0.121116187226079,
      0.121116187226079,
      0.121116187226079,
      0.5000652938222,
      0.5000652938222,
      0.5000652938222
    ),
    y = c(
      0.0550096885713325,
      0.184185156607651,
      0.35641911398941,
      0.31336062464397,
      0.657828539407487,
      0.787004007443805
    )
  ),
  row.names = c(NA, 6L),
  class = "data.frame")

  expect_equivalent(tts[[1]], expected_output)
  expect_equivalent(tts[[2]], expected_output)
  expect_equivalent(tts[[3]], expected_output)
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

  expected_output <- structure(
    list(
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
      ),
      x = c(
        0.0827625914790071,
        0.364022293624201,
        0.0827625914790071,
        0.364022293624201,
        0.0827625914790071,
        0.768540464051703,
        0.487280761906509,
        0.768540464051703,
        0.487280761906509,
        0.768540464051703
      ),
      y = c(
        0.0986635541430063,
        0.66812342497274,
        0.150432633309346,
        0.771661583305419,
        0.253970791642025,
        0.616354345806401,
        0.461047108307382,
        0.719892504139079,
        0.771661583305419,
        0.771661583305419
      )
    ),
    row.names = c(1L, 4L, 2L, 5L, 3L, 8L, 6L,
                  9L, 7L, 10L),
    class = "data.frame"
  )
  expect_equivalent(tts[[1]], expected_output)
  expect_equivalent(tts[[2]], expected_output)
  expect_equivalent(tts[[3]], expected_output)
})
####
###### missing data ----
test_that("missing data is handled properly", {
    d_miss_rand <- readRDS(system.file(
      file.path("testtada", "missings_random_cells.rds"),
      package = "ggtips"
    ))
    p <- ggplot(
      data = d_miss_rand,
      aes(x = am, fill = cyl)
    ) + geom_bar()
    testgetTooltip(p, varDict)


    d_miss_cyl <- readRDS(system.file(
      file.path("testtada", "missings_cyl4.rds"),
      package = "ggtips"
    ))

    d_miss_gear4_am <- readRDS(system.file(
      file.path("testtada", "missings_gear4_am.rds"),
      package = "ggtips"
    ))
})


# tests for unmapFactor
origin <- d
g <- ggplot_build(p)
g$data
