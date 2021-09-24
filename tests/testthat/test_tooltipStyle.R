context("Testing css variables for min and max width of a tooltip")

test_that("Character value works", {
  val <- "200px"
  res <- tooltipStyle(val)
  expected_res <- "--ggtips-max-width:200px; --ggtips-min-width:200px"
  expect_equal(res, expected_res)

  val <- "50%"
  res <- tooltipStyle(val)
  expected_res <- "--ggtips-max-width:50%; --ggtips-min-width:50%"
  expect_equal(res, expected_res)
})

test_that("Character vector works", {
  vals <- c("200px", "400px")
  res <- tooltipStyle(vals)
  expected_res <- "--ggtips-max-width:400px; --ggtips-min-width:200px"
  expect_equal(res, expected_res)

  vals <- c("30%", "70%")
  res <- tooltipStyle(vals)
  expected_res <- "--ggtips-max-width:70%; --ggtips-min-width:30%"
  expect_equal(res, expected_res)

  vals <- c("9%", "80%")
  res <- tooltipStyle(vals)
  expected_res <- "--ggtips-max-width:80%; --ggtips-min-width:9%"
  expect_equal(res, expected_res)

  vals <- c("220px", "80%")
  res <- tooltipStyle(vals)
  expected_res <- "--ggtips-max-width:80%; --ggtips-min-width:220px"
  expect_equal(res, expected_res)
})

test_that("Numeric value works", {
  val <- 40
  res <- tooltipStyle(val)
  expected_res <- "--ggtips-max-width:40px; --ggtips-min-width:40px"
  expect_equal(res, expected_res)
})

test_that("Numeric vector works", {
  vals <- c(200, 400)
  res <- tooltipStyle(vals)
  expected_res <- "--ggtips-max-width:400px; --ggtips-min-width:200px"
  expect_equal(res, expected_res)

  vals <- c(400, 200)
  res <- tooltipStyle(vals)
  expected_res <- "--ggtips-max-width:400px; --ggtips-min-width:200px"
  expect_equal(res, expected_res)
})

test_that("Invalid character vector fails", {
  vals <- c("-400px")
  expect_error(tooltipStyle(vals))

  vals <- c("200px", "-400px")
  expect_error(tooltipStyle(vals))

  vals <- c("-30%", "70%")
  expect_error(tooltipStyle(vals))

  vals <- c("-9%", "-80%")
  expect_error(tooltipStyle(vals))

  vals <- c("220px", "-80%")
  expect_error(tooltipStyle(vals))
})

test_that("Invalid numeric vector fails", {
  vals <- c(-400)
  expect_error(tooltipStyle(vals))

  vals <- c(200, -400)
  expect_error(tooltipStyle(vals))
})


test_that("Returns NULL if empty", {
  vals <- NA
  expect_true(is.null(tooltipStyle(vals)))

  vals <- c(NA, NA)
  expect_true(is.null(tooltipStyle(vals)))

  vals <- c(NA, "30")
  expect_true(is.null(tooltipStyle(vals)))

  vals <- c(NA, 30)
  expect_true(is.null(tooltipStyle(vals)))

  vals <- c("30", NA)
  expect_true(is.null(tooltipStyle(vals)))

  vals <- c(30, NA)
  expect_true(is.null(tooltipStyle(vals)))

  vals <- NULL
  expect_true(is.null(tooltipStyle(vals)))

  vals <- c(NA, NULL)
  expect_true(is.null(tooltipStyle(vals)))

  vals <- NULL
  expect_true(is.null(tooltipStyle(vals)))
})

test_that("Handles NULL and proper values well", {
  vals <- c(NULL, "20px")
  res <- tooltipStyle(vals)
  expected_res <- "--ggtips-max-width:20px; --ggtips-min-width:20px"
  expect_equal(res, expected_res)

  vals <- c(NULL, "20%")
  res <- tooltipStyle(vals)
  expected_res <- "--ggtips-max-width:20%; --ggtips-min-width:20%"
  expect_equal(res, expected_res)

  vals <- c(NULL, 20)
  res <- tooltipStyle(vals)
  expected_res <- "--ggtips-max-width:20px; --ggtips-min-width:20px"
  expect_equal(res, expected_res)
})
