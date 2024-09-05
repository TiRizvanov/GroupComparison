library(testthat)
library(GroupComparison)
library(ggplot2)

# Tests for split_violin_plot

test_that("split_violin_plot works with valid input", {
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 100),
    value = c(rnorm(100, mean = 0), rnorm(100, mean = 1), rnorm(100, mean = 2)),
    split_criteria = sample(c(TRUE, FALSE), 300, replace = TRUE)
  )

  colors <- c("TRUE" = "#FF9999", "FALSE" = "#99CCFF")
  labels <- c("TRUE" = "first group", "FALSE" = "second group")

  p <- split_violin_plot(data, "group", "value", "split_criteria", colors, labels = labels)

  expect_true(!is.null(p), info = "The plot object should not be NULL")
})

test_that("axis labels are customizable", {
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 100),
    value = c(rnorm(100, mean = 0), rnorm(100, mean = 1), rnorm(100, mean = 2)),
    split_criteria = sample(c(TRUE, FALSE), 300, replace = TRUE)
  )

  colors <- c("TRUE" = "#FF9999", "FALSE" = "#99CCFF")

  p <- split_violin_plot(data, "group", "value", "split_criteria", colors,
                         x_lab = "Custom X", y_lab = "Custom Y")

  expect_true(!is.null(p), info = "The plot object should not be NULL")
})

test_that("y-axis breaks and limits can be customized", {
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 100),
    value = c(rnorm(100, mean = 0), rnorm(100, mean = 1), rnorm(100, mean = 2)),
    split_criteria = sample(c(TRUE, FALSE), 300, replace = TRUE)
  )

  colors <- c("TRUE" = "#FF9999", "FALSE" = "#99CCFF")

  p <- split_violin_plot(data, "group", "value", "split_criteria", colors,
                         breaks = c(-3, 0, 3), limits = c(-5, 5))

  expect_true(!is.null(p), info = "The plot object should not be NULL")
})

test_that("outliers can be included or excluded", {
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 100),
    value = c(rnorm(100, mean = 0), rnorm(100, mean = 1), rnorm(100, mean = 2)),
    split_criteria = sample(c(TRUE, FALSE), 300, replace = TRUE)
  )

  colors <- c("TRUE" = "#FF9999", "FALSE" = "#99CCFF")

  p <- split_violin_plot(data, "group", "value", "split_criteria", colors, outliers = FALSE)

  expect_true(!is.null(p), info = "The plot object should not be NULL")
})

test_that("confidence intervals (CI) can be toggled", {
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 100),
    value = c(rnorm(100, mean = 0), rnorm(100, mean = 1), rnorm(100, mean = 2)),
    split_criteria = sample(c(TRUE, FALSE), 300, replace = TRUE)
  )

  colors <- c("TRUE" = "#FF9999", "FALSE" = "#99CCFF")

  p <- split_violin_plot(data, "group", "value", "split_criteria", colors, CI = FALSE)

  expect_true(!is.null(p), info = "The plot object should not be NULL")
})

test_that("medians can be included or excluded", {
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 100),
    value = c(rnorm(100, mean = 0), rnorm(100, mean = 1), rnorm(100, mean = 2)),
    split_criteria = sample(c(TRUE, FALSE), 300, replace = TRUE)
  )

  colors <- c("TRUE" = "#FF9999", "FALSE" = "#99CCFF")

  p <- split_violin_plot(data, "group", "value", "split_criteria", colors, median = FALSE)

  expect_true(!is.null(p), info = "The plot object should not be NULL")
})

test_that("number of observations (n) can be toggled", {
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 100),
    value = c(rnorm(100, mean = 0), rnorm(100, mean = 1), rnorm(100, mean = 2)),
    split_criteria = sample(c(TRUE, FALSE), 300, replace = TRUE)
  )

  colors <- c("TRUE" = "#FF9999", "FALSE" = "#99CCFF")

  p <- split_violin_plot(data, "group", "value", "split_criteria", colors, n_obs = TRUE)

  expect_true(!is.null(p), info = "The plot object should not be NULL")
})

test_that("custom legend labels are applied", {
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 100),
    value = c(rnorm(100, mean = 0), rnorm(100, mean = 1), rnorm(100, mean = 2)),
    split_criteria = sample(c(TRUE, FALSE), 300, replace = TRUE)
  )

  colors <- c("TRUE" = "#FF9999", "FALSE" = "#99CCFF")
  labels <- c("TRUE" = "First Group", "FALSE" = "Second Group")

  p <- split_violin_plot(data, "group", "value", "split_criteria", colors, labels = labels)

  expect_true(!is.null(p), info = "The plot object should not be NULL")
})
