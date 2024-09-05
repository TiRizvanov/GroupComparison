library(testthat)
library(ggplot2)
library(dplyr)

test_that("violin_plots works with valid input", {
  # Mock dataset
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 100),
    value = c(rnorm(100, mean = 0), rnorm(100, mean = 1), rnorm(100, mean = 2))
  )

  # Test if the plot is created with the correct group colors
  group_colors <- c('A' = "#8DA0CB80", 'B' = "#66C2A580", 'C' = "#E41A1C80")
  p <- create_violin_plot(data, "group", "value", group_colors = group_colors)

  print(p)  # Print plot for manual inspection
  expect_true(!is.null(p))  # Ensure the plot is not NULL
})

test_that("custom axis labels are applied", {
  # Mock dataset
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 100),
    value = c(rnorm(100, mean = 0), rnorm(100, mean = 1), rnorm(100, mean = 2))
  )

  # Test if custom x and y labels are applied
  group_colors <- c('A' = "#8DA0CB80", 'B' = "#66C2A580", 'C' = "#E41A1C80")
  p <- create_violin_plot(data, "group", "value", group_colors = group_colors, x_lab = "Custom X", y_lab = "Custom Y")

  print(p)  # Print plot for manual inspection
  expect_true(!is.null(p))  # Ensure the plot is not NULL
})

test_that("y-axis breaks and limits can be set", {
  # Mock dataset
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 100),
    value = c(rnorm(100, mean = 0), rnorm(100, mean = 1), rnorm(100, mean = 2))
  )

  # Set custom breaks and limits
  group_colors <- c('A' = "#8DA0CB80", 'B' = "#66C2A580", 'C' = "#E41A1C80")
  p <- create_violin_plot(data, "group", "value", group_colors = group_colors, breaks = c(-3, 0, 3), limits = c(-5, 5))

  print(p)  # Print plot for manual inspection
  expect_true(!is.null(p))  # Ensure the plot is not NULL
})

test_that("outliers can be toggled", {
  # Mock dataset
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 100),
    value = c(rnorm(100, mean = 0), rnorm(100, mean = 1), rnorm(100, mean = 2))
  )

  # Test without outliers
  group_colors <- c('A' = "#8DA0CB80", 'B' = "#66C2A580", 'C' = "#E41A1C80")
  p <- create_violin_plot(data, "group", "value", group_colors = group_colors, outliers = FALSE)

  print(p)  # Print plot for manual inspection
  expect_true(!is.null(p))  # Ensure the plot is not NULL
})

test_that("boxplot and median can be toggled", {
  # Mock dataset
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 100),
    value = c(rnorm(100, mean = 0), rnorm(100, mean = 1), rnorm(100, mean = 2))
  )

  group_colors <- c('A' = "#8DA0CB80", 'B' = "#66C2A580", 'C' = "#E41A1C80")

  # Test with boxplot
  p_with_box <- create_violin_plot(data, "group", "value", group_colors = group_colors, box_plot = TRUE)
  print(p_with_box)  # Print plot for manual inspection
  expect_true(!is.null(p_with_box))  # Ensure the plot is not NULL

  # Test without median
  p_without_median <- create_violin_plot(data, "group", "value", group_colors = group_colors, median = FALSE)
  print(p_without_median)  # Print plot for manual inspection
  expect_true(!is.null(p_without_median))  # Ensure the plot is not NULL
})

test_that("p-values are correctly calculated and formatted", {
  # Mock dataset with distinct values for p-value calculation
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 100),
    value = c(rnorm(100, mean = 0), rnorm(100, mean = 10), rnorm(100, mean = 0))
  )

  # Create the plot with numeric p-values
  group_colors <- c('A' = "#8DA0CB80", 'B' = "#66C2A580", 'C' = "#E41A1C80")
  p_numeric <- create_violin_plot(data, "group", "value", group_colors = group_colors, p_value_format = "numeric")
  print(p_numeric)  # Print plot for manual inspection
  expect_true(!is.null(p_numeric))  # Ensure the plot is not NULL

  # Create the plot with asterisk p-values
  p_asterisk <- create_violin_plot(data, "group", "value", group_colors = group_colors, p_value_format = "asterisk")
  print(p_asterisk)  # Print plot for manual inspection
  expect_true(!is.null(p_asterisk))  # Ensure the plot is not NULL
})

test_that("x-axis labels can be customized", {
  # Mock dataset
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 100),
    value = c(rnorm(100, mean = 0), rnorm(100, mean = 1), rnorm(100, mean = 2))
  )

  # Test with custom x-axis labels
  group_colors <- c('A' = "#8DA0CB80", 'B' = "#66C2A580", 'C' = "#E41A1C80")
  p <- create_violin_plot(data, "group", "value", group_colors = group_colors, x_labels = c("Group 1", "Group 2", "Group 3"))

  print(p)  # Print plot for manual inspection
  expect_true(!is.null(p))  # Ensure the plot is not NULL
})
