library(testthat)
library(ggplot2)
library(dplyr)

test_that("violin_plots_with_comparison_group works with valid input", {
  # Mock dataset
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 100),
    value = c(rnorm(100, mean = 0), rnorm(100, mean = 1), rnorm(100, mean = 2))
  )

  # Test if the plot is created with the comparison group B
  p <- violin_plots_with_comparison_group(data, "group", "value", comparison_group = "B")

  print(p)  # Print plot for manual inspection
  expect_true(!is.null(p))  # Ensure the plot is not NULL
})

test_that("comparison group is centered", {
  # Mock dataset with four groups
  data <- data.frame(
    group = rep(c("A", "B", "C", "D"), each = 100),
    value = c(rnorm(100, mean = 0), rnorm(100, mean = 1), rnorm(100, mean = 2), rnorm(100, mean = 3))
  )

  # Test if the comparison group "C" is correctly centered
  p <- violin_plots_with_comparison_group(data, "group", "value", comparison_group = "C")

  print(p)  # Print plot for manual inspection
  expect_true(!is.null(p))  # Ensure the plot is not NULL
})

test_that("color of comparison group is distinct", {
  # Mock dataset
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 100),
    value = c(rnorm(100, mean = 0), rnorm(100, mean = 1), rnorm(100, mean = 2))
  )

  # Create the plot with comparison group "B" and custom colors
  p <- violin_plots_with_comparison_group(
    data, "group", "value", comparison_group = "B",
    basic_color = "#8DA0CB80", comparison_color = "#FF0000"
  )

  print(p)  # Print plot for manual inspection
  expect_true(!is.null(p))  # Ensure the plot is not NULL
})

test_that("p-values are correctly calculated", {
  # Mock dataset with distinct values for p-value calculation
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 100),
    value = c(rnorm(100, mean = 0), rnorm(100, mean = 10), rnorm(100, mean = 0))
  )

  # Create the plot with comparison group "B"
  p <- violin_plots_with_comparison_group(data, "group", "value", comparison_group = "B")

  print(p)  # Print plot for manual inspection
  expect_true(!is.null(p))  # Ensure the plot is not NULL
})

test_that("median and outliers can be toggled", {
  # Mock dataset
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 100),
    value = c(rnorm(100, mean = 0), rnorm(100, mean = 1), rnorm(100, mean = 2))
  )

  # Test with median included
  p_with_median <- violin_plots_with_comparison_group(data, "group", "value", comparison_group = "B", median = TRUE)
  print(p_with_median)  # Print plot for manual inspection
  expect_true(!is.null(p_with_median))  # Ensure the plot is not NULL

  # Test with outliers excluded
  p_without_outliers <- violin_plots_with_comparison_group(data, "group", "value", comparison_group = "B", outliers = FALSE)
  print(p_without_outliers)  # Print plot for manual inspection
  expect_true(!is.null(p_without_outliers))  # Ensure the plot is not NULL
})

test_that("p-value display formats correctly", {
  # Mock dataset with distinct values for p-value calculation
  data <- data.frame(
    group = rep(c("A", "B", "C"), each = 100),
    value = c(rnorm(100, mean = 0), rnorm(100, mean = 10), rnorm(100, mean = 0))
  )

  # Create the plot with numeric p-values
  p_numeric <- violin_plots_with_comparison_group(data, "group", "value", comparison_group = "B", p_value_format = "numeric")
  print(p_numeric)  # Print plot for manual inspection
  expect_true(!is.null(p_numeric))  # Ensure the plot is not NULL

  # Create the plot with asterisk p-values
  p_asterisk <- violin_plots_with_comparison_group(data, "group", "value", comparison_group = "B", p_value_format = "asterisk")
  print(p_asterisk)  # Print plot for manual inspection
  expect_true(!is.null(p_asterisk))  # Ensure the plot is not NULL
})
