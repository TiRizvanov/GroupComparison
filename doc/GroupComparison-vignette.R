## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----violin-plot, message=FALSE, warning=FALSE--------------------------------
library(GroupComparison)
library(ggplot2)

# Example data
data <- data.frame(
  group = rep(c("A", "B", "C"), each = 100),
  value = c(rnorm(100, mean = 0), rnorm(100, mean = 1), rnorm(100, mean = 2))
)

# Custom group colors
group_colors <- c('A' = "#8DA0CB80", 'B' = "#66C2A580", 'C' = "#E41A1C80")

# Create a violin plot
p <- create_violin_plot(
  data = data,
  group_column = "group",
  value_column = "value",
  group_colors = group_colors,
  p_value = TRUE,
  p_value_format = "asterisk",
  y_lab = bquote(bold(Delta ~ Log(activity))),
  breaks = c(-3, 0, 1, 2, 3),
  limits = c(-3, 3)
)

# Plot the violin plot
print(p)

## ----violin-plot-comparison, message=FALSE, warning=FALSE---------------------
# Example data
data <- data.frame(
  group = rep(c("A", "B", "C"), each = 100),
  value = c(rnorm(100, mean = 0), rnorm(100, mean = 1), rnorm(100, mean = 2))
)

# Create a violin plot with a comparison group
p_comparison <- violin_plots_with_comparison_group(
  data = data,
  group_column = "group",
  value_column = "value",
  comparison_group = "A",
  p_value = TRUE,
  p_value_format = "asterisk",
  y_lab = bquote(bold(Delta ~ Log(activity))),
  breaks = c(-3, 0, 1, 2, 3),
  limits = c(-3, 3)
)

# Plot the violin plot with comparison group
print(p_comparison)

## ----split-violin-plot, message=FALSE, warning=FALSE--------------------------
# Example data with a split criterion
data <- data.frame(
  group = rep(c("A", "B", "C"), each = 100),
  value = c(rnorm(100, mean = 0), rnorm(100, mean = 1), rnorm(100, mean = 2)),
  split_criteria = sample(c(TRUE, FALSE), 300, replace = TRUE)
)

# Colors for the split halves
colors <- c("TRUE" = "#FF9999", "FALSE" = "#99CCFF")

# Labels for the legend
labels <- c("TRUE" = "First Group", "FALSE" = "Second Group")

# Create a split violin plot
p_split <- split_violin_plot(
  data = data,
  group_column = "group",
  value_column = "value",
  split_column = "split_criteria",
  colors = colors,
  labels = labels,
  x_lab = "Groups",
  y_lab = bquote(bold(Delta ~ Log(activity))),
  outliers = TRUE,
  CI = TRUE,
  median = TRUE,
  n_obs = TRUE
)

# Plot the split violin plot
print(p_split)

