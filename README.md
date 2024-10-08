
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GroupComparison

<!-- badges: start -->
<!-- badges: end -->

`GroupComparison` is an R package designed to create violin plots with
advanced customization options, including the ability to compare groups
with split violins, display p-values, medians, confidence intervals, and
outliers. The package provides flexibility to handle different
visualization needs for comparing groups in your data.

## Installation

You can install the development version of `GroupComparison` from
GitHub:

``` r
# If you don't have the 'devtools' package, install it first
install.packages("devtools")

# Install GroupComparison from GitHub
devtools::install_github("https://github.com/TiRizvanov/GroupComparison")
library(GroupComparison)
```

## Functions Overview

### 1. `violin_plots`

The `violin_plots` function generates a violin plot with optional
elements such as boxplots, medians, p-values, and outliers. It allows
users to customize colors, axis labels, and other plot features.

#### Example:

``` r
# Example data
data <- data.frame(
  group = rep(c("A", "B", "C"), each = 100),
  value = c(rnorm(100, mean = 0), rnorm(100, mean = 1), rnorm(100, mean = 2))
)

# Define group colors
group_colors <- c('A' = "#8DA0CB80", 'B' = "#66C2A580", 'C' = "#E41A1C80")

# Create violin plot
p <- violin_plots(data, "group", "value", group_colors = group_colors, y_lab = bquote(bold(Delta ~ Log(activity))))
print(p)
```

### 2. `violin_plots_with_comparison_group`

This function generates violin plots with a highlighted comparison
group, displaying it in the center of the plot for easy visual
comparison with other groups. P-values for comparisons between the
comparison group and other groups can also be displayed.

#### Example:

``` r
# Example data
data <- data.frame(
  group = rep(c("A", "B", "C"), each = 100),
  value = c(rnorm(100, mean = 0), rnorm(100, mean = 1), rnorm(100, mean = 2))
)

# Create violin plot with a comparison group
p <- violin_plots_with_comparison_group(data, "group", "value", comparison_group = "A")
print(p)
```

### 3. `split_violin_plot`

The `split_violin_plot` function allows users to create split violin
plots, where the data is divided into two halves based on a user-defined
split criterion. The plot can display medians, quartiles, confidence
intervals, and outliers.

#### Example:

``` r
# Example data
data <- data.frame(
  group = rep(c("A", "B", "C"), each = 100),
  value = c(rnorm(100, mean = 0), rnorm(100, mean = 1), rnorm(100, mean = 2)),
  split_criteria = sample(c(TRUE, FALSE), 300, replace = TRUE)
)

# Define colors for each split
colors <- c("TRUE" = "#FF9999", "FALSE" = "#99CCFF")
labels <- c("TRUE" = "first group", "FALSE" = "second group")

# Create split violin plot
p <- split_violin_plot(data, "group", "value", "split_criteria", colors, labels = labels,
                       x_lab = "Groups", y_lab = bquote(bold(Delta ~ Log(activity))), 
                       outliers = TRUE, CI = TRUE, median = TRUE, n_obs = TRUE)
print(p)
```

## Contributing

Contributions and feedback are welcome. Please contact me on
<tirizvanov@gmail.com>.

## License

This package is licensed under the MIT License.
