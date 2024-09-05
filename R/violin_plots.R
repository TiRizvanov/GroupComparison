#' Create Violin Plot with Custom Elements, P-Values, and Flexible Options
#'
#' This function generates a violin plot with additional elements like boxplots, medians, outliers,
#' and p-values for pairwise group comparisons. Users can optionally set y-axis breaks, limits, outlier color,
#' axis labels, and control the inclusion of p-values, boxplots, and medians.
#'
#' @param data A data frame containing the data to plot.
#' @param group_column A string representing the column name that defines the groups (x-axis).
#' @param value_column A string representing the column name that contains the values (y-axis).
#' @param x_labels A character vector representing the custom labels for the x-axis groups. Default is `NULL`.
#' @param group_colors A named character vector of colors, where the names correspond to the group levels and values correspond to the colors.
#' @param breaks An optional numeric vector specifying the breaks on the y-axis. Default is `NULL`.
#' @param limits An optional numeric vector of length two specifying the limits on the y-axis. Default is `NULL`.
#' @param outliers_color A string representing the color of the outliers. Default is "red".
#' @param x_lab A string representing the label for the x-axis (default: "").
#' @param y_lab A string representing the label for the y-axis (default: the `value_column` name).
#' @param p_value Logical, whether to include p-values in the plot. Default is TRUE.
#' @param p_value_format A string, either "asterisk" or "numeric", to control how p-values are shown. Default is "asterisk".
#' @param box_plot Logical, whether to include a boxplot inside the violin plot. Default is TRUE.
#' @param outliers Logical, whether to include outliers on the plot. Default is TRUE.
#' @param median Logical, whether to include the median point on the plot. Default is TRUE.
#'
#' @return A `ggplot2` object representing the violin plot.
#' @import ggplot2 dplyr ggpubr rlang
#' @export
#'
#' @examples
#' # Example usage with mock data
#' data <- data.frame(
#'   group = rep(c("A", "B", "C"), each = 100),
#'   value = c(rnorm(100, mean = 0), rnorm(100, mean = 1), rnorm(100, mean = 2))
#' )
#' group_colors <- c('A' = "#8DA0CB80", 'B' = "#66C2A580", 'C' = "#E41A1C80")
#' p <- create_violin_plot(data, "group", "value", group_colors = group_colors, y_lab = bquote(bold(Delta ~ Log(activity))))
#' print(p)

create_violin_plot <- function(data, group_column, value_column, x_labels = NULL, group_colors,
                               breaks = NULL, limits = NULL, outliers_color = "red",
                               x_lab = "", y_lab = NULL, p_value = TRUE, p_value_format = "asterisk",
                               box_plot = TRUE, outliers = TRUE, median = TRUE) {

  # Rename columns for consistency
  data <- data %>%
    rename(delta = !!rlang::sym(value_column), group_label = !!rlang::sym(group_column))

  # Define the order of the groups for plotting
  group_order <- names(group_colors)

  # Ensure factor levels are correctly ordered
  data$group_label <- factor(data$group_label, levels = group_order)

  # Set default y-axis label to the name of the value column if not provided
  if (is.null(y_lab)) {
    y_lab <- value_column
  }

  # Create a function to calculate 95% confidence interval limits
  calc_ci <- function(x) {
    q <- quantile(x, probs = c(0.025, 0.975))
    return(q)
  }

  # Calculate the confidence intervals and identify outliers
  data <- data %>%
    group_by(group_label) %>%
    mutate(lower_ci = calc_ci(delta)[1],
           upper_ci = calc_ci(delta)[2]) %>%
    ungroup()

  # Filter data for violin plot (excluding outliers)
  data_violin <- data %>%
    filter(delta >= lower_ci & delta <= upper_ci)

  # Identify outliers
  data_outliers <- data %>%
    filter(delta < lower_ci | delta > upper_ci)

  # Calculate p-values for all pairwise comparisons if p_value is TRUE
  if (p_value) {
    pairwise_groups <- combn(levels(data$group_label), 2, simplify = FALSE)
    p_values <- lapply(pairwise_groups, function(pair) {
      data1 <- data %>% filter(group_label == pair[1])
      data2 <- data %>% filter(group_label == pair[2])
      p_value <- wilcox.test(data1$delta, data2$delta)$p.value
      return(data.frame(group1 = pair[1], group2 = pair[2], p_value = p_value))
    })

    p_values_df <- do.call(rbind, p_values) %>%
      mutate(label = if (p_value_format == "asterisk") {
        ifelse(p_value < 0.001, "***",
               ifelse(p_value < 0.01, "**",
                      ifelse(p_value < 0.05, "*", "")))
      } else {
        format(round(p_value, 3), nsmall = 3)
      },
      y.position = max(data$delta) + (1:nrow(.)) * 0.05 * diff(range(data$delta)))
  }

  # Create the base violin plot
  p <- ggplot() +
    geom_violin(data = data_violin, aes(x = group_label, y = delta, fill = group_label), trim = FALSE, show.legend = FALSE, width = 0.8, adjust = 2.2) +
    scale_fill_manual(values = group_colors) +
    labs(x = x_lab, y = y_lab) +
    theme_minimal() +
    theme(axis.title.x = element_text(size = 14, face = "bold"),
          axis.title.y = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 12, face = "bold"),
          axis.text.y = element_text(size = 14, face = "bold"),
          panel.grid.minor = element_blank(),
          panel.spacing = unit(0.02, "lines"))  # Further reduce space between plots

  # Conditionally add elements based on user input
  if (box_plot) {
    p <- p + geom_boxplot(data = data_violin, aes(x = group_label, y = delta), width = 0.1, outlier.shape = NA, color = "#4D4D4D", fill = "#4D4D4D", notch = FALSE, show.legend = FALSE)
  }

  if (median) {
    p <- p + stat_summary(data = data_violin, aes(x = group_label, y = delta), fun = "median", geom = "point", shape = 21, size = 2, fill = "white", color = "white", show.legend = FALSE)
  }

  if (outliers) {
    p <- p + geom_point(data = data_outliers, aes(x = group_label, y = delta), color = outliers_color, size = 1, show.legend = FALSE)
  }

  # Add p-values and comparison brackets if p_value is TRUE
  if (p_value) {
    p <- p + stat_pvalue_manual(p_values_df, label = "label", tip.length = 0.01, step.increase = 0.01)
  }

  # Apply breaks and limits to the y-axis if provided
  if (!is.null(breaks) || !is.null(limits)) {
    p <- p + scale_y_continuous(breaks = breaks, limits = limits)
  }

  # Use group names as x-axis labels if no custom labels are provided
  if (is.null(x_labels)) {
    x_labels <- levels(data$group_label)
  }
  p <- p + scale_x_discrete(labels = x_labels)

  return(p)
}
