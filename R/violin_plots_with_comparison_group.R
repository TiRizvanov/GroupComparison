#' Create Violin Plots with a Comparison Group
#'
#' This function creates violin plots for different groups, with a specific comparison group
#' highlighted. The comparison group is displayed in a different color, and the function can
#' optionally calculate p-values for comparisons between the other groups and the comparison group.
#' Additionally, it allows for absolute value transformation, adding a chart title, and includes
#' footer text with statistical test information.
#'
#' @param data A data frame containing the data to plot.
#' @param group_column A string representing the name of the column that defines the groups (x-axis).
#' @param value_column A string representing the name of the column containing the values (y-axis).
#' @param comparison_group A string representing the name of the group to be used as the comparison group.
#' @param basic_color A string representing the color for the non-comparison groups. Default is "#8DA0CB80".
#' @param comparison_color A string representing the color for the comparison group. Default is "#66C2A580".
#' @param x_labels A character vector representing the custom labels for the x-axis groups. Default is `NULL`.
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
#' @param abs Logical, whether to transform all data to their absolute values. Default is FALSE.
#' @param name An optional string for the title of the chart (default: empty).
#'
#' @return A `ggplot2` object representing the violin plot with custom annotations.
#' @import ggplot2 dplyr ggpubr rlang gridExtra grid colorspace
#' @export
#'
#' @examples
#' # Example usage with mock data
#' data <- data.frame(
#'   group = rep(c("A", "B", "C"), each = 100),
#'   value = c(rnorm(100, mean = 0), rnorm(100, mean = 1), rnorm(100, mean = 2))
#' )
#' p <- violin_plots_with_comparison_group(
#'   data, "group", "value", comparison_group = "A",
#'   name = "Violin Plot Comparison",
#'   abs = TRUE
#' )
#' print(p)
violin_plots_with_comparison_group <- function(data, group_column, value_column, comparison_group,
                                               basic_color = "#8DA0CB80", comparison_color = "#66C2A580",
                                               x_labels = NULL, breaks = NULL, limits = NULL,
                                               outliers_color = "red", x_lab = "", y_lab = NULL,
                                               p_value = TRUE, p_value_format = "asterisk",
                                               box_plot = TRUE, outliers = TRUE, median = TRUE,
                                               abs = FALSE, name = "") {

  # Load required packages
  require(ggplot2)
  require(dplyr)
  require(ggpubr)
  require(rlang)
  require(gridExtra)
  require(grid)
  require(colorspace)

  # Rename columns for consistency
  data <- data %>%
    rename(delta = !!rlang::sym(value_column), group_label = !!rlang::sym(group_column))

  # Set default y-axis label to the name of the value column if not provided
  if (is.null(y_lab)) {
    y_lab <- value_column
  }

  # Ensure comparison group is in the data
  if (!(comparison_group %in% unique(data$group_label))) {
    stop("Comparison group is not found in the data.")
  }

  # Transform to absolute values if abs = TRUE
  if (abs) {
    data <- data %>%
      mutate(delta = abs(delta))
  }

  # Calculate p-values for pairwise comparisons with the comparison group
  if (p_value) {
    pairwise_groups <- setdiff(unique(data$group_label), comparison_group)
    p_values <- lapply(pairwise_groups, function(group) {
      data1 <- data %>% filter(group_label == group)
      data2 <- data %>% filter(group_label == comparison_group)
      # Perform Kolmogorov–Smirnov test
      p_val <- ks.test(data1$delta, data2$delta)$p.value
      return(data.frame(group1 = group, group2 = comparison_group, p_value = p_val))
    })

    # Combine and adjust p-values using the Bonferroni method
    p_values_df <- do.call(rbind, p_values)
    p_values_df$p_value_adj <- p.adjust(p_values_df$p_value, method = "bonferroni")

    # Generate labels
    p_values_df$label <- ifelse(p_value_format == "asterisk",
                                ifelse(p_values_df$p_value_adj < 0.001, "***",
                                       ifelse(p_values_df$p_value_adj < 0.01, "**",
                                              ifelse(p_values_df$p_value_adj < 0.05, "*", format(round(p_values_df$p_value_adj, 2), nsmall = 2)))),
                                format(round(p_values_df$p_value_adj, 3), nsmall = 3))

    # Assign y.position for p-values
    max_delta <- max(data$delta, na.rm = TRUE)
    data_range <- diff(range(data$delta, na.rm = TRUE))
    p_values_df$y.position <- max_delta + 0.05 * data_range + (1:nrow(p_values_df)) * 0.05 * data_range
  }

  # Sort groups with comparison group in the middle
  group_levels <- unique(data$group_label)
  other_groups <- setdiff(group_levels, comparison_group)
  total_groups <- length(group_levels)
  middle_position <- ceiling(total_groups / 2)

  new_order <- if (total_groups %% 2 == 0) {
    # For even number of groups, comparison group goes just after the middle
    append(other_groups, comparison_group, after = middle_position - 1)
  } else {
    # For odd number of groups, comparison group goes in the middle
    append(other_groups, comparison_group, after = middle_position - 1)
  }

  # Reorder factor levels and adjust group positions
  data$group_label <- factor(data$group_label, levels = new_order)

  # Assign colors to groups (comparison group gets special color)
  group_colors <- rep(basic_color, length(new_order))
  names(group_colors) <- new_order
  group_colors[comparison_group] <- comparison_color

  # Create a function to calculate 95% confidence interval limits
  calc_ci <- function(x) {
    q <- quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
    return(q)
  }

  # Calculate the confidence intervals and identify outliers
  summary_stats <- data %>%
    group_by(group_label) %>%
    summarize(
      median = median(delta, na.rm = TRUE),
      Q1 = quantile(delta, 0.25, na.rm = TRUE),
      Q3 = quantile(delta, 0.75, na.rm = TRUE),
      n = n(),
      lower_ci = ifelse(abs, 0, calc_ci(delta)[1]),
      upper_ci = calc_ci(delta)[2],
      .groups = 'drop'
    )

  # Filter data for violin plot (excluding outliers)
  data_violin <- data %>%
    left_join(summary_stats, by = "group_label") %>%
    filter(delta >= lower_ci & delta <= upper_ci)

  # Identify outliers
  if (abs) {
    data_outliers <- data %>%
      left_join(summary_stats, by = "group_label") %>%
      filter(delta > upper_ci)
  } else {
    data_outliers <- data %>%
      left_join(summary_stats, by = "group_label") %>%
      filter(delta < lower_ci | delta > upper_ci)
  }

  # Create the base violin plot
  p <- ggplot() +
    geom_violin(
      data = data_violin,
      aes(x = group_label, y = delta, fill = group_label),
      trim = FALSE, show.legend = FALSE, width = 0.8, adjust = 2.2
    ) +
    scale_fill_manual(values = group_colors) +
    labs(x = x_lab, y = y_lab) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 12, face = "bold"),
      axis.text.y = element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(0.02, "lines")
    )

  # Conditionally add boxplots
  if (box_plot) {
    p <- p + geom_boxplot(
      data = data_violin,
      aes(x = group_label, y = delta),
      width = 0.1, outlier.shape = NA,
      color = "#4D4D4D", fill = "#4D4D4D",
      notch = FALSE, show.legend = FALSE
    )
  }

  # Conditionally add medians
  if (median) {
    p <- p + stat_summary(
      data = data_violin,
      aes(x = group_label, y = delta),
      fun = "median",
      geom = "point",
      shape = 21, size = 2, fill = "white", color = "white",
      show.legend = FALSE
    )
  }

  # Conditionally add outliers
  if (outliers) {
    p <- p +
      geom_point(
        data = data_outliers,
        aes(x = group_label, y = delta),
        color = outliers_color, size = 1, show.legend = FALSE
      )
  }

  # Conditionally add p-values and comparison brackets
  if (p_value) {
    p <- p + ggpubr::stat_pvalue_manual(
      p_values_df,
      label = "label",
      xmin = "group1",
      xmax = "group2",
      y.position = "y.position",
      tip.length = 0.01,
      step.increase = 0.05,
      bracket.size = 0.5
    )
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

  # Create custom legend
  legend <- grid::grobTree(
    grid::textGrob("Legend:", x = 0.1, y = 0.9, hjust = 0, gp = grid::gpar(fontface = "bold")),
    grid::rectGrob(x = 0.15, y = 0.85, width = 0.02, height = 0.02,
                  gp = grid::gpar(fill = group_colors[comparison_group], col = darken(group_colors[comparison_group], 0.15))),
    grid::textGrob(labels[comparison_group], x = 0.21, y = 0.85, hjust = 0),
    grid::rectGrob(x = 0.15, y = 0.8, width = 0.02, height = 0.02,
                  gp = grid::gpar(fill = setdiff(group_colors, group_colors[comparison_group]), 
                                  col = darken(setdiff(group_colors, group_colors[comparison_group]), 0.15))),
    grid::textGrob(setdiff(labels, labels[comparison_group]), x = 0.21, y = 0.8, hjust = 0)
  )

  # Create footer text with "Kolmogorov–Smirnov test" and "Bonferroni" in bold
  footer_text <- grid::textGrob(
    expression(paste("pwc: ", bold("Kolmogorov–Smirnov test"), "; p.adjust: ", bold("Bonferroni"))),
    x = 0.99, y = 0.01, hjust = 1, vjust = 0,
    gp = grid::gpar(fontsize = 9)
  )

  # Combine plot, legend, and footer
  combined_plot <- gridExtra::grid.arrange(
    p, legend,
    footer_text,
    ncol = 2, nrow = 2,
    widths = c(3, 1),
    heights = c(10, 1.5),
    layout_matrix = rbind(c(1, 2),
                          c(3, 3))
  )

  return(combined_plot)
}

#' @importFrom colorspace darken

