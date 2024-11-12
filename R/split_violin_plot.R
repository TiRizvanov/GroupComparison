#' Create a Split Violin Plot with Custom Elements and Options
#'
#' This function generates split violin plots with custom elements such as Q1, Q3, medians,
#' confidence intervals, outliers, observation counts, absolute values, p-values, and a custom title.
#' It allows for flexible inclusion or exclusion of these elements, and customization of axis labels.
#'
#' @param data A data frame containing the data to plot.
#' @param group_column A string representing the name of the column that defines the groups (x-axis).
#' @param value_column A string representing the name of the column containing the values (y-axis).
#' @param split_column A string representing the name of the column used to split the violins.
#' @param colors A named vector of two colors to be used for the left and right halves of the violins.
#' @param labels An optional named vector of labels for the legend corresponding to the `split` values.
#' @param x_lab A string representing the label for the x-axis (default: "Groups").
#' @param y_lab A string representing the label for the y-axis (default: the `value_column` name).
#' @param name An optional string for the title of the chart (default: empty).
#' @param breaks An optional vector of breaks for the y-axis.
#' @param limits An optional vector of length two defining the limits of the y-axis.
#' @param outliers Logical, whether to include outliers (default TRUE).
#' @param CI Logical, whether to include confidence intervals (default TRUE).
#' @param median Logical, whether to include medians (default TRUE).
#' @param n_obs Logical, whether to include number of observations (default TRUE).
#' @param abs Logical, whether to use absolute values (default FALSE).
#' @param p_value Logical, whether to include p-values in the plot (default TRUE).
#' @param p_value_format A string, either "asterisk" or "numeric", to control how p-values are shown (default "asterisk").
#'
#' @return A ggplot2 object with the split violin plot and custom legend.
#' @import ggplot2 dplyr gridExtra rlang colorspace grid
#' @export
#'
#' @examples
#' # Example data
#' data <- data.frame(
#'   group = rep(c("A", "B", "C"), each = 100),
#'   value = c(rnorm(100, mean = 0), rnorm(100, mean = 1), rnorm(100, mean = 2)),
#'   split_criteria = sample(c(TRUE, FALSE), 300, replace = TRUE)
#' )
#'
#' colors <- c("TRUE" = "#FF9999", "FALSE" = "#99CCFF")
#' labels <- c("TRUE" = "first group", "FALSE" = "second group")
#'
#' # Generate the split violin plot with custom labels and title
#' p <- split_violin_plot(data, "group", "value", "split_criteria", colors, labels = labels,
#'                        x_lab = "Groups", y_lab = bquote(bold(Delta ~ Log(activity))),
#'                        name = "My Chart Title",
#'                        outliers = TRUE, CI = TRUE, median = TRUE, n_obs = TRUE,
#'                        abs = TRUE, p_value = TRUE, p_value_format = "asterisk")
#' print(p)

split_violin_plot <- function(data, group_column, value_column, split_column, colors, labels = NULL,
                              x_lab = "Groups", y_lab = value_column, name = "",
                              breaks = NULL, limits = NULL,
                              outliers = TRUE, CI = TRUE, median = TRUE, n_obs = TRUE,
                              abs = FALSE, p_value = TRUE, p_value_format = "asterisk") {
  
  # Rename columns for consistency
  data <- data %>%
    dplyr::rename(group = !!rlang::sym(group_column),
                  value = !!rlang::sym(value_column),
                  split = !!rlang::sym(split_column))
  
  # Ensure 'group' and 'split' are character
  data <- data %>%
    mutate(group = as.character(group),
           split = as.character(split))
  
  # If abs is TRUE, take absolute values
  if (abs) {
    data$value <- abs(data$value)
  }
  
  # Default y-axis label is the value_column if not provided
  if (is.null(y_lab)) {
    y_lab <- value_column
  }
  
  # Calculate summary statistics
  summary_stats <- data %>%
    dplyr::group_by(group, split) %>%
    dplyr::summarize(
      median = median(value),
      Q1 = quantile(value, 0.25),
      Q3 = quantile(value, 0.75),
      n = dplyr::n(),
      lower_ci = quantile(value, 0.025),
      upper_ci = quantile(value, 0.975),
      .groups = 'drop'
    )
  
  # Filter outliers based on CI
  if (abs) {
    # When abs = TRUE, we do not show outliers below lower_ci (near zero)
    outlier_data <- data %>%
      dplyr::left_join(summary_stats, by = c("group", "split")) %>%
      dplyr::filter(value > upper_ci)
  } else {
    outlier_data <- data %>%
      dplyr::left_join(summary_stats, by = c("group", "split")) %>%
      dplyr::filter(value < lower_ci | value > upper_ci)
  }
  
  # Filter data for the plot (values inside the CI limits)
  data_filtered <- data %>%
    dplyr::left_join(summary_stats, by = c("group", "split")) %>%
    dplyr::filter(value >= lower_ci & value <= upper_ci)
  
  # Define border and quantile colors (10% darker)
  border_colors <- colorspace::darken(colors, 0.15)
  q_colors <- border_colors
  border_colors_reverse <- c(border_colors[2], border_colors[1])
  
  # Base plot with custom axis labels and optional title
  p <- ggplot2::ggplot(data_filtered, aes(x = factor(group), y = value, fill = split, color = split)) +
    geom_split_violin(trim = FALSE, size = 0.5, show.legend = FALSE, adjust = 2.2) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::scale_color_manual(values = border_colors_reverse) +
    ggplot2::scale_x_discrete() +
    ggplot2::labs(x = x_lab, y = y_lab) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.title.x = element_text(size = 14, face = "bold"),
                   axis.title.y = element_text(size = 14, face = "bold"),
                   axis.text.x = element_text(size = 12, face = "bold"),
                   axis.text.y = element_text(size = 14, face = "bold"),
                   plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                   panel.grid.minor = element_blank(),
                   panel.spacing = unit(0.02, "lines"),
                   legend.position = "none")
  
  # Add title if provided
  if (name != "") {
    p <- p + ggplot2::ggtitle(name)
  }
  
  # Conditionally add breaks and limits for y-axis
  if (!is.null(breaks) || !is.null(limits)) {
    p <- p + scale_y_continuous(breaks = breaks, limits = limits, expand = c(0, 0))
  } else if (abs) {
    # When abs = TRUE and limits are not provided, set y-axis to start at 0
    p <- p + scale_y_continuous(limits = c(0, NA), expand = c(0, 0))
  }
  
  # Conditionally add outliers
  if (outliers) {
    p <- p +
      ggplot2::geom_point(data = outlier_data %>% dplyr::filter(split == names(colors)[1]),
                          aes(x = as.numeric(factor(group)) + 0.0625, y = value),
                          color = border_colors[1], size = 1.5, shape = 21, fill = "white",
                          inherit.aes = FALSE) +
      ggplot2::geom_point(data = outlier_data %>% dplyr::filter(split == names(colors)[2]),
                          aes(x = as.numeric(factor(group)) - 0.0625, y = value),
                          color = border_colors[2], size = 1.5, shape = 21, fill = "white",
                          inherit.aes = FALSE)
  }
  
  # Calculate data range
  data_range <- diff(range(data$value))
  
  # Conditionally add CIs (along with Q1, Q3 lines)
  if (CI) {
    p <- p +
      ggplot2::geom_segment(data = summary_stats %>% dplyr::filter(split == names(colors)[1]),
                            aes(x = as.numeric(factor(group)), xend = as.numeric(factor(group)) + 0.125, y = Q1, yend = Q1),
                            color = scales::alpha(q_colors[1], 0.9), size = 0.5,
                            inherit.aes = FALSE) +
      ggplot2::geom_segment(data = summary_stats %>% dplyr::filter(split == names(colors)[2]),
                            aes(x = as.numeric(factor(group)) - 0.125, xend = as.numeric(factor(group)), y = Q1, yend = Q1),
                            color = scales::alpha(q_colors[2], 0.9), size = 0.5,
                            inherit.aes = FALSE) +
      ggplot2::geom_segment(data = summary_stats %>% dplyr::filter(split == names(colors)[1]),
                            aes(x = as.numeric(factor(group)), xend = as.numeric(factor(group)) + 0.125, y = Q3, yend = Q3),
                            color = scales::alpha(q_colors[1], 0.9), size = 0.5,
                            inherit.aes = FALSE) +
      ggplot2::geom_segment(data = summary_stats %>% dplyr::filter(split == names(colors)[2]),
                            aes(x = as.numeric(factor(group)) - 0.125, xend = as.numeric(factor(group)), y = Q3, yend = Q3),
                            color = scales::alpha(q_colors[2], 0.9), size = 0.5,
                            inherit.aes = FALSE) +
      ggplot2::geom_segment(data = summary_stats %>% dplyr::filter(split == names(colors)[1]),
                            aes(x = as.numeric(factor(group)) + 0.0625, xend = as.numeric(factor(group)) + 0.0625, y = Q1, yend = lower_ci),
                            color = scales::alpha(q_colors[1], 0.9), size = 0.5,
                            inherit.aes = FALSE) +
      ggplot2::geom_segment(data = summary_stats %>% dplyr::filter(split == names(colors)[2]),
                            aes(x = as.numeric(factor(group)) - 0.0625, xend = as.numeric(factor(group)) - 0.0625, y = Q1, yend = lower_ci),
                            color = scales::alpha(q_colors[2], 0.9), size = 0.5,
                            inherit.aes = FALSE) +
      ggplot2::geom_segment(data = summary_stats %>% dplyr::filter(split == names(colors)[1]),
                            aes(x = as.numeric(factor(group)) + 0.0625, xend = as.numeric(factor(group)) + 0.0625, y = Q3, yend = upper_ci),
                            color = scales::alpha(q_colors[1], 0.9), size = 0.5,
                            inherit.aes = FALSE) +
      ggplot2::geom_segment(data = summary_stats %>% dplyr::filter(split == names(colors)[2]),
                            aes(x = as.numeric(factor(group)) - 0.0625, xend = as.numeric(factor(group)) - 0.0625, y = Q3, yend = upper_ci),
                            color = scales::alpha(q_colors[2], 0.9), size = 0.5,
                            inherit.aes = FALSE)
  }
  
  # Conditionally add medians
  if (median) {
    p <- p +
      ggplot2::geom_segment(data = summary_stats %>% dplyr::filter(split == names(colors)[1]),
                            aes(x = as.numeric(factor(group)), xend = as.numeric(factor(group)) + 0.25, y = median, yend = median),
                            color = "black", size = 0.5,
                            inherit.aes = FALSE) +
      ggplot2::geom_segment(data = summary_stats %>% dplyr::filter(split == names(colors)[2]),
                            aes(x = as.numeric(factor(group)) - 0.25, xend = as.numeric(factor(group)), y = median, yend = median),
                            color = "black", size = 0.5,
                            inherit.aes = FALSE)
  }
  
  # Conditionally add number of observations (n)
  if (n_obs) {
    p <- p +
      ggplot2::geom_text(data = summary_stats %>% dplyr::filter(split == names(colors)[1]),
                         aes(x = as.numeric(factor(group)) + 0.15, y = median - 0.05 * data_range, label = paste("n =", n)),
                         color = "black", size = 3,
                         inherit.aes = FALSE) +
      ggplot2::geom_text(data = summary_stats %>% dplyr::filter(split == names(colors)[2]),
                         aes(x = as.numeric(factor(group)) - 0.15, y = median - 0.05 * data_range, label = paste("n =", n)),
                         color = "black", size = 3,
                         inherit.aes = FALSE)
  }
  
  # Default labels use split values if not provided
  if (is.null(labels)) {
    labels <- setNames(names(colors), names(colors))
  }
  
  # Conditionally calculate and add p-values
  if (p_value) {
    # Prepare groups and splits
    groups <- unique(data$group)
    splits <- unique(data$split)
    
    p_values_list <- list()
    
    # Comparisons between splits within the same group
    for (grp in groups) {
      grp_data <- data %>% filter(group == grp)
      if (length(unique(grp_data$split)) > 1) {
        split_pairs <- combn(unique(grp_data$split), 2, simplify = FALSE)
        for (pair in split_pairs) {
          data1 <- grp_data %>% filter(split == pair[1]) %>% pull(value)
          data2 <- grp_data %>% filter(split == pair[2]) %>% pull(value)
          p_val <- ks.test(data1, data2)$p.value
          p_values_list <- append(p_values_list, list(data.frame(
            group1 = grp,
            split1 = pair[1],
            group2 = grp,
            split2 = pair[2],
            p_value = p_val,
            comparison_type = "within"
          )))
        }
      }
    }
    
    # Comparisons between same splits across groups
    for (spl in splits) {
      spl_data <- data %>% filter(split == spl)
      if (length(unique(spl_data$group)) > 1) {
        group_pairs <- combn(unique(spl_data$group), 2, simplify = FALSE)
        for (pair in group_pairs) {
          data1 <- spl_data %>% filter(group == pair[1]) %>% pull(value)
          data2 <- spl_data %>% filter(group == pair[2]) %>% pull(value)
          p_val <- ks.test(data1, data2)$p.value
          p_values_list <- append(p_values_list, list(data.frame(
            group1 = pair[1],
            split1 = spl,
            group2 = pair[2],
            split2 = spl,
            p_value = p_val,
            comparison_type = "across"
          )))
        }
      }
    }
    
    # Combine p-values
    p_values_df <- do.call(rbind, p_values_list)
    
    # Adjust p-values using Bonferroni correction
    p_values_df$p_value_adj <- p.adjust(p_values_df$p_value, method = "bonferroni")
    
    # Generate labels
    p_values_df$label <- ifelse(p_value_format == "asterisk",
                                ifelse(p_values_df$p_value_adj < 0.001, "***",
                                       ifelse(p_values_df$p_value_adj < 0.01, "**",
                                              ifelse(p_values_df$p_value_adj < 0.05, "*", ""))),
                                format(round(p_values_df$p_value_adj, 3), nsmall = 3))
    
    # Get the max value for each (group, split) combination
    max_values <- data %>%
      group_by(group, split) %>%
      summarize(max_value = max(value), .groups = "drop") %>%
      mutate(group = as.character(group),
             split = as.character(split))
    
    # Merge max_values into p_values_df
    p_values_df <- p_values_df %>%
      left_join(max_values, by = c("group1" = "group", "split1" = "split")) %>%
      rename(max_value1 = max_value) %>%
      left_join(max_values, by = c("group2" = "group", "split2" = "split")) %>%
      rename(max_value2 = max_value)
    
    # Get the max value across all data
    max_value_overall <- max(data$value)
    data_range <- diff(range(data$value))
    
    # Set y.position for 'within' comparisons
    y_position_within <- max_value_overall + 0.05 * data_range
    
    # Process 'within' comparisons
    p_values_within <- p_values_df %>% filter(comparison_type == "within") %>%
      mutate(y.position = y_position_within)
    
    # Process 'across' comparisons
    p_values_across <- p_values_df %>% filter(comparison_type == "across") %>%
      arrange(max_value1, max_value2) %>%
      mutate(y.position = y_position_within + 0.1 * data_range + (row_number() - 1) * 0.05 * data_range)
    
    # Combine the p-values
    p_values_df <- bind_rows(p_values_within, p_values_across)
    
    # Compute x positions
    group_levels <- levels(factor(data$group))
    group_positions <- data.frame(group = group_levels, x = as.numeric(factor(group_levels)))
    
    split_levels <- unique(data$split)
    
    p_values_df <- p_values_df %>%
      left_join(group_positions, by = c("group1" = "group")) %>%
      rename(x1 = x) %>%
      left_join(group_positions, by = c("group2" = "group")) %>%
      rename(x2 = x)
    
    # Adjust x positions based on split
    p_values_df <- p_values_df %>%
      mutate(
        x1 = x1 + ifelse(split1 == split_levels[1], 0.15, -0.15),
        x2 = x2 + ifelse(split2 == split_levels[1], 0.15, -0.15),
        x_label = (x1 + x2) / 2
      )
    
    # Add p-value brackets and labels to the plot
    p <- p +
      geom_segment(data = p_values_df,
                   aes(x = x1, xend = x2, y = y.position, yend = y.position),
                   color = "black",
                   inherit.aes = FALSE) +
      geom_segment(data = p_values_df,
                   aes(x = x1, xend = x1, y = y.position, yend = y.position - 0.01 * data_range),
                   color = "black",
                   inherit.aes = FALSE) +
      geom_segment(data = p_values_df,
                   aes(x = x2, xend = x2, y = y.position, yend = y.position - 0.01 * data_range),
                   color = "black",
                   inherit.aes = FALSE) +
      geom_text(data = p_values_df,
                aes(x = x_label, y = y.position + 0.02 * data_range, label = label),
                size = 3, color = "black",
                inherit.aes = FALSE)
  }
  
  # Create custom legend
  legend <- grobTree(
    textGrob("Legend:", x = 0.1, y = 0.9, hjust = 0, gp = gpar(fontface = "bold")),
    rectGrob(x = 0.15, y = 0.85, width = 0.02, height = 0.02, gp = gpar(fill = colors[1], col = border_colors[1])),
    textGrob(labels[1], x = 0.21, y = 0.85, hjust = 0),
    rectGrob(x = 0.15, y = 0.8, width = 0.02, height = 0.02, gp = gpar(fill = colors[2], col = border_colors[2])),
    textGrob(labels[2], x = 0.21, y = 0.8, hjust = 0)
  )
  
  # Create footer text with "Kolmogorov–Smirnov test" and "Bonferroni" in bold
  footer_text <- textGrob(expression(paste("pwc: ", bold("Kolmogorov–Smirnov test"), "; p.adjust: ", bold("Bonferroni"))),
                          x = 0.99, y = 0.01, hjust = 1, vjust = 0,
                          gp = gpar(fontsize = 9))
  
  # Combine plot, legend, and footer
  combined_plot <- grid.arrange(p, legend, footer_text, ncol = 2, nrow = 2,
                                widths = c(3, 1), heights = c(9, 1),
                                layout_matrix = rbind(c(1, 2),
                                                      c(3, 3)))
  
  return(combined_plot)
}

# Custom GeomSplitViolin and geom_split_violin function
GeomSplitViolin <- ggplot2::ggproto("GeomSplitViolin", ggplot2::GeomViolin,
                                    draw_group = function(self, data, ..., draw_quantiles = NULL) {
                                      data <- transform(data, xminv = x - violinwidth * (x - xmin),
                                                        xmaxv = x + violinwidth * (xmax - x))
                                      grp <- data[1, "group"]
                                      newdata <- plyr::arrange(transform(data,
                                                                         x = if (grp %% 2 == 1) xminv else xmaxv),
                                                               if (grp %% 2 == 1) y else -y)
                                      newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                                      newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                                      if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                                        stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))
                                        quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                                        aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                                        aesthetics$alpha <- rep(1, nrow(quantiles))
                                        both <- cbind(quantiles, aesthetics)
                                        quantile_grob <- ggplot2::GeomPath$draw_panel(both, ...)
                                        ggplot2:::ggname("geom_split_violin",
                                                         grid::grobTree(ggplot2::GeomPolygon$draw_panel(newdata, ...),
                                                                        quantile_grob))
                                      } else {
                                        ggplot2:::ggname("geom_split_violin",
                                                         ggplot2::GeomPolygon$draw_panel(newdata, ...))
                                      }
                                    })


geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                              position = "identity", ..., draw_quantiles = NULL, trim = TRUE,
                              scale = "area", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin,
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles,
                               na.rm = na.rm, ...))
}
