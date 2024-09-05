#' Create a Split Violin Plot with Custom Elements and Options
#'
#' This function generates split violin plots with custom elements such as Q1, Q3, medians,
#' confidence intervals, outliers, and observation counts. It allows for flexible inclusion or exclusion
#' of these elements, and customization of axis labels.
#'
#' @param data A data frame containing the data to plot.
#' @param group_column A string representing the name of the column that defines the groups (x-axis).
#' @param value_column A string representing the name of the column containing the values (y-axis).
#' @param split_column A string representing the name of the column used to split the violins.
#' @param colors A named vector of two colors to be used for the left and right halves of the violins.
#' @param labels An optional named vector of labels for the legend corresponding to the `split` values.
#' @param x_lab A string representing the label for the x-axis (default: "Groups").
#' @param y_lab A string representing the label for the y-axis (default: the `value_column` name).
#' @param breaks An optional vector of breaks for the y-axis.
#' @param limits An optional vector of length two defining the limits of the y-axis.
#' @param outliers Logical, whether to include outliers (default TRUE).
#' @param CI Logical, whether to include confidence intervals (default TRUE).
#' @param median Logical, whether to include medians (default TRUE).
#' @param n_obs Logical, whether to include number of observations (default TRUE).
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
#' # Generate the split violin plot with custom labels
#' p <- split_violin_plot(data, "group", "value", "split_criteria", colors, labels = labels,
#'                        x_lab = "Groups", y_lab = bquote(bold(Delta ~ Log(activity))), outliers = TRUE, CI = TRUE, median = TRUE, n_obs = TRUE)
#' print(p)

split_violin_plot <- function(data, group_column, value_column, split_column, colors, labels = NULL,
                              x_lab = "Groups", y_lab = value_column, breaks = NULL, limits = NULL,
                              outliers = TRUE, CI = TRUE, median = TRUE, n_obs = TRUE) {

  # Rename columns for consistency
  data <- data %>%
    dplyr::rename(group = !!rlang::sym(group_column), value = !!rlang::sym(value_column), split = !!rlang::sym(split_column))

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
      upper_ci = quantile(value, 0.975)
    )

  # Filter outliers based on CI (values outside the CI limits are outliers)
  outlier_data <- data %>%
    dplyr::left_join(summary_stats, by = c("group", "split")) %>%
    dplyr::filter(value < lower_ci | value > upper_ci)

  # Filter data for the plot (values inside the CI limits)
  data_filtered <- data %>%
    dplyr::left_join(summary_stats, by = c("group", "split")) %>%
    dplyr::filter(value >= lower_ci & value <= upper_ci)

  # Define border and quantile colors (10% darker)
  border_colors <- colorspace::darken(colors, 0.15)
  q_colors <- border_colors
  border_colors_reverse <- c(border_colors[2], border_colors[1])

  # Base plot with custom axis labels
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
                   panel.grid.minor = element_blank(),
                   panel.spacing = unit(0.02, "lines"),
                   legend.position = "none")

  # Conditionally add breaks and limits for y-axis
  if (!is.null(breaks)) {
    p <- p + scale_y_continuous(breaks = breaks, limits = limits, expand = c(0, 0))

  }

  # Conditionally add outliers
  if (outliers) {
    p <- p +
      ggplot2::geom_point(data = outlier_data %>% dplyr::filter(split == names(colors)[1]),
                          aes(x = as.numeric(factor(group)) + 0.0625, y = value), color = border_colors[1], size = 1.5, shape = 21, fill = "white") +
      ggplot2::geom_point(data = outlier_data %>% dplyr::filter(split == names(colors)[2]),
                          aes(x = as.numeric(factor(group)) - 0.0625, y = value), color = border_colors[2], size = 1.5, shape = 21, fill = "white")
  }

  # Conditionally add CIs (along with Q1, Q3 lines)
  if (CI) {
    p <- p +
      ggplot2::geom_segment(data = summary_stats %>% dplyr::filter(split == names(colors)[1]),
                            aes(x = as.numeric(factor(group)), xend = as.numeric(factor(group)) + 0.125, y = Q1, yend = Q1),
                            color = scales::alpha(q_colors[1], 0.9), size = 0.5) +
      ggplot2::geom_segment(data = summary_stats %>% dplyr::filter(split == names(colors)[2]),
                            aes(x = as.numeric(factor(group)) - 0.125, xend = as.numeric(factor(group)), y = Q1, yend = Q1),
                            color = scales::alpha(q_colors[2], 0.9), size = 0.5) +
      ggplot2::geom_segment(data = summary_stats %>% dplyr::filter(split == names(colors)[1]),
                            aes(x = as.numeric(factor(group)), xend = as.numeric(factor(group)) + 0.125, y = Q3, yend = Q3),
                            color = scales::alpha(q_colors[1], 0.9), size = 0.5) +
      ggplot2::geom_segment(data = summary_stats %>% dplyr::filter(split == names(colors)[2]),
                            aes(x = as.numeric(factor(group)) - 0.125, xend = as.numeric(factor(group)), y = Q3, yend = Q3),
                            color = scales::alpha(q_colors[2], 0.9), size = 0.5) +
      ggplot2::geom_segment(data = summary_stats %>% dplyr::filter(split == names(colors)[1]),
                            aes(x = as.numeric(factor(group)) + 0.0625, xend = as.numeric(factor(group)) + 0.0625, y = Q1, yend = lower_ci),
                            color = scales::alpha(q_colors[1], 0.9), size = 0.5) +
      ggplot2::geom_segment(data = summary_stats %>% dplyr::filter(split == names(colors)[2]),
                            aes(x = as.numeric(factor(group)) - 0.0625, xend = as.numeric(factor(group)) - 0.0625, y = Q1, yend = lower_ci),
                            color = scales::alpha(q_colors[2], 0.9), size = 0.5) +
      ggplot2::geom_segment(data = summary_stats %>% dplyr::filter(split == names(colors)[1]),
                            aes(x = as.numeric(factor(group)) + 0.0625, xend = as.numeric(factor(group)) + 0.0625, y = Q3, yend = upper_ci),
                            color = scales::alpha(q_colors[1], 0.9), size = 0.5) +
      ggplot2::geom_segment(data = summary_stats %>% dplyr::filter(split == names(colors)[2]),
                            aes(x = as.numeric(factor(group)) - 0.0625, xend = as.numeric(factor(group)) - 0.0625, y = Q3, yend = upper_ci),
                            color = scales::alpha(q_colors[2], 0.9), size = 0.5)
  }

  # Conditionally add medians
  if (median) {
    p <- p +
      ggplot2::geom_segment(data = summary_stats %>% dplyr::filter(split == names(colors)[1]),
                            aes(x = as.numeric(factor(group)), xend = as.numeric(factor(group)) + 0.25, y = median, yend = median),
                            color = "black", size = 0.5) +
      ggplot2::geom_segment(data = summary_stats %>% dplyr::filter(split == names(colors)[2]),
                            aes(x = as.numeric(factor(group)) - 0.25, xend = as.numeric(factor(group)), y = median, yend = median),
                            color = "black", size = 0.5)
  }

  # Conditionally add number of observations (n)
  if (n_obs) {
    p <- p +
      ggplot2::geom_text(data = summary_stats %>% dplyr::filter(split == names(colors)[1]),
                         aes(x = as.numeric(factor(group)) + 0.15, y = median - 0.14, label = paste("n =", n)),
                         color = "black", size = 3) +
      ggplot2::geom_text(data = summary_stats %>% dplyr::filter(split == names(colors)[2]),
                         aes(x = as.numeric(factor(group)) - 0.15, y = median - 0.14, label = paste("n =", n)),
                         color = "black", size = 3)
  }

  # Default labels use split values if not provided
  if (is.null(labels)) {
    labels <- setNames(names(colors), names(colors))
  }

  # Create custom legend
  legend <- grobTree(
    textGrob("Legend:", x = 0.1, y = 0.9, hjust = 0, gp = gpar(fontface = "bold")),
    rectGrob(x = 0.15, y = 0.85, width = 0.02, height = 0.02, gp = gpar(fill = colors[1], col = border_colors[1])),
    textGrob(labels[1], x = 0.21, y = 0.85, hjust = 0),
    rectGrob(x = 0.15, y = 0.8, width = 0.02, height = 0.02, gp = gpar(fill = colors[2], col = border_colors[2])),
    textGrob(labels[2], x = 0.21, y = 0.8, hjust = 0)
  )

  # Combine plot and legend
  combined_plot <- grid.arrange(p, legend, ncol = 2, widths = c(3, 1))

  return(combined_plot)
}

# Custom GeomSplitViolin and geom_split_violin function
GeomSplitViolin <- ggplot2::ggproto("GeomSplitViolin", ggplot2::GeomViolin,
                                    draw_group = function(self, data, ..., draw_quantiles = NULL) {
                                      data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                                      grp <- data[1, "group"]
                                      newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                                      newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                                      newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                                      if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                                        stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))
                                        quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                                        aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                                        aesthetics$alpha <- rep(1, nrow(quantiles))
                                        both <- cbind(quantiles, aesthetics)
                                        quantile_grob <- ggplot2::GeomPath$draw_panel(both, ...)
                                        ggplot2:::ggname("geom_split_violin", grid::grobTree(ggplot2::GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                                      } else {
                                        ggplot2:::ggname("geom_split_violin", ggplot2::GeomPolygon$draw_panel(newdata, ...))
                                      }
                                    })


geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ...,
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin,
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}
