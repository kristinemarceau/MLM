############################################################
# Viz helper functions
#
# This file contains reusable plotting functions that can be
# sourced into any R script or R Markdown file.
#
# To use in an .Rmd file, place this in your setup chunk:
#
#   source("R/plotting_functions.R")
#
# or, if sourcing directly from GitHub:
#
#   source("https://raw.githubusercontent.com/kristinemarceau/MLM/main/Viz_helper_functions.R")
#
# Required packages:
#   dplyr
#   ggplot2
#   rlang
#   viridis
#   moments
#   tidyr
#   psych
#   DT
#   htmltools
#   knitr
#   kableExtra
#   psych
#   DT
#   htmltools
#
# Recommended setup chunk:
#
#   library(tidyverse)
#   library(moments)
#   library(viridis)
#   source("Viz_helper_functions.R")
#
# NOTE: Hmisc is NOT required by anything in this file, even though older
# versions of this comment said otherwise. Don't load it alongside psych in
# scripts that call describe() - Hmisc::describe() masks psych::describe()
# and returns an object kbl()/as.data.frame() can't handle the same way.
#
############################################################


############################################################
# Shared color palettes
#
# cleanplots is the general-purpose project palette.
# To customize plot colors, edit this vector or pass a palette/color
# directly to the plotting function.
############################################################

cleanplots <- c(
  "#D50000", "#82C0DF", "#808080", "#000000",
  "#00D5D5", "#800080", "#FFD200", "#006000",
  "#938DD2", "#1A476F"
)


############################################################
# plot_distribution()
#
# Purpose:
#   Creates a histogram for a single numeric variable, with
#   vertical reference lines for the mean, +/- 1 SD, and
#   +/- 3 SD. The plot also annotates N, mean, SD, skewness,
#   and kurtosis.
#
# Arguments:
#   data:
#     A data frame.
#
#   var:
#     The unquoted name of the numeric variable to plot.
#
#   title:
#     Optional plot title. If NULL, the variable name is used.
#
#   fill_color:
#     Optional fill color for histogram bars. Defaults to the
#     first color in the cleanplots palette.
#
#   xlab:
#     Optional x-axis label. If NULL, the variable name is used.
#
#   annotate_x:
#     Optional x-position for the summary text. If NULL, the
#     minimum observed value is used.
#
# Example:
#   plot_distribution(
#     data = mh_checklists,
#     var = cbcl_externalizing,
#     title = "CBCL Externalizing",
#     xlab = "Externalizing Problems"
#   )
#
# Notes:
#   - var should be numeric.
#   - Missing values are removed before plotting.
#   - Requires moments::skewness() and moments::kurtosis().
############################################################

plot_distribution <- function(data, var, title = NULL,
                              fill_color = cleanplots[1],
                              xlab = NULL,
                              annotate_x = NULL) {
  var_sym <- rlang::enquo(var)
  var_str <- rlang::as_label(var_sym)

  values <- data %>% dplyr::pull(!!var_sym)

  N <- sum(!is.na(values))
  M <- round(mean(values, na.rm = TRUE), 2)
  SD <- round(sd(values, na.rm = TRUE), 2)
  Skew <- round(moments::skewness(values, na.rm = TRUE), 2)
  Kurt <- round(moments::kurtosis(values, na.rm = TRUE), 2)

  summary_text <- paste0(
    "N = ", N, "\n",
    "Mean = ", M, "\n",
    "SD = ", SD, "\n",
    "Skew = ", Skew, "\n",
    "Kurtosis = ", Kurt
  )

  if (is.null(annotate_x)) {
    annotate_x <- min(values, na.rm = TRUE)
  }

  ggplot2::ggplot(
    data = data %>% dplyr::filter(!is.na(!!var_sym)),
    ggplot2::aes(x = !!var_sym)
  ) +
    ggplot2::geom_histogram(color = "#000000", fill = fill_color, bins = 30) +
    ggplot2::geom_vline(xintercept = M, color = "#000000", linewidth = 1.25) +
    ggplot2::geom_vline(xintercept = M + SD, color = "#000000", linewidth = 1, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = M - SD, color = "#000000", linewidth = 1, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = M + 3 * SD, color = "#000000", linewidth = 1, linetype = "dotted") +
    ggplot2::geom_vline(xintercept = M - 3 * SD, color = "#000000", linewidth = 1, linetype = "dotted") +
    ggplot2::annotate(
      "text",
      x = annotate_x,
      y = Inf,
      label = summary_text,
      hjust = 0,
      vjust = 1.05,
      size = 4,
      fontface = "italic"
    ) +
    ggplot2::ggtitle(ifelse(is.null(title), var_str, title)) +
    ggplot2::labs(
      x = ifelse(is.null(xlab), var_str, xlab),
      caption = "solid line = mean\n dashed line = +/-1SD\n dotted line = +/-3SD"
    ) +
    ggplot2::theme_minimal()
}


############################################################
# plot_distribution_by_time()
#
# Purpose:
#   Creates faceted histograms for a single numeric variable by
#   timepoint or another grouping variable. Within each facet, the
#   function computes and displays time-specific N, mean, SD,
#   skewness, and kurtosis. It also draws time-specific vertical
#   reference lines for the mean, +/- 1 SD, and +/- 3 SD.
#
# Arguments:
#   data:
#     A data frame.
#
#   var:
#     The unquoted name of the numeric variable to plot.
#
#   time_var:
#     The unquoted name of the timepoint/grouping variable used for
#     facet_wrap(). For ABCD-style data, this is often session_id.
#
#   title:
#     Optional plot title. If NULL, the variable name is used.
#
#   xlab:
#     Optional x-axis label. If NULL, the variable name is used.
#
#   annotate_x:
#     Optional x-position for the summary text. If NULL, the minimum
#     observed value within each timepoint is used.
#
#   bins:
#     Number of histogram bins. Defaults to 30.
#
#   fill_palette:
#     Optional vector of fill colors. Defaults to cleanplots.
#
# Example:
#   plot_distribution_by_time(
#     data = mh,
#     var = mh_y_bpm__ext_sum,
#     time_var = session_id,
#     title = "Child Reported Externalizing",
#     xlab = "Externalizing Problems",
#     annotate_x = 5
#   )
#
# Notes:
#   - var should be numeric.
#   - Missing values on var or time_var are removed before plotting.
#   - Summary statistics and reference lines are calculated separately
#     within each timepoint.
#   - Uses cleanplots as the default discrete fill palette.
############################################################

plot_distribution_by_time <- function(data,
                                      var,
                                      time_var,
                                      title = NULL,
                                      xlab = NULL,
                                      annotate_x = NULL,
                                      bins = 30,
                                      fill_palette = c(cleanplots)) {
  var_sym <- rlang::enquo(var)
  time_sym <- rlang::enquo(time_var)
  var_str <- rlang::as_label(var_sym)
  time_str <- rlang::as_label(time_sym)

  plot_data <- data %>%
    dplyr::filter(!is.na(!!var_sym), !is.na(!!time_sym))

  stats_by_time <- plot_data %>%
    dplyr::group_by(!!time_sym) %>%
    dplyr::summarise(
      N = sum(!is.na(!!var_sym)),
      M = mean(!!var_sym, na.rm = TRUE),
      SD = stats::sd(!!var_sym, na.rm = TRUE),
      Skew = moments::skewness(!!var_sym, na.rm = TRUE),
      Kurt = moments::kurtosis(!!var_sym, na.rm = TRUE),
      min_x = min(!!var_sym, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      summary_text = paste0(
        "N = ", N, "\n",
        "Mean = ", round(M, 2), "\n",
        "SD = ", round(SD, 2), "\n",
        "Skew = ", round(Skew, 2), "\n",
        "Kurtosis = ", round(Kurt, 2)
      ),
      annotate_x = if (is.null(annotate_x)) min_x else annotate_x
    )

  ggplot2::ggplot(
    data = plot_data,
    ggplot2::aes(x = !!var_sym, fill = !!time_sym)
  ) +
    ggplot2::geom_histogram(color = "#000000", bins = bins, show.legend = FALSE) +
    ggplot2::geom_vline(
      data = stats_by_time,
      ggplot2::aes(xintercept = M),
      color = "#000000",
      linewidth = 1.25,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_vline(
      data = stats_by_time,
      ggplot2::aes(xintercept = M + SD),
      color = "#000000",
      linewidth = 1,
      linetype = "dashed",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_vline(
      data = stats_by_time,
      ggplot2::aes(xintercept = M - SD),
      color = "#000000",
      linewidth = 1,
      linetype = "dashed",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_vline(
      data = stats_by_time,
      ggplot2::aes(xintercept = M + 3 * SD),
      color = "#000000",
      linewidth = 1,
      linetype = "dotted",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_vline(
      data = stats_by_time,
      ggplot2::aes(xintercept = M - 3 * SD),
      color = "#000000",
      linewidth = 1,
      linetype = "dotted",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_text(
      data = stats_by_time,
      ggplot2::aes(x = annotate_x, y = Inf, label = summary_text),
      hjust = 0,
      vjust = 1.05,
      size = 4,
      fontface = "italic",
      inherit.aes = FALSE
    ) +
    ggplot2::facet_wrap(ggplot2::vars(!!time_sym)) +
    ggplot2::scale_fill_manual(values = fill_palette) +
    ggplot2::ggtitle(ifelse(is.null(title), var_str, title)) +
    ggplot2::labs(
      x = ifelse(is.null(xlab), var_str, xlab),
      caption = paste0(
        "Facets = ", time_str, "\n",
        "solid line = time-specific mean\n",
        "dashed line = time-specific +/-1SD\n",
        "dotted line = time-specific +/-3SD"
      )
    ) +
    ggplot2::theme_minimal()
}


############################################################
# plot_longitudinal_subset()
#
# Purpose:
#   Creates a longitudinal spaghetti plot for a random subset
#   of participants, families, or other repeated-measures units.
#   The plot shows individual trajectories over time plus an
#   overall LOESS-smoothed trend line.
#
# Arguments:
#   data:
#     A long-format data frame.
#
#   var:
#     The unquoted name of the numeric repeated-measures variable
#     to plot on the y-axis.
#
#   time_var:
#     The unquoted name of the time variable to plot on the x-axis.
#
#   id_var:
#     The unquoted name of the participant/family/person ID used
#     to group repeated observations.
#
#   sample_n:
#     Number of unique IDs to randomly sample. Defaults to 100.
#
#   seed:
#     Random seed for reproducible sampling. Defaults to 2025.
#
#   title:
#     Optional plot title. If NULL, the variable name is used.
#
#   xlab:
#     Optional x-axis label. If NULL, the time variable name is used.
#
#   ylab:
#     Optional y-axis label. If NULL, the outcome variable name is used.
#
# Example:
#   plot_longitudinal_subset(
#     data = mh,
#     var = mh_y_bpm__ext_sum,
#     time_var = timepoint,
#     id_var = participant_id,
#     sample_n = 100,
#     seed = 2025,
#     title = "Child Reported Externalizing",
#     xlab = "Time",
#     ylab = "Externalizing Problems"
#   )
#
# Notes:
#   - Data should be in long format, with one row per person/family
#     per timepoint.
#   - Missing values on var, time_var, or id_var are removed before plotting.
#   - If sample_n is larger than the number of available IDs, all IDs are used.
#   - Uses cleanplots[6] for the LOESS trend line, which is purple.
############################################################

plot_longitudinal_subset <- function(data,
                                     var,
                                     time_var,
                                     id_var,
                                     sample_n = 100,
                                     seed = 2025,
                                     title = NULL,
                                     xlab = NULL,
                                     ylab = NULL) {

  var_sym <- rlang::enquo(var)
  time_sym <- rlang::enquo(time_var)
  id_sym <- rlang::enquo(id_var)

  var_str <- rlang::as_label(var_sym)
  time_str <- rlang::as_label(time_sym)

  plot_data_base <- data %>%
    dplyr::filter(
      !is.na(!!var_sym),
      !is.na(!!time_sym),
      !is.na(!!id_sym)
    ) %>%
    dplyr::group_by(!!id_sym, !!time_sym) %>%
    dplyr::summarise(
      value = mean(!!var_sym, na.rm = TRUE),
      .groups = "drop"
    )

  set.seed(seed)

  id_list <- plot_data_base %>%
    dplyr::distinct(!!id_sym)

  sampled_ids <- id_list %>%
    dplyr::slice_sample(
      n = min(sample_n, nrow(id_list))
    ) %>%
    dplyr::pull(!!id_sym)

  plot_data <- plot_data_base %>%
    dplyr::filter(!!id_sym %in% sampled_ids)

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = !!time_sym,
      y = value,
      group = !!id_sym
    )
  ) +
    ggplot2::geom_line(alpha = 0.15) +
    ggplot2::geom_point(size = 1, alpha = 0.60) +
    ggplot2::geom_smooth(
      ggplot2::aes(group = 1),
      method = "loess",
      color = cleanplots[1],
      linewidth = 1.2,
      se = TRUE
    ) +
    ggplot2::labs(
      title = ifelse(is.null(title), var_str, title),
      x = ifelse(is.null(xlab), time_str, xlab),
      y = ifelse(is.null(ylab), var_str, ylab)
    ) +
    ggplot2::theme_minimal()
}


############################################################
# plot_longitudinal_facets()
#
# Purpose:
#   Creates a small-multiples ("facet") plot of a random subset of
#   participants, families, or other repeated-measures units - one
#   panel per unit, each with its own points and its own linear fit
#   (unlike plot_longitudinal_subset(), which overlays everyone on
#   one panel with a single overall trend line). Useful when you
#   want students to see individual-level shape of change up close,
#   e.g. right after the overall spaghetti plot.
#
# Arguments:
#   data:
#     A long-format data frame.
#
#   var:
#     The unquoted name of the numeric repeated-measures variable
#     to plot on the y-axis.
#
#   time_var:
#     The unquoted name of the time variable to plot on the x-axis.
#
#   id_var:
#     The unquoted name of the participant/family/person ID used
#     to group repeated observations and to facet by.
#
#   sample_n:
#     Number of unique IDs to randomly sample. Defaults to 12 (a
#     number of panels that's still readable on one page). Set to
#     Inf, or to a number >= the number of available IDs, to facet
#     everyone.
#
#   seed:
#     Random seed for reproducible sampling. Defaults to 2025.
#
#   title:
#     Optional plot title. If NULL, the variable name is used.
#
#   xlab:
#     Optional x-axis label. If NULL, the time variable name is used.
#
#   ylab:
#     Optional y-axis label. If NULL, the outcome variable name is used.
#
#   ncol:
#     Optional number of facet columns, passed to facet_wrap(). If
#     NULL, ggplot2 picks a layout automatically.
#
# Example:
#   plot_longitudinal_facets(
#     data = age_psi_long,
#     var = PSI,
#     time_var = assess,
#     id_var = ID,
#     sample_n = 12,
#     seed = 1,
#     xlab = "Assessment",
#     ylab = "Adolescents' Perceptions of Peer Approval of Alcohol Use"
#   )
#
# Notes:
#   - Data should be in long format, with one row per person/family
#     per timepoint.
#   - Missing values on var, time_var, or id_var are removed before plotting.
#   - If sample_n is larger than the number of available IDs, all IDs are used.
#   - Each panel gets its own lm() fit via stat_smooth(), so this is best
#     for a handful of panels at a time, not hundreds.
############################################################

plot_longitudinal_facets <- function(data,
                                     var,
                                     time_var,
                                     id_var,
                                     sample_n = 12,
                                     seed = 2025,
                                     title = NULL,
                                     xlab = NULL,
                                     ylab = NULL,
                                     ncol = NULL) {

  var_sym <- rlang::enquo(var)
  time_sym <- rlang::enquo(time_var)
  id_sym <- rlang::enquo(id_var)

  var_str <- rlang::as_label(var_sym)
  time_str <- rlang::as_label(time_sym)

  plot_data_base <- data %>%
    dplyr::filter(
      !is.na(!!var_sym),
      !is.na(!!time_sym),
      !is.na(!!id_sym)
    )

  set.seed(seed)

  id_list <- plot_data_base %>%
    dplyr::distinct(!!id_sym)

  sampled_ids <- id_list %>%
    dplyr::slice_sample(
      n = min(sample_n, nrow(id_list))
    ) %>%
    dplyr::pull(!!id_sym)

  plot_data <- plot_data_base %>%
    dplyr::filter(!!id_sym %in% sampled_ids)

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = !!time_sym, y = !!var_sym)
  ) +
    ggplot2::geom_point() +
    ggplot2::stat_smooth(
      method = "lm",
      fullrange = TRUE,
      se = TRUE,
      color = cleanplots[1]
    ) +
    ggplot2::facet_wrap(ggplot2::vars(!!id_sym), ncol = ncol) +
    ggplot2::labs(
      title = ifelse(is.null(title), var_str, title),
      x = ifelse(is.null(xlab), time_str, xlab),
      y = ifelse(is.null(ylab), var_str, ylab)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 14),
      axis.text = ggplot2::element_text(size = 11),
      strip.text = ggplot2::element_text(size = 12)
    )
}
      
############################################################
# plot_nesting_boxplot()
#
# Purpose:
#   Creates boxplots for a random subset of participants,
#   families, clusters, or other grouping units. Each boxplot
#   shows the distribution of repeated observations within one
#   unit, making within- versus between-unit variability easy
#   to see.
#
# Arguments:
#   data:
#     A long-format data frame.
#
#   var:
#     The unquoted name of the numeric variable to plot.
#
#   id_var:
#     The unquoted name of the participant/family/cluster ID
#     defining the nesting unit.
#
#   sample_n:
#     Number of unique IDs to randomly sample. Defaults to 12.
#
#   seed:
#     Random seed for reproducible sampling. Defaults to 2025.
#
#   title:
#     Optional plot title. If NULL, a title is generated.
#
#   xlab:
#     Optional x-axis label. If NULL, the ID variable name is used.
#
#   ylab:
#     Optional y-axis label. If NULL, the outcome variable name is used.
#
# Example:
#   plot_nesting_boxplot(
#     data = all_merged,
#     var = cesd,
#     id_var = AID,
#     sample_n = 12,
#     seed = 1,
#     title = "CES-D by Participant (Random 12 Participants)",
#     xlab = "Participant",
#     ylab = "CES-D Depressive Symptoms"
#   )
#
# Notes:
#   - Data should be in long format.
#   - Missing values on var or id_var are removed before plotting.
#   - If sample_n is larger than the number of available IDs,
#     all IDs are used.
#   - Boxes summarize within-unit distributions; differences
#     between boxes illustrate between-unit variability.
############################################################

plot_nesting_boxplot <- function(data,
                                 var,
                                 id_var,
                                 sample_n = 12,
                                 seed = 2025,
                                 title = NULL,
                                 xlab = NULL,
                                 ylab = NULL) {

  var_sym <- rlang::enquo(var)
  id_sym  <- rlang::enquo(id_var)

  var_str <- rlang::as_label(var_sym)
  id_str  <- rlang::as_label(id_sym)

  plot_data_base <- data %>%
    dplyr::filter(
      !is.na(!!var_sym),
      !is.na(!!id_sym)
    )

  set.seed(seed)

  id_list <- plot_data_base %>%
    dplyr::distinct(!!id_sym)

  sampled_ids <- id_list %>%
    dplyr::slice_sample(
      n = min(sample_n, nrow(id_list))
    ) %>%
    dplyr::pull(!!id_sym)

  plot_data <- plot_data_base %>%
    dplyr::filter(!!id_sym %in% sampled_ids)

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = factor(!!id_sym),
      y = !!var_sym,
      fill = factor(!!id_sym)
    )
  ) +
    ggplot2::geom_boxplot(
      alpha = 0.7,
      color = "black"
    ) +
    ggplot2::geom_jitter(
      width = 0.15,
      size = 2,
      alpha = 0.6,
      shape = 21,
      color = "black"
    ) +
    ggplot2::scale_fill_manual(
      values = rep(cleanplots, length.out = length(sampled_ids))
    ) +
    ggplot2::labs(
      title = ifelse(
        is.null(title),
        paste0(var_str, " by ", id_str),
        title
      ),
      x = ifelse(is.null(xlab), id_str, xlab),
      y = ifelse(is.null(ylab), var_str, ylab)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none"
    )
}

############################################################
# plot_nominal()
#
# Purpose:
#   Creates a bar chart for a categorical variable, displaying
#   counts and percentages for each category.
#
# Arguments:
#   data:
#     A data frame.
#
#   var:
#     The unquoted name of the categorical variable to plot.
#
#   title:
#     Optional plot title. If NULL, the variable name is used.
#
#   xlab:
#     Optional x-axis label. If NULL, the variable name is used.
#
#   ylab:
#     Optional y-axis label. Defaults to "Count".
#
#   show_percent:
#     Whether to display percentages above bars. Defaults to TRUE.
#
#   sort:
#     Category ordering method. Defaults to "none", which preserves
#     factor level order if var is a factor and otherwise preserves
#     order of first appearance in the data. Options are:
#       "none"
#       "count_desc"
#       "count_asc"
#       "alphabetical"
#       "reverse_alphabetical"
#       "custom"
#
#   category_order:
#     Character vector specifying category order when sort = "custom".
#
# Example:
#   plot_nominal(
#     data = mh,
#     var = sex,
#     title = "Participant Sex"
#   )
#
#   plot_nominal(
#     data = mh,
#     var = ordinal_var,
#     sort = "custom",
#     category_order = c("Never", "Rarely", "Sometimes", "Often", "Always")
#   )
#
# Notes:
#   - Missing values are removed before plotting.
#   - Categories are not sorted by frequency by default because many
#     categorical and ordinal variables have a meaningful original order.
#   - Uses cleanplots as the default discrete fill palette.
############################################################

plot_nominal <- function(data,
                         var,
                         title = NULL,
                         xlab = NULL,
                         ylab = "Count",
                         show_percent = TRUE,
                         sort = c("none", "count_desc", "count_asc",
                                  "alphabetical", "reverse_alphabetical",
                                  "custom"),
                         category_order = NULL) {

  sort <- match.arg(sort)

  var_sym <- rlang::enquo(var)
  var_str <- rlang::as_label(var_sym)

  original_values <- data %>%
    dplyr::pull(!!var_sym)

  plot_data <- data %>%
    dplyr::filter(!is.na(!!var_sym)) %>%
    dplyr::count(!!var_sym, name = "n") %>%
    dplyr::mutate(
      category = as.character(!!var_sym),
      pct = 100 * n / sum(n),
      label = paste0(round(pct, 1), "%")
    )

  if (sort == "none") {
    if (is.factor(original_values)) {
      category_levels <- levels(original_values)
    } else {
      category_levels <- data %>%
        dplyr::filter(!is.na(!!var_sym)) %>%
        dplyr::distinct(!!var_sym) %>%
        dplyr::pull(!!var_sym) %>%
        as.character()
    }
  } else if (sort == "count_desc") {
    category_levels <- plot_data %>%
      dplyr::arrange(dplyr::desc(n), category) %>%
      dplyr::pull(category)
  } else if (sort == "count_asc") {
    category_levels <- plot_data %>%
      dplyr::arrange(n, category) %>%
      dplyr::pull(category)
  } else if (sort == "alphabetical") {
    category_levels <- plot_data %>%
      dplyr::arrange(category) %>%
      dplyr::pull(category)
  } else if (sort == "reverse_alphabetical") {
    category_levels <- plot_data %>%
      dplyr::arrange(dplyr::desc(category)) %>%
      dplyr::pull(category)
  } else if (sort == "custom") {
    if (is.null(category_order)) {
      stop("When sort = 'custom', provide a character vector to category_order.")
    }
    category_levels <- category_order
  }

  plot_data <- plot_data %>%
    dplyr::mutate(
      category = factor(category, levels = category_levels)
    ) %>%
    dplyr::filter(!is.na(category)) %>%
    dplyr::arrange(category)

  N <- sum(plot_data$n)
  K <- nrow(plot_data)

  summary_text <- paste0(
    "N = ", N,
    "
Categories = ", K
  )

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = category,
      y = n,
      fill = category
    )
  ) +
    ggplot2::geom_col(color = "black") +
    ggplot2::scale_fill_manual(
      values = rep(cleanplots, length.out = nrow(plot_data))
    ) +
    ggplot2::labs(
      title = ifelse(is.null(title), var_str, title),
      x = ifelse(is.null(xlab), var_str, xlab),
      y = ylab,
      caption = summary_text
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none"
    )

  if (show_percent) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = label),
        vjust = -0.3,
        size = 4
      )
  }

  return(p)
}

############################################################
# plot_cormat_heatmap()
#
# Purpose:
#   Creates a lower-triangle correlation heatmap for a wide data
#   frame of numeric variables. This is intended to resemble a
#   typical printed correlation matrix, with correlations shown in
#   the lower-left half and the upper half empty.
#
# Arguments:
#   wide_subset:
#     A data frame containing the variables to correlate.
#     Ideally, this should include only numeric columns.
#
# What it does:
#   - Converts columns ending in "_dev" to absolute values.
#   - Computes Pearson correlations using pairwise complete
#     observations.
#   - Displays only the lower triangle of the correlation matrix.
#   - Labels each tile with the rounded correlation value.
#   - Uses a viridis ordinal/sequential palette by default.
#
# Example:
#   vars_for_correlation <- mh_checklists %>%
#     select(cbcl_externalizing, bpm_externalizing, age, sex)
#
#   plot_cormat_heatmap(vars_for_correlation)
#
# Notes:
#   - Non-numeric columns are dropped before correlations are computed.
#   - Best practice is to select only variables intended for the
#     correlation matrix before calling this function.
#   - Requires viridis.
############################################################

plot_cormat_heatmap <- function(wide_subset) {

  wide_subset <- wide_subset %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("_dev"), abs))

  numeric_subset <- wide_subset %>%
    dplyr::select(where(is.numeric))

  cormat <- round(
    stats::cor(numeric_subset, use = "pairwise.complete.obs"),
    2
  )

  get_lower_tri <- function(cormat) {
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }

  lower_tri <- get_lower_tri(cormat)

  cor_df <- as.data.frame(lower_tri) %>%
    dplyr::mutate(Var1 = row.names(.)) %>%
    tidyr::pivot_longer(
      cols = -Var1,
      names_to = "Var2",
      values_to = "value"
    ) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(
      Var1 = factor(Var1, levels = rev(colnames(cormat))),
      Var2 = factor(Var2, levels = colnames(cormat))
    )

  ggplot2::ggplot(cor_df, ggplot2::aes(Var2, Var1, fill = value)) +
    ggplot2::geom_tile(color = "white", width = 1, height = 1) +
    ggplot2::geom_text(
      ggplot2::aes(label = value),
      color = "black",
      size = 3
    ) +
    viridis::scale_fill_viridis(
      option = "D",
      limits = c(-1, 1),
      name = "Pearson
Correlation"
    ) +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        angle = 45,
        vjust = 1,
        hjust = 1,
        size = 12
      ),
      axis.text.y = ggplot2::element_text(size = 12),
      panel.grid = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      legend.position = "right"
    )
}


############################################################
# format_corr_table()
#
# Purpose:
#   Computes a correlation matrix and formats each cell as the
#   correlation coefficient, an optional significance marker, and
#   the pairwise sample size. The returned data frame can be printed
#   to either HTML or PDF with print_corr_table().
#
# Arguments:
#   x:
#     A numeric data frame or matrix.
#
#   y:
#     Optional second numeric data frame or matrix. If NULL, correlations
#     are computed among the variables in x. If supplied, correlations
#     are computed between variables in x and variables in y.
#
#   method:
#     Correlation method passed to psych::corr.test(). Defaults to
#     "pearson". Other options include "spearman" and "kendall".
#
#   alpha:
#     P-value threshold for adding a significance marker. Defaults to .05.
#
#   digits:
#     Number of decimal places shown for correlations. Defaults to 2.
#
#   show_n:
#     Whether to display the pairwise sample size below each correlation.
#     Defaults to TRUE.
#
# Example:
#   formatted_corr <- format_corr_table(
#     timing_vars,
#     outcome_vars
#   )
#
# Notes:
#   - Uses pairwise sample sizes returned by psych::corr.test().
#   - Cells use <br> internally; print_corr_table() converts these to
#     LaTeX-safe line breaks automatically when knitting to PDF.
############################################################

format_corr_table <- function(x,
                              y = NULL,
                              method = "pearson",
                              alpha = .05,
                              digits = 2,
                              show_n = TRUE) {

  res <- if (is.null(y)) {
    psych::corr.test(x, method = method)
  } else {
    psych::corr.test(x, y, method = method)
  }

  formatted <- matrix(
    mapply(
      function(r, p, n) {
        sig <- ifelse(!is.na(p) & p < alpha, "*", "")
        cor_text <- ifelse(
          !is.na(r),
          paste0(format(round(r, digits), nsmall = digits), sig),
          ""
        )

        if (show_n) {
          paste0(cor_text, "<br>(n=", n, ")")
        } else {
          cor_text
        }
      },
      as.vector(res$r),
      as.vector(res$p),
      as.vector(res$n)
    ),
    nrow = nrow(res$r),
    dimnames = dimnames(res$r)
  )

  as.data.frame(formatted, check.names = FALSE)
}


############################################################
# print_corr_table()
#
# Purpose:
#   Prints a formatted correlation table created by format_corr_table().
#   The output is selected automatically for HTML versus PDF/LaTeX.
#
# Arguments:
#   x:
#     A formatted data frame, typically returned by
#     format_corr_table().
#
#   caption:
#     Optional table caption.
#
#   scroll_y:
#     Optional vertical scrolling height for HTML output. Defaults to
#     NULL, which leaves vertical scrolling off. Ignored for PDF output.
#
# Example:
#   print_corr_table(
#     format_corr_table(timing_vars, outcome_vars),
#     caption = "Puberty variables with outcomes; * p < .05"
#   )
#
# Notes:
#   - HTML output uses DT::datatable() with horizontal scrolling.
#   - PDF output uses knitr::kable() and kableExtra::kable_styling().
#   - <br> line breaks created by format_corr_table() are converted to
#     LaTeX-safe line breaks automatically for PDF output.
############################################################

print_corr_table <- function(x,
                             caption = "Correlations; * p < .05",
                             scroll_y = NULL) {

  if (knitr::is_html_output()) {

    options <- list(
      scrollX = TRUE,
      paging = FALSE,
      searching = FALSE,
      ordering = FALSE
    )

    if (!is.null(scroll_y)) {
      options$scrollY <- scroll_y
    }

    return(
      DT::datatable(
        x,
        escape = FALSE,
        caption = htmltools::tags$caption(caption),
        options = options,
        class = "stripe hover nowrap"
      )
    )
  }

  if (knitr::is_latex_output()) {

    x_pdf <- x %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::everything(),
          ~ kableExtra::linebreak(
            gsub("<br>", "\n", .x, fixed = TRUE),
            align = "c"
          )
        )
      )

    return(
      knitr::kable(
        x_pdf,
        format = "latex",
        booktabs = TRUE,
        escape = FALSE,
        caption = caption,
        row.names = TRUE
      ) %>%
        kableExtra::kable_styling(
          latex_options = c("hold_position", "scale_down")
        )
    )
  }

  knitr::kable(
    x,
    caption = caption,
    row.names = TRUE
  )
}
