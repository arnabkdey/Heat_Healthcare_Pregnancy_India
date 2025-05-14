#' @title Plot Full Model Results
#' @description Creates a forest plot visualization of odds ratios and their confidence intervals
#' for temperature threshold exposure periods.
#' 
#' @param df_plot A data frame containing the following columns:
#'   - Exposure: Number of 10-day periods above/below temperature thresholds
#'   - Odds_Ratio: Calculated odds ratio values
#'   - CI_Lower: Lower bound of 95% confidence interval
#'   - CI_Upper: Upper bound of 95% confidence interval
#' @param title Character string for the plot title (default: "Here goes the title")
#' 
#' @return A ggplot object showing the forest plot with:
#'   - Odds ratios as points
#'   - 95% confidence intervals as horizontal lines
#'   - Reference line at OR = 1
#'   - Log-scaled x-axis
#'   - Formatted labels and theme
#' 
#' @author [Your Name]
#' @date Created: [Current Date]
#' @version 1.0
#' 
#' @examples
#' # Example usage:
#' # plot <- func_plot_full_model(df_results, title = "Temperature Threshold Effects")
#' # print(plot)

# Function 
func_plot_full_model <- function(df_plot, title = "Here goes the title") {
  
  # Calculate the range of OR values including CI bounds
  x_range <- range(c(df_plot$CI_Lower, df_plot$CI_Upper), na.rm = TRUE)
  # Add some padding to the range
  x_range <- c(min(x_range) * 0.8, max(x_range) * 1.2)
  
  # Plot
  plot <- ggplot(df_plot, aes(x = Odds_Ratio, y = Exposure)) +
    # facet_wrap(~threshold_label, ncol = 1) +
    geom_vline(xintercept = 1, colour = "gray50", linetype = "dashed", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.2f", Odds_Ratio)), vjust = -1.5, hjust = 0.5, size = 3.5) +
    geom_point(color = "#dd1c77", size = 3) +
    geom_pointrange(aes(xmin = CI_Lower, xmax = CI_Upper), color = "#dd1c77", linewidth = 0.5, alpha = 0.5) +
    labs(
      x = "Odds Ratio [95% CI]",
      y = "Num of days above/below temperature thresholds",
      title = title 
    ) +
    scale_x_log10(
      breaks = scales::breaks_log(n = 8),
      labels = scales::label_number(accuracy = 0.01),
      limits = x_range
    ) +
    theme_classic(base_size = 14, base_family = "Times New Roman") +
    theme(
      panel.grid.major = element_line(linewidth=0.25), 
      panel.grid.minor.x = element_line(linewidth=0.15),
      strip.background = element_blank(),  
      strip.placement = "outside",  
      strip.text = element_text(face = "bold", size = 14),
      axis.ticks = element_blank(), 
      axis.title.x = element_text(margin = margin(t = 15)),
      axis.title.y = element_text(margin = margin(r = 15)),
      panel.border = element_blank(),
      legend.position="none",
      panel.spacing=unit(0, "cm"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16) 
    )
  return(plot)
}
