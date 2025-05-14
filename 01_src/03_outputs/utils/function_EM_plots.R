#' @title Effect Modification Odds Ratio Plot Generator
#' @description Creates forest plots for visualizing effect modification through odds ratios
#' @author Arnab Dey
#' @date Created: May 2025
#' @version 1.0
#' @details
#' This function generates forest plots for effect modification analysis, displaying odds ratios
#' with confidence intervals. The plots are formatted for publication quality with specific
#' styling for medical/scientific journals.
#' 
#' @param data A data frame containing the following columns:
#'   - ModifierLevel: The levels of the effect modifier
#'   - OR: Odds ratio values
#'   - OR_LowerCI: Lower confidence interval for odds ratio
#'   - OR_UpperCI: Upper confidence interval for odds ratio
#'   - Modifier: The name of the effect modifier
#' @param modifier The name of the effect modifier variable
#' 
#' @return A ggplot object containing the forest plot
#' 
#' @examples
#' # Example usage:
#' # create_or_plot_em(data = effect_mod_data, modifier = "AgeGroup")
#' 
#' @import ggplot2
#' @import scales
#' 
#' @export

## plot for Effect modification: Odds Ratio
create_or_plot_em <- function(data, modifier) {
  # Check if we have data for this modifier
  if(nrow(data) == 0) {
    return(NULL)
  }
  
  # Calculate x-axis range with padding (log scale)
  x_range <- range(c(data$OR_LowerCI, data$OR_UpperCI), na.rm = TRUE)
  x_range <- c(min(x_range) * 0.8, max(x_range) * 1.2)

  ggplot(data, aes(y = ModifierLevel)) +
    geom_vline(xintercept = 1, colour = "gray50", linetype = "dashed", linewidth = 0.5) +
    geom_pointrange(aes(x = OR, xmin = OR_LowerCI, xmax = OR_UpperCI), color = "#A11217", size = 1, fatten = 2.5, alpha = 0.8) +
    geom_point(aes(x = OR), size = 3.5, color = "#A11217") +
    geom_text(aes(x = OR, label = sprintf("%.2f", OR)), vjust = -1.2, hjust = 0.5, size = 3.5, family = "Times New Roman") +
    facet_wrap(~ Modifier, scales = "free_y", ncol = 2, strip.position = "top") +
    labs(y = NULL, x = "Odds Ratio [95% CI]") +
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
      strip.text = element_text(face = "bold", size = 13, family = "Times New Roman"),
      axis.ticks = element_blank(),
      axis.title.x = element_text(margin = margin(t = 15)),
      axis.title.y = element_text(margin = margin(r = 15)),
      panel.border = element_blank(),
      legend.position = "none",
      panel.spacing = unit(0.7, "cm"),
      plot.title = element_blank(),
      axis.text.y = element_text(size = 11, family = "Times New Roman"),
      axis.text.x = element_text(size = 11, face = "bold", family = "Times New Roman"),
      plot.background = element_rect(fill = "white", color = "white"),
      panel.background = element_rect(fill = "white", color = "white"),
      plot.margin = margin(10, 20, 10, 20)
    )
}
