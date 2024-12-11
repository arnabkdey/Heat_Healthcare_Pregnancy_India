# -------------------------------------------------------------------------------
# @project: Heat and healthcare contact during pregnancy in India
# @author: Arnab K. Dey,  arnabxdey@gmail.com 
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script contains functions for creating standardized visualizations of model results and effect modifications.
# @date: Dec 12, 2024

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)


# Function to process data for plotting (Effect modification) ----
compare_model_coefficients <- function(model1, model2, varlist_exp, model1_name = "Model 1", model2_name = "Model 2") {
  # Tidy both models
  tidy1 <- tidy(model1, conf.int = TRUE)
  tidy2 <- tidy(model2, conf.int = TRUE)
  
  # Filter coefficients of interest
  coef1 <- tidy1 |> 
    filter(term %in% varlist_exp) |>
    mutate(model = model1_name)
  
  coef2 <- tidy2 |> 
    filter(term %in% varlist_exp) |>
    mutate(model = model2_name)
  
  # Combine results
  combined_coef <- bind_rows(coef1, coef2)
  
  # Reshape data for easier comparison
  comparison <- combined_coef |>
    select(model, term, estimate, std.error, conf.low, conf.high, std.error) |>
    pivot_wider(
      names_from = model, 
      values_from = c(estimate, std.error, conf.low, conf.high, std.error),
      names_glue = "{model}_{.value}"
    )

  # Rename the term variables
  comparison <- comparison |>
      dplyr::mutate(
      term = case_when(
        term == "exp_bin_below_10_10" ~ "Below 10 0C",
        term == "exp_bin_10_15_10" ~ "10-15 0C",
        term == "exp_bin_20_25_10" ~ "20-25 0C",
        term == "exp_bin_25_30_10" ~ "25-30 0C",
        term == "exp_bin_above_30_10" ~ "Above 30 0C"
      ))

  return(comparison)
}


# Function to full model ----
func_plot_full_model <- function(df_plot, title = "Here goes the title") {
    plot <- ggplot(df_plot, aes(x = OR, y = term)) +
        # facet_wrap(~threshold_label, ncol = 1) +
        geom_vline(xintercept = 1, colour = "black", linetype = "dashed", linewidth = 1.5) +
        geom_text(aes(label = sprintf("%.2f", OR)), vjust = -1.5, hjust = 0.5, size = 3.5) +
        geom_point(color = "#dd1c77", size = 3) +
        geom_pointrange(aes(xmin = ci_low, xmax = ci_high), color = "#dd1c77", linewidth = 1.5, alpha = 0.5) +
        labs(
            x = "Odds Ratio [95% CI]",
            y = "Temperature Bins",
            title = title 
        ) +
        xlim(0.4, 2) +
        theme_classic(base_size = 14, base_family = "Times New Roman") +
        theme(
            panel.grid.major = element_line(linewidth=0.25), 
            panel.grid.minor.x = element_line(linewidth=0.15),
            strip.background = element_blank(),  
            strip.placement = "outside",  
            strip.text.y.left = element_text(angle = 0),
            strip.text = element_text(face = "bold", size = 14),
            axis.ticks = element_blank(), 
            axis.title.x = element_text(margin = margin(t = 15)),
            axis.title.y = element_text(margin = margin(r = 15)),
            panel.border = element_blank(),
            legend.position="none",
            panel.spacing=unit(0, "cm"),
            plot.title = element_text(hjust = 0.5, face = "bold", size = 16)  # Center the title and make it bold
        )
    return(plot)
}

# Function to plot effect modifiers ----
func_plot_em <- function(comparison_data, title = "Coefficient Comparison", 
                                        x_label = "Temperature Bins", 
                                        y_label = "Odds Ratio [95% CI]",
                                        colors = c("Rural" = "#dd1c77", "Urban" = "#756bb1")) {
  
  p <- ggplot(df_rural, aes(x = term, y = OR, color = level)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    geom_point(position = position_dodge(width = 0.5), size = 3) +
    geom_pointrange(aes(ymin = ci_low, ymax = ci_high), 
                  position = position_dodge(width = 0.5)) +
    geom_text(aes(label = sprintf("%.2f", OR)), 
              position = position_dodge(width = 0.5),
              vjust = -1.5, hjust = 0.5, size = 3) +  # Add data labels
    coord_flip() +
    # scale_y_continuous(trans = 'log10', labels = label_number(accuracy = 0.01)) +  # Log scale for y-axis
    scale_color_manual(values = colors) +  # Use custom colors
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.y = element_text(angle = 0, hjust = 1),
      panel.grid.major.y = element_blank()
    ) +
    labs(
      title = title,
      x = x_label,
      y = y_label,
      color = "Level"
    )    
  return(p)
}


func_plot_em <- function(comparison_data, title = "Coefficient Comparison", 
                         x_label = "Temperature Bins", 
                         y_label = "Odds Ratio [95% CI]",
                         colors = NULL) {
  
  # Determine the unique levels in the data
  levels <- unique(comparison_data$level)
  
  # If colors are not provided, generate a default color palette
  if (is.null(colors)) {
    default_colors <- c("#dd1c77", "#756bb1", "#2c7fb8", "#31a354", "#636363")
    colors <- setNames(default_colors[1:length(levels)], levels)
  } else if (length(colors) != length(levels)) {
    stop("The number of colors provided must match the number of levels in the data.")
  }
  
  # Ensure that the names of the colors match the levels in the data
  if (!all(levels %in% names(colors))) {
    warning("Color names don't match data levels. Adjusting color names to match data levels.")
    names(colors) <- levels
  }
  
  p <- ggplot(comparison_data, aes(x = term, y = OR, color = level)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    geom_point(position = position_dodge(width = 0.5), size = 3) +
    geom_pointrange(aes(ymin = ci_low, ymax = ci_high), 
                    position = position_dodge(width = 0.5)) +
    geom_text(aes(label = sprintf("%.2f", OR)), 
              position = position_dodge(width = 0.5),
              vjust = -1.5, hjust = 0.5, size = 3) +
    coord_flip() +
    scale_color_manual(values = colors) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.y = element_text(angle = 0, hjust = 1),
      panel.grid.major.y = element_blank()
    ) +
    labs(
      title = title,
      x = x_label,
      y = y_label,
      color = "Levels"
    )    
  return(p)
}

# Usage for specifying colors
# plot <- func_plot_em(df_wealth, colors = c("Richer" = "#ff0000", "Poorer" = "#00ff00"))
