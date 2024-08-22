# Function to full model ----
func_plot_full_model <- function(df_plot, title = "Here goes the title") {
    plot <- ggplot(df_plot, aes(x = coef, y = threshold_label)) +
        # facet_wrap(~threshold_label, ncol = 1) +
        geom_vline(xintercept = 1, colour = "black", linetype = "dashed", linewidth = 1.5) +
        geom_text(aes(label = sprintf("%.2f", coef)), vjust = -1.5, hjust = 0.5, size = 3.5) +
        geom_point(color = "#dd1c77", size = 3) +
        geom_pointrange(aes(xmin = lci, xmax = uci), color = "#dd1c77", linewidth = 1.5, alpha = 0.5) +
        labs(
            x = "Relative Risk [95% CI]",
            y = "Sum of exceedences above threshold",
            title = title  # Add the title here
        ) +
        # xlim(0.8, 2) +
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

# Function to plot trimesters
func_plot_trim <- function(df_plot, title = "Here goes the title") {
    plot <- ggplot(df_plot, aes(x = coef, y = threshold_label)) +
        geom_point(color = "#dd1c77", size = 1.5) +
        facet_wrap(~ trim, ncol = 3) +
        geom_vline(xintercept = 1, colour = "black", linetype = "dashed") +
        geom_text(aes(label = sprintf("%.2f", coef)), vjust = -1.5, hjust = 0.5, size = 3.5) +
        geom_pointrange(aes(xmin = lci, xmax = uci), color = "#dd1c77", linewidth = 0.5, alpha = 0.5) +
        labs(
            x = "Relative Risk [95% CI]",
            y = "Sum of exceedences above threshold",
            title = title  # Add the title here
        ) +
        # xlim(0.8, 2) +
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
#' @ param data: data frame with the following columns: exp_label, OR, CILow, CIHigh, contrast, effect_modifier
#' @ param effect_modifier_col: column name of the effect modifier
#' @ param effect_modifier_value: value of the effect modifier to filter the data
#' @ return: a plot with the effect modifier as the facet

plot_effect_modifier <- function(data, effect_modifier_col, effect_modifier_value) {
  # Pre-filter the data based on the effect modifier provided by the user
  effect_modifier_col_sym <- sym(effect_modifier_col)
  filtered_data <- data %>% filter(!!effect_modifier_col_sym == effect_modifier_value)
  # Calculate the min and max for CILow and CIHigh
  # y_min <- min(filtered_data$CILow, na.rm = TRUE)
  # y_min <- min(filtered_data$CILow, na.rm = TRUE)
  # y_max <- max(filtered_data$CIHigh, na.rm = TRUE)

  # Generate the plot with Segoe UI font
  plot <- ggplot(filtered_data, aes(y=contrast, x=OR)) +
    # facet_wrap(~exp_label, labeller = label_wrap_gen(width=22), nrow = 1) +
    geom_vline(xintercept=1, colour="darkgray") +  
    geom_pointrange(aes(xmin=CILow, xmax=CIHigh), position = position_dodge(width = 0.65), size = 0.5) +
    labs(y=effect_modifier_value, x="aOR") +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, linetype="dashed") +
    theme_classic(base_size = 12, base_family = "Calibri") +
    theme(
      panel.grid.major = element_line(linewidth=0.25), 
      panel.grid.minor.x = element_line(linewidth=0.15),
      strip.background = element_blank(),  
      strip.placement = "outside",  
      strip.text.y.left = element_text(angle = 0),
      axis.ticks = element_blank(), 
      axis.title.x = element_text(margin = margin(t = 15)), # Increase gap bw text and x axis by adjusting the top margin
      axis.title.y = element_text(margin = margin(r = 15)), # Increase gap bw text and y axis by adjusting the top margin
      panel.border = element_blank(),
      legend.position="none",
      panel.spacing=unit(0, "cm")
    ) + 
    coord_flip() 

  return(plot)
}

plot_wrapper <- function(df) {
  plot_residence <- plot_effect_modifier(df, "effect_modifier", "Residence")
  plot_place_del <- plot_effect_modifier(df, "effect_modifier", "Place of Delivery")
  plot_caste <- plot_effect_modifier(df, "effect_modifier", "Caste")
  plot_religion <- plot_effect_modifier(df, "effect_modifier", "Religion")
  plot_wealth <- plot_effect_modifier(df, "effect_modifier", "Wealth Quintile")
  plot_parity <- plot_effect_modifier(df, "effect_modifier", "Parity")
  
  # Combine the plots using patchwork
  plot_out_patch <- (plot_residence + plot_place_del + 
                      plot_caste + plot_religion + 
                      plot_wealth + plot_parity) + 
    plot_layout(ncol = 2, byrow = TRUE) +
    plot_annotation(tag_levels = "a") 

  return(plot_out_patch)
}


