# --------------------
# @project: Heat and Home Births in India
# @author: Anna Dimitrova, Arnab K. Dey
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script plots the coefficients of the full models for maximum and minimum temperatures
# @date: March 2025

# load packages ----
rm(list = ls())
pacman::p_load(here, readxl, dplyr, tidyverse, stringr, ggpubr, cowplot, gtable, grid)

# set paths ----
source(here("paths.R"))

# load data ----
data <- read_excel(here(path_outputs, 
  "models", "full_models", "coefs_all_models.xlsx"), 
  col_names = TRUE)

# data cleaning ----
## rename tertile to sample
data <- data |> 
  rename(sample = tertile)

## Clean threshold - fixing the 'th' superscript
data$threshold[data$threshold == "800"] <- "80th pc"
data$threshold[data$threshold=="825"] <- "82.5th pc"
data$threshold[data$threshold=="850"] <- "85th pc"
data$threshold[data$threshold=="875"] <- "87.5th pc"
data$threshold[data$threshold=="900"] <- "90th pc"
data$threshold[data$threshold=="925"] <- "92.5th pc"
data$threshold[data$threshold=="950"] <- "95th pc"
data$threshold[data$threshold=="050"] <- "5th pc"
data$threshold[data$threshold=="075"] <- "7.5th pc"
data$threshold[data$threshold=="100"] <- "10th pc"
data$threshold[data$threshold=="125"] <- "12.5th pc"
data$threshold[data$threshold=="150"] <- "15th pc"
data$threshold[data$threshold=="175"] <- "17.5th pc"
data$threshold[data$threshold=="200"] <- "20th pc"

# process data for the plots ----
## Split data into WBGT and DB, then into Tmax and Tmin
# WBGT data
data_wbgt_tmax <- data |> 
  filter(grepl("tmax_wbgt", exposure, ignore.case = TRUE)) |>
  # Create factor with proper order for Tmax thresholds (descending)
  mutate(threshold = factor(threshold, 
    levels = c("95th pc", "92.5th pc", "90th pc",
               "87.5th pc", "85th pc", "82.5th pc", 
               "80th pc"))) |>
  arrange(threshold) |>
  mutate(temp_id = 1:n()) |>
  mutate(temp_id = as.factor(temp_id))

data_wbgt_tmin <- data |> 
  filter(grepl("tmin_wbgt", exposure, ignore.case = TRUE)) |>
  # Create factor with proper order for Tmin thresholds (descending)
  mutate(threshold = factor(threshold, 
    levels = c("20th pc", "17.5th pc", "15th pc", 
               "12.5th pc", "10th pc", "7.5th pc", 
               "5th pc"))) |>
  arrange(threshold) |>
  mutate(temp_id = 1:n()) |>
  mutate(temp_id = as.factor(temp_id))

# Dry bulb data
data_db_tmax <- data |> 
  filter(grepl("tmax_db", exposure, ignore.case = TRUE)) |>
  # Create factor with proper order for Tmax thresholds (descending)
  mutate(threshold = factor(threshold, 
    levels = c("95th pc", "92.5th pc", "90th pc",
               "87.5th pc", "85th pc", "82.5th pc", 
               "80th pc"))) |>
  arrange(threshold) |>
  mutate(temp_id = 1:n()) |>
  mutate(temp_id = as.factor(temp_id))

data_db_tmin <- data |> 
  filter(grepl("tmin_db", exposure, ignore.case = TRUE)) |>
  # Create factor with proper order for Tmin thresholds (descending)
  mutate(threshold = factor(threshold, 
    levels = c("20th pc", "17.5th pc", "15th pc", 
               "12.5th pc", "10th pc", "7.5th pc", 
               "5th pc"))) |>
  arrange(threshold) |>
  mutate(temp_id = 1:n()) |>
  mutate(temp_id = as.factor(temp_id))

# Define color palette ----
# Option 1: Orange-Red palette for Tmax (heat)
colors_tmax <- c(
  "#F98B93",  # Lightest red (made even darker)
  "#F66D7B",
  "#F24E5E",
  "#EC3041",
  "#E01B2C",
  "#CB181D",
  "#99000D"   # Darkest red
)

# Option 3: Purple palette for Tmin
colors_tmin <- c(
  "#4A1486",   # Darkest purple
  "#6A51A3",
  "#807DBA",
  "#9E9AC8",
  "#B5AED6",
  "#C2B7DE",
  "#CFBFE6"  # Lightest purple (made even darker)
)

# Create plot function to maintain consistency
create_temp_plot <- function(data, title, legend_position = "bottom", is_tmax = TRUE) {
  ggplot(data, aes(y = temp_id, x = estimate, color = threshold)) +
    geom_vline(xintercept = 1, color = "black") +
    geom_pointrange(aes(xmin = conf_low, xmax = conf_high), 
                   position = position_dodge(width = 0.5), size = 0.6) +
    geom_point(size = 3) +
    # Add text labels for estimates
    geom_text(aes(label = sprintf("%.2f", estimate)), 
              vjust = -1.2, size = 2.8, color = "black") +
    scale_color_manual(values = if(is_tmax) colors_tmax else colors_tmin, 
                      # Reverse the breaks for legend order
                      breaks = rev(levels(data$threshold))) +
    scale_y_discrete(labels = NULL) +  # Remove y-axis numbers
    labs(
      title = title,
      y = "",
      x = "OR and 95% CI",
      color = ""
    ) +
    theme_minimal() +
    xlim(0.9, 1.15) +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0),
      legend.position = legend_position,
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      panel.grid.minor.y = element_line(linetype = "dotted", color = "gray", size=0.4),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linetype = "dotted", color = "gray", size=0.4),
      axis.text.y = element_blank(),  # Remove y-axis text
      axis.text.x = element_text(size = 7.5, angle = 45, hjust = 1),
      axis.title.x = element_text(size = 8.5),
      axis.title.y = element_text(size = 8.5)
    )
}

# Function to extract legend from a ggplot object
extract_legend <- function(p) {
  g <- ggplotGrob(p)
  legends <- g$grobs[which(sapply(g$grobs, function(x) x$name) == "guide-box")]
  
  if (length(legends) > 0) {
    return(legends[[1]])  # Select the first legend if multiple exist
  } else {
    return(NULL)  # Return NULL if no legend found
  }
}

# Create individual plots ----
## WBGT plots
plot_wbgt_tmax <- create_temp_plot(data_wbgt_tmax, "A. Maximum WBGT", is_tmax = TRUE)
plot_wbgt_tmin <- create_temp_plot(data_wbgt_tmin, "B. Minimum WBGT", is_tmax = FALSE)

## DB plots
plot_db_tmax <- create_temp_plot(data_db_tmax, "A. Maximum Dry Bulb Temperature", is_tmax = TRUE)
plot_db_tmin <- create_temp_plot(data_db_tmin, "B. Minimum Dry Bulb Temperature", is_tmax = FALSE)

# Extract legends
legend_tmax <- extract_legend(
  plot_wbgt_tmax + theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box = "horizontal"
  )
)

legend_tmin <- extract_legend(
  plot_wbgt_tmin + theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box = "horizontal"
  )
)

# Create combined plots ----
## WBGT combined plot
plot_wbgt_tmax_no_legend <- plot_wbgt_tmax + theme(legend.position = "none")
plot_wbgt_tmin_no_legend <- plot_wbgt_tmin + theme(legend.position = "none")

combined_plot_wbgt <- plot_grid(
  plot_wbgt_tmax_no_legend,
  plot_wbgt_tmin_no_legend,
  ncol = 2, align = "v"
)

## DB combined plot
plot_db_tmax_no_legend <- plot_db_tmax + theme(legend.position = "none")
plot_db_tmin_no_legend <- plot_db_tmin + theme(legend.position = "none")

combined_plot_db <- plot_grid(
  plot_db_tmax_no_legend,
  plot_db_tmin_no_legend,
  ncol = 2, align = "v"
)

# Create final plots with legends
create_final_plot <- function(combined_plot, legend_tmax, legend_tmin) {
  ggdraw() + 
    draw_plot(combined_plot, 0, 0.15, 1, 0.85) +
    draw_plot(ggdraw(legend_tmax) + 
                theme(plot.background = element_rect(fill = "white", color = "white")), 
              0, 0.05, 0.5, 0.1) +
    draw_plot(ggdraw(legend_tmin) + 
                theme(plot.background = element_rect(fill = "white", color = "white")), 
              0.5, 0.05, 0.5, 0.1) +
    theme(plot.background = element_rect(fill = "white", color = "white"))
}

final_plot_wbgt <- create_final_plot(combined_plot_wbgt, legend_tmax, legend_tmin)
final_plot_db <- create_final_plot(combined_plot_db, legend_tmax, legend_tmin)

# Save the plots
ggsave(here(path_outputs, "figures", "full_models", "plot_full_models_wbgt.pdf"),
       plot = final_plot_wbgt, 
       width = 8, 
       height = 8, 
       dpi = 600)

ggsave(here(path_outputs, "figures", "full_models", "plot_full_models_db.pdf"),
       plot = final_plot_db, 
       width = 8, 
       height = 8, 
       dpi = 600)
