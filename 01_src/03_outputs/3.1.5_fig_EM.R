# ---------------------------------------------------------------------------------
# @project: Heat and Home Births in India
# @author: Anna Dimitrova, Arnab K. Dey
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script plots the odds ratios and RERI for the effect modification analysis
# @date: March 2025

# load packages ----
rm(list = ls())
pacman::p_load(tidyverse, readxl, ggpubr, cowplot)

# set paths ----
source(here("paths.R"))

# source functions ----
source(here("01_src", "03_outputs", "utils", "function_process_excel_data.R"))
source(here("01_src", "03_outputs", "utils", "function_EM_plots.R"))

# load data ----
## odds ratios ----
file_path_or <- here(path_outputs, 
                     "models", "effect_modification", 
                     "subgroup_odds_ratios.xlsx")

df_or <- read_excel_sheets(file_path_or) |>
  standardize_modifiers() |>
  standardize_levels()

## RERI ----
file_path_reri <- here(path_outputs, 
                       "models", "effect_modification", 
                       "subgroup_reri.xlsx")

df_reri <- read_excel_sheets(file_path_reri) |>
  standardize_modifiers() |>
  standardize_levels()

# Filter by exposure type
## Odds Ratios
df_or_tmax_wb <- df_or |> filter(Exposure == "days_cutoff_tmax_wbgt_900_greater")
df_or_tmin_wb <- df_or |> filter(Exposure == "days_cutoff_tmin_wbgt_100_less")
df_or_tmax_db <- df_or |> filter(Exposure == "days_cutoff_tmax_db_era5_900_greater")
df_or_tmin_db <- df_or |> filter(Exposure == "days_cutoff_tmin_db_era5_100_less")

## RERI
df_reri_tmax_wb <- df_reri |> filter(Exposure == "days_cutoff_tmax_wbgt_900_greater")
df_reri_tmin_wb <- df_reri |> filter(Exposure == "days_cutoff_tmin_wbgt_100_less")
df_reri_tmax_db <- df_reri |> filter(Exposure == "days_cutoff_tmax_db_era5_900_greater")
df_reri_tmin_db <- df_reri |> filter(Exposure == "days_cutoff_tmin_db_era5_100_less")

# Define modifier categories
modifiers <- c(
  "A. Residence", 
  "B. Distance is a big problem", 
  "C. Household wealth", 
  "D. Woman's education"
)
create_or_plot(df_or_tmax_wb, modifiers)
min(df_or_tmin_wb$OR_LowerCI)
max(df_or_tmin_wb$OR_UpperCI)
min(df_reri_tmin_wb$RERI_LowerCI)
max(df_reri_tmin_wb$RERI_UpperCI)

# Create plots ----
## Wet bulb Tmax plots
final_plots_tmax_wb <- create_modifier_plots(df_or_tmax_wb, df_reri_tmax_wb, modifiers)

### Combine all wet bulb Tmax plots
all_final_plots_tmax_wb <- plot_grid(plotlist = final_plots_tmax_wb, ncol = 2, align = "v") +
  theme(plot.background = element_rect(fill = "white", color = "white"))

### Save wet bulb Tmax plot
ggsave(here(path_outputs, "figures", "effect_modification", "Plot_EM_wbgt_tmax.pdf"),
  plot = all_final_plots_tmax_wb, 
  width = 7.5, 
  height = 8, 
  dpi = 600)

## Wet bulb Tmin plots
final_plots_tmin_wb <- create_modifier_plots(df_or_tmin_wb, df_reri_tmin_wb, modifiers)

### Combine all wet bulb Tmin plots
all_final_plots_tmin_wb <- plot_grid(plotlist = final_plots_tmin_wb, ncol = 2, align = "v") +
  theme(plot.background = element_rect(fill = "white", color = "white"))

### Save wet bulb Tmin plot
ggsave(here(path_outputs, "figures", "effect_modification", "Plot_EM_wbgt_tmin.pdf"),
  plot = all_final_plots_tmin_wb, 
  width = 7.5, 
  height = 8, 
  dpi = 600)

## Dry bulb Tmax plots
final_plots_tmax_db <- create_modifier_plots(df_or_tmax_db, df_reri_tmax_db, modifiers)

### Combine all dry bulb Tmax plots
all_final_plots_tmax_db <- plot_grid(plotlist = final_plots_tmax_db, ncol = 2, align = "v") +
  theme(plot.background = element_rect(fill = "white", color = "white"))

### Save dry bulb Tmax plot
ggsave(here(path_outputs, "figures", "effect_modification", "Plot_EM_db_tmax.pdf"),
  plot = all_final_plots_tmax_db, 
  width = 7.5, 
  height = 8, 
  dpi = 600)

## Dry bulb Tmin plots
final_plots_tmin_db <- create_modifier_plots(df_or_tmin_db, df_reri_tmin_db, modifiers)

### Combine all dry bulb Tmin plots
all_final_plots_tmin_db <- plot_grid(plotlist = final_plots_tmin_db, ncol = 2, align = "v") +
  theme(plot.background = element_rect(fill = "white", color = "white"))

### Save dry bulb Tmin plot
ggsave(here(path_outputs, "figures", "effect_modification", "Plot_EM_db_tmin.pdf"),
  plot = all_final_plots_tmin_db, 
  width = 7.5, 
  height = 8, 
  dpi = 600)

# Save consolidated data ----
write_csv(df_or, here(path_outputs, "models", "effect_modification", "consolidated", "subgroup_ORs_consolidated.csv"))
write_csv(df_reri, here(path_outputs, "models", "effect_modification", "consolidated", "subgroup_reri_consolidated.csv"))