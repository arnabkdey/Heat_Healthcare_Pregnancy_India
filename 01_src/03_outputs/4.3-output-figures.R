# --------------------
# @project: Heat and healthcare contact during pregnancy in India
# @author: Arnab K. Dey,  arnabxdey@gmail.com 
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script creates publication-ready figures visualizing main effects and effect modification results.
# @date: Dec 12, 2024

# Load required libraries
rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, openxlsx, googledrive, here, beepr, Hmisc)
library(gridExtra)
library(patchwork)
source("paths.R")

# Load results ----
all_models <- openxlsx::read.xlsx(here(path_project, "outputs", "models", "stata-models", "all_models_cleaned.xlsx"))

## create final coefs and CI
all_models <- all_models |>
  dplyr::mutate(OR = round(OR, 2),
                ci_low = round(lci, 2),
                ci_high = round(uci, 2)) |>
  dplyr::select(-lci, -uci)

## Relevel the variable
all_models$term <- factor(all_models$term, levels = c("Above 30 C", "25 to 30 C", "10 to 15 C", "Below 10 C"))

tail(all_models)

# Source function for plots ----
source(here("1-scripts", "6.3-function-to-plot-models.R"))

# Plot ----
## Full model ----
### Data for plotting 
# df_plot_full <- func_proc_df_plot(full_model)
df_plot_full <- all_models |> 
  dplyr::filter(type == "full") |> 
  dplyr::select(-type, -level) 

### Plot ----
p_full <- func_plot_full_model(df_plot_full, title = "") 

### Save plot
ggsave(here(path_project, "outputs", "figures", "final-figs", "fig-1-full-model.png"), 
       bg = "white",
       p_full, 
       width = 8, height = 8, dpi = 600)

## Effect modification by Rural/Urban ----
### Data for plotting ----
df_rural <- all_models |> 
  dplyr::filter(type == "em-rural") 

### Plot ----
p_em_rural <- func_plot_em(df_rural, title = "")

### Save plot
ggsave(here(path_project, "outputs", "figures", "final-figs", "fig-2-em-rural.svg"), p_em_rural, width = 8, height = 8, dpi = 600)

## Effect modification by Access to healthcare ----
### Data for plotting ----
df_access <- all_models |> 
  dplyr::filter(type == "em-distance")

### Plot ----
p_em_access <- func_plot_em(df_access, title = "")

### Save plot
ggsave(here(path_project, "outputs", "figures", "final-figs", "fig-3-em-access.png"), 
       p_em_access, 
       bg = "white",
       width = 8, height = 8, dpi = 600)

## Effect modification by Wealth ----
### Data for plotting ----
df_wealth <- all_models |> 
  dplyr::filter(type == "em-wealth")

### Plot ----
p_em_wealth <- func_plot_em(df_wealth, title = "", colors = c("Poorer" = "#dd1c77", "Richer" = "#756bb1"))

### Save plot
ggsave(here(path_project, "outputs", "figures", "final-figs", "fig-4-em-wealth.png"), 
       p_em_wealth, 
       bg = "white",
       width = 8, height = 8, dpi = 600)

## Effect modification by Education ----
### Data for plotting ----
df_edu <- all_models |> 
  dplyr::filter(type == "em-edu") |>
  dplyr::mutate(level = ifelse(level == "Higher education", "Secondary or higher secondary", level))

### Plot ----
p_em_edu <- func_plot_em(df_edu, title = "", colors = c("Primary education or less" = "#dd1c77", "Secondary or higher secondary" = "#756bb1"))

### Save plot
ggsave(here(path_project, "outputs", "figures", "final-figs", "fig-5-em-edu.png"), 
       p_em_edu, 
       bg = "white",
       width = 8, height = 8, dpi = 600)


