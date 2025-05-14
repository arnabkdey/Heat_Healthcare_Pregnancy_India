# load libraries ----
rm(list = ls())
pacman::p_load(tidyverse, janitor, data.table, fst, openxlsx, here, cowplot)

# set paths ----
source(here("paths.R"))

# source functions ----
# source(here("01_src", "03_outputs", "utils", "function_EM_plots.R"))

# load data ----
## read data for ORs from sheet titled 'Odds_Ratios'
df_or <- openxlsx::read.xlsx(here(path_outputs, "models", "effect_modification", "main_models", "effect_modification_results.xlsx"), sheet = "Odds_Ratios")

# data processing ----
## Rename ModifierLevels
rename_modifier_levels <- function(df) {
  df |> mutate(ModifierLevel = case_when(
    ModifierLevel == "urban" ~ "Urban",
    ModifierLevel == "rural" ~ "Rural",
    ModifierLevel == "big-problem" ~ "Distance is a big problem",
    ModifierLevel == "not-a-big-prob" ~ "Distance is not a big problem",
    ModifierLevel == "richer3" ~ "Top 3 wealth quintile",
    ModifierLevel == "poorer" ~ "Bottom 2 wealth quintile",
    ModifierLevel == "primary or higher" ~ "Primary or higher",
    ModifierLevel == "no education" ~ "No education",
    ModifierLevel == "Not-Hindu" ~ "Other religion",
    # ModifierLevel == "SC/ST/OBC" ~ "SC/ST/OBC",
    # ModifierLevel == "General" ~ "General",
    ModifierLevel == "JSY" ~ "JSY states",
    ModifierLevel == "Non-JSY" ~ "Non-JSY states",
    TRUE ~ ModifierLevel
  ))
}

df_or <- rename_modifier_levels(df_or)

## Filter out caste 
df_or <- df_or |> filter(!grepl("General", ModifierLevel) & !grepl("SC/ST/OBC", ModifierLevel))

## Process data for plotting
process_em_data <- function(df, exposure_pattern, exposure_label) {
  # First filter the data
  filtered_df <- df |>
    filter(grepl(exposure_pattern, Model)) 
    # filter(!grepl("caste", Model) & !grepl("religion", Model))
  
  # Check if we have data after filtering
  if(nrow(filtered_df) == 0) {
    warning(sprintf("No data found for pattern: %s", exposure_pattern))
    return(NULL)
  }
  
  # Add Modifier column and Exposure
  filtered_df <- filtered_df |>
    mutate(
      Exposure = exposure_label,
      Modifier = case_when(
        ModifierLevel %in% c("Urban", "Rural") ~ "A. Residence",
        ModifierLevel %in% c("Distance is a big problem", "Distance is not a big problem") ~ "B. Distance is a big problem",
        ModifierLevel %in% c("Top 3 wealth quintile", "Bottom 2 wealth quintile") ~ "C. Household wealth",
        ModifierLevel %in% c("Primary or higher", "No education") ~ "D. Maternal education",
        ModifierLevel %in% c("Other religion", "Hindu") ~ "E. Religion",
        ModifierLevel %in% c("JSY states", "Non-JSY states") ~ "F. Janani Suraksha Yojana"
      )
    ) 
  
  # Verify required columns exist
  required_cols <- c("ModifierLevel", "Modifier", "Exposure")
  missing_cols <- setdiff(required_cols, names(filtered_df))
  if(length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }
  
  filtered_df <- filtered_df %>%
    mutate(
      ModifierLevel = case_when(
        Modifier == "E. Religion" ~ factor(ModifierLevel, levels = c("Hindu", "Other religion")),
        Modifier == "F. Janani Suraksha Yojana" ~ factor(ModifierLevel, levels = c("JSY states", "Non-JSY states")),
        TRUE ~ as.factor(ModifierLevel)
      )
    )
  
  return(filtered_df)
}

# Processed data for Tmax and Tmin, OR and RERI
## WBGT
df_or_plot_tmax_wbgt  <- process_em_data(df_or, "tmax_wbgt_exp_cumu_90", "exp_cumu_90_scaled10")
df_or_plot_tmin_wbgt  <- process_em_data(df_or,   "tmin_wbgt_exp_cumu_10", "exp_cumu_10_scaled10")

## DB
df_or_plot_tmax_db  <- process_em_data(df_or, "tmax_db_era5_exp_cumu_90", "exp_cumu_90_scaled10")
df_or_plot_tmin_db  <- process_em_data(df_or, "tmin_db_era5_exp_cumu_10", "exp_cumu_10_scaled10")


# Verify we have data before proceeding
if(is.null(df_or_plot_tmax_wbgt) || is.null(df_or_plot_tmin_wbgt) ||
   is.null(df_or_plot_tmax_db) || is.null(df_or_plot_tmin_db)) {
  stop("One or more datasets are empty after processing")
}

# set constants ----
## Define modifier categories based on available data
modifiers <- c(
  "A. Residence", 
  "B. Distance is a big problem", 
  "C. Household wealth", 
  "D. Maternal education",
  "E. Religion",
  "F. Janani Suraksha Yojana"
)

# Create Plots ----
## All OR plots
plot_or_tmax_wbgt <- create_or_plot_em(df_or_plot_tmax_wbgt, modifiers)
plot_or_tmin_wbgt <- create_or_plot_em(df_or_plot_tmin_wbgt, modifiers)
plot_or_tmax_db <- create_or_plot_em(df_or_plot_tmax_db, modifiers)
plot_or_tmin_db <- create_or_plot_em(df_or_plot_tmin_db, modifiers)

# Save plots as PNG ----
ggsave(here(path_outputs, "figures", "effect_modification", "png", "EM_tmax_wbgt.png"), plot_or_tmax_wbgt, width = 10, height = 8, dpi = 300)
ggsave(here(path_outputs, "figures", "effect_modification", "png", "EM_tmin_wbgt.png"), plot_or_tmin_wbgt, width = 10, height = 8, dpi = 300)
ggsave(here(path_outputs, "figures", "effect_modification", "png", "EM_tmax_db.png"), plot_or_tmax_db, width = 10, height = 8, dpi = 300)
ggsave(here(path_outputs, "figures", "effect_modification", "png", "EM_tmin_db.png"), plot_or_tmin_db, width = 10, height = 8, dpi = 300)

# Save plots as SVG ----
ggsave(here(path_outputs, "figures", "effect_modification", "svg", "EM_tmax_wbgt.svg"), plot_or_tmax_wbgt, width = 10, height = 8, dpi = 300)
ggsave(here(path_outputs, "figures", "effect_modification", "svg", "EM_tmin_wbgt.svg"), plot_or_tmin_wbgt, width = 10, height = 8, dpi = 300)
ggsave(here(path_outputs, "figures", "effect_modification", "svg", "EM_tmax_db.svg"), plot_or_tmax_db, width = 10, height = 8, dpi = 300)
ggsave(here(path_outputs, "figures", "effect_modification", "svg", "EM_tmin_db.svg"), plot_or_tmin_db, width = 10, height = 8, dpi = 300)

