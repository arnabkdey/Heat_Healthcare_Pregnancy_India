pacman::p_load(tidyverse, janitor, data.table, fst, openxlsx, here)

# Set paths
source(here("paths.R"))
path_outcome_models <- here(path_outputs, "models", "full_models", "main_models", "models")

# Load functions
source(here("01_src", "03_outputs", "utils", "function_plot_full_models.R"))

# Read the output from @02_models/01_full_models/2.1.2_full_models.R
df_all_coefficients <- openxlsx::read.xlsx(here(path_outcome_models, "consolidated_coefficients.xlsx"))
head(df_all_coefficients)


# Data processing ----
## Create plots for each dataset
df_plot_tmin_wbgt <- df_all_coefficients |> dplyr::filter(Dataset == "tmin_wbgt")
df_plot_tmax_wbgt <- df_all_coefficients |> dplyr::filter(Dataset == "tmax_wbgt")
df_plot_db_tmin <- df_all_coefficients |> dplyr::filter(Dataset == "tmin_db_era5")
df_plot_db_tmax <- df_all_coefficients |> dplyr::filter(Dataset == "tmax_db_era5")

# Define custom labels and order for exposures
exposure_labels_tmin <- c(
  "exp_cumu_5_scaled10" = "5th percentile",
  "exp_cumu_10_scaled10" = "10th percentile",
  "exp_cumu_15_scaled10" = "15th percentile"
)
exposure_order_tmin <- c("exp_cumu_15_scaled10", "exp_cumu_10_scaled10", "exp_cumu_5_scaled10")

exposure_labels_tmax <- c(
  "exp_cumu_95_scaled10" = "95th percentile",
  "exp_cumu_90_scaled10" = "90th percentile",
  "exp_cumu_85_scaled10" = "85th percentile"
)
exposure_order_tmax <- c("exp_cumu_85_scaled10", "exp_cumu_90_scaled10", "exp_cumu_95_scaled10")

# Apply to Tmin - WBGT
df_plot_tmin_wbgt <- df_plot_tmin_wbgt %>%
  mutate(
    Exposure = factor(
      Exposure,
      levels = exposure_order_tmin,
      labels = exposure_labels_tmin[exposure_order_tmin]
    )
  )
# Apply to Tmax - WBGT
df_plot_tmax_wbgt <- df_plot_tmax_wbgt %>%
  mutate(
    Exposure = factor(
      Exposure,
      levels = exposure_order_tmax,
      labels = exposure_labels_tmax[exposure_order_tmax]
    )
  )
# Apply to Tmin - DB
df_plot_db_tmin <- df_plot_db_tmin %>%
  mutate(
    Exposure = factor(
      Exposure,
      levels = exposure_order_tmin,
      labels = exposure_labels_tmin[exposure_order_tmin]
    )
  )
# Apply to Tmax - DB
df_plot_db_tmax <- df_plot_db_tmax %>%
  mutate(
    Exposure = factor(
      Exposure,
      levels = exposure_order_tmax,
      labels = exposure_labels_tmax[exposure_order_tmax]
    )
  )

# Create plots ----
plot_tmax_wbgt <- func_plot_full_model(df_plot_tmax_wbgt, title = "A. Tmax - Wet Bulb Globe Temperature")
plot_tmin_wbgt <- func_plot_full_model(df_plot_tmin_wbgt, title = "B. Tmin - Wet Bulb Globe Temperature")
plot_db_tmax <- func_plot_full_model(df_plot_db_tmax, title = "C. Tmax - Dry Bulb Temperature")
plot_db_tmin <- func_plot_full_model(df_plot_db_tmin, title = "D. Tmin - Dry Bulb Temperature")

# Arrange plots ----
## Plots A and B for WBGT
plot_wbgt <- cowplot::plot_grid(
  plot_tmax_wbgt, plot_tmin_wbgt, 
  ncol = 2,
  rel_widths = c(1, 1)
)

## Plots C and D for DB
plot_db <- cowplot::plot_grid(
  plot_db_tmax, plot_db_tmin, 
  ncol = 2,
  rel_widths = c(1, 1)
)

# Save plots ----
ggplot2::ggsave(
  here(path_outputs, "figures", "full_models", "wbgt.png"),
  plot_wbgt,
  width = 12,
  height = 8
)

ggplot2::ggsave(
  here(path_outputs, "figures", "full_models", "db.png"),
  plot_db,
  width = 12,
  height = 5
)
