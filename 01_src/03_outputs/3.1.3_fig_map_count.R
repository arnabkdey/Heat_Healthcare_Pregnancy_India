# --------------------
# @project: Heat and healthcare contact during pregnancy in India
# @author: Arnab K. Dey,  arnabxdey@gmail.com
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script calculates temperature exposure variables for specific time periods related to healthcare visits.
# @date: Dec 12, 2024

rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, googledrive, here)
pacman::p_load(ggplot2, tidyr, maps, viridis, patchwork)

# set paths ----
source(here("paths.R"))

# Read datasets -----
## Temperature cutoffs data
### WBGT
df_tmax_wbgt_lt_psu <- read_fst(here(path_processed, "1.3.1.a_df_dhs_tmax_wbgt_psu_5mo.fst"))
df_tmin_wbgt_lt_psu <- read_fst(here(path_processed, "1.3.1.b_df_dhs_tmin_wbgt_psu_5mo.fst"))

### DB ERA
df_tmax_db_era_lt_psu <- read_fst(here(path_processed, "1.3.1.d_df_dhs_tmax_db_era5_psu_5mo.fst"))
df_tmin_db_era_lt_psu <- read_fst(here(path_processed, "1.3.1.e_df_dhs_tmin_db_era5_psu_5mo.fst"))

### save as a list
list_all_psu_data <- list(
  tmax_wbgt = df_tmax_wbgt_lt_psu,
  tmin_wbgt = df_tmin_wbgt_lt_psu,
  tmax_db_era = df_tmax_db_era_lt_psu,            
  tmin_db_era = df_tmin_db_era_lt_psu
)

## Lat long for PSUs
df_dhs_psu_geo <- readRDS(here(path_processed, "1.1.4.a_df_dhs_psu_geo.rds"))

### Convert psu to character and select only the columns we need
df_dhs_psu_geo <- df_dhs_psu_geo |> 
  mutate(psu = as.character(psu)) |>
  dplyr::select(psu, lat, long) |>
  as.data.frame() 

# Merge lat long for PSUs with all datasets ----
all_weighted_data <- lapply(list_all_psu_data, function(df) {
  df <- df |> 
    mutate(psu = as.character(psu)) |>
    left_join(df_dhs_psu_geo, by = c("psu" = "psu"))
  return(df)
})

# Create datasets for plotting ----
## Tmax - WBGT
df_plot_tmax_wbgt <- all_weighted_data$tmax_wbgt |>
  group_by(psu) |>
  summarise(
    lat = first(lat),
    long = first(long),
    cutoff_tmax_wbgt_900 = first(cutoff_tmax_wbgt_900)
  )
nrow(df_plot_tmax_wbgt) # 7860

## Tmin - WBGT
df_plot_tmin_wbgt <- all_weighted_data$tmin_wbgt |>
  group_by(psu) |>
  summarise(
    lat = first(lat),
    long = first(long),
    cutoff_tmin_wbgt_100 = first(cutoff_tmin_wbgt_100)
  )
nrow(df_plot_tmin_wbgt) # 7860

## Tmax - DB ERA
df_plot_tmax_db_era <- all_weighted_data$tmax_db_era |>
  group_by(psu) |>
  summarise(
    lat = first(lat),
    long = first(long),
    cutoff_tmax_db_era5_900 = first(cutoff_tmax_db_era5_900)
  )
nrow(df_plot_tmax_db_era) # 7860

## Tmin - DB ERA
df_plot_tmin_db_era <- all_weighted_data$tmin_db_era |>
  group_by(psu) |>
  summarise(
    lat = first(lat),
    long = first(long),
    cutoff_tmin_db_era5_100 = first(cutoff_tmin_db_era5_100)
  )

nrow(df_plot_tmin_db_era) # 10606

# Plot ----
## Tmax WBGT
plot_tmax_wbgt_max <- ggplot(df_plot_tmax_wbgt) +
  geom_point(aes(x = long, y = lat, color = cutoff_tmax_wbgt_900), size = 0.01, alpha = 0.7) +
  scale_color_gradient2(
    low = "orange", mid = "red", high = "darkred",
    midpoint = median(df_plot_tmax_wbgt$cutoff_tmax_wbgt_900),  
    name = "Long-term cutoff \n (90th percentile)"
  ) +
  coord_map(xlim = c(68, 98), ylim = c(8, 37)) +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  ggtitle("Maximum WBGT 90th percentile") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    legend.position = "right",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text = element_text(size = 8)
  )
plot_tmax_wbgt_max

## Tmin WBGT
plot_tmin_wbgt_max <- ggplot(df_plot_tmin_wbgt) +
  geom_point(aes(x = long, y = lat, color = cutoff_tmin_wbgt_100), size = 0.01, alpha = 0.7) +
  scale_color_gradient2(low = "blue", mid = "green", high = "red", 
                       midpoint = median(df_plot_tmin_wbgt$cutoff_tmin_wbgt_100),
                       name = "Long-term cutoff \n (10th percentile)") +
  coord_map(xlim = c(68, 98), ylim = c(8, 37)) +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  ggtitle("Minimum WBGT 10th percentile") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    legend.position = "right",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text = element_text(size = 8)
  )
plot_tmin_wbgt_max

## Tmax DB ERA
plot_tmax_db_era_max <- ggplot(df_plot_tmax_db_era) +
  geom_point(aes(x = long, y = lat, color = cutoff_tmax_db_era5_900), size = 0.01, alpha = 0.7) +
  scale_color_gradient2(
    low = "orange", mid = "red", high = "darkred",
    midpoint = median(df_plot_tmax_db_era$cutoff_tmax_db_era5_900),
    name = "Long-term cutoff \n (90th percentile)"
  ) +
  coord_map(xlim = c(68, 98), ylim = c(8, 37)) +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  ggtitle("Maximum dry bulb temperature 90th percentile") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    legend.position = "right",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text = element_text(size = 8)
  )
plot_tmax_db_era_max

## Tmin DB ERA
plot_tmin_db_era_max <- ggplot(df_plot_tmin_db_era) +
  geom_point(aes(x = long, y = lat, color = cutoff_tmin_db_era5_100), size = 0.01, alpha = 0.7) +
  scale_color_gradient2(low = "blue", mid = "green", high = "red", 
                       midpoint = median(df_plot_tmin_db_era$cutoff_tmin_db_era5_100),
                       name = "Long-term cutoff \n (10th percentile)") +
  coord_map(xlim = c(68, 98), ylim = c(8, 37)) +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  ggtitle("Minimum dry bulb temperature 10th percentile") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    legend.position = "right",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text = element_text(size = 8)
  )
plot_tmin_db_era_max

## Combine plots usig cowplot
## WBGT 
plots_combined_wbgt <- cowplot::plot_grid(plot_tmax_wbgt_max, plot_tmin_wbgt_max, nrow = 1, ncol = 2)
plots_combined_wbgt

## DB ERA
plots_combined_db_era <- cowplot::plot_grid(plot_tmax_db_era_max, plot_tmin_db_era_max, nrow = 1, ncol = 2)
plots_combined_db_era


# Save plots ----
## WBGT
ggsave(here(path_outputs, "figures", "spatial_maps", "fig_map_wbgt_max_tmax.pdf"), plots_combined_wbgt, bg = "white", width = 10, height = 8, dpi = 300)
ggsave(here(path_outputs, "figures", "spatial_maps", "fig_map_wbgt_max_tmin.pdf"), plots_combined_wbgt, bg = "white", width = 10, height = 8, dpi = 300)


