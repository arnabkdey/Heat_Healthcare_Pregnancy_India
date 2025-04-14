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
## Final paper dataset
df_paper_final <- readRDS(here(path_processed, "1.3.2.df_IR_temp_counts_merged.rds"))
df_paper_final$psu <- as.character(df_paper_final$psu)

## Lat long for PSUs
df_dhs_psu_geo <- readRDS(here(path_processed, "1.1.4.a-df-dhs-psu-geo.rds"))
df_dhs_psu_geo$psu <- as.character(df_dhs_psu_geo$psu)

## Lat long for PSUs
df_lat_long <- df_dhs_psu_geo |> 
  as.data.frame() |> 
  select(psu, lat, long)

# Import lat long for PSUs ----
df_paper_final <- df_paper_final |>
  left_join(df_dhs_psu_geo, by = c("psu" = "psu"))

# Create dataset for plotting ----
# Select only the columns we need before converting to data.table
df_plot <- df_paper_final |>
  select(psu, lat, long,
         days_cutoff_tmax_wbgt_900_greater,
         days_cutoff_tmax_db_era5_900_greater,
         days_cutoff_tmin_wbgt_100_less,
         days_cutoff_tmin_db_era5_100_less) |>
  as.data.table()

df_plot <- df_plot[, .(
  lat = first(lat),
  long = first(long),
  tmax_wbgt_90 = sum(days_cutoff_tmax_wbgt_900_greater),
  tmax_db_era_90 = sum(days_cutoff_tmax_db_era5_900_greater),
  tmin_wbgt_10 = sum(days_cutoff_tmin_wbgt_100_less),
  tmin_db_era_10 = sum(days_cutoff_tmin_db_era5_100_less)
), by = psu]


# Plot Tmax and Tmin variables side by side on a spatial map and create different panels for WBGT and DB ERA
## WBGT
### First reshape the data from wide to long format
df_plot_long_wbgt <- df_plot %>%
  pivot_longer(
    cols = c(tmax_wbgt_90, tmin_wbgt_10),
    names_to = "percentiles",
    values_to = "count"
  )

### Create factor with nice labels
df_plot_long_wbgt$percentiles <- factor(
  df_plot_long_wbgt$percentiles,
  levels = c("tmax_wbgt_90", "tmin_wbgt_10"),
  labels = c("Tmax WBGT 90th percentile", "Tmin WBGT 10th percentile")
)

### Split the data by temperature type
df_plot_long_wbgt_max <- df_plot_long_wbgt %>% 
  filter(percentiles == "Tmax WBGT 90th percentile")
df_plot_long_wbgt_min <- df_plot_long_wbgt %>% 
  filter(percentiles == "Tmin WBGT 10th percentile")

### Create separate plots for max and min
plot_wbgt_max <- ggplot(df_plot_long_wbgt_max) +
  geom_point(aes(x = long, y = lat, color = count), size = 0.01, alpha = 0.7) +
  scale_color_gradient2(low = "yellow", mid = "orange", high = "red", 
                       midpoint = mean(df_plot_long_wbgt_max$count),
                       name = "Number of Days\n(Maximum)") +
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

plot_wbgt_min <- ggplot(df_plot_long_wbgt_min) +
  geom_point(aes(x = long, y = lat, color = count), size = 0.01, alpha = 0.7) +
  scale_color_gradient2(low = "lightblue", mid = "blue", high = "darkblue",
                       midpoint = mean(df_plot_long_wbgt_min$count),
                       name = "Number of Days\n(Minimum)") +
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

### Combine the plots
combined_plot <- plot_wbgt_max + plot_wbgt_min

### save the plot
ggsave(here(path_outputs, "figures", "spatial_maps", "fig-spatial-map-wbgt-temp-percentiles.png"), 
       combined_plot,
       width = 12, height = 5, dpi = 600, bg = "white")

## DB ERA 
### First reshape the data from wide to long format
df_plot_long_db_era <- df_plot %>%
  pivot_longer(
    cols = c(tmax_db_era_90, tmin_db_era_10),
    names_to = "percentiles",
    values_to = "count"
  )

### Create factor with nice labels
df_plot_long_db_era$percentiles <- factor(
  df_plot_long_db_era$percentiles,  
  levels = c("tmax_db_era_90", "tmin_db_era_10"),
  labels = c("Tmax DB ERA 90th percentile", "Tmin DB ERA 10th percentile")
)

### Split the data by temperature type
df_plot_long_db_era_max <- df_plot_long_db_era %>% 
  filter(percentiles == "Tmax DB ERA 90th percentile")
df_plot_long_db_era_min <- df_plot_long_db_era %>% 
  filter(percentiles == "Tmin DB ERA 10th percentile")

### Create separate plots for max and min
plot_db_era_max <- ggplot(df_plot_long_db_era_max) +
  geom_point(aes(x = long, y = lat, color = count), size = 0.01, alpha = 0.7) +
  scale_color_gradient2(low = "yellow", mid = "orange", high = "red",
                       midpoint = mean(df_plot_long_db_era_max$count),
                       name = "Number of Days\n(Maximum)") +
  coord_map(xlim = c(68, 98), ylim = c(8, 37)) +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  ggtitle("Maximum DB ERA 90th percentile") +
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

### Create separate plots for min
plot_db_era_min <- ggplot(df_plot_long_db_era_min) +
  geom_point(aes(x = long, y = lat, color = count), size = 0.01, alpha = 0.7) +
  scale_color_gradient2(low = "lightblue", mid = "blue", high = "darkblue",
                       midpoint = mean(df_plot_long_db_era_min$count),
                       name = "Number of Days\n(Minimum)") +
  coord_map(xlim = c(68, 98), ylim = c(8, 37)) +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  ggtitle("Minimum DB ERA 10th percentile") +
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

### Combine the plots
combined_plot_db_era <- plot_db_era_max + plot_db_era_min

### save the plot
ggsave(here(path_outputs, "figures", "spatial_maps", "fig-spatial-map-db-era-temp-percentiles.png"), 
       combined_plot_db_era,
       width = 12, height = 5, dpi = 600, bg = "white")
