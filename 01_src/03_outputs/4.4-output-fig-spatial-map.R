# --------------------
# @project: Heat and healthcare contact during pregnancy in India
# @author: Arnab K. Dey,  arnabxdey@gmail.com 
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script calculates temperature exposure variables for specific time periods related to healthcare visits.
# @date: Dec 12, 2024

rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, googledrive, here)
library(climExposuR)
source("paths.R")

# Read datasets -----
## Long-term WBGT data ----
df_lt_vars_2014 <- read_fst(here(path_project, "processed-data", "2.2-df_wbgt_hw_exceed_vars.fst"), 
  as.data.table = T, 
  columns = c("psu", "date", "lat", "long", "max_temp_wb"))

# filter to the year 2021
df_wbgt_study <- df_lt_vars_2014[date >= "2020-01-01" & date <= "2020-12-31", .(psu = as.factor(psu), lat, long, date, max_temp_wb)]
df_wbgt_study |> glimpse()
rm(df_lt_vars_2014)

# Count number of days when WBGT is below 10 for each PSU using data.table
df_plot <- df_wbgt_study[, .(
  lat = first(lat),
  long = first(long),
  below_10 = sum(max_temp_wb < 10),
  btn_10_15 = sum(max_temp_wb >= 10 & max_temp_wb < 15),
  btn_15_25 = sum(max_temp_wb >= 15 & max_temp_wb < 25),
  btn_25_30 = sum(max_temp_wb >= 25 & max_temp_wb < 30),
  above_30 = sum(max_temp_wb >= 30)
), by = psu]

df_plot |> glimpse()

## Check the total num of days
df_test <- df_plot[, .(
  sum_days = below_10 + btn_10_15 + btn_15_25 + btn_25_30 + above_30)]

tabyl(df_test$sum_days)
## num of days bw 2019-01-01 and 2020-12-31 = 730

# Plot each of the 5 variables on a spatial map in five different panels
library(ggplot2)
library(tidyr)
library(maps)
library(viridis)

# First reshape the data from wide to long format
df_plot_long <- df_plot %>%
  pivot_longer(
    cols = c(below_10, btn_10_15, btn_15_25, btn_25_30, above_30),
    names_to = "temp_range",
    values_to = "count"
  )

# Create factor with nice labels
df_plot_long$temp_range <- factor(
  df_plot_long$temp_range,
  levels = c("below_10", "btn_10_15", "btn_15_25", "btn_25_30", "above_30"),
  labels = c("Below 10°C", "10-15°C", "15-25°C", "25-30°C", "Above 30°C")
)

# Create the plot
ggplot(df_plot_long) +
  geom_point(aes(x = long, y = lat, color = count), size = 0.01, alpha = 0.7) +
  facet_wrap(~temp_range, nrow = 1) +
  scale_color_viridis(option = "plasma") +
  coord_map(xlim = c(68, 98), ylim = c(8, 37)) +  # India's approximate bounds
  labs(
    title = "Spatial Distribution of number of days across WBGT temperature bins in 2020\n", 
    color = "Number of Days",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    title = element_text(size = 14, face = "bold"),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(size = 12),
    legend.position = "bottom",
    legend.text = element_text(size = 10, face = "plain"),
    legend.title = element_text(size = 10, face = "plain"),
    axis.title.x = element_text(size = 10, face = "plain"),
    axis.title.y = element_text(size = 10, face = "plain"),
  )

## save the plot
ggsave(here(path_project, "outputs", "figures", "final-figures", "fig-spatial-map-wbgt-temp-ranges.png"), 
  width = 10, height = 4, dpi = 600, bg = "white")
