# Load required libraries
pacman::p_load(tidyverse, data.table, janitor, fst, openxlsx, googledrive, here, beepr, Hmisc)
library(gridExtra)
library(patchwork)

# Load data
path_processed <- here("2-data", "2.2-processed-data")
df_paper_final <- readRDS(here(path_processed, "1.4-dhs-IR-merged-w-clim-zones.rds"))
colnames(df_paper_final)


tabyl(df_paper_final, any_preg_comp_sum)
tabyl(df_paper_final, any_del_comp_sum)

# Create data frames
df_preg <- data.frame(
  any_preg_comp_sum = c(0, 1, 2, 3),
  n = c(96115, 42070, 14477, 6134),
  percent = c(0.60527343, 0.26493111, 0.09116728, 0.03862818)
)

df_del <- data.frame(
  any_del_comp_sum = c(0, 1, 2, 3),
  n = c(77899, 34221, 36196, 10480),
  percent = c(0.49056022, 0.21550291, 0.22794025, 0.06599662)
)

# Create bar chart for pregnancy complications
plot_preg <- ggplot(df_preg, aes(x = factor(any_preg_comp_sum), y = percent)) +
  geom_bar(stat = "identity", fill = "#b521b8") +
  geom_text(aes(label = sprintf("%.2f%%", percent * 100)), vjust = -0.5) +
  labs(title = "Pregnancy Complications",
       x = "Number of Complications",
       y = "Percentage") +
  theme_minimal() +
  ylim(0, max(df_preg$percent) * 1.1)

# Create bar chart for delivery complications
plot_del <- ggplot(df_del, aes(x = factor(any_del_comp_sum), y = percent)) +
  geom_bar(stat = "identity", fill = "#6f92d3") +
  geom_text(aes(label = sprintf("%.2f%%", percent * 100)), vjust = -0.5) +
  labs(title = "Delivery Complications",
       x = "Number of Complications",
       y = "Percentage") +
  theme_minimal() +
  ylim(0, max(df_del$percent) * 1.1)

# Arrange plots side by side
comb_plot <- grid.arrange(plot_preg, plot_del, ncol = 2)

# Save plot
ggsave(here("3-outputs", "figures", "fig-1-descr.png"), comb_plot, width = 10, height = 5, units = "in", dpi = 300)
