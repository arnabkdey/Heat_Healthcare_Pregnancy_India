rm(list =ls())
options(scipen=999)
options(digits=5)
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(sjPlot)
library(readxl)
library(here)
library(extrafont)
library(ggbreak)
library(patchwork)
# font_import() # run only once
# loadfonts(device="win") # run only once


# Function to read interaction data from excel and combine them ----
read_excel_sheets_and_combine <- function(file_path, effect_modifier_name) {
  # Reading sheet names from the Excel file
  sheets_to_read <- excel_sheets(file_path)
  
  # Reading each sheet, adding the sheet name and effect modifier
  combined_data <- lapply(sheets_to_read, function(sheet_name) {
    read_excel(file_path, sheet = sheet_name) %>%
      mutate(tabname = sheet_name) # Add the sheet name as a column
  }) %>%
    bind_rows() %>%
    mutate(effect_modifier = effect_modifier_name) # Add the effect modifier
  
  return(combined_data)
}

# Run the function for all the effect modifiers ----
here_output_files <- here("3-outputs", "models", "models-with-interaction")

df_wealth <- read_excel_sheets_and_combine(here(here_output_files, "multcomp-cis-wealth.xlsx"), "Wealth")
df_age <- read_excel_sheets_and_combine(here(here_output_files, "multcomp-cis-age.xlsx"), "Age")

data <- rbind(df_wealth, df_age)
data <- data |> mutate(outcome = case_when(
                            str_detect(tabname, "preg") ~ "preg_comp",
                            str_detect(tabname, "del") ~ "del_comp"))

data <- data |> 
          mutate(OR = exp(estimate), 
                 CILow = exp(conf.low), 
                 CIHigh = exp(conf.high))        
# max(data$CIHigh)
# colnames(data)
# dim(data_b)

## Filter and transform data for specified contrasts, calculate OR and CI, and retain relevant columns.
data <- data %>% 
  filter(contrast %in% c("Poor", "Rich",
                         "15-24", "25-34", "35-49")) |> 
  rename(exposure = tabname) 


## Order the levels of the Contrast variable
ord_contrast <- c(
          "Poor", "Rich",
          "15-24", "25-34", "35-49")

data$contrast <- factor(data$contrast, levels=rev(ord_contrast))
data <- data %>% mutate(contrast = fct_reorder(contrast, desc(contrast))) 

## Create outcome_cat variable for plotting
data <- data |> 
          mutate(outcome_cat = case_when(
            outcome == "preg_comp" ~ "Pregnancy Complications",
            outcome == "del_comp" ~ "Delivery Complications"
          ))

## Order the levels of the outcome_cat variable
ord_outcome <- c("Pregnancy Complications", "Delivery Complications")
data$outcome_cat <- factor(data$outcome_cat, levels=ord_outcome)
data <- data |> 
          mutate(outcome_cat = fct_reorder(outcome_cat, desc(outcome_cat)))

# Create and save the plots ---- 
## Create directory -----
path_fig_out <- here("3-outputs", "figures", "interactions")
if (!dir.exists(path_fig_out)) {
  # Create the directory if it does not exist
  dir.create(path_fig_out, showWarnings = TRUE, recursive = TRUE)
}

## Plot for Wealth ----

### Process data
df_plot_wealth <- data |> filter(effect_modifier == "Wealth") 
df_plot_wealth$outcome_cat <- factor(df_plot_wealth$outcome_cat, levels=ord_outcome)
df_plot_wealth <- df_plot_wealth |> mutate(outcome_cat = fct_reorder(outcome_cat, desc(outcome_cat)))

### Create plot
plot_wealth <- ggplot(df_plot_wealth, aes(x = OR, y = contrast)) + 
          facet_wrap(~outcome_cat, ncol = 2) + 
          geom_point(color = "#756bb1", size = 3) +
          geom_pointrange(aes(xmin = CILow, xmax = CIHigh), 
                            position = position_dodge(width = 0.65), 
                            color = "#756bb1",
                            linewidth = 1.5,
                            alpha = 0.5) +
          geom_vline(xintercept = 1, 
                      colour = "darkgray", 
                      linewidth = 1.5,
                      linetype = "dashed") +
          labs(y= "Household Wealth", x="Relative Risk [95% CI]") +
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

### Save plot
ggsave(here(path_fig_out, "plot-int-wealth.svg"), plot_wealth, width = 10, height = 8, dpi = 600)

## Plot for Age ----
### Process data
df_plot_age <- data |> filter(effect_modifier == "Age") 
df_plot_age$outcome_cat <- factor(df_plot_age$outcome_cat, levels=ord_outcome)
df_plot_age <- df_plot_age |> mutate(outcome_cat = fct_reorder(outcome_cat, desc(outcome_cat)))


### Create plot
plot_age <- ggplot(df_plot_age, aes(x = OR, y = contrast)) + 
          facet_wrap(~outcome_cat, ncol = 2) + 
          geom_point(color = "#756bb1", size = 3) +
          geom_pointrange(aes(xmin = CILow, xmax = CIHigh), 
                            position = position_dodge(width = 0.65), 
                            color = "#756bb1",
                            linewidth = 1.5,
                            alpha = 0.5) +
          geom_vline(xintercept = 1, 
                      colour = "darkgray", 
                      linewidth = 1.5,
                      linetype = "dashed") +
          labs(y= "Age Group", x="Relative Risk [95% CI]") +
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

### Save plot
ggsave(here(path_fig_out, "plot-int-age.svg"), plot_age, width = 10, height = 8, dpi = 600)
