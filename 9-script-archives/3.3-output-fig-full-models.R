rm(list =ls())
options(scipen=999)
options(digits=5)
pacman::p_load(tidyverse, ggpubr, ggplot2, sjPlot, readxl, here, extrafont, ggbreak, patchwork)


# font_import() # run only once
# loadfonts(device="win") # run only once

# Load data ----
here_output_files <- here("3-outputs", "models", "models-no-interaction")
df_full_models <- read.csv(here(here_output_files, "models_consolidated_coefficients_comp_age.csv"))
df_full_models <- df_full_models |> filter(!str_detect(exposure_name, "ident"))
df_full_models_t1 <- read.csv(here(here_output_files, "models_consolidated_coefficients_comp_t1.csv"))
df_full_models_t1 <- df_full_models_t1 |> filter(!str_detect(exposure_name, "ident")) |> mutate(trim = "T1")
df_full_models_t2 <- read.csv(here(here_output_files, "models_consolidated_coefficients_comp_t2.csv"))
df_full_models_t2 <- df_full_models_t2 |> filter(!str_detect(exposure_name, "ident")) |> mutate(trim = "T2")
df_full_models_t3 <- read.csv(here(here_output_files, "models_consolidated_coefficients_comp_t3.csv"))
df_full_models_t3 <- df_full_models_t3 |> filter(!str_detect(exposure_name, "ident")) |> mutate(trim = "T3")

head(df_full_models)

# Constants ---------------
## Call the function to plot ----
source(here("1-scripts", "6.3-function-to-plot-models-and-effect-modifiers.R"))
## Path for outputs ----
path_out <- here("3-outputs", "figures", "full-models")
!dir.exists(path_out) && dir.create(path_out, recursive = TRUE)

# Add threshold label to all datasets ----
## Function to add the variable and order the labels ----
func_add_threshold_label <- function(df) {
    # Create new variable for threshold label
    df <- df |> 
        mutate(threshold_label = case_when(
            str_detect(exposure_name, "30") ~ "WBGT>=30°C",
            str_detect(exposure_name, "31") ~ "WBGT>=31°C",
            str_detect(exposure_name, "32") ~ "WBGT>=32°C"
        ))
    # Order the levels of the threshold_label
    ord_thresh <- c("WBGT>=30°C", "WBGT>=31°C", "WBGT>=32°C")
    df$threshold_label <- factor(df$threshold_label, levels=ord_thresh)
    df$threshold_label <- fct_reorder(df$threshold_label, desc(df$threshold_label))    
    return(df)
}

## Apply the function to all datasets ----
df_full_models <- df_full_models |> func_add_threshold_label()
df_full_models_t1 <- df_full_models_t1 |> func_add_threshold_label()
df_full_models_t2 <- df_full_models_t2 |> func_add_threshold_label()
df_full_models_t3 <- df_full_models_t3 |> func_add_threshold_label()


# Subset data based on the dep_var column ----
df_full_preg <- df_full_models %>% filter(dep_var == "preg_comp")
df_full_del <- df_full_models %>% filter(dep_var == "del_comp")
df_t1_preg <- df_full_models_t1 %>% filter(dep_var == "preg_comp")
df_t1_del <- df_full_models_t1 %>% filter(dep_var == "del_comp")
df_t2_preg <- df_full_models_t2 %>% filter(dep_var == "preg_comp")
df_t2_del <- df_full_models_t2 %>% filter(dep_var == "del_comp")
df_t3_preg <- df_full_models_t3 %>% filter(dep_var == "preg_comp")
df_t3_del <- df_full_models_t3 %>% filter(dep_var == "del_comp")


# Plotting the full models ----
## List of dataframes ----
list_df_depvar <- list(
        df_full_preg = df_full_preg, 
        df_full_del = df_full_del)

## Save individual plots ----
# list_plots <- list()
for (i in seq_along(list_df_depvar)) {
    df <- list_df_depvar[[i]]
    dep_var_name <- df$dep_var[1]
    df_name <- names(list_df_depvar)[i]
    df_name_short <- gsub("df_|_preg|_del", "", df_name)
    plot_name <- paste0("plot_", df_name_short, "_", dep_var_name)

    ## Assign full names for the dep_var
    dep_var_name_full <- if (dep_var_name == "preg_comp") {
        "Pregnancy Complications"
    } else if (dep_var_name == "del_comp") {
        "Delivery Complications"
    } else {
        dep_var_name  # Default to the original name if not matched
    }
    
    ## Plot and Save: Absolute temperature ----
    plot_cur <- func_plot_full_model(df, title = dep_var_name_full)
    
    ggsave(here(path_out, paste0(plot_name, ".jpeg")), plot_cur, width = 6, height = 8, dpi = 600)
    
    ## Save the plot to list_plots
    # list_plots[[plot_name]] <- plot_cur
}

# Plotting the trims ----
## Data processing ----
# df_trim_preg <- rbind(df_t1_preg, df_t2_preg, df_t3_preg)
df_trim_del <- rbind(df_t1_del, df_t2_del, df_t3_del)

## Generate the plots ----
plot_trims_del <- func_plot_trim(df_trim_del, title = "Delivery Complications by Trimester")
ggsave(here(path_out, "plot_trims_del.jpeg"), plot_trims_del, width = 14, height = 8, dpi = 600)
