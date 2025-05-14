#' Create and Save Love Plots for IPTW Model Comparisons
#'
#' @param df_weighted Dataframe with weights from create_weights_tw_stage function
#' @param exposure_var Name of the exposure variable
#' @param confounders Vector of confounder variable names
#' @param weight_var Name of the weight variable (default = "iptw_weights_trimmed")
#' @param var_labels Named vector of variable labels (optional)
#' @param exposure_label Label for the exposure variable (optional)
#' @param output_dir Directory to save the plot (default = NULL, which doesn't save)
#' @param file_format Format for saving ("png", "pdf", or "jpg", default = "png")
#' @param plot_width Width of the plot (default = 12)
#' @param plot_height Height of the plot (default = 10)
#' @param plot_dpi Resolution for saving (default = 300)
#' @param dataset_name Name of the dataset (optional, will use attribute if not provided)
#'
#' @return A ggplot object of the love plot
#'
#' @examples
#' # After creating weights
#' # df_weighted <- create_weights_tw_stage(df, "exposure", confounders)
#' # love_plot <- create_love_plot(df_weighted, "exposure", confounders)
#' # print(love_plot)
#'
create_love_plot <- function(
    df_weighted,
    exposure_var,
    confounders,
    weight_var = "iptw_weights_trimmed",
    var_labels = NULL,
    exposure_label = NULL,
    output_dir = NULL,
    file_format = "png",
    plot_width = 12,
    plot_height = 10,
    plot_dpi = 300,
    dataset_name = NULL
) {
  # Load required packages
  required_packages <- c("cobalt", "ggplot2")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("Package", pkg, "is required but not installed."))
    }
    library(pkg, character.only = TRUE)
  }
  
  # Check if df has the weight variable
  if (!weight_var %in% names(df_weighted)) {
    stop(paste("Weight variable", weight_var, "not found in dataframe."))
  }
  
  # If no var_labels provided, use the names of the confounders
  if (is.null(var_labels)) {
    var_labels <- setNames(confounders, confounders)
  } else if (!all(confounders %in% names(var_labels))) {
    # If var_labels doesn't include all confounders, add the missing ones
    missing_vars <- confounders[!confounders %in% names(var_labels)]
    var_labels <- c(var_labels, setNames(missing_vars, missing_vars))
  }
  
  # Set exposure label if not provided
  if (is.null(exposure_label)) {
    exposure_label <- exposure_var
  }
  
  # Extract model type used for weighting (from attributes)
  model_used <- attr(df_weighted, "model_used")
  if (is.null(model_used)) {
    model_used <- "unknown"
  }
  
  # Get dataset name from attributes if not provided
  if (is.null(dataset_name)) {
    dataset_name <- attr(df_weighted, "dataset_name")
    if (is.null(dataset_name)) {
      dataset_name <- "unknown_dataset"
    }
  }
  
  # Create the love plot
  love_plot <- cobalt::love.plot(
    df_weighted[confounders], 
    treat = df_weighted[[exposure_var]],
    weights = df_weighted[[weight_var]],
    binary = "std",
    threshold = 0.1,
    abs = FALSE,
    var.order = "alphabetical",
    var.names = var_labels,
    colours = c("#E41A1C", "#377EB8"),  # Red for unweighted, blue for weighted
    shapes = c(16, 17),                  # Circle for unweighted, triangle for weighted
    size = 3,
    position = "center",
    title = paste0("Covariate Balance Before and After Weighting\n", 
                   "Exposure Variable: ", exposure_label),
    sample.names = c("Unweighted", "Weighted"),
    line = FALSE
  )
  
  # Customize the plot to move the legend to the bottom
  love_plot <- love_plot +
    ggplot2::theme(
      legend.position = "bottom",         # Move legend to the bottom
      legend.title = element_blank(),     # Remove legend title
      legend.text = element_text(size = 12),  # Adjust legend text size
      legend.key.size = unit(1.5, "lines"),   # Adjust legend key size
      plot.title = element_text(hjust = 0.5), # Center title
      axis.text.y = element_text(size = 10),  # Adjust y-axis label size for readability
      axis.title = element_text(size = 12),   # Adjust axis title size
      panel.grid.minor = element_blank(),     # Remove minor grid lines
      panel.grid.major.y = element_line(color = "gray90"), # Lighten horizontal grid lines
      panel.background = element_rect(fill = "white", color = NA) # White background
    )
  
  # Save the plot if output_dir is specified
  if (!is.null(output_dir)) {
    # Create output directory if it doesn't exist
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    # Create filename with model type and dataset name
    filename <- paste0(dataset_name, "_", exposure_var, "_", model_used, ".", file_format)
    file_path <- file.path(output_dir, filename)
    
    # Save plot
    message(paste("Saving plot to:", file_path))
    
    ggplot2::ggsave(
      file_path,
      plot = love_plot,
      width = plot_width,
      height = plot_height,
      units = "in",
      dpi = plot_dpi,
      bg = "white"
    )
  }
  
  # Return the plot object
  return(love_plot)
}



# Example usage:
# Create love plot for a single model
# df_gaussian <- all_weighted_data_two_stage_gaussian[["tmax_wbgt"]][["exp_cumu_85_scaled10"]]
# colnames(df_gaussian)

# love_plot <- create_love_plot(
#   df_weighted = df_gaussian,
#   exposure_var = "exp_cumu_85_scaled10",
#   confounders = confounders,
#   dataset_name = "tmax_wbgt",
#   output_dir = here(path_outputs, "figures", "love_plots", "two_stage_gaussian")
# )
