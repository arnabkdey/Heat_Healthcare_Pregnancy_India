#' Process Statistical Models and Export Results to Excel
#'
#' @description
#' Extracts key information from a collection of statistical models (likely GEE models),
#' processes exposure coefficients and confidence intervals, and exports the 
#' organized results to an Excel file.
#'
#' @param input_rds Character string specifying the path to the RDS file containing saved models.
#' @param output_xlsx Character string specifying the path where the Excel output file should be saved.
#'
#' @return No return value, but creates an Excel file at the specified location with model results.
#'
#' @details
#' This function:
#' 1. Loads a collection of models from an RDS file
#' 2. Extracts key coefficients for the exposure variables
#' 3. Computes odds ratios and 95% confidence intervals
#' 4. Parses model names to extract metadata (tertile, exposure, threshold, temp_type)
#' 5. Checks model convergence
#' 6. Classifies exposures (wet bulb/dry bulb temperature, threshold types, temp_type)
#' 7. Exports organized results to an Excel file
#'
#' The function expects model names to follow a specific format with components separated
#' by hyphens, where the third component is the tertile and the fourth is the exposure variable.
#' For example: "model-type-tertile1-tmax_wb_hotday_90".
#'
#' @note
#' - The function includes extensive error handling and debugging output
#' - Designed for GEE (Generalized Estimating Equation) models based on the convergence check
#' - Automatically categorizes exposures by type (wet bulb/dry bulb), threshold (percentile/absolute),
#'   and temp_type (1-5 days)
#' - Results are sorted by tertile, threshold, and temp_type
#'
#' @importFrom tidyverse |> 
#' @importFrom broom tidy
#' @importFrom dplyr bind_rows mutate case_when arrange
#' @importFrom stringr str_split str_extract grepl
#' @importFrom openxlsx write.xlsx
#' @importFrom here here
#'
#' @author Arnab K. Dey
#' @date March 2025

# Load required libraries
library(tidyverse)
library(broom)
library(here)
library(openxlsx)

# Define the function
process_models_to_excel <- function(input_rds, output_xlsx) {
  # Read the saved models
  all_models <- readRDS(input_rds)
  
  # Print number of models for debugging
  cat("Number of models loaded:", length(all_models), "\n")
  
  # Print first few model names
  cat("First few model names:\n")
  print(head(names(all_models)))
  
  # Initialize empty dataframe with specified columns
  results <- data.frame(
    model = character(),
    tertile = character(),
    exposure = character(),
    estimate = numeric(),
    conf_low = numeric(),
    conf_high = numeric(),
    converged = logical(),
    n_rows = numeric(),
    stringsAsFactors = FALSE
  )

  # Loop through each model and extract coefficients
  for (model_name in names(all_models)) {
    cat("\nProcessing model:", model_name, "\n")
    
    # Parse model name to get components
    components <- str_split(model_name, "-")[[1]]
    
    # Debug output for components
    cat("Components:", paste(components, collapse=", "), "\n")
    
    if(length(components) < 4) {
      warning(paste("Unexpected model name format:", model_name))
      next
    }
    
    tertile <- components[3]
    exposure_var <- components[4]
    
    # Get the model
    model <- all_models[[model_name]]
    
    # Check if model exists
    if(is.null(model)) {
      warning(paste("Model not found:", model_name))
      next
    }
    
    # Debug output for model
    cat("Model class:", class(model), "\n")
    
    # Check convergence
    converged <- !is.null(model) && !is.null(model$geese$error) && model$geese$error == 0
    
    # Get the number of rows in the model data
    n_rows <- tryCatch({
      if(!is.null(model$data)) {
        nrow(model$data)
      } else {
        NA_integer_
      }
    }, error = function(e) {
      warning(paste("Error getting row count:", e$message))
      NA_integer_
    })
    
    # Try to extract coefficients with detailed error handling
    model_coef <- tryCatch({
      # Get model summary without confidence intervals
      tidy_result <- broom::tidy(model, exponentiate = FALSE)
      
      # Filter for exposure variable
      exposure_coef <- tidy_result[tidy_result$term == exposure_var, ]
      
      if(nrow(exposure_coef) > 0) {
        # Calculate confidence intervals manually
        # For 95% CI, use 1.96 * standard error
        beta <- exposure_coef$estimate
        se <- exposure_coef$std.error
        ci_lower <- beta - (1.96 * se)
        ci_upper <- beta + (1.96 * se)
        
        # Exponentiate everything for odds ratios
        data.frame(
          estimate = exp(beta),
          conf.low = exp(ci_lower),
          conf.high = exp(ci_upper)
        )
      } else {
        warning(paste("No coefficient found for exposure:", exposure_var))
        NULL
      }
    }, error = function(e) {
      warning(paste("Error extracting coefficients:", e$message))
      NULL
    })
    
    # If successful, add to results
    if (!is.null(model_coef)) {
      new_row <- data.frame(
        model = model_name,
        tertile = tertile,
        exposure = exposure_var,
        estimate = model_coef$estimate,
        conf_low = model_coef$conf.low,
        conf_high = model_coef$conf.high,
        converged = converged,
        n_rows = n_rows,
        stringsAsFactors = FALSE
      )
      
      results <- bind_rows(results, new_row)
      cat("Successfully added results for model:", model_name, "\n")
    }
  }
  
  # Check if we have any results
  if(nrow(results) == 0) {
    stop("No results were extracted from any models")
  }
  
  # Print structure of results before mutation
  cat("\nStructure of results before adding categories:\n")
  str(results)
  
  # Add exposure type and threshold columns
  results <- results |>
    mutate(
      exposure_type = case_when(
        grepl("wb", exposure) ~ "Wet Bulb",
        grepl("era", exposure) ~ "Dry Bulb - ERA5",
        TRUE ~ "Dry Bulb - NOAA"
      ),
      threshold = str_extract(exposure, "(?<=_)\\d+"),
      temp_type = case_when(
        grepl("tmax", exposure) ~ "Tmax",
        grepl("tmin", exposure) ~ "Tmin",
        TRUE ~ NA_character_
      ),
      threshold_type = case_when(
        grepl("perc", exposure) ~ "Percentile",
        grepl("abs", exposure) ~ "Absolute",
        TRUE ~ NA_character_
      ),
    )
  
  # Arrange results
  results <- results |>
    arrange(exposure_type, temp_type, threshold_type, threshold)
  
  # Save results to Excel
  write.xlsx(results, output_xlsx)
  
  # Print final summary
  cat("\nNumber of models processed:", nrow(results), "\n")
  print(head(results))
}

