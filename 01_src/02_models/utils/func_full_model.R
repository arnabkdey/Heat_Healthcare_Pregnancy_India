#' Fit Generalized Linear Mixed Effects Models with Multiple Datasets
#'
#' @description
#' This function fits generalized linear mixed effects models (GLMER) for multiple datasets and exposure variables. 
#' It handles weighted data, random effects, seasonal parameters, and confounders. The function generates comprehensive 
#' output including coefficient tables, odds ratios/rate ratios, and confidence intervals.
#'
#' @param all_weighted_data A list of datasets, where each dataset contains weighted data for different exposure variables.
#' @param outcome_var A string specifying the outcome variable name. Default is "dv_no_contact_3mo".
#' @param outcome_type A string specifying the outcome type: "binary" or "count". Default is "binary".
#' @param random_effects A string specifying the random effects structure. Default is "meta_state_name/meta_dist_name".
#' @param seasonal_params A character vector of seasonal parameters to include in the model. Default is NULL.
#' @param confounders A character vector of confounding variables to include in the model. Default is NULL.
#' @param output_path A string specifying the path where output files should be saved. If NULL, no files are saved.
#'
#' @return A list containing coefficient tables for each dataset and exposure variable. Each coefficient table includes:
#' \itemize{
#'   \item Variable: Name of the predictor variable
#'   \item Estimate: Coefficient estimate
#'   \item Std_Error: Standard error of the estimate
#'   \item Z_Value: Z-statistic
#'   \item P_Value: P-value
#'   \item Odds_Ratio/Rate_Ratio: Exponentiated coefficient
#'   \item CI_Lower: Lower bound of 95% confidence interval
#'   \item CI_Upper: Upper bound of 95% confidence interval
#' }
#'
#' @details
#' The function performs the following operations:
#' \itemize{
#'   \item Processes each dataset and exposure variable
#'   \item Fits GLMER models with specified parameters
#'   \item Calculates coefficient estimates and confidence intervals
#'   \item Generates Excel workbooks with detailed results
#'   \item Creates consolidated results across all datasets
#' }
#'
#' If output_path is provided, the function saves:
#' \itemize{
#'   \item Individual model RDS files
#'   \item Dataset-specific coefficient Excel files
#'   \item Consolidated coefficient Excel files
#' }
#'
#' @examples
#' \dontrun{
#' # Example usage with binary outcome:
#' results <- fit_full_models(
#'   all_weighted_data = my_weighted_data,
#'   outcome_var = "dv_no_contact_3mo",
#'   outcome_type = "binary",
#'   random_effects = "meta_state_name/meta_dist_name",
#'   seasonal_params = c("sin_month", "cos_month"),
#'   confounders = c("age", "education"),
#'   output_path = "path/to/output"
#' )
#' 
#' # Example usage with count outcome:
#' results <- fit_full_models(
#'   all_weighted_data = my_weighted_data,
#'   outcome_var = "dv_num_contact_3mo",
#'   outcome_type = "count",
#'   seasonal_params = c("sin_month", "cos_month"),
#'   confounders = c("age", "education"),
#'   output_path = "path/to/output"
#' )
#' }
#'
#' @importFrom lme4 glmer
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @importFrom here here
#' @importFrom stats as.formula binomial pnorm poisson
#' @importFrom dplyr select
#'
fit_full_models <- function(all_weighted_data, 
                           outcome_var = "dv_no_contact_3mo",
                           outcome_type = "binary",
                           random_effects = "meta_state_name/meta_dist_name",
                           seasonal_params = NULL,
                           confounders = NULL,
                           output_path = NULL) {
  
  # Initialize list to store all coefficients
  all_coefficients <- list()
  
  # Process each dataset and exposure
  for (dataset_name in names(all_weighted_data)) {
    cat("\n  Processing dataset:", dataset_name, "\n")
    
    # Initialize workbook for storing coefficients
    wb <- createWorkbook()
    
    # Initialize list to store coefficients for this dataset
    dataset_coefficients <- list()
    
    for (exposure_var in names(all_weighted_data[[dataset_name]])) {
      cat("\n  Processing exposure:", exposure_var, "\n")
      
      # Get weighted dataset
      df_weighted <- all_weighted_data[[dataset_name]][[exposure_var]]
      
      # Check if weighted dataset is not null
      if (!is.null(df_weighted)) {
        # Prepare formula components
        formula_parts <- c(exposure_var)
        
        # Add confounders if provided
        if (!is.null(confounders)) {
          formula_parts <- c(formula_parts, confounders)
        }
        
        # Add seasonal parameters if provided
        if (!is.null(seasonal_params)) {
          formula_parts <- c(formula_parts, seasonal_params)
        }
        
        # Create formula string
        formula_str <- paste(outcome_var, "~", 
                           paste(formula_parts, collapse = " + "), 
                           "+ (1|", random_effects, ")")
        
        # Fit model based on outcome type
        if (outcome_type == "binary") {
          model <- glmer(as.formula(formula_str),
                        data = df_weighted,
                        family = binomial(link = "logit"),
                        weights = df_weighted$iptw_weights_trimmed)
        } else if (outcome_type == "count") {
          model <- glmer(as.formula(formula_str),
                        data = df_weighted,
                        family = poisson(link = "log"),
                        weights = df_weighted$iptw_weights_trimmed)
        } else {
          stop("outcome_type must be either 'binary' or 'count'")
        }
        
        # Get model summary
        model_summary <- summary(model)
        
        # Create coefficient table
        coef_table <- data.frame(
          Variable = names(fixef(model)),
          Estimate = fixef(model),
          Std_Error = sqrt(diag(vcov(model))),
          Z_Value = fixef(model)/sqrt(diag(vcov(model))),
          P_Value = 2 * (1 - pnorm(abs(fixef(model)/sqrt(diag(vcov(model))))))
        )
        
        # Add odds ratios/rate ratios and confidence intervals
        ratio_name <- ifelse(outcome_type == "binary", "Odds_Ratio", "Rate_Ratio")
        coef_table[[ratio_name]] <- exp(coef_table$Estimate)
        coef_table$CI_Lower <- exp(coef_table$Estimate - 1.96 * coef_table$Std_Error)
        coef_table$CI_Upper <- exp(coef_table$Estimate + 1.96 * coef_table$Std_Error)
        
        # Store exposure coefficients for consolidation
        exposure_coef <- coef_table[coef_table$Variable == exposure_var, ]
        exposure_coef$Dataset <- dataset_name
        exposure_coef$Exposure <- exposure_var
        dataset_coefficients[[exposure_var]] <- exposure_coef
        
        # Create a new sheet for this exposure
        sheet_name <- substr(exposure_var, 1, 31) # Excel sheet name limit
        addWorksheet(wb, sheet_name)
        
        # Write coefficients to sheet
        writeData(wb, sheet = sheet_name, x = coef_table)
        
        # Save model summary if output path is provided
        if (!is.null(output_path)) {
          saveRDS(model, here(output_path,
                             paste0(dataset_name, "_", exposure_var, "_model.rds")))
        }
      }
    }
    
    # Save workbook for this dataset if output path is provided
    if (!is.null(output_path)) {
      saveWorkbook(wb, here(output_path, 
                           paste0(dataset_name, "_coefficients.xlsx")), 
                  overwrite = TRUE)
    }
    
    # Combine all coefficients for this dataset
    all_coefficients[[dataset_name]] <- do.call(rbind, dataset_coefficients)
  }
  
  # Create consolidated workbook if output path is provided
  if (!is.null(output_path)) {
    # Create workbook for individual coefficients
    individual_wb <- createWorkbook()
    
    # Add each dataset's coefficients to separate sheets
    for (dataset_name in names(all_coefficients)) {
      addWorksheet(individual_wb, dataset_name)
      writeData(individual_wb, sheet = dataset_name, x = all_coefficients[[dataset_name]])
    }
    
    # Save consolidated workbook
    saveWorkbook(individual_wb, 
                here(output_path, "individual_coefficients.xlsx"),
                overwrite = TRUE)
    
    # Create workbook for consolidated exposures
    consolidated_wb <- createWorkbook()
    addWorksheet(consolidated_wb, "consolidated_exposures")
    
    # Combine all rows from all datasets' coefficient tables
    consolidated_results <- do.call(rbind, all_coefficients)
    
    # Select relevant columns
    ratio_name <- ifelse(outcome_type == "binary", "Odds_Ratio", "Rate_Ratio")
    consolidated_results <- consolidated_results |>
      dplyr::select(Dataset, Exposure, !!sym(ratio_name), CI_Lower, CI_Upper, P_Value)
    
    # Write the consolidated results to the workbook
    writeData(consolidated_wb, sheet = "consolidated_exposures", x = consolidated_results)
    
    # Save the consolidated workbook
    saveWorkbook(consolidated_wb, 
                here(output_path, "consolidated_coefficients.xlsx"),
                overwrite = TRUE)
  }
  
  # Return the coefficients
  return(all_coefficients)
}
