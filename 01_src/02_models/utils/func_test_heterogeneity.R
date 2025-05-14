#' Test Effect Modification Using Delta Method
#'
#' @description
#' This function tests for effect modification using the delta method from the msm package.
#' It calculates subgroup-specific odds ratios, interaction p-values, and RERI (Relative
#' Excess Risk due to Interaction) from regression models containing interaction terms.
#'
#' @param model_list A list of fitted models, where each model contains interaction terms
#'                  between an exposure and a modifier.
#' @param path_output Character string specifying where to save the results.
#'
#' @return A list containing three data frames:
#' \describe{
#'   \item{odds_ratios}{Data frame with exposure odds ratios for each level of the modifier}
#'   \item{interaction_p_values}{Data frame with p-values for each interaction term}
#'   \item{reri}{Data frame with RERI estimates and confidence intervals}
#' }
#'
#' @importFrom msm deltamethod
#' @importFrom openxlsx createWorkbook addWorksheet writeDataTable setColWidths createStyle addStyle saveWorkbook
#' @importFrom here here
#' @importFrom lme4 getME

test_effect_modification <- function(model_list, path_output) {
  # Load required package
  if (!requireNamespace("msm", quietly = TRUE)) {
    stop("Package 'msm' is needed for this function to work. Please install it.")
  }
  
  # Initialize results storage
  all_results <- list(
    odds_ratios = data.frame(),
    interaction_p_values = data.frame(),
    reri = data.frame()
  )
  # Process each model
  for (model_name in names(model_list)) {
    model <- model_list[[model_name]]
    
    # Extract exposure and modifier from model name
    parts <- strsplit(model_name, "_X_")[[1]]
    exposure <- sub(".*(exp.*)", "\\1", parts[1])
    modifier <- parts[2]
    
    # Get model coefficients and variance-covariance matrix
    coefs <- fixef(model)  # Use fixef instead of coef for glmer models
    vcov_matrix <- vcov(model)
    
    # Get the data frame from the model
    model_data <- model@frame  # Access the frame slot directly
    
    # Identify interaction terms
    interaction_terms <- grep(paste0("^", exposure, ":", modifier), names(coefs), value = TRUE)
    
    # Get modifier levels from the data
    modifier_var <- grep(paste0("^", modifier), names(model_data), value = TRUE)[1]
    all_levels <- levels(model_data[[modifier_var]])
    
    # Get reference level (first level)
    reference_level <- all_levels[1]
    
    # Main effect of exposure
    beta_exposure <- coefs[exposure]
    se_exposure <- sqrt(vcov_matrix[exposure, exposure])
    
    # Calculate odds ratios for each level
    for (level in all_levels) {
      if (level == reference_level) {
        # Reference level
        OR <- exp(beta_exposure)
        OR_SE <- se_exposure * exp(beta_exposure)
      } else {
        # Non-reference level
        interaction_term <- paste0(exposure, ":", modifier, level)
        beta_interaction <- coefs[interaction_term]
        se_interaction <- sqrt(vcov_matrix[interaction_term, interaction_term])
        cov_exp_int <- vcov_matrix[exposure, interaction_term]
        
        # Combined effect
        effect <- beta_exposure + beta_interaction
        se_effect <- sqrt(se_exposure^2 + se_interaction^2 + 2*cov_exp_int)
        
        OR <- exp(effect)
        OR_SE <- se_effect * exp(effect)
      }
      
      # Store odds ratio results
      all_results$odds_ratios <- rbind(all_results$odds_ratios,
        data.frame(
          Model = model_name,
          ModifierLevel = level,
          OR = OR,
          OR_SE = OR_SE,
          OR_LowerCI = OR - 1.96 * OR_SE,
          OR_UpperCI = OR + 1.96 * OR_SE
        )
      )
      
      # Calculate interaction p-value for non-reference levels
      if (level != reference_level) {
        interaction_term <- paste0(exposure, ":", modifier, level)
        z_val <- coefs[interaction_term] / sqrt(vcov_matrix[interaction_term, interaction_term])
        p_val <- 2 * pnorm(-abs(z_val))
        
        all_results$interaction_p_values <- rbind(all_results$interaction_p_values,
          data.frame(
            Model = model_name,
            ModifierLevel = level,
            Interaction_P = p_val
          )
        )
        
        # Calculate RERI
        RR11 <- OR
        RR10 <- exp(beta_exposure)
        RR01 <- exp(coefs[paste0(modifier, level)])
        
        # Create parameter vector for delta method
        param_vector <- c(beta_exposure, 
                         coefs[paste0(modifier, level)],
                         coefs[interaction_term])
        
        # Define RERI formula
        reri_formula <- expression(exp(x1 + x2 + x3) - exp(x1) - exp(x2) + 1)
        
        # Get relevant portion of variance-covariance matrix
        param_names <- c(exposure, 
                        paste0(modifier, level),
                        interaction_term)
        vcov_subset <- vcov_matrix[param_names, param_names]
        
        # Calculate RERI and its standard error
        RERI <- RR11 - RR10 - RR01 + 1
        RERI_SE <- deltamethod(reri_formula, param_vector, vcov_subset)
        
        all_results$reri <- rbind(all_results$reri,
          data.frame(
            Model = model_name,
            ModifierLevel = level,
            RERI = RERI,
            RERI_SE = RERI_SE,
            RERI_LowerCI = RERI - 1.96 * RERI_SE,
            RERI_UpperCI = RERI + 1.96 * RERI_SE
          )
        )
      }
    }
  }
  
  # Save results to Excel
  wb <- createWorkbook()
  
  # Add sheets for each type of result
  addWorksheet(wb, "Odds_Ratios")
  addWorksheet(wb, "Interaction_P_Values")
  addWorksheet(wb, "RERI")
  
  # Write data to sheets
  writeDataTable(wb, "Odds_Ratios", all_results$odds_ratios)
  writeDataTable(wb, "Interaction_P_Values", all_results$interaction_p_values)
  writeDataTable(wb, "RERI", all_results$reri)
  
  # Format odds ratios
  setColWidths(wb, sheet = "Odds_Ratios", cols = 1:6, widths = c(30, 20, 15, 15, 15, 15))
  if (nrow(all_results$odds_ratios) > 0) {
    for (i in 2:(nrow(all_results$odds_ratios) + 1)) {
      for (j in 3:6) {
        style <- createStyle(numFmt = "0.00")
        addStyle(wb, sheet = "Odds_Ratios", style = style, rows = i, cols = j)
      }
    }
  }
  
  # Format RERI
  setColWidths(wb, sheet = "RERI", cols = 1:6, widths = c(30, 20, 15, 15, 15, 15))
  if (nrow(all_results$reri) > 0) {
    for (i in 2:(nrow(all_results$reri) + 1)) {
      for (j in 3:6) {
        style <- createStyle(numFmt = "0.00")
        addStyle(wb, sheet = "RERI", style = style, rows = i, cols = j)
      }
    }
  }
  
  # Format p-values
  setColWidths(wb, sheet = "Interaction_P_Values", cols = 1:3, widths = c(30, 20, 15))
  for (i in 2:(nrow(all_results$interaction_p_values) + 1)) {
    style <- createStyle(numFmt = "0.0000")
    addStyle(wb, sheet = "Interaction_P_Values", style = style, rows = i, cols = 3)
  }
  
  # Save workbook
  saveWorkbook(wb, here(path_output, "effect_modification_results.xlsx"), overwrite = TRUE)
  
  return(all_results)
} 
