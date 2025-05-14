#' Run Effect Modification Models
#'
#' @description
#' This function runs GLMER models with interaction terms between exposure variables
#' and effect modifiers. It saves all models in a list for further analysis.
#'
#' @param weighted_data A list of weighted datasets, where each element contains
#'                     a dataset for a specific exposure.
#' @param exposure_vars Character vector of exposure variable names.
#' @param modifier_vars Character vector of modifier variable names.
#' @param seasonal_params Character vector of seasonal parameter names.
#' @param path_output Character string specifying where to save the models.
#'
#' @return A list of fitted models, named by exposure and modifier combinations.
#'
#' @importFrom lme4 glmer
#' @importFrom here here
#' @importFrom openxlsx saveWorkbook

run_effect_modification_models <- function(weighted_data, 
                                         exposure_vars,
                                         modifier_vars,
                                         seasonal_params,
                                         path_output) {
  
  # Initialize list to store all models
  all_models <- list()
  
  # Process each dataset
  for (dataset_name in names(weighted_data)) {
    cat("\nProcessing dataset:", dataset_name, "\n")
    
    # Process each exposure within the dataset
    for (exposure_var in names(weighted_data[[dataset_name]])) {
      cat("\nProcessing exposure:", exposure_var, "\n")
      
      # Get weighted dataset
      df_weighted <- weighted_data[[dataset_name]][[exposure_var]]
      
      # Check if weighted dataset is not null
      if (!is.null(df_weighted)) {
        
        # Process each modifier
        for (modifier in modifier_vars) {
          cat("\nProcessing modifier:", modifier, "\n")
          
          # Create model name that includes dataset
          model_name <- paste0(dataset_name, "_", exposure_var, "_X_", modifier)
          
          # Prepare formula for GLMER model with interaction
          formula_str <- paste("dv_no_contact_3mo ~", 
                             exposure_var, "+", 
                             modifier, "+",
                             paste0(exposure_var, ":", modifier), "+",
                             paste(seasonal_params, collapse = " + "), 
                             "+ (1|meta_state_name)")
          
          # Fit weighted GLMER model
          model <- tryCatch({
            glmer(as.formula(formula_str),
                  data = df_weighted,
                  family = binomial(link = "logit"),
                  weights = df_weighted$iptw_weights_trimmed)
          }, error = function(e) {
            cat("Error fitting model for", model_name, ":", e$message, "\n")
            return(NULL)
          })
          
          # Store model if successfully fitted
          if (!is.null(model)) {
            all_models[[model_name]] <- model
            
            # Save individual model
            saveRDS(model, here(path_output,
                              paste0(model_name, "_model.rds")))
          }
        }
      }
    }
  }
  
  # Save all models as a list
  saveRDS(all_models, here(path_output, "all_effect_modification_models.rds"))
  
  return(all_models)
} 
