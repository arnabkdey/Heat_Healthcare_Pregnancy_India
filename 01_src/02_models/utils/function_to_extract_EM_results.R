#' Calculate Subgroup Effects from Interaction Models
#'
#' @description
#' Extracts subgroup-specific odds ratios, interaction p-values, and RERI (Relative
#' Excess Risk due to Interaction) from regression models containing interaction terms.
#' Uses the msm package for delta method calculations of confidence intervals.
#'
#' @param model A fitted regression model object (typically glm with family=binomial for logistic regression).
#'              The model must include interaction terms between exposure and modifier variables.
#' @param exposure Character string specifying the name of the exposure variable in the model.
#' @param modifier Character string specifying the name of the effect modifier variable in the model.
#'
#' @return A list containing three data frames:
#' \describe{
#'   \item{odds_ratios}{Data frame with exposure odds ratios for each level of the modifier variable}
#'   \item{interaction_p_values}{Data frame with p-values for each interaction term}
#'   \item{reri}{Data frame with RERI estimates and confidence intervals for each non-reference level}
#' }
#'
#' @details
#' This function analyzes interaction effects in regression models by:
#' 1. Extracting odds ratios for the exposure within each level of the modifier
#' 2. Calculating p-values for each interaction term
#' 3. Computing RERI (Relative Excess Risk due to Interaction) as a measure of additive interaction
#'    using the msm package's deltamethod function for accurate confidence intervals
#'
#' The function assumes:
#' - The model is properly specified with interaction terms in the format "exposure:modifierlevel"
#' - For categorical modifiers, the reference level is correctly set in the model
#' - The model's vcov matrix is available and contains all necessary terms
#'
#' @note
#' RERI is calculated as RR11 - RR10 - RR01 + 1, where:
#' - RR11 is the joint effect of exposure and modifier
#' - RR10 is the effect of exposure alone (at reference level of modifier)
#' - RR01 is the effect of modifier alone (without exposure)
#'
#' RERI > 0 indicates positive interaction on the additive scale.
#'
#' @importFrom msm deltamethod
#'
#' @author Arnab K. Dey
#' @date April 2025

calculate_subgroup_effects <- function(model, exposure, modifier) {
  # Load required package
  if (!requireNamespace("msm", quietly = TRUE)) {
    stop("Package 'msm' is needed for this function to work. Please install it.")
  }
  # Step-0: Preprocessing
  ## Get model coefficients and variance-covariance matrix
  coefs <- coef(model)
  vcov_matrix <- vcov(model)

  ## Identify interaction terms
  interaction_terms <- grep(paste0("^", exposure, ":", modifier), names(coefs), value = TRUE)
  modifier_levels <- gsub(paste0(exposure, ":", modifier), "", interaction_terms)

  ## Get all levels from the data
  all_levels <- levels(model$data[[modifier]])
  reference_level <- all_levels[!all_levels %in% modifier_levels][1]

  ## Main effect of exposure
  beta_exposure <- coefs[exposure]
  se_exposure <- sqrt(vcov_matrix[exposure, exposure])

  ## Initialize results dataframe
  levels_to_process <- all_levels
  n_levels <- length(levels_to_process)

  # Step-1: Extract odds ratios and confidence intervals
  ## Initialize results data frame
  results <- data.frame(
    ModifierLevel = levels_to_process,
    OR = numeric(n_levels),
    OR_LowerCI = numeric(n_levels),
    OR_UpperCI = numeric(n_levels),
    stringsAsFactors = FALSE
  )

  ## Find reference level index
  ref_idx <- which(results$ModifierLevel == reference_level)

  ## Calculate OR for reference level
  results[ref_idx, "OR"] <- exp(beta_exposure)
  results[ref_idx, "OR_LowerCI"] <- exp(beta_exposure - 1.96 * se_exposure)
  results[ref_idx, "OR_UpperCI"] <- exp(beta_exposure + 1.96 * se_exposure)

  ## Calculate OR for each non-reference level
  for (i in 1:n_levels) {
    if (i != ref_idx) {
      level <- levels_to_process[i]
      interaction_term <- paste0(exposure, ":", modifier, level)
      
      if (interaction_term %in% names(coefs)) {
        beta_interaction <- coefs[interaction_term]
        se_interaction <- sqrt(vcov_matrix[interaction_term, interaction_term])
        cov_exp_int <- vcov_matrix[exposure, interaction_term]
        
        # Combined effect
        effect <- beta_exposure + beta_interaction
        se_effect <- sqrt(se_exposure^2 + se_interaction^2 + 2*cov_exp_int)
        
        # Calculate OR and CI
        results[i, "OR"] <- exp(effect)
        results[i, "OR_LowerCI"] <- exp(effect - 1.96 * se_effect)
        results[i, "OR_UpperCI"] <- exp(effect + 1.96 * se_effect)
      }
    }
  }

  # Step-2: Calculate interaction p-values
  ## Initialize interaction p-values data frame
  interaction_tests <- data.frame(
    ModifierLevel = modifier_levels,
    Interaction_P = numeric(length(modifier_levels)),
    stringsAsFactors = FALSE
  )
  ## Find reference level index
  for (i in 1:length(interaction_terms)) {
    term <- interaction_terms[i]
    if (term %in% names(coefs)) {
      coef_val <- coefs[term]
      se_val <- sqrt(vcov_matrix[term, term])
      z_val <- coef_val / se_val
      interaction_tests$Interaction_P[i] <- 2 * pnorm(-abs(z_val))
    }
  }

  # Step-3: Calculate RERI
  ## Initialize RERI results data frame
  reri_results <- data.frame(
    ModifierLevel = modifier_levels,
    RERI = numeric(length(modifier_levels)),
    RERI_LowerCI = numeric(length(modifier_levels)),
    RERI_UpperCI = numeric(length(modifier_levels)),
    stringsAsFactors = FALSE
  )

  ## Get reference level values
  RR10 <- results[ref_idx, "OR"] # Exposure effect in reference group

  ## calculate RERI for each modifier level
  for (i in 1:length(modifier_levels)) {
  level <- modifier_levels[i]
  modifier_term <- paste0(modifier, level)
  interaction_term <- paste0(exposure, ":", modifier, level)
  
  # Get level index in results
  level_idx <- which(results$ModifierLevel == level)
      
  # Get RR01 (modifier effect alone)
  if (modifier_term %in% names(coefs)) {
    beta_modifier <- coefs[modifier_term]
    RR01 <- exp(beta_modifier)
  } else {
    RR01 <- 1
    beta_modifier <- 0
  }
      
  # Calculate RERI using msm package's deltamethod function
  if (interaction_term %in% names(coefs)) {
    beta_interaction <- coefs[interaction_term]
    
    # Create parameter vector
    param_vector <- c(beta_exposure, beta_modifier, beta_interaction)
    
    # Define RERI formula as an expression
    reri_formula <- expression(exp(x1 + x2 + x3) - exp(x1) - exp(x2) + 1)
    
    # Extract relevant portion of variance-covariance matrix
    if (modifier_term %in% names(coefs)) {
      # Both modifier main effect and interaction are in model
      param_names <- c(exposure, modifier_term, interaction_term)
    } else {
      # Only interaction term is in model (no modifier main effect)
      param_names <- c(exposure, interaction_term)
      param_vector <- c(beta_exposure, beta_interaction)
      reri_formula <- expression(exp(x1 + x3) - exp(x1) - 1 + 1) # Simplified since exp(0) = 1
    }
    
    # Get the subset of the variance-covariance matrix
    vcov_subset <- vcov_matrix[param_names, param_names]
    
    # Calculate RERI
    RR11 <- exp(beta_exposure + beta_modifier + beta_interaction)
    RERI <- RR11 - RR10 - RR01 + 1
    
    # Use deltamethod to calculate standard error
    RERI_SE <- deltamethod(reri_formula, param_vector, vcov_subset)
    
  } else {
    # No interaction term in model
    RERI <- NA
    RERI_SE <- NA
  }
  
  # Confidence intervals
  reri_results$RERI[i] <- RERI
  if (!is.na(RERI_SE)) {
    reri_results$RERI_LowerCI[i] <- RERI - 1.96 * RERI_SE
    reri_results$RERI_UpperCI[i] <- RERI + 1.96 * RERI_SE
  } else {
    reri_results$RERI_LowerCI[i] <- NA
    reri_results$RERI_UpperCI[i] <- NA
  }
  }

  # Return results as a list
  return(list(
    odds_ratios = results,
    interaction_p_values = interaction_tests,
    reri = reri_results
  ))
}

#' Process Interaction Models and Export Results to Excel
#'
#' @description
#' This function processes a list of interaction models, extracting subgroup effects,
#' interaction p-values, and RERI (Relative Excess Risk due to Interaction) measures.
#' Results are organized by modifiers and exposures and exported to three separate
#' Excel workbooks.
#'
#' @param model_list A named list of fitted models. Model names should follow the pattern
#'                  "exposure_X_modifier" to enable automatic extraction of variables.
#' @param path_output Character string specifying the directory path where the Excel
#'                   workbooks should be saved.
#'
#' @return A list containing three workbook objects:
#' \describe{
#'   \item{odds_ratios_wb}{Workbook with odds ratios and confidence intervals by modifier and exposure}
#'   \item{interaction_p_wb}{Workbook with interaction p-values by modifier and exposure}
#'   \item{reri_wb}{Workbook with RERI values and confidence intervals by modifier and exposure}
#' }
#'
#' @details
#' The function extracts exposure and modifier information from model names,
#' processes each combination, and organizes results into separate worksheets by modifier.
#' Each worksheet contains results for all exposures that interact with that modifier.
#'
#' This function depends on the `calculate_subgroup_effects` function which
#' extracts odds ratios, interaction p-values, and RERI from the models.
#'
#' @note
#' Excel workbooks are saved with the following names:
#' - subgroup_odds_ratios.xlsx
#' - subgroup_interaction_pvalues.xlsx
#' - subgroup_reri.xlsx
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeDataTable setColWidths createStyle addStyle saveWorkbook
#' @importFrom here here
#' @importFrom msm deltamethod
#'
#' @author Arnab K. Dey
#' @date April 2025

library(openxlsx)
library(here)
library(msm) # Add msm package dependency

# Main processing code remains unchanged
process_models <- function(model_list, path_output) {
  # Extract all unique modifiers and exposures from model names
  all_model_names <- names(model_list)

  # Parse model names to extract exposures and modifiers
  model_info <- lapply(all_model_names, function(name) {
    # Extract parts based on the pattern described
    parts <- strsplit(name, "_X_")[[1]]
    if (length(parts) < 2) {
      return(NULL) # Skip if pattern doesn't match
    }
    exposure <- parts[1]
    modifier_part <- parts[2]

    # Use the modifier part directly without additional parsing
    modifier <- modifier_part

    return(list(
      model_name = name,
      exposure = exposure,
      modifier = modifier
    ))
  })

  # Remove NULL entries
  model_info <- model_info[!sapply(model_info, is.null)]

  # Extract unique modifiers and exposures
  unique_modifiers <- unique(sapply(model_info, function(x) x$modifier))
  unique_exposures <- unique(sapply(model_info, function(x) x$exposure))

  # Create three workbooks
  wb_odds_ratios <- createWorkbook()
  wb_interaction_p <- createWorkbook()
  wb_reri <- createWorkbook()

  # Process each modifier
  for (modifier in unique_modifiers) {
    # Create a clean sheet name (remove special characters that Excel doesn't like)
    sheet_name <- gsub("[\\/:*?\"<>|]", "_", modifier)

    # Create sheets for each modifier in each workbook
    addWorksheet(wb_odds_ratios, sheet_name)
    addWorksheet(wb_interaction_p, sheet_name)
    addWorksheet(wb_reri, sheet_name)

    # Create data frames to store all results for this modifier
    odds_results <- data.frame(
      Exposure = character(),
      ModifierLevel = character(),
      OR = numeric(),
      OR_LowerCI = numeric(),
      OR_UpperCI = numeric(),
      stringsAsFactors = FALSE
    )

    int_p_results <- data.frame(
      Exposure = character(),
      ModifierLevel = character(),
      Interaction_P = numeric(),
      stringsAsFactors = FALSE
    )

    reri_results <- data.frame(
      Exposure = character(),
      ModifierLevel = character(),
      RERI = numeric(),
      RERI_LowerCI = numeric(),
      RERI_UpperCI = numeric(),
      stringsAsFactors = FALSE
    )

    # Process each exposure for this modifier
    for (exposure in unique_exposures) {
      # Find the model with this exposure and modifier
      matching_model_info <- NULL
      for (info in model_info) {
        if (info$exposure == exposure && info$modifier == modifier) {
          matching_model_info <- info
          break
        }
      }

      if (!is.null(matching_model_info)) {
        model_name <- matching_model_info$model_name
        model <- model_list[[model_name]]

        # Extract the actual exposure and modifier variable names from the model
        exposure_var <- exposure
        modifier_var <- modifier

        # Calculate subgroup effects
        effects <- calculate_subgroup_effects(model, exposure_var, modifier_var)

        # Process odds ratios
        odds_data <- effects$odds_ratios
        odds_data$Exposure <- exposure
        odds_df <- data.frame(
          Exposure = odds_data$Exposure,
          ModifierLevel = odds_data$ModifierLevel,
          OR = odds_data$OR,
          OR_LowerCI = odds_data$OR_LowerCI,
          OR_UpperCI = odds_data$OR_UpperCI,
          stringsAsFactors = FALSE
        )
        odds_results <- rbind(odds_results, odds_df)

        # Add blank row
        odds_results <- rbind(odds_results, data.frame(
          Exposure = "",
          ModifierLevel = "",
          OR = NA,
          OR_LowerCI = NA,
          OR_UpperCI = NA,
          stringsAsFactors = FALSE
        ))

        # Process interaction p-values
        p_data <- effects$interaction_p_values
        if (nrow(p_data) > 0) {
          p_data$Exposure <- exposure
          p_df <- data.frame(
            Exposure = p_data$Exposure,
            ModifierLevel = p_data$ModifierLevel,
            Interaction_P = p_data$Interaction_P,
            stringsAsFactors = FALSE
          )
          int_p_results <- rbind(int_p_results, p_df)

          # Add blank row
          int_p_results <- rbind(int_p_results, data.frame(
            Exposure = "",
            ModifierLevel = "",
            Interaction_P = NA,
            stringsAsFactors = FALSE
          ))
        }

        # Process RERI
        reri_data <- effects$reri
        if (nrow(reri_data) > 0) {
          reri_data$Exposure <- exposure
          reri_df <- data.frame(
            Exposure = reri_data$Exposure,
            ModifierLevel = reri_data$ModifierLevel,
            RERI = reri_data$RERI,
            RERI_LowerCI = reri_data$RERI_LowerCI,
            RERI_UpperCI = reri_data$RERI_UpperCI,
            stringsAsFactors = FALSE
          )
          reri_results <- rbind(reri_results, reri_df)

          # Add blank row
          reri_results <- rbind(reri_results, data.frame(
            Exposure = "",
            ModifierLevel = "",
            RERI = NA,
            RERI_LowerCI = NA,
            RERI_UpperCI = NA,
            stringsAsFactors = FALSE
          ))
        }
      }
    }

    # Write all results to their respective sheets
    if (nrow(odds_results) > 0) {
      writeDataTable(wb_odds_ratios,
        sheet = sheet_name,
        x = odds_results,
        startRow = 1, startCol = 1,
        tableStyle = "TableStyleMedium2"
      )
    }

    if (nrow(int_p_results) > 0) {
      writeDataTable(wb_interaction_p,
        sheet = sheet_name,
        x = int_p_results,
        startRow = 1, startCol = 1,
        tableStyle = "TableStyleMedium2"
      )
    }

    if (nrow(reri_results) > 0) {
      writeDataTable(wb_reri,
        sheet = sheet_name,
        x = reri_results,
        startRow = 1, startCol = 1,
        tableStyle = "TableStyleMedium2"
      )
    }

    # Format numbers for better readability
    if (nrow(odds_results) > 0) {
      setColWidths(wb_odds_ratios, sheet = sheet_name, cols = 1:5, widths = c(30, 20, 15, 15, 15))

      # Format numbers using style
      for (i in 2:(nrow(odds_results) + 1)) {
        for (j in 3:5) {
          style <- createStyle(numFmt = "0.00")
          addStyle(wb_odds_ratios, sheet = sheet_name, style = style, rows = i, cols = j)
        }
      }
    }

    if (nrow(int_p_results) > 0) {
      setColWidths(wb_interaction_p, sheet = sheet_name, cols = 1:3, widths = c(30, 20, 15))

      # Format p-values
      for (i in 2:(nrow(int_p_results) + 1)) {
        style <- createStyle(numFmt = "0.0000")
        addStyle(wb_interaction_p, sheet = sheet_name, style = style, rows = i, cols = 3)
      }
    }

    if (nrow(reri_results) > 0) {
      setColWidths(wb_reri, sheet = sheet_name, cols = 1:5, widths = c(30, 20, 15, 15, 15))

      # Format numbers
      for (i in 2:(nrow(reri_results) + 1)) {
        for (j in 3:5) {
          style <- createStyle(numFmt = "0.00")
          addStyle(wb_reri, sheet = sheet_name, style = style, rows = i, cols = j)
        }
      }
    }
  }

  # Save the workbooks
  saveWorkbook(wb_odds_ratios, here(path_output, "subgroup_odds_ratios.xlsx"), overwrite = TRUE)
  saveWorkbook(wb_interaction_p, here(path_output, "subgroup_interaction_pvalues.xlsx"), overwrite = TRUE)
  saveWorkbook(wb_reri, here(path_output, "subgroup_reri.xlsx"), overwrite = TRUE)

  return(list(
    odds_ratios_wb = wb_odds_ratios,
    interaction_p_wb = wb_interaction_p,
    reri_wb = wb_reri
  ))
}
