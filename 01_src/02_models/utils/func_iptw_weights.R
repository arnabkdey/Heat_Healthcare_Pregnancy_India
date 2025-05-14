pacman::p_load(dplyr, janitor, data.table, fst, openxlsx, here)
source(here("paths.R"))

create_iptw_weights <- function(
    df, 
    exposure_var, 
    confounders, 
    second_stage_model = c("gaussian", "poisson", "negbin", NULL),
    trim_quantile = 0.99,
    symmetric_trim = TRUE,
    trim_quantile_upper = 0.95,
    verbose = TRUE,
    positivity_threshold = c(0.01, 0.99)
) {
  # Load required packages
  required_packages <- c("CBPS", "MASS", "cobalt")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("Package", pkg, "is required but not installed."))
    }
    library(pkg, character.only = TRUE)
  }
  
  # Handle NULL for second_stage_model
  if (is.null(second_stage_model)) {
    single_stage <- TRUE
  } else {
    single_stage <- FALSE
    second_stage_model <- match.arg(second_stage_model)
  }
  
  # --- 0. Preprocessing and Checks ---
  if (verbose) cat("\n=== Preprocessing Checks ===\n")
  
  # Remove missing data
  df <- df[complete.cases(df[c(exposure_var, confounders)]), ]
  if (nrow(df) == 0) stop("No complete cases found.")
  
  # Sample size check
  if (nrow(df) < 10 * length(confounders)) {
    warning("Sample size may be too small for confounders (n < 10*p)")
  }
  
  # Check for perfect separation - different handling for single stage
  if (single_stage) {
    # For single stage, we use the actual exposure variable
    exposure_values <- df[[exposure_var]]
  } else {
    # For two-stage, we first check binary part
    binary_exposure <- as.numeric(df[[exposure_var]] > 0)
    test_logit <- glm(binary_exposure ~ ., 
                     data = df[confounders], 
                     family = binomial)
    if (any(!is.finite(coef(test_logit)))) {
      warning("Perfect separation detected in binary model")
    }
  }
  
  # Correlation check (using model.matrix for factors)
  if (length(confounders) > 1) {
    mm <- model.matrix(~ . - 1, data = df[confounders])
    cor_matrix <- cor(mm)
    high_cor <- which(abs(cor_matrix) > 0.9 & upper.tri(cor_matrix), arr.ind = TRUE)
    if (nrow(high_cor) > 0 && verbose) {
      cat("High correlations (>0.9) found between:\n")
      for (i in seq_len(nrow(high_cor))) {
        v1 <- rownames(cor_matrix)[high_cor[i, 1]]
        v2 <- colnames(cor_matrix)[high_cor[i, 2]]
        val <- cor_matrix[high_cor[i, 1], high_cor[i, 2]]
        cat(sprintf("  %s and %s: %.2f\n", v1, v2, val))
      }
    }
  }
  
  # --- SINGLE STAGE MODEL ---
  if (single_stage) {
    if (verbose) cat("\n=== Fitting Single Stage IPTW Model ===\n")
    
    # For continuous exposure, use CBPS with gaussian family
    if (is.numeric(df[[exposure_var]]) && length(unique(df[[exposure_var]])) > 2) {
      model_formula <- as.formula(paste(exposure_var, "~", paste(confounders, collapse = "+")))
      
      iptw_model <- tryCatch(
        CBPS(model_formula, data = df, family = gaussian, method = "exact"),
        error = function(e) {
          warning("Exact CBPS failed, trying over-parameterized method")
          tryCatch(
            CBPS(model_formula, data = df, family = gaussian, method = "over"),
            error = function(e2) {
              warning("CBPS failed completely, using standard glm")
              glm(model_formula, data = df, family = gaussian)
            }
          )
        }
      )
      
      # Calculate density for continuous exposure
      fitted_values <- tryCatch({
        iptw_model$fitted.values
      }, error = function(e) {
        predict(iptw_model, type = "response")
      })
      
      fitted_sd <- sqrt(mean((df[[exposure_var]] - fitted_values)^2))
      
      # Calculate conditional density
      cond_density <- dnorm(df[[exposure_var]], mean = fitted_values, sd = fitted_sd)
      
      # Calculate marginal density
      marg_mean <- mean(df[[exposure_var]])
      marg_sd <- sd(df[[exposure_var]])
      marg_density <- dnorm(df[[exposure_var]], mean = marg_mean, sd = marg_sd)
      
      # Store model metrics
      model_fit_metrics <- list(
        single_stage_gaussian = list(
          AIC = AIC(glm(model_formula, data = df, family = gaussian)),
          BIC = BIC(glm(model_formula, data = df, family = gaussian))
        )
      )
      
    } else {
      # For binary/categorical exposure, use multinomial logistic regression
      is_binary <- length(unique(df[[exposure_var]])) == 2
      
      if (is_binary) {
        if (verbose) cat("Detected binary exposure, using logistic regression\n")
        
        # Use factor to ensure proper handling
        df[[exposure_var]] <- as.factor(df[[exposure_var]])
        
        model_formula <- as.formula(paste(exposure_var, "~", paste(confounders, collapse = "+")))
        
        iptw_model <- tryCatch(
          CBPS(model_formula, data = df, family = binomial, method = "exact"),
          error = function(e) {
            warning("Exact CBPS failed for binary model, trying over-parameterized method")
            tryCatch(
              CBPS(model_formula, data = df, family = binomial, method = "over"),
              error = function(e2) {
                warning("CBPS failed completely, using standard glm")
                glm(model_formula, data = df, family = binomial)
              }
            )
          }
        )
        
        # Get probabilities and check positivity
        predicted_probs <- tryCatch({
          iptw_model$fitted.values
        }, error = function(e) {
          predict(iptw_model, type = "response")
        })
        
        # Positivity check
        extreme_ps <- sum(predicted_probs < positivity_threshold[1] | 
                          predicted_probs > positivity_threshold[2])
        if (extreme_ps > 0 && verbose) {
          cat("Positivity issues in binary model:", extreme_ps, "extreme propensity scores\n")
          if (verbose) hist(predicted_probs, main = "Propensity Scores")
        }
        
        # For binary, calculate probability for each level
        levels_exposure <- levels(as.factor(df[[exposure_var]]))
        level_1 <- df[[exposure_var]] == levels_exposure[1]
        level_2 <- !level_1
        
        # Calculate conditional densities based on each observation's exposure value
        cond_density <- rep(NA, nrow(df))
        cond_density[level_1] <- 1 - predicted_probs[level_1]  # P(X=0|Z)
        cond_density[level_2] <- predicted_probs[level_2]      # P(X=1|Z)
        
        # Calculate marginal probabilities (prevalence)
        marg_level_1 <- mean(level_1)
        marg_level_2 <- mean(level_2)
        
        # Calculate marginal densities
        marg_density <- rep(NA, nrow(df))
        marg_density[level_1] <- marg_level_1
        marg_density[level_2] <- marg_level_2
        
        # Store model metrics
        model_fit_metrics <- list(
          single_stage_binary = list(
            AIC = AIC(glm(model_formula, data = df, family = binomial)),
            BIC = BIC(glm(model_formula, data = df, family = binomial))
          )
        )
        
      } else {
        # For categorical with >2 levels, use multinomial model
        if (verbose) cat("Detected categorical exposure with multiple levels\n")
        
        # Need nnet package for multinom
        if (!requireNamespace("nnet", quietly = TRUE)) {
          stop("Package nnet is required for multinomial models but not installed.")
        }
        library("nnet")
        
        # Convert to factor
        df[[exposure_var]] <- as.factor(df[[exposure_var]])
        
        model_formula <- as.formula(paste(exposure_var, "~", paste(confounders, collapse = "+")))
        
        # Fit multinomial model
        iptw_model <- tryCatch({
          nnet::multinom(model_formula, data = df, trace = FALSE)
        }, error = function(e) {
          stop("Multinomial model failed: ", e$message)
        })
        
        # Get predicted probabilities for each level
        predicted_probs <- predict(iptw_model, type = "probs", newdata = df)
        
        # If there are only 3 levels, predicted_probs might be a matrix
        if (!is.matrix(predicted_probs)) {
          predicted_probs <- matrix(predicted_probs, ncol = length(levels(df[[exposure_var]])))
        }
        
        # Calculate conditional densities
        cond_density <- numeric(nrow(df))
        for (i in 1:nrow(df)) {
          # Get the probability for the actual level
          actual_level <- df[[exposure_var]][i]
          level_index <- which(levels(df[[exposure_var]]) == actual_level)
          cond_density[i] <- predicted_probs[i, level_index]
        }
        
        # Calculate marginal probabilities (prevalence of each level)
        level_props <- prop.table(table(df[[exposure_var]]))
        
        # Calculate marginal densities
        marg_density <- numeric(nrow(df))
        for (i in 1:nrow(df)) {
          # Get the probability for the actual level
          actual_level <- df[[exposure_var]][i]
          marg_density[i] <- level_props[actual_level]
        }
        
        # Store model metrics if AIC/BIC are available for multinom
        model_fit_metrics <- list(
          single_stage_multinomial = list(
            AIC = AIC(iptw_model),
            BIC = BIC(iptw_model)
          )
        )
      }
    }
  } else {
    # --- TWO STAGE MODEL APPROACH ---
    # --- 1. Binary Part (Zero vs. Positive) ---
    if (verbose) cat("\n=== Fitting Binary Model ===\n")
    
    binary_exposure <- as.numeric(df[[exposure_var]] > 0)
    
    logit_model <- tryCatch(
      CBPS(binary_exposure ~ ., data = df[confounders], family = binomial, method = "exact"),
      error = function(e) {
        warning("Exact CBPS failed, trying over-parameterized method")
        CBPS(binary_exposure ~ ., data = df[confounders], family = binomial, method = "over")
      }
    )
    
    prob_positive <- logit_model$fitted.values
    
    # Positivity check
    extreme_ps <- sum(prob_positive < positivity_threshold[1] | prob_positive > positivity_threshold[2])
    if (extreme_ps > 0 && verbose) {
      cat("Positivity issues in binary model:", extreme_ps, "extreme propensity scores\n")
      if (verbose) hist(prob_positive, main = "Binary Propensity Scores")
    }
    
    # --- 2. Second Stage Model (Positive Values) ---
    pos_data <- df[df[[exposure_var]] > 0, ]
    if (nrow(pos_data) == 0) stop("No positive exposure values found")
    
    if (verbose) cat(paste("\n=== Fitting", toupper(second_stage_model), "Model ===\n"))
    
    # Formula for second stage
    second_stage_formula <- as.formula(paste(exposure_var, "~", paste(confounders, collapse = "+")))
    
    # Fit different models based on the specified distribution
    model_fit_metrics <- list()
    
    if (second_stage_model == "gaussian") {
      # Gaussian model
      gaussian_model <- tryCatch(
        CBPS(second_stage_formula, data = pos_data, family = gaussian, method = "exact"),
        error = function(e) {
          warning("Exact CBPS failed for Gaussian model, trying over-parameterized method")
          CBPS(second_stage_formula, data = pos_data, family = gaussian, method = "over")
        }
      )
      
      # Store model for metrics
      second_stage_glm <- glm(second_stage_formula, data = pos_data, family = gaussian)
      model_fit_metrics[["gaussian"]] <- list(
        AIC = AIC(second_stage_glm),
        BIC = BIC(second_stage_glm),
        logLik = logLik(second_stage_glm)
      )
      
      # Get fitted values for density calculation
      fitted_values <- gaussian_model$fitted.values
      fitted_sd <- sqrt(mean((pos_data[[exposure_var]] - fitted_values)^2))
      
      # Density calculation function
      density_func <- function(x, mean_val, sd_val) {
        dnorm(x, mean = mean_val, sd = sd_val)
      }
      
      # Marginal parameters
      marg_mean <- mean(pos_data[[exposure_var]])
      marg_sd <- sd(pos_data[[exposure_var]])
      
    } else if (second_stage_model == "poisson") {
      # Try CBPS with Poisson
      poisson_model <- tryCatch(
        CBPS(second_stage_formula, data = pos_data, family = poisson, method = "exact"),
        error = function(e) {
          warning("Exact CBPS failed for Poisson model, trying over-parameterized method")
          tryCatch(
            CBPS(second_stage_formula, data = pos_data, family = poisson, method = "over"),
            error = function(e2) {
              warning("CBPS failed completely for Poisson, using standard glm")
              glm(second_stage_formula, data = pos_data, family = poisson)
            }
          )
        }
      )
      
      # Store model for metrics
      second_stage_glm <- glm(second_stage_formula, data = pos_data, family = poisson)
      model_fit_metrics[["poisson"]] <- list(
        AIC = AIC(second_stage_glm),
        BIC = BIC(second_stage_glm),
        logLik = logLik(second_stage_glm)
      )
      
      # Get fitted values for density calculation
      fitted_values <- tryCatch({
        poisson_model$fitted.values
      }, error = function(e) {
        warning("Could not extract fitted.values, using glm predictions as fallback")
        predict(second_stage_glm, type = "response")
      })
      
      # Density calculation function
      density_func <- function(x, lambda, dummy) {
        dpois(x, lambda = lambda)
      }
      
      # Marginal parameter
      marg_mean <- mean(pos_data[[exposure_var]])
      marg_sd <- NULL  # Not used for Poisson
      
    } else if (second_stage_model == "negbin") {
      # Negative Binomial model
      negbin_model <- tryCatch({
        glm.nb(second_stage_formula, data = pos_data)
      }, error = function(e) {
        warning("glm.nb failed, using standard Poisson as fallback")
        glm(second_stage_formula, data = pos_data, family = poisson)
      })
      
      # Store model for metrics
      model_fit_metrics[["negbin"]] <- list(
        AIC = AIC(negbin_model),
        BIC = BIC(negbin_model),
        logLik = logLik(negbin_model)
      )
      
      # Get fitted values and theta for density calculation
      fitted_values <- predict(negbin_model, type = "response")
      theta <- tryCatch({
        negbin_model$theta
      }, error = function(e) {
        warning("Theta extraction failed, using estimated value")
        # Estimate theta using method of moments if extraction fails
        mu <- mean(pos_data[[exposure_var]])
        var <- var(pos_data[[exposure_var]])
        # If var > mu, estimate theta, otherwise default to 1
        ifelse(var > mu, mu^2 / (var - mu), 1)
      })
      
      # Density calculation function
      density_func <- function(x, mu, theta) {
        dnbinom(x, mu = mu, size = theta)
      }
      
      # Marginal parameters (mean and theta from the data)
      marg_mean <- mean(pos_data[[exposure_var]])
      obs_var <- var(pos_data[[exposure_var]])
      marg_theta <- ifelse(obs_var > marg_mean,
                         marg_mean^2 / (obs_var - marg_mean), 
                         theta)  # Default to model theta if variance ≤ mean
    }
    
    # --- 3. Weight Calculation for Two-Stage Model ---
    # Marginal probability of being positive
    marg_prob_pos <- mean(binary_exposure)
    
    # Initialize weight vectors
    cond_density <- rep(NA, nrow(df))
    marg_density <- rep(NA, nrow(df))
    
    # Calculate conditional density based on model type
    cond_density[df[[exposure_var]] == 0] <- 1 - prob_positive[df[[exposure_var]] == 0]
    
    # Calculate marginal density for zeros
    marg_density[df[[exposure_var]] == 0] <- 1 - marg_prob_pos
    
    # Indices for positive values
    pos_indices <- which(df[[exposure_var]] > 0)
    
    # Fit indices (mapping from positive data to original data)
    fit_indices <- match(pos_indices, which(df[[exposure_var]] > 0))
    
    # Calculate densities for positive values based on model type
    if (second_stage_model == "gaussian") {
      cond_density[pos_indices] <- prob_positive[pos_indices] * 
        density_func(df[[exposure_var]][pos_indices], 
                    fitted_values[fit_indices], 
                    fitted_sd)
      
      marg_density[pos_indices] <- marg_prob_pos * 
        density_func(df[[exposure_var]][pos_indices], 
                    marg_mean, 
                    marg_sd)
      
    } else if (second_stage_model == "poisson") {
      cond_density[pos_indices] <- prob_positive[pos_indices] * 
        density_func(df[[exposure_var]][pos_indices], 
                    fitted_values[fit_indices], 
                    NULL)
      
      marg_density[pos_indices] <- marg_prob_pos * 
        density_func(df[[exposure_var]][pos_indices], 
                    marg_mean, 
                    NULL)
      
    } else if (second_stage_model == "negbin") {
      cond_density[pos_indices] <- prob_positive[pos_indices] * 
        density_func(df[[exposure_var]][pos_indices], 
                    fitted_values[fit_indices], 
                    theta)
      
      marg_density[pos_indices] <- marg_prob_pos * 
        density_func(df[[exposure_var]][pos_indices], 
                    marg_mean, 
                    marg_theta)
    }
  }
  
  # Ensure no zeros in conditional density (prevents infinite weights)
  if (any(cond_density <= 0, na.rm = TRUE)) {
    warning(paste0("Zero conditional densities found: ", sum(cond_density <= 0, na.rm = TRUE),
                  ". Replacing with small value."))
    cond_density[cond_density <= 0] <- min(cond_density[cond_density > 0]) / 10
  }
  
  # Calculate weights - same for both approaches
  df$iptw_weights <- marg_density / cond_density
  
  # --- 4. Trimming ---
  if (verbose) cat("\n=== Trimming Weights ===\n")
  
  upper_quant <- ifelse(symmetric_trim, trim_quantile, trim_quantile_upper)
  lower_quant <- 1 - trim_quantile
  
  if (lower_quant >= upper_quant) {
    stop("Invalid trimming quantiles. Lower must be less than upper.")
  }
  
  trim_limits <- quantile(df$iptw_weights, c(lower_quant, upper_quant), na.rm = TRUE)
  df$iptw_weights_trimmed <- pmin(pmax(df$iptw_weights, trim_limits[1]), trim_limits[2])
  
  # --- 5. Diagnostics ---
  if (verbose) {
    cat("\n=== Weight Diagnostics ===\n")
    cat("Unweighted sample size:", nrow(df), "\n")
    cat("Weighted ESS:", round(sum(df$iptw_weights_trimmed)^2 / sum(df$iptw_weights_trimmed^2)), "\n")
    
    cat("\nWeight Summary (Before Trimming):\n")
    print(summary(df$iptw_weights))
    
    cat("\nWeight Summary (After Trimming):\n")
    print(summary(df$iptw_weights_trimmed))
    
    cat("\nModel Fit Metrics:\n")
    if (single_stage) {
      if ("single_stage_gaussian" %in% names(model_fit_metrics)) {
        cat("Single-stage continuous model - AIC:", round(model_fit_metrics[["single_stage_gaussian"]]$AIC, 2), 
            "BIC:", round(model_fit_metrics[["single_stage_gaussian"]]$BIC, 2), "\n")
      }
      if ("single_stage_binary" %in% names(model_fit_metrics)) {
        cat("Single-stage binary model - AIC:", round(model_fit_metrics[["single_stage_binary"]]$AIC, 2), 
            "BIC:", round(model_fit_metrics[["single_stage_binary"]]$BIC, 2), "\n")
      }
      if ("single_stage_multinomial" %in% names(model_fit_metrics)) {
        cat("Single-stage multinomial model - AIC:", round(model_fit_metrics[["single_stage_multinomial"]]$AIC, 2), 
            "BIC:", round(model_fit_metrics[["single_stage_multinomial"]]$BIC, 2), "\n")
      }
    } else {
      if ("gaussian" %in% names(model_fit_metrics)) {
        cat("Gaussian model - AIC:", round(model_fit_metrics[["gaussian"]]$AIC, 2), 
            "BIC:", round(model_fit_metrics[["gaussian"]]$BIC, 2), "\n")
      }
      if ("poisson" %in% names(model_fit_metrics)) {
        cat("Poisson model - AIC:", round(model_fit_metrics[["poisson"]]$AIC, 2), 
            "BIC:", round(model_fit_metrics[["poisson"]]$BIC, 2), "\n")
      }
      if ("negbin" %in% names(model_fit_metrics)) {
        cat("Negative binomial model - AIC:", round(model_fit_metrics[["negbin"]]$AIC, 2), 
            "BIC:", round(model_fit_metrics[["negbin"]]$BIC, 2), "\n")
      }
    }
    
    # Balance plots - handle differently for binary vs continuous exposure
    cat("\n=== Covariate Balance Assessment ===\n")
    
    # For continuous exposure, need to create bins for balance checking
    if (!single_stage) {
      # For two-stage model, use the binary part for balance
      treat_var <- as.numeric(df[[exposure_var]] > 0)
      bal <- bal.tab(df[confounders], treat = treat_var, 
                    weights = df$iptw_weights_trimmed, method = "weighting")
      print(bal)
      love.plot(bal, threshold = 0.1, title = "Covariate Balance")
    } else {
      # For single stage, handle differently based on type
      if (is.factor(df[[exposure_var]]) || length(unique(df[[exposure_var]])) <= 10) {
        # For categorical or discrete with few levels
        bal <- bal.tab(df[confounders], treat = df[[exposure_var]], 
                      weights = df$iptw_weights_trimmed, method = "weighting")
        print(bal)
        love.plot(bal, threshold = 0.1, title = "Covariate Balance")
      } else {
        # For continuous exposure, create quantiles for balance check
        # Fix: Check for duplicate break values and use unique breaks
        quant_breaks <- quantile(df[[exposure_var]], probs = seq(0, 1, 0.25))
        
        # Check if breaks are unique
        if (length(unique(quant_breaks)) < length(quant_breaks)) {
          # If not unique, try to create unique breaks
          if (length(unique(df[[exposure_var]])) <= 4) {
            # If very few unique values, just use the unique values
            exposure_bins <- factor(df[[exposure_var]])
          } else {
            # Try to create bins with equal counts instead
            n_bins <- 4  # Same as using quartiles
            exposure_bins <- cut(rank(df[[exposure_var]]), 
                               breaks = seq(0, nrow(df), length.out = n_bins + 1),
                               include.lowest = TRUE, labels = paste0("Q", 1:n_bins))
          }
        } else {
          # Original code if breaks are unique
          exposure_bins <- cut(df[[exposure_var]], breaks = quant_breaks, 
                            include.lowest = TRUE)
        }
        
        bal <- bal.tab(df[confounders], treat = exposure_bins, 
                      weights = df$iptw_weights_trimmed, method = "weighting")
        print(bal)
        love.plot(bal, threshold = 0.1, title = "Covariate Balance")
      }
    }
    
    # Weight distributions
    par(mfrow = c(1, 2))
    hist(df$iptw_weights, main = "Raw Weights", xlab = "Weights")
    hist(df$iptw_weights_trimmed, main = "Trimmed Weights", xlab = "Weights")
    par(mfrow = c(1, 1))
  }
  
  # Add model fit metrics to return
  attr(df, "model_fit_metrics") <- model_fit_metrics
  attr(df, "model_used") <- ifelse(single_stage, "single_stage", second_stage_model)
  
  return(df)
}


# Example usage:
# all_data <- readRDS(here(path_processed, "1.3.3_cumulative_bins_all_datasets.rds"))
# df <- all_data$tmax_wbgt
# confounders <- c("mat_edu_level", "mat_age_at_int_cat", "mat_parity_bi",
#                  "mat_media_exp_any", "ses_caste_2", "ses_religion_2_hindu",
#                  "ses_access_issue_distance", "hh_wealth_quintile_ru_og", 
#                  "meta_rural")

# # # Single stage IPTW
# df_single <- create_iptw_weights(
#   df, 
#   "days_compu_bins_cumulative_10", 
#   confounders = confounders,
#   second_stage_model = NULL,
#   trim_quantile = 0.99,
#   verbose = TRUE
# )

# # # Two stage models
# df_two_stage_gaussian <- create_iptw_weights(
#   df, 
#   "days_compu_bins_cumulative_10", 
#   confounders = confounders,
#   second_stage_model = "gaussian",
#   trim_quantile = 0.99,
#   verbose = TRUE
# )

# df_two_stage_negbin <- create_iptw_weights(
#   df, 
#   "days_compu_bins_cumulative_10", 
#   confounders = confounders,
#   second_stage_model = "negbin",
#   trim_quantile = 0.99,
#   verbose = TRUE
# )


# # Compare models
# cat("Single Stage AIC:", attr(df_single, "model_fit_metrics")$single_stage_gaussian$AIC, "\n")
# cat("Gaussian Two-Stage AIC:", attr(df_two_stage_gaussian, "model_fit_metrics")$gaussian$AIC, "\n")
# cat("Negative Binomial Two-Stage AIC:", attr(df_negbin, "model_fit_metrics")$negbin$AIC, "\n") 
# cat("Poisson Two-Stage AIC:", attr(df_poisson, "model_fit_metrics")$poisson$AIC, "\n")
