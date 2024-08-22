parallel_quantile_regression <- function(df, formula, variable_name, quantile) {
  
  vec_psu <- unique(df$psu)

  # Ensure df is a data.table
  setDT(df)

  # Set up parallel backend to use multiple processors
  n_cores <- detectCores() - 5
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)

  # Parallel processing
  results <- foreach(cur_psu = vec_psu, .combine = 'rbind', 
                      .packages = c('data.table', 'quantreg')) %dopar% {
      # Ensure df is a data.table within each worker
      setDT(df)
      
      # Efficiently subset the data for the current PSU
      df_subset <- df[psu == cur_psu]

      # Fit the model
      model_quantreg <- quantreg::rq(data = df_subset,
                                        formula = formula,
                                        tau = quantile)
          
      # Generate the column name for the cutoff
      # cutoff_column_name <- paste0(variable_name, formatC(quantile, format = "f", digits = 2))
      cutoff_column_name <- paste0(variable_name, as.integer(quantile * 100))
          
      # Assign fitted values back to the subset
      df_subset[, (cutoff_column_name) := model_quantreg$fitted.values]
      
      
      # Return the modified subset
      return(df_subset)
  }

  # Stop the parallel cluster
  stopCluster(cl)

  # Merge the results back into the main data table
  setDT(results)
  df <- merge(df, results, all.x = TRUE, by = intersect(names(df), names(results)))

  return(df)
}
