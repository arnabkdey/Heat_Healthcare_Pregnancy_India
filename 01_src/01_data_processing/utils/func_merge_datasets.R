#' Prepare datasets for spline analysis
#' @param df_IR_6mo DHS IR dataset
#' @param temp_files List of temperature files
#' @param path_processed Path to processed data
#' @return List of processed datasets
merge_datasets <- function(df_IR_6mo, temp_files, path_processed) {
  # Load and merge temperature data
  datasets <- lapply(names(temp_files), function(name) {
    df_temp <- fst::read_fst(here(path_processed, temp_files[[name]]), 
                            as.data.table = TRUE)
    
    df_merged <- df_IR_6mo |> 
      inner_join(df_temp, by = c("caseid", "psu", "doi"))
    
    # Add dataset name as attribute
    attr(df_merged, "dataset_name") <- name
    return(df_merged)
  })
  
  names(datasets) <- names(temp_files)
  return(datasets)
} 
