# Library ----
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, googledrive, here)
library(performance)
library(merDeriv)
rm(list = ls())

# Create a folder for the outputs ----
path_out <- here("3-outputs", "models", "models-no-interaction")
if (!dir.exists(path_out)) {
  # Create the directory if it does not exist
  dir.create(path_out, showWarnings = TRUE, recursive = TRUE)
}

# Load models ----
path_processed <- here("2-data", "2.2-processed-data")
model_outputs <- readRDS(here(path_processed, "2.1a-models-no-interaction-comp-t1.rds"))
print("finished loading models")
length(model_outputs)

# Function for calculating CIs
coefcis <- function(coef, se, exponentiate = TRUE) {
  lci = coef - 1.96*se
  uci = coef + 1.96*se
  res <- cbind(coef, lci, uci)
  res_exp = exp(res)
  if (exponentiate == TRUE) {
    res_full <- exp(res)
  } else {
    res_full <- res
  }
  return(res_full)
}

# Step-1: Extract Tidy Outputs ----
## Clean up the names of the model outputs
names(model_outputs) <- gsub("_sum|any_|sum_|_scale_iqr", "", names(model_outputs))

## Initialize an empty list to store tidy outputs
tidy_outputs <- list()

## Iterate over model_outputs to generate tidy outputs using broom.mixed::tidy()
for(exposure in names(model_outputs)) {
  model <- model_outputs[[exposure]]
  m_vcov <- vcov.glmerMod(model)
  m_se <- sqrt(diag(m_vcov))
  m_coef <- model@beta
  tidy_outputs[[exposure]] <- coefcis(m_coef, m_se, exponentiate = T) |> as.data.frame() |> rownames_to_column()
  print(paste0("finished processing", exposure))
}

# convert the firs column 

## Save Tidy Outputs to separate workbooks ----- 
### Create a new workbook
wb <- createWorkbook()
### Iterate over the tidy_outputs to add each to a new sheet in the workbook
for(exposure in names(tidy_outputs)) {
  output <- tidy_outputs[[exposure]] |> as.data.frame() |> column_to_rownames()
  # Create a new sheet with the name of the exposure
  addWorksheet(wb, exposure)
  # Write the tidy output to the sheet
  writeData(wb, exposure, tidy_outputs[[exposure]])
  print(paste0("finished writing", exposure))
}

## Write Step-1 output to a file
saveWorkbook(wb, here(path_out, "models_full_comp_t1.xlsx"), overwrite = TRUE)

# Step-2: Consolidate coefficients for the primary exposure  in a single CSV ----
## Initialize an empty dataframe to store the estimates for the exposure
combined_exposures <- data.frame(a = integer(), b = integer())

## Loop through each model in the list

for(model_name in names(tidy_outputs)) {
  # Extract the model from the list
  model <- tidy_outputs[[model_name]]
  dep_var <- str_extract(model_name, "^[^-]+")
  # Extract the 2nd row and the 3rd, 4th, 8th and 9th column from the current model
  exposure_name <- model[2, 1]  
  estimates <- round(model[2, c(2,3,4)], 2)
  
  # Combine the exposure and the estimates
  second_row <- cbind(dep_var, exposure_name, estimates)

  # Append the extracted row to the combined dataframe
  combined_exposures <- rbind(combined_exposures, second_row)
}

head(combined_exposures)
## Save Step-2  output to a CSV ----
write.csv(combined_exposures, here(path_out, "models_consolidated_coefficients_comp_t1.csv"), row.names = FALSE)

