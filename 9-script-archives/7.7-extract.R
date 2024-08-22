rm(list = setdiff(ls(), c("path_processed_data", "path_raw_data")))
pacman::p_load(tidyverse, data.table, janitor, fst, openxlsx, googledrive, here, beepr, Hmisc)
library(performance)

# Save all objects starting with "model" as an RDS file ----
results <- readRDS(here(path_processed_data, "7.5-models.rds"))

# Create a folder for the outputs ----
path_out <- here("3-outputs", "models", "models-anna")
if (!dir.exists(path_out)) {
  # Create the directory if it does not exist
  dir.create(path_out, showWarnings = TRUE, recursive = TRUE)
}

# Toolbox
names(results)
model <- results[["comp_excessive_bi"]][[1]]
View(broom::tidy(model, exp = TRUE))
class(model)
names(model)
model$formula

# Step-1: Extract Tidy Outputs ----
## Create vector for trimesters ----
trimesters <- c("t1", "t2", "t3", "full")

## Initialize an empty list to store tidy outputs
tidy_outputs <- list()

## Iterate over results to generate tidy outputs using broom.mixed::tidy()
for(exposure in names(results)) {
    for (trim in seq_along(trimesters)) {
        model <- results[[exposure]][[trim]]
        br <- broom.mixed::tidy(model, exponentiate = TRUE, conf.int = TRUE)
        tidy_outputs[[paste0(exposure, "_", trimesters[trim])]] <- br
        print(paste0("finished processing", trim))
    }
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
saveWorkbook(wb, here(path_out, "models_full_comp_age.xlsx"), overwrite = TRUE)
