library(googledrive)
library(here)

# Function to upload files ----------------------------------------------------
func_upload <- function(file_list, folder_id) {
  for (file in file_list) {
    ## Get the file name
    file_name <- basename(file)
    
    ## Upload the file
    drive_upload(media = file, 
                 name = file_name, 
                 path = as_id(folder_id),
                 overwrite = TRUE)
  }
}

# Upload all processed data to Google Drive -----------------------------------
## Get ID of the folder to upload to
folder_id_proc <- "1anDrAusoWSW9uNHoTeAEo5FYvSZizAbZ"

## List all processed files
file_list_proc <- list.files(here("2-data", "2.2-processed-data"), full.names = TRUE)

## Run the function to upload all files
func_upload(file_list_proc, folder_id_proc)

