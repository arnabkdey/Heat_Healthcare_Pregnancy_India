library(googledrive)
library(here)

# Function to upload files ----------------------------------------------------
func_download <- function(folder_id, path_dest_folder) {
    ## List all files in the folder
    file_list <- googledrive::drive_ls(as_id(folder_id))
    ## Download all files to the local folder
    for (i in seq_len(nrow(file_list))) {
        cur_file <- file_list[i, ]
    googledrive::drive_download(as_id(cur_file$id), path = here(path_dest_folder, cur_file$name), overwrite = TRUE)
    }
}

# Upload all processed data to Google Drive -----------------------------------
## Get ID of the google drive folder to download from
folder_id_proc <- "1anDrAusoWSW9uNHoTeAEo5FYvSZizAbZ"

## Specify the destination folder
path_dest_proc <- here("2-data", "2.2-processed-data")

## Run the function to download all files
func_download(folder_id_proc, path_dest_proc)

