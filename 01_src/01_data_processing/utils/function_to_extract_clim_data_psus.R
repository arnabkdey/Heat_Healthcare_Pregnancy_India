#' Merge DHS Survey Data with Climate Variables from NetCDF Files
#'
#' @description
#' Extracts climate data from NetCDF files for specific geographic locations (PSUs) 
#' from Demographic and Health Surveys (DHS). The function processes multiple NetCDF files,
#' extracts values at survey locations, and creates a long-format dataset with dates and climate variables.
#'
#' @param path Character string specifying the directory path containing NetCDF (.nc) files.
#' @param region_boundary An sf or SpatVector object defining the geographic boundary to crop/mask climate data.
#'                       Default is 'india_boundary_buf'.
#' @param dhs_lat_long_sf An sf object containing DHS survey cluster (PSU) locations with coordinates.
#'                       Default is 'df_dhs_psu_geo_sf'.
#' @param clim_var Character string naming the climate variable being extracted (used for column naming).
#'                This parameter is required with no default value.
#' @param from_index Integer specifying the starting index of files to process. Default is 1.
#' @param to_index Integer specifying the ending index of files to process. If NULL, all files from from_index
#'                to the end will be processed. Default is NULL.
#'
#' @return A data.table in long format containing:
#' \describe{
#'   \item{psu}{Survey primary sampling unit identifier}
#'   \item{lat}{Latitude of the PSU}
#'   \item{long}{Longitude of the PSU}
#'   \item{dist_name}{District name}
#'   \item{variable}{Time period identifier from NetCDF files}
#'   \item{clim_var}{The climate variable values (using the name provided in the clim_var parameter)}
#'   \item{date}{Date corresponding to each observation}
#' }
#'
#' @details
#' This function performs the following operations:
#' 1. Reads NetCDF files from the specified directory
#' 2. For each file:
#'    - Crops and masks the raster data to the region boundary
#'    - Extracts climate values at each PSU location
#'    - Determines the date range from the NetCDF metadata
#'    - Creates a long-format dataset with dates and climate values
#' 3. Combines all processed files into a single data.table
#'
#' The function uses the terra package for spatial operations which is more efficient
#' than the older raster package for large datasets.
#'
#' @note
#' - Requires NetCDF files with proper time dimension and attributes
#' - Supports various time units (days, hours, minutes, seconds)
#' - Coordinate reference systems (CRS) should be compatible between region_boundary and input files
#' - Memory usage can be high when processing multiple large NetCDF files
#' - Checks for either 'time' or 'valid_time' variables in NetCDF files
#'
#' @importFrom terra rast crop mask extract crs crs<-
#' @importFrom sf st_as_sf
#' @importFrom data.table data.table melt rbindlist
#' @importFrom ncdf4 nc_open nc_close ncvar_get ncatt_get
#' @importFrom lubridate dseconds dminutes dhours
#'
#' @author Arnab K. Dey
#' @date March 2025

merge_dhs_climate <- function(path, region_boundary = india_boundary_buf,
                              dhs_lat_long_sf = df_dhs_psu_geo_sf,
                              clim_var, from_index = 1, to_index = NULL) {
    require(terra) # Use terra instead of raster
    require(data.table)
    require(sf)

    # Check if clim_var is specified
    if (missing(clim_var)) {
        stop("clim_var must be specified")
    }
    
    # Get the list of files
    file_list <- list.files(path = path, pattern = "\\.nc")
    
    # Validate and adjust file index range
    if (from_index < 1 || from_index > length(file_list)) {
        stop("from_index is out of range")
    }
    
    if (is.null(to_index)) {
        to_index <- length(file_list)
    } else if (to_index < from_index || to_index > length(file_list)) {
        stop("to_index is out of range or less than from_index")
    }
    
    # Select files within the specified range
    file_list <- file_list[from_index:to_index]
    
    # Initialize empty list for data frames
    df_list <- list()
    
    # Run a loop
    for (i in seq_along(file_list)) {
        # Step-1: Read the raster file and crop/mask it to the region boundary
        ## Read the files as raster (SpatRaster in terra)
        rd0 <- terra::rast(paste0(path, "/", file_list[i]))
        ## Restrict the spatial data to the region boundary
        cd1 <- crop(rd0, region_boundary)
        ## Set the crs to the region boundary
        crs(cd1) <- crs(region_boundary)
        # summary(cd1)
        ## Mask the data to the region boundary
        cd2 <- mask(cd1, region_boundary)
        # summary(cd2)

        # Step-2: Extract the climate data for each PSU location and process it
        ## Extract the climate data for each PSU location
        df1 <- extract(cd2, dhs_lat_long_sf, df = TRUE, na.rm = TRUE)
        ## Remove the first column ID
        df1 <- df1[, -1]
        ## Add the PSU information from sf object, need to convert sf to data frame first
        dhs_data <- as.data.frame(dhs_lat_long_sf)
        ## Combine the extracted data with the PSU data
        df1 <- data.table(cbind(dhs_data, df1))
        ## Remove column geometry
        df1 <- df1[, geometry := NULL]
        ## Reshape the data frame from wide to long
        df2 <- melt(df1, id.vars = c("psu", "lat", "long", "dist_name"), value.name = clim_var)
        
        # Step-3: Extract dates
        ## Get start and end dates from the nc file
        ### Open the NetCDF file
        nc_data <- nc_open(paste0(path, "/", file_list[i]))
        ### Check for time variable (either 'time' or 'valid_time')
        if ("time" %in% names(nc_data$dim)) {
            time_var_name <- "time"
        } else if ("valid_time" %in% names(nc_data$dim)) {
            time_var_name <- "valid_time"
        } else {
            stop("Neither 'time' nor 'valid_time' variable found in NetCDF file")
        }
        
        ### Get the time variable
        time_var <- ncvar_get(nc_data, time_var_name)
        time_units <- ncatt_get(nc_data, time_var_name, "units")$value
        
        ### Extract the reference date from the time units
        ref_date_str <- sub(".*since ", "", time_units)
        ### Determine the time unit (days, hours, etc.)
        unit <- strsplit(time_units, " ")[[1]][1]
        ### Convert the reference date string to a Date or POSIXct object
        if (grepl("days", time_units)) {
            ref_date <- as.Date(ref_date_str)
            actual_dates <- ref_date + as.numeric(time_var) # Add time_var as days
        } else if (grepl("seconds", time_units)) {
            ref_date <- as.POSIXct(ref_date_str, tz = "UTC")
            actual_dates <- ref_date + dseconds(as.numeric(time_var)) # Add time_var as seconds
        } else if (grepl("minutes", time_units)) {
            ref_date <- as.POSIXct(ref_date_str, tz = "UTC")
            actual_dates <- ref_date + dminutes(as.numeric(time_var)) # Add time_var as minutes
        } else if (grepl("hours", time_units)) {
            ref_date <- as.POSIXct(ref_date_str, tz = "UTC")
            actual_dates <- ref_date + dhours(as.numeric(time_var)) # Add time_var as hours
        } else {
            stop("Time units not recognized or supported")
        }
        
        ### Get the start and end dates
        start_date <- min(actual_dates)
        end_date <- max(actual_dates)
        ### Close the NetCDF file when done
        nc_close(nc_data)
        
        ## Create a sequence of dates
        dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")
        dates_full <- rep(dates, each = nrow(df1))
        ## Add the date column
        df2$date <- dates_full

        # Step-4: Add the data frame to the list
        df_list[[i]] <- df2
        print(paste("finished processing", i, "of", length(file_list), 
                    "(file", from_index + i - 1, "overall)"))
    }
    
    df_out <- rbindlist(df_list)
    return(df_out)
}