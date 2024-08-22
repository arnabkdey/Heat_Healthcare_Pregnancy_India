merge_dhs_climate <- function(path, region_boundary = india_boundary_buf, 
                              dhs_lat_long_sf = df_dhs_psu_geo_sf,
                              clim_var = "tmax_wb") {
    require(terra)  # Use terra instead of raster
    require(data.table)
    require(sf)
    
    # Get the list of files
    a <- list.files(path = path, pattern ="\\.nc")
    # Initialize empty list for data frames
    df_list <- list()
    # Run a loop 
    for (i in seq_along(a)) {
        
        # Step-1: Read the raster file and crop/mask it to the region boundary
        ## Read the files as raster (SpatRaster in terra)
        rd0 <- terra::rast(paste0(path, "/", a[i]))
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
        df1 <- extract(cd2, dhs_lat_long_sf, df=TRUE, na.rm=TRUE)
        ## Remove the first column ID
        df1 <- df1[,-1]        
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
        nc_data <- nc_open(paste0(path, "/", a[i]))
        ### Get the time variable
        time_var <- ncvar_get(nc_data, "time")
        time_units <- ncatt_get(nc_data, "time", "units")$value
        ### Extract the reference date from the time units
        ref_date_str <- sub(".*since ", "", time_units)
        ### Determine the time unit (days, hours, etc.)
        unit <- strsplit(time_units, " ")[[1]][1]
        ### Convert the reference date string to a Date or POSIXct object
        if(grepl("days", time_units)) {
            ref_date <- as.Date(ref_date_str)
            actual_dates <- ref_date + as.numeric(time_var)  # Add time_var as days
            } else if(grepl("seconds", time_units)) {
            ref_date <- as.POSIXct(ref_date_str, tz = "UTC")
            actual_dates <- ref_date + dseconds(as.numeric(time_var))  # Add time_var as seconds
            } else if(grepl("minutes", time_units)) {
            ref_date <- as.POSIXct(ref_date_str, tz = "UTC")
            actual_dates <- ref_date + dminutes(as.numeric(time_var))  # Add time_var as minutes
            } else if(grepl("hours", time_units)) {
            ref_date <- as.POSIXct(ref_date_str, tz = "UTC")
            actual_dates <- ref_date + dhours(as.numeric(time_var))  # Add time_var as hours
            } else {
            stop("Time units not recognized or supported")
        }
        ### Get the start and end dates
        start_date <- min(actual_dates) 
        end_date <- max(actual_dates)
        ### Close the NetCDF file when done
        nc_close(nc_data)
        ## Create a sequence of dates
        dates <- seq(as.Date(start_date), as.Date(end_date), by="day")
        dates_full <- rep(dates, each = nrow(df1))        
        ## Add the date column
        df2$date <- dates_full
        
        # Step-4: Add the data frame to the list
        df_list[[i]] <- df2
        print(paste("finished processing", i))
    }
    df_out <- rbindlist(df_list)
    return(df_out)
}
