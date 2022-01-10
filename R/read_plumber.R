#' Reads plumber2 netcdf data
#'
#' Reads data in a given directory
#' by (fluxnet) site name, optionally
#' only returns the meta-data of the
#' site data
#'
#' @param site fluxnet site name
#' @param path path with plumber2 data (both flux and meteo data files)
#'
#' @return data frame with merged meteo and flux data
#' @export

read_plumber <- function(
  site = "AT-Neu",
  path,
  meta_data = FALSE
){
  
  # list all files
  files <- list.files(
    path,
    utils::glob2rx(paste0(site,"*.nc")),
    full.names = TRUE,
    recursive = TRUE
  )
  
  # check if both files are there
  if (length(files) != 2){
    stop("Missing either flux or meteo data for the requested site")
  }
  
  # cut back on read times by only reading
  # flux data when extracting meta-data
  if (meta_data) {
    files <- files[grepl(glob2rx("*_Flux.nc"), files)]
  }
  
  df <- lapply(files, function(file){
    # convert time (needs attribute read)
    nc <- ncdf4::nc_open(file)
    
    # Get time vector
    time <- ncdf4::ncvar_get(nc, "time")
    
    # Get time units
    time_units <- strsplit(
      ncdf4::ncatt_get(nc, "time")$units,
      "seconds since ")[[1]][2]
    
    # Convert to Y-M-D h-m-s (hack around attribute issues in dates
    # when using bind_cols() or joins)
    time_date <- as.character(as.POSIXct(time, origin = time_units, tz="GMT"))
    time_date <- as.POSIXct(time_date, tz = "GMT")
    
    # Get variable names
    vars <- names(nc$var)
    
    # Load variable data
    df <- as.data.frame(
      lapply(vars, function(x) ncdf4::ncvar_get(nc, x))
    )
    
    # close file
    ncdf4::nc_close(nc)
    
    # Set names
    colnames(df) <- vars
    
    # add time column
    df$time <- time_date
    
    # remove trailing / leading white spaces
    # in IGBP classes
    df$IGBP_veg_short <- trimws(df$IGBP_veg_short)
    
    # drop long names
    df <- subset(df, select = -IGBP_veg_long)
    
    # subset and constrain data
    if (meta_data) {
      
      df$year_start <- format(min(df$time),"%Y")
      df$year_end <- format(max(df$time),"%Y")
      
      df <- df[1,c("latitude", "longitude", "reference_height",
                   "canopy_height", "elevation", "IGBP_veg_short",
                   "year_start","year_end")]
      df$sitename <- site
    }
    
    # return data frame
    return(df)
  })
  
  # only return meta-data if requested
  # don't merge with met data
  if (!meta_data) {
    all <- df
  } else {
    # combine met and flux data
    all <- left_join(df[[1]], df[[2]])
  }
  
  # return the merged file
  return(all)
}

