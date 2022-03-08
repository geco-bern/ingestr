#' Reads plumber2 netcdf data
#'
#' Reads data in a given directory
#' by (fluxnet) site name, optionally
#' only returns the meta-data of the
#' site data
#'
#' @param site fluxnet site name
#' @param path path with plumber2 data (both flux and meteo data files)
#' @param fluxnet_format convert to fluxnet formatting (TRUE/FALSE)
#' @param meta_data return meta-data TRUE/FALSE
#' @param out_path where to store the converted data if converted to
#'  fluxnet formatting
#'
#' @return data frame with merged meteo and flux data
#' @export

read_plumber <- function(
  site = "AT-Neu",
  path,
  fluxnet_format = FALSE,
  meta_data = FALSE,
  out_path
){
  
  # CRAN settings
  IGBP_veg_long <- time <- TIMESTAMP_START <-
    TIMESTAMP_END <- P <- TA_F <- PA_F <- CO2_F <- NULL
  
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
    files <- files[grepl(utils::glob2rx("*_Flux.nc"), files)]
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
    print("join")
    # combine met and flux data
    all <- left_join(df[[1]], df[[2]])
  } else {
    all <- df
  }
  
  # format data as fluxnet compatible
  if (fluxnet_format && !meta_data) {
    # convert time, and only return
    # FLUXNET formatted columns
    
    start_year <- format(min(all$time), "%Y")
    end_year <- format(max(all$time), "%Y")
    
    all <- all %>%
      mutate(
        TIMESTAMP_START = format(time, "%Y%m%d%H%M"),
        TIMESTAMP_END = format(time + 30 * 60, "%Y%m%d%H%M")
      )
    
    keys <- c(
      # MICROMET
      P_F = "Precip", # in mm -s
      TA_F = "Tair",
      SW_IN_F = "SWdown",
      LW_IN_F = "LWdown",
      VPD_F = "VPD",
      WS_F = "Wind",
      PA_F = "Psurf",
      CO2_F = "CO2air",
      # FLUXES
      NETRAD = "Rnet",
      USTAR = "Ustar",
      SW_OUT = "SWup",
      LE_F_MDS = "Qle",
      LE_CORR = "Qle_cor",
      H_F_MDS = "Qh",
      H_CORR = "Qh_cor",
      NEE_VUT_REF = "NEE",
      GPP_VUT_REF = "GPP", # uncertain
      GPP_VUT_REF_SE = "GPP_se", # uncertain
      GPP_DT_VUT_REF = "GPP_DT",
      GPP_DT_VUT_SE = "GPP_DT_se",
      RECO_NT_VUT_REF = "Resp",
      RECO_NT_VUT_SE = "Resp_se"
    )
    
    # columns to select
    loc <- names(keys[which(keys %in% colnames(all))])
    
    # loop over key and rename columns
    for(i in 1:length(keys)) {
      key <- keys[i]
      if(key %in% colnames(all)) {
        new_name <- as.character(names(key))
        old_name <- as.character(key)
        all <- all %>%
          dplyr::rename(
           !!new_name := !!old_name
          )
      }
    }
    
  # return fluxnet selection
  all  <- all %>%
      select(
        TIMESTAMP_START,
        TIMESTAMP_END,
        !!loc
      )
  
  # remaining unit conversions
  # K to C
  # mm s-1 to mm (sum)
  # other conversions
  # https://github.com/aukkola/FluxnetLSM/blob/a256ffc894ed8182f9399afa1d83dea43ac36a95/R/Conversions.R
  all <- all %>%
    mutate(
      P = P * 60 * 30, # mm/s to mm
      TA_F = TA_F - 273.15, # K to C
      PA_F = PA_F / 1000, # Pa to kPa
      CO2_F = CO2_F, # ppm to umolCO2 mol-1
      
      # adding missing data required by ingestr
      # VPD
      VPD_F_QC = 0,
      VPD_F_MDS = NA,
      VPD_F_MDS_QC = NA,
      VPD_ERA = NA,
      
      # Temperature
      TA_F_QC = 0,
      TA_F_MDS = NA,
      TA_F_MDS_QC = NA,
      TA_ERA = NA,
      
      TMIN_F_QC = 0,
      TMIN_F_MDS = NA,
      TMIN_F_MDS_QC = NA,
      TMIN_ERA = NA,
      
      TMAX_F_QC = 0,
      TMAX_F_MDS = NA,
      TMAX_F_MDS_QC = NA,
      TMAX_ERA = NA,
      
      #QA/QC
      NEE_VUT_REF_QC = 1,
      GPP_VUT_REF_QC = 1
    )
  }
  
  # save data to file, using FLUXNET formatting
  if (fluxnet_format && !missing(out_path)) {
    
    message("writing datat to file")
    filename <- sprintf("FLX_%s_PLUMBER_FULLSET_HH_%s_%s_2-3.csv",
                       site,
                       start_year,
                       end_year
                       )
    
    filename <- file.path(
      out_path,
      filename
    )
    
    utils::write.table(
      all,
      file = filename,
      quote = FALSE,
      col.names = TRUE,
      row.names = FALSE,
      sep = ","
    )
  }
  
  # return the merged file
  return(all)
}

