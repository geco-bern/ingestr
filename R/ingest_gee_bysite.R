#' Defines settings for settings for Google Earth Engine download
#'
#' Returns a list of two data frames, one with data at original
#' modis dates (df), and one interpolated to all days (ddf).
#'
#' @param df_siteinfo xxx
#' @param start_date xxx
#' @param end_date xxx
#' @param overwrite_raw xxx
#' @param overwrite_interpol xxx
#' @param band_var xxx
#' @param band_qc xxx
#' @param prod xxx
#' @param prod_suffix xxx
#' @param varnam xxx
#' @param productnam xxx
#' @param scale_factor xxx
#' @param period xxx
#' @param python_path xxx
#' @param gee_path xxx
#' @param data_path xxx
#' @param method_interpol xxx
#' @param keep xxx
#'
#' @return A named list containing information required for download from Google
#' Earth Engine.
#' @export
#'
#' @examples settings_gee <- get_settings_gee( bundle = "modis_fpar" )
#'
ingest_gee_bysite <- function(
  df_siteinfo,
  start_date,
  end_date,
  overwrite_raw,
  overwrite_interpol,
  band_var,
  band_qc,
  prod,
  prod_suffix,
  varnam,
  productnam,
  scale_factor,
  period,
  python_path,
  gee_path,
  data_path,
  method_interpol,
  keep
  ){

  # CRAN compliance, define variables
  lat <- lon <- ymd <- longitude <- latitude <- product <- NULL
  
  # Define names
  
  # this function is hacked to only do one site at a time
  sitename <- df_siteinfo$sitename[1]
  
  df_siteinfo <- slice(df_siteinfo, 1)

  dirnam_daily_csv <- data_path
  dirnam_raw_csv <- paste0(data_path, "/raw/")  #paste0( dirnam_nice_csv, "/raw/" )

  if (!dir.exists(dirnam_daily_csv)){
    dir.create(dirnam_daily_csv)
  }
  
  if (!dir.exists(dirnam_raw_csv)) {
    dir.create(dirnam_raw_csv)
  }

  # create a new prod suffix
  suffix <- paste(prod_suffix, band_var, band_qc, sep = "_")
  
  filnam_daily_csv <- paste0( dirnam_daily_csv, "/",varnam,"_", sitename, ".csv" )
  filnam_raw_csv <- paste0( dirnam_raw_csv, sitename, "_", suffix, "_gee_subset.csv" )

  if(method_interpol == "none"){
    do_continue <- FALSE
  } else{
    do_continue <- TRUE  
  }

  # Save error code
  # 0: no error
  # 1: error: file downloaded bu all data is NA,
  # 2: file not downloaded
  df_error <- tibble()

  if (file.exists(filnam_daily_csv) && !overwrite_interpol){
    
    # Read daily interpolated and gapfilled
    
    ddf <- readr::read_csv( filnam_daily_csv )

  } else {

    if (!file.exists(filnam_raw_csv) || overwrite_raw){
      
      # Download via Google Earth Engine using the python function
      
      path_info <- paste0(dirnam_raw_csv, "info_lonlat.csv")
      utils::write.csv( dplyr::select(
        df_siteinfo,
        site = sitename,
        latitude = lat,
        longitude = lon),
        file=path_info,
        row.names=FALSE )

      start = Sys.time()
      system(sprintf("%s %s/gee_subset.py -p %s -b %s %s -s %s -e %s -f %s -d %s -sc 30",
                     python_path,
                     gee_path,
                     prod,
                     band_var,
                     band_qc,
                     start_date,
                     end_date,
                     path_info,
                     dirnam_raw_csv
      ), wait = TRUE)

      end = Sys.time()
      proc_time = as.vector(end - start)
      message( paste( "... completed in", format( proc_time, digits = 3), "sec" ) )

      # Raw downloaded data is saved to file
      message( paste( "raw data file written:", filnam_raw_csv ) )
    }

    # Read raw data and create a data frame holding the complete time series
    # Note: 'date', 'doy', 'dom', etc. refer to the date, centered within
    # each N-day period. 'date_start' refers to the beginning of each
    # N-day period.
    
    if (file.exists(filnam_raw_csv)){
      
      message("- file exists, reading in previously downloaded data...")

      df <- readr::read_csv( filnam_raw_csv) %>%   #, col_types = cols()
        dplyr::mutate(  date = lubridate::ymd(date) ) %>%
        dplyr::select( -longitude, -latitude, -product )

      # Apply scale factor, specific for each product
      if (any(!is.na(df[[band_var]]))){
        df[[band_var]] <- df[[band_var]] * scale_factor
      } else {
        do_continue <- FALSE
      }

    } else {

      warning( paste( "WARNING: RAW DATA FILE NOT FOUND FOR SITE:", sitename ) )
      df_error <- df_error %>% bind_rows( tibble( mysitename=sitename, error=2 ) )
      out <- NA
      do_continue <- FALSE

    }

    if (do_continue){
      
      message("- interpolating data...")
      
      # Clean (gapfill and interpolate) full time series data to 8-days, daily, and monthly
      ddf <- gapfill_interpol_gee(
        df,
        sitename,
        year_start = lubridate::year(df_siteinfo$date_start),
        year_end   = lubridate::year(df_siteinfo$date_end),
        var_name = band_var,
        qc_name = band_qc,
        prod = prod_suffix,
        method_interpol = method_interpol,
        keep = keep
      )

      message("- writing data to file...")
      
      # save cleaned and interpolated data to file
      readr::write_csv( ddf, file = filnam_daily_csv )

    } else {

      ddf <- init_dates_dataframe(
        lubridate::year(start_date),
        lubridate::year(end_date)
        ) %>%
        dplyr::mutate(fapar = NA)
    }
  }

  df_error <- df_error %>%
    bind_rows( tibble( sitename=sitename, error=0 ) )

  ddf <- ddf %>%
    ungroup() %>%
    dplyr::mutate(sitename = sitename)

  return(ddf)
}

#' Gapfill GEE data products
#' 
#' Gap filling routine for remote
#' sensing data downloaded using the
#' GEE routine
#'
#' @param df GEE downloaded data frame
#' @param sitename site name
#' @param year_start start year
#' @param year_end end year
#' @param var_name variable
#' @param qc_name quality control field
#' @param prod product name
#' @param method_interpol interpolation method
#' @param keep keep (intermediate data??)
#'
#' @return gap filled remote sensing time series
#' @export

gapfill_interpol_gee <- function( 
  df,
  sitename,
  year_start,
  year_end,
  var_name,
  qc_name,
  prod,
  method_interpol,
  keep
  ) {
  

  # Returns data frame containing data
  # (and year, moy, doy) for all available
  # months. Interpolated to mid-months
  # from original 16-daily data.

  # CRAN compliance, predefine internal variables
  value <- modisvar <- qc <- qc_bitname <- vi_useful <- aerosol <- 
    adjcloud <- brdf_corr <- mixcloud <- snowice <- shadow <- qc_bit0 <- 
    qc_bit1 <- qc_bit2 <- qc_bit3 <- qc_bit4 <- CloudState <- modisvar_filtered <- 
    good_quality <- SCF_QC <- sur_refl_qc_500m <- modland_qc <- pixel <- 
    settings_modis <- approx <- prevdate <- modisvar_filled <- 
    Psn_QC <- Gpp <- outlier <- FparLai_QC <- pixel_quality <- data_quality <-  NULL

  # CLEAN AND GAP-FILL

  if (grepl("MOD13Q1", prod)) {

    # This is for MOD13Q1 Vegetation indeces (NDVI, EVI) data downloaded from Google Earth Engine

    # QC interpreted according to https://vip.arizona.edu/documents/MODIS/MODIS_VI_UsersGuide_June_2015_C6.pdf
    df <- df %>%
      dplyr::rename(modisvar = value) %>%
      dplyr::mutate(modisvar_filtered = modisvar) %>%

      # separate into bits
      rowwise() %>%
      mutate(qc_bitname = intToBits( qc ) %>%
               as.character() %>%
               paste(collapse = "")
             ) %>%

      # Bits 0-1: VI Quality
      #   00 VI produced with good quality
      #   01 VI produced, but check other QA
      mutate(vi_quality = substr( qc_bitname, start=1, stop=2 )) %>%

      # Bits 2-5: VI Usefulness
      #   0000 Highest quality
      #   0001 Lower quality
      #   0010 Decreasing quality
      #   0100 Decreasing quality
      #   1000 Decreasing quality
      #   1001 Decreasing quality
      #   1010 Decreasing quality
      #   1100 Lowest quality
      #   1101 Quality so low that it is not useful
      #   1110 L1B data faulty
      #   1111 Not useful for any other reason/not processed
      mutate(vi_useful = substr( qc_bitname, start=3, stop=6 )) %>%
      dplyr::mutate(
        modisvar_filtered = ifelse(
          vi_useful %in% c("0000", "0001", "0010", "0100",
                           "1000", "1001", "1010", "1100"), modisvar, NA)) %>%

      # Bits 6-7: Aerosol Quantity
      #  00 Climatology
      #  01 Low
      #  10 Intermediate
      #  11 High
      mutate(aerosol = substr( qc_bitname, start=7, stop=8 )) %>%
      dplyr::mutate(modisvar_filtered = ifelse(aerosol %in% c("00", "01", "10"),
                                               modisvar, NA)) %>%

      # Bit 8: Adjacent cloud detected
      #  0 No
      #  1 Yes
      mutate(adjcloud = substr( qc_bitname, start=9, stop=9 )) %>%
      dplyr::mutate(modisvar_filtered = ifelse(adjcloud %in% c("0"),
                                               modisvar, NA)) %>%

      # Bits 9: Atmosphere BRDF Correction
      #   0 No
      #   1 Yes
      mutate(brdf_corr = substr( qc_bitname, start=10, stop=10 )) %>%
      dplyr::mutate(modisvar_filtered = ifelse(brdf_corr %in% c("1"),
                                               modisvar, NA)) %>%

      # Bits 10: Mixed Clouds
      #   0 No
      #   1 Yes
      mutate(mixcloud = substr( qc_bitname, start=11, stop=11 )) %>%
      dplyr::mutate(modisvar_filtered = ifelse(mixcloud %in% c("0"),
                                               modisvar, NA)) %>%

      # Bits 11-13: Land/Water Mask
      #  000 Shallow ocean
      #  001 Land (Nothing else but land)
      #  010 Ocean coastlines and lake shorelines
      #  011 Shallow inland water
      #  100 Ephemeral water
      #  101 Deep inland water
      #  110 Moderate or continental ocean
      #  111 Deep ocean
      mutate(mask = substr( qc_bitname, start=12, stop=14 )) %>%

      # Bits 14: Possible snow/ice
      #  0 No
      #  1 Yes
      mutate(snowice = substr( qc_bitname, start=15, stop=15 )) %>%
      dplyr::mutate(modisvar_filtered = ifelse(snowice %in% c("0"), modisvar, NA)) %>%

      # Bits 15: Possible shadow
      #  0 No
      #  1 Yes
      mutate(shadow = substr( qc_bitname, start=16, stop=16 )) %>%
      dplyr::mutate(modisvar_filtered = ifelse(shadow %in% c("0"), modisvar, NA)) %>%

      # drop it
      dplyr::select(-qc_bitname)

  } else if (grepl("MCD15A3H", prod)) {
    
    message("- processing fpar data")
    
    # QC interpreted according to https://explorer.earthengine.google.com/#detail/MODIS%2F006%2FMCD15A3H:
    # This is interpreted according to https://lpdaac.usgs.gov/documents/2/mod15_user_guide.pdf, p.9
    
    df <- df %>%

      dplyr::rename(modisvar = !!var_name) %>%
      dplyr::mutate(modisvar_filtered = modisvar) %>%

      # separate into bits
      rowwise() %>%
      mutate(qc_bitname = intToBits( FparLai_QC )[1:8] %>%
               rev() %>%
               as.character() %>% 
               paste(collapse = "")
             )
    
    message("- extracted qc bits")
    
    df <- df %>%
      
      # MODLAND_QC bits
      # 0: Good  quality (main algorithm with or without saturation)
      # 1: Other quality (backup  algorithm or fill values)
      mutate(qc_bit0 = substr( qc_bitname, start=8, stop=8 )) %>%
      mutate(good_quality = ifelse( qc_bit0=="0", TRUE, FALSE )) %>%

      # Sensor
      # 0: Terra
      # 1: Aqua
      mutate(qc_bit1 = substr( qc_bitname, start=7, stop=7 )) %>%
      mutate(terra = ifelse( qc_bit1 == "0", TRUE, FALSE )) %>%

      # Dead detector
      # 0: Detectors apparently  fine  for up  to  50% of  channels  1,  2
      # 1: Dead  detectors caused  >50%  adjacent  detector  retrieval
      mutate(qc_bit2 = substr( qc_bitname, start=6, stop=6 )) %>%
      mutate(dead_detector = ifelse( qc_bit2=="1", TRUE, FALSE )) %>%

      # CloudState
      # 00 0  Significant clouds  NOT present (clear)
      # 01 1  Significant clouds  WERE  present
      # 10 2  Mixed cloud present in  pixel
      # 11 3  Cloud state not defined,  assumed clear
      mutate(qc_bit3 = substr( qc_bitname, start=4, stop=5 )) %>%
      mutate(CloudState = ifelse( qc_bit3=="00", 0,
                                  ifelse( qc_bit3=="01",
                                    1, ifelse( qc_bit3=="10", 2, 3 ) ) )) %>%

      # SCF_QC (five level confidence score)
      # 000 0 Main (RT) method used, best result possible (no saturation)
      # 001 1 Main (RT) method used with saturation. Good, very usable
      # 010 2 Main (RT) method failed due to bad geometry, empirical algorithm used
      # 011 3 Main (RT) method failed due to problems other than geometry, empirical algorithm used
      # 100 4 Pixel not produced at all, value couldn???t be retrieved (possible reasons: bad L1B data, unusable MOD09GA data)
      mutate(qc_bit4 = substr( qc_bitname, start=1, stop=3 )) %>%
      mutate(SCF_QC = ifelse(
        qc_bit4=="000",
        0, ifelse( qc_bit4=="001",
         1, ifelse( qc_bit4=="010", 2, ifelse( qc_bit4=="011", 3, 4 ) ) ) )) %>%

      # Actually do the filtering
      mutate(modisvar_filtered = ifelse( CloudState %in% c(0), modisvar_filtered, NA )) %>%
      mutate(modisvar_filtered = ifelse( good_quality, modisvar_filtered, NA )) %>%

      # new addition 5.1.2021
      # mutate(modisvar_filtered = ifelse( !dead_detector, modisvar_filtered, NA )) %>%
      mutate(modisvar_filtered = ifelse( SCF_QC %in% c(0,1), modisvar_filtered, NA ))
    
  } else if (grepl("MOD17A2H", prod)) {
    # Contains MODIS GPP
    # quality bitmap interpreted based on https://lpdaac.usgs.gov/dataset_discovery/modis/modis_products_table/mod17a2

    df$qc_bitname <- sapply(
      seq(nrow(df)), function(x) as.integer( intToBits( df$Psn_QC[x] )[1:8] ) %>%
        rev() %>%
        as.character() %>%
        paste( collapse="" )  )

    # MODLAND_QC bits
    # 0: Good  quality (main algorithm with  or without saturation)
    # 1: Other quality (backup  algorithm or  fill  values)
    df$qc_bit0 <- substr( df$qc_bitname, start=8, stop=8 )

    # Sensor
    # 0: Terra
    # 1: Aqua
    df$qc_bit1 <- substr( df$qc_bitname, start=7, stop=7 )

    # Dead detector
    # 0: Detectors apparently  fine  for up  to  50% of  channels  1,  2
    # 1: Dead  detectors caused  >50%  adjacent  detector  retrieval
    df$qc_bit2 <- substr( df$qc_bitname, start=6, stop=6 )

    # CloudState
    # 00 0  Significant clouds  NOT present (clear)
    # 01 1  Significant clouds  WERE  present
    # 10 2  Mixed cloud present in  pixel
    # 11 3  Cloud state not defined,  assumed clear
    df$qc_bit3 <- substr( df$qc_bitname, start=4, stop=5 )

    # SCF_QC (five level confidence score)
    # 000 0 Very best possible
    # 001 1 Good, very usable, but not the best (saturation in FPAR/LAI has occurred)
    # 010 2 Substandard due to geometry problems ??? use with caution
    # 011 3 Substandard due to other than geometry problems ??? use with caution
    # 100 4  Couldn't retrieve pixel (NOT PRODUCED AT ALL ??? non-terrestrial biome)
    # 111 7  Fill Value
    df$qc_bit4 <- substr( df$qc_bitname, start=1, stop=3 )

    df <- df %>%  dplyr::mutate(
      good_quality  = ifelse( qc_bit0=="0", TRUE, FALSE ),
       terra = ifelse( qc_bit1=="0", TRUE, FALSE ),
       dead_detector = ifelse( qc_bit2=="1", TRUE, FALSE ),
       CloudState = ifelse( qc_bit3=="00",
                            0,
                            ifelse( qc_bit3=="01",
                                    1, ifelse( qc_bit3=="10", 2, 3 ) ) ),
       SCF_QC = ifelse( qc_bit4=="000",
                        0,
                        ifelse( qc_bit4=="001",
                                1,
                                ifelse( qc_bit4=="010",
                                        2,
                                        ifelse( qc_bit4=="011",
                                                3,
                                                ifelse( qc_bit4=="111", 7, NA)
                                                )
                                        )
                                )
                        )
      ) %>%
      dplyr::select(
        -qc_bitname,
        -Psn_QC,
        -qc_bit0,
        -qc_bit1,
        -qc_bit2,
        -qc_bit3,
        -qc_bit4 
        ) %>%
      dplyr::rename( modisvar = Gpp ) %>%

      # Identify outliers, i.e. whether value is exceedingly high, 
      # i.e. if the distance of the value to the median is more than 5 times
      #  the distance of the distance of the 75% quantile to the median
      dplyr::mutate(
        outlier = ifelse(
          modisvar - stats::median( modisvar, na.rm=TRUE ) > 5 * 
          ( stats::quantile( modisvar, probs=0.75, na.rm=TRUE  ) - 
              stats::median( modisvar, na.rm=TRUE ) ), TRUE, FALSE ) ) %>%

      # Filter, i.e. replacing by NA in order to keep all dates
      dplyr::mutate( 
          modisvar_filtered = ifelse( ( !outlier ), modisvar, NA ) 
        ) %>%

      # no replacement with mean seasonal cycle here
      dplyr::mutate( modisvar_filled = modisvar_filtered )


  } else if (prod == "MOD09A1"){
    
    # Filter surface reflectance data
    
    # QC interpreted according to 
    # https://modis-land.gsfc.nasa.gov/pdf/MOD09_UserGuide_v1.4.pdf,
    # Section 3.2.2, Table 10 500 m, 1 km and Coarse Resolution Surface 
    # Reflectance Band Quality Description (32-bit). Bit 0 is LSB.
    clean_sur_refl <- function(x, qc_binary){
      ifelse(qc_binary, x, NA)
    }
    
    df <- df %>%
      
      # separate into bits
      rowwise() %>%
      mutate(qc_bitname = intToBits( sur_refl_qc_500m ) %>%
               as.character() %>%
               paste(collapse = "")
      ) %>%
      
      # Bits 0-1: MODLAND QA bits
      #   00 corrected product produced at ideal quality -- all bands
      #   01 corrected product produced at less than ideal quality -- some or all bands
      #   10 corrected product not produced due to cloud effects -- all bands
      #   11 corrected product not produced for other reasons -- some or all bands, may be fill value (11) [Note that a value of (11) overrides a value of (01)].
      mutate(
        modland_qc = substr( qc_bitname, start=1, stop=2 )
      ) %>%
      mutate(
        modland_qc_binary = ifelse(modland_qc %in% c("00"), TRUE, FALSE)
      ) %>%   # false for removing data
      mutate(across(starts_with("sur_refl_b"),
                    ~clean_sur_refl(., modland_qc_binary))) %>%
      
      # drop it
      dplyr::select(-ends_with("_qc"), -ends_with("_qc_binary"))
    
  } else if (prod == "MOD11A2"){
    
    # Filter available landsurface temperature data for daily-means
    # QC interpreted according to 
    # https://lpdaac.usgs.gov/documents/118/MOD11_User_Guide_V6.pdf
    df <- df %>%
      dplyr::rename(modisvar = value) %>%
      dplyr::mutate(modisvar_filtered = modisvar) %>%
      rowwise() %>%
      mutate(
        qc_bitname = intToBits( qc )[1:8] %>%
          rev() %>%
          as.character() %>%
          paste(collapse = "")
      ) %>%
      
      # Bits 0-1: Pixel Quality
      #   00 Pixel produced with good quality
      #   01 Pixel produced, but check other QA
      dplyr::mutate(
        pixel_quality = substr( qc_bitname, start = 1, stop = 2 )
      ) %>%
      dplyr::mutate(
        modisvar_filtered = ifelse(
          pixel_quality %in% c("00", "01"),
          modisvar_filtered, NA)
      ) %>%
      
      # Bits 2-3: Data Quality
      #   00 = Good data quality of L1B bands 29, 31, 32
      #   01 = other quality data
      #   10 = 11 = TBD
      dplyr::mutate(
        data_quality = substr( qc_bitname, start = 3, stop = 4 )
      ) %>%
      
      dplyr::mutate(
        modisvar_filtered = ifelse(
          data_quality %in% c("00", "01"),
          modisvar_filtered, NA),
        modisvar_filtered = ifelse(
          modisvar_filtered <= 0, NA, modisvar_filtered)
      ) %>%
      
      # drop it
      dplyr::select(-qc_bitname)
    
  } else if (prod == "MCD43A4") {
    
    # MCD43A4 uses a simple binary quality control flag
    # with 0 = good quality, 1 = incomplete inversion
    df <- df %>%
      dplyr::rename(modisvar = value) %>%
      dplyr::mutate(modisvar_filtered = modisvar) %>%
      mutate(
        pixel_quality = contains("Quality_Band")
      ) %>%
      dplyr::mutate(
        modisvar_filtered = ifelse(
          pixel_quality == 0,
          modisvar_filtered, NA)
      )
    
  } else if (prod == "MODOCGA") {
      
      # dynamic filtering depending
      # on the band used
      band <- names(df)[3]
      
      if (grepl("b08", band)){
        bits <- c(0,3)
      }
      
      if (grepl("b09", band)){
        bits <- c(4,7)
      }
      
      if (grepl("b10", band)){
        bits <- c(8,11)
      }
      
      if (grepl("b11", band)){
        bits <- c(12,15)
      }
      
      if (grepl("b12", band)){
        bits <- c(16,19)
      }
      
      if (grepl("b13", band)){
        bits <- c(20,23)
      }
      
      if (grepl("b14", band)){
        bits <- c(24,27)
      }
      
      if (grepl("b15", band)){
        bits <- c(28,31)
      }
      
      if (grepl("b16", band)){
        bits <- c(4,7)
      }
      
      df <- df %>%
        dplyr::rename(modisvar = value) %>%
        dplyr::mutate(modisvar_filtered = modisvar) %>%
        mutate(
          
          qc_bitname = intToBits(qc) %>%
            rev() %>%
            as.character() %>%
            paste(collapse = "")
          
        ) %>%
        
        # Bits 0-1: Pixel Quality
        #   00 Pixel produced with good quality
        #   01 Pixel produced, but check other QA
        dplyr::mutate(
          pixel_quality = as.numeric(substr(qc_bitname, start = bits[1], stop = bits[2]))
        ) %>%
        dplyr::mutate(
          modisvar_filtered = ifelse(
            pixel_quality == 0,
            modisvar_filtered, NA)
        ) %>%
        
        # drop it
        dplyr::select(-qc_bitname)
    
  }

  
  # Create daily dataframe
  message("- generate daily values")
  ddf <- init_dates_dataframe( year_start, year_end ) %>%

      # decimal date
      mutate(year_dec = lubridate::decimal_date(date))

  # merge N-day dataframe into daily one.
  # Warning: here, 'date' must be centered within 4-day period - 
  # thus not equal to start date but (start date + 2)
  message("- merging daily time series with original data")
  ddf <- ddf %>%
    left_join( df, by = "date" )

  if (method_interpol == "loess" || keep){

    # get LOESS spline model for predicting daily values (used below)

    message("loess...")

    # determine periodicity
    period <- ddf %>%
      filter(!is.na(modisvar_filtered)) %>%
      mutate(prevdate = lag(date)) %>%
      mutate(period = as.integer(difftime(date, prevdate))) %>%
      pull(period) %>%
      min(na.rm = TRUE)

    # take a three-weeks window for locally weighted regression (loess)
    # good explanation: 
    # https://rafalab.github.io/dsbook/smoothing.html#local-weighted-regression-loess
    
    ndays_tot <- lubridate::time_length(diff(range(ddf$date)), unit = "day")
    span <- 100/ndays_tot 

    idxs    <- which(!is.na(ddf$modisvar_filtered))
    myloess <- try(stats::loess(modisvar_filtered ~ year_dec,
                                data = ddf[idxs,], span = span))

    # predict stats::loess to all dates with missing data
    tmp <- try(stats::predict( myloess, newdata = ddf ) )
    if (inherits(tmp, 'try-error')){
      ddf$loess <- rep( NA, nrow(ddf) )
    } else {
      ddf$loess <- tmp
    }
  }

  if (method_interpol == "spline" || keep){

    # get SPLINE model for predicting daily values (used below)

    message("spline...")
    idxs   <- which(!is.na(ddf$modisvar_filtered))
    spline <- try(
      with(ddf,
          stats::smooth.spline(year_dec[idxs],
                        modisvar_filtered[idxs], spar = 0.01)))

    # predict SPLINE
    tmp <- try( with( ddf, stats::predict( spline, year_dec ) )$y)
    if (inherits(tmp, 'try-error')){
      ddf$spline <- rep( NA, nrow(ddf) )
    } else {
      ddf$spline <- tmp
    }
  }

  if (method_interpol == "linear" || keep){

    # LINEAR INTERPOLATION
    
    message("linear ...")
    ddf$linear <- stats::approx(
      ddf$year_dec,
      ddf$modisvar_filtered,
      xout=ddf$year_dec )$y
  }

  if (method_interpol == "sgfilter" || keep){
    
    # SAVITZKY GOLAY FILTER
    
    message("sgfilter ...")
    ddf$sgfilter <- rep( NA, nrow(ddf) )
    idxs <- which(!is.na(ddf$modisvar_filtered))
    tmp <- try(signal::sgolayfilt( ddf$modisvar_filtered[idxs], p=3, n=51 ))
    if (inherits(tmp, 'try-error')){
      #
    } else {
      ddf$sgfilter[idxs] <- tmp
    }
    
  }

  
  # # Define 'fapar'
  
  # if (method_interpol == "loess"){
  #   ddf$modisvar_filled <- ddf$loess
  # } else if (method_interpol == "spline"){
  #   ddf$modisvar_filled <- ddf$spline
  # } else if (method_interpol == "linear"){
  #   ddf$modisvar_filled <- ddf$linear
  # } else if (method_interpol == "sgfilter"){
  #   ddf$modisvar_filled <- ddf$sgfilter
  # }
  # 
  # # limit to within 0 and 1 (loess spline sometimes "explodes")
  # ddf <- ddf %>%
  #   dplyr::mutate(
  #     modisvar_filled = replace( modisvar_filled, modisvar_filled<0, 0)
  #     ) %>%
  #   dplyr::mutate(
  #     modisvar_filled = replace( modisvar_filled, modisvar_filled>1, 1)
  #     )
  # 
  # # extrapolate missing values at head and tail again
  
  # ddf$modisvar_filled <- extrapolate_missing_headtail(
  #   dplyr::select(ddf, var = modisvar_filled)
  #   )

  return( ddf )

}

# extrapolate to missing values at head and tail using mean seasonal cycle

extrapolate_missing_headtail <- function(ddf){
  
  # define variables
  var <- NULL
  
  # new: fill gaps at head
  idxs <- findna_head( ddf$var )
  if (length(idxs)>0){
    warning("Filling values with last available data point at head")
  }
  
  ddf$var[idxs] <- ddf$var[max(idxs)+1]

  # new: fill gaps at tail
  idxs <- findna_tail( ddf$var )
  if (length(idxs)>0){
    warning("Filling values with last available data point at tail.")
  } 
  ddf$var[idxs] <- ddf$var[min(idxs)-1]

  vec <- ddf %>%
    dplyr::pull(var)

  return(vec)
}