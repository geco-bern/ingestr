#' Gap fills a modis time series
#'
#' @param df data frame with time series data
#' @param sitename a site name
#' @param date_start a start year
#' @param date_end an end year
#' @param settings processing settings file

#' @return gap filled time series
#' @export

gapfill_interpol <- function(
  df,
  sitename,
  date_start,
  date_end,
  settings
){
  
  value <- modisvar <- qc <- qc_bitname <- vi_useful <- aerosol <-
    adjcloud <- brdf_corr <- mixcloud <- snowice <- shadow <-
    qc_bit0 <- qc_bit1 <- qc_bit2 <- qc_bit3 <- qc_bit4 <- modisvar_filtered <- 
    good_quality <- SCF_QC <- modland_qc <- pixel_quality <- data_quality <-
    prevdate <- CloudState <- sur_refl_qc_500m <- pixel <- NULL
  
  # split out variables for readability
  prod <- settings$prod
  method_interpol <- settings$method_interpol
  keep <- settings$keep
  n_focal <- settings$n_focal
  varnam <- settings$varnam

  # Returns data frame containing data
  # (and year, moy, doy) for all available
  # months. Interpolated to mid-months
  # from original 16-daily data.
  
  # CLEAN AND GAP-FILL
  
  if (prod == "MOD13Q1"){
    
    # This is for MOD13Q1 Vegetation indeces (NDVI, EVI) 
    # data downloaded from MODIS LP DAAC
    
    # QC interpreted according to 
    # https://vip.arizona.edu/documents/MODIS/MODIS_VI_UsersGuide_June_2015_C6.pdf
    df <- df %>%
      dplyr::rename(modisvar = value) %>%
      dplyr::mutate(modisvar_filtered = modisvar) %>%
      
      # separate into bits
      rowwise() %>%
      mutate(
        qc_bitname = intToBits( qc ) %>%
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
                           "1000", "1001", "1010", "1100"),
          modisvar, NA)) %>%
      
      # Bits 6-7: Aerosol Quantity
      #  00 Climatology
      #  01 Low
      #  10 Intermediate
      #  11 High
      mutate(aerosol = substr( qc_bitname, start=7, stop=8 )) %>%
      dplyr::mutate(
        modisvar_filtered = ifelse(
          aerosol %in% c("00", "01", "10"), modisvar, NA)) %>%
      
      # Bit 8: Adjacent cloud detected
      #  0 No
      #  1 Yes
      mutate(adjcloud = substr( qc_bitname, start=9, stop=9 )) %>%
      dplyr::mutate(
        modisvar_filtered = ifelse(
          adjcloud %in% c("0"), modisvar, NA)) %>%
      
      # Bits 9: Atmosphere BRDF Correction
      #   0 No
      #   1 Yes
      mutate(brdf_corr = substr( qc_bitname, start=10, stop=10 )) %>%
      dplyr::mutate(
        modisvar_filtered = ifelse(
          brdf_corr %in% c("1"), modisvar, NA)) %>%
      
      # Bits 10: Mixed Clouds
      #   0 No
      #   1 Yes
      mutate(mixcloud = substr( qc_bitname, start=11, stop=11 )) %>%
      dplyr::mutate(
        modisvar_filtered = ifelse(
          mixcloud %in% c("0"), modisvar, NA)) %>%
      
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
      dplyr::mutate(
        modisvar_filtered = ifelse(
          snowice %in% c("0"), modisvar, NA)) %>%
      
      # Bits 15: Possible shadow
      #  0 No
      #  1 Yes
      mutate(shadow = substr( qc_bitname, start=16, stop=16 )) %>%
      dplyr::mutate(
        modisvar_filtered = ifelse(shadow %in% c("0"), modisvar, NA)) %>%
      
      # drop it
      dplyr::select(-qc_bitname)
    
    
  } else if (prod == "MCD15A3H"){
    
    # QC interpreted according to
    #  https://explorer.earthengine.google.com/#detail/MODIS%2F006%2FMCD15A3H:
    
    # This is interpreted according to 
    # https://lpdaac.usgs.gov/documents/2/mod15_user_guide.pdf, p.9
    df <- df %>%
      
      dplyr::rename(modisvar = value) %>%
      dplyr::mutate(modisvar_filtered = modisvar) %>%
      
      # separate into bits
      rowwise() %>%
      mutate(
        qc_bitname = intToBits( qc )[1:8] %>%
          rev() %>%
          as.character() %>%
          paste(collapse = "")
      ) %>%
      
      # MODLAND_QC bits
      # 0: Good  quality (main algorithm with or without saturation)
      # 1: Other quality (backup  algorithm or fill values)
      mutate(qc_bit0 = substr( qc_bitname, start=8, stop=8 )) %>%
      mutate(good_quality = ifelse( qc_bit0=="0", TRUE, FALSE )) %>%
      
      # Sensor
      # 0: Terra
      # 1: Aqua
      mutate(qc_bit1 = substr( qc_bitname, start=7, stop=7 )) %>%
      mutate(terra = ifelse( qc_bit1=="0", TRUE, FALSE )) %>%
      
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
      mutate(CloudState = ifelse(
        qc_bit3=="00", 0,
        ifelse( qc_bit3=="01", 1,
                ifelse( qc_bit3=="10", 2, 3 ) ) )
      ) %>%
      
      # SCF_QC (five level confidence score)
      # 000 0 Main (RT) method used, best result possible (no saturation)
      # 001 1 Main (RT) method used with saturation. Good, very usable
      # 010 2 Main (RT) method failed due to bad geometry, empirical algorithm used
      # 011 3 Main (RT) method failed due to problems other than geometry, empirical algorithm used
      # 100 4 Pixel not produced at all, value couldn???t be retrieved 
      # (possible reasons: bad L1B data, unusable MOD09GA data)
      mutate(qc_bit4 = substr( qc_bitname, start=1, stop=3 )) %>%
      mutate(SCF_QC = ifelse(
        qc_bit4=="000", 0,
        ifelse( qc_bit4=="001", 1,
                ifelse( qc_bit4=="010", 2,
                        ifelse( qc_bit4=="011", 3, 4 ) ) ) )
      ) %>%
      
      # Actually do the filtering
      mutate(modisvar_filtered = ifelse(
        CloudState %in% c(0), modisvar_filtered, NA )) %>%
      mutate(modisvar_filtered = ifelse(
        good_quality, modisvar_filtered, NA )) %>%
      
      # new addition 5.1.2021
      mutate(modisvar_filtered = ifelse(
        SCF_QC %in% c(0,1), modisvar_filtered, NA ))
    
    
  } else if (prod=="MOD17A2H"){
    # Contains MODIS GPP
    # quality bitmap interpreted based on 
    # https://lpdaac.usgs.gov/dataset_discovery/modis/modis_products_table/mod17a2
    
    # No filtering here!
    df <- df %>%
      dplyr::rename(modisvar = value) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(modisvar_filtered = modisvar)
    
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
      
  }
  
  # Create daily dataframe
  
  ddf <- init_dates_dataframe(
    lubridate::year(date_start),
    lubridate::year(date_end)
    ) %>%
    
    # decimal date
    mutate(year_dec = lubridate::decimal_date(date))
  
  
  # Average across pixels by date
  npixels <- df %>%
    pull(pixel) %>%
    unique() %>%
    length()
  n_side <- sqrt(npixels)
  
  # determine across which pixels to average
  arr_pixelnumber <- matrix(1:npixels, n_side, n_side, byrow = TRUE)
  
  # determine the distance from ("layer around") the focal point
  i_focal <- ceiling(n_side/2)
  j_focal <- ceiling(n_side/2)
  if (i_focal != j_focal){ rlang::abort("Aborting. Non-quadratic subset.") }
  if ((n_focal + 1) > i_focal){
    rlang::abort(
      paste("Aborting.
             Not enough pixels available for your choice of n_focal:",
            n_focal)) }
  
  arr_distance <- matrix(rep(NA, npixels), n_side, n_side, byrow = TRUE)
  for (i in seq(n_side)){
    for (j in seq(n_side)){
      arr_distance[i,j] <- max(abs(i - i_focal), abs(j - j_focal))
    }
  }
  
  # create a mask based on the selected layer (0 for focal point only)
  arr_mask <- arr_pixelnumber
  arr_mask[which(arr_distance > n_focal)] <- NA
  vec_usepixels <- c(arr_mask) %>%
    stats::na.omit() %>%
    as.vector()
  
  message(paste("Number of available pixels: ", npixels))
  message(paste("Averaging across number of pixels: ",
                      length(vec_usepixels)))
  
  # take mean across selected pixels
  if (prod == "MOD09A1"){
    varnams <- settings$band_var
  } else {
    varnams <- c("modisvar", "modisvar_filtered")
  }
  
  # to control which pixel's information to be used.
  df <- df %>%
    group_by(date) %>%
    dplyr::filter(pixel %in% vec_usepixels) %>%    
    summarise(across(varnams, ~mean(., na.rm = TRUE)))
  
  
  # merge N-day dataframe into daily one.
  # Warning: here, 'date' must be centered within 4-day period - 
  # thus not equal to start date but (start date + 2)
  
  ddf <- ddf %>%
    left_join( df, by="date" )
  
  # # extrapolate missing values at head and tail
  # ddf$modisvar <- 
  # extrapolate_missing_headtail(dplyr::select(ddf, var = modisvar, doy))
  
  if (settings$interpol){
    
    # Interpolate to daily
    
    if (prod=="MOD09A1"){
      
      # For reflectance data, linearly interpolate all bands to daily
      myapprox <- function(vec){
        x <- 1:length(vec)
        stats::approx(x, vec, xout = x)$y
      }
      
      ddf <- ddf %>%
        mutate(across(settings$band_var, ~myapprox(.)))
      
    } else {
      
      if (method_interpol == "loess" || keep){
        
        # get LOESS spline model for predicting daily values (used below)
        
        message("loess...")
        
        # determine periodicity
        period <- ddf %>%
          dplyr::filter(!is.na(modisvar_filtered)) %>%
          mutate(prevdate = lag(date)) %>%
          mutate(period = as.integer(difftime(date, prevdate))) %>%
          pull(period) %>%
          min(na.rm = TRUE)
        
        # take a three-weeks window for locally weighted regression (loess)
        # good explanation: 
        # https://rafalab.github.io/dsbook/smoothing.html#local-weighted-regression-loess
        ndays_tot <- lubridate::time_length(diff(range(ddf$date)), unit = "day")
        # (20*period)/ndays_tot  
        # multiply with larger number to get smoother curve
        span <- 100/ndays_tot
        
        idxs    <- which(!is.na(ddf$modisvar_filtered))
        myloess <- try(stats::loess( modisvar_filtered ~ year_dec,
                                     data = ddf[idxs,], span=span ),
                       silent = TRUE)
        
        # predict LOESS to all dates with missing data
        tmp <- try(stats::predict( myloess, newdata = ddf ), silent = TRUE)
        if (class(tmp)!="try-error"){
          ddf$loess <- tmp
        } else {
          ddf$loess <- rep( NA, nrow(ddf) )
        }
      }
      
      if (method_interpol == "spline" || keep){
        
        # get SPLINE model for predicting daily values (used below)
        
        message("spline...")
        idxs   <- which(!is.na(ddf$modisvar_filtered))
        spline <- try(
          with(ddf,
               stats::smooth.spline(
                 year_dec[idxs],
                 modisvar_filtered[idxs], spar=0.01)),
          silent = TRUE)
        
        # predict SPLINE
        tmp <- try( with( ddf, predict( spline, year_dec ) )$y)
        if (class(tmp)!="try-error"){
          ddf$spline <- tmp
        } else {
          ddf$spline <- rep( NA, nrow(ddf) )
        }
        
      }
      
      if (method_interpol == "linear" || keep){
        
        # LINEAR INTERPOLATION
        
        message("linear ...")
        tmp <- try(
          stats::approx(ddf$year_dec,
                        ddf$modisvar_filtered,
                        xout=ddf$year_dec
                        ),
                   silent = TRUE)
        if (class(tmp) == "try-error"){
          ddf <- ddf %>% mutate(linear = NA)
        } else {
          ddf$linear <- tmp$y
        }
      }
      
      # commented out to avoid dependency to 'signal'
      if (method_interpol == "sgfilter" || keep){
        
        # SAVITZKY GOLAY FILTER
        
        message("sgfilter ...")
        ddf$sgfilter <- rep( NA, nrow(ddf) )
        ddf <- ddf %>%
          mutate(
            modisvar_filtered = ifelse(
              is.nan(modisvar_filtered),
              NA,
              modisvar_filtered)
          )
        idxs <- which(!is.na(ddf$modisvar_filtered))
        tmp <- 
          suppressMessages(
            try(
              signal::sgolayfilt( ddf$modisvar_filtered[idxs], p=3, n=51 ),
              silent = TRUE
            )
          )
        
        if (!inherits(tmp, "try-error")){
          ddf$sgfilter[idxs] <- tmp
        } else {
          message("SG filter failed, no smoothing returned...")
        }
        
      }
      
      
      # Define the variable to be returned in 
      # the column given by settings$varnam
      
      if (method_interpol == "loess"){
        ddf[[ varnam ]] <- ddf$loess
      } else if (method_interpol == "spline"){
        ddf[[ varnam ]] <- ddf$spline
      } else if (method_interpol == "linear"){
        ddf[[ varnam ]] <- ddf$linear
      } else if (method_interpol == "sgfilter"){
        ddf[[ varnam ]] <- ddf$sgfilter
      }
      
    }
    
  }
  
  # filter by year
  ddf <- ddf %>%
    dplyr::filter(
      date >= as.Date(date_start) &
        date <= as.Date(date_end)) %>% 
    dplyr::mutate(
      modisvar_filtered = ifelse(
        is.nan(modisvar_filtered),
        NA,
        modisvar_filtered)
      )
  
  return(ddf)
}