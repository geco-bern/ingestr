#' Defines settings for settings for Google Earth Engine download
#'
#' Returns a list of two data frames, one with data at original
#' modis dates (df), and one interpolated to all days (ddf).
#'
#' @param df_siteinfo xxx
#' @param settings xxx
#'
#' @return A named list containing information required for download from Google
#' Earth Engine.
#' @export
#'
#' @examples settings_gee <- get_settings_gee( bundle = "modis_fpar" )
#'
ingest_modis_bysite <- function( df_siteinfo, settings ){

  ##---------------------------------------------
  ## Define names
  ##---------------------------------------------
  ## this function is hacked to only do one site at a time
  sitename <- df_siteinfo$sitename[1]
  df_siteinfo <- slice(df_siteinfo, 1)

  dirnam_daily_csv <- settings$data_path
  dirnam_raw_csv <- paste0(settings$data_path, "/raw/")  #paste0( dirnam_nice_csv, "/raw/" )

  if (!dir.exists(dirnam_daily_csv)) system( paste( "mkdir -p ", dirnam_daily_csv ) )
  if (!dir.exists(dirnam_raw_csv)) system( paste( "mkdir -p ", dirnam_raw_csv ) )

  filnam_daily_csv <- paste0( dirnam_daily_csv, "/", settings$productnam, "_daily_", sitename, ".csv" )
  filnam_raw_csv <- paste0( dirnam_raw_csv, "/", settings$productnam, "_", sitename, ".csv" )

  do_continue <- TRUE

  ## Save error code (0: no error, 1: error: file downloaded bu all data is NA, 2: file not downloaded)
  df_error <- tibble()

  if (file.exists(filnam_daily_csv) && !settings$overwrite_interpol){
    ##---------------------------------------------
    ## Read daily interpolated and gapfilled
    ##---------------------------------------------
    ddf <- readr::read_csv( filnam_daily_csv )

  } else {

    if (!file.exists(filnam_raw_csv) || settings$overwrite_raw){
      ##---------------------------------------------
      ## Download from MODIS DAAC server
      ##---------------------------------------------
      df <- MODISTools::mt_subset(
          product   = settings$prod,                          # the chosen product (e.g., for NDVI "MOD13Q1")
          lat       = df_siteinfo$lat,                        # latitude
          lon       = df_siteinfo$lon,                        # longitude
          band      = c(settings$band_var, settings$band_qc), # chosen bands
          start     = df_siteinfo$date_start,                 # start date: 1st Jan 2009
          end       = df_siteinfo$date_end,                   # end date: 19th Dec 2014
          km_lr     = 0.55,                                   # kilometers left & right of the chosen location (lat/lon above)
          km_ab     = 0.55,                                   # kilometers above and below the location
          site_name = df_siteinfo$sitename,                   # the site name we want to give the data
          internal  = TRUE,
          progress  = TRUE
          ) %>%
        as_tibble()

      ## Raw downloaded data is saved to file
      rlang::inform( paste( "raw data file written:", filnam_raw_csv ) )
      readr::write_csv(df, path = filnam_raw_csv)

    } else {

      ## read from file
      df <- readr::read_csv( filnam_raw_csv )

    }


    ##--------------------------------------------------------------------
    ## Reformat raw data
    ##--------------------------------------------------------------------
    df <- df %>%

      ## put QC info to a separate column
      dplyr::mutate(date = lubridate::ymd(calendar_date)) %>%
      dplyr::select(pixel, date, band, value) %>%
      tidyr::pivot_wider(values_from = value, names_from = band) %>%

      ## rename to standard
      rename(value = !!settings$band_var, qc = !!settings$band_qc)

    ## Determine scale factor from band info and scale values
    bands <- MODISTools::mt_bands(product = settings$prod) %>%
      as_tibble()
    scale_factor <- bands %>%
      dplyr::filter(band == settings$band_var) %>%
      pull(scale_factor) %>%
      as.numeric()
    df <- df %>%
      mutate(value = scale_factor * value)

    ##--------------------------------------------------------------------
    ## Clean (gapfill and interpolate) full time series data to daily
    ##--------------------------------------------------------------------
    ddf <- gapfill_interpol(
      df,
      sitename,
      year_start      = lubridate::year(df_siteinfo$date_start),
      year_end        = lubridate::year(df_siteinfo$date_end),
      prod            = settings$prod,
      method_interpol = settings$method_interpol,
      keep            = settings$keep
    )

    ##---------------------------------------------
    ## save cleaned and interpolated data to file
    ##---------------------------------------------
    readr::write_csv( ddf, path=filnam_daily_csv )

  }

  ddf <- ddf %>%
    ungroup() %>%
    dplyr::mutate(sitename = sitename)

  return(ddf)
}


gapfill_interpol <- function( df, sitename, year_start, year_end, prod, method_interpol, keep ){
  ##--------------------------------------
  ## Returns data frame containing data
  ## (and year, moy, doy) for all available
  ## months. Interpolated to mid-months
  ## from original 16-daily data.
  ##--------------------------------------

  # require( signal )  ## for sgolayfilt, masks filter()

  ##--------------------------------------
  ## CLEAN AND GAP-FILL
  ##--------------------------------------
  if (prod=="MOD13Q1"){
    ##--------------------------------------
    ## This is for MOD13Q1 Vegetation indeces (NDVI, EVI) data downloaded from MODIS LP DAAC
    ##--------------------------------------
    ## QC interpreted according to https://vip.arizona.edu/documents/MODIS/MODIS_VI_UsersGuide_June_2015_C6.pdf
    df <- df %>%
      dplyr::rename(modisvar = value) %>%
      dplyr::mutate(modisvar_filtered = modisvar) %>%

      ## separate into bits
      rowwise() %>%
      mutate(qc_bitname = intToBits( qc ) %>% as.character() %>% paste(collapse = "")) %>%

      ## Bits 0-1: VI Quality
      ##   00 VI produced with good quality
      ##   01 VI produced, but check other QA
      mutate(vi_quality = substr( qc_bitname, start=1, stop=2 )) %>%

      ## Bits 2-5: VI Usefulness
      ##   0000 Highest quality
      ##   0001 Lower quality
      ##   0010 Decreasing quality
      ##   0100 Decreasing quality
      ##   1000 Decreasing quality
      ##   1001 Decreasing quality
      ##   1010 Decreasing quality
      ##   1100 Lowest quality
      ##   1101 Quality so low that it is not useful
      ##   1110 L1B data faulty
      ##   1111 Not useful for any other reason/not processed
      mutate(vi_useful = substr( qc_bitname, start=3, stop=6 )) %>%
      dplyr::mutate(modisvar_filtered = ifelse(vi_useful %in% c("0000", "0001", "0010", "0100", "1000", "1001", "1010", "1100"), modisvar, NA)) %>%

      ## Bits 6-7: Aerosol Quantity
      ##  00 Climatology
      ##  01 Low
      ##  10 Intermediate
      ##  11 High
      mutate(aerosol = substr( qc_bitname, start=7, stop=8 )) %>%
      dplyr::mutate(modisvar_filtered = ifelse(aerosol %in% c("00", "01", "10"), modisvar, NA)) %>%

      ## Bit 8: Adjacent cloud detected
      ##  0 No
      ##  1 Yes
      mutate(adjcloud = substr( qc_bitname, start=9, stop=9 )) %>%
      dplyr::mutate(modisvar_filtered = ifelse(adjcloud %in% c("0"), modisvar, NA)) %>%

      ## Bits 9: Atmosphere BRDF Correction
      ##   0 No
      ##   1 Yes
      mutate(brdf_corr = substr( qc_bitname, start=10, stop=10 )) %>%
      dplyr::mutate(modisvar_filtered = ifelse(brdf_corr %in% c("1"), modisvar, NA)) %>%

      ## Bits 10: Mixed Clouds
      ##   0 No
      ##   1 Yes
      mutate(mixcloud = substr( qc_bitname, start=11, stop=11 )) %>%
      dplyr::mutate(modisvar_filtered = ifelse(mixcloud %in% c("0"), modisvar, NA)) %>%

      ## Bits 11-13: Land/Water Mask
      ##  000 Shallow ocean
      ##  001 Land (Nothing else but land)
      ##  010 Ocean coastlines and lake shorelines
      ##  011 Shallow inland water
      ##  100 Ephemeral water
      ##  101 Deep inland water
      ##  110 Moderate or continental ocean
      ##  111 Deep ocean
      mutate(mask = substr( qc_bitname, start=12, stop=14 )) %>%

      ## Bits 14: Possible snow/ice
      ##  0 No
      ##  1 Yes
      mutate(snowice = substr( qc_bitname, start=15, stop=15 )) %>%
      dplyr::mutate(modisvar_filtered = ifelse(snowice %in% c("0"), modisvar, NA)) %>%

      ## Bits 15: Possible shadow
      ##  0 No
      ##  1 Yes
      mutate(shadow = substr( qc_bitname, start=16, stop=16 )) %>%
      dplyr::mutate(modisvar_filtered = ifelse(shadow %in% c("0"), modisvar, NA)) %>%

      ## drop it
      dplyr::select(-qc_bitname)


  } else if (prod=="MCD15A3H"){

    ## QC interpreted according to https://explorer.earthengine.google.com/#detail/MODIS%2F006%2FMCD15A3H:

    ## This is interpreted according to https://lpdaac.usgs.gov/documents/2/mod15_user_guide.pdf, p.9
    df <- df %>%

      dplyr::rename(modisvar = value) %>%
      dplyr::mutate(modisvar_filtered = modisvar) %>%

      ## separate into bits
      rowwise() %>%
      mutate(qc_bitname = intToBits( qc )[1:8] %>% rev() %>% as.character() %>% paste(collapse = "")) %>%

      ## MODLAND_QC bits
      ## 0: Good  quality (main algorithm with or without saturation)
      ## 1: Other quality (backup  algorithm or fill values)
      mutate(qc_bit0 = substr( qc_bitname, start=8, stop=8 )) %>%
      mutate(good_quality = ifelse( qc_bit0=="0", TRUE, FALSE )) %>%

      ## Sensor
      ## 0: Terra
      ## 1: Aqua
      mutate(qc_bit1 = substr( qc_bitname, start=7, stop=7 )) %>%
      mutate(terra = ifelse( qc_bit1=="0", TRUE, FALSE )) %>%

      ## Dead detector
      ## 0: Detectors apparently  fine  for up  to  50% of  channels  1,  2
      ## 1: Dead  detectors caused  >50%  adjacent  detector  retrieval
      mutate(qc_bit2 = substr( qc_bitname, start=6, stop=6 )) %>%
      mutate(dead_detector = ifelse( qc_bit2=="1", TRUE, FALSE )) %>%

      ## CloudState
      ## 00 0  Significant clouds  NOT present (clear)
      ## 01 1  Significant clouds  WERE  present
      ## 10 2  Mixed cloud present in  pixel
      ## 11 3  Cloud state not defined,  assumed clear
      mutate(qc_bit3 = substr( qc_bitname, start=4, stop=5 )) %>%
      mutate(CloudState = ifelse( qc_bit3=="00", 0, ifelse( qc_bit3=="01", 1, ifelse( qc_bit3=="10", 2, 3 ) ) )) %>%

      ## SCF_QC (five level confidence score)
      ## 000 0 Main (RT) method used, best result possible (no saturation)
      ## 001 1 Main (RT) method used with saturation. Good, very usable
      ## 010 2 Main (RT) method failed due to bad geometry, empirical algorithm used
      ## 011 3 Main (RT) method failed due to problems other than geometry, empirical algorithm used
      ## 100 4 Pixel not produced at all, value couldn???t be retrieved (possible reasons: bad L1B data, unusable MOD09GA data)
      mutate(qc_bit4 = substr( qc_bitname, start=1, stop=3 )) %>%
      mutate(SCF_QC = ifelse( qc_bit4=="000", 0, ifelse( qc_bit4=="001", 1, ifelse( qc_bit4=="010", 2, ifelse( qc_bit4=="011", 3, 4 ) ) ) )) %>%

      ## Actually do the filtering
      mutate(modisvar_filtered = ifelse( CloudState %in% c(0), modisvar_filtered, NA )) %>%
      mutate(modisvar_filtered = ifelse( good_quality, modisvar_filtered, NA )) %>%

      ## new addition 5.1.2021
      # mutate(modisvar_filtered = ifelse( !dead_detector, modisvar_filtered, NA )) %>%
      mutate(modisvar_filtered = ifelse( SCF_QC %in% c(0,1), modisvar_filtered, NA ))


  } else if (prod=="MOD17A2H"){
    ## Contains MODIS GPP
    ## quality bitmap interpreted based on https://lpdaac.usgs.gov/dataset_discovery/modis/modis_products_table/mod17a2

    ## No filtering here!
    df <- df %>%
      dplyr::rename(modisvar = value) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(modisvar_filtered = modisvar)

  }

  ##--------------------------------------
  ## Create daily dataframe
  ##--------------------------------------
  ddf <- init_dates_dataframe( year_start, year_end ) %>%

      ## decimal date
      mutate(year_dec = lubridate::decimal_date(date))

  ##--------------------------------------
  ## Average across pixels by date
  ##--------------------------------------
  npixels <- df %>% pull(pixel) %>% unique() %>% length()
  rlang::inform(paste("Averaging across number of pixels: ", npixels))

  df <- df %>%
    group_by(date) %>%
    # dplyr::filter(pixel == XXX) %>%    # to control wich pixel's information to be used.
    summarise(modisvar_filtered = mean(modisvar_filtered, na.rm = TRUE),
              modisvar = mean(modisvar, na.rm = TRUE))

  ##--------------------------------------
  ## merge N-day dataframe into daily one.
  ## Warning: here, 'date' must be centered within 4-day period - thus not equal to start date but (start date + 2)
  ##--------------------------------------
  ddf <- ddf %>%
    left_join( df, by="date" )

  # ## extrapolate missing values at head and tail
  # ddf$modisvar <- extrapolate_missing_headtail(dplyr::select(ddf, var = modisvar, doy))

  if (method_interpol == "loess" || keep){
    ##--------------------------------------
    ## get LOESS spline model for predicting daily values (used below)
    ##--------------------------------------
    rlang::inform("loess...")

    ## determine periodicity
    period <- ddf %>%
      filter(!is.na(modisvar_filtered)) %>%
      mutate(prevdate = lag(date)) %>%
      mutate(period = as.integer(difftime(date, prevdate))) %>%
      pull(period) %>%
      min(na.rm = TRUE)

    ## take a three-weeks window for locally weighted regression (loess)
    ## good explanation: https://rafalab.github.io/dsbook/smoothing.html#local-weighted-regression-loess
    ndays_tot <- lubridate::time_length(diff(range(ddf$date)), unit = "day")
    span <- 100/ndays_tot # (20*period)/ndays_tot  # multiply with larger number to get smoother curve

    idxs    <- which(!is.na(ddf$modisvar_filtered))
    myloess <- try( loess( modisvar_filtered ~ year_dec, data = ddf[idxs,], span=span ) )

    ## predict LOESS to all dates with missing data
    tmp <- try( predict( myloess, newdata = ddf ) )
    if (class(tmp)!="try-error"){
      ddf$loess <- tmp
    } else {
      ddf$loess <- rep( NA, nrow(ddf) )
    }
  }

  if (method_interpol == "spline" || keep){
    ##--------------------------------------
    ## get SPLINE model for predicting daily values (used below)
    ##--------------------------------------
    rlang::inform("spline...")
    idxs   <- which(!is.na(ddf$modisvar_filtered))
    spline <- try( with( ddf, smooth.spline( year_dec[idxs], modisvar_filtered[idxs], spar=0.01 ) ) )

    ## predict SPLINE
    tmp <- try( with( ddf, predict( spline, year_dec ) )$y)
    if (class(tmp)!="try-error"){
      ddf$spline <- tmp
    } else {
      ddf$spline <- rep( NA, nrow(ddf) )
    }

  }

  if (method_interpol == "linear" || keep){
    ##--------------------------------------
    ## LINEAR INTERPOLATION
    ##--------------------------------------
    rlang::inform("linear ...")
    ddf$linear <- approx( ddf$year_dec, ddf$modisvar_filtered, xout=ddf$year_dec )$y
  }

  ## commented out to avoid dependency to 'signal'
  # if (method_interpol == "sgfilter" || keep){
  #   ##--------------------------------------
  #   ## SAVITZKY GOLAY FILTER
  #   ##--------------------------------------
  #   rlang::inform("sgfilter ...")
  #   ddf$sgfilter <- rep( NA, nrow(ddf) )
  #   idxs <- which(!is.na(ddf$modisvar_filtered))
  #   tmp <- try(signal::sgolayfilt( ddf$modisvar_filtered[idxs], p=3, n=51 ))
  #   if (class(tmp)!="try-error"){
  #     ddf$sgfilter[idxs] <- tmp
  #   }

  # }

  ##--------------------------------------
  ## Define 'fapar'
  ##--------------------------------------
  if (method_interpol == "loess"){
    ddf$modisvar_filled <- ddf$loess
  } else if (method_interpol == "spline"){
    ddf$modisvar_filled <- ddf$spline
  } else if (method_interpol == "linear"){
    ddf$modisvar_filled <- ddf$linear
  }

  # else if (method_interpol == "sgfilter"){
  #   ddf$modisvar_filled <- ddf$sgfilter
  # }

  # ## plot daily smoothed line and close plotting device
  # if (do_plot_interpolated) with( ddf, lines( year_dec, fapar, col='red', lwd=2 ) )
  # if (do_plot_interpolated) with( ddf, lines( year_dec, sgfilter, col='springgreen3', lwd=1 ) )
  # if (do_plot_interpolated) with( ddf, lines( year_dec, spline, col='cyan', lwd=1 ) )
  # if (do_plot_interpolated){
  #   legend( "topright",
  #           c("Savitzky-Golay filter", "Spline", "Linear interpolation (standard)"),
  #           col=c("springgreen3", "cyan", "red" ), lty=1, lwd=c(1,1,2), bty="n", inset = c(0,-0.2)
  #   )
  # }

  ## limit to within 0 and 1 (loess spline sometimes "explodes")
  ddf <- ddf %>%
    dplyr::mutate( modisvar_filled = replace( modisvar_filled, modisvar_filled<0, 0  ) ) %>%
    dplyr::mutate( modisvar_filled = replace( modisvar_filled, modisvar_filled>1, 1  ) )

  ## extrapolate missing values at head and tail again
  ##--------------------------------------
  ddf$modisvar_filled <- extrapolate_missing_headtail(dplyr::select(ddf, var = modisvar_filled))

  return( ddf )

}

extrapolate_missing_headtail <- function(ddf){
  ## extrapolate to missing values at head and tail using mean seasonal cycle
  ##--------------------------------------

  ## new: fill gaps at head
  idxs <- findna_head( ddf$var )
  if (length(idxs)>0) rlang::warn("Filling values with last available data point at head")
  ddf$var[idxs] <- ddf$var[max(idxs)+1]

  ## new: fill gaps at tail
  idxs <- findna_tail( ddf$var )
  if (length(idxs)>0) rlang::warn("Filling values with last available data point at tail.")
  ddf$var[idxs] <- ddf$var[min(idxs)-1]

  # ## get mean seasonal cycle
  # ddf_meandoy <- ddf %>%
  #   dplyr::group_by( doy ) %>%
  #   dplyr::summarise( meandoy = mean( var , na.rm=TRUE ) )
  #
  # ## attach mean seasonal cycle as column 'meandoy' to daily dataframe
  # ddf <- ddf %>%
  #   dplyr::left_join( ddf_meandoy, by="doy" )
  #
  # ## fill gaps at head and tail
  # ddf$var[ idxs ] <- ddf$meandoy[ idxs ]

  vec <- ddf %>%
    dplyr::pull(var)

  return(vec)
}


findna_headtail <- function( vec ){

  ## Remove (cut) NAs from the head and tail of a vector.
  ## Returns the indexes to be dropped from a vector

  idxs <- c(findna_head(vec), findna_tail(vec))

  return(idxs)

}

findna_head <- function( vec ){

  ## Remove (cut) NAs from the head and tail of a vector.
  ## Returns the indexes to be dropped from a vector

  ## Get indeces of consecutive NAs at head
  if (is.na(vec[1])){
    idx <- 0
    cuthead <- 1
    while ( idx < length(vec) ){
      idx <- idx + 1
      test <- head( vec, idx )
      if (any(!is.na(test))){
        ## first non-NA found at position idx
        cuthead <- idx - 1
        break
      }
    }
    idxs_head <- 1:cuthead
  } else {
    idxs_head <- c()
  }

  return(idxs_head)

}

findna_tail <- function( vec ){

  ## Remove (cut) NAs from the head and tail of a vector.
  ## Returns the indexes to be dropped from a vector

  ## Get indeces of consecutive NAs at tail
  if (is.na(vec[length(vec)])){
    idx <- 0
    cuttail <- 1
    while ( idx < length(vec) ){
      idx <- idx + 1
      test <- tail( vec, idx )
      if (any(!is.na(test))){
        ## first non-NA found at position idx, counting from tail
        cuttail <- idx - 1
        break
      }
    }
    idxs_tail <- (length(vec)-cuttail+1):length(vec)
  } else {
    idxs_tail <- c()
  }

  return(idxs_tail)

}

na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }

## some old code:

##--------------------------------------
## This is for data downloaded using RModisTools
##--------------------------------------

# ##--------------------------------------
# ## data cleaning
# ##--------------------------------------
# ## Replace data points with quality flag = 2 (snow covered) by 0
# # df_gapfld$centre[ which(df_gapfld$centre_qc==2) ] <- max( min( df_gapfld$centre ), 0.0 )
# df_gapfld$raw <- df_gapfld$centre
# df_gapfld$centre[ which(df_gapfld$centre<0) ] <- NA

# if (!is.null(df_gapfld$centre_qc)){
#   ## Drop all data with quality flag 3, 1 or -1
#   df_gapfld$centre[ which(df_gapfld$centre_qc==3) ]  <- NA  # Target not visible, covered with cloud
#   df_gapfld$centre[ which(df_gapfld$centre_qc==1) ]  <- NA  # Useful, but look at other QA information
#   df_gapfld$centre[ which(df_gapfld$centre_qc==-1) ] <- NA  # Not Processed
# }

# ## open plot for illustrating gap-filling
# if (do_plot_interpolated) pdf( paste("fig/evi_fill_", sitename, ".pdf", sep="" ), width=10, height=6 )
# if (do_plot_interpolated) plot( df_gapfld$year_dec, df_gapfld$raw, pch=16, col='black', main=sitename, ylim=c(0,1), xlab="year", ylab="MODIS EVI 250 m", las=1 )
# left <- seq(2000, 2016, 2)
# right <- seq(2001, 2017, 2)
# if (do_plot_interpolated) rect( left, -99, right, 99, border=NA, col=rgb(0,0,0,0.2) )
# if (do_plot_interpolated) points( df_gapfld$year_dec, df_gapfld$centre, pch=16, col='red' )

# # ## Drop all data identified as outliers = lie outside 5*IQR
# # df_gapfld$centre <- remove_outliers( df_gapfld$centre, coef=5 ) ## maybe too dangerous - removes peaks

# ## add points to plot opened before
# if (do_plot_interpolated) points( df_gapfld$year_dec, df_gapfld$centre, pch=16, col='blue' )

# ##--------------------------------------
# ## get LOESS spline model for predicting daily values (below)
# ##--------------------------------------
# idxs    <- which(!is.na(df_gapfld$centre))
# myloess <- try( with( df_gapfld, loess( centre[idxs] ~ year_dec[idxs], span=0.01 ) ))
# i <- 0
# while (class(myloess)=="try-error" && i<50){
#   i <- i + 1
#   print(paste("i=",i))
#   myloess <- try( with( df_gapfld, loess( centre[idxs] ~ year_dec[idxs], span=(0.01+0.002*(i-1)) ) ))
# }
# print("ok now...")

# ##--------------------------------------
# ## get spline model for predicting daily values (below)
# ##--------------------------------------
# spline <- try( with( df_gapfld, smooth.spline( year_dec[idxs], centre[idxs], spar=0.001 ) ) )

# ## aggregate by DOY
# agg <- aggregate( centre ~ doy, data=df_gapfld, FUN=mean, na.rm=TRUE )
# if (is.element("centre_meansurr", names(df_gapfld))){
#   agg_meansurr <- aggregate( centre_meansurr ~ doy, data=df_gapfld, FUN=mean, na.rm=TRUE )
#   agg <- agg %>% left_join( agg_meansurr ) %>% dplyr::rename( centre_meandoy=centre, centre_meansurr_meandoy=centre_meansurr )
# } else {
#   agg <- agg %>% dplyr::rename( centre_meandoy=centre )
# }
# df_gapfld <- df_gapfld %>% left_join( agg )

# ##--------------------------------------
# ## gap-fill with information from surrounding pixels - XXX CHANGED THIS FOR AMERIWUE: NO INFO FROM MEAN OF SURROUNDINGS USED XXX
# ##--------------------------------------
# idxs <- which( is.na(df_gapfld$centre) )
# if (is.element("centre_meansurr", names(df_gapfld))){
#   ## get current anomaly of mean across surrounding pixels w.r.t. its mean annual cycle
#   df_gapfld$anom_surr    <- df_gapfld$centre_meansurr / df_gapfld$centre_meansurr_meandoy
#   # df_gapfld$centre[idxs] <- df_gapfld$centre_meandoy[idxs] * df_gapfld$anom_surr[idxs]
# } else {
#   # df_gapfld$centre[idxs] <- df_gapfld$centre_meandoy[idxs]
# }
# if (do_plot_interpolated) with( df_gapfld, points( year_dec[idxs], centre[idxs], pch=16, col='green' ) )
# if (do_plot_interpolated) legend("topright", c("modis", "outliers", "after bad values dropped and outliers removed", "added from mean of surrounding" ), col=c("black", "red", "blue", "green" ), pch=16, bty="n" )
# # legend("topleft", c("R LOESS smoothing with span=0.01", "R smooth.spline"), col=c("red", "dodgerblue"), lty=1, bty="n" )


# # points( df_gapfld$yr_dec_read[idxs],  df_gapfld$centre[idxs],  pch=16 )
# # points( df_gapfld$yr_dec_read[-idxs], df_gapfld$centre[-idxs], pch=16, col='blue' )

# # ## Gap-fill remaining again by mean-by-DOY
# # idxs <- which( is.na(df_gapfld$centre) )
# # df_gapfld$centre[idxs] <- df_gapfld$centre_meandoy[idxs]
# # # points( df_gapfld$yr_dec_read[idxs], df_gapfld$centre[idxs], pch=16, col='red' )

# # ## Gap-fill still remaining by linear approximation
# # idxs <- which( is.na(df_gapfld$centre) )
# # if (length(idxs)>1){
# #   df_gapfld$centre <- approx( df_gapfld$year_dec[-idxs], df_gapfld$centre[-idxs], xout=df_gapfld$year_dec )$y
# # }

# # points( df_gapfld$yr_dec_read[idxs], df_gapfld$centre[idxs], pch=16, col='green' )
# # lines( df_gapfld$yr_dec_read, df_gapfld$centre )
# # dev.off()
