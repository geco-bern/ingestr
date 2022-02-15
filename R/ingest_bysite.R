#' Data ingest for a single site
#'
#' Ingests data for a single site and one specific data type, specified by argument \code{source}.
#'
#' @param sitename A character string used as site identification. When data is extracted from
#' global files or remote servers, \code{sitename} is simply used as a label and any string can
#' be provided. When data is extraced from site-specific files (e.g. \code{source = "fluxnet"}),
#' then \code{sitename} is used to identify the file from which data is read.
#' @param source A character used as identifiyer for the type of data source
#' (e.g., \code{"fluxnet"}). See vignette for a full description of available options.
#' @param getvars A named list of characters specifying the variable names in
#' the original source dataset and the variable names in the ingested data frame. Use, e.g.,
#' \code{getvars = list("gpp" = "GPP_NT_VUT_REF")} to read the variable \code{"GPP_NT_VUT_REF"}
#' from the original file and convert its name to \code{"gpp"} in the ingested data frame.
#' @param dir A character specifying the directory where the data is located.
#' @param settings A list of additional, source-specific settings used for reading and processing
#' original files. Defaults to an empty list which triggers the use of default settings (see
#' e.g., \link{get_settings_fluxnet}) for \code{source = "fluxnet"}.
#' @param timescale A character or vector of characters, specifying the time scale of data used from
#' the respective source (if multiple time scales are available, otherwise is disregarded). Implemented
#' time scales are \code{c("d", "m", "y")} for daily, monthly, and yearly, respectively. Defaults
#' to \code{"d"}.
#' @param year_start An integer specifying the first year for which data is to be ingested.
#' @param year_end An integer specifying the last year for which data is to be ingested (full years
#' are read, i.e. all days, or hours, or months in each year).
#' @param lon A numeric value specifying the longitude for which data is extraced from global files
#' or remote data servers. If \code{source = "fluxnet"}, this is not required and set ot \code{NA}.
#' @param lat A numeric value specifying the longitude for which data is extraced from global files
#' or remote data servers. If \code{source = "fluxnet"}, this is not required and set ot \code{NA}.
#' @param elv A numeric value specifying the elevation of the site in m a.s.l., This is only required
#' for \code{source = "watch_wfdei"}, where the ingested data for atmospheric pressure (\code{patm}) 
#' is bias-corrected by elevation using  the adiabatic lapse rate (implemented by \link{calc_patm}).
#' @param verbose if \code{TRUE}, additional messages are printed. Defaults to \code{FALSE}.
#'
#' @return A data frame (tibble) containing the time series of ingested data.
#' @export
#'
#' @examples \dontrun{inputdata <- ingest_bysite()}
#'
ingest_bysite <- function(
  sitename,
  source,
  getvars,
  dir = NULL,
  settings  = NULL,
  timescale = "d",
  year_start = NA,
  year_end = NA,
  lon = ifelse(source=="fluxnet", NA),
  lat = ifelse(source=="fluxnet", NA),
  elv = NA,
  verbose = FALSE
  ){

  # CRAN compliance, declaring unstated variables
  lon <- lat <- date_start <- date_end <- problem <-
    year_start_tmp <- x <- y <- lat_orig <- success <- elv <- patm <-
    patm_base <-patm_mean <- month <- tavg <-temp <- temp_fine <-
    tmax <- tmax_fine <- tmin <- tmin_fine <- prec <- prec_fine <-
    days_in_month <- rain <- snow <- srad <- srad_fine <- ppfd <-
    ppfd_fine <- wind <- wind_fine <- qair <- vap <- vapr <- vapr_fine <-
    ilon <- data <- yy <- mm <- co2_avg <- year <- . <- bias <-
    co2 <- lon...1 <- lat...2 <- bottom <- top <- depth <- var <-
    var_wgt <- depth_tot_cm <- NULL
  
  if (!(source %in% c("etopo1", "hwsd", "soilgrids", "wise", "gsde", "worldclim"))){
    
    ## initialise data frame with all required dates
    df <- init_dates_dataframe(
      year_start,
      year_end,
      noleap = TRUE,
      timescale = timescale
      )
      # dplyr::select(-year_dec)

    if (timescale=="m"){
      df <- df %>%
        mutate(month = lubridate::month(date), year = lubridate::year(date))
    } else if (timescale=="y"){
      df <- df %>%
        mutate(year = lubridate::year(date))
    }
  }

  ##-----------------------------------------------------------
  ## FLUXNET 2015 reading
  ##-----------------------------------------------------------
  if (source == "fluxnet"){

    ## complement un-specified settings with default
    settings_default <- get_settings_fluxnet()
    fill_settings_with_default <- function(element, settings, default){
      if (is.null(settings[[element]])) settings[[element]] <- default[[element]]
      return(settings)
    }
    for (element in names(settings_default)){
      settings <- fill_settings_with_default(element, settings, settings_default)
    }

    df_tmp <- get_obs_bysite_fluxnet(sitename,
                                     dir             = dir,
                                     dir_hh          = settings$dir_hh,
                                     dir_hr          = settings$dir_hr,
                                     timescale       = timescale,
                                     getvars         = getvars,
                                     getswc          = settings$getswc,
                                     threshold_GPP   = settings$threshold_GPP,
                                     threshold_LE    = settings$threshold_LE,
                                     threshold_H     = settings$threshold_H,
                                     threshold_SWC   = settings$threshold_SWC,
                                     threshold_WS    = settings$threshold_WS,
                                     threshold_USTAR = settings$threshold_USTAR,
                                     threshold_T     = settings$threshold_T,
                                     threshold_NETRAD= settings$threshold_NETRAD,
                                     filter_ntdt     = settings$filter_ntdt,
                                     return_qc       = settings$return_qc,
                                     remove_neg      = settings$remove_neg,
                                     verbose         = verbose
                                    ) %>%
      mutate(sitename = sitename)

  } else if (source == "cru" || source == "watch_wfdei" || source == "ndep" || source == "wfde5"){
    #-----------------------------------------------------------
    # Get data from global fields and one single site
    #-----------------------------------------------------------
    siteinfo <- tibble(
        sitename = sitename,
        lon = lon,
        lat = lat) %>%
      mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>%
      mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31")))
    
    if (!identical(NULL, settings$correct_bias)){
      if (settings$correct_bias == "worldclim"){
        
        ## save data frame with required dates
        ddf_dates <- purrr::map(
          as.list(seq(nrow(siteinfo))),
          ~init_dates_dataframe(
            lubridate::year(siteinfo$date_start[.]),
            lubridate::year(siteinfo$date_end[.]),
            noleap = TRUE,
            timescale = timescale))
        names(ddf_dates) <- siteinfo$sitename
        ddf_dates <- ddf_dates %>%
          bind_rows(.id = "sitename")
        
        year_start_wc <- 1970
        year_end_wc <- 2000
        
        if (source == "watch_wfdei"){
          message("Beware: WorldClim data is for years 1970-2000. Therefore WATCH_WFDEI data is ingested for 1979-(at least) 2000.")
          year_start_wc <- 1979  # no earlier years available
          siteinfo <- siteinfo %>% 
            mutate(year_start = ifelse(year_start < year_start_wc, year_start, year_start_wc),
                   year_end = ifelse(year_end > year_end_wc, year_end, year_end_wc))
        } else if (source == "wfde5"){
          rlang::inform("Beware: WorldClim data is for years 1970-2000. Therefore WFDE5 data is ingested for 1979-(at least) 2000.")
          year_start_wc <- 1979  # no earlier years available
          siteinfo <- siteinfo %>% 
            mutate(year_start = ifelse(year_start < year_start_wc, year_start, year_start_wc),
                   year_end = ifelse(year_end > year_end_wc, year_end, year_end_wc))
        } else if (source == "cru"){
          siteinfo <- siteinfo %>% 
            mutate(year_start = ifelse(year_start < year_start_wc, year_start, year_start_wc),
                   year_end   = ifelse(year_end > year_end_wc, year_end, year_end_wc))
        }
      }
    }

    ## this returns a flat data frame with data from all sites
    df_tmp <- ingest_globalfields(siteinfo  = siteinfo,
                                  source    = source,
                                  getvars   = getvars,
                                  dir       = dir,
                                  timescale = timescale,
                                  verbose   = FALSE
                                  )
    
    ## bias-correct atmospheric pressure - per default
    if ("patm" %in% getvars){
      if (is.na(elv)){
        stop("Aborting. Argument elv is missing.")
      }
      patm_mean_watch <- df_tmp %>% 
        summarise(patm = mean(patm, na.rm = TRUE)) %>% 
        pull(patm)
      scale <- calc_patm(elv) / patm_mean_watch
      df_tmp <- df_tmp %>% 
        mutate(patm = patm * scale)
    }
    
    if (!identical(NULL, settings$correct_bias)){
      if (settings$correct_bias == "worldclim"){
        #-----------------------------------------------------------
        # Bias correction using WorldClim data
        #-----------------------------------------------------------
        getvars_wc <- c()
        if ("temp" %in% getvars){getvars_wc <- c(getvars_wc, "tavg")}
        if ("tmin" %in% getvars){getvars_wc <- c(getvars_wc, "tmin")}
        if ("tmax" %in% getvars){getvars_wc <- c(getvars_wc, "tmax")}
        if ("prec" %in% getvars){getvars_wc <- c(getvars_wc, "prec")}
        if ("ppfd" %in% getvars){getvars_wc <- c(getvars_wc, "srad")}
        if ("wind" %in% getvars){getvars_wc <- c(getvars_wc, "wind")}
        if ("vpd"  %in% getvars){getvars_wc <- c(getvars_wc, "vapr", "tmin", "tmax")}
        if ("swin" %in% getvars){rlang::inform("Bias Correction: Not yet implemented for swin.")}
        if ("lwin" %in% getvars){rlang::inform("Bias Correction: Not yet implemented for lwin.")}
        
        df_fine <- ingest_globalfields(siteinfo,
                                       source = "worldclim",
                                       dir = settings$dir_bias,
                                       getvars = NULL,
                                       timescale = NULL,
                                       verbose = FALSE,
                                       layer = unique(getvars_wc)
        )
        
        ## Bias correction for temperature: subtract difference
        if ("tavg" %in% getvars_wc){
          df_bias <- df_fine %>% 
            tidyr::pivot_longer(cols = starts_with("tavg_"), names_to = "month", values_to = "tavg", names_prefix = "tavg_") %>% 
            mutate(month = as.integer(month)) %>% 
            rename(temp_fine = tavg) %>% 
            right_join(df_tmp %>% 
                         dplyr::filter(lubridate::year(date) %in% year_start_wc:year_end_wc) %>% 
                         mutate(month = lubridate::month(date)) %>% 
                         group_by(month) %>% 
                         summarise(temp = mean(temp, na.rm = TRUE)),
                       by = "month") %>% 
            mutate(bias = temp - temp_fine) %>% 
            dplyr::select(-temp, -temp_fine, -sitename)
          
          ## correct bias by month
          df_tmp <- df_tmp %>% 
            mutate(month = lubridate::month(date)) %>% 
            left_join(df_bias %>% dplyr::select(month, bias), by = "month") %>% 
            mutate(temp = ifelse(!(is.na(bias)), temp - bias, temp)) %>% 
            dplyr::select(-bias, -month)
        }

        ## Bias correction for minimum temperature: subtract difference
        if ("tmin" %in% getvars_wc){
          df_bias <- df_fine %>% 
            tidyr::pivot_longer(cols = starts_with("tmin_"), names_to = "month", values_to = "tmin", names_prefix = "tmin_") %>% 
            mutate(month = as.integer(month)) %>% 
            rename(tmin_fine = tmin) %>% 
            right_join(df_tmp %>% 
                         dplyr::filter(lubridate::year(date) %in% year_start_wc:year_end_wc) %>% 
                         mutate(month = lubridate::month(date)) %>% 
                         group_by(month) %>% 
                         summarise(tmin = mean(tmin, na.rm = TRUE)),
                       by = "month") %>% 
            mutate(bias = tmin - tmin_fine) %>% 
            dplyr::select(-tmin, -tmin_fine, -sitename)
          
          ## correct bias by month
          df_tmp <- df_tmp %>% 
            mutate(month = lubridate::month(date)) %>% 
            left_join(df_bias %>% dplyr::select(month, bias), by = "month") %>% 
            mutate(tmin = ifelse(!(is.na(bias)), tmin - bias, tmin)) %>% 
            dplyr::select(-bias, -month)
        }
        
        ## Bias correction for temperature: subtract difference
        if ("tmax" %in% getvars_wc){
          df_bias <- df_fine %>% 
            tidyr::pivot_longer(cols = starts_with("tmax_"), names_to = "month", values_to = "tmax", names_prefix = "tmax_") %>% 
            mutate(month = as.integer(month)) %>% 
            rename(tmax_fine = tmax) %>% 
            right_join(df_tmp %>% 
                         dplyr::filter(lubridate::year(date) %in% year_start_wc:year_end_wc) %>% 
                         mutate(month = lubridate::month(date)) %>% 
                         group_by(month) %>% 
                         summarise(tmax = mean(tmax, na.rm = TRUE)),
                       by = "month") %>% 
            mutate(bias = tmax - tmax_fine) %>% 
            dplyr::select(-tmax, -tmax_fine, -sitename)
          
          ## correct bias by month
          df_tmp <- df_tmp %>% 
            mutate(month = lubridate::month(date)) %>% 
            left_join(df_bias %>% dplyr::select(month, bias), by = "month") %>% 
            mutate(tmax = ifelse(!(is.na(bias)), tmax - bias, tmax)) %>% 
            dplyr::select(-bias, -month)
        }
        
        ## Bias correction for precipitation: scale by ratio (snow and rain equally)
        if ("prec" %in% getvars_wc){
          df_bias <- df_fine %>% 
            tidyr::pivot_longer(cols = starts_with("prec_"), names_to = "month", values_to = "prec", names_prefix = "prec_") %>% 
            mutate(month = as.integer(month)) %>% 
            rename(prec_fine = prec) %>% 
            mutate(prec_fine = prec_fine / days_in_month(month)) %>%   # mm/month -> mm/d
            mutate(prec_fine = prec_fine / (60 * 60 * 24)) %>%         # mm/d -> mm/sec
            right_join(df_tmp %>% 
                         dplyr::filter(lubridate::year(date) %in% year_start_wc:year_end_wc) %>% 
                         mutate(month = lubridate::month(date)) %>% 
                         group_by(month) %>% 
                         summarise(prec = mean(prec, na.rm = TRUE)),
                       by = "month") %>% 
            mutate(scale = prec_fine / prec) %>% 
            dplyr::select(-prec, -prec_fine, -sitename)
          
          ## correct bias by month
          if (source == "watch_wfdei" || source == "wfde5"){
            ## scaling also snow and rain rates
            df_tmp <- df_tmp %>% 
              mutate(month = lubridate::month(date)) %>% 
              left_join(df_bias %>% dplyr::select(month, scale), by = "month") %>% 
              mutate(prec = ifelse(is.na(scale), prec, prec * scale),
                     rain = ifelse(is.na(scale), rain, rain * scale),
                     snow = ifelse(is.na(scale), snow, snow * scale)) %>% 
              dplyr::select(-scale, -month)
          } else {
            df_tmp <- df_tmp %>% 
              mutate(month = lubridate::month(date)) %>% 
              left_join(df_bias %>% dplyr::select(month, scale), by = "month") %>% 
              mutate(prec = ifelse(is.na(scale), prec, prec * scale)) %>% 
              dplyr::select(-scale, -month)
          }
        }
        
        ## Bias correction for shortwave radiation: scale by ratio
        if ("srad" %in% getvars_wc){
          kfFEC <- 2.04
          df_bias <- df_fine %>% 
            tidyr::pivot_longer(cols = starts_with("srad_"), names_to = "month", values_to = "srad", names_prefix = "srad_") %>% 
            mutate(month = as.integer(month)) %>% 
            rename(srad_fine = srad) %>% 
            mutate(ppfd_fine = 1e3 * srad_fine * kfFEC * 1.0e-6 / (60 * 60 * 24) ) %>%   # kJ m-2 day-1 -> mol m−2 s−1 PAR
            right_join(df_tmp %>% 
                         dplyr::filter(lubridate::year(date) %in% year_start_wc:year_end_wc) %>% 
                         mutate(month = lubridate::month(date)) %>% 
                         group_by(month) %>% 
                         summarise(ppfd = mean(ppfd, na.rm = TRUE)),
                       by = "month") %>% 
            mutate(scale = ppfd_fine / ppfd) %>% 
            dplyr::select(-srad_fine, -ppfd_fine, -ppfd, -sitename)
          
          ## correct bias by month
          df_tmp <- df_tmp %>% 
            mutate(month = lubridate::month(date)) %>% 
            left_join(df_bias %>% dplyr::select(month, scale), by = "month") %>% 
            mutate(ppfd = ifelse(is.na(scale), ppfd, ppfd * scale)) %>% 
            dplyr::select(-scale, -month)
        }
        
        ## Bias correction for atmospheric pressure: scale by ratio
        if ("wind" %in% getvars_wc){
          df_bias <- df_fine %>% 
            tidyr::pivot_longer(cols = starts_with("wind_"), names_to = "month", values_to = "wind", names_prefix = "wind_") %>% 
            mutate(month = as.integer(month)) %>% 
            rename(wind_fine = wind) %>% 
            right_join(df_tmp %>% 
                         dplyr::filter(lubridate::year(date) %in% year_start_wc:year_end_wc) %>% 
                         mutate(month = lubridate::month(date)) %>% 
                         group_by(month) %>% 
                         summarise(wind = mean(wind, na.rm = TRUE)),
                       by = "month") %>% 
            mutate(scale = wind_fine / wind) %>% 
            dplyr::select(-wind_fine, -wind, -sitename)
          
          ## correct bias by month
          df_tmp <- df_tmp %>% 
            mutate(month = lubridate::month(date)) %>% 
            left_join(df_bias %>% dplyr::select(month, scale), by = "month") %>% 
            mutate(wind = ifelse(is.na(scale), wind, wind * scale)) %>% 
            dplyr::select(-scale, -month)
        }
        
        ## Bias correction for relative humidity (actually vapour pressure): scale
        if ("vapr" %in% getvars_wc){
          
          ## calculate vapour pressure from specific humidity - needed for bias correction with worldclim data
          if (source == "watch_wfdei"){
            ## specific humidity (qair, g g-1) is read, convert to vapour pressure (vapr, Pa)
            df_tmp <- df_tmp %>% 
              rowwise() %>% 
              dplyr::mutate(vapr = calc_vp(qair = qair, tc = temp, patm = patm)) %>% 
              ungroup()
            
          } else if (source == "cru"){
            ## vapour pressure is read from file, convert from hPa to Pa
            df_tmp <- df_tmp %>% 
              dplyr::mutate(vapr = 1e2 * vap) %>% 
              dplyr::select(-vap)
            
          }
          
          df_bias <- df_fine %>% 
            tidyr::pivot_longer(cols = starts_with("vapr_"), names_to = "month", values_to = "vapr", names_prefix = "vapr_") %>% 
            mutate(month = as.integer(month)) %>% 
            rename(vapr_fine = vapr) %>% 
            mutate(vapr_fine = vapr_fine * 1e3) %>%   # kPa -> Pa
            right_join(df_tmp %>% 
                         dplyr::filter(lubridate::year(date) %in% year_start_wc:year_end_wc) %>% 
                         mutate(month = lubridate::month(date)) %>% 
                         group_by(month) %>% 
                         summarise(vapr = mean(vapr, na.rm = TRUE)),
                       by = "month") %>% 
            mutate(scale = vapr_fine / vapr) %>% 
            dplyr::select(month, scale)
          
          ## correct bias by month
          df_tmp <- df_tmp %>% 
            mutate(month = lubridate::month(date)) %>% 
            left_join(df_bias %>% dplyr::select(month, scale), by = "month") %>% 
            mutate(vapr = ifelse(is.na(scale), vapr, vapr * scale)) %>% 
            dplyr::select(-scale, -month)
        }      
        
        
        ## Calculate vapour pressure deficit from specific humidity
        if ("vpd" %in% getvars){
          
          if (source == "watch_wfdei" || source == "wfde5"){
            ## use daily mean temperature
            df_tmp <- df_tmp %>%
              rowwise() %>%
              dplyr::mutate(vpd = calc_vpd(eact = vapr, tc = temp)) %>% 
              ungroup()
            
          } else if (source == "cru"){
            ## use daily minimum and maximum temperatures
            df_tmp <- df_tmp %>%
              rowwise() %>%
              dplyr::mutate(vpd = calc_vpd(eact = vapr, tmin = tmin, tmax = tmax)) %>% 
              ungroup()
          }
          
        }
        
        ## keep only required dates
        df_tmp <- df_tmp %>% 
          right_join(ddf_dates, by = c("sitename", "date"))
        
      }
      
    } else {

      # Calculate vapour pressure deficit from specific humidity
      # this calculates this variable for cases where there is
      # no bias correction
      
      if ("vpd" %in% getvars){
        
        # xxxxxxx
        if (!("vapr" %in% names(df_tmp))){
          ## calculate vapour pressure from specific humidity - needed for bias correction with worldclim data
          if (source == "watch_wfdei"){
            ## specific humidity (qair, g g-1) is read, convert to vapour pressure (vapr, Pa)
            df_tmp <- df_tmp %>% 
              rowwise() %>% 
              dplyr::mutate(vapr = calc_vp(qair = qair, tc = temp, patm = patm)) %>% 
              ungroup()
            
          } else if (source == "cru"){
            ## vapour pressure is read from file, convert from hPa to Pa
            df_tmp <- df_tmp %>% 
              dplyr::mutate(vapr = 1e2 * vap) %>% 
              dplyr::select(-vap)
            
          }
        }

        if (source == "watch_wfdei"){
          ## use daily mean temperature
          df_tmp <- df_tmp %>%
            rowwise() %>%
            dplyr::mutate(vpd = calc_vpd(eact = vapr, tc = temp)) %>% 
            ungroup()
          
        } else if (source == "cru"){
          ## use daily minimum and maximum temperatures
          df_tmp <- df_tmp %>%
            rowwise() %>%
            dplyr::mutate(vpd = calc_vpd(eact = vapr, tmin = tmin, tmax = tmax)) %>% 
            ungroup()
        }
      }
    }
    
  } else if (source == "modis"){
    
    if (!is.na(settings$network)){
      lon = NA
      lat = NA
    }

    siteinfo <- tibble(
      sitename = sitename,
      lon = lon,
      lat = lat) %>%
      mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>%
      mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31")))

    df_tmp <- ingest_modis_bysite(siteinfo, settings)


  } else if (source == "gee"){
    #-----------------------------------------------------------
    # Get data from Google Earth Engine
    #-----------------------------------------------------------
    siteinfo <- tibble(
      sitename = sitename,
      lon = lon,
      lat = lat) %>%
      mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>%
      mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31")))

    df_tmp <- ingest_gee_bysite(
      siteinfo,
      start_date           = paste0(year_start, "-01-01"),
      end_date             = paste0(year_end, "-12-31"),
      overwrite_raw        = settings$overwrite_raw,
      overwrite_interpol   = settings$overwrite_interpol,
      band_var             = settings$band_var,
      band_qc              = settings$band_qc,
      prod                 = settings$prod,
      prod_suffix          = settings$prod_suffix,
      varnam               = settings$varnam,
      productnam           = settings$productnam,
      scale_factor         = settings$scale_factor,
      period               = settings$period,
      python_path          = settings$python_path,
      gee_path             = settings$gee_path,
      data_path            = settings$data_path,
      method_interpol      = settings$method_interpol,
      keep                 = settings$keep
    )

  } else if (source == "co2_mlo"){
    #-----------------------------------------------------------
    # Get CO2 data year, independent of site
    #-----------------------------------------------------------
    ## if 'dir' is provided, try reading from existing file, otherwise download
    path <- paste0(dir, "/df_co2_mlo.csv")
    if (!identical(NULL, dir)){
      if (file.exists(path)){
        df_co2 <- readr::read_csv(path)
      } else {
          dplyr::select(year = yy, month = mm, co2_avg)
        readr::write_csv(df_co2, file = path)        
      }
    } else {
      df_co2 <- climate::meteo_noaa_co2() %>%
        dplyr::select(year = yy, month = mm, co2_avg)
    }
    
    df_tmp <- init_dates_dataframe( year_start, year_end, timescale = timescale ) %>%
      dplyr::mutate(month = lubridate::month(date), year = lubridate::year(date)) %>%
      dplyr::left_join(
        df_co2,
        by = c("year", "month")
      ) %>%
      dplyr::mutate(sitename = sitename) %>%
      dplyr::select(sitename, date, co2 = co2_avg)


  } else if (source == "co2_cmip"){
    #-----------------------------------------------------------
    # Get CO2 data year, independent of site
    #-----------------------------------------------------------
    ## if 'dir' is provided, try reading from existing file, otherwise download
    path <- paste0(dir, "/cCO2_rcp85_const850-1765.csv")
    if (file.exists(path)){
      df_co2 <- readr::read_csv(path)
    } else {
      stop("File cCO2_rcp85_const850-1765.csv must be available in directory specified by 'dir'.")     
    }
    
    df_tmp <- init_dates_dataframe( year_start, year_end, timescale = timescale ) %>%
      dplyr::mutate(month = lubridate::month(date), year = lubridate::year(date)) %>%
      dplyr::left_join(
        df_co2,
        by = c("year")
      ) %>%
      dplyr::mutate(sitename = sitename) %>%
      dplyr::select(sitename, date, co2)
    
    
  } else if (source == "fapar_unity"){
    #-----------------------------------------------------------
    # Assume fapar = 1 for all dates
    #-----------------------------------------------------------
    df_tmp <- init_dates_dataframe( year_start, year_end, timescale = timescale ) %>%
      dplyr::mutate(sitename = sitename, fapar = 1.0)

  } else if (source == "etopo1"){
    #-----------------------------------------------------------
    # Get ETOPO1 elevation data. year_start and year_end not required
    #-----------------------------------------------------------
    siteinfo <- tibble(
      sitename = sitename,
      lon = lon,
      lat = lat
      )

    df <- ingest_globalfields(siteinfo,
                                  source = source,
                                  getvars = NULL,
                                  dir = dir,
                                  timescale = NULL,
                                  verbose = FALSE
    )

  } else if (source == "hwsd"){
    #-----------------------------------------------------------
    # Get HWSD soil data. year_start and year_end not required
    #-----------------------------------------------------------
    siteinfo <- tibble(
      lon = lon,
      lat = lat
    )
    con <- rhwsd::get_hwsd_con()
    df <- rhwsd::get_hwsd(x = siteinfo, con = con, hwsd.bil = settings$fil )

  } else if (source == "soilgrids"){
    #-----------------------------------------------------------
    # Get SoilGrids soil data. year_start and year_end not required
    # Code from https://git.wur.nl/isric/soilgrids/soilgrids.notebooks/-/blob/master/markdown/xy_info_from_R.md
    #-----------------------------------------------------------
    df <- ingest_soilgrids(
      tibble(sitename = sitename, lon = lon, lat = lat), 
      settings
      )

  } else if (source == "wise"){
    #-----------------------------------------------------------
    # Get WISE30secs soil data. year_start and year_end not required
    #-----------------------------------------------------------
    siteinfo <- data.frame(
      sitename = sitename,
      lon = lon,
      lat = lat
    )

    df <- purrr::map_dfc(as.list(settings$varnam), ~ingest_wise_byvar(., siteinfo, layer = settings$layer, dir = dir))

    if (length(settings$varnam) > 1){
      df <- df %>%
        rename(lon = lon...1, lat = lat...2) %>%
        dplyr::select(-starts_with("lon..."), -starts_with("lat...")) %>%
        right_join(dplyr::select(siteinfo, sitename, lon, lat), by = c("lon", "lat")) %>%
        dplyr::select(-lon, -lat) %>%
        group_by(sitename) %>%
        tidyr::nest()
    } else {
      df <- df %>%
        right_join(dplyr::select(siteinfo, sitename, lon, lat), by = c("lon", "lat")) %>%
        dplyr::select(-lon, -lat) %>%
        group_by(sitename) %>%
        tidyr::nest()

    }

  } else if (source == "gsde"){
    #-----------------------------------------------------------
    # Get GSDE soil data from tif files (2 files, for bottom and top layers)
    #-----------------------------------------------------------
    siteinfo <- tibble(
      sitename = sitename,
      lon = lon,
      lat = lat
    )
    
    aggregate_layers <- function(df, varnam, layer){
      
      df_layers <- tibble(
        layer = 1:8,
        bottom = c(4.5, 9.1, 16.6, 28.9, 49.3, 82.9, 138.3, 229.6)
        ) %>% 
        mutate(top = lag(bottom)) %>% 
        mutate(top = ifelse(is.na(top), 0, top)) %>% 
        rowwise() %>% 
        mutate(depth = bottom - top) %>% 
        dplyr::select(-top, -bottom)
      
      z_tot_use <- df_layers %>%
        ungroup() %>% 
        dplyr::filter(layer %in% settings$layer) %>%
        summarise(depth_tot_cm = sum(depth)) %>%
        pull(depth_tot_cm)
      
      ## weighted sum, weighting by layer depth
      df %>%
        left_join(df_layers, by = "layer") %>%
        rename(var = !!varnam) %>% 
        dplyr::filter(layer %in% settings$layer) %>%
        mutate(var_wgt = var * depth / z_tot_use) %>%
        group_by(sitename) %>%
        summarise(var := sum(var_wgt)) %>% 
        rename(!!varnam := var)
    }
    
    df <- purrr::map(
      as.list(settings$varnam),
      ~ingest_globalfields(siteinfo,
                           source = source,
                           getvars = NULL,
                           dir = dir,
                           timescale = NULL,
                           verbose = FALSE,
                           layer = .
      )) %>% 
      map2(as.list(settings$varnam), ~aggregate_layers(.x, .y, settings$layer)) %>% 
      purrr::reduce(left_join, by = "sitename") %>%
      group_by(sitename) %>%
      tidyr::nest()
    
    
  }  else if (source == "worldclim"){
    #-----------------------------------------------------------
    # Get WorldClim data from global raster file
    #-----------------------------------------------------------
    siteinfo <- tibble(
      sitename = sitename,
      lon = lon,
      lat = lat
    )
    
    df <- ingest_globalfields(siteinfo,
                               source = source,
                               dir = dir,
                               getvars = NULL,
                               timescale = NULL,
                               verbose = FALSE,
                               layer = settings$varnam
      ) %>% 
      group_by(sitename) %>% 
      tidyr::nest()
    
  }  else {
    rlang::warn(paste("you selected source =", source))
    stop("ingest(): Argument 'source' could not be identified. Use one of 'fluxnet', 'cru', 'watch_wfdei', 'wfde5', 'co2_mlo', 'etopo1', or 'gee'.")
  }

  ## add data frame to nice data frame containing all required time steps
  if (!(source %in% c("etopo1", "hwsd", "soilgrids", "wise", "gsde", "worldclim"))){
    if (timescale=="m"){
      df <- df_tmp %>%
        mutate(month = lubridate::month(date), year = lubridate::year(date)) %>%
        dplyr::select(-date) %>%
        right_join(df, by = c("year", "month"))

    } else if (timescale=="y"){
      df <- df_tmp %>%
        mutate(year = lubridate::year(date)) %>%
        dplyr::select(-date) %>%
        right_join(df, by = "year")

    } else if (timescale %in% c("d", "h", "hh")){
      df <- df_tmp %>%
        right_join(df, by = "date")
    }
  }

  # df <- df %>%
  #   tidyr::drop_na(sitename)

  return( df )

}
