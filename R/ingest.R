#' Data ingest
#'
#' Ingests data for site scale simulations with rsofun (or any other Dynamic 
#' Vegetation Model).
#'
#' @param siteinfo A data frame containing site meta info. Required columns are:
#'  \code{"sitename", "date_start", "date_end", "lon", "lat", "elv"}.
#' @param source A character used as identifiyer for the type of data source
#'  (e.g., \code{"fluxnet"}). See vignette for a full description of available 
#'  options.
#' @param getvars A named list of characters specifying the variable names in
#'  the source dataset corresponding to standard names \code{"temp"}
#'  for temperature, \code{"prec"} for precipitation, \code{"patm"} for 
#'  atmospheric pressure, \code{"vpd"} for vapour pressure deficit,
#'  \code{"netrad"} for net radiation, \code{"swin"} for shortwave incoming 
#'  radiation, \code{"lwin"} for longwave incoming radiation, \code{"wind"} 
#'  for wind.
#' @param dir A character specifying the directory where data is located.
#' @param settings A list of additional settings used for reading original files.
#' @param timescale A character or vector of characters, specifying the time 
#'  scale of data used from the respective source (if multiple time scales are
#'  available, otherwise is disregarded).
#' @param parallel A logical specifying whether ingest is run as parallel jobs
#'  for each site. This option is only available for \code{source = "modis"}
#'  and requires argument \code{ncores} to be set.
#' @param ncores An integer specifying the number of cores for parallel runs of
#'  ingest per site. Required only if \code{parallel = TRUE}
#' @param find_closest A logical specifying whether to extract data from the
#' closest gridcell with data if no data is available for the specified
#' location. Defaults to \code{FALSE}.
#' @param verbose if \code{TRUE}, additional messages are printed.
#'
#' @return A named list of data frames (tibbles) containing input data for each
#'  site is returned.
#' @import purrr dplyr
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' \dontrun{
#'   inputdata <- prepare_input_sofun(
#'    settings_input = settings_input,
#'    settings_sims = settings_sims,
#'    verwrite_climate = FALSE,
#'    verbose = TRUE )
#' }

ingest <- function(
	siteinfo,
	source,
	getvars      = c(),
	dir          = NULL,
	settings     = NULL,
	timescale    = "d",
	parallel     = FALSE,
	ncores       = NULL,
	find_closest = FALSE,
	verbose      = FALSE
  ){

  # CRAN compliance, declaring unstated variables
  sitename <- lon <- lat <- date_start <- date_end <- problem <-
  year_start_tmp <- x <- y <- lat_orig <- success <- elv <- patm <-
  patm_base <-patm_mean <- month <- tavg <-temp <- temp_fine <-
  tmax <- tmax_fine <- tmin <- tmin_fine <- prec <- prec_fine <-
  days_in_month <- rain <- snow <- srad <- srad_fine <- ppfd <-
  ppfd_fine <- wind <- wind_fine <- qair <- vap <- vapr <- vapr_fine <-
  ilon <- data <- yy <- mm <- co2_avg <- year <- . <- bias <- NULL
  
  # Check: all sites are distinct wrt name, lon and lat
  if (nrow(siteinfo) != nrow(dplyr::distinct(siteinfo, sitename, lon, lat))){
    stop("Non-distinct sites present w.r.t. name, lon, and lat.")
  }

  # Check dates and years for time series types
  if (!(source %in% c(
    "hwsd",
    "etopo1",
    "stocker23",
    "wwf",
    "soilgrids",
    "wise",
    "gsde",
    "worldclim"
    ))
    ) {
    
    # complement dates information
    if (!("year_start" %in% names(siteinfo))){
      if ("date_start" %in% names(siteinfo)){
        siteinfo <- siteinfo %>%
          mutate(year_start = lubridate::year(date_start))
      } else {
        stop("ingest(): Columns 'year_start' and 'date_start' missing
                     in object provided by argument 'siteinfo'")
      }
    }
    if (!("year_end" %in% names(siteinfo))){
      if ("date_end" %in% names(siteinfo)){
        siteinfo <- siteinfo %>%
          mutate(year_end = lubridate::year(date_end))
      } else {
        stop("ingest(): Columns 'year_end' and 'date_end' missing 
             in object provided by argument 'siteinfo'")
      }
    }

    if (!("date_start" %in% names(siteinfo))){
      if ("year_start" %in% names(siteinfo)){
        siteinfo <- siteinfo %>%
          mutate(
            date_start = lubridate::ymd(paste0(as.character(year_start),
                                               "-01-01")
                                        )
            )
      } else {
        stop("ingest(): Columns 'year_start' and 'date_start' missing
             in object provided by argument 'siteinfo'")
      }
    }
    if (!("date_end" %in% names(siteinfo))){
      if ("year_end" %in% names(siteinfo)){
        siteinfo <- siteinfo %>%
          mutate(
            date_end = lubridate::ymd(paste0(as.character(year_end),
                                                  "-12-31")
                                      )
            )
      } else {
        stop("ingest(): Columns 'year_end' and 'date_end' missing
             in object provided by argument 'siteinfo'")
      }
    }
    
    # check start < end
    if (siteinfo %>% 
        mutate(problem = year_start > year_end) %>% 
        pull(problem) %>% 
        any()){
      warning("At least one case found where year_start > year_end.
              The are exchanged now")
      siteinfo <- siteinfo %>% 
        mutate(year_start_tmp = ifelse(year_start > year_end,
                                       year_end,
                                       year_start)) %>% 
        mutate(year_end = ifelse(year_start > year_end,
                                 year_start,
                                 year_end)) %>% 
        mutate(year_start = year_start_tmp) %>% 
        dplyr::select(-year_start_tmp) %>% 
        mutate(
          date_start = lubridate::ymd(paste0(as.character(year_start),
                                             "-01-01")
                                      )
          ) %>% 
        mutate(
          date_end = lubridate::ymd(paste0(as.character(year_end),
                                           "-12-31")
                                    )
          )
    }
  }

	if (source == "fluxnet"){
	  
	  # Get data from sources given by site
	  
	  ddf <- purrr::map(
	    as.list(seq(nrow(siteinfo))),
	    ~ingest_bysite(
	      siteinfo$sitename[.],
	      source = source,
	      getvars = getvars,
	      dir = dir,
	      settings = settings,
	      timescale = timescale,
	      year_start = lubridate::year(siteinfo$date_start[.]),
	      year_end = lubridate::year(siteinfo$date_end[.]),
	      verbose = verbose
	    )
	  ) %>%
	    bind_rows()

	} else if (
	  source == "cru" ||
	  source == "watch_wfdei" ||
	  source == "ndep" ||
	  source == "wfde5"
	  ) {
	  
	  # Get data from global fields
	  
	  # special treatment of dates when bias correction is applied
	  if (!identical(NULL, settings$correct_bias)){
	    if (settings$correct_bias == "worldclim"){

        # save data frame with required dates for
        #  all sites (may be different by site)
        ddf_dates <- purrr::map(
          as.list(seq(nrow(siteinfo))),
          ~ingestr::init_dates_dataframe(
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
          message(
          "Beware: WorldClim data is for years 1970-2000.
          Therefore WATCH_WFDEI data is ingested for 1979-(at least) 2000.")
          year_start_wc <- 1979  # no earlier years available
          siteinfo <- siteinfo %>% 
            mutate(year_start = ifelse(year_start < year_start_wc, year_start, year_start_wc),
                   year_end   = ifelse(year_end > year_end_wc, year_end, year_end_wc))
        } else if (source == "wfde5"){
          message(
            "Beware: WorldClim data is for years 1970-2000.
            Therefore WFDE5 data is ingested for 1979-(at least) 2000.")
          year_start_wc <- 1979  # no earlier years available
          siteinfo <- siteinfo %>% 
            mutate(year_start = ifelse(year_start < year_start_wc, year_start, year_start_wc),
                   year_end   = ifelse(year_end > year_end_wc, year_end, year_end_wc))

        } else if (source == "cru"){
          message(
            "Beware: WorldClim data is for years 1970-2000. 
            Therefore CRU data is ingested for 1970-(at least) 2000.")
          siteinfo <- siteinfo %>% 
            mutate(year_start = ifelse(year_start < year_start_wc,
                                       year_start, year_start_wc),
                   year_end   = ifelse(year_end > year_end_wc,
                                       year_end, year_end_wc))
        }
	    }
	  }

		# this returns a flat data frame with data from all sites
    ddf <- ingest_globalfields(
      siteinfo,
      source = source,
      dir = dir,
      getvars = getvars,
      timescale = timescale,
      verbose = FALSE
      )
    
    
    if (find_closest){
      # check if data was extracted for all sites (may be located over ocean)
      sites_missing <- ddf %>%
        group_by(sitename) %>%
        summarise(across(tidyselect::vars_select_helpers$where(is.double),
                         ~sum(!is.na(.x)))) %>%
        dplyr::filter(across(c(-sitename, -date), ~ .x == 0)) %>%
        pull(sitename)
      
      if (length(sites_missing) > 0 & !identical(NULL, settings$correct_bias)){
        # determine closest cell with non-NA
        if (source == "watch_wfdei"){
          path <- paste0(dir, "/WFDEI-elevation.nc")
        } else if (source == "cru"){
          path <- paste0(dir, "/elv_cru_halfdeg.nc")
        }
        if (!file.exists(path)) {
          stop(paste0("Looking for elevation file for determining 
                    closest land cell, but not found under ", path))
        }
        rasta <- raster::raster(path)
        siteinfo_missing <- siteinfo %>%
          dplyr::filter(sitename %in% sites_missing)
        siteinfo_missing <- siteinfo_missing %>%
          dplyr::select(x = lon, y = lat) %>%
          mutate(
            lon = raster::xFromCell(
              rasta,
              which.min(replace(raster::distanceFromPoints(rasta, .),
                                is.na(rasta), NA))[1]),
            lat = raster::yFromCell(
              rasta, which.min(replace(raster::distanceFromPoints(rasta, .),
                                       is.na(rasta), NA))[1])) %>%
          rename(lon_orig = x, lat_orig = y) %>%
          bind_cols(siteinfo_missing %>% dplyr::select(-lon, -lat)) %>%
          mutate(success = ifelse(abs(lat-lat_orig)>1.0, FALSE, TRUE))
        
        # extract again for sites with missing data
        ddf_missing <- ingest_globalfields(siteinfo_missing,
                                           source = source,
                                           dir = dir,
                                           getvars = getvars,
                                           timescale = timescale,
                                           verbose = FALSE)
        
        if (sum(!siteinfo_missing$success)>0){
          warning(
            "No land found within 1 degree latitude for the following sites:
        Consider excluding them.")
          print(siteinfo_missing %>% dplyr::filter(!success))
        }
        
        # replace site with adjusted location
        ddf <- ddf %>%
          dplyr::filter(!(sitename %in% sites_missing)) %>%
          bind_rows(ddf_missing)
      }
    }

    # bias-correct atmospheric pressure - per default
    if (!is.null(getvars)){
      if ("patm" %in% getvars){
        
        df_patm_base <- siteinfo %>%
          dplyr::select(sitename, elv) %>%
          mutate(patm_base = calc_patm(elv))
        
        ddf <- ddf %>%
          group_by(sitename) %>%
          summarise(patm_mean = mean(patm, na.rm = TRUE)) %>%
          left_join(df_patm_base, by = "sitename") %>%
          mutate(scale = patm_base / patm_mean) %>%
          right_join(ddf, by = "sitename") %>%
          mutate(patm = patm * scale) %>%
          dplyr::select(-patm_base, -elv, -patm_mean, -scale)
        
      }
    }

    if (!identical(NULL, settings$correct_bias)){
      if (settings$correct_bias == "worldclim"){
        
        # Bias correction using WorldClim data
        
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
                                       layer = unique(getvars_wc))

        # Bias correction for temperature: subtract difference
        if ("tavg" %in% getvars_wc){
          df_bias <- df_fine %>%
            dplyr::select(sitename, starts_with("tavg_")) %>%
            tidyr::pivot_longer(
              cols = starts_with("tavg_"),
              names_to = "month",
              values_to = "tavg",
              names_prefix = "tavg_") %>%
            mutate(month = as.integer(month)) %>%
            rename(temp_fine = tavg) %>%
            right_join(ddf %>%
                         dplyr::filter(
                           lubridate::year(date) %in% year_start_wc:year_end_wc
                           ) %>%
                         mutate(month = lubridate::month(date)) %>%
                         group_by(sitename, month) %>%
                         summarise(temp = mean(temp, na.rm = TRUE)),
                       by = c("sitename", "month")) %>%
            mutate(bias = temp - temp_fine) %>%
            dplyr::select(-temp, -temp_fine)

          # correct bias by month
          ddf <- ddf %>%
            mutate(month = lubridate::month(date)) %>%
            left_join(df_bias %>% dplyr::select(sitename, month, bias),
                      by = c("sitename", "month")) %>%
            arrange(sitename, date) %>%
            mutate(temp = ifelse(is.na(bias), temp, temp - bias)) %>%
            dplyr::select(-bias, -month)
        }

        # Bias correction for temperature: subtract difference
        if ("tmin" %in% getvars_wc){
          if (source == "cru"){ # no tmin or tmax in wwfd
            df_bias <- df_fine %>%
              dplyr::select(sitename, starts_with("tmin_")) %>%
              tidyr::pivot_longer(
                cols = starts_with("tmin_"),
                names_to = "month",
                values_to = "tmin",
                names_prefix = "tmin_") %>%
              mutate(month = as.integer(month)) %>%
              rename(tmin_fine = tmin) %>%
              right_join(ddf %>%
                           dplyr::filter(
                             lubridate::year(date) %in% year_start_wc:year_end_wc
                           ) %>%
              mutate(month = lubridate::month(date)) %>%
              group_by(sitename, month) %>%
              summarise(tmin = mean(tmin, na.rm = TRUE)),
                         by = c("sitename", "month")) %>%
              mutate(bias = tmin - tmin_fine) %>%
              dplyr::select(-tmin, -tmin_fine)
            
            # correct bias by month
            ddf <- ddf %>%
              mutate(month = lubridate::month(date)) %>%
              left_join(
                df_bias %>% dplyr::select(sitename, month, bias),
                by = c("sitename", "month")) %>%
              arrange(sitename, date) %>%
              mutate(tmin = ifelse(is.na(bias), tmin, tmin - bias)) %>%
              dplyr::select(-bias, -month)
          }
        }    


        # Bias correction for temperature: subtract difference
        if ("tmax" %in% getvars_wc){
          if (source == "cru"){ # no tmin or tmax in wwfd
            df_bias <- df_fine %>%
              dplyr::select(sitename, starts_with("tmax_")) %>%
              tidyr::pivot_longer(
                cols = starts_with("tmax_"),
                names_to = "month",
                values_to = "tmax",
                names_prefix = "tmax_") %>%
              mutate(month = as.integer(month)) %>%
              rename(tmax_fine = tmax) %>%
              right_join(ddf %>%
               dplyr::filter(
                 lubridate::year(date) %in% year_start_wc:year_end_wc
                 ) %>%
               mutate(month = lubridate::month(date)) %>%
               group_by(sitename, month) %>%
               summarise(tmax = mean(tmax, na.rm = TRUE)),
               by = c("sitename", "month")) %>%
              mutate(bias = tmax - tmax_fine) %>%
              dplyr::select(-tmax, -tmax_fine)
  
            # correct bias by month
            ddf <- ddf %>%
              mutate(month = lubridate::month(date)) %>%
              left_join(
                df_bias %>% dplyr::select(sitename, month, bias),
                by = c("sitename", "month")
                ) %>%
              arrange(sitename, date) %>%
              mutate(tmax = ifelse(is.na(bias), tmax, tmax - bias)) %>%
              dplyr::select(-bias, -month)
          }
        }            

        # Bias correction for precipitation: scale by ratio (snow and rain equally)
        if ("prec" %in% getvars_wc){
          df_bias <- df_fine %>%
            dplyr::select(sitename, starts_with("prec_")) %>%
            tidyr::pivot_longer(cols = starts_with("prec_"), names_to = "month", values_to = "prec", names_prefix = "prec_") %>%
            mutate(month = as.integer(month)) %>%
            rename(prec_fine = prec) %>%
            mutate(prec_fine = prec_fine / lubridate::days_in_month(month)) %>%   # mm/month -> mm/d
            mutate(prec_fine = prec_fine / (60 * 60 * 24)) %>%         # mm/d -> mm/sec
            right_join(ddf %>%
                         dplyr::filter(lubridate::year(date) %in% year_start_wc:year_end_wc) %>%
                         mutate(month = lubridate::month(date)) %>%
                         group_by(sitename, month) %>%
                         summarise(prec = mean(prec, na.rm = TRUE)),
                       by = c("sitename", "month")) %>%
            mutate(scale = prec_fine / prec) %>%
            dplyr::select(sitename, month, scale)

          # correct bias by month
          if (source == "watch_wfdei" || source == "wfde5"){
            ddf <- ddf %>%
              mutate(month = lubridate::month(date)) %>%
              left_join(df_bias %>% dplyr::select(sitename, month, scale), by = c("sitename", "month")) %>%
              arrange(sitename, date) %>%
              mutate(scale = ifelse(is.infinite(scale), 0, scale)) %>%
              mutate(prec = ifelse(is.na(scale), prec, prec * scale),
                     rain = ifelse(is.na(scale), rain, rain * scale),
                     snow = ifelse(is.na(scale), snow, snow * scale)) %>%
              dplyr::select(-scale, -month)

          } else {
            ddf <- ddf %>%
              mutate(month = lubridate::month(date)) %>%
              left_join(df_bias %>% dplyr::select(sitename, month, scale), by = c("sitename", "month")) %>%
              arrange(sitename, date) %>%
              mutate(scale = ifelse(is.infinite(scale), 0, scale)) %>%
              mutate(prec = ifelse(is.na(scale), prec, prec * scale)) %>%
              dplyr::select(-scale, -month)

          }

        }

        # Bias correction for shortwave radiation: scale by ratio
        if ("srad" %in% getvars_wc){
          kfFEC <- 2.04
          df_bias <- df_fine %>%
            dplyr::select(sitename, starts_with("srad_")) %>%
            tidyr::pivot_longer(cols = starts_with("srad_"), names_to = "month", values_to = "srad", names_prefix = "srad_") %>%
            mutate(month = as.integer(month)) %>%
            rename(srad_fine = srad) %>%
            mutate(ppfd_fine = 1e3 * srad_fine * kfFEC * 1.0e-6 / (60 * 60 * 24) ) %>%   # kJ m-2 day-1 -> mol m−2 s−1 PAR
            right_join(ddf %>%
                         dplyr::filter(lubridate::year(date) %in% year_start_wc:year_end_wc) %>%
                         mutate(month = lubridate::month(date)) %>%
                         group_by(sitename, month) %>%
                         summarise(ppfd = mean(ppfd, na.rm = TRUE)),
                       by = c("sitename", "month")) %>%
            mutate(scale = ppfd_fine / ppfd) %>%
            dplyr::select(sitename, month, scale)

          # correct bias by month
          ddf <- ddf %>%
            mutate(month = lubridate::month(date)) %>%
            left_join(df_bias %>% dplyr::select(sitename, month, scale), by = c("sitename", "month")) %>%
            arrange(sitename, date) %>%
            mutate(scale = ifelse(is.infinite(scale), 0, scale)) %>%
            mutate(ppfd = ifelse(is.na(scale), ppfd, ppfd * scale)) %>%
            dplyr::select(-scale, -month)
        }

        # Bias correction for atmospheric pressure: scale by ratio
        if ("wind" %in% getvars_wc){
          df_bias <- df_fine %>%
            dplyr::select(sitename, starts_with("wind_")) %>%
            tidyr::pivot_longer(cols = starts_with("wind_"),
                                names_to = "month",
                                values_to = "wind",
                                names_prefix = "wind_") %>%
            mutate(month = as.integer(month)) %>%
            rename(wind_fine = wind) %>%
            right_join(ddf %>%
                         dplyr::filter(
                           lubridate::year(date) %in% year_start_wc:year_end_wc
                           ) %>%
                         mutate(month = lubridate::month(date)) %>%
                         group_by(sitename, month) %>%
                         summarise(wind = mean(wind, na.rm = TRUE)),
                       by = c("sitename", "month")) %>%
            mutate(scale = wind_fine / wind) %>%
            dplyr::select(sitename, month, scale)

          # correct bias by month
          ddf <- ddf %>%
            mutate(month = lubridate::month(date)) %>%
            left_join(df_bias %>% dplyr::select(sitename, month, scale),
                      by = c("sitename", "month")) %>%
            arrange(sitename, date) %>%
            mutate(scale = ifelse(is.infinite(scale), 0, scale)) %>%
            mutate(wind = ifelse(is.na(scale), wind, wind * scale)) %>%
            dplyr::select(-scale, -month)

        }

        # Bias correction for relative humidity (actually vapour pressure): scale
        if ("vapr" %in% getvars_wc){

          # calculate vapour pressure from specific humidity - needed for bias correction with worldclim data
          if (source == "watch_wfdei" || source == "wfde5"){
            # specific humidity (qair, g g-1) is read, convert to vapour pressure (vapr, Pa)
            ddf <- ddf %>% 
              rowwise() %>% 
              dplyr::mutate(
                vapr = calc_vp(qair = qair,
                               patm = patm)
                ) %>% 
              ungroup()
            
          } else if (source == "cru"){
            # vapour pressure is read from file, convert from hPa to Pa
            ddf <- ddf %>% 
              dplyr::mutate(vapr = 1e2 * vap) %>% 
              dplyr::select(-vap)
            
          }

          df_bias <- df_fine %>%
            dplyr::select(sitename, starts_with("vapr_")) %>%
            tidyr::pivot_longer(
              cols = starts_with("vapr_"),
              names_to = "month",
              values_to = "vapr",
              names_prefix = "vapr_") %>%
            mutate(month = as.integer(month)) %>%
            rename(vapr_fine = vapr) %>%
            mutate(vapr_fine = vapr_fine * 1e3) %>%   # kPa -> Pa
            right_join(ddf %>%
                         dplyr::filter(lubridate::year(date) %in% year_start_wc:year_end_wc) %>%
                         mutate(month = lubridate::month(date)) %>%
                         group_by(sitename, month) %>%
                         summarise(vapr = mean(vapr, na.rm = TRUE)),
                       by = c("sitename", "month")) %>%
            mutate(scale = vapr_fine / vapr) %>%
            dplyr::select(sitename, month, scale)

          # correct bias by month
          ddf <- ddf %>%
            mutate(month = lubridate::month(date)) %>%
            left_join(df_bias %>% dplyr::select(sitename, month, scale), by = c("sitename", "month")) %>%
            arrange(sitename, date) %>%
            mutate(scale = ifelse(is.infinite(scale), 0, scale)) %>%
            mutate(vapr = ifelse(is.na(scale), vapr, vapr * scale)) %>%
            dplyr::select(-scale, -month)

        }

        # Calculate vapour pressure deficit from specific humidity
        if ("vpd" %in% getvars){
          
          if (source == "watch_wfdei" || source == "wfde5"){
            # use daily mean temperature
            ddf <- ddf %>%
              rowwise() %>%
              dplyr::mutate(
                vapr = calc_vp(
                  qair = qair,
                  patm = patm
                  ),
                vpd = calc_vpd(eact = vapr, tc = temp)) %>% 
              ungroup()
            
          } else if (source == "cru"){
            # use daily minimum and maximum temperatures
            ddf <- ddf %>%
              rowwise() %>%
              dplyr::mutate(
                vpd = calc_vpd(eact = vapr, tmin = tmin, tmax = tmax)
                ) %>% 
              ungroup()
          }
        }

        # keep only required dates
        ddf <- ddf %>%
          right_join(ddf_dates, by = c("sitename", "date"))

      }

    } else {

      # Calculate vapour pressure deficit from specific humidity
      # this calculates this variable for cases where there is
      # no bias correction
      
      if ("vpd" %in% getvars){

        if (source == "watch_wfdei" || source == "wfde5"){
          # use daily mean temperature
          ddf <- ddf %>%
            rowwise() %>%
            dplyr::mutate(
              vapr = calc_vp(qair = qair, patm = patm),
              vpd = calc_vpd(eact = vapr, tc = temp)
              ) %>% 
            ungroup()
          
        } else if (source == "cru"){
          # use daily minimum and maximum temperatures
          ddf <- ddf %>%
            rowwise() %>%
            dplyr::mutate(
              vpd = calc_vpd(eact = vapr, tmin = tmin, tmax = tmax)
              ) %>% 
            ungroup()
        }

      }

    }

	} else if (source == "gee"){
	  
	  # Get data from the remote server
	  
	  # Define years covered based on site meta info:
	  # take all years used for at least one site.
	  year_start <- siteinfo %>%
	    pull(year_start) %>%
	    min()

	  year_end <- siteinfo %>%
	    pull(year_end) %>%
	    max()

	  ddf <- purrr::map(
	    as.list(seq(nrow(siteinfo))),
	    ~ingest_gee_bysite(
	      slice(siteinfo, .),
	      start_date           = paste0(as.character(year_start), "-01-01"),
	      end_date             = paste0(as.character(year_end), "-12-31"),
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
	  )

	} else if (source == "modis"){
	  
	  # Get data from the remote server
		if (parallel){

			if (is.null(ncores)){
			  stop("Aborting. Please provide number of cores for parallel jobs.")
			} 

	    cl <- multidplyr::new_cluster(ncores) %>%
	      multidplyr::cluster_assign(settings = settings) %>%
	      multidplyr::cluster_library(
	        c("dplyr", 
	          "purrr",
	          "rlang",
	          "ingestr",
	          "readr", 
	          "lubridate",
	          "MODISTools",
	          "tidyr"))

		  # distribute to cores, making sure all data from 
		  # a specific site is sent to the same core
		  ddf <- tibble(ilon = seq(nrow(siteinfo))) %>%
		    multidplyr::partition(cl) %>%
		    dplyr::mutate(data = purrr::map( ilon,
		                                    ~ingest_modis_bysite(
		                                    	slice(siteinfo, .),
				      														settings))) %>%
		    collect() %>%
		    tidyr::unnest(data)

		} else {
		  
		  ddf <- purrr::map(
		    as.list(seq(nrow(siteinfo))),
		    ~ingest_modis_bysite(
		      slice(siteinfo, .),
		      settings
		    )
		  )
		  
		}


	} else if (source == "co2_mlo"){
	  
	  # Get CO2 data year, independent of site
	  
	  # if 'dir' is provided, try reading from existing file, otherwise download
	  path <- paste0(dir, "/df_co2_mlo.csv")
	  if (!identical(NULL, dir)){
	    if (file.exists(path)){
	      df_co2 <- readr::read_csv(path)
	    } else {
	      df_co2 <- climate::meteo_noaa_co2() %>%
	        dplyr::select(year = yy, month = mm, co2_avg)
	      readr::write_csv(df_co2, file = path)
	    }
	  } else {
	    df_co2 <- climate::meteo_noaa_co2() %>%
	      dplyr::select(year = yy, month = mm, co2_avg)
	  }

	  # aggregate to annual means
	  df_co2 <- df_co2 %>%
	    group_by(year) %>%
	    summarise(co2 = mean(co2_avg, na.rm = TRUE))

	  # expand to data frame for each site
	  ddf <- purrr::map(
	    as.list(seq(nrow(siteinfo))),
	    ~expand_co2_bysite(
	      df_co2,
	      sitename = siteinfo$sitename[.],
	      year_start = lubridate::year(siteinfo$date_start[.]),
	      year_end   = lubridate::year(siteinfo$date_end[.]),
	      timescale  = timescale
	      )
	    )


	} else if (source == "co2_cmip"){
	  
	  # Get CO2 data year, independent of site
	  
	  # if 'dir' is provided, try reading from existing file, otherwise download
	  path <- paste0(dir, "/cCO2_rcp85_const850-1765.csv")
	  if (file.exists(path)){
	    df_co2 <- readr::read_csv(path)
	  } else {
	    stop(
	      "File cCO2_rcp85_const850-1765.csv must be available in directory
	      specified by 'dir'."
	      )
	  }

	  # expand to data frame for each site
	  ddf <- purrr::map(
	    as.list(seq(nrow(siteinfo))),
	    ~expand_co2_bysite(
	      df_co2,
	      sitename = siteinfo$sitename[.],
	      year_start = lubridate::year(siteinfo$date_start[.]),
	      year_end   = lubridate::year(siteinfo$date_end[.]),
	      timescale = timescale
	    )
	  )

	} else if (source == "fapar_unity"){
	  
	  # Assume fapar = 1 for all dates
	  
	  ddf <- purrr::map(
	    as.list(seq(nrow(siteinfo))),
	    ~expand_bysite(
	      sitename   = siteinfo$sitename[.],
	      year_start = lubridate::year(siteinfo$date_start[.]),
	      year_end   = lubridate::year(siteinfo$date_end[.]),
	      timescale  = timescale
	      ) %>%
	      mutate(fapar = 1.0)
	  )

	} else if (source == "etopo1"){
	  
	  # Get ETOPO1 elevation data. year_start and year_end not required
	  
	  ddf <- ingest_globalfields(
	    siteinfo,
	    source = source,
	    dir = dir,
	    getvars = NULL,
	    timescale = NULL,
	    verbose = FALSE
	  )
	  
	} else if (source == "stocker23"){
	  
	  # Get root zone water storage capacity data. year_start and year_end not required
	  
	  ddf <- ingest_globalfields(
	    siteinfo,
	    source = source,
	    dir = dir,
	    getvars = NULL,
	    timescale = NULL,
	    verbose = FALSE
	  )
	  
	} else if (source == "hwsd"){
	  
	  
	  # https://github.com/bluegreen-labs/hwsdr
	  # Get HWSD soil data. year_start and year_end not required
	  
	  # TODO replace with {hwsdr} call
	  # con <- rhwsd::get_hwsd_con()
	  # ddf <- rhwsd::get_hwsd_siteset(
	  #   x = dplyr::select(siteinfo, sitename, lon, lat),
	  #   con = con, hwsd.bil = settings$fil ) %>%
	  #   dplyr::ungroup() %>%
	  #   dplyr::select(sitename, data) %>%
	  #   tidyr::unnest(data)

	} else if (source == "wwf"){
	  
	  # Get WWF ecoregion data. year_start and year_end not required
	  ddf <- ingest_globalfields(
	    siteinfo,
	    source = source,
	    dir = dir,
	    getvars = NULL,
	    timescale = NULL,
	    verbose = FALSE,
	    layer = settings$layer
	  )

	} else if (source == "soilgrids"){
	  
	  # Get SoilGrids soil data. year_start and year_end not required
	  # Code from https://git.wur.nl/isric/soilgrids/soilgrids.notebooks/-/blob/master/markdown/xy_info_from_R.md
	  
	  ddf <- ingest_soilgrids(siteinfo, settings)

	} else if (source == "wise"){
	  
	  # Get WISE30secs soil data. year_start and year_end not required
	  ddf <- purrr::map(as.list(settings$varnam),
	                    ~ingest_wise_byvar(.,
	                                       siteinfo,
	                                       layer = settings$layer, dir = dir)) %>%
	    purrr::reduce(left_join, by = c("lon", "lat")) %>%
	    distinct() %>% 
	    right_join(
	      dplyr::select(all_of(
	        siteinfo, 
	        sitename, 
	        lon, 
	        lat
	        )), 
	      by = c("lon", "lat")) %>%
	    dplyr::select(-lon, -lat)

	} else if (source == "gsde"){
	  
	  # Get GSDE soil data from tif files (2 files, for bottom and top layers)
	  ddf <- purrr::map(
	    as.list(settings$varnam),
	    ~ingest_globalfields(
	      siteinfo,
	      source = source,
	      getvars = NULL,
	      dir = dir,
	      timescale = NULL,
	      verbose = FALSE,
	      layer = .
	    )) %>%
	    map2(as.list(settings$varnam),
	         ~aggregate_layers_gsde(.x, .y, settings$layer)) %>%
	    purrr::reduce(left_join, by = "sitename")

	 }  else if (source == "worldclim"){
	   
	   # Get WorldClim data from global raster file
	   
	   ddf <- ingest_globalfields(
	     siteinfo,
	     source = source,
	     dir = dir,
	     getvars = NULL,
	     timescale = NULL,
	     verbose = FALSE,
	     layer = settings$varnam
	   )

    ddf <- purrr::map(
      as.list(settings$varnam),
      ~worldclim_pivot_longer(ddf, .)
      ) |>
      purrr::reduce(left_join, by = c("sitename", "month"))

	 } else {

	  rlang::warn(paste("you selected source =", source))
	  stop(
	    "ingest(): Argument 'source' could not be identified. 
	     Use one of 'fluxnet', 'cru', 'watch_wfdei', 'wfde5',
	     co2_mlo', 'etopo1', 'stocker23', or 'gee'.")
	}

  ddf <- ddf %>%
    bind_rows() %>%
    filter(!is.na(sitename)) %>%
    group_by(sitename) %>%
    tidyr::nest()
  
  return(ddf)

}

# give each site and day within year the same co2 value
expand_co2_bysite <- function(df, sitename, year_start, year_end, timescale){

  ddf <- init_dates_dataframe( year_start, year_end, timescale = timescale) %>%
    dplyr::mutate(year = lubridate::year(date)) %>%
    dplyr::left_join(
      df,
      by = "year"
    ) %>%
    dplyr::mutate(sitename = sitename)

  return(ddf)
}

expand_bysite <- function(sitename, year_start, year_end, timescale ){

  ddf <- init_dates_dataframe( year_start, year_end, timescale = timescale) %>%
    dplyr::mutate(sitename = sitename)

  return(ddf)

}

aggregate_layers_gsde <- function(df, varnam, use_layer){
  
  # define state variables
  var <- above_1 <- above_2 <- above_3 <- above_4 <- above_5 <-
    above_6 <- above_7 <- layer <- data <- sitename <- 
    bottom <- top <- layer <- depth <- depth_tot_cm <-
    var <- var_wgt <- NULL
  
  fill_layer_from_above <- function(df, varnam){
    
    df %>% 
      rename(var = !!varnam) %>% 
      mutate(above_1 = lag(var), 
             above_2 = lag(var, n = 2),
             above_3 = lag(var, n = 3),
             above_4 = lag(var, n = 4),
             above_5 = lag(var, n = 5),
             above_6 = lag(var, n = 6),
             above_7 = lag(var, n = 7)) %>% 
      mutate(var = ifelse(is.na(var),
                          ifelse(is.na(above_1),
                                 ifelse(is.na(above_2),
                                        ifelse(is.na(above_3),
                                               ifelse(is.na(above_4),
                                                      ifelse(is.na(above_5),
                                                             ifelse(is.na(above_6),
                                                                    ifelse(is.na(above_7),
                                                                           above_7,
                                                                           above_7),
                                                                    above_6),
                                                             above_5),
                                                      above_4),
                                               above_3),
                                        above_2),
                                 above_1),
                          var)) %>% 
      dplyr::select(var, layer) %>% 
      rename(!!varnam := var)
  }
  
  # fill missing values with next available value from layer above
  df <- df %>% 
    group_by(sitename) %>% 
    tidyr::nest() %>% 
    mutate(data = purrr::map(data, ~fill_layer_from_above(., varnam))) %>% 
    tidyr::unnest(data)
  
  df_layers <- tibble(layer = 1:8, bottom = c(4.5, 9.1, 16.6, 28.9, 49.3, 82.9, 138.3, 229.6)) %>%
    mutate(top = lag(bottom)) %>%
    mutate(top = ifelse(is.na(top), 0, top)) %>%
    rowwise() %>%
    mutate(depth = bottom - top) %>%
    dplyr::select(-top, -bottom)
  
  z_tot_use <- df_layers %>%
    ungroup() %>%
    dplyr::filter(layer %in% use_layer) %>%
    summarise(depth_tot_cm = sum(depth)) %>%
    pull(depth_tot_cm)
  
  # weighted sum, weighting by layer depth
  df %>%
    left_join(df_layers, by = "layer") %>%
    rename(var = !!varnam) %>%
    dplyr::filter(layer %in% use_layer) %>%
    mutate(var_wgt = var * depth / z_tot_use) %>%
    group_by(sitename) %>%
    summarise(var := sum(var_wgt)) %>%
    rename(!!varnam := var)
}

worldclim_pivot_longer <- function(df, varnam){

  df |>
    # tidyr::unnest(data) |> 
    dplyr::select(sitename, starts_with(paste0(varnam, "_"))) |>
    tidyr::pivot_longer(
      cols = starts_with(paste0(varnam, "_")),
      names_to = "month",
      values_to = varnam,
      names_prefix = paste0(varnam, "_")) |>
    mutate(month = as.integer(month))

}
