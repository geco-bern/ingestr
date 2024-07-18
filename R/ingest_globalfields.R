#' Ingest from global fields
#'
#' Read climate data from files as global fields
#'
#' @param siteinfo A data frame with rows for each site and columns `lon` 
#' for longitude, `lat` for latitude, `date_start` and `date_end` 
#' specifying required dates.
#' @param source A character used as identifiyer for the type of data source
#' (\code{"watch_wfdei"}, or \code{"cru"}).
#' @param getvars A named list of characters specifying the variable names in
#' the source dataset corresponding to standard names \code{"temp"} for
#' temperature.
#' @param dir A character specifying the directory where data is located.
#' \code{"prec"} for precipitation, \code{"patm"} for atmospheric pressure,
#' \code{"vpd"} for vapour pressure deficit, \code{"netrad"} for net radiation,
#' \code{"swin"} for shortwave incoming radiation.
#' @param timescale A character or vector of characters, specifying the time
#'  scale of data used from the respective source (if multiple time scales are
#'  available, otherwise is disregarded).
#' @param standardise_units A logical specifying whether units in ingested data
#' are to be standardised following ingestr-standard units.
#' @param layer (Optional) A character string specifying the layer from a 
#' shapefile or a raster brick to be read or to be used to identify file
#' name for gsde.
#' @param verbose if \code{TRUE}, additional messages are printed.
#'
#' @return A data frame (tibble) containing the time series of ingested data,
#'  nested for each site.
#' @import purrr dplyr
#' @export
#'
#' @examples \dontrun{inputdata <- ingest_bysite()}
#'
ingest_globalfields <- function(
  siteinfo,
  source,
  getvars,
  dir,
  timescale,
  standardise_units = TRUE,
  layer = NULL,
  verbose = FALSE
){
  
  # CRAN compliance, define state variables
  myvar <- temp <- rain <- snow <- sitename <- year <- moy <- 
  vap <- tmin <- tmax <- prec <- days_in_month <- nhx <- noy <-
    lon <- lat <- data <- V1 <- elv <- varnam <- value <- fact <- NULL
  
  if (any(is.na(siteinfo$sitename)) ||
      any(is.null(siteinfo$sitename))){
    stop("At least one entry for siteinfo$sitename is missing.")
  }
  
  if (!(source %in% c("etopo1", "stocker23", "wwf", "gsde", "worldclim"))){
    
    # get a daily (monthly) data frame with all dates for all sites
    # (if monthly, day 15 of each month)
    df_out <- purrr::map(
      as.list(seq(nrow(siteinfo))),
      ~ingestr::init_dates_dataframe(
        lubridate::year(siteinfo$date_start[.]),
        lubridate::year(siteinfo$date_end[.]),
        noleap = TRUE,
        timescale = timescale))
    names(df_out) <- siteinfo$sitename
    df_out <- df_out %>%
      bind_rows(.id = "sitename")
    
  } else {
    df_out <- tibble()
  }
  
  if (source=="watch_wfdei"){
    
    # Read WATCH-WFDEI data (extracting from NetCDF files for this site)
    
    # vpd based on relative humidity, air temperature, and atmospheric pressure
    if ("vpd" %in% getvars){
      df_out <- ingest_globalfields_watch_byvar( df_out, siteinfo, dir, "Qair_daily" ) %>%
        dplyr::rename(qair = myvar) %>%
        dplyr::right_join(df_out, by = c("sitename", "date")) %>%
        left_join(
          ingest_globalfields_watch_byvar( df_out, siteinfo, dir, "Tair_daily" ) %>%
            dplyr::rename(temp = myvar) %>%
            dplyr::mutate(temp = temp - 273.15),
          by = c("sitename", "date")
        ) %>%
        left_join(
          ingest_globalfields_watch_byvar( df_out, siteinfo, dir, "PSurf_daily" ) %>%
            dplyr::rename(patm = myvar),
          by = c("sitename", "date")
        )
    }
    
    # precipitation
    if ("prec" %in% getvars){
      df_out <- ingest_globalfields_watch_byvar( df_out, siteinfo, dir, "Rainf_daily" ) %>%
        dplyr::rename( rain = myvar ) %>%
        left_join(
          ingest_globalfields_watch_byvar( df_out, siteinfo, dir, "Snowf_daily" ) %>%
            dplyr::rename( snow = myvar ),
          by = c("sitename", "date")
        ) %>%
        dplyr::mutate(prec = (rain + snow) ) %>%  # kg/m2/s
        dplyr::right_join(df_out, by = c("sitename", "date"))
    }
    
    # temperature
    if ("temp" %in% getvars && !("temp" %in% names(df_out))){
      df_out <- ingest_globalfields_watch_byvar( df_out, siteinfo, dir, "Tair_daily" ) %>%
        dplyr::rename(temp = myvar) %>%
        dplyr::mutate(temp = temp - 273.15) %>%
        dplyr::right_join(df_out, by = c("sitename", "date"))
    }
    
    # atmospheric pressure
    if ("patm" %in% getvars && !("patm" %in% names(df_out))){
      df_out <- ingest_globalfields_watch_byvar( df_out, siteinfo, dir, "PSurf_daily" ) %>%
        dplyr::rename(patm = myvar) %>%
        dplyr::right_join(df_out, by = c("sitename", "date"))
    }
    
    # PPFD
    if ("ppfd" %in% getvars){
      kfFEC <- 2.04
      df_out <- ingest_globalfields_watch_byvar( df_out, siteinfo, dir, "SWdown_daily" ) %>%
        dplyr::mutate(ppfd = myvar * kfFEC * 1.0e-6 ) %>%  # W m-2 -> mol m-2 s-1
        dplyr::right_join(df_out, by = c("sitename", "date"))
    }
    
    # remove spurious myvar columns
    df_out <- df_out %>%
      dplyr::select(-starts_with("myvar"))
    
    if (timescale=="m"){
      stop("ingest_globalfields(): aggregating WATCH-WFDEI to monthly not implemented yet.")
    }
    
  } else if (source=="wfde5"){
    
    # Read WFDE5 data (extracting from NetCDF files for this site)
    
    # Development Checks
    if (timescale != "h"){
      rlang::abort("ingest_globalfields(): WFDE5 currently only available for hourly output.")
    }
    
    # vpd based on relative humidity, air temperature, and atmospheric pressure
    if ("vpd" %in% getvars){
      df_out <- ingest_globalfields_wfde5_byvar( df_out, siteinfo, dir, "Qair" ) %>%
        dplyr::rename(qair = myvar) %>%
        dplyr::right_join(df_out, by = c("sitename", "date")) %>%
        left_join(
          ingest_globalfields_wfde5_byvar( df_out, siteinfo, dir, "Tair" ) %>%
            dplyr::rename(temp = myvar) %>%
            dplyr::mutate(temp = temp - 273.15),
          by = c("sitename", "date")
        ) %>%
        left_join(
          ingest_globalfields_wfde5_byvar( df_out, siteinfo, dir, "PSurf" ) %>%
            dplyr::rename(patm = myvar),
          by = c("sitename", "date")
        )
    }
    
    # precipitation
    if ("prec" %in% getvars){
      df_out <- ingest_globalfields_wfde5_byvar( df_out, siteinfo, dir, "Rainf" ) %>%
        dplyr::rename( rain = myvar ) %>%
        left_join(
          ingest_globalfields_wfde5_byvar( df_out, siteinfo, dir, "Snowf" ) %>%
            dplyr::rename( snow = myvar ),
          by = c("sitename", "date")
        ) %>%
        dplyr::mutate(prec = (rain + snow) ) %>%  # kg/m2/s
        dplyr::right_join(df_out, by = c("sitename", "date"))
    }
    
    # temperature
    if ("temp" %in% getvars && !("temp" %in% names(df_out))){
      df_out <- ingest_globalfields_wfde5_byvar( df_out, siteinfo, dir, "Tair" ) %>%
        dplyr::rename(temp = myvar) %>%
        dplyr::mutate(temp = temp - 273.15) %>%
        dplyr::right_join(df_out, by = c("sitename", "date"))
    }
    
    # atmospheric pressure
    if ("patm" %in% getvars && !("patm" %in% names(df_out))){
      df_out <- ingest_globalfields_wfde5_byvar( df_out, siteinfo, dir, "PSurf" ) %>%
        dplyr::rename(patm = myvar) %>%
        dplyr::right_join(df_out, by = c("sitename", "date"))
    }
    
    # PPFD
    if ("ppfd" %in% getvars){
      kfFEC <- 2.04
      df_out <- ingest_globalfields_wfde5_byvar( df_out, siteinfo, dir, "SWdown" ) %>%
        dplyr::mutate(ppfd = myvar * kfFEC * 1.0e-6 ) %>%  # W m-2 -> mol m-2 s-1
        dplyr::right_join(df_out, by = c("sitename", "date"))
    }
    
    # short-wave irradiation
    if ("swin" %in% getvars && !("swin" %in% names(df_out))){
      df_out <- ingest_globalfields_wfde5_byvar( df_out, siteinfo, dir, "SWdown" ) %>%
        dplyr::rename(swin = myvar) %>%
        dplyr::right_join(df_out, by = c("sitename", "date"))
    }
    
    # long-wave irradiation
    if ("lwin" %in% getvars && !("lwin" %in% names(df_out))){
      df_out <- ingest_globalfields_wfde5_byvar( df_out, siteinfo, dir, "LWdown" ) %>%
        dplyr::rename(lwin = myvar) %>%
        dplyr::right_join(df_out, by = c("sitename", "date"))
    }
    
    # wind
    if ("wind" %in% getvars && !("wind" %in% names(df_out))){
      df_out <- ingest_globalfields_wfde5_byvar( df_out, siteinfo, dir, "Wind" ) %>%
        dplyr::rename(wind = myvar) %>%
        dplyr::right_join(df_out, by = c("sitename", "date"))
    }
    
    # remove spurious myvar columns
    df_out <- df_out %>%
      dplyr::select(-starts_with("myvar"))
    
    if (timescale=="m"){
      rlang::abort("ingest_globalfields(): aggregating WATCH-WFDEI to monthly not implemented yet.")
    }
    
  } else if (source=="cru"){
    
    # Read CRU monthly data (extracting from NetCDF files for this site)
    
    # create a monthly data frame
    mdf <- df_out %>%
      dplyr::select(sitename, date) %>%
      dplyr::mutate(year = lubridate::year(date), moy = lubridate::month(date)) %>%
      dplyr::select(sitename, year, moy) %>%
      dplyr::distinct()
    
    cruvars <- c()
    
    # temperature (daily mean air)
    if ("temp" %in% getvars){
      cruvars <- c(cruvars, "temp")
      mdf <- ingest_globalfields_cru_byvar(siteinfo, dir, "tmp" ) %>%
        dplyr::select(sitename, date, myvar) %>%
        dplyr::rename(temp = myvar) %>%
        dplyr::mutate(year = lubridate::year(date), moy = lubridate::month(date)) %>%
        dplyr::select(-date) %>%
        dplyr::right_join(mdf, by = c("sitename", "year", "moy"))
    }
    
    # daily minimum temperature
    if ("tmin" %in% getvars){
      cruvars <- c(cruvars, "tmin")
      mdf <- ingest_globalfields_cru_byvar(siteinfo, dir, "tmn" ) %>%
        dplyr::select(sitename, date, myvar) %>%
        dplyr::rename(tmin = myvar) %>%
        dplyr::mutate(year = lubridate::year(date), moy = lubridate::month(date)) %>%
        dplyr::select(-date) %>%
        dplyr::right_join(mdf, by = c("sitename", "year", "moy"))
    }
    
    # daily maximum temperature
    if ("tmax" %in% getvars){
      cruvars <- c(cruvars, "tmax")
      mdf <- ingest_globalfields_cru_byvar(siteinfo, dir, "tmx" ) %>%
        dplyr::select(sitename, date, myvar) %>%
        dplyr::rename(tmax = myvar) %>%
        dplyr::mutate(year = lubridate::year(date), moy = lubridate::month(date)) %>%
        dplyr::select(-date) %>%
        dplyr::right_join(mdf, by = c("sitename", "year", "moy"))
    }
    
    # precipitation
    if ("prec" %in% getvars){
      cruvars <- c(cruvars, "prec")
      mdf <- ingest_globalfields_cru_byvar(siteinfo, dir, "pre" ) %>%
        dplyr::select(sitename, date, myvar) %>%
        dplyr::rename(prec = myvar) %>%
        dplyr::mutate(year = lubridate::year(date), moy = lubridate::month(date)) %>%
        dplyr::select(-date) %>%
        dplyr::right_join(mdf, by = c("sitename", "year", "moy"))
      
      # also get wet days to generate daily values
      cruvars <- c(cruvars, "wetd")
      mdf <- ingest_globalfields_cru_byvar(siteinfo,  dir, "wet" ) %>%
        dplyr::select(sitename, date, myvar) %>%
        dplyr::rename(wetd = myvar) %>%
        dplyr::mutate(year = lubridate::year(date), moy = lubridate::month(date)) %>%
        dplyr::select(-date) %>%
        dplyr::right_join(mdf, by = c("sitename", "year", "moy"))
    }
    
    # vpd from vapour pressure
    if ("vpd" %in% getvars){
      cruvars <- c(cruvars, "vap")
      mdf <- ingest_globalfields_cru_byvar(siteinfo, dir, "vap" ) %>%
        dplyr::select(sitename, date, myvar) %>%
        dplyr::rename(vap = myvar) %>%
        dplyr::mutate(year = lubridate::year(date), moy = lubridate::month(date)) %>%
        dplyr::select(-date) %>%
        dplyr::right_join(mdf, by = c("sitename", "year", "moy"))
      
      # also get daily minimum and maximum temperature to convert vapour pressure to vpd
      if (!("tmin" %in% names(mdf))){
        if (!("tmin" %in% cruvars)) cruvars <- c(cruvars, "tmin")
        mdf <- ingest_globalfields_cru_byvar(siteinfo, dir, "tmn" ) %>%
          dplyr::select(sitename, date, myvar) %>%
          dplyr::rename(tmin = myvar) %>%
          dplyr::mutate(year = lubridate::year(date), moy = lubridate::month(date)) %>%
          dplyr::select(-date) %>%
          dplyr::right_join(mdf, by = c("sitename", "year", "moy"))
      }
      
      if (!("tmax" %in% names(mdf))){
        if (!("tmax" %in% cruvars)) cruvars <- c(cruvars, "tmax")
        mdf <- ingest_globalfields_cru_byvar(siteinfo, dir, "tmx" ) %>%
          dplyr::select(sitename, date, myvar) %>%
          dplyr::rename(tmax = myvar) %>%
          dplyr::mutate(year = lubridate::year(date), moy = lubridate::month(date)) %>%
          dplyr::select(-date) %>%
          dplyr::right_join(mdf, by = c("sitename", "year", "moy"))
      }      
      
    }
    
    # cloud cover
    if ("ccov" %in% getvars){
      cruvars <- c(cruvars, "ccov")
      mdf <- ingest_globalfields_cru_byvar(siteinfo, dir, "cld" ) %>%
        dplyr::select(sitename, date, myvar) %>%
        dplyr::rename(ccov = myvar) %>%
        dplyr::mutate(year = lubridate::year(date), moy = lubridate::month(date)) %>%
        dplyr::select(-date) %>%
        dplyr::right_join(mdf, by = c("sitename", "year", "moy"))
    }
    
    if (timescale == "d"){
      
      # expand monthly to daily data
      
      if (length(cruvars)>0){
        df_out <- expand_clim_cru_monthly( mdf, cruvars ) %>%
          right_join( df_out, by = "date" )
      }
      
      if ("vpd" %in% getvars){
        # Calculate VPD based on monthly data (vap is in hPa) - important: after downscaling to daily because of non-linearity
        df_out <- df_out %>% 
          rowwise() %>%
          mutate(vpd = calc_vpd( eact = 1e2 * vap, tmin = tmin, tmax = tmax ))
        
      }
      
      if ("prec" %in% getvars){
        # convert units -> mm/sec
        df_out <- df_out %>% 
          mutate(prec = prec / (60 * 60 * 24))  # mm/d -> mm/sec
      }
      
    } else if (timescale == "m"){
      
      if ("vpd" %in% getvars){
        # Calculate VPD based on monthly data (vap is in hPa)
        mdf <- mdf %>% 
          rowwise() %>%
          mutate(vpd = calc_vpd( eact = 1e2 * vap, tmin = tmin, tmax = tmax ))
      }
      
      df_out <- mdf %>% 
        right_join(df_out %>% 
                     mutate(year = lubridate::year(date), moy = lubridate::month(date)), 
                   by = c("sitename", "year", "moy")) %>% 
        dplyr::select(-year, -moy)
      
      if ("prec" %in% getvars){
        # convert units -> mm/sec
        df_out <- df_out %>% 
          mutate(moy = lubridate::month(date)) %>% 
          mutate(prec = prec / days_in_month(moy)) %>%   # mm/month -> mm/d
          mutate(prec = prec / (60 * 60 * 24))  # mm/d -> mm/sec
      }
    }
    
  } else if (source == "ndep"){
    
    # create a annual data frame
    adf <- df_out %>%
      dplyr::select(sitename, date) %>%
      dplyr::filter(lubridate::yday(date)==1) %>% 
      dplyr::distinct()
    
    # extract the data for NHx
    adf <- ingest_globalfields_ndep_byvar(siteinfo, dir, "nhx") %>%
      dplyr::select(sitename, date, nhx) %>%
      dplyr::right_join(adf, by = c("sitename", "date"))
    
    # extract the data for NOy
    adf <- ingest_globalfields_ndep_byvar(siteinfo, dir, "noy") %>%
      dplyr::select(sitename, date, noy) %>%
      dplyr::right_join(adf, by = c("sitename", "date"))
    
    
    if (timescale != "y"){
      stop("ingest_globalfields() for source = ndep: come up with solution for non-annual time step")
    } else {
      df_out <- adf
    }
    
  } else if (source == "etopo1"){
    
    filename <- list.files(dir, pattern = "ETOPO1_Bed_g_geotiff.tif")
    if (length(filename) > 1) stop("ingest_globalfields(): Found more than 1 file for source 'etopo1'.")
    if (length(filename) == 0) stop("ingest_globalfields(): Found no files for source 'etopo1' in the directory provided by argument 'dir'.")
    
    # re-construct this data frame (tibble) - otherwise SpatialPointsDataframe() won't work
    df_lonlat <- tibble(
      sitename = siteinfo$sitename,
      lon      = siteinfo$lon,
      lat      = siteinfo$lat
    )
    
    df_out <- extract_pointdata_allsites( paste0(dir, filename), df_lonlat, get_time = FALSE ) |>
      dplyr::ungroup() |> 
      dplyr::select(-lon, -lat) |>
      tidyr::unnest(data) |>
      dplyr::rename(elv = ETOPO1_Bed_g_geotiff) |>
      dplyr::select(sitename, elv)
    
  } else if (source == "stocker23"){
    
    filename <- list.files(dir, pattern = "cwdx80_forcing_halfdeg.nc")
    if (length(filename) > 1) stop("ingest_globalfields(): Found more than 1 file for source 'stocker23'.")
    if (length(filename) == 0) stop("ingest_globalfields(): Found no files for source 'stocker23' in the directory provided by argument 'dir'.")
    
    # re-construct this data frame (tibble) - otherwise SpatialPointsDataframe() won't work
    df_lonlat <- tibble(
      sitename = siteinfo$sitename,
      lon      = siteinfo$lon,
      lat      = siteinfo$lat
    )
    
    df_out <- extract_pointdata_allsites( paste0(dir, filename), df_lonlat, get_time = FALSE ) |>
      dplyr::ungroup() |> 
      dplyr::select(-lon, -lat) |>
      tidyr::unnest(data) |>
      dplyr::rename(whc = cwdx80_forcing) |>
      dplyr::select(sitename, whc)
    
  } else if (source == "gsde"){
    
    # re-construct this data frame (tibble) - otherwise SpatialPointsDataframe() won't work
    df_lonlat <- tibble(
      sitename = siteinfo$sitename,
      lon      = siteinfo$lon,
      lat      = siteinfo$lat
    )
    
    # top soil layers
    filename <- list.files(dir, pattern = paste0(layer, "1.nc"))
    if (length(filename) > 1) stop("ingest_globalfields(): Found more than 1 file for source 'gsde'.")
    if (length(filename) == 0) stop("ingest_globalfields(): Found no files for source 'gsde' in the directory provided by argument 'dir'.")
    df_out_top <- extract_pointdata_allsites( paste0(dir, "/", filename), df_lonlat, get_time = FALSE ) %>%
      dplyr::select(-lon, -lat) %>%
      tidyr::unnest(data) %>%
      tidyr::pivot_longer(cols = starts_with("PBR_depth")) %>% 
      dplyr::rename(!!layer := value, depth = name) %>%
      dplyr::mutate(depth = as.numeric(str_remove(depth, "PBR_depth="))) %>% 
      dplyr::select(sitename, !!layer, depth)
    
    # bottom soil layers
    filename <- list.files(dir, pattern = paste0(layer, "2.nc"))
    if (length(filename) > 1) stop("ingest_globalfields(): Found more than 1 file for source 'gsde'.")
    if (length(filename) == 0) stop(paste("ingest_globalfields(): Found no files for source 'gsde' in the directory provided by argument 'dir' for layer", layer))
    df_out_bottom <- extract_pointdata_allsites( paste0(dir, "/", filename), df_lonlat, get_time = FALSE ) %>%
      dplyr::select(-lon, -lat) %>%
      tidyr::unnest(data) %>%
      tidyr::pivot_longer(cols = starts_with("PBR_depth")) %>% 
      dplyr::rename(!!layer := value, depth = name) %>%
      dplyr::mutate(depth = as.numeric(str_remove(depth, "PBR_depth="))) %>% 
      dplyr::select(sitename, !!layer, depth)
    
    # combine for layers read from each file
    df_out <- bind_rows(df_out_top, df_out_bottom) %>% 
      group_by(sitename) %>% 
      tidyr::nest() %>% 
      mutate(data = purrr::map(data, ~mutate(., layer = 1:8))) %>% 
      tidyr::unnest(data)
    
    # apply conversion factor
    df_conv <- tibble(varnam := c("TC", "OC", "TN", "PHH2O", "PHK", "PHCA", "EXA", "PBR", "POL", "PNZ", "PHO", "PMEH", "TP", "TK"),    
                      fact = c(0.01, 0.01, 0.01, 0.1, 0.1, 0.1, 0.01, 0.01, 0.01, 0.01, 0.0001, 0.01, 0.0001, 0.01))
    
    df_out <- df_out %>% 
      mutate(varnam = !!layer) %>% 
      left_join(df_conv, by = "varnam") %>%
      rename(value = !!layer) %>% 
      
      # interpret missing values
      ungroup() %>% 
      mutate(value = ifelse(value == -999, NA, value)) %>% 
      mutate(value = ifelse(varnam %in% c("PHH2O", "PHK", "PHCA") & value == 100,
                            NA,
                            value)) %>% 
      
      # apply conversion factor
      mutate(value = value * fact) %>% 
      dplyr::select(-fact, -varnam) %>% 
      rename(!!layer := value)
    
  } else if (source == "wwf"){
    
    df_biome_codes <- tibble(
      BIOME = 1:14,
      BIOME_NAME = c(
        "Tropical & Subtropical Moist Broadleaf Forests",
        "Tropical & Subtropical Dry Broadleaf Forests",
        "Tropical & Subtropical Coniferous Forests",
        "Temperate Broadleaf & Mixed Forests",
        "Temperate Conifer Forests",
        "Boreal Forests/Taiga",
        "Tropical & Subtropical Grasslands, Savannas & Shrublands",
        "Temperate Grasslands, Savannas & Shrublands",
        "Flooded Grasslands & Savannas",
        "Montane Grasslands & Shrublands",
        "Tundra",
        "Mediterranean Forests, Woodlands & Scrub",
        "Deserts & Xeric Shrublands",
        "Mangroves")
    )
    
    df_out <- extract_pointdata_allsites_shp( dir, dplyr::select(siteinfo, sitename, lon, lat), layer ) %>%
      left_join(df_biome_codes, by = "BIOME")
    
    
  } else if (source == "worldclim"){
    
    # re-construct this data frame (tibble) - otherwise SpatialPointsDataframe() won't work
    df_lonlat <- tibble(
      sitename = siteinfo$sitename,
      lon      = siteinfo$lon,
      lat      = siteinfo$lat
    )
    
    ingest_globalfields_worldclim_byvar <- function(varnam){
      
      vec_filn <- list.files(dir, pattern = paste0(varnam, ".*.tif"))
      
      if (length(vec_filn) > 0){
        df_out <- purrr::map2(
          as.list(vec_filn),
          as.list(stringr::str_remove(vec_filn,
                                      paste0("wc2.1_30s_", varnam, "_")) %>%
                    stringr::str_remove(".tif")),
          ~{extract_pointdata_allsites( paste0(dir, "/", .x),
                                        df_lonlat, get_time = FALSE ) %>%
              dplyr::select(-lon, -lat) %>%
              tidyr::unnest(data) %>%
              dplyr::rename(!!paste0(varnam, "_", .y) := V1) %>%
              dplyr::select(sitename, !!paste0(varnam, "_", .y))}) %>% 
          purrr::reduce(left_join, by = "sitename")
      } else {
        df_out <- tibble()
      }
      
      return(df_out)
    }
    
    df_out <- purrr::map(as.list(layer),
                         ~ingest_globalfields_worldclim_byvar(.)) %>% 
      purrr::reduce(left_join, by = c("sitename"))
    
  }
  
  return( df_out )
  
}


# Extract temperature time series for a set of sites at once (opening
# each file only once).

ingest_globalfields_watch_byvar <- function( ddf, siteinfo, dir, varnam ) {
  
  # define variables
  yr <- mo <- filename <- drop_na <- data <- sitename <- 
    dom <- myvar <- doy <- data_pre <- . <- NULL
  
  dirn <- paste0( dir, "/", varnam)
  
  # loop over all year and months that are required
  year_start <- ddf %>%
    dplyr::pull(date) %>%
    min() %>%
    lubridate::year()
  
  year_end <- ddf %>%
    dplyr::pull(date) %>%
    max() %>%
    lubridate::year()
  
  # check if data is required for years before 1979 (when watch wfdei is available)
  pre_data <- year_start < 1979
  
  # if pre-1979 data are required, read at least 10 first years to get mean climatology
  if (pre_data){
    year_start_read <- 1979
    year_end_read <- max(1988, year_end)
  } else {
    year_start_read <- year_start
    year_end_read <- year_end
  }
  
  # construct data frame holding longitude and latitude info
  df_lonlat <- tibble(
    sitename = siteinfo$sitename,
    lon      = siteinfo$lon,
    lat      = siteinfo$lat
  )
  
  if (varnam %in% c("Rainf_daily", "Snowf_daily")){
    addstring <- "_WFDEI_CRU_"
  } else {
    addstring <- "_WFDEI_"
  }
  
  # extract all the data for all the dates (cutting to required dates by site is done in ingest())
  allmonths <- 1:12
  allyears <- year_start_read:year_end_read
  df <- expand.grid(allmonths, allyears) %>%
    dplyr::as_tibble() %>%
    stats::setNames(c("mo", "yr")) %>%
    rowwise() %>%
    dplyr::mutate(filename = paste0( dirn, "/", varnam, addstring, sprintf( "%4d", yr ), sprintf( "%02d", mo ), ".nc" )) %>%
    ungroup() %>%
    dplyr::mutate(data = purrr::map(filename, ~extract_pointdata_allsites(., df_lonlat, get_time = TRUE ) ))
  
  # rearrange to a daily data frame
  complement_df <- function(df){
    df <- df |> 
      dplyr::select(dom = tstep, myvar = value)
    return(df)
  }
  
  ddf <- df %>%
    tidyr::unnest(data) %>%
    dplyr::mutate(data = purrr::map(data, ~complement_df(.))) %>%
    tidyr::unnest(data) %>%
    dplyr::select(sitename, mo, yr, dom, myvar) %>%
    dplyr::mutate(date = lubridate::ymd(paste0(as.character(yr), "-", sprintf( "%02d", mo), "-", sprintf( "%02d", dom))) ) %>%
    dplyr::select(-mo, -yr, -dom)
  
  # create data frame containing all dates, using mean annual cycle (of 1979-1988) for all years before 1979
  if (pre_data){
    message("Data for years before 1979 requested. Taking mean annual cycle of 10 years (1979-1988) for all years before 1979.")
    
    # get mean seasonal cycle, averaged over 1979:1988
    ddf_meandoy <- ddf %>% 
      dplyr::filter(lubridate::year(date) %in% 1979:1988) %>% 
      mutate(doy = lubridate::yday(date)) %>% 
      group_by(sitename, doy) %>% 
      summarise(myvar = mean(myvar))
    
    # get a data frame with all dates for all sites
    ddf_tmp <- purrr::map(
      as.list(seq(nrow(siteinfo))),
      ~ingestr::init_dates_dataframe(
        lubridate::year(siteinfo$date_start[.]),
        min(1978, lubridate::year(siteinfo$date_end[.])),
        noleap = TRUE,
        timescale = "d"))
    names(ddf_tmp) <- siteinfo$sitename
    ddf_pre <- ddf_tmp %>%
      bind_rows(.id = "sitename") %>%
      drop_na() %>% 
      mutate(doy = lubridate::yday(date)) %>%
      left_join(ddf_meandoy, by = c("sitename", "doy")) %>%
      dplyr::select(-doy)
    
    # ddf_pre <- init_dates_dataframe(year_start, min(1978, year_end)) %>% 
    #   mutate(doy = lubridate::yday(date)) %>% 
    #   left_join(ddf_pre, by = "doy") %>% 
    #   dplyr::select(-doy)
    
    # combine the two along rows
    ddf <- left_join(
      ddf %>% 
        ungroup() %>% 
        group_by(sitename) %>% 
        tidyr::nest(),
      ddf_pre %>% 
        ungroup() %>% 
        group_by(sitename) %>% 
        tidyr::nest() %>% 
        rename(data_pre = data),
      by = "sitename") %>% 
      mutate(data = purrr::map2(data_pre, data, ~bind_rows(.x, .y))) %>% 
      dplyr::select(-data_pre) %>% 
      tidyr::unnest(data) %>% 
      arrange(date) %>%   # to make sure
      distinct() # out of desperation
  }
  
  return( ddf )
}

ingest_globalfields_wfde5_byvar <- function(ddf, siteinfo, dir, varnam) {
  
  yr <- mo  <- filename <- . <- data <- sitename <- dom <- hod <- 
    myvar <- doy <- drop_na <- data_pre <- NULL
  
  dirn <- paste0( dir, "/", varnam)
  
  # loop over all year and months that are required
  year_start <- ddf %>%
    dplyr::pull(date) %>%
    min() %>%
    lubridate::year()
  
  year_end <- ddf %>%
    dplyr::pull(date) %>%
    max() %>%
    lubridate::year()
  
  # check if data is required for years before 1980 (when watch wfdei is available)
  pre_data <- year_start < 1980
  
  # if pre-1980 data are required, read at least 10 first years to get mean climatology
  if (pre_data){
    year_start_read <- 1980
    year_end_read <- max(1989, year_end)
  } else {
    year_start_read <- year_start
    year_end_read <- year_end
  }
  
  # construct data frame holding longitude and latitude info
  df_lonlat <- tibble(
    sitename = siteinfo$sitename,
    lon      = siteinfo$lon,
    lat      = siteinfo$lat
  )
  
  if (varnam %in% c("Rainf", "Snowf")){
    addstring <- "_WFDE5_CRU+GPCC_"
  } else {
    addstring <- "_WFDE5_CRU_"
  }
  
  if (varnam %in% c("Tair", "Qair")) {
    endstring <- "_v1.0"
  } else {
    endstring <- "_v1.1"
  }
  
  # extract all the data for all the dates (cutting to required dates by site is done in ingest())
  alldays   <- 1:31
  allmonths <- 1:12
  allyears <- year_start_read:year_end_read
  df <- 
    expand.grid(allmonths, allyears) %>%
    dplyr::as_tibble() %>%
    stats::setNames(c("mo", "yr")) %>%
    rowwise() %>%
    dplyr::mutate(
      filename = paste0( dirn, "/", varnam, addstring, sprintf( "%4d", yr ),
                         sprintf( "%02d", mo ), endstring, ".nc" )) %>%
    ungroup() %>%
    dplyr::mutate(
      data = purrr::map(
        filename,
        ~extract_pointdata_allsites(., df_lonlat, get_time = FALSE ) ))
  
  # rearrange to a daily data frame
  complement_df <- function(df){
    df <- df %>%
      stats::setNames(., c("myvar")) %>%
      mutate(row = 1:nrow(.),
             hod = rep(0:23, nrow(.)/24),
             dom = ceiling(row/24))
    return(df)
  }
  
  ddf <- df %>%
    tidyr::unnest(data) %>%
    dplyr::mutate(data = purrr::map(data, ~complement_df(.))) %>%
    tidyr::unnest(data) %>%
    dplyr::select(sitename, mo, yr, dom, hod, myvar) %>%
    dplyr::mutate(
      date = lubridate::ymd_h(
        paste0(as.character(yr),
               "-", sprintf( "%02d", mo),
               "-", sprintf( "%02d", dom),
               " ", sprintf( "%02d", hod)))
      ) %>%
    dplyr::select(-mo, -yr, -dom, -hod)
  
  # create data frame containing all dates, using mean annual cycle
  # (of 1980-1989) for all years before 1980
  if (pre_data){
    message("
      Data for years before 1979 requested.
      Taking mean annual cycle of 10 years (1979-1989)
      for all years before 1979.")
    message("
      NCDF file on Euler lacks first seven hours of 1979-01-01.
      Thus, only cycle from 1980-1989 is taken.")
    
    # get mean seasonal cycle, averaged over 1980:1989
    ddf_mean <- ddf %>% 
      dplyr::filter(lubridate::year(date) %in% 1980:1989) %>% 
      mutate(hod = rep(0:23, nrow(.)/24),
             doy = lubridate::yday(date)) %>% 
      group_by(sitename, hod, doy) %>% 
      summarise(myvar = mean(myvar), .groups = "keep")
    
    # get a data frame with all dates for all sites
    ddf_tmp <- purrr::map(
      as.list(seq(nrow(siteinfo))),
      ~ingestr::init_dates_dataframe(
        lubridate::year(siteinfo$date_start[.]),
        min(1979, lubridate::year(siteinfo$date_end[.])),
        noleap = TRUE,
        timescale = timescale))
    
    names(ddf_tmp) <- siteinfo$sitename
    
    ddf_pre <- ddf_tmp %>%
      bind_rows(.id = "sitename") %>%
      drop_na() %>% 
      mutate(hod = rep(0:23, nrow(.)/24),
             doy = lubridate::yday(date)) %>%
      left_join(ddf_mean, by = c("sitename", "doy", "hod")) %>%
      dplyr::select(-doy, -hod)
    
    # combine the two along rows
    ddf <- left_join(
      ddf %>% 
        ungroup() %>% 
        group_by(sitename) %>% 
        tidyr::nest(),
      ddf_pre %>% 
        ungroup() %>% 
        group_by(sitename) %>% 
        tidyr::nest() %>% 
        rename(data_pre = data),
      by = "sitename") %>% 
      mutate(data = purrr::map2(data_pre, data, ~bind_rows(.x, .y))) %>% 
      dplyr::select(-data_pre) %>% 
      tidyr::unnest(data) %>% 
      arrange(date) %>%   # to make sure
      distinct() # out of desperation
  }
  
  return( ddf )
}


# Extract N deposition time series for a set of sites at once (opening
# each file only once).

ingest_globalfields_ndep_byvar <- function(siteinfo, dir, varnam){
  
  # define variable
  data <- NULL
  
  # construct data frame holding longitude and latitude info
  df_lonlat <- tibble(
    sitename = siteinfo$sitename,
    lon      = siteinfo$lon,
    lat      = siteinfo$lat
  )
  
  # extract the data
  filename <- list.files(
    dir, paste0("ndep_", varnam, "_lamarque11cc_historical_halfdeg.nc") )
  df <- extract_pointdata_allsites(
    paste0(dir, filename), df_lonlat, get_time = TRUE) %>%
    dplyr::mutate(
      data = purrr::map(data, ~stats::setNames(., c(varnam, "year")))
      ) %>% 
    dplyr::mutate(
      data = purrr::map(
        data,
        ~mutate(., date = lubridate::ymd(paste0(as.character(year), "-01-01")))
        )
      )
  
  adf <- df %>%
    tidyr::unnest(data)
  
  return(adf)
}


# Extract temperature time series for a set of sites at once (opening
# each file only once).

ingest_globalfields_cru_byvar <- function( siteinfo, dir, varnam ){
  
  # define variables
  data <- year <- moy <- NULL 
  
  # construct data frame holding longitude and latitude info
  df_lonlat <- tibble(
    sitename = siteinfo$sitename,
    lon      = siteinfo$lon,
    lat      = siteinfo$lat
  )
  
  # extract the data
  filename <- list.files( dir, pattern=paste0( varnam, ".dat.nc" ) )
  if (length(filename)==0) stop(paste("Aborting. No files found for CRU variable", varnam))
  df <- extract_pointdata_allsites( paste0(dir, filename), df_lonlat, get_time = TRUE ) %>%
    dplyr::mutate(data = purrr::map(data, ~stats::setNames(., c("myvar", "date"))))
  
  # rearrange to a monthly data frame. Necesary work-around with date,
  # because unnest() seems to have a bug
  # when unnesting a dataframe that contains a lubridate ymd objet.
  get_month_year <- function(df){
    df %>%
      mutate(year = lubridate::year(date),
             moy = lubridate::month(date))
  }
  
  mdf <- df %>%
    mutate(data = purrr::map(data, ~get_month_year(.))) %>%
    mutate(data = purrr::map(data, ~dplyr::select(., -date))) %>%
    tidyr::unnest(data) %>%
    rowwise() %>%
    mutate(date = lubridate::ymd(paste0(as.character(year),
                                        "-", sprintf( "%02d", moy), "-15"))) %>%
    dplyr::select(-year, -moy)
  
  return( mdf )
}


# Interpolates monthly data to daily data using polynomials or linear
# for a single year

expand_clim_cru_monthly <- function( mdf, cruvars ){
  
  ddf <- purrr::map(as.list(unique(mdf$year)),
      ~expand_clim_cru_monthly_byyr( ., mdf, cruvars ) ) %>%
    bind_rows()
  
  return( ddf )
  
}


# Interpolates monthly data to daily data using polynomials or linear
# for a single year

expand_clim_cru_monthly_byyr <- function( yr, mdf, cruvars ){

  # define variables  
  year <- ccov_int <- NULL
  nmonth <- 12
  
  startyr <- mdf$year %>% first()
  endyr   <- mdf$year %>% last()
  
  yr_pvy <- max(startyr, yr-1)
  yr_nxt <- min(endyr, yr+1)
  
  # add first and last year to head and tail of 'mdf'
  first <- mdf[1:12,] %>% mutate( year = year - 1)
  last  <- mdf[(nrow(mdf)-11):nrow(mdf),] %>% mutate( year = year + 1 )
  
  ddf <- init_dates_dataframe( yr, yr )
  
  
  # air temperature: interpolate using polynomial
  
  if ("temp" %in% cruvars){
    mtemp     <- dplyr::filter( mdf, year==yr     )$temp
    mtemp_pvy <- dplyr::filter( mdf, year==yr_pvy )$temp
    mtemp_nxt <- dplyr::filter( mdf, year==yr_nxt )$temp
    if (length(mtemp_pvy)==0){
      mtemp_pvy <- mtemp
    }
    if (length(mtemp_nxt)==0){
      mtemp_nxt <- mtemp
    }
    
    ddf <- init_dates_dataframe( yr, yr ) %>%
      mutate(
        temp = monthly2daily(
          mtemp,
          "polynom",
          mtemp_pvy[nmonth],
          mtemp_nxt[1],
          leapyear = lubridate::leap_year(yr) 
          ) 
        ) %>%
      right_join( ddf, by = c("date") )
  }
  
  
  # daily minimum air temperature: interpolate using polynomial
  
  if ("tmin" %in% cruvars){
    mtmin     <- dplyr::filter( mdf, year==yr     )$tmin
    mtmin_pvy <- dplyr::filter( mdf, year==yr_pvy )$tmin
    mtmin_nxt <- dplyr::filter( mdf, year==yr_nxt )$tmin
    if (length(mtmin_pvy)==0){
      mtmin_pvy <- mtmin
    }
    if (length(mtmin_nxt)==0){
      mtmin_nxt <- mtmin
    }
    
    ddf <- init_dates_dataframe( yr, yr ) %>%
      mutate( tmin = monthly2daily( mtmin, "polynom", mtmin_pvy[nmonth], mtmin_nxt[1], leapyear = lubridate::leap_year(yr) ) ) %>%
      right_join( ddf, by = c("date") )
  }
  
  
  # daily minimum air temperature: interpolate using polynomial
  
  if ("tmax" %in% cruvars){
    mtmax     <- dplyr::filter( mdf, year==yr     )$tmax
    mtmax_pvy <- dplyr::filter( mdf, year==yr_pvy )$tmax
    mtmax_nxt <- dplyr::filter( mdf, year==yr_nxt )$tmax
    if (length(mtmax_pvy)==0){
      mtmax_pvy <- mtmax
    }
    if (length(mtmax_nxt)==0){
      mtmax_nxt <- mtmax
    }
    
    ddf <- init_dates_dataframe( yr, yr ) %>%
      mutate( tmax = monthly2daily( mtmax, "polynom", mtmax_pvy[nmonth], mtmax_nxt[1], leapyear = lubridate::leap_year(yr) ) ) %>%
      right_join( ddf, by = c("date") )
  }
  
  
  # precipitation: interpolate using weather generator
  
  if ("prec" %in% cruvars){
    mprec <- dplyr::filter( mdf, year==yr )$prec
    mwetd <- dplyr::filter( mdf, year==yr )$wetd
    
    if (any(!is.na(mprec))&&any(!is.na(mwetd))){
      ddf <-  init_dates_dataframe( yr, yr ) %>%
        mutate( prec = get_daily_prec( mprec, mwetd, leapyear = lubridate::leap_year(yr) ) ) %>%
        right_join( ddf, by = c("date") )
    }
  }
  
  
  # cloud cover: interpolate using polynomial
  
  if ("ccov" %in% cruvars){
    mccov     <- dplyr::filter( mdf, year==yr     )$ccov
    mccov_pvy <- dplyr::filter( mdf, year==yr_pvy )$ccov
    mccov_nxt <- dplyr::filter( mdf, year==yr_nxt )$ccov
    if (length(mccov_pvy)==0){
      mccov_pvy <- mccov
    }
    if (length(mccov_nxt)==0){
      mccov_nxt <- mccov
    }
    
    ddf <-  init_dates_dataframe( yr, yr ) %>%
      mutate( ccov_int = monthly2daily( mccov, "polynom", mccov_pvy[nmonth], mccov_nxt[1], leapyear = lubridate::leap_year(yr) ) ) %>%
      # Reduce CCOV to a maximum 100%
      mutate( ccov = ifelse( ccov_int > 100, 100, ccov_int ) ) %>%
      right_join( ddf, by = c("date") )
  }
  
  
  # VPD: interpolate using polynomial
  
  if ("vap" %in% cruvars){
    mvap     <- dplyr::filter( mdf, year==yr     )$vap
    mvap_pvy <- dplyr::filter( mdf, year==yr_pvy )$vap
    mvap_nxt <- dplyr::filter( mdf, year==yr_nxt )$vap
    if (length(mvap_pvy)==0){
      mvap_pvy <- mvap
    }
    if (length(mvap_nxt)==0){
      mvap_nxt <- mvap
    }
    
    ddf <- init_dates_dataframe( yr, yr ) %>%
      mutate( vap = monthly2daily( mvap, "polynom", mvap_pvy[nmonth], mvap_nxt[1], leapyear = lubridate::leap_year(yr) ) ) %>%
      right_join( ddf, by = c("date") )
    
  }
  
  return( ddf )
  
}


# Finds the closest land cell in the CRU dataset at the same latitude

find_nearest_cruland_by_lat <- function( lon, lat, filn ){
  
  if (!requireNamespace("ncdf4", quietly = TRUE))
    stop("Please, install 'ncdf4' package")
  
  nc <- ncdf4::nc_open( filn, readunlim=FALSE )
  crufield <- ncdf4::ncvar_get( nc, varid="TMP" )
  lon_vec <- ncdf4::ncvar_get( nc, varid="LON" )
  lat_vec <- ncdf4::ncvar_get( nc, varid="LAT" )
  crufield[crufield==-9999] <- NA
  ncdf4::nc_close(nc)
  
  ilon <- which.min( abs( lon_vec - lon ) )
  ilat <- which.min( abs( lat_vec - lat ) )
  
  if (!is.na(crufield[ilon,ilat])) {print("WRONG: THIS SHOULD BE NA!!!")}
  for (n in seq(2*length(lon_vec))){
    ilon_look <- (-1)^(n+1)*round((n+0.1)/2)+ilon
    if (ilon_look > length(lon_vec)) {ilon_look <- ilon_look %% length(lon_vec)} # Wrap search around globe in latitudinal direction
    if (ilon_look < 1)               {ilon_look <- ilon_look + length(lon_vec) }
    print(paste("ilon_look",ilon_look))
    if (!is.na(crufield[ilon_look,ilat])) {
      break
    }
  }
  # if (!is.na(crufield[ilon_look,ilat])) {print("SUCCESSFULLY FOUND DATA")}
  return( lon_vec[ ilon_look ] )
  
}


# Extracts point data for a set of sites given by df_lonlat using
# functions from the raster package.

extract_pointdata_allsites <- function(
  filename,
  df_lonlat,
  get_time = FALSE
  ) {
  
  # define variables
  lon <- lat <- data <- NULL
  
  # load file using the raster library
  #print(paste("Creating raster brick from file", filename))
  if (!file.exists(filename)) stop(paste0("File not found: ", filename))
  
  # message(paste0("Reading file: ", filename))

  # new code with terra library
  rasta <- terra::rast(filename)
  coords <- dplyr::select(df_lonlat, lon, lat)
  points <- terra::vect(coords, geom = c("lon", "lat"), crs = "EPSG:4326")
  values <- terra::extract(rasta, points, xy = FALSE, ID = FALSE, method = "bilinear")
  
  if (get_time){

    out <- df_lonlat |> 
      dplyr::select(sitename, lon, lat) |> 
      bind_cols(
        values
      ) |> 
      tidyr::pivot_longer(-one_of(c("lon", "lat", "sitename")), names_to = "tstep") |> 
      tidyr::separate_wider_delim(
        tstep,
        delim = "=",
        names = c("varnam", "tstep")
      ) |> 
      dplyr::mutate(
        tstep = as.numeric(tstep) + 1,
        varnam = stringr::str_remove(varnam, "_tstep")
      ) |> 
      dplyr::group_by(sitename, lon, lat) |> 
      tidyr::nest()
    
  } else {
    
    out <- df_lonlat |> 
      dplyr::select(sitename, lon, lat) |> 
      bind_cols(
        values
      ) |> 
      dplyr::group_by(sitename, lon, lat) |> 
      tidyr::nest()
    
  }
  
  # # old code with {raster} library:
  # rasta <- raster::brick(filename)
  # df_lonlat <- raster::extract(
  #   rasta,
  #   sp::SpatialPoints(dplyr::select(df_lonlat, lon, lat)), # , proj4string = rasta@crs
  #   sp = TRUE
  # ) %>%
  #   as_tibble() %>%
  #   tidyr::nest(data = c(-lon, -lat)) %>%
  #   right_join(df_lonlat, by = c("lon", "lat")) %>%
  #   mutate( data = purrr::map(data, ~dplyr::slice(., 1)) ) %>%
  #   dplyr::mutate(data = purrr::map(data, ~t(.))) %>%
  #   dplyr::mutate(data = purrr::map(data, ~as_tibble(.)))
  # 
  # if (get_time){
  #   timevals <- raster::getZ(rasta)
  #   df_lonlat <- df_lonlat %>%
  #     mutate( data = purrr::map(data, ~bind_cols(., tibble(date = timevals))))
  # }
  
  return(out)
}


# Extracts point data for a set of sites given by df_lonlat for a
# shapefile. df_lonlat requires columns sitename, lon, and lat.

extract_pointdata_allsites_shp <- function(dir, df_lonlat, layer) {
  
  # solves error, see https://stackoverflow.com/questions/75927165/error-in-wk-handle-wk-wkbwkb-s2-geography-writeroriented-oriented-loop-0
  sf::sf_use_s2(FALSE)
  
  # Load spatial data using sf
  shp <- sf::st_read(dsn = dir, layer = layer)
  
  # Create SpatialPoints object for sites
  df_clean <- df_lonlat %>%
    ungroup() %>%
    dplyr::select(lon, lat) %>%
    tidyr::drop_na()
  
  # Create sf points object
  pts <- sf::st_as_sf(df_clean, coords = c("lon", "lat"), crs = sf::st_crs(shp))
  
  # Spatial join and data manipulation
  df <- sf::st_join(pts, shp) |> 
    dplyr::select(-geometry) |> 
    dplyr::bind_cols(df_lonlat)

  # Alternative fix:
  # define variables
  # lon <- lat <- . <- NULL

  # sf::sf_use_s2(FALSE)
  
  # create SpatialPoints object for plots
  # df_clean <- df_lonlat |>
  #   ungroup() |>
  #   tidyr::drop_na(c(lon, lat))
  
  # shp <- sf::st_read(dsn = dir, layer = layer)
  # pts <- sf::st_as_sf(
  #   df_clean |>
  #     dplyr::select(lon, lat), 
  #   coords = c("lon","lat"), 
  #   crs = sf::st_crs(shp)
  #   )
  # df <- sf::st_join(pts, shp, left = TRUE) |> 
  #   as_tibble() |>
  #   bind_cols(df_clean, .)
    # dplyr::select(-geometry) |> 
    # right_join(df_lonlat, by = join_by(lon, lat)) |>
    # dplyr::select(-lon, -lat)
 
  return(df)
}

# extract_pointdata_allsites_shp <- function( dir, df_lonlat, layer ){
  
#   # define variables
#   lon <- lat <- . <- NULL
  
#   shp <- rgdal::readOGR(dsn = dir, layer = layer)
  
#   geo.proj <- sp::proj4string(shp)
  
#   # create SpatialPoints object for sites
#   df_clean <- df_lonlat %>%
#     ungroup() %>%
#     dplyr::select(lon, lat) %>%
#     tidyr::drop_na()
  
#   pts <- sp::SpatialPoints(df_clean, proj4string = sp::CRS(geo.proj))
  
#   # creates object that assigns each site index to an ecoregion
#   df <- sp::over(pts, shp) %>%
#     as_tibble() %>%
#     bind_cols(df_clean, .) %>%
#     right_join(df_lonlat, by = c("lon", "lat")) %>%
#     dplyr::select(-lon, -lat)
  
#   return(df)
# }

#' Implements a weather generator
#'
#' Implements a weather generator to simulate daily precipitation, given the monthly total and the number of days with rain for each month.
#'
#' @param mval_prec A vector of twelve numeric values for monthly values of total precipitation.
#' @param mval_wet A vector of twelve integer values for the number of wet days in each month.
#' @param set_seed A logical specifying whether a random seed is set.
#' @param leapyear A logical specifying whether interpolation is done for a leap year (with 366 days).
#'
#' @return A named list of data frames (tibbles) containing input data for each site is returned.
#'
get_daily_prec <- function( mval_prec, mval_wet, set_seed=FALSE, leapyear=FALSE ){
  
  # Distributes monthly total precipitation to days, given number of
  # monthly wet days. Adopted from LPX.
  
  if (leapyear){
    ndaymonth <- c(31,29,31,30,31,30,31,31,30,31,30,31)
  } else {
    ndaymonth <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  }
  ndayyear <- sum(ndaymonth)
  nmonth <- length(ndaymonth)
  
  c1 <- 1.0
  c2 <- 1.2
  
  if (set_seed) {set.seed(0)}
  prdaily_random <- array( NA, dim=c(ndayyear,2))
  for (doy in 1:ndayyear){
    prdaily_random[doy,] <- stats::runif(2)
  }
  
  dval_prec <- rep(NA,ndayyear)
  doy <- 0
  prob <- 0.0
  prob_rain <- rep(NA,nmonth)
  mprecave <- rep(NA,nmonth)
  mprecip <- rep(NA,nmonth)
  for (moy in 1:nmonth){
    prob_rain[moy] <- 0.0
    mprecave[moy] <- 0.0
    mprecip[moy] <- 0.0
  }
  daysum <- 0
  
  set.seed( prdaily_random[1,1] * 1e7 )
  
  for (moy in 1:nmonth){
    if ( mval_wet[moy]<=1.0 ) {mval_wet[moy] <- 1.0}
    prob_rain[moy] <- mval_wet[moy] / ndaymonth[moy]
    mprecave[moy] <- mval_prec[moy] / mval_wet[moy]
    dry <- TRUE
    iloop <- 0
    
    
    while( dry ){
      iloop <- iloop + 1
      nwet <- 0
      for (dm in 1:ndaymonth[moy]){
        doy <- doy + 1
        
        # Transitional probabilities (Geng et al. 1986)
        if (doy>1) {
          if (dval_prec[doy-1] < 0.1) {
            prob <- 0.75 * prob_rain[moy]
          } else {
            prob <- 0.25 + (0.75 * prob_rain[moy])
          }
        }
        # Determine we randomly and use Krysanova / Cramer estimates of
        # parameter values (c1,c2) for an exponential distribution
        if (iloop==1) {
          vv <- prdaily_random[doy,1]
        } else {
          # problem: rand() generates a random number that leads to 
          # floating point exception
          vv <- stats::runif(1)
        }
        
        
        if (vv>prob) {
          dval_prec[doy] <- 0.0
        } else {
          nwet <- nwet + 1
          v1 <- prdaily_random[doy,2]
          dval_prec[doy] <- ((-log(v1))^c2) * mprecave[moy] * c1
          if (dval_prec[doy] < 0.1) dval_prec[doy] <- 0.0
        }
        
        mprecip[moy] <- mprecip[moy] + dval_prec[doy]
      }
      
      # If it never rained this month and mprec[moy]>0 and mval_wet[moy]>0, do
      # again
      dry <- (nwet==0 && iloop<50 && mval_prec[moy]>0.1)
      if (iloop>50) {
        print('Daily.F, prdaily: Warning stopped after 50 tries in cell')
      }
      
      # Reset counter to start of month
      if (dry) {
        doy <- doy - ndaymonth[moy]
      }
      
    } #while
    
    # normalise generated precipitation by monthly CRU values
    if ( moy > 1 ) {daysum <- daysum + ndaymonth[moy-1]}
    if ( mprecip[moy] < 1.0 ) {mprecip[moy] <- 1.0}
    for (dm in 1:ndaymonth[moy]){
      doy <- daysum + dm
      dval_prec[doy] <- dval_prec[doy] * (mval_prec[moy] / mprecip[moy])
      if ( dval_prec[doy] < 0.1 ) {dval_prec[doy] <- 0.0}
      # dval_prec[doy] <- mval_prec[moy] / ndaymonth[moy]  #no generator
    }
    
    # Alternative: equal distribution of rain for fixed number of wet days
    # prob <- prob_rain[moy] + prob
    # if (prob.ge.1.0) then
    #   dval_prec[doy] <- mprec[moy]
    #   prob <- prob-1.0
    # } else {
    #   dval_prec[doy] <- 0.0
    #   prob <- prob
    # }
    
  }
  
  return( dval_prec )
  
}


#' Interpolates monthly to daily values
#'
#' Implements different methods to interpolate from monthly to daily values,
#' including fitting a polynomial.
#'
#' @param mval A vector of twelve numeric values for monthly values.
#' @param method A character string specifying the method for interpolation. 
#'  Defaults to \code{"polynom"} for using a polynomial.
#' @param mval_prev The monthly value of the month before the twelve months for 
#'  which values are provided by argument \code{mval}.
#' @param mval_next The monthly value of the month after the twelve months for 
#'  which values are provided by argument \code{mval}.
#' @param leapyear A logical specifying whether interpolation is done for a 
#'  leap year (with 366 days).
#'
#' @return A named list of data frames (tibbles) containing 
#'  input data for each site is returned.
#'
monthly2daily <- function(
  mval,
  method="polynom",
  mval_prev=mval[nmonth],
  mval_next=mval[1],
  leapyear=FALSE 
  ) {
  
  # mval <- 20*sin( seq(0, 2*pi, 2*pi/11)-0.5*pi)
  # mval_prev <- mval[12]
  # mval_next <- mval[1]
  
  if (leapyear){
    ndaymonth <- c(31,29,31,30,31,30,31,31,30,31,30,31)
  } else {
    ndaymonth <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  }
  nmonth <- length(ndaymonth)
  dval <- rep(NA,sum(ndaymonth))
  
  if (method=="polynom"){
    
    # Starting conditons of december in previous year
    startt <- -30.5            # midpoint between Nov-Dec of previous year
    endt <- 0.5                # midpoint between Dec-Jan of this year
    dt <- 31.0                 # number of Dec days
    lastmonthtemp <- mval_prev # Dec mean temperature
    day <- 0                   # initialisation of this years days
    
    for (month in 1:nmonth){
      dtold <- dt
      dt <- (ndaymonth[month])
      startt <- endt
      endt <- endt + dt
      if (month<nmonth) {
        dtnew <- (ndaymonth[month+1])
        nextmonthtemp <- mval[month+1]
      } else {
        dtnew <- (ndaymonth[1])
        nextmonthtemp <- mval_next
      }
      
      starttemp <- (mval[month]*dt+lastmonthtemp*dtold)/(dt+dtold)
      endtemp <- (nextmonthtemp*dtnew+mval[month]*dt)/(dtnew+dt)
      deltatemp <- endtemp-starttemp
      
      # calculate vars for a,b,c coefficients in polynom y <- ax^2 +bx + c
      d2t <- endt^2.0 - startt^2.0
      d3t <- endt^3.0 - startt^3.0
      
      # Take a sheet of paper and try solve the polynom,
      # well here is the outcome
      polya <- (mval[month]*dt - deltatemp*d2t/dt/2.0 - 
                  starttemp*dt + deltatemp*startt) / 
        (d3t/3.0 - d2t^2.0/dt/2.0 - dt*startt^2.0 + startt*d2t)
      polyb <- deltatemp/dt - polya*(startt+endt)
      polyc <- starttemp - polya*startt^2.0 - polyb*startt
      
      # calculate daily values with the polynom function
      for (d in 1:ndaymonth[month]) {
        day <- day + 1
        dval[day] <- polya*(day)^2.0 + polyb*(day) + polyc
      }
      lastmonthtemp <- mval[month]
    }
    
    # calculate monthly means after interpolation - 
    # not absolutely identical to input
    mtempint <- rep(NA,nmonth)
    day <- 0
    for (m in 1:nmonth){
      mtempint[m] <- 0.0
      for (d in 1:ndaymonth[m]){
        day <- day + 1
        mtempint[m] <- mtempint[m]+dval[day]/(ndaymonth[m])
      }
    }
    
  } else if (method=="step"){
    
    dval[] <- rep( mval, times=ndaymonth )
    
  } else {
    print( "Method (2nd argument) not valid." )
  }
  
  return(dval)
  
}


# fills gaps (NAs) by (1.) linear interpolation, (2.) 
# extending first/last to head/tail

fill_gaps <- function( vec, is.prec = FALSE ){
  
  xvals <- seq(length(vec))
  
  if ( is.prec ){
    # assume precipitation = 0 where missing
    if (any(is.na(vec))){
      vec[ is.na(vec) ] <- 0.0
    }
    
  } else {
    # linear approximation
    if ( any(is.na(vec)) && any(!is.na(vec)) ){
      vec <- stats::approx( xvals, vec, xout=xvals )$y
    }
    
    # extend to missing in head and tail
    if ( any(is.na(vec))  && any(!is.na(vec)) ){
      for (idx in seq(length(vec))){
        if ( any( is.na( utils::tail( vec, n=idx ) ) ) &&
             any( !is.na(utils::tail( vec, n=(idx+1) ) ) ) ){
          if (length(vec[ (length(vec)-idx) ])>0){
            vec[ (length(vec)-idx+1):length(vec) ] <- vec[ (length(vec)-idx) ]
          } else {
            vec[ (length(vec)-idx+1):length(vec) ] <- NA
          }
          break
        }
      }
    }
    
  }
  
  return( vec )
  
}
