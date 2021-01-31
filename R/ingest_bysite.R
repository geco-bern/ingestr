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
  dir,
  settings = list(),
  timescale = "d",
  year_start = NA,
  year_end = NA,
  lon = ifelse(source=="fluxnet", NA),
  lat = ifelse(source=="fluxnet", NA),
  verbose = FALSE
  ){

  if (!(source %in% c("etopo1", "hwsd", "soilgrids", "wise"))){
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
  ## FLUXNET 2015 readin
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

  } else if (source == "cru" || source == "watch_wfdei" || source == "ndep"){
    #-----------------------------------------------------------
    # Get data from global fields and one single site
    #-----------------------------------------------------------
    siteinfo <- tibble(
        sitename = sitename,
        lon = lon,
        lat = lat) %>%
      mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>%
      mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31")))

    df_tmp <- ingest_globalfields(siteinfo,
                               source = source,
                               getvars = getvars,
                               dir = dir,
                               timescale = timescale,
                               verbose = FALSE
                              )

  } else if (source == "modis"){

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
    df_co2 <- climate::meteo_noaa_co2() %>%
      dplyr::select(year = yy, month = mm, co2_avg)

    df_tmp <- init_dates_dataframe( year_start, year_end ) %>%
      dplyr::mutate(month = month(date), year = year(date)) %>%
      dplyr::left_join(
        df_co2,
        by = c("year", "month")
      ) %>%
      dplyr::mutate(sitename = sitename) %>%
      dplyr::select(sitename, date, co2 = co2_avg)


  }  else if (source == "fapar_unity"){
    #-----------------------------------------------------------
    # Assume fapar = 1 for all dates
    #-----------------------------------------------------------
    df_tmp <- init_dates_dataframe( year_start, year_end ) %>%
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
    siteinfo <- data.frame(
      id = sitename,
      longitude = lon,
      latitude = lat
    )

    spdata <- sf::st_as_sf(siteinfo, coords = c("longitude", "latitude"), crs = 4326)

    igh <- '+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs'
    spdata_igh <- sf::st_transform(spdata, igh)

    data_igh <- data.frame(sf::st_coordinates(spdata_igh), id = spdata_igh$id)

    fun_pixel_values  <- function(rowPX, data, VOI, VOI_LYR){
      as.numeric(
        gdallocationinfo(
          srcfile = paste0(settings$webdav_path, "/", VOI, "/", VOI_LYR, ".vrt"),
          x = data[rowPX, "X"],
          y = data[rowPX, "Y"],
          geoloc = TRUE,
          valonly = TRUE))
    }

    #value_pixels <- unlist(lapply(1:nrow(siteinfo), function(x){fun_pixel_values(x, data_igh, settings$voi, settings$voi_layer)}))
    value_pixels <- fun_pixel_values(1, data_igh, settings$voi, settings$voi_layer)
    df <- siteinfo %>%
      as_tibble() %>%
      mutate(var = value_pixels) %>%
      dplyr::select(-longitude, -latitude) %>%
      rename(sitename = id, !!settings$voi := var) %>%
      group_by(sitename) %>%
      nest()

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
        nest()
    } else {
      df <- df %>%
        right_join(dplyr::select(siteinfo, sitename, lon, lat), by = c("lon", "lat")) %>%
        dplyr::select(-lon, -lat) %>%
        group_by(sitename) %>%
        nest()

    }

  } else {
    rlang::warn(paste("you selected source =", source))
    rlang::abort("ingest(): Argument 'source' could not be identified. Use one of 'fluxnet', 'cru', 'watch_wfdei', 'co2_mlo', 'etopo1', or 'gee'.")
  }

  ## add data frame to nice data frame containing all required time steps
  if (!(source %in% c("etopo1", "hwsd", "soilgrids", "wise"))){
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
