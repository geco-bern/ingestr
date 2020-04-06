#' Data ingest for a single site
#'
#' Ingests data for a single site and one specific data type, specified by argument \code{source}.
#'
#' @param sitename A character string used as site identification. When data is extracted from 
#' global files or remote servers, \code{sitename} is simply used as a label and any string can
#' be provided. When data is extraced from site-specific files (e.g. \code{source = "fluxnet"}),
#' then \code{sitename} is used to identify the file from which data is read. 
#' @param source A character used as identifiyer for the type of data source
#' (e.g., \code{"fluxnet"}).
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
  year_start,
  year_end,
  lon = ifelse(source=="fluxnet", NA),
  lat = ifelse(source=="fluxnet", NA),
  verbose = FALSE
  ){
  
  ## initialise data frame with all required dates
  if (timescale=="d"){
    freq = "days"
  } else if (timescale=="m"){
    freq = "months"
  } else if (timescale=="y"){
    freq = "years"
  }
  
  df <- init_dates_dataframe(
    year_start,
    year_end,
    noleap = TRUE,
    freq = freq) %>%
    dplyr::select(-year_dec)
  
  if (timescale=="m"){
    df <- df %>%
      mutate(month = lubridate::month(date), year = lubridate::year(date))
  } else if (timescale=="y"){
    df <- df %>%
      mutate(year = lubridate::year(date))
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
    
  } else if (source == "cru" || source == "watch_wfdei"){
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
    
  } else if (source == "co2"){
    #-----------------------------------------------------------
    # Get CO2 data per year, independent of site
    #-----------------------------------------------------------
    df_tmp <- readr::read_csv(settings$path) %>% 
      dplyr::filter(year>1750) %>% 
      dplyr::mutate(date = lubridate::ymd(paste0(as.integer(year), "-01-01"))) %>% 
      dplyr::mutate(year = lubridate::year(date)) %>% 
      dplyr::select(-date) %>% 
      mutate(sitename = sitename)
    
  }  else if (source == "etopo1"){
    #-----------------------------------------------------------
    # Get ETOPO1 elevation data. year_start and year_end not required
    #-----------------------------------------------------------
    siteinfo <- tibble(
      sitename = sitename,
      lon = lon,
      lat = lat
      )
    
    df_tmp <- ingest_globalfields(siteinfo,
                                  source = source,
                                  getvars = NULL,
                                  dir = dir,
                                  timescale = NULL,
                                  verbose = FALSE
    )
    
  } else {
    rlang::abort("ingest(): Argument 'source' could not be identified. Use one of 'fluxnet', 'cru', 'watch_wfdei', or 'gee'.")
  }

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
    
  } else if (timescale=="d"){
    if (source == "co2"){
      df <- df_tmp %>%
        right_join(df %>% mutate(year = lubridate::year(date)), by = "year")
    } else {
      df <- df_tmp %>%
        right_join(df, by = "date")
    }
  }
  
  df <- df %>% 
    tidyr::drop_na(sitename)
  
    
  return( df )
  
}