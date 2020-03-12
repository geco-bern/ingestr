#' Data ingest for a single site
#'
#' Ingests data for a single site and one specific data type, specified by argument \code{source}.
#'
#' @param sitename A character string used as site identification. When data is extracted from 
#' global files or remote servers, \code{sitename} is simply used as a label and any string can
#' be provided. When data is extraced from site-specific files (e.g. \code{source = "FLUXNET2015"}),
#' then \code{sitename} is used to identify the file from which data is read. 
#' @param source A character used as identifiyer for the type of data source
#' (e.g., \code{"fluxnet2015"}).
#' @param getvars A named list of characters specifying the variable names in
#' the source dataset corresponding to standard names \code{"temp"} for temperature,
#' \code{"prec"} for precipitation, \code{"patm"} for atmospheric pressure,
#' \code{"vpd"} for vapour pressure deficit, \code{"netrad"} for net radiation,
#' \code{"swin"} for shortwave incoming radiation.
#' @param dir A character specifying the directory where data is located.
#' @param settings A list of additional settings used for reading original files.
#' @param timescale A character or vector of characters, specifying the time scale of data used from
#' the respective source (if multiple time scales are available, otherwise is disregarded).
#' @param year_start An integer specifying the first year for which data is to be ingested.
#' @param year_end An integer specifying the last year for which data is to be ingested (full years
#' are read, i.e. all days, or hours, or months in each year).
#' @param lon A numeric value specifying the longitude for which data is extraced from global files
#' or remote data servers. If \code{source = "FLUXNET2015"}, this is not required and set ot \code{NA}.
#' @param lat A numeric value specifying the longitude for which data is extraced from global files
#' or remote data servers. If \code{source = "FLUXNET2015"}, this is not required and set ot \code{NA}.
#' @param verbose if \code{TRUE}, additional messages are printed.
#'
#' @return A data frame (tibble) containing the time series of ingested data. 
#' @export
#'
#' @examples \dontrun{inputdata <- ingest_bysite()}  
#'
ingest_bysite <- function(
  sitename,
  source,
  getvars = NULL,
  dir = NULL,
  settings = list(),
  timescale = "d",
  year_start,
  year_end,
  lon = ifelse(source=="fluxnet2015", NA),
  lat = ifelse(source=="fluxnet2015", NA),
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
  if (source == "fluxnet2015"){
    df_tmp <- get_obs_bysite_fluxnet2015(sitename,
                                         path_fluxnet2015 = dir,
                                         path_fluxnet2015_hh = settings$dir_hh,
                                         timescale        = timescale,
                                         getvars          = getvars,
                                         getswc           = settings$getswc,
                                         threshold_GPP    = settings$threshold_GPP,
                                         verbose          = verbose
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
    
    df_tmp <- ingest_gee( 
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
      do_plot_interpolated = settings$do_plot_interpolated, 
      python_path          = settings$python_path,
      gee_path             = settings$gee_path,
      data_path            = settings$data_path,
      splined              = settings$splined
    )
    
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
    df <- df_tmp %>%
      right_join(df, by = "date")
  }
  
    
  return( df )
  
}