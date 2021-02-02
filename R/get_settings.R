#' Defines settings for settings for Google Earth Engine download
#'
#' Defines settings for settings for Google Earth Engine download
#' for a pre-defined set of "bundles" (\code{c("modis_fpar",
#' "modis_evi", "modis_lai", "modis_gpp")}).
#'
#' @param bundle A character string specifying which dataset (bundle) to download.
#' Defaults to \code{"modis_fpar"}. Available are: \code{c("modis_fpar", "modis_evi", "modis_lai", "modis_gpp")}.
#' @param python_path A character string specifying the local path to the python executable
#' @param gee_path A character string specifying the local path to the \code{gee_subseet} library.
#' Defaults to \code{"."} (present working directory).
#' @param data_path A character string specifying the path of where the data should be downloaded to.
#' Defaults to \code{"."} (present working directory).
#' @param method_interpol A character string specifying which interpolation method to use. Defaults to linear interpolation (\code{"linear"}).
#' Alternatives are
#' @param keep A logical specifying whether to keep all intermediate data (before filtering, and before imputing mean seasonal cycle),
#' and all alternative interpolation results. Defaults to \code{FALSE}.
#' @param overwrite_raw A logical specifying whether raw data as downloaded from GEE is to be overwritten. Defaults to \code{FALSE},
#' i.e. data is read from exisitng file if available.
#' @param overwrite_interpol A logical specifying whether processed (interpolated) data, is to be overwritten. Defaults to \code{FALSE},
#' i.e. data is read from exisitng file if available.
#' @return A named list containing information required for download from Google
#' Earth Engine.
#' @export
#'
#' @examples \dontrun{settings_gee <- get_settings_gee()}
#'
get_settings_gee <- function( bundle = "modis_fpar", python_path = system("which python", intern = TRUE),
                              gee_path = ".", data_path = ".", method_interpol = "linear", keep = FALSE,
                              overwrite_raw = FALSE, overwrite_interpol = FALSE ){

  if (bundle == "modis_fpar"){
    ##--------------------------------------------------------------------
    ## MODIS FPAR, 500 m, 4-daily
    ## Info see here: https://explorer.earthengine.google.com/#detail/MODIS%2F006%2FMCD15A3H
    ##--------------------------------------------------------------------
    out <- list(
      band_var = "Fpar",
      band_qc  = "FparLai_QC",
      prod     = "MODIS/006/MCD15A3H",
      prod_suffix = "MCD15A3H",
      varnam   = "fapar",
      productnam = "MODIS_FPAR_MCD15A3H_gee",
      scale_factor = 0.01,
      period = 4,
      asfaparinput = TRUE
      )

  } else if (bundle == "modis_evi"){
    ##--------------------------------------------------------------------
    ## EVI
    ## See info here: https://explorer.earthengine.google.com/#detail/MODIS%2F006%2FMOD13Q1
    ##--------------------------------------------------------------------
    out <- list(
      band_var   = "EVI",                # string defining the variable name in Google Earth Engine
      band_qc    = "SummaryQA",          # string defining the quality flag variable name in Google Earth Engine
      prod       = "MODIS/006/MOD13Q1",  # string defining the "ImageCollection ID" on Google Earth Engine
      prod_suffix = "MOD13Q1",           # string to be used here for defining product source (must correspond to part after last / in 'prod')
      varnam     = "fapar",                # string to be used here for defining variable
      productnam = "MODIS_EVI_MOD13Q1_gee",        # string to be used here for defining product source
      scale_factor = 0.0001,
      period = 16,
      asfaparinput = TRUE
      )

  } else if (bundle == "modis_lai"){
    ##--------------------------------------------------------------------
    ## LAI
    ##--------------------------------------------------------------------
    out <- list(
      band_var = "Lai_1km",
      band_qc  = "FparLai_QC",
      prod     = "MOD15A2",
      varnam   = "lai",
      productnam = "lai"
      )

  } else if (bundle == "modis_gpp"){
    ##--------------------------------------------------------------------
    ## GPP (kg C m-2), 500 m, 8-daily
    ##--------------------------------------------------------------------
    out <- list(
      band_var = "Gpp",                 # string defining the variable name in Google Earth Engine
      band_qc  = "Psn_QC",              # string defining the quality flag variable name in Google Earth Engine
      prod     = "MODIS/006/MOD17A2H",  # string defining the "ImageCollection ID" on Google Earth Engine
      prod_suffix = "MOD17A2H",         # string to be used here for defining product source (must correspond to part after last / in 'prod')
      varnam   = "gpp",                 # string to be used here for defining variable
      productnam = "MODIS_GPP",         # string to be used here for defining product source
      scale_factor = 0.0001,
      period = 8,
      asfaparinput = FALSE
      )

  } else {
    rlang::abort("get_settings_gee(): Could not identify required argument 'bundle'.")
  }

  out$python_path        <- python_path
  out$gee_path           <- gee_path
  out$data_path          <- data_path
  out$method_interpol    <- method_interpol
  out$keep               <- keep
  out$overwrite_raw      <- overwrite_raw
  out$overwrite_interpol <- overwrite_interpol

  return(out)

}


#' Defines settings for settings for MODIS download using MODISTools
#'
#' Defines settings for settings for Google Earth Engine download
#' for a pre-defined set of "bundles" (\code{c("modis_fpar",
#' "modis_evi", "modis_lai", "modis_gpp")}).
#'
#' @param bundle A character string specifying which dataset (bundle) to download.
#' Defaults to \code{"modis_fpar"}. Available are: \code{c("modis_fpar", "modis_evi", "modis_lai", "modis_gpp")}.
#' @param data_path A character string specifying the path of where the data should be downloaded to.
#' Defaults to \code{"."} (present working directory).
#' @param method_interpol A character string specifying which interpolation method to use. Defaults to linear interpolation (\code{"linear"}).
#' Alternatives are
#' @param keep A logical specifying whether to keep all intermediate data (before filtering, and before imputing mean seasonal cycle),
#' and all alternative interpolation results. Defaults to \code{FALSE}.
#' @param overwrite_raw A logical specifying whether raw data as downloaded from GEE is to be overwritten. Defaults to \code{FALSE},
#' i.e. data is read from exisitng file if available.
#' @param overwrite_interpol A logical specifying whether processed (interpolated) data, is to be overwritten. Defaults to \code{FALSE},
#' i.e. data is read from exisitng file if available.
#' @param n_focal An integer specifying the distance (in number of pixels) around the center pixel to be used for averaging. Defaults
#' to zero (using only the center pixel).
#' @return A named list containing information required for download from Google
#' Earth Engine.
#' @export
#'
#' @examples \dontrun{settings_gee <- get_settings_gee()}
#'
get_settings_modis <- function( bundle = "modis_fpar", data_path = ".", method_interpol = "linear", keep = FALSE,
                              overwrite_raw = FALSE, overwrite_interpol = FALSE, n_focal = 0 ){

  if (bundle == "modis_fpar"){
    ##--------------------------------------------------------------------
    ## MODIS FPAR, 500 m, 4-daily
    ##--------------------------------------------------------------------
    out <- list(
      prod     = "MCD15A3H",
      band_var = "Fpar_500m",
      band_qc  = "FparLai_QC",
      varnam   = "fapar",
      period   = 4,
      prod_suffix = "MCD15A3H",
      productnam = "MODIS_FPAR_MCD15A3H"
      )

  } else if (bundle == "modis_lai"){
    ##--------------------------------------------------------------------
    ## MODIS LAI, 500 m, 4-daily
    ##--------------------------------------------------------------------
    out <- list(
      prod     = "MCD15A3H",
      band_var = "Lai_500m",
      band_qc  = "FparLai_QC",
      varnam   = "lai",
      period   = 4,
      prod_suffix = "MCD15A3H",
      productnam = "MODIS_LAI_MCD15A3H"
    )

  } else if (bundle == "modis_evi"){
    ##--------------------------------------------------------------------
    ## EVI
    ##--------------------------------------------------------------------
    out <- list(
      prod     = "MOD13Q1",
      band_var = "250m_16_days_EVI",
      band_qc  = "250m_16_days_VI_Quality",
      varnam   = "fapar",
      period   = 16,
      prod_suffix = "MOD13Q1",
      productnam = "MODIS_EVI_MOD13Q1"
      )

  } else if (bundle == "modis_ndvi"){
    ##--------------------------------------------------------------------
    ## EVI
    ## See info here: https://explorer.earthengine.google.com/#detail/MODIS%2F006%2FMOD13Q1
    ##--------------------------------------------------------------------
    out <- list(
      prod     = "MOD13Q1",
      band_var = "250m_16_days_NDVI",
      band_qc  = "250m_16_days_VI_Quality",
      varnam   = "ndvi",
      period   = 16,
      prod_suffix = "MOD13Q1",
      productnam = "MODIS_NDVI_MOD13Q1"
      )

  } else {
    rlang::abort("get_settings_modis(): Could not identify required argument 'bundle'.")
  }

  out$data_path          <- data_path
  out$method_interpol    <- method_interpol
  out$keep               <- keep
  out$overwrite_raw      <- overwrite_raw
  out$overwrite_interpol <- overwrite_interpol
  out$n_focal            <- n_focal

  return(out)

}


#' Defines settings for settings for FLUXNET data ingestion
#'
#' Defines settings for settings for FLUXNET data ingestion
#'
#' @param dir_hh A character string specifying the local path of
#' half-hourly FLUXNET data, required to get daytime VPD. Defaults to
#' \code{"."} (present working directory).
#' @param dir_hr A character string specifying the local path of
#' hourly FLUXNET data, required to get daytime VPD. Defaults to
#' \code{"."} (present working directory).
#' @param getswc  If \code{getswc==TRUE}, then all soil water content data
#' (variables starting with \code{"SWC_"}) are read. Defaults to \code{TRUE}.
#' @param threshold_GPP A numeric value (between 0 and 1 for daily, weekly,
#' monthly, and annual data or 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data). The value specifies the
#' threshold for excluding data during data cleaning. The threshold is
#' with respect to the data quality flag in the FLUXNET data, indicating
#' the fraction of measured and good quality gapfilled data for daily, weekly,
#' monthly, and annual data and 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data. Defaults to \code{threshold_GPP=0}
#' meaning no data is excluded.
#' @param threshold_LE A numeric value (between 0 and 1 for daily, weekly,
#' monthly, and annual data or 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data). The value specifies the
#' threshold for excluding data during data cleaning. The threshold is
#' with respect to the data quality flag in the FLUXNET data, indicating
#' the fraction of measured and good quality gapfilled data for daily, weekly,
#' monthly, and annual data and 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data. Defaults to \code{threshold_GPP=0}
#' meaning no data is excluded.
#' @param threshold_H A numeric value (between 0 and 1 for daily, weekly,
#' monthly, and annual data or 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data). The value specifies the
#' threshold for excluding data during data cleaning. The threshold is
#' with respect to the data quality flag in the FLUXNET data, indicating
#' the fraction of measured and good quality gapfilled data for daily, weekly,
#' monthly, and annual data and 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data. Defaults to \code{threshold_GPP=0}
#' meaning no data is excluded.
#' @param threshold_SWC A numeric value (between 0 and 1 for daily, weekly,
#' monthly, and annual data or 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data). The value specifies the
#' threshold for excluding data during data cleaning. The threshold is
#' with respect to the data quality flag in the FLUXNET data, indicating
#' the fraction of measured and good quality gapfilled data for daily, weekly,
#' monthly, and annual data and 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data. Defaults to \code{threshold_GPP=0}
#' meaning no data is excluded.
#' @param threshold_WS A numeric value (between 0 and 1 for daily, weekly,
#' monthly, and annual data or 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data). The value specifies the
#' threshold for excluding data during data cleaning. The threshold is
#' with respect to the data quality flag in the FLUXNET data, indicating
#' the fraction of measured and good quality gapfilled data for daily, weekly,
#' monthly, and annual data and 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data. Defaults to \code{threshold_GPP=0}
#' meaning no data is excluded.
#' @param threshold_USTAR A numeric value (between 0 and 1 for daily, weekly,
#' monthly, and annual data or 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data). The value specifies the
#' threshold for excluding data during data cleaning. The threshold is
#' with respect to the data quality flag in the FLUXNET data, indicating
#' the fraction of measured and good quality gapfilled data for daily, weekly,
#' monthly, and annual data and 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data. Defaults to \code{threshold_GPP=0}
#' meaning no data is excluded.
#' @param threshold_T A numeric value (between 0 and 1 for daily, weekly,
#' monthly, and annual data or 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data). The value specifies the
#' threshold for excluding data during data cleaning. The threshold is
#' with respect to the data quality flag in the FLUXNET data, indicating
#' the fraction of measured and good quality gapfilled data for daily, weekly,
#' monthly, and annual data and 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data. Defaults to \code{threshold_GPP=0}
#' meaning no data is excluded.
#' @param threshold_NETRAD A numeric value (between 0 and 1 for daily, weekly,
#' monthly, and annual data or 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data). The value specifies the
#' threshold for excluding data during data cleaning. The threshold is
#' with respect to the data quality flag in the FLUXNET data, indicating
#' the fraction of measured and good quality gapfilled data for daily, weekly,
#' monthly, and annual data and 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data. Defaults to \code{threshold_GPP=0}
#' meaning no data is excluded.
#' @param filter_ntdt A logical specifying whether agreement of daytime and nighttime-
#' based GPP estimates is to be used as a filter. Data points are removed
#' where their difference is below the the 97.5% and above the 2.5% quantile of all
#' difference values per site. Defaults to \code{FALSE}.
#' @param return_qc A logical specifying whether quality control variables
#' should be returned. Defaults to \code{FALSE}.
#' @param remove_neg A logical specifying whether negative GPP values are to
#' be removed (replaces with NA). Defaults to \code{FALSE}.
#'
#' @return A named list containing information required for read data from standard
#' FLUXNET data files (CSV files).
#' @export
#'
#' @examples \dontrun{settings_gee <- get_settings_fluxnet()}
#'
get_settings_fluxnet <- function(
  dir_hh= ".",
  dir_hr= ".",
  getswc             = TRUE,
  threshold_GPP      = 0.0,
  threshold_LE       = 0.0,
  threshold_H        = 0.0,
  threshold_SWC      = 0.0,
  threshold_WS       = 0.0,
  threshold_USTAR    = 0.0,
  threshold_T        = 0.0,
  threshold_NETRAD   = 0.0,
  filter_ntdt        = FALSE,
  return_qc          = FALSE,
  remove_neg         = FALSE,
  verbose            = TRUE
  ){

  settings <- list(
    dir_hh= dir_hh,
    dir_hr= dir_hr,
    getswc             = getswc,
    threshold_GPP      = threshold_GPP,
    threshold_LE       = threshold_LE,
    threshold_H        = threshold_H,
    threshold_SWC      = threshold_SWC,
    threshold_WS       = threshold_WS,
    threshold_USTAR    = threshold_USTAR,
    threshold_T        = threshold_T,
    threshold_NETRAD   = threshold_NETRAD,
    filter_ntdt        = filter_ntdt,
    return_qc          = return_qc,
    remove_neg         = remove_neg,
    verbose            = verbose
    )

  return(settings)

}

#' Defines settings for settings for SoilGrids ingest
#'
#' Defines settings for settings for SoilGrids ingest. Handles the specification of the required layer identifier.
#'
#' @param varnam A charachter string specifying the variable of interest.
#' See \url{https://www.isric.org/explore/soilgrids/faq-soilgrids#What_do_the_filename_codes_mean} for naming conventions.
#' @param layer An integer or vector of integers specifying the soil layers.
#' See \url{https://www.isric.org/explore/soilgrids/faq-soilgrids#What_do_the_filename_codes_mean} for available layers.
#' Defaults to \code{"0-5cm"}.
#' @param agg A character string specifying the aggregation statistic for depth layer-specific values. Defaults to
#' \code{"mean"}. See \url{https://www.isric.org/explore/soilgrids/faq-soilgrids#What_do_the_filename_codes_mean} for
#' available statistics.
#' @return A named list containing information required for download from SoilGrids
#' @export
#'
#' @examples \dontrun{settings <- get_settings_soilgrids("soc")}
#'
get_settings_soilgrids <- function(varnam, layer = 1, agg = "mean"){

  ## for association of layer character codes
  df_layer_code <- tibble(layer = 1:6, code = c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm"))

  ## for association of conversion factors
  df_conversion <- tibble(varnam = c("bdod", "cec", "cfvo", "clay", "nitrogen", "phh2o", "sand", "silt", "soc", "ocd", "ocs"),
                          factor = c(100, 10 , 10 , 10 , 100, 10 , 10 , 10 , 10 , 10 , 10))
  
  ## specify layer of interest
  df_voi_layer <- expand.grid(varnam, layer) %>% 
    setNames(c("varnam", "layer")) %>% 
    as_tibble() %>% 
    left_join(df_layer_code, by = "layer") %>% 
    mutate(data_layer = paste(varnam, code, agg, sep = "_")) %>% 
    left_join(df_conversion, by = "varnam")
  
  out <- list()
  out$voi_layer <- df_voi_layer %>% pull(data_layer)
  out$voi <- df_voi_layer %>% pull(varnam)
  out$factor <- df_voi_layer %>% pull(factor)
  out$layer <- df_voi_layer %>% pull(layer)
  
  ## set other variables necessary for the WCS call for all kinds of requests
  out$webdav_path = '/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/'

  return(out)
}

#' Defines settings for settings for WISE30sec ingest
#'
#' Defines settings for settings for WISE30sec ingest. Handles the specification of the required layer identifier.
#'
#' @param varnam A charachter string specifying the variable of interest.
#' See \url{https://www.isric.org/documents/document-type/isric-report-201501-world-soil-property-estimates-broad-scale-modelling} for naming conventions.
#' @param layer An integer specifying soil layer.
#' See \url{https://www.isric.org/documents/document-type/isric-report-201501-world-soil-property-estimates-broad-scale-modelling} for available layers.
#' Defaults to \code{1}.
#' @return A named list containing information required for download from WISE30sec
#' @export
#'
#' @examples \dontrun{settings <- get_settings_wise("CNrt")}
#'
get_settings_wise <- function(varnam, layer = 1){
  
  out <- list()
  
  out$varnam <- varnam
  
  ## specify layer of interest
  out$layer <- layer
  
  return(out)
}
