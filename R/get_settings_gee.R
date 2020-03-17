#' Defines settings for settings for Google Earth Engine download
#'
#' Defines settings for settings for Google Earth Engine download
#' for a pre-defined set of "bundles" (\code{c("modis_fpar", "modis_evi", "modis_lai", "modis_gpp")}).
#' 
#' @param bundle A character string specifying which dataset (bundle) to download.
#' Defaults to \code{"modis_fpar"}. Available are: \code{c("modis_fpar", "modis_evi", "modis_lai", "modis_gpp")}.
#' @param python_path A character string specifying the local path to the python executable
#' @param gee_path A character string specifying the local path to the \code{gee_subseet} library.
#' @param data_path A character string specifying the path of where the data should be downloaded to.
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
#' @examples \dontrun{settings_gee <- get_settings_gee( bundle = "modis_fpar" )}
#' 
get_settings_gee <- function( bundle = "modis_fpar", python_path = system("which python", intern = TRUE),
                              gee_path, data_path, method_interpol = "linear", keep = FALSE, overwrite_raw = FALSE, overwrite_interpol = FALSE ){

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
