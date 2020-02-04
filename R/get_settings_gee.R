#' Defines settings for settings for Google Earth Engine download
#'
#' Defines settings for settings for Google Earth Engine download
#' for a pre-defined set of "bundles" (\code{c("fpar", "evi", "lai", "gpp")}).
#' 
#' @param bundle A character string specifying which dataset (bundle) to download.
#' Defaults to \code{"fpar"}. Available are: \code{c("fpar", "evi", "lai", "gpp")}.
#' @return A named list containing information required for download from Google
#' Earth Engine.
#' @export
#'
#' @examples settings_gee <- get_settings_gee( bundle = "fpar" )
#' 
get_settings_gee <- function( bundle = "fpar", python_path = system("which python", intern = TRUE),
                              gee_path = "~/gee_subset/gee_subset/" ){

  if (bundle == "fpar"){
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
      asfaparinput = TRUE,
      do_plot_interpolated = TRUE
      )

  } else if (bundle == "evi"){
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
      do_plot_interpolated = TRUE,
      scale_factor = 0.0001,
      period = 16,
      asfaparinput = TRUE
      )

  } else if (bundle == "lai"){
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

  } else if (bundle == "gpp"){
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
      do_plot_interpolated = FALSE,
      scale_factor = 0.0001,
      period = 8,
      asfaparinput = FALSE
      )

  } else {
    rlang::abort("get_settings_gee(): Could not identify required argument 'bundle'.")
  }

  out$python_path <- python_path
  out$gee_path    <- gee_path
  
  return(out)
}
