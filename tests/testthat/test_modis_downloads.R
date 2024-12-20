test_that("test MODIS LST download", {
  skip_on_cran()
  testthat::skip() # TODO: remove again
  
  settings_modis <- get_settings_modis(
    bundle            = "modis_lst",
    data_path         = tempdir(),
    method_interpol   = "loess",
    keep              = TRUE,
    overwrite_raw     = TRUE,
    overwrite_interpol= TRUE,
    n_focal           = 0,
    network           = "FLUXNET"
  )
  
  df <- ingest_bysite(
    sitename  = "CH-Lae",
    source    = "modis",
    year_start = 2018,
    year_end  = 2019,
    settings  = settings_modis,
    verbose   = FALSE
  )
  
  expect_type(df, "list")
  
  settings_modis <- get_settings_modis(
    bundle            = "modis_lst",
    data_path         = tempdir(),
    method_interpol   = "loess",
    keep              = TRUE,
    overwrite_raw     = FALSE,
    overwrite_interpol= TRUE,
    n_focal           = 0,
    network           = "FLUXNET"
  )
  
  df <- ingest_bysite(
    sitename  = "CH-Lae",
    source    = "modis",
    year_start= 2018,
    year_end  = 2019,
    settings  = settings_modis,
    verbose   = FALSE
  )
  
  expect_type(df, "list")
  
})
