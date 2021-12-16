# is the server reachable
euler <- grepl('eu-', Sys.info()['nodename'])

test_that("test init data frame", {
  skip_on_cran()
  df <- init_dates_dataframe(
    2018,
    2019,
    noleap = TRUE,
    timescale = "d"
  )
  
  expect_type(df, "list")
  
})

test_that("test MODIS LST download", {
  skip_on_cran()
  
  settings_modis <- get_settings_modis(
    bundle            = "modis_lst_daily",
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
    year_start= 2018,
    year_end  = 2018,
    settings  = settings_modis,
    verbose   = FALSE
  )
  
  expect_type(df, "list")
  
})

test_that("test MODIS NDVI download", {
  skip_on_cran()
  
  settings_modis <- get_settings_modis(
    bundle            = "modis_ndvi",
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
    year_start= 2018,
    year_end  = 2018,
    settings  = settings_modis,
    verbose   = FALSE
  )
  
  expect_type(df, "list")
  
})

test_that("test MODIS FPAR download", {
  skip_on_cran()
  
  settings_modis <- get_settings_modis(
    bundle            = "modis_fpar",
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
    year_start= 2018,
    year_end  = 2018,
    settings  = settings_modis,
    verbose   = FALSE
  )
  
  expect_type(df, "list")
  
})
