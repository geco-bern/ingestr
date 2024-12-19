# test CRU, WFDEI, NDEP data

test_that("test CRU data (monthly and downscaled daily)", {
  skip_on_cran()
  
  ## get monthly CRU data
  mdf <- ingest_bysite(
    sitename  = "CH-Lae",
    source    = "cru",
    getvars   = c("tmax", "tmin", "prec", "vpd"),
    # dir       = "/data/archive/cru_NA_2021/data/",
    dir       = "/data/archive/cru_harris_2024/data/",
    timescale = "m",
    year_start = 1901,
    year_end  = 2018,
    lon       = 8.365,
    lat       = 47.4781, 
    elv       = 689,
    verbose   = FALSE
  )
  
  ## get daily data (with temporal downscaling)
  ddf <- ingest_bysite(
    sitename  = "CH-Lae",
    source    = "cru",
    getvars   = c("tmax", "tmin", "prec", "vpd"),
    dir       = "/data/archive/cru_harris_2024/data/",
    timescale = "d",
    year_start = 1901,
    year_end  = 2018,
    lon       = 8.365,
    lat       = 47.4781,
    elv       = 689,
    verbose   = FALSE
  )
  
  ## get yearly data (not supported)
  # ydf <- ingest_bysite(
  #   sitename  = "CH-Lae",
  #   source    = "cru",
  #   getvars   = c("tmax", "tmin", "prec", "vpd"),
  #   dir       = "/data/archive/cru_harris_2024/data/",
  #   timescale = "y",  requesting yearly cru data errors!
  #   year_start = 1901,
  #   year_end  = 2018,
  #   lon       = 8.365,
  #   lat       = 47.4781,
  #   elv       = 689,
  #   verbose   = FALSE
  # )
  
  
  # library(ggplot2)
  # ggplot(mdf, aes(x=date, y=prec)) + geom_line()
  # ggplot(mdf, aes(x=moy, y=prec, group = year)) + geom_line()
  # ggplot(mdf, aes(x=date, y=tmin)) + geom_line()
  # ggplot(mdf, aes(x=moy, y=tmin, group = year)) + geom_line()
  
  # library(ggplot2)
  # ggplot(ddf, aes(x=date, y=prec)) + geom_line()
  # ggplot(ddf, aes(x=lubridate::yday(date), y=prec, group = lubridate::year(date))) + geom_line()
  # ggplot(ddf, aes(x=date, y=tmin)) + geom_line()
  # ggplot(ddf, aes(x=lubridate::yday(date), y=tmin, group = lubridate::year(date))) + geom_line()
  # 
  # pl1 <- ggplot(ddf, aes(x=lubridate::yday(date), y=tmin, group = lubridate::year(date))) + 
  #   geom_line(aes(color = "daily\ninterpolated\nCRU\nobservations\n")) +
  #   geom_point(data = mdf, aes(color = "monthly\nCRU\nobservations")) +
  #   scale_color_manual(values = c('black', 'red'))
  # pl1 + aes(x = date)
  
  testthat::expect_equal(
    mdf[c(1,100,1416),], # use dput() to derive below hardcoded reference
    tidyr::tibble(sitename = c("CH-Lae", "CH-Lae", "CH-Lae"), 
                  wetd = c(12.3695813093777, 15.8190953985062, 20.2960408158111), 
                  prec = c(Jan = 1.5274476784002e-05, Apr = 2.91171954319271e-05, Dec = 4.01811756550539e-05), 
                  tmax = c(-0.0131488023838055, 13.764172279623, 4.65204431463909), 
                  tmin = c(-5.86579624579048, 3.4969638061452, 0.201791803788692), 
                  vpd = c(97.352996115735, 423.985071897943, 108.397108058279), 
                  moy = c(1, 4, 12), 
                  vapr = c(415.709569987869, 756.009354235459, 634.833568499851), 
                  month = c(1, 4, 12), 
                  year = c(1901, 1909, 2018), 
                  date = lubridate::ymd(c("1901-01-15","1909-04-15","2018-12-15")))
  )
  
  testthat::expect_equal(tolerance = 0.001, # we need a tolerance because of precip zeroes
                         ddf[c(1,100,1416, 43070),], # use dput() to derive below hardcoded reference
                         tidyr::tibble(date = lubridate::ymd(c("1901-01-01","1901-04-10","1904-11-17","2018-12-31")), 
                                       prec = c(0, 0, 0, 0), 
                                       tmax = c(1.36408937038989, 11.3535456681846, 4.26505071209226, 5.3088883537248), 
                                       tmin = c(-3.7938395642009, 2.84610676114661, -1.17652509826678, 0.529165441280156), 
                                       sitename = c("CH-Lae", "CH-Lae", "CH-Lae", "CH-Lae"), 
                                       vpd = c(75.4734411654391, 445.173057078465, 136.571370463, 136.784738582639), 
                                       vapr = c(523.440022615601, 601.876765774174, 558.154229833713, 626.47826413593)
                         )
  )
  
  # TODO: note that the output columns are different depending on timescale = "m" vs timescale = "d
  
  # # TODO: note that low values around 0 are not always read out similarly, depending oth the request: 
  # ingest_bysite(
  #   sitename  = "CH-Lae",
  #   source    = "cru",
  #   getvars   = c("tmin", "prec"),
  #   dir       = "/data/archive/cru_harris_2024/data/",
  #   timescale = "d",
  #   year_start = 2018,
  #   year_end  = 2018,
  #   lon       = 8.365,
  #   lat       = 47.4781,
  #   elv       = 689,
  #   verbose   = FALSE
  # ) |> tail()
  # ingest_bysite(
  #   sitename  = "CH-Lae",
  #   source    = "cru",
  #   getvars   = c("tmax", "tmin", "prec", "vpd"),#c("prec"),
  #   dir       = "/data/archive/cru_harris_2024/data/",
  #   timescale = "d",
  #   year_start = 2018,
  #   year_end  = 2018,
  #   lon       = 8.365,
  #   lat       = 47.4781,
  #   elv       = 689,
  #   verbose   = FALSE
  # ) |> tail()

})


test_that("test CRU data multisite downscaling (monthly and downscaled daily)", {
  skip_on_cran()
  library(dplyr)
  library(tidyr)
  library(ingestr)
  library(testthat)

  siteinfo_test <- tibble(
    sitename   = c("Reichetal_Colorado", "Reichetal_New_Mexico", "Reichetal_Venezuela", "Reichetal_Wisconsin", "Lulea"),
    lon        = c(-105.60, -107.00, -67.05, -90.00, 22.15),
    lat        = c(  40.05,   34.00,   1.93,  42.50, 65.59),
    elv        = c(  3360L,   1620L,   120L,   275L,    0L),
    year_start = c(2010, 2010, 2010, 2010, 2010),
    year_end   = c(2015, 2015, 2015, 2015, 2015))

  # Daily:
  # site-separate downscaling
  df_cru_daily_separate <- siteinfo_test[c(2,3,5),] |> 
    rowwise() |> group_split() |> # do it separately for each row
    lapply(function(curr_site_inf){
      ingestr::ingest(
        siteinfo = curr_site_inf,
        source = "cru",
        getvars = c("temp", "ccov"), # , "prec" NOTE: we can't test prec, unless we specify a seed somewhere
        dir = "/data/archive/cru_harris_2024/data", 
        timescale = "d")}) |> bind_rows() |> ungroup() |> unnest(data)
  
  # site-combined downscaling
  df_cru_daily_combined <- siteinfo_test[c(2,3,5),] |> 
    ungroup() |> group_split() |> # do it together for all rows
    lapply(function(curr_site_inf){
      ingestr::ingest(
        siteinfo = curr_site_inf,
        source = "cru",
        getvars = c("temp", "ccov"), # , "prec" NOTE: we can't test prec, unless we specify a seed somewhere
        dir = "/data/archive/cru_harris_2024/data", 
        timescale = "d")}) |> bind_rows() |> ungroup() |> unnest(data)
  
  testthat::expect_equal(df_cru_daily_separate, 
                         df_cru_daily_combined)
  
  # # Monthly:
  # # site-combined monthly
  # df_cru_monthly_combined <- siteinfo_test[c(2,3,5),] |> 
  #   ungroup() |> group_split() |> # do it together for all rows
  #   lapply(function(curr_site_inf){
  #     ingestr::ingest(
  #       siteinfo = curr_site_inf,
  #       source = "cru",
  #       getvars = c("temp", "ccov"), # , "prec" NOTE: we can't test prec, unless we specify a seed somewhere
  #       dir = "/data/archive/cru_harris_2024/data", 
  #       timescale = "m")}) |> bind_rows() |> ungroup() |> unnest(data)
  # 
  # # Illustration of failing test
  # library(ggplot2)
  # p1 <- ggplot(df_cru_monthly_combined, aes(y=temp, x=date, linetype=sitename, color=sitename)) + ggtitle("df_cru_monthly_combined") + geom_point() # CORRECT
  # p2 <- ggplot(df_cru_daily_separate, aes(y=temp, x=date, linetype=sitename, color=sitename))   + ggtitle("df_cru_daily_separate")   + geom_line()   # CORRECT DOWNSCALING
  # p3 <- ggplot(df_cru_daily_combined, aes(y=temp, x=date, linetype=sitename, color=sitename))   + ggtitle("df_cru_daily_combined")   + geom_line()   # WRONG DOWNSCALING: Gives same time series for each site
  # gridExtra::grid.arrange(p1, p2, p3) 
  
})


test_that("test WATCH_WFDEI data (daily)", {
  skip_on_cran()
  
  # df_watch <- ingest_bysite(
  #   sitename  = "FR-Pue",
  #   source    = "watch_wfdei",
  #   getvars   = c("temp"),
  #   dir       = "/data/archive/wfdei_weedon_2014/data/",
  #   timescale = "d",
  #   year_start = 1976,
  #   year_end  = 1982,
  #   lon       = 3.5958,
  #   lat       = 43.7414,
  #   verbose   = TRUE
  #   #settings  = list(correct_bias = "worldclim", dir_bias = "~/data/worldclim")
  # )
  
  # library(ggplot2)
  # ggplot(df_watch, aes(x=lubridate::yday(date), y=temp, group = lubridate::year(date))) + geom_line()
  # ggplot(df_watch, aes(x=date, y=temp)) + geom_line()
  
  # WATCH_WFDEI-test
  df_watch2 <- ingest_bysite(
    sitename  = "FR-Pue",
    source    = "watch_wfdei",
    getvars   = c("temp","prec","ppfd","wind","vpd"),
    dir       = "/data/archive/wfdei_weedon_2014/data/",
    timescale = "d",
    year_start = 1976,
    year_end  = 1982,
    lon       = 3.5958,
    lat       = 43.7414,
    verbose   = TRUE
    #settings  = list(correct_bias = "worldclim", dir_bias = "~/data/worldclim")
  )
  
  testthat::expect_equal(
    df_watch2[c(1,100,1416, 2546),], # use dput() to derive below hardcoded reference
    tidyr::tibble(sitename = c("FR-Pue", "FR-Pue", "FR-Pue", "FR-Pue"),
                  date = lubridate::ymd(c("1976-01-01","1976-04-10","1979-11-17","1982-12-22")), 
                  ppfd = c(0.000121494193775998, 0.000463321711450072, 0.000158704938322483, 0.000136168702821456), 
                  rain = c(1.04287650166711e-05, 1.50037226251495e-05, 3.42658596342262e-06, 0), 
                  snow = c(1.16922735984436e-06, 0, 0, 0), 
                  prec = c(1.15979923765154e-05, 1.50037226251495e-05, 3.42658596342262e-06, 0), 
                  qair = c(0.00445236525960527, 0.00546576460676214, 0.00414670119491026, 0.00380845699253127), 
                  temp = c(5.26007027738387, 10.1696075275532, 4.79854696611329, 3.2901508237403), 
                  patm = c(98319.1269542875, 97914.4488131635, 97631.138619465, 97144.4980819063), 
                  vapr = c(701.689376749628, 857.328665401018, 649.064121009744, 593.270511888794), 
                  vpd = c(186.875760082974, 385.06692545368, 211.337540044264, 180.495864250968)
    )
  )
  
})

test_that("test CRU data (monthly and downscaled daily)", {
  skip_on_cran()
  
  df_ndep <- ingest(
    ingestr::siteinfo_fluxnet2015 |> 
      dplyr::slice(1:3) |> 
      dplyr::select(sitename, lon, lat) |> 
      dplyr::mutate(year_start = 1990, year_end = 2009),
    source    = "ndep",
    timescale = "y",
    dir       = "/data/scratch/bstocker/ndep_lamarque/",
    verbose   = FALSE
  ) 
  # TODO: note that the output is differently structured in source = "ndep" (timescale = "y") 
  #       vs source = "cru", timescale = "m"
  #       see: mdf
  #       see: df_ndep |> tidyr::unnest(data)
  
  testthat::expect_equal(
    dplyr::ungroup(tidyr::unnest(df_ndep, data))[c(1,10,60),], # use dput() to derive below hardcoded reference
    tidyr::tibble(sitename = c("AR-SLu", "AR-SLu", "AT-Neu"), 
                  date = structure(c(7305, 10592, 14245), class = "Date"), 
                  noy = c(0.114221848547459, 0.118227459490299, 0.685620393264294), 
                  nhx = c(0.224458619952202, 0.187207788228989, 0.79076456451416))
  )
  
})
