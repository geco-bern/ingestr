#' Processes SOFUN model input.
#'
#' Handles the processing of model inputs (including the optional downloading from
#' a remote server), and links input files to the SOFUN model input directory (\code{lonlat} setup),
#' or writes text files of time series (site-scale setup). 
#' 
#' @param settings_input A list containging the model input settings. See vignette_rsofun.pdf for more information and examples.
#' @param settings_sims A list containing model simulation settings from \code{\link{prepare_setup_sofun}}.  See vignette_rsofun.pdf for more information and examples.
#' @param overwrite_climate if \code{TRUE}, yearly climate input text files in the site-scale setup are overwritten.
#' @param overwrite_csv_climate_lev1 if \code{TRUE}, climate input CSV files created after collecting site-scale meteo data are overwritten.
#' @param overwrite_csv_climate_lev2 if \code{TRUE}, climate input CSV files created after collecting data from global field for all sites are overwritten.
#' @param overwrite_csv_climate_lev3 if \code{TRUE}, climate input CSV files created after deriving SOFUN-standard input (naming, units, and interpolation) are overwritten.
#' @param overwrite_csv_fapar if \code{TRUE}, fAPAR input CSV files in the site-scale setup are overwritten.
#' @param verbose if \code{TRUE}, additional messages are printed.
#'
#' @return A named list of data frames (tibbles) containing input data for each site is returned.
#' @export
#'
#' @examples inputdata <- prepare_input_sofun( settings_input = settings_input, settings_sims = settings_sims, overwrite_climate = FALSE, verbose = TRUE )
#' 
prepare_input_sofun <- function( settings_input, 
                                 settings_sims, 
                                 overwrite_csv_climate_lev1=FALSE, overwrite_csv_climate_lev2=FALSE, overwrite_csv_climate_lev3=FALSE,
                                 overwrite_rdata_climate=FALSE,
                                 overwrite_csv_fapar=FALSE, verbose=FALSE ){

  # # If FLUXNET 2015 data is required, make sure it's available locally    
  # #-----------------------------------------------------------
  # if (any( c( 
  #   "fluxnet2015" %in% settings_input$temperature, 
  #   "fluxnet2015" %in% settings_input$precipitation, 
  #   "fluxnet2015" %in% settings_input$vpd, 
  #   "fluxnet2015" %in% settings_input$ppfd,
  #   "fluxnet2015" %in% settings_input$netrad
  #   ))){
  
  #   error <- check_download_fluxnet2015( settings_input$path_fluxnet2015 )
  
  # }
  
  #-----------------------------------------------------------
  # Get climate input
  #-----------------------------------------------------------
  ## Second, get climate data from global files
  dir <- paste0( settings_input$path_input, "/sitedata/climate/" )
  filnam_clim <- paste0( dir, "/clim_daily.Rdata" )
  if (!dir.exists(dir)) system( paste0( "mkdir -p ", dir ) )
  
  ## First, get climate data from site-specific
  if (!file.exists(filnam_clim) || overwrite_rdata_climate){
    
    ddf_climate <- purrr::map(
      as.list(settings_sims$sitename),
      ~get_input_sofun_climate_bysite( .,
                                       settings_input,
                                       dplyr::filter(settings_sims, sitename == .),
                                       overwrite_csv = overwrite_csv_climate_lev1,
                                       verbose = verbose
      )
    )
    names(ddf_climate) <- settings_sims$sitename
    ddf_climate <- ddf_climate %>%
      bind_rows(.id = "sitename")
    
    ddf_climte_globalfields <- get_input_sofun_climate_globalfields(
      dplyr::select(ddf_climate, sitename, date),
      settings_input,
      settings_sims,
      overwrite_csv = overwrite_csv_climate_lev2,
      verbose = FALSE
    )
    ddf_climate <- ddf_climate %>%
      left_join(
        ddf_climte_globalfields,
        by = c("sitename", "date")
      )
    
    ## Then, prepare climate input data files for sofun
    ddf_climate <- purrr::map(
      as.list(settings_sims$sitename),
      ~prepare_input_sofun_climate_bysite(
        .,
        dplyr::filter(ddf_climate, sitename == .),
        settings_input,
        settings_sims,
        overwrite_csv = overwrite_csv_climate_lev3,
        verbose = verbose
      )
    )
    names(ddf_climate) <- settings_sims$sitename
    ddf_climate <- ddf_climate %>%
      bind_rows()
    
    ## write to file
    save(ddf_climate, file = filnam_clim)
    
  } else {
    
    load(filnam_clim)
    
  }
  
  
  ##-----------------------------------------------------------
  ## Get fapar input
  ##-----------------------------------------------------------
  ## Create data frame for site info for batch-download using MODISTools or GEE
  df_lonlat <- settings_sims %>% 
    dplyr::mutate(year_start = lubridate::year(date_start)) %>% 
    dplyr::select(sitename, lon, lat, year_start, year_end)
  
  # if (!dir.exists(settings_input$path_fapar)) system(paste0("mkdir -p ", settings_input$path_fapar))
  
  # ## Using MODISTools
  # bands <- MODISTools::mt_bands(product = "MCD15A3H") %>% View()
  # dates <- MODISTools::mt_dates(product = "MCD15A3H", lat = df_lonlat$lat[1], lon = df_lonlat$lon[1]) %>% View()
  # subsets <- MODISTools::mt_batch_subset(
  #              df = dplyr::distinct(df_lonlat, lon, lat, .keep_all=TRUE) %>% 
  #                slice(1:2),
  #              product = "MCD15A3H",
  #              band = "Fpar_500m",
  #              internal = TRUE,
  #              start = "2000-01-01",
  #              end = "2018-12-31",
  #              out_dir = settings_input$path_fapar)
  
  ## Using the Google EE function
  ddf_fapar <- purrr::map(
    as.list(seq(nrow(df_lonlat))),
    ~prepare_input_sofun_fapar_bysite_GEE( 
      slice(df_lonlat, .), 
      start_date           = "2000-01-01",
      end_date             = "2018-12-31", 
      settings_sims        = settings_sims, 
      settings_input       = settings_input,
      overwrite_raw        = FALSE,
      overwrite_nice       = overwrite_csv_fapar,
      overwrite_csv        = overwrite_csv_fapar,
      band_var             = settings_input$settings_gee$band_var, 
      band_qc              = settings_input$settings_gee$band_qc, 
      prod                 = settings_input$settings_gee$prod, 
      prod_suffix          = settings_input$settings_gee$prod_suffix, 
      varnam               = settings_input$settings_gee$varnam, 
      productnam           = settings_input$settings_gee$productnam, 
      scale_factor         = settings_input$settings_gee$scale_factor, 
      period               = settings_input$settings_gee$period, 
      do_plot_interpolated = settings_input$settings_gee$do_plot_interpolated, 
      python_path          = settings_input$settings_gee$python_path,
      gee_path             = settings_input$settings_gee$gee_path
    )
  )
  
  ## get missing
  names(ddf_fapar) <- settings_sims$sitename
  missing_fapar <- which(is.na(ddf_fapar)) %>% names()
  
  ## rearrange to flat table
  ddf_fapar <- ddf_fapar %>%
    # na.omit.list() %>% 
    bind_rows(.id = "sitename") %>% 
    dplyr::select(sitename, date, fapar = modisvar_interpol)
  
  
  ##-----------------------------------------------------------
  ## Get CO2 input
  ##-----------------------------------------------------------
  df_co2 <- readr::read_csv(settings_input$path_co2) %>% 
    dplyr::filter(year>1750) %>% 
    dplyr::mutate(date = lubridate::ymd(paste0(as.integer(year), "-01-01"))) %>% 
    dplyr::mutate(year = lubridate::year(date))

  out <- ddf_climate %>% left_join( ddf_fapar, by=c("date", "sitename")) %>% 
    dplyr::mutate(year = lubridate::year(date)) %>% 
    dplyr::left_join(dplyr::select(df_co2, -date), by = "year")

  return(out)

}

##-----------------------------------------------------------
## Read climate data from files given by sites
##-----------------------------------------------------------
get_input_sofun_climate_bysite <- function( sitename, settings_input, settings_sims, overwrite_csv=FALSE, verbose=FALSE ){

  # if (verbose) print(paste("Getting climate data for site", sitename ))
  
  ## path of CSV file with data for this site
  dir <- paste0( settings_input$path_input, "/sitedata/climate/", sitename )
  if (!dir.exists(dir)) system( paste0( "mkdir -p ", dir ) )
  csvfiln <- paste0( dir, "/clim_daily_lev1_", sitename, ".csv" )
  
  if (file.exists(csvfiln) && !overwrite_csv){
    
    ddf <- readr::read_csv(csvfiln)
  
  } else {
  
    ## Initialise daily dataframe (WITHOUT LEAP YEARS, SOFUN USES FIXED 365-DAYS YEARS!)
    ddf <- init_dates_dataframe( 
      year(settings_sims$date_start), 
      year(settings_sims$date_end), 
      noleap = TRUE) %>% 
      dplyr::select(-year_dec)
    
    ##----------------------------------------------------------------------
    ## Read daily FLUXNET 2015 meteo data for each site (reads all variables)
    ## A file must be found containing the site name in the file name and located in <settings_input$path_fluxnet2015>
    ##----------------------------------------------------------------------
    fluxnetvars <- c()
    if ("fluxnet2015" %in% settings_input$temperature)   fluxnetvars <- c( fluxnetvars, "temp_day" )
    if ("fluxnet2015" %in% settings_input$precipitation) fluxnetvars <- c( fluxnetvars, "prec" )
    if ("fluxnet2015" %in% settings_input$vpd)           fluxnetvars <- c( fluxnetvars, "vpd_day" )
    if ("fluxnet2015" %in% settings_input$ppfd)          fluxnetvars <- c( fluxnetvars, "ppfd" )
    if ("fluxnet2015" %in% settings_input$netrad)        fluxnetvars <- c( fluxnetvars, "netrad" )
    if ("fluxnet2015" %in% settings_input$patm)          fluxnetvars <- c( fluxnetvars, "patm" )
    
    getvars <- c()
    if ("fluxnet2015" %in% settings_input$temperature)   getvars <- c( getvars, "TA_F_DAY" ) # c( getvars, "TA_F" )  # 
    if ("fluxnet2015" %in% settings_input$precipitation) getvars <- c( getvars, "P_F" )
    if ("fluxnet2015" %in% settings_input$vpd)           getvars <- c( getvars, "VPD_F_DAY" ) #c( getvars, "VPD_F" ) # 
    if ("fluxnet2015" %in% settings_input$ppfd)          getvars <- c( getvars, "SW_IN_F" )
    if ("fluxnet2015" %in% settings_input$netrad)        getvars <- c( getvars, "NETRAD" )
    if ("fluxnet2015" %in% settings_input$patm)          getvars <- c( getvars, "PA_F" )
    
    # ## xxx debug
    # fluxnetvars <- c()
    # getvars <- c()
    
    if (length(fluxnetvars)>0){
      
      ## Make sure data is available for this site
      error <- check_download_fluxnet2015( settings_input$path_fluxnet2015, sitename )
      
      ddf <- get_obs_bysite_fluxnet2015(sitename, 
                                        path_fluxnet2015 = settings_input$path_fluxnet2015, 
                                        path_fluxnet2015_hh = settings_input$path_fluxnet2015_hh,
                                        timescale        = "d", 
                                        getvars          = getvars, 
                                        getswc           = FALSE,
                                        threshold_GPP    = settings_input$threshold_GPP
                                        )
      
      if (any(!(fluxnetvars %in% names(ddf)))){
        rlang::warn(paste("Could not get all flunetvars for site", sitename))
        rlang::warn("Missing: ")
        missing <- fluxnetvars[!(fluxnetvars %in% names(ddf))]
        rlang::warn(missing)
        if ("vpd_day" %in% missing){
          rlang::warn("Getting mean daily VPD instead of mean daytime VPD.")
          ## re-read mean daily VPD instead of daytime VPD
          fluxnetvars <- c(fluxnetvars[-which(fluxnetvars=="vpd_day")], "vpd")
          getvars <- "VPD_F"
          ddf <- get_obs_bysite_fluxnet2015(sitename, 
                                            path_fluxnet2015 = settings_input$path_fluxnet2015, 
                                            path_fluxnet2015_hh = settings_input$path_fluxnet2015_hh,
                                            timescale        = "d", 
                                            getvars          = getvars, 
                                            getswc           = FALSE,
                                            threshold_GPP    = settings_input$threshold_GPP
                                            ) %>% 
            right_join(ddf, by = "date")
        }
        if ("temp_day" %in% missing){
          rlang::warn("Getting mean daily VPD instead of mean daytime VPD.")
          ## re-read mean daily VPD instead of daytime VPD
          fluxnetvars <- c(fluxnetvars[-which(fluxnetvars=="temp_day")], "temp")
          getvars <- "TA_F"
          ddf <- get_obs_bysite_fluxnet2015(sitename, 
                                            path_fluxnet2015 = settings_input$path_fluxnet2015, 
                                            path_fluxnet2015_hh = settings_input$path_fluxnet2015_hh,
                                            timescale        = "d", 
                                            getvars          = getvars, 
                                            getswc           = FALSE,
                                            threshold_GPP    = settings_input$threshold_GPP
                                            ) %>% 
            right_join(ddf, by = "date")
        }
        
      } else {
        
        ddf <- ddf %>% 
          dplyr::select( date, one_of(fluxnetvars) ) %>% 
          setNames( c("date", paste0( fluxnetvars, "_fluxnet2015" ))) %>%
          right_join( ddf, by = "date" )   
        
      }
    }
    
    ## some FLUXNET2015 data has missing 31-Dec at the end of the time series
    ## take 30-Dec data to fill the gap
    if (lubridate::mday(ddf$date[nrow(ddf)])==30){
      ddf <- ddf %>% 
        dplyr::bind_cols( dplyr::slice(ddf, nrow(ddf)))
      ddf$date[nrow(ddf)] <- ddf$date[nrow(ddf)] + lubridate::days(1)
    }
    
    ## Write to temporary file
    readr::write_csv(ddf, path = csvfiln)

  }

  return( ddf )

}


##-----------------------------------------------------------
## Read climate data from files as global fields
##-----------------------------------------------------------
get_input_sofun_climate_globalfields <- function( ddf, settings_input, settings_sims, overwrite_csv=FALSE, verbose=FALSE ){

  if (verbose) print("Getting climate data from global fields...")

  ## path of CSV file with data for this site
  dir <- paste0( settings_input$path_input, "/sitedata/climate/" )
  if (!dir.exists(dir)) system( paste0( "mkdir -p ", dir ) )
  csvfiln <- paste0( dir, "/clim_daily_lev2.csv" )
  
  if (overwrite_csv || !file.exists(csvfiln)){  
    ##----------------------------------------------------------------------
    ## Read WATCH-WFDEI data (extracting from NetCDF files for this site)
    ##----------------------------------------------------------------------
    ## temperature
    if ("watch_wfdei" %in% settings_input$temperature){
      ddf <- get_input_sofun_climate_globalfields_watch_byvar( "Tair_daily", settings_input, settings_sims ) %>%
        dplyr::rename(temp_watch = myvar) %>% 
        dplyr::mutate(temp_watch = temp_watch - 273.15) %>%
        dplyr::right_join(ddf, by = c("sitename", "date"))
    }
    
    ## precipitation
    if ("watch_wfdei" %in% settings_input$precipitation){
      ddf <- get_input_sofun_climate_globalfields_watch_byvar( "Rainf_daily", settings_input, settings_sims ) %>%
        dplyr::mutate( rain = myvar ) %>%
        left_join(
          get_input_sofun_climate_globalfields_watch_byvar( "Snowf_daily", settings_input, settings_sims ) %>%
            dplyr::mutate( snow = myvar ),
          by = c("sitename", "date")
        ) %>%
        dplyr::rename(prec_watch = (rain + snow) * 60 * 60 * 24 ) %>%  # kg/m2/s -> mm/day
        dplyr::right_join(ddf_prec, by = c("sitename", "date"))
    }
    
    ## humidity
    if ("watch_wfdei" %in% settings_input$vpd){
      ddf <- get_input_sofun_climate_globalfields_watch_byvar( "Qair_daily", settings_input, settings_sims ) %>%
        dplyr::rename(qair_watch = myvar) %>% 
        dplyr::right_join(ddf_qair, by = c("sitename", "date"))
    }
    
    ## PPFD
    if ("watch_wfdei" %in% settings_input$ppfd){
      kfFEC <- 2.04    
      ddf <- get_input_sofun_climate_globalfields_watch_byvar( "SWdown_daily", settings_input, settings_sims ) %>%
        dplyr::rename(ppfd_watch = myvar * kfFEC * 1.0e-6 * 60 * 60 * 24 ) %>%  # umol m-2 s-1 -> mol m-2 d-1
        dplyr::right_join(ddf_ppfd, by = c("sitename", "date"))
    }
    
    
    ##----------------------------------------------------------------------
    ## Fill missing variables
    ##----------------------------------------------------------------------
    if (is.na(settings_input$cloudcover)){
      rlang::warn("Filling column ccov_dummy with value 50 (%).")
      ddf <- ddf %>% mutate( ccov_dummy = 50 )
    }
    
    ##----------------------------------------------------------------------
    ## Read CRU monthly data (extracting from NetCDF files for this site)
    ##----------------------------------------------------------------------
    cruvars <- c()
    mdf <- ddf %>%
      dplyr::select(sitename, date) %>% 
      dplyr::mutate(year = lubridate::year(date), moy = lubridate::month(date)) %>% 
      dplyr::select(sitename, year, moy) %>% 
      dplyr::distinct()
    
    ## temperature
    if ("cru" %in% settings_input$temperature){
      cruvars <- c(cruvars, "temp")
      mdf <- get_input_sofun_climate_globalfields_cru_byvar( "tmp", settings_input, settings_sims ) %>%
        dplyr::select(sitename, date, myvar) %>% 
        dplyr::rename(temp_cru = myvar) %>% 
        dplyr::mutate(year = lubridate::year(date), moy = lubridate::month(date)) %>% 
        dplyr::select(-date) %>% 
        dplyr::right_join(mdf, by = c("sitename", "year", "moy"))
      
    }
    
    ## precipitation
    if ("cru" %in% settings_input$precipitation){
      cruvars <- c(cruvars, "prec")
      mdf <- get_input_sofun_climate_globalfields_cru_byvar( "pre", settings_input, settings_sims ) %>%
        dplyr::select(sitename, date, myvar) %>% 
        dplyr::rename(prec_cru = myvar) %>% 
        dplyr::mutate(year = lubridate::year(date), moy = lubridate::month(date)) %>% 
        dplyr::select(-date) %>% 
        dplyr::right_join(mdf, by = c("sitename", "year", "moy"))
    }
    
    ## vpd from vapour pressure
    if ("cru" %in% settings_input$temperature){
      cruvars <- c(cruvars, "vap")
      mdf <- get_input_sofun_climate_globalfields_cru_byvar( "vap", settings_input, settings_sims ) %>%
        dplyr::select(sitename, date, myvar) %>% 
        dplyr::rename(vap_cru = myvar) %>% 
        dplyr::mutate(year = lubridate::year(date), moy = lubridate::month(date)) %>% 
        dplyr::select(-date) %>% 
        dplyr::right_join(mdf, by = c("sitename", "year", "moy"))
    }
    
    ## cloud cover
    if ("cru" %in% settings_input$cloudcover){
      cruvars <- c(cruvars, "ccov")
      mdf <- get_input_sofun_climate_globalfields_cru_byvar( "cld", settings_input, settings_sims ) %>%
        dplyr::select(sitename, date, myvar) %>% 
        dplyr::rename(ccov_cru = myvar) %>% 
        dplyr::mutate(year = lubridate::year(date), moy = lubridate::month(date)) %>% 
        # dplyr::select(-date) %>% 
        dplyr::right_join(mdf, by = c("sitename", "year", "moy"))
    }
    
    ## wet days
    if ("cru" %in% settings_input$wetdays){
      cruvars <- c(cruvars, "wetd")
      mdf <- get_input_sofun_climate_globalfields_cru_byvar( "wet", settings_input, settings_sims ) %>%
        dplyr::select(sitename, date, myvar) %>% 
        dplyr::rename(wetd_cru = myvar) %>% 
        dplyr::mutate(year = lubridate::year(date), moy = lubridate::month(date)) %>% 
        dplyr::select(-date) %>% 
        dplyr::right_join(mdf, by = c("sitename", "year", "moy"))
    }  
    
    ## VPD
    ## calculated as a function of vapour pressure and temperature, vapour
    ## pressure is given by CRU data.
    if ("vap" %in% cruvars){
      ## calculate VPD (vap is in hPa)
      mdf <-  mdf %>%
        mutate( vpd_vap_cru_temp_cru = calc_vpd( eact = 1e2 * vap_cru, tc = temp_cru ) )
    }
    
    ## expand monthly to daily data
    if (length(cruvars)>0){ 
      ddf <- expand_clim_cru_monthly( mdf, cruvars ) %>%
        right_join( ddf, by = "date" )
    }

    ## Write to file    
    readr::write_csv(ddf, path = csvfiln)
    
  } else {
    ## Read from file
    ddf <- readr::read_csv( csvfiln )
    
  }
  return( ddf )

}


##--------------------------------------------------------------------
## Extract temperature time series for a set of sites at once (opening
## each file only once).
##--------------------------------------------------------------------
get_input_sofun_climate_globalfields_watch_byvar <- function( varnam, settings_input, settings_sims ){

  dirn <- paste0( settings_input$path_watch_wfdei, "/", varnam, "/" )

  ## loop over all year and months that are required
  year_start <- settings_sims %>% 
    tidyr::unnest(params_siml) %>%
    dplyr::pull(firstyeartrend) %>% 
    min()
  
  year_end <- settings_sims %>% 
    dplyr::pull(year_end) %>% 
    max()
  
  allmonths <- 1:12
  allyears <- year_start:year_end

  ## construct data frame holding longitude and latitude info
  df_lonlat <- tibble(
    sitename = settings_sims$sitename,
    lon      = settings_sims$lon,
    lat      = settings_sims$lat
    )

  ## extract all the data
  df <- expand.grid(allmonths, allyears) %>%
    dplyr::as_tibble() %>%
    setNames(c("mo", "yr")) %>%
    rowwise() %>%
    dplyr::mutate(filename = paste0( settings_input$path_watch, "/", varnam, "/", varnam, "_WFDEI_", sprintf( "%4d", yr ), sprintf( "%02d", mo ), ".nc" )) %>%
    dplyr::mutate(data = purrr::map(filename, ~extract_pointdata_allsites(., df_lonlat ) ))

  ## rearrange to a daily data frame
  complement_df <- function(df){
    df <- df %>% 
      setNames(., c("myvar")) %>%
      mutate( dom = 1:nrow(.))
    return(df)
  }
  ddf <- df %>% 
    tidyr::unnest(data) %>% 
    dplyr::mutate(data = purrr::map(data, ~complement_df(.))) %>%
    tidyr::unnest(data) %>%
    dplyr::select(sitename, mo, yr, dom, myvar) %>% 
    dplyr::mutate(date = lubridate::ymd(paste0(as.character(yr), "-", sprintf( "%02d", mo), "-", sprintf( "%02d", dom))) ) %>% 
    dplyr::select(-mo, -yr, -dom)
  
  return( ddf )
}


##--------------------------------------------------------------------
## Extract temperature time series for a set of sites at once (opening
## each file only once).
##--------------------------------------------------------------------
get_input_sofun_climate_globalfields_cru_byvar <- function( varnam, settings_input, settings_sims ){

  ## construct data frame holding longitude and latitude info
  df_lonlat <- tibble(
    sitename = settings_sims$sitename,
    lon      = settings_sims$lon,
    lat      = settings_sims$lat
    )

  ## extract the data
  filename <- list.files( settings_input$path_cru, pattern=paste0( varnam, ".dat.nc" ) )
  df <- extract_pointdata_allsites( paste0(settings_input$path_cru, filename), df_lonlat, get_time = TRUE ) %>% 
    dplyr::mutate(data = purrr::map(data, ~setNames(., c("myvar", "date"))))

  ## rearrange to a daily data frame
  ddf <- df %>% 
    tidyr::unnest(data)
  
  return( ddf )
}


##-----------------------------------------------------------
## Returns a dataframe with all climate input data for one site
## and writes this to CSV and Fortran-formatted input files
## on the fly.
##-----------------------------------------------------------
prepare_input_sofun_climate_bysite <- function( sitename, ddf, settings_input, settings_sims, overwrite_csv=FALSE, verbose=FALSE ){

  if (verbose) print(paste("Writing climate input files...", sitename ))

  ## path of CSV file with data for this site
  dir <- paste0( settings_input$path_input, "/sitedata/climate/", sitename )
  if (!dir.exists(dir)) system( paste0( "mkdir -p ", dir ) )
  csvfiln <- paste0( dir, "/clim_daily_lev3_", sitename, ".csv" )

  # ## Add site name to dataframe (is merged by rows with ddf of other sites)
  # ddf <- ddf %>% dplyr::select( -(starts_with("year_dec")) ) %>% mutate( sitename = sitename )

  if (overwrite_csv || !file.exists(csvfiln)){
    ##----------------------------------------------------------------------
    ## Write fortran-formatted ascii files with daily values for each year 
    ## based on the CSV file written above (clim_daily_<sitename>.csv)
    ## Necessary because reading from CSV is a pain in Fortran.
    ##----------------------------------------------------------------------
    ## temperature
    if ("temp_day_fluxnet2015" %in% names(ddf)){
      ddf <- ddf %>% mutate( temp = temp_day_fluxnet2015 )        
    } else if ("temp_fluxnet2015" %in% names(ddf)) {
      ddf <- ddf %>% mutate( temp = temp_fluxnet2015 )
    } else {
      ddf <- ddf %>% mutate( temp = NA )
    }
    if ("temp_watch" %in% names(ddf) && "temp_cru_int" %in% names(ddf) ){
      ddf <- ddf %>% mutate( temp = ifelse( !is.na(temp), temp, ifelse( !is.na(temp_watch), temp_watch, temp_cru_int ) ) )
    } else if ("temp_watch" %in% names(ddf)){
      ddf <- ddf %>% mutate( temp = ifelse( !is.na(temp), temp, temp_watch ) )
    }
    
    ## precipitation
    if ("prec_fluxnet2015" %in% names(ddf)){
      ddf <- ddf %>% mutate( prec = prec_fluxnet2015 )        
    } else {
      ddf <- ddf %>% mutate( prec = NA )
    }      
    if ("prec_watch" %in% names(ddf) && "prec_cru_int" %in% names(ddf) ){
      ddf <- ddf %>% mutate( prec = ifelse( !is.na(prec), prec, ifelse( !is.na(prec_watch), prec_watch, prec_cru_gen ) ) )
    } else if ("prec_watch" %in% names(ddf)){
      ddf <- ddf %>% mutate( prec = ifelse( !is.na(prec), prec, prec_watch ) )
    }
    
    ## VPD
    if ("vpd_day_fluxnet2015" %in% names(ddf)){
      ddf <- ddf %>% mutate( vpd = vpd_day_fluxnet2015 )        
    } else if ("vpd_fluxnet2015" %in% names(ddf)){
      ddf <- ddf %>% mutate( vpd = vpd_fluxnet2015 )        
    } else {
      ddf <- ddf %>% mutate( vpd = NA )
    }
    if ("vpd_qair_watch_temp_watch" %in% names(ddf) && "vpd_vap_cru_temp_cru_int" %in% names(ddf) ){
      ddf <- ddf %>% mutate( vpd = ifelse( !is.na(vpd), vpd, ifelse( !is.na(vpd_qair_watch_temp_watch), vpd_qair_watch_temp_watch, vpd_vap_cru_temp_cru_int ) ) )
    } else if ("vpd_qair_watch_temp_watch" %in% names(ddf)){
      ddf <- ddf %>% mutate( vpd = ifelse( !is.na(vpd), vpd, temp_watch ) )
    }
    
    ## ppfd
    if ("ppfd_fluxnet2015" %in% names(ddf)){
      ddf <- ddf %>% mutate( ppfd = ppfd_fluxnet2015 )        
    } else {
      ddf <- ddf %>% mutate( ppfd = NA )
    }
    if ("ppfd_watch" %in% names(ddf) ){
      ddf <- ddf %>% mutate( ppfd = ifelse( !is.na(ppfd), ppfd, ifelse( !is.na(ppfd_watch), ppfd_watch, NA ) ) )
    } 
    
    ## patm
    if ("patm_fluxnet2015" %in% names(ddf)){
      ddf <- ddf %>% mutate( patm = patm_fluxnet2015 )        
    } else {
      ddf <- ddf %>% mutate( patm = NA )
    }
    if ("patm_watch" %in% names(ddf) ){
      ddf <- ddf %>% mutate( patm = ifelse( !is.na(patm), patm, ifelse( !is.na(patm_watch), patm_watch, NA ) ) )
    }    
    
    ## netrad
    if (settings_sims$params_siml[[1]]$in_netrad){
      ddf <- ddf %>%  mutate( nrad = netrad_fluxnet2015 )
      if ("nrad_watch" %in% names(ddf) ){
        ddf <- ddf %>% mutate( nrad = ifelse( !is.na(nrad), nrad, ifelse( !is.na(nrad_watch), nrad_watch, NA ) ) )
      } 
      
    } else {
      
      ## cloud cover
      if ( "ccov_cru_int" %in% names(ddf) ){
        ddf <- ddf %>%  mutate( ccov = ccov_cru_int )
      } else if ("ccov_dummy" %in% names(ddf)){
        ddf <- ddf %>% mutate( ccov = ccov_dummy )
      }
      
    }
    
    keepvars <- c("sitename", "date", "temp", "prec", "temp", "vpd", "ppfd", "patm")
    ddf <- ddf %>% mutate(  temp   = fill_gaps( temp   ),
                            prec   = fill_gaps( prec, is.prec=TRUE ),
                            vpd    = fill_gaps( vpd    ),
                            ppfd   = fill_gaps( ppfd   ),
                            ccov   = fill_gaps( ccov ),
                            patm   = fill_gaps( patm ),
                          ) %>% 

      ## remove leap year dates (sofun doesn't have leap years)
      dplyr::filter( !( lubridate::month(date)==2 & lubridate::mday(date)==29 ) )
    
    ## Help. I don't know why this doesn't work with ifelse inside mutate
    if (settings_sims$params_siml[[1]]$in_netrad){
      ddf <- ddf %>% mutate( nrad = fill_gaps( nrad ) )
      keepvars <- c(keepvars, "nrad")
    } else {
      ddf <- ddf %>% mutate( ccov = fill_gaps( ccov ) )
      keepvars <- c(keepvars, "ccov")
    }
    ddf <- ddf %>% 
      dplyr::select(keepvars)
    
    ##----------------------------------------------------------------------
    ## Write climate data to CSV files: 
    ## <settings_input$path_input>/sitedata/climate/<sitename>/clim_daily_<sitename>.csv 
    ## (may be read by Python directly???)
    ##----------------------------------------------------------------------
    readr::write_csv( ddf, path = csvfiln ) 
    
  } else {
    
    ##----------------------------------------------------------------------
    ## Read from file
    ##----------------------------------------------------------------------
    ddf <- readr::read_csv( csvfiln ) 

  }

  return( ddf )

}

##-----------------------------------------------------------
## Returns a dataframe with fAPAR input data for one site
## and writes this to CSV and Fortran-formatted input files
## on the fly.
##-----------------------------------------------------------
prepare_input_sofun_fapar_bysite <- function( sitename, settings_input, settings_sims, overwrite=FALSE, overwrite_csv=FALSE, verbose=FALSE ){

  if (verbose) print(paste0("Getting fAPAR data for site ", sitename, " ..." ) )

  ## File path of fAPAR CSV file for this site
  dir <- paste0( settings_input$path_input, "/sitedata/fapar/", sitename )
  
  if (!dir.exists(dir)) system( paste0( "mkdir -p ", dir ) )
  
  csvfiln <- paste0( dir, "/fapar_daily_", sitename, ".csv" )

  if ( file.exists( csvfiln ) && !overwrite_csv ){

    ddf <- readr::read_csv( csvfiln ) %>% mutate( fapar = as.numeric(fapar) )

  } else {

    ## Initialise daily dataframe (WITHOUT LEAP YEARS, SOFUN USES FIXED 365-DAYS YEARS!)
    ddf <- init_dates_dataframe( year(settings_sims$date_start), year(settings_sims$date_end), noleap = TRUE ) %>%
           ## Add site name to dataframe (is merged by rows with ddf of other sites)
           mutate( sitename = sitename )

    ##----------------------------------------------------------------------
    ## Download (if necessary) and read
    ##----------------------------------------------------------------------
    if (settings_input$fapar=="MODIS_FPAR_MCD15A3H"){

      ## Make sure data is available for this site
      error <- check_download_MODIS_FPAR_MCD15A3H( settings_input, settings_sims, sitename )

      if (error!=1){
        ## Take only file for this site
        filn <- list.files( settings_input$path_MODIS_FPAR_MCD15A3H, pattern = paste0("dfapar_MODIS_FPAR_MCD15A3H_gee_MCD15A3H_", sitename, "_gee_subset.csv") )

        ## This returns a data frame with columns (date, temp, prec, nrad, ppfd, vpd, ccov)
        ## IMPORTANT: This is gapfilled data. Original data is in <settings_input$path_MODIS_FPAR_MCD15A3H>/raw/
        ## Gap-filling is done with 'getin/gapfill_modis.R'. The gapfilling step is not yet implemented within prepare_input_sofun().
        if (length(filn)!=0){
          tmp <- readr::read_csv( paste0( settings_input$path_MODIS_FPAR_MCD15A3H, filn ) )
          if (settings_input$splined_fapar){
            tmp <- tmp %>% dplyr::select( date, fapar = spline )
          } else {
            tmp <- tmp %>% dplyr::select( date, fapar = interpl )
          }
          ddf <- tmp %>%
            mutate( fapar = as.numeric(fapar) ) %>%
            right_join( ddf, by = "date" )
        } else {
          error <- 1
          ddf <- ddf %>% mutate( fapar = NA )
        }

      }

    } else if (settings_input$fapar=="MODIS_EVI_MOD13Q1"){
      
      ## Make sure data is available for this site
      error <- check_download_MODIS_EVI_MOD13Q1( settings_input, settings_sims, sitename )
      
      if (error!=1){
        ## Take only file for this site
        filn <- list.files( settings_input$path_MODIS_EVI_MOD13Q1, pattern = paste0("dfapar_MODIS_EVI_MOD13Q1_gee_MOD13Q1_", sitename, "_gee_subset.csv") )
        
        ## This returns a data frame with columns (date, temp, prec, nrad, ppfd, vpd, ccov)
        ## IMPORTANT: This is gapfilled data. Original data is in <settings_input$path_MODIS_EVI_MOD13Q1>/raw/
        ## Gap-filling is done with 'getin/gapfill_modis.R'. The gapfilling step is not yet implemented within prepare_input_sofun().
        if (length(filn)!=0){
          tmp <- readr::read_csv( paste0( settings_input$path_MODIS_EVI_MOD13Q1, filn ) )
          if (settings_input$splined_fapar){
            tmp <- tmp %>% dplyr::select( date, fapar = spline )
          } else {
            tmp <- tmp %>% dplyr::select( date, fapar = interpl )
          }
          ddf <- tmp %>%
            mutate( fapar = as.numeric(fapar) ) %>%
            right_join( ddf, by = "date" )
        } else {
          error <- 1
          ddf <- ddf %>% mutate( fapar = NA )
        }
      }
      
    }    

    if (error!=1){
      ##----------------------------------------------------------------------
      ## Write fapar data to CSV files: 
      ## <settings_input$path_input>/sitedata/fapar/<sitename>/fapar_daily_<sitename>.csv 
      ## (may be read by Python directly???)
      ##----------------------------------------------------------------------
      write_csv( ddf, path = csvfiln )

      filelist <- list.files( paste0( settings_input$path_input, "/sitedata/fapar/", sitename, "/", as.character(year(ddf$date[1])), "/" ) )
      
      if (length(filelist)==0 || overwrite){
        ##----------------------------------------------------------------------
        ## Write Fortran-formatted ASCII files with daily values for each year 
        ## based on the CSV file written above (clim_daily_<sitename>.csv)
        ## Necessary because reading from CSV is a pain in Fortran.
        ##----------------------------------------------------------------------
        if (settings_sims$implementation=="fortran"){

          ## get mean seasonal cycle. This is relevant for years where no MODIS data is available.
          ddf_meandoy <- ddf %>% mutate( doy=yday(date) ) %>% group_by( doy ) %>% summarise( meandoy = mean( fapar , na.rm=TRUE ) )
          
          ## subset only this year
          out <- ddf %>% mutate( doy=yday(date) )
          
          ## fill gaps with mean seasonal cycle (for pre-MODIS years, entire year is mean seasonal cycle)
          if (nrow(out)==0){
            out <- init_dates_dataframe( min(unique(year(ddf$date))), max(unique(year(ddf$date))) ) %>% 
              mutate( fapar = NA ) %>% 
              dplyr::filter( !( month(date)==2 & mday(date)==29 ) ) %>% 
              mutate( doy=yday(date) )
          }
          
          out <- out %>% left_join( ddf_meandoy, by="doy" )
          
          ## fill gaps with mean by DOY/MOY
          out$fapar[ which( is.na( out$fapar ) ) ] <- out$meandoy[ which( is.na( out$fapar ) ) ]
          
          ## define directory name for SOFUN input
          dirnam <- paste0( settings_input$path_input, "/sitedata/fapar/", sitename, "/", as.character(yr), "/" )
          system( paste( "mkdir -p", dirnam ) )
          
        }
      }
    }
  }

  ## Add site name to dataframe (is merged by rows with ddf of other sites)
  ddf <- ddf %>% dplyr::select( -(starts_with("year_dec")) )
  
  return(ddf)

}


##-----------------------------------------------------------
## Returns a list of two data frames, one with data at original
## modis dates (df), and one interpolated to all days (ddf).
##-----------------------------------------------------------
prepare_input_sofun_fapar_bysite_GEE <- function( df_siteinfo, start_date,
                                                  end_date, settings_sims, settings_input,
                                                  overwrite_raw, overwrite_nice, overwrite_csv,
                                                  band_var, band_qc, prod, prod_suffix, varnam, productnam, scale_factor, 
                                                  period, do_plot_interpolated, python_path, gee_path){
  
  ##---------------------------------------------
  ## Define names
  ##---------------------------------------------
  sitename <- df_siteinfo$sitename[1]
  df_siteinfo <- slice(df_siteinfo, 1)
  #print(paste("getting fapar for site", sitename))
  
  dirnam_daily_csv <- paste0(settings_input$path_input, "sitedata/fapar/", sitename)
  dirnam_nice_csv <- settings_input[paste0("path_", stringr::str_replace(productnam ,"_gee", ""))] %>% unlist() %>% unname()
  dirnam_raw_csv <- paste0( dirnam_nice_csv, "/raw/" )
  
  if (!dir.exists(dirnam_daily_csv)) system( paste( "mkdir -p ", dirnam_daily_csv ) )
  if (!dir.exists(dirnam_nice_csv)) system( paste( "mkdir -p ", dirnam_nice_csv ) )
  if (!dir.exists(dirnam_raw_csv)) system( paste( "mkdir -p ", dirnam_raw_csv ) )
  
  filnam_daily_csv <- paste0( dirnam_daily_csv, "/fapar_daily_", sitename, ".csv" )
  filnam_nice_csv <- paste0( dirnam_nice_csv, "/", varnam, "_", productnam, "_", sitename, "_subset.csv" )
  filnam_raw_csv <- paste0( dirnam_raw_csv, sitename, "_", prod_suffix, "_gee_subset.csv" )
  
  out <- list()
  do_continue <- TRUE
  
  ## Save error code (0: no error, 1: error: file downloaded bu all data is NA, 2: file not downloaded)
  df_error <- tibble()
  
  if (file.exists(filnam_daily_csv) && !overwrite_csv){
    ##---------------------------------------------
    ## Read daily interpolated and gapfilled
    ##---------------------------------------------
    out$ddf <- readr::read_csv( filnam_daily_csv, col_types = cols() )
    
  } else {
    
    if (file.exists(filnam_nice_csv) && file.exists(filnam_daily_csv) && !overwrite_nice){
      ##---------------------------------------------
      ## Read nicely formatted 8-daily      
      ##---------------------------------------------
      out$df  <- readr::read_csv( filnam_nice_csv, col_types = cols() )
      out$ddf <- readr::read_csv( filnam_daily_csv, col_types = cols() )
      
    } else {
      
      if (file.exists(filnam_raw_csv) && !overwrite_raw){
        ## Raw downloaded file will be read separately
        # print( paste( "File exists already:", filnam_modis_raw_csv ) )
        # print(paste("site", sitename))
        
      } else {
        ##---------------------------------------------
        ## Download via Google Earth Engine using the python function
        ##---------------------------------------------
        path_info <- paste0(filnam_raw_csv, "info_lonlat.csv")
        write.csv( dplyr::select( df_siteinfo, site = sitename, latitude = lat, longitude = lon), file=path_info, row.names=FALSE )
        
        start = Sys.time()
        system(sprintf("%s %s/gee_subset.py -p %s -b %s %s -s %s -e %s -f %s -d %s -sc 30",
                       python_path,
                       gee_path,
                       prod,
                       band_var,
                       band_qc,
                       start_date,
                       end_date,
                       path_info,
                       filnam_raw_csv
        ), wait = TRUE)
        
        end = Sys.time()
        proc_time = as.vector(end - start)
        print( paste( "... completed in", format( proc_time, digits = 3), "sec" ) )
        
        ## Raw downloaded data is saved to file
        print( paste( "raw data file written:", filnam_raw_csv ) )
        
      }
      
      ##--------------------------------------------------------------------
      ## Read raw data and create a data frame holding the complete time series
      ## Note: 'date', 'doy', 'dom', etc. refer to the date, centered within 
      ## each N-day period. 'date_start' refers to the beginning of each 
      ## N-day period.
      ##--------------------------------------------------------------------
      if (file.exists(filnam_raw_csv)){
        
        df <- readr::read_csv( filnam_raw_csv, col_types = cols() ) %>%
          dplyr::mutate(  date_start = ymd(date) ) %>%
          dplyr::mutate(  date = date_start + days( as.integer(period/2) ),
                          doy = yday(date),
                          year = year(date)
          ) %>% 
          dplyr::mutate( ndayyear = ifelse( leap_year( date ), 366, 365 ) ) %>%
          dplyr::mutate( year_dec = year(date) + (yday(date)-1) / ndayyear ) %>% 
          dplyr::select( -longitude, -latitude, -product, -ndayyear )
        
        ## Apply scale factor, specific for each product
        if (any(!is.na(df[[band_var]]))){
          df[[band_var]] <- df[[band_var]] * scale_factor
        } else {
          do_continue <- FALSE
        }
        
      } else {
        
        print( paste( "WARNING: RAW DATA FILE NOT FOUND FOR SITE:", sitename ) )
        df_error <- df_error %>% bind_rows( tibble( mysitename=sitename, error=2 ) ) 
        out <- NA
        do_continue <- FALSE
        
      }      
      
      if (do_continue){
        ##---------------------------------------------
        ## Clean (gapfill and interpolate) full time series data to 8-days, daily, and monthly
        ##--------------------------------------------------------------------
        print("gapfilling and interpolating to daily ...")
        out <- gapfill_modis(
          df,
          sitename, 
          year_start = df_siteinfo$year_start,
          year_end   = df_siteinfo$year_end,
          qc_name = band_qc, 
          prod = prod_suffix,
          splined_fapar = settings_input$splined_fapar,
          do_interpolate = TRUE,
          do_plot_interpolated = do_plot_interpolated,
          dir = settings_input$path_input
        )
        
        ##---------------------------------------------
        ## save nicely formatted 8-daily to file
        ##---------------------------------------------
        readr::write_csv( out$df,  path=filnam_nice_csv )
        readr::write_csv( out$ddf, path=filnam_daily_csv )
        
      } else {
        ddf_out <- init_dates_dataframe(
          lubridate::year(start_date),
          lubridate::year(end_date)
          ) %>% 
          mutate(modisvar_interpol = NA)
      }
      
    }
  }
  
  if (do_continue){
    ##---------------------------------------------
    ## Write SOFUN-formatted input
    ##---------------------------------------------
    ## get mean seasonal cycle. This is relevant for years where no MODIS data is available.
    df_meandoy <- out$ddf %>% 
      group_by( doy ) %>% 
      summarise( meandoy = mean( modisvar_interpol , na.rm=TRUE ) ) 
    
    ## in separate formatted file 
    ddf_out <- out$ddf
    
    ## fill gaps with mean seasonal cycle (for pre-MODIS years, entire year is mean seasonal cycle)
    if (nrow(ddf_out)==0){
      ddf_out <- init_dates_dataframe( df_siteinfo$year_start, df_siteinfo$year_end ) %>% 
        mutate( modisvar_interpol = NA ) %>% 
        dplyr::filter( !( month(date)==2 & mday(date)==29 ) ) %>% 
        mutate( doy=yday(date) )
    }
    ddf_out <- ddf_out %>% left_join( df_meandoy, by="doy" )
    
    ## fill gaps with mean by DOY/MOY
    ddf_out$modisvar_interpol[ which( is.na( ddf_out$modisvar_interpol ) ) ] <- ddf_out$meandoy[ which( is.na( ddf_out$modisvar_interpol ) ) ]
    
  }
  
  df_error <- df_error %>% bind_rows( tibble( mysitename=sitename, error=0 ) ) 
  
  return(ddf_out) 
}


gapfill_modis <- function( df, sitename, year_start, year_end, qc_name, prod, splined_fapar, do_interpolate=FALSE, do_plot_interpolated=FALSE, dir = "./" ){
  ##--------------------------------------
  ## Returns data frame containing data 
  ## (and year, moy, doy) for all available
  ## months. Interpolated to mid-months
  ## from original 16-daily data.
  ##--------------------------------------

  # require( signal )  ## for sgolayfilt, masks filter()

  ##--------------------------------------
  ## CLEAN AND GAP-FILL
  ##--------------------------------------
  if (prod=="MOD13Q1"){
    ##--------------------------------------
    ## This is for MOD13Q1 EVI data downloaded from Google Earth Engine with gee_subset
    ## USED AS MODIS EVI GEE for P-model
    ##--------------------------------------
    ## QC interpreted according to https://explorer.earthengine.google.com/#detail/MODIS%2F006%2FMOD13Q1:
    ## 0: Good data, use with confidence
    ## 1: Marginal data, useful but look at detailed QA for more information
    ## 2: Pixel covered with snow/ice
    ## 3: Pixel is cloudy
    ##--------------------------------------
    df <- df %>%  mutate( good_quality  = ifelse( SummaryQA %in% c(0, 1, 2), TRUE, FALSE ) ) %>%
                  rename( modisvar = EVI )

    ## Plot effect of filtering steps
    if (do_plot_interpolated){
      dir <- paste0(dir, "/fig_fapar_gapfilling/")
      if (!dir.exists(dir)) system(paste0("mkdir ", dir))
      plotfiln <- paste0( dir, "evi_MOD13Q1gee_", sitename, ".pdf" )
      print( paste( "Gapfilling illustrated in:", plotfiln ) )
      pdf( plotfiln, width=15, height=6 )
      par(xpd=TRUE)
      with( df, plot( year_dec, modisvar, pch=16, col='black', main=sitename, ylim=c(0,1), xlab="year", ylab="MOD13Q1gee", las=1 ) )
      with( dplyr::filter( df, good_quality & modisvar > 0.0), points( year_dec, modisvar, pch=16, col='blue' ) )

      legend( "topleft", 
        c( "initial", "good_quality" ), 
        col=c("black", "blue" ), pch=16, bty="n", inset = c(0,-0.2)
        )

    }

    ## Actually filter
    df <- df %>% mutate( modisvar = ifelse( good_quality, modisvar, NA )  )  ## replace by NA for values to be filtered out
    # df <- df %>% dplyr::filter( good_quality )

    ##--------------------------------------
    ## replace missing values with mean by DOY (mean seasonal cycle)
    ##--------------------------------------
    ## get mean seasonal cycle
    ddf_meandoy <- df %>% group_by( doy ) %>% summarise( meandoy = mean( modisvar , na.rm=TRUE ) )

    ## attach mean seasonal cycle as column 'meandoy' to daily dataframe
    df <- df %>% left_join( ddf_meandoy, by="doy" ) %>%

      ## fill gaps at head and tail
      mutate( modisvar = ifelse( is.na(modisvar), meandoy, modisvar ) )


    ##--------------------------------------
    ## This is for data downloaded using RModisTools
    ##--------------------------------------

    # ##--------------------------------------
    # ## data cleaning
    # ##--------------------------------------
    # ## Replace data points with quality flag = 2 (snow covered) by 0
    # # df_gapfld$centre[ which(df_gapfld$centre_qc==2) ] <- max( min( df_gapfld$centre ), 0.0 )
    # df_gapfld$raw <- df_gapfld$centre
    # df_gapfld$centre[ which(df_gapfld$centre<0) ] <- NA

    # if (!is.null(df_gapfld$centre_qc)){
    #   ## Drop all data with quality flag 3, 1 or -1
    #   df_gapfld$centre[ which(df_gapfld$centre_qc==3) ]  <- NA  # Target not visible, covered with cloud
    #   df_gapfld$centre[ which(df_gapfld$centre_qc==1) ]  <- NA  # Useful, but look at other QA information
    #   df_gapfld$centre[ which(df_gapfld$centre_qc==-1) ] <- NA  # Not Processed
    # }

    # ## open plot for illustrating gap-filling
    # if (do_plot_interpolated) pdf( paste("fig/evi_fill_", sitename, ".pdf", sep="" ), width=10, height=6 )
    # if (do_plot_interpolated) plot( df_gapfld$year_dec, df_gapfld$raw, pch=16, col='black', main=sitename, ylim=c(0,1), xlab="year", ylab="MODIS EVI 250 m", las=1 )
    # left <- seq(2000, 2016, 2)
    # right <- seq(2001, 2017, 2)
    # if (do_plot_interpolated) rect( left, -99, right, 99, border=NA, col=rgb(0,0,0,0.2) )
    # if (do_plot_interpolated) points( df_gapfld$year_dec, df_gapfld$centre, pch=16, col='red' )

    # # ## Drop all data identified as outliers = lie outside 5*IQR
    # # df_gapfld$centre <- remove_outliers( df_gapfld$centre, coef=5 ) ## maybe too dangerous - removes peaks

    # ## add points to plot opened before
    # if (do_plot_interpolated) points( df_gapfld$year_dec, df_gapfld$centre, pch=16, col='blue' )

    # ##--------------------------------------
    # ## get LOESS spline model for predicting daily values (below)
    # ##--------------------------------------
    # idxs    <- which(!is.na(df_gapfld$centre))
    # myloess <- try( with( df_gapfld, loess( centre[idxs] ~ year_dec[idxs], span=0.01 ) ))
    # i <- 0
    # while (class(myloess)=="try-error" && i<50){
    #   i <- i + 1
    #   print(paste("i=",i))
    #   myloess <- try( with( df_gapfld, loess( centre[idxs] ~ year_dec[idxs], span=(0.01+0.002*(i-1)) ) ))
    # }
    # print("ok now...")

    # ##--------------------------------------
    # ## get spline model for predicting daily values (below)
    # ##--------------------------------------
    # spline <- try( with( df_gapfld, smooth.spline( year_dec[idxs], centre[idxs], spar=0.001 ) ) )

    # ## aggregate by DOY
    # agg <- aggregate( centre ~ doy, data=df_gapfld, FUN=mean, na.rm=TRUE )
    # if (is.element("centre_meansurr", names(df_gapfld))){
    #   agg_meansurr <- aggregate( centre_meansurr ~ doy, data=df_gapfld, FUN=mean, na.rm=TRUE )
    #   agg <- agg %>% left_join( agg_meansurr ) %>% dplyr::rename( centre_meandoy=centre, centre_meansurr_meandoy=centre_meansurr )
    # } else {
    #   agg <- agg %>% dplyr::rename( centre_meandoy=centre )      
    # }
    # df_gapfld <- df_gapfld %>% left_join( agg )

    # ##--------------------------------------
    # ## gap-fill with information from surrounding pixels - XXX CHANGED THIS FOR AMERIWUE: NO INFO FROM MEAN OF SURROUNDINGS USED XXX
    # ##--------------------------------------
    # idxs <- which( is.na(df_gapfld$centre) )
    # if (is.element("centre_meansurr", names(df_gapfld))){
    #   ## get current anomaly of mean across surrounding pixels w.r.t. its mean annual cycle
    #   df_gapfld$anom_surr    <- df_gapfld$centre_meansurr / df_gapfld$centre_meansurr_meandoy
    #   # df_gapfld$centre[idxs] <- df_gapfld$centre_meandoy[idxs] * df_gapfld$anom_surr[idxs]
    # } else {
    #   # df_gapfld$centre[idxs] <- df_gapfld$centre_meandoy[idxs]
    # }
    # if (do_plot_interpolated) with( df_gapfld, points( year_dec[idxs], centre[idxs], pch=16, col='green' ) )
    # if (do_plot_interpolated) legend("topright", c("modis", "outliers", "after bad values dropped and outliers removed", "added from mean of surrounding" ), col=c("black", "red", "blue", "green" ), pch=16, bty="n" )
    # # legend("topleft", c("R LOESS smoothing with span=0.01", "R smooth.spline"), col=c("red", "dodgerblue"), lty=1, bty="n" )


    # # points( df_gapfld$yr_dec_read[idxs],  df_gapfld$centre[idxs],  pch=16 )
    # # points( df_gapfld$yr_dec_read[-idxs], df_gapfld$centre[-idxs], pch=16, col='blue' )

    # # ## Gap-fill remaining again by mean-by-DOY
    # # idxs <- which( is.na(df_gapfld$centre) )
    # # df_gapfld$centre[idxs] <- df_gapfld$centre_meandoy[idxs]
    # # # points( df_gapfld$yr_dec_read[idxs], df_gapfld$centre[idxs], pch=16, col='red' )

    # # ## Gap-fill still remaining by linear approximation
    # # idxs <- which( is.na(df_gapfld$centre) )
    # # if (length(idxs)>1){
    # #   df_gapfld$centre <- approx( df_gapfld$year_dec[-idxs], df_gapfld$centre[-idxs], xout=df_gapfld$year_dec )$y
    # # }

    # # points( df_gapfld$yr_dec_read[idxs], df_gapfld$centre[idxs], pch=16, col='green' )
    # # lines( df_gapfld$yr_dec_read, df_gapfld$centre )
    # # dev.off()

  } else if (prod=="MCD15A3H"){
    ##--------------------------------------
    ## This is for MCD15A3H FPAR data downloaded from Google Earth Engine with gee_subset
    ## USED AS MODIS FPAR GEE for P-model
    ##--------------------------------------
    ## QC interpreted according to https://explorer.earthengine.google.com/#detail/MODIS%2F006%2FMCD15A3H:
    ## Bit 0: MODLAND_QC bits
    ##   0: Good quality (main algorithm with or without saturation)
    ##   1: Other quality (back-up algorithm or fill values)
    ## Bit 1: Sensor
    ##   0: Terra
    ##   1: Aqua
    ## Bit 2: Dead detector
    ##   0: Detectors apparently fine for up to 50% of channels 1, 2
    ##   1: Dead detectors caused >50% adjacent detector retrieval
    ## Bits 3-4: Cloud state
    ##   0: Significant clouds NOT present (clear)
    ##   1: Significant clouds WERE present
    ##   2: Mixed cloud present in pixel
    ##   3: Cloud state not defined, assumed clear
    ## Bits 5-7: SCF_QC
    ##   0: Main (RT) method used with no saturation, best result possible
    ##   1: Main (RT) method used with saturation, good and very usable
    ##   2: Main (RT) method failed due to bad geometry, empirical algorithm used
    ##   3: Main (RT) method failed due to problems other than geometry, empirical algorithm used
    ##   4: Pixel not produced at all, value couldn't be retrieved (possible reasons: bad L1B data, unusable MOD09GA data)
    ##--------------------------------------

    ## This is interpreted according to https://lpdaac.usgs.gov/sites/default/files/public/product_documentation/mod15_user_guide.pdf, p.9
    ## see mod15_user_guide.pdf

    df$qc_bitname <- sapply( seq(nrow(df)), function(x) as.integer( intToBits( df$FparLai_QC[x] )[1:8] ) %>% rev() %>% as.character() %>% paste( collapse="" )  )

    ## MODLAND_QC bits
    ## 0: Good  quality (main algorithm with or without saturation)
    ## 1: Other quality (backup  algorithm or fill values)
    df$qc_bit0 <- substr( df$qc_bitname, start=8, stop=8 )

    ## Sensor
    ## 0: Terra
    ## 1: Aqua
    df$qc_bit1 <- substr( df$qc_bitname, start=7, stop=7 )

    ## Dead detector
    ## 0: Detectors apparently  fine  for up  to  50% of  channels  1,  2
    ## 1: Dead  detectors caused  >50%  adjacent  detector  retrieval
    df$qc_bit2 <- substr( df$qc_bitname, start=6, stop=6 )

    ## CloudState
    ## 00 0  Significant clouds  NOT present (clear)
    ## 01 1  Significant clouds  WERE  present
    ## 10 2  Mixed cloud present in  pixel
    ## 11 3  Cloud state not defined,  assumed clear
    df$qc_bit3 <- substr( df$qc_bitname, start=4, stop=5 )

    ## SCF_QC (five level confidence score)
    ## 000 0 Main (RT) method used, best result possible (no saturation)
    ## 001 1 Main (RT) method used with saturation. Good, very usable
    ## 010 2 Main (RT) method failed due to bad geometry, empirical algorithm used
    ## 011 3 Main (RT) method failed due to problems other than geometry, empirical algorithm used
    ## 100 4 Pixel not produced at all, value couldn???t be retrieved (possible reasons: bad L1B data, unusable MOD09GA data)
    df$qc_bit4 <- substr( df$qc_bitname, start=1, stop=3 )

    df <- df %>%  mutate(  good_quality  = ifelse( qc_bit0=="0", TRUE, FALSE ),
                           terra         = ifelse( qc_bit1=="0", TRUE, FALSE ),
                           dead_detector = ifelse( qc_bit2=="1", TRUE, FALSE ),
                           CloudState    = ifelse( qc_bit3=="00", 0, ifelse( qc_bit3=="01", 1, ifelse( qc_bit3=="10", 2, 3 ) ) ),
                           SCF_QC        = ifelse( qc_bit4=="000", 0, ifelse( qc_bit4=="001", 1, ifelse( qc_bit4=="010", 2, ifelse( qc_bit4=="011", 3, 4 ) ) ) )
                          ) %>%
                  select( -qc_bitname, -FparLai_QC, -qc_bit0, -qc_bit1, -qc_bit2, -qc_bit3, -qc_bit4 ) %>% 
                  rename( modisvar = Fpar )

    ## Plot effect of filtering steps
    if (do_plot_interpolated){
      dir <- paste0(dir, "/fig_fapar_gapfilling/")
      if (!dir.exists(dir)) system(paste0("mkdir ", dir))
      plotfiln <- paste0( dir, "fpar_MCD15A3H_v2_fill_", sitename, ".pdf" )
      print( paste( "Gapfilling illustrated in:", plotfiln ) )
      pdf( plotfiln, width=15, height=6 )
      par(xpd=TRUE)
      with( df, plot( year_dec, modisvar, pch=16, col='black', main=sitename, ylim=c(0,1), xlab="year", ylab="MCD15A3H", las=1 ) )
      with( dplyr::filter( df, CloudState!=1, modisvar!=1.0  ), points( year_dec, modisvar, pch=16, col='blue' ) ) # good_quality &  & SCF_QC!=4  & SCF_QC %in% c(0,1)

      # with( dplyr::filter( df, good_quality), points( year_dec, modisvar, pch=16, col='blue' ) )
      # with( dplyr::filter( df, good_quality & CloudState==0), points( year_dec, modisvar, pch=16, col='springgreen3' ) )
      # with( dplyr::filter( df, good_quality & CloudState==0 & !dead_detector ), points( year_dec, modisvar, pch=16, col='orchid' ) )
      # with( dplyr::filter( df, good_quality & CloudState==0 & !dead_detector & SCF_QC==0 ), points( year_dec, modisvar, pch=16, col='black' ) )
      legend( "topleft", 
        c("initial", "cleaned"
          # "good_quality, but not CloudState==0 and not(dead_detector) and not(SCF_QC==0)", 
          # "good_quality and CloudState==0, but not(dead_detector) and not(SCF_QC==0)", 
          # "good_quality and CloudState==0 and not(dead_detector), but not not(SCF_QC==0)",
          # "remaining"
          ), 
        col=c("black", "blue"
          # ,"springgreen3", "orchid", "black" 
          ), pch=16, bty="n", inset = c(0,-0.2)
        )
    }

    ## Actually filter
    # df <- df %>% dplyr::filter( CloudState!=1 )  #  good_quality & CloudState!=1 & SCF_QC!=4  & CloudState==0 & !dead_detector & SCF_QC==0
    df <- df %>% mutate( modisvar = ifelse( CloudState!=1 , modisvar, NA )  ) %>%  ## replace by NA for values to be filtered out   & SCF_QC %in% c(0,1)

      ## don't believe the hype
      mutate( modisvar = ifelse( modisvar==1.0, NA, modisvar ) ) %>%  

      ## Drop all data identified as outliers = lie outside 5*IQR
      mutate( modisvar = remove_outliers( modisvar, coef=3 ) )  # maybe too dangerous - removes peaks

    ##--------------------------------------
    ## replace missing values with mean by DOY (mean seasonal cycle)
    ##--------------------------------------
    ## get mean seasonal cycle
    ddf_meandoy <- df %>% group_by( doy ) %>% summarise( meandoy = mean( modisvar , na.rm=TRUE ) )

    ## attach mean seasonal cycle as column 'meandoy' to daily dataframe
    df <- df %>% left_join( ddf_meandoy, by="doy" ) %>%

      ## fill gaps at head and tail
      mutate( modisvar = ifelse( is.na(modisvar), meandoy, modisvar ) )

    
  }  else if (prod=="MOD15A2"){
    ## MOD15A2 contains fpar

    ##--------------------------------------
    ## data cleaning
    ##--------------------------------------
    # ## Drop all data with quality flag != 0
    # if (!is.null(df_gapfld$centre_qc)){
    #   df_gapfld$centre[ which(df_gapfld$centre_qc!=0) ] <- NA
    # }

    ## no quality info available for fpar from trevor

    ## remove values that are above 1
    df_gapfld$centre <- replace( df_gapfld$centre,  df_gapfld$centre>1.0, NA )

    ## open plot for illustrating gap-filling
    if (do_plot_interpolated) pdf( paste("fig/fpar_fill_", sitename, ".pdf", sep="" ), width=10, height=6 )
    if (do_plot_interpolated) plot( modis$year_dec, modis$centre, pch=16, col='black', main=sitename, ylim=c(0,1), xlab="year", ylab="MODIS FPAR 1 km", las=1 )
    left <- seq(2000, 2016, 2)
    right <- seq(2001, 2017, 2)
    if (do_plot_interpolated) rect( left, -99, right, 99, border=NA, col=rgb(0,0,0,0.2) )
    if (do_plot_interpolated) points( df_gapfld$year_dec, df_gapfld$centre, pch=16, col='red' )

    # ## Drop all data identified as outliers = lie outside 5*IQR
    # df_gapfld$centre <- remove_outliers( df_gapfld$centre, coef=5 ) ## maybe too dangerous - removes peaks

    ## add points to plot opened before
    if (do_plot_interpolated) points( df_gapfld$year_dec, df_gapfld$centre, pch=16, col='springgreen3' )

    ##--------------------------------------
    ## get LOESS spline model for predicting daily values (below)
    ##--------------------------------------
    idxs <- which(!is.na(df_gapfld$centre))
    if (length(idxs)>0){
      myloess <- try( with( df_gapfld, loess( centre[idxs] ~ year_dec[idxs], span=0.01 ) ))
      i <- 0
      while (class(myloess)=="try-error" && i<50){
        i <- i + 1
        print(paste("i=",i))
        myloess <- try( with( df_gapfld, loess( centre[idxs] ~ year_dec[idxs], span=(0.01+0.002*(i-1)) ) ))
      }
      print("ok now...")
    } else {
      missing <- TRUE
    }

    ##--------------------------------------
    ## get spline model for predicting daily values (below)
    ##--------------------------------------
    if (!missing){
      spline <- try( with( df_gapfld, smooth.spline( year_dec[idxs], centre[idxs], spar=0.001 ) ))
    }

    if (!missing){
      ## aggregate by DOY
      agg <- aggregate( centre ~ doy, data=df_gapfld, FUN=mean, na.rm=TRUE )
      if (is.element("centre_meansurr", names(df_gapfld))){
        agg_meansurr <- aggregate( centre_meansurr ~ doy, data=df_gapfld, FUN=mean, na.rm=TRUE )
        agg <- agg %>% left_join( agg_meansurr ) %>% dplyr::rename( centre_meandoy=centre, centre_meansurr_meandoy=centre_meansurr )
      } else {
        agg <- agg %>% dplyr::rename( centre_meandoy=centre )
      }
      df_gapfld <- df_gapfld %>% left_join( agg )

      ## get consecutive data gaps and fill only by mean seasonality if more than 3 consecutive dates are missing
      na_instances <- get_consecutive( is.na(df_gapfld$centre), leng_threshold=4, do_merge=FALSE )
      if (nrow(na_instances)>0){
        for (iinst in 1:nrow(na_instances)){
          idxs <- na_instances$idx_start[iinst]:(na_instances$idx_start[iinst]+na_instances$len[iinst]-1)
          if (is.element("centre_meansurr", names(df_gapfld))){
            ## get current anomaly of mean across surrounding pixels w.r.t. its mean annual cycle
            df_gapfld$anom_surr    <- df_gapfld$centre_meansurr / df_gapfld$centre_meansurr_meandoy
            df_gapfld$centre[idxs] <- df_gapfld$centre_meandoy[idxs] * df_gapfld$anom_surr[idxs]
          } else {
            df_gapfld$centre[idxs] <- df_gapfld$centre_meandoy[idxs]
          }
        }
      }

    }

    # ## Gap-fill still remaining by linear approximation
    # idxs <- which( is.na(df_gapfld$centre) )
    # if (length(idxs)>1){
    #   df_gapfld$centre <- approx( df_gapfld$year_dec[-idxs], df_gapfld$centre[-idxs], xout=df_gapfld$year_dec )$y
    # }

    # points( df_gapfld$year_dec[idxs], df_gapfld$centre[idxs], pch=16 )
    # points( df_gapfld$year_dec[-idxs], df_gapfld$centre[-idxs], pch=16, col='blue' )
    # lines(  df_gapfld$year_dec, df_gapfld$centre )

    # ## for pixels with low quality information, use mean of surroundings
    # if (is.element("centre_meansurr", names(df_gapfld))){
    #   for (idx in seq(nrow(df_gapfld))){
    #     if (df_gapfld_qc[idx,usecol]!=0) {
    #       df_gapfld$centre[idx] <- unname( apply( df_gapfld[idx,1:npixels], 1, FUN=mean, na.rm=TRUE ))
    #     }
    #   }
    # }

    # dev.off()

  } else if (prod=="MOD17A2H"){
    ## Contains MODIS GPP
    ## quality bitmap interpreted based on https://lpdaac.usgs.gov/dataset_discovery/modis/modis_products_table/mod17a2

    df$qc_bitname <- sapply( seq(nrow(df)), function(x) as.integer( intToBits( df$Psn_QC[x] )[1:8] ) %>% rev() %>% as.character() %>% paste( collapse="" )  )

    ## MODLAND_QC bits
    ## 0: Good  quality (main algorithm with  or without saturation)
    ## 1: Other quality (backup  algorithm or  fill  values)
    df$qc_bit0 <- substr( df$qc_bitname, start=8, stop=8 )
    # >>>>>>> 4a98b7722357887423c4ebe4f33497586902ab25

    ## Sensor
    ## 0: Terra
    ## 1: Aqua
    df$qc_bit1 <- substr( df$qc_bitname, start=7, stop=7 )

    ## Dead detector
    ## 0: Detectors apparently  fine  for up  to  50% of  channels  1,  2
    ## 1: Dead  detectors caused  >50%  adjacent  detector  retrieval
    df$qc_bit2 <- substr( df$qc_bitname, start=6, stop=6 )

    ## CloudState
    ## 00 0  Significant clouds  NOT present (clear)
    ## 01 1  Significant clouds  WERE  present
    ## 10 2  Mixed cloud present in  pixel
    ## 11 3  Cloud state not defined,  assumed clear
    df$qc_bit3 <- substr( df$qc_bitname, start=4, stop=5 )

    ## SCF_QC (five level confidence score)
    ## 000 0 Very best possible
    ## 001 1 Good, very usable, but not the best (saturation in FPAR/LAI has occurred)
    ## 010 2 Substandard due to geometry problems ??? use with caution
    ## 011 3 Substandard due to other than geometry problems ??? use with caution
    ## 100 4  Couldn't retrieve pixel (NOT PRODUCED AT ALL ??? non-terrestrial biome)
    ## 111 7  Fill Value
    df$qc_bit4 <- substr( df$qc_bitname, start=1, stop=3 )

    df <- df %>%  mutate(  good_quality  = ifelse( qc_bit0=="0", TRUE, FALSE ),
                           terra         = ifelse( qc_bit1=="0", TRUE, FALSE ),
                           dead_detector = ifelse( qc_bit2=="1", TRUE, FALSE ),
                           CloudState    = ifelse( qc_bit3=="00", 0, ifelse( qc_bit3=="01", 1, ifelse( qc_bit3=="10", 2, 3 ) ) ),
                           SCF_QC        = ifelse( qc_bit4=="000", 0, ifelse( qc_bit4=="001", 1, ifelse( qc_bit4=="010", 2, ifelse( qc_bit4=="011", 3, ifelse( qc_bit4=="111", 7, NA ) ) ) ) )
                          ) %>%
                  select( -qc_bitname, -Psn_QC, -qc_bit0, -qc_bit1, -qc_bit2, -qc_bit3, -qc_bit4 ) %>% 
                  rename( modisvar = Gpp )

    ## Identify outliers, i.e. whether value is exceedingly high, i.e. if the distance of the value to the median is more than 5 times the distance of the distance of the 75% quantile to the median
    df <- df %>%  mutate( outlier = ifelse( modisvar - median( modisvar, na.rm=TRUE ) > 5 * ( quantile( modisvar, probs=0.75, na.rm=TRUE  ) - median( modisvar, na.rm=TRUE ) ), TRUE, FALSE ) )

    ## Plot effect of filtering steps
    dir <- paste0(dir, "/fig_fapar_gapfilling/")
    if (!dir.exists(dir)) system(paste0("mkdir ", dir))
    plotfiln <- paste0( dir, "gpp_MOD17A2H_fill_", sitename, ".pdf" )
    print( paste( "Gapfilling illustrated in:", plotfiln ) )
    pdf( plotfiln, width=15, height=6 )
    par(xpd=TRUE)
    with( df, plot( year_dec, modisvar, pch=16, col='red', main=sitename, xlab="year", ylab="MOD17A2H", las=1 ) )
    with( dplyr::filter( df, !outlier ), points( year_dec, modisvar, pch=16, col='black' ) )
    # with( dplyr::filter( df, good_quality), points( year_dec, modisvar, pch=16, col='blue' ) )
    # with( dplyr::filter( df, good_quality & CloudState==0), points( year_dec, modisvar, pch=16, col='springgreen3' ) )
    # with( dplyr::filter( df, good_quality & CloudState==0 & !dead_detector ), points( year_dec, modisvar, pch=16, col='orchid' ) )
    # with( dplyr::filter( df, good_quality & CloudState==0 & !dead_detector & SCF_QC==0 ), points( year_dec, modisvar, pch=16, col='black' ) )
    legend( "topleft", 
      c("initial", "!outlier"), 
      col=c( "red", "black" ), pch=16, bty="n", inset = c(0,-0.2)
      )

    ## Filter, i.e. replacing by NA in order to keep all dates
    df <- df %>% mutate( modisvar = ifelse( ( !outlier ), modisvar, NA ) )
    
  }

  if (do_interpolate){
    ##--------------------------------------
    ## Create daily dataframe
    ##--------------------------------------
    ddf <- init_dates_dataframe( year_start, year_end ) %>% dplyr::mutate( doy = lubridate::yday(date) )
    
    ## merge N-day dataframe into daily one. 
    ## Warning: here, 'date' must be centered within 4-day period - thus not equal to start date but (start date + 2)
    ddf <- ddf %>% left_join( dplyr::select( df, date, modisvar ), by="date" )

    ## extrapolate missing values at head and tail
    ddf$modisvar <- extrapolate_missing_headtail(dplyr::select(ddf, var = modisvar, doy))

    ##--------------------------------------
    ## get LOESS spline model for predicting daily values (used below)
    ##--------------------------------------
    idxs    <- which(!is.na(ddf$modisvar))
    myloess <- try( with( ddf, loess( modisvar[idxs] ~ year_dec[idxs], span=0.01 ) ))
    i <- 0
    while (class(myloess)=="try-error" && i<50){
      i <- i + 1
      # print(paste("i=",i))
      myloess <- try( with( ddf, loess( modisvar[idxs] ~ year_dec[idxs], span=(0.01+0.002*(i-1)) ) ))
    }

    ##--------------------------------------
    ## get SPLINE model for predicting daily values (used below)
    ##--------------------------------------
    idxs   <- which(!is.na(ddf$modisvar))
    spline <- try( with( ddf, smooth.spline( year_dec[idxs], modisvar[idxs], spar=0.01 ) ) )

    ## predict LOESS
    ##--------------------------------------
    tmp <- try( with( ddf, predict( myloess, year_dec ) ) )
    if (class(tmp)!="try-error"){
      ddf$data_loess <- tmp
    } else {
      ddf$data_loess <- rep( NA, nrow(ddf) )
    }

    ## predict SPLINE
    ##--------------------------------------
    tmp <- try( with( ddf, predict( spline, year_dec ) )$y)
    if (class(tmp)!="try-error"){
      ddf$spline <- tmp
    } else {
      ddf$spline <- rep( NA, nrow(ddf) )
    }

    ## LINEAR INTERPOLATION
    ##--------------------------------------
    ddf$interpl <- approx( ddf$year_dec, ddf$modisvar, xout=ddf$year_dec )$y 

    ## SAVITZKY GOLAY FILTER
    ##--------------------------------------
    ddf$sgfiltered <- rep( NA, nrow(ddf) )
    idxs <- which(!is.na(ddf$interpl))
    ddf$sgfiltered[idxs] <- signal::sgolayfilt( ddf$interpl[idxs], p=3, n=51 ) 
      
    ##--------------------------------------
    ## DEFINE STANDARD: LINEAR INTERPOLATION OR SPLINE
    ##--------------------------------------
    if (splined_fapar){
      ddf$modisvar_interpol <- ddf$spline
    } else {
      ddf$modisvar_interpol <- ddf$interpl
    }

    ## limit to within 0 and 1 (loess spline sometimes "explodes")
    ddf <- ddf %>% mutate( modisvar_interpol = replace( modisvar_interpol, modisvar_interpol<0, 0  ) ) %>%
                   mutate( modisvar_interpol = replace( modisvar_interpol, modisvar_interpol>1, 1  ) )

    ## plot daily smoothed line and close plotting device
    if (do_plot_interpolated) with( ddf, lines( year_dec, modisvar_interpol, col='red', lwd=2 ) ) 
    if (do_plot_interpolated) with( ddf, lines( year_dec, sgfiltered, col='springgreen3', lwd=1 ) )
    if (do_plot_interpolated) with( ddf, lines( year_dec, spline, col='cyan', lwd=1 ) )
    legend( "topright", 
            c("Savitzky-Golay filter", "Spline", "Linear interpolation (standard)"), 
            col=c("springgreen3", "cyan", "red" ), lty=1, lwd=c(1,1,2), bty="n", inset = c(0,-0.2)
          )
    
    ## extrapolate missing values at head and tail again
    ##--------------------------------------
    ddf$modisvar_interpol <- extrapolate_missing_headtail(dplyr::select(ddf, var = modisvar_interpol, doy))
    

  } else {

    ddf = tibble()

  }

  dev.off()

  return( list( df=df, ddf=ddf ) )

}


##--------------------------------------------------------------------
## Extracts point data for a set of sites given by df_lonlat using
## functions from the raster package.
##--------------------------------------------------------------------
extract_pointdata_allsites <- function( filename, df_lonlat, get_time = FALSE ){

  ## load file using the raster library
  print(paste("Creating raster brick from file", filename))
  if (!file.exists(filename)) rlang::abort(paste0("File not found: ", filename))
  rasta <- raster::brick(filename)

  df_lonlat <- raster::extract(rasta, sp::SpatialPoints(dplyr::select(df_lonlat, lon, lat)), sp = TRUE) %>% 
    as_tibble() %>% 
    tidyr::nest(data = c(-lon, -lat)) %>%
    right_join(df_lonlat, by = c("lon", "lat")) %>%
    mutate( data = purrr::map(data, ~dplyr::slice(., 1)) ) %>% 
    dplyr::mutate(data = purrr::map(data, ~t(.))) %>% 
    dplyr::mutate(data = purrr::map(data, ~as_tibble(.)))
  
  if (get_time){
    timevals <- raster::getZ(rasta)
    df_lonlat <- df_lonlat %>% 
      mutate( data = purrr::map(data, ~bind_cols(., tibble(date = timevals))))
  }

  return(df_lonlat)
}


# ##--------------------------------------------------------------------
# ## Get monthly data from CRU
# ##--------------------------------------------------------------------
# get_clim_cru_monthly_bysite <- function( lon, lat, settings, cruvars ){

#   ## get last year for which data is available
#   filn <- list.files( settings$path_cru, pattern="cld.dat.nc")
#   start <- regexpr( 20, filn)[1]
#   stop <- start + 3
#   yrend <- substr( filn, start, stop ) %>% as.numeric %>% ifelse( length(.)==0, 2010, . )

#   ## cloud cover
#   mdf <- get_pointdata_monthly_cru( "cld", lon, lat, settings, yrend=yrend )

#   ## Check if data is available at that location, otherwise use nearest gridcell
#   if (!is.data.frame(mdf)){
#     lon_look <- find_nearest_cruland_by_lat( lon, lat, paste0( settings$path_cru, filn ) )
#     mdf <- get_pointdata_monthly_cru( "cld", lon_look, lat, settings, yrend=yrend )
#   } else {
#     lon_look <- lon
#   }
#   mdf <- mdf %>% dplyr::rename( ccov_cru = mdata )    

#   ## precipitation
#   if ("prec" %in% cruvars){
#     mdf <- get_pointdata_monthly_cru( "pre", lon_look, lat, settings, yrend=yrend ) %>% dplyr::rename( prec_cru = mdata ) %>% 
#       right_join( mdf, by = c("date", "year_dec") )
#   }

#   ## wet days
#   if ("wetd" %in% cruvars){
#     mdf <- get_pointdata_monthly_cru( "wet", lon_look, lat, settings, yrend=yrend ) %>% dplyr::rename( wetd_cru = mdata ) %>% 
#       right_join( mdf, by = c("date", "year_dec") )
#   }

#   ## air temperature
#   if ("temp" %in% cruvars){
#     mdf <- get_pointdata_monthly_cru( "tmp", lon_look, lat, settings, yrend=yrend ) %>% dplyr::rename( temp_cru = mdata ) %>% 
#       right_join( mdf, by = c("date", "year_dec") )
#   }

#   ## VPD 
#   ## calculated as a function of vapour pressure and temperature, vapour
#   ## pressure is given by CRU data.
#   if ("vap" %in% cruvars){
#     mdf <-  get_pointdata_monthly_cru( "vap", lon_look, lat, settings, yrend=yrend ) %>%  
#                 dplyr::rename( vap_cru = mdata ) %>%
#                 ## merge temperature data in here for VPD calculation
#                 left_join( mdf_temp, by =  c("date", "year_dec") ) %>%
#                 ## calculate VPD (vap is in hPa)
#                 mutate( vpd_vap_cru_temp_cru = calc_vpd( eact=1e2*vap_cru, tc=temp_cru ) ) %>% 
#                 ## avoid duplicate 
#                 dplyr::select( -temp_cru ) %>% 
#                 right_join( mdf, by = c("date", "year_dec") )
#   }


#   return( mdf )

# }


##--------------------------------------------------------------------
## Interpolates monthly data to daily data using polynomials or linear
## for a single year
##--------------------------------------------------------------------
expand_clim_cru_monthly <- function( mdf, cruvars ){

  ddf <- purrr::map( as.list( unique( year( mdf$date ) ) ), ~expand_clim_cru_monthly_byyr( ., mdf, cruvars ) ) %>% 
    bind_rows()

  return( ddf )

}


##--------------------------------------------------------------------
## Interpolates monthly data to daily data using polynomials or linear
## for a single year
##--------------------------------------------------------------------
expand_clim_cru_monthly_byyr <- function( yr, mdf, cruvars ){

  nmonth <- 12

  startyr_cru <- year(mdf$date) %>% unique() %>% first()
  endyr_cru   <- year(mdf$date) %>% unique() %>% last()
  
  yr_pvy <- max(startyr_cru, yr-1)
  yr_nxt <- min(endyr_cru, yr+1)

  ## add first and last year to head and tail of 'mdf'
  first <- mdf[1:12,] %>% mutate( date = date - years(1) )
  last  <- mdf[(nrow(mdf)-11):nrow(mdf),] %>% mutate( date = date + years(1) )

  ddf <- init_dates_dataframe( yr, yr )

  ##--------------------------------------------------------------------
  ## air temperature: interpolate using polynomial
  ##--------------------------------------------------------------------
  if ("temp" %in% cruvars){
    mtemp     <- dplyr::filter( mdf, year(date)==yr     )$temp_cru
    mtemp_pvy <- dplyr::filter( mdf, year(date)==yr_pvy )$temp_cru
    mtemp_nxt <- dplyr::filter( mdf, year(date)==yr_nxt )$temp_cru
    if (length(mtemp_pvy)==0){
      mtemp_pvy <- mtemp
    }
    if (length(mtemp_nxt)==0){
      mtemp_nxt <- mtemp
    }

    ddf <- init_dates_dataframe( yr, yr ) %>%
           mutate( temp_cru_int = monthly2daily( mtemp, "polynom", mtemp_pvy[nmonth], mtemp_nxt[1], leapyear = leap_year(yr) ) ) %>% 
           right_join( ddf, by = c("date", "year_dec") )
  }

  ##--------------------------------------------------------------------
  ## precipitation: interpolate using weather generator
  ##--------------------------------------------------------------------
  if ("prec" %in% cruvars){
    mprec <- dplyr::filter( mdf, year(date)==yr )$prec_cru
    mwetd <- dplyr::filter( mdf, year(date)==yr )$wetd_cru

    if (any(!is.na(mprec))&&any(!is.na(mwetd))){
      ddf <-  init_dates_dataframe( yr, yr ) %>% 
              mutate( prec_cru_gen = get_daily_prec( mprec, mwetd, leapyear = leap_year(yr) ) ) %>% 
              right_join( ddf, by = c("date", "year_dec") )
    }
  }

  ##--------------------------------------------------------------------
  ## cloud cover: interpolate using polynomial
  ##--------------------------------------------------------------------
  if ("ccov" %in% cruvars){
    mccov     <- dplyr::filter( mdf, year(date)==yr     )$ccov_cru
    mccov_pvy <- dplyr::filter( mdf, year(date)==yr_pvy )$ccov_cru
    mccov_nxt <- dplyr::filter( mdf, year(date)==yr_nxt )$ccov_cru
    if (length(mccov_pvy)==0){
      mccov_pvy <- mccov
    }
    if (length(mccov_nxt)==0){
      mccov_nxt <- mccov
    }

    ddf <-  init_dates_dataframe( yr, yr ) %>%
            mutate( ccov_cru_int = monthly2daily( mccov, "polynom", mccov_pvy[nmonth], mccov_nxt[1], leapyear = leap_year(yr) ) ) %>%
            ## Reduce CCOV to a maximum 100%
            mutate( ccov_cru_int = ifelse( ccov_cru_int > 100, 100, ccov_cru_int ) ) %>%
            right_join( ddf, by = c("date", "year_dec") )
  }

  ##--------------------------------------------------------------------
  ## VPD: interpolate using polynomial
  ##--------------------------------------------------------------------
  if ("vap" %in% cruvars){
    mvpd     <- dplyr::filter( mdf, year(date)==yr     )$vpd_vap_cru_temp_cru
    mvpd_pvy <- dplyr::filter( mdf, year(date)==yr_pvy )$vpd_vap_cru_temp_cru
    mvpd_nxt <- dplyr::filter( mdf, year(date)==yr_nxt )$vpd_vap_cru_temp_cru
    if (length(mvpd_pvy)==0){
      mvpd_pvy <- mvpd
    }
    if (length(mvpd_nxt)==0){
      mvpd_nxt <- mvpd
    }

    ddf <- init_dates_dataframe( yr, yr ) %>%
               mutate( vpd_vap_cru_temp_cru_int = monthly2daily( mvpd, "polynom", mvpd_pvy[nmonth], mvpd_nxt[1], leapyear = (yr %% 4 == 0) ) ) %>% 
               right_join( ddf, by = c("date", "year_dec") )
  }

  return( ddf )

}

##--------------------------------------------------------------------
## Finds the closest land cell in the CRU dataset at the same latitude
##--------------------------------------------------------------------
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
    if (ilon_look > length(lon_vec)) {ilon_look <- ilon_look %% length(lon_vec)} ## Wrap search around globe in latitudinal direction
    if (ilon_look < 1)               {ilon_look <- ilon_look + length(lon_vec) }
    print(paste("ilon_look",ilon_look))
    if (!is.na(crufield[ilon_look,ilat])) {
      break
    }
  }
  # if (!is.na(crufield[ilon_look,ilat])) {print("SUCCESSFULLY FOUND DATA")}
  return( lon_vec[ ilon_look ] )
  
}


##--------------------------------------------------------------------------
## Checks if WATCH-WFDEI files are available for this variable and initiates download if not.
##--------------------------------------------------------------------------
check_download_watch_wfdei <- function( varnam, settings_input, settings_sims ){

  ## Determine file name, given <settings_input$path_fluxnet2015>
  ## look for data in the given directory
  filelist <- list.files( settings_input$path_watch_wfdei, pattern = paste0( varnam, "_daily_WFDEI_.*.nc"), recursive = TRUE )

  if (length(filelist)==0){

    ## No files found at specified location
    rlang::warn( paste0("No files found for WATCH-WFDEI in directory ", settings_input$path_watch_wfdei) )

    ## Search at a different location?
    path <- readline( prompt="Would you like to search for files recursively from a different directory? Enter the path from which search is to be done: ")
    filelist <- list.files( path, pattern = paste0( varnam, "_daily_WFDEI_.*.nc"), recursive = TRUE )

    if (length(filelist)==0){
     
      ## Search from home
      rlang::warn( paste0("Still nothing found at specified location ", path ) )
      ans <- readline( prompt="Would you like to search for files recursively from your home directory (y/n): ")
      
      if (ans=="y"){
      
        filelist <- list.files( "~/", pattern = paste0( varnam, "_daily_WFDEI_.*.nc"), recursive = TRUE )
      
      } else {
        
        ## Still no files found at specified location. Try to download from remote server and place in <settings_input$path_watch_wfdei>
        if (!is.null(settings_input$get_from_remote)){
          if (settings_input$get_from_remote){
            rlang::warn( "Initiating download from remote server..." )
            getfiles <- getfilenames_watch_wfdei( settings_input$date_start, settings_input$date_end )
            error <- download_from_remote(
              settings_input$path_remote_watch_wfdei,  
              settings_input$path_watch_wfdei, 
              getfiles = getfiles, 
              uname = settings_input$uname_remote, 
              address_remote = settings_input$address_remote 
            )        
          }
          filelist <- list.files( settings_input$path_watch_wfdei, pattern = paste0( varnam, "_daily_WFDEI_.*.nc"), recursive = TRUE )
        }
        
      }

      if (length(filelist)==0){
        ## Still no files found at specified location. Try to download from remote server and place in <settings_input$path_watch_wfdei>
        abort("check_download_watch_wfdei(): No files downloaded.")
      }

    }

  } 

  ## Check if files are now available at specified location.
  filelist <- list.files( settings_input$path_watch_wfdei, pattern = paste0( varnam, "_daily_WFDEI_.*.nc"), recursive = TRUE )
  if (length(filelist)==0) abort("check_download_watch_wfdei(): No files downloaded.")
  
}

## Returns the WATCH-WFDEI file names that are still missing locally 
getfilenames_watch_wfdei <- function( date_start, date_end ){

  ## determine simulation years ocurring in this ensemble
  allyears <- seq( from = purrr::map_dbl( settings_sims$date_start, ~year(.) ) %>% min(),
                   to   = purrr::map_dbl( settings_sims$date_end, ~year(.) ) %>% max(),
                   by   = 1 ) %>% as.list

  ## Determine missing files for this variable, given start and end years of all the simulations in this ensemble
  getfiles <- purrr::map( allyears, ~check_watch_wfdei_year( ., varnam, settings_input ) ) %>% unlist()

  return(getfiles)  

}


##--------------------------------------------------------------------------
## Returns the file names of missing files for this year
##--------------------------------------------------------------------------
check_watch_wfdei_year <- function( year, varnam, settings_input ){

  ## construct file names of all months' files (12 for each year)
  allfiles <- purrr::map_chr( as.list(sprintf("%02d", 1:12 )), ~paste0( varnam, "_daily/", varnam, "_daily_WFDEI_", as.character(year), ., ".nc" ) )
  avlfiles <- list.files( settings_input$path_watch_wfdei, pattern = paste0( varnam, "_daily_WFDEI_", as.character(year), ".*.nc"), recursive = TRUE )

  getfiles <- allfiles[!(allfiles %in% avlfiles)]

  return(getfiles)

}

##--------------------------------------------------------------------------
## Checks if CRU TS files are available for this variable and initiates download if not.
##--------------------------------------------------------------------------
check_download_cru <- function( varnam, settings_input, settings_sims ){

  ## Determine file name, given <settings_input$path_fluxnet2015>
  ## look for data in the given directory
  getfiles <- list.files( settings_input$path_cru, pattern = paste0( varnam, ".dat.nc" ) )

  if (length(getfiles)==0){

    ## No files found at specified location
    rlang::warn( paste0("No files found for CRU TS in directory ", settings_input$path_cru) )

    ## Search at a different location?
    path <- readline( prompt="Would you like to search for files recursively from a different directory? Enter the path from which search is to be done: ")
    getfiles <- list.files( path, pattern = paste0( "cru_ts*.1901.2016.", varnam, ".dat.nc") )

    if (length(getfiles)==0){
     
      ## Search from home
      rlang::warn( paste0("Still nothing found at specified location ", path ) )
      ans <- readline( prompt="Would you like to search for files recursively from your home directory (y/n): ")
      
      if (ans=="y"){
      
        getfiles <- list.files( "~/", pattern = paste0( "cru_ts*.1901.2016.", varnam, ".dat.nc") )
      
      } else {
        
        ## Still no files found at specified location. Try to download from remote server and place in <settings_input$path_cru>
        if (!is.null(settings_input$get_from_remote)){
          if (settings_input$get_from_remote){
            rlang::warn( "Initiating download from remote server ..." )
            error <- download_from_remote( 
              settings_input$path_remote_cru, 
              settings_input$path_cru, 
              pattern = varnam, 
              uname = settings_input$uname_remote, 
              address_remote = settings_input$address_remote 
            )
          }
          getfiles <- list.files( settings_input$path_cru, pattern = paste0( "cru_ts*.1901.2016.", varnam, ".dat.nc") )
        }
      }

      if (length(getfiles)==0){
        ## Still no files found at specified location. Try to download from remote server and place in <settings_input$path_cru>
        abort("check_download_cru(): No files downloaded from remote.")
      }

    }

  } 

  ## Check if files are now available at specified location.
  getfiles <- list.files( settings_input$path_cru, pattern = paste0( varnam, ".dat.nc" ) )
  if (length(getfiles)==0) abort("Download of CRU data was not successful. No files found.")
  
}

##--------------------------------------------------------------------------
## Checks if MODIS_FPAR_MCD15A3H files are available for this variable and initiates download if not.
##--------------------------------------------------------------------------
check_download_MODIS_FPAR_MCD15A3H <- function( settings_input, settings_sims, sitename=NA ){

  error <- 0

  ## Determine file name, given <settings_input$path_MODIS_FPAR_MCD15A3H>
  ## look for data for this site in the given directory
  filelist <- list.files( settings_input$path_MODIS_FPAR_MCD15A3H, pattern = "dfapar_MODIS_FPAR_MCD15A3H_gee_MCD15A3H_.*_gee_subset.csv" )

  if (length(filelist)==0){

    ## No files found at specified location
    rlang::warn( paste0("No files found for MODIS_FPAR_MCD15A3H in directory ", settings_input$path_MODIS_FPAR_MCD15A3H) )

    ## Search at a different location?
    path <- readline( prompt="Would you like to search for files recursively from a certain directory? Enter the path from which search is to be done: ")
    filelist <- list.files( path, pattern = "dfapar_MODIS_FPAR_MCD15A3H_gee_MCD15A3H_.*_gee_subset.csv", recursive = TRUE )

    if (length(filelist)==0){
     
      ## Search from home
      rlang::warn( paste0("Still nothing found at specified location ", path ) )
      ans <- readline( prompt="Would you like to search for files recursively from your home directory (y/n): ")
      if (ans=="y"){
        filelist <- list.files( "~/", pattern = "dfapar_MODIS_FPAR_MCD15A3H_gee_MCD15A3H_.*_gee_subset.csv", recursive = TRUE )
      
      } else {
      
        ## Still no files found at specified location. Try to download from remote server and place in <settings_input$path_MODIS_FPAR_MCD15A3H>
        if (settings_input$get_from_remote){
          rlang::warn( "Initiating download from remote server..." )
          error <- download_from_remote( 
            settings_input$path_remote_MODIS_FPAR_MCD15A3H, 
            settings_input$path_MODIS_FPAR_MCD15A3H, 
            pattern = NA,
            uname = settings_input$uname_remote, 
            address_remote = settings_input$address_remote 
            )
        }
        filelist <- list.files( settings_input$path_MODIS_FPAR_MCD15A3H, pattern = "dfapar_MODIS_FPAR_MCD15A3H_gee_MCD15A3H_.*_gee_subset.csv" )
      }

      if (length(filelist)==0){
        if (settings_input$get_from_remote){
          ## Still no files found at specified location. Try to download from remote server and place in <settings_input$path_MODIS_FPAR_MCD15A3H>
          rlang::warn( "Initiating download from remote server..." )
          error <- download_from_remote( 
            settings_input$path_remote_MODIS_FPAR_MCD15A3H, 
            settings_input$path_MODIS_FPAR_MCD15A3H, 
            pattern = NA,
            uname = settings_input$uname_remote, 
            address_remote = settings_input$address_remote 
            )
        }
      }

    }

  }

  if (!is.na(sitename)){
    ## Check if a file is available for a given site
    filelist <- list.files( settings_input$path_MODIS_FPAR_MCD15A3H, pattern = paste0("dfapar_MODIS_FPAR_MCD15A3H_gee_MCD15A3H_", sitename, "_gee_subset.csv") )

    if (length(filelist)==0){
      if (settings_input$get_from_remote){
        ## Download missing file
        error <- download_from_remote( 
          settings_input$path_remote_MODIS_FPAR_MCD15A3H, 
          settings_input$path_MODIS_FPAR_MCD15A3H, 
          pattern = sitename,
          uname = settings_input$uname_remote, 
          address_remote = settings_input$address_remote 
          )
      }
    }

  }
  return( error )
}


##--------------------------------------------------------------------------
## Checks if MODIS_EVI_MOD13Q1 files are available for this variable and initiates download if not.
##--------------------------------------------------------------------------
check_download_MODIS_EVI_MOD13Q1 <- function( settings_input, settings_sims, sitename=NA ){

  error <- 0

  ## Determine file name, given <settings_input$path_MODIS_EVI_MOD13Q1>
  ## look for data for this site in the given directory
  filelist <- list.files( settings_input$path_MODIS_EVI_MOD13Q1, pattern = "fapar_MODIS_EVI_MOD13Q1_gee_MOD13Q1_.*_gee_subset.csv" )

  if (length(filelist)==0){

    ## No files found at specified location
    rlang::warn( paste0("No files found for MODIS_EVI_MOD13Q1 in directory ", settings_input$path_MODIS_EVI_MOD13Q1) )

    ## Search at a different location?
    path <- readline( prompt="Would you like to search for files recursively from a certain directory? Enter the path from which search is to be done: ")
    filelist <- list.files( path, pattern = "fapar_MODIS_EVI_MOD13Q1_gee_MOD13Q1_.*_gee_subset.csv", recursive = TRUE )

    if (length(filelist)==0){
     
      ## Search from home
      rlang::warn( paste0("Still nothing found at specified location ", path ) )
      ans <- readline( prompt="Would you like to search for files recursively from your home directory (y/n): ")
      if (ans=="y"){
        filelist <- list.files( "~/", pattern = "fapar_MODIS_EVI_MOD13Q1_gee_MOD13Q1_.*_gee_subset.csv", recursive = TRUE )
      } else {
        if (settings_input$get_from_remote){
          ## Still no files found at specified location. Try to download from remote server and place in <settings_input$path_MODIS_EVI_MOD13Q1>
          rlang::warn( "Initiating download from remote server..." )
          error <- download_from_remote( 
            settings_input$path_remote_MODIS_EVI_MOD13Q1, 
            settings_input$path_MODIS_EVI_MOD13Q1, 
            sitename = NA,
            uname = settings_input$uname_remote, 
            address_remote = settings_input$address_remote 
            )
        }
        filelist <- list.files( settings_input$path_MODIS_EVI_MOD13Q1, pattern = "fapar_MODIS_EVI_MOD13Q1_gee_MOD13Q1_.*_gee_subset.csv" )
      }

      if (length(filelist)==0){
        if (settings_input$get_from_remote){
          ## Still no files found at specified location. Try to download from remote server and place in <settings_input$path_MODIS_EVI_MOD13Q1>
          rlang::warn( "Initiating download from remote server..." )
          error <- download_from_remote( 
            settings_input$path_remote_MODIS_EVI_MOD13Q1, 
            settings_input$path_MODIS_EVI_MOD13Q1, 
            sitename = NA,
            uname = settings_input$uname_remote, 
            address_remote = settings_input$address_remote 
            )
        }
      }

    }

  }

  if (!is.na(sitename)){
    ## Check if a file is available for a given site
    filelist <- list.files( settings_input$path_MODIS_EVI_MOD13Q1, pattern = paste0("fapar_MODIS_EVI_MOD13Q1_gee_MOD13Q1_", sitename, "_gee_subset.csv") )

    if (length(filelist)==0 && settings_input$get_from_remote){
      ## Download missing file
      error <- download_from_remote( 
        settings_input$path_remote_MODIS_EVI_MOD13Q1, 
        settings_input$path_MODIS_EVI_MOD13Q1, 
        pattern = sitename,
        uname = settings_input$uname_remote, 
        address_remote = settings_input$address_remote 
        )
    }

  }
  return( error )
}

##--------------------------------------------------------------------------
## Checks if CMIP CO2 files are available and initiates download if not.
##--------------------------------------------------------------------------
check_download_co2 <- function( settings_input, settings_sims, sitename = NA ){

  error <- 0

  if (!file.exists(settings_input$path_co2) && settings_input$get_from_remote){

    error <- download_from_remote( settings_input$path_remote_co2, settings_input$path_co2, uname = settings_input$uname_remote, address_remote = settings_input$address_remote )

    # origpath <- dirname(settings_input$path_co2)

    # ## No files found at specified location
    # system( paste0( "rsync -avz ", uname, "@", settings$address_remote, ":", settings_input$path_remote_co2, " ", localdir ) )

  }

  if (!is.na(sitename)){
    ## Check if a file is available for a given site
    localdir_bysite <- paste0( settings_input$path_input, "/sitedata/co2/", sitename )
    if (!dir.exists(localdir_bysite)) system( paste0("mkdir -p ", localdir_bysite ) )
    filelist <- list.files( localdir_bysite, pattern = basename(settings_input$path_co2) )

    if (length(filelist)==0){
      ## link file into site-level directory
      system( paste0( "ln -svf ", settings_input$path_co2, " ", localdir_bysite, "/" ) )
    }

  }
  return( error )
}




# ##-----------------------------------------------------------
# ## Manages the path specification for CRU TS 4.01 data download from CX1
# ##-----------------------------------------------------------
# download_cru_from_remote <- function( varnam, settings_input ){

#   # ## the path of CRU TS 4.01 data on cx1
#   # origpath <- "/work/bstocker/labprentice/data/cru/ts_4.01/" 
#   # filn <-  paste0( "cru_ts4.01.1901.2016.", varnam, ".dat.nc")
  
#   ## Interactive part
#   ans <- readline( prompt = "Do you have access to Imperial's CX1? (y/n) " )
#   if (ans=="y"){
#     ans <- readline( prompt = "Have you connected to Imperial's VPN? (y/n) " )
#     if (ans=="y"){
#       ans <- readline( prompt = paste0("Are you still happy with downloading to ", settings_input$path_cru, "? (y/n)") )
#       if (ans=="y"){
#         error <- download_cru_from_remote_filn( varnam, settings_input, filn = filn )
#       } else {
#         path <- readline( prompt = "Please specify a new path: " )
#         settings_input$path_cru <- path
#         error <- download_cru_from_remote_filn( varnam, settings_input, filn = filn )
#       }
#     } else {
#       abort( "CRU TS 4.01 data download not possible.")
#     }
#   } else {
#     abort( "CRU TS 4.01 data download not possible.")
#   }

#   return(error)

# }


# ##-----------------------------------------------------------
# ## Downloads CRU TS 4.01 data from CX1
# ##-----------------------------------------------------------
# download_cru_from_remote_filn <- function( varnam, settings_input, filn ){

#   origpath <- "/work/bstocker/labprentice/data/cru/ts_4.01/"

#   filn <-  paste0( "cru_ts4.01.1901.2016.", varnam, ".dat.nc")

#   if (!dir.exists(settings_input$path_cru)) system(paste0("mkdir -p ", settings_input$path_cru))
#   if (!exists("uname")) uname <<- readline( prompt = "Enter your user name for logging onto remote server: " )

#   system( paste0( "rsync -avz ", uname, "@", settings$address_remote, ":", origpath, filn, " ", settings_input$path_cru ) )

#   return(NULL)
# }


monthly2daily <- function( mval, method="polynom", mval_prev=mval[nmonth], mval_next=mval[1], leapyear=FALSE ){

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

      # Take a sheet of paper and try solve the polynom, well here is the outcome
      polya <- (mval[month]*dt - deltatemp*d2t/dt/2.0 - starttemp*dt + deltatemp*startt) / (d3t/3.0 - d2t^2.0/dt/2.0 - dt*startt^2.0 + startt*d2t)
      polyb <- deltatemp/dt - polya*(startt+endt)
      polyc <- starttemp - polya*startt^2.0 - polyb*startt

      # calculate daily values with the polynom function
      for (d in 1:ndaymonth[month]) {
        day <- day + 1
        dval[day] <- polya*(day)^2.0 + polyb*(day) + polyc
      }
      lastmonthtemp <- mval[month]
    }

    # calculate monthly means after interpolation - not absolutely identical to input
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


get_daily_prec <- function( mval_prec, mval_wet, set_seed=FALSE, leapyear=FALSE ){
  #--------------------------------------------------------------------
  # Distributes monthly total precipitation to days, given number of 
  # monthly wet days. Adopted from LPX.
  #--------------------------------------------------------------------
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
    prdaily_random[doy,] <- runif(2)
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
          # xxx problem: rand() generates a random number that leads to floating point exception
          vv <- runif(1)
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

##-----------------------------------------------------------
## fills gaps (NAs) by (1.) linear interpolation, (2.) extending first/last to head/tail
##-----------------------------------------------------------
fill_gaps <- function( vec, is.prec = FALSE ){
  
  xvals <- seq(length(vec))

  if ( is.prec ){
    ## assume precipitation = 0 where missing
    if (any(is.na(vec))){
      vec[ is.na(vec) ] <- 0.0
    }

  } else {
    ## linear approximation
    if ( any(is.na(vec)) && any(!is.na(vec)) ){
        vec <- approx( xvals, vec, xout=xvals )$y
    }

    ## extend to missing in head and tail
    if ( any(is.na(vec))  && any(!is.na(vec)) ){
      for (idx in seq(length(vec))){
        if ( any( is.na( tail( vec, n=idx ) ) ) && any( !is.na( tail( vec, n=(idx+1) ) ) ) ){
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


## write output with standard formatting
write_sofunformatted <- function( filnam, data ){

  if ( is.vector( data ) ){
    len <- length(data)
    formatted <- vector( "character", len )
    for (i in 1:len){
      formatted[i] <- sprintf("%16.6f", data[i] )
    }
    writeLines( formatted, filnam )
  } else if ( is.data.frame( data ) && length( dim( data ) )==2 ){
    len <- dim( data )[1]
    formatted <- vector( "character", len )
    for (i in 1:len){
      formatted[i] <- sprintf("%16.6f    %f", data[i,1], data[i,2] )
    }
    writeLines( formatted, filnam )    
  }

}


# ##-----------------------------------------------------------
# ## Downloads MODIS FPAR data from CX1
# ##-----------------------------------------------------------
# download_MODIS_FPAR_MCD15A3H_from_remote_path <- function( path, sitename=NA ){

#   error <- 0

#   ## get user name from user
#   if (!exists("uname")) uname <<- readline( prompt = "Enter your user name for logging onto remote server: " )

#   ## the path of fluxnet daily data on cx1
#   origpath <- "/work/bstocker/labprentice/data/fapar_MODIS_FPAR_MCD15A3H_fluxnet2015_gee_subset/"

#   if (is.na(sitename)){
#     ## Download the whole bunch

#     ## create required directory
#     if (!dir.exists(path)) system( paste0("mkdir -p ", path ) )

#     system( paste0( "rsync -avz ", uname, "@", settings$address_remote, ":", origpath, " ", path ) )

#   } else {
#     ## Download only data for a specific site
#     ## get a file list of what's on CX1
#     filelist <- system( paste0( "ssh ", uname, "@", settings$address_remote, " ls ", origpath ), intern = TRUE )

#     ## use one file(s) for this site
#     filelist <- filelist[ grepl(sitename, filelist) ]
#     filelist <- filelist[ grepl("fapar_MODIS_FPAR_MCD15A3H_", filelist) ]

#     if (length(filelist)==0){
#       ## no data available for this site
#       error <- 1
#       rlang::warn(paste0("No MODIS_FPAR_MCD15A3H data available for site ", sitename ) )
#     } else {
#       purrr::map( as.list(filelist), ~system( paste0( "rsync -avz ", uname, "@", settings$address_remote, ":", origpath, .," ", path ) ) )    
#     }

#   }
#   return( error )
# }


# ##-----------------------------------------------------------
# ## Downloads MODIS EVI data from CX1
# ##-----------------------------------------------------------
# download_MODIS_EVI_MOD13Q1_from_remote_path <- function( path, sitename=NA ){

#   error <- 0

#   ## get user name from user
#   if (!exists("uname") && is.null(settings_input$uname)) uname <<- readline( prompt = "Enter your user name for logging onto remote server: " )

#   ## the path of fluxnet daily data on cx1
#   origpath <- settings$path_remote_MODIS_EVI_MOD13Q1

#   if (is.na(sitename)){
#     ## Download the whole bunch

#     ## create required directory
#     if (!dir.exists(path)) system( paste0("mkdir -p ", path ) )

#     system( paste0( "rsync -avz ", uname, "@", settings$address_remote, ":", settings$path_remote_MODIS_EVI_MOD13Q1 ) )

#   } else {
#     ## Download only data for a specific site
#     ## get a file list of what's on CX1
#     filelist <- system( paste0( "ssh ", uname, "@", settings$address_remote, " ls ", dirname(settings$path_remote_MODIS_EVI_MOD13Q1) ), intern = TRUE )

#     ## use one file(s) for this site
#     filelist <- filelist[ grepl(sitename, filelist) ]
#     filelist <- filelist[ grepl("evi_MOD13Q1_gee_MOD13Q1_", filelist) ]

#     if (length(filelist)==0){
#       ## no data available for this site
#       error <- 1
#       rlang::warn(paste0("No MODIS_EVI_MOD13Q1 data available for site ", sitename ) )
#     } else {
#       purrr::map( as.list(filelist), ~system( paste0( "rsync -avz ", uname, "@", settings$address_remote, ":", settings$path_remote_MODIS_EVI_MOD13Q1, .," ", settings$path_MODIS_EVI_MOD13Q1 ) ) )    
#     }

#   }
#   return( error )
# }


# ##-----------------------------------------------------------
# ## Manages the path specification for MODIS FPAR data download from CX1
# ##-----------------------------------------------------------
# download_MODIS_FPAR_MCD15A3H_from_remote <- function( settings_input ){
  
#   ans <- readline( prompt = "Do you have access to Imperial's CX1? (y/n) " )
#   if (ans=="y"){
#     ans <- readline( prompt = "Have you connected to Imperial's VPN? (y/n) " )
#     if (ans=="y"){
#       ans <- readline( prompt = paste0("Are you still happy with downloading to ", settings_input$path_MODIS_FPAR_MCD15A3H, "? (y/n)") )
#       if (ans=="y"){
#         # error <- download_MODIS_FPAR_MCD15A3H_from_remote_path( settings_input$path_MODIS_FPAR_MCD15A3H )
#         error <- download_from_remote_path(  settings_input$path_MODIS_FPAR_MCD15A3H )
#         download_from_remote_path( dir_remote, dir_local, sitename = NA, uname = NULL, address_remote = NULL )
#       } else {
#         path <- readline( prompt = "Please specify a new path: " )
#         # settings_input$path_MODIS_FPAR_MCD15A3H <- path
#         # error <- download_MODIS_FPAR_MCD15A3H_from_remote_path( settings_input$path_MODIS_FPAR_MCD15A3H )
#       }
#     } else {
#       abort( "MODIS_FPAR_MCD15A3H data download not possible.")
#     }
#   } else {
#     abort( "MODIS_FPAR_MCD15A3H data download not possible.")
#   }

#   return(error)

# }


# ##-----------------------------------------------------------
# ## Manages the path specification for WATCH-WFDEI data download from CX1
# ##-----------------------------------------------------------
# download_watch_wfdei_from_remote <- function( varnam, settings_input, settings_sims ){

#   # ## determine simulation years ocurring in this ensemble
#   # allyears <- seq( from = purrr::map_dbl( settings_sims$date_start, ~year(.) ) %>% min(),
#   #                  to   = purrr::map_dbl( settings_sims$date_start, ~year(.) ) %>% max(),
#   #                  by   = 1 ) %>% as.list

#   # ## Determine missing files for this variable, given start and end years of all the simulations in this ensemble
#   # getfiles <- purrr::map( allyears, ~check_watch_wfdei_year( ., varnam, settings_input ) ) %>% unlist()

#   ans <- readline( prompt = paste0("Are you still happy with downloading to ", settings_input$path_watch_wfdei, "? (y/n)") )
#   if (ans=="y"){
#     error <- download_watch_wfdei_from_remote_path( settings_input$path_watch_wfdei, getfiles )
#   } else {
#     path <- readline( prompt = "Please specify a new path: " )
#     settings_input$path_watch_wfdei <- path
#     error <- download_watch_wfdei_from_remote_path( settings_input$path_watch_wfdei, getfiles )
#   }

#   return(error)

# }

# ##-----------------------------------------------------------
# ## Downloads WATCH-WFDEI data from CX1 xxxxx
# ##-----------------------------------------------------------
# download_watch_wfdei_from_remote_path <- function( path, getfiles ){

#   ## the path of WATCH-WFDEI daily data on cx1
#   origpath <- "/work/bstocker/labprentice/data/watch_wfdei/"

#   ## create required directory
#   if (!dir.exists(path)) system( paste0("mkdir -p ", path ) )

#   ## files are in sub-directories, determine them
#   subdir <- getfiles %>% dirname() %>% unique()
#   if (!dir.exists(paste0( path, "/", subdir ))) system( paste0("mkdir -p ", paste0( path, "/", subdir ) ) )

#   if (!exists("uname")) uname <<- readline( prompt = "Enter your user name for logging onto remote server: " )
#   error <- purrr::map( as.list(getfiles[1]), ~system( paste0( "rsync -avz ", uname, "@", settings$address_remote, ":", origpath, ., " ", paste0( path, subdir ) ) ) )

#   ## Show files in directory
#   print( paste0("Files in directory: ", path) )
#   print( list.files( path ) )

# }


# ##-----------------------------------------------------------
# ## Manages the path specification for MODIS EVI data download from CX1
# ##-----------------------------------------------------------
# download_MODIS_EVI_MOD13Q1_from_remote <- function( settings_input ){
  
#   ans <- readline( prompt = "Do you have access to Imperial's CX1? (y/n) " )
#   if (ans=="y"){
#     ans <- readline( prompt = "Have you connected to Imperial's VPN? (y/n) " )
#     if (ans=="y"){
#       ans <- readline( prompt = paste0("Are you still happy with downloading to ", settings_input$path_MODIS_EVI_MOD13Q1, "? (y/n)") )
#       if (ans=="y"){
#         error <- download_MODIS_EVI_MOD13Q1_from_remote_path( settings_input$path_MODIS_EVI_MOD13Q1 )
#       } else {
#         path <- readline( prompt = "Please specify a new path: " )
#         settings_input$path_MODIS_EVI_MOD13Q1 <- path
#         error <- download_MODIS_EVI_MOD13Q1_from_remote_path( settings_input$path_MODIS_EVI_MOD13Q1 )
#       }
#     } else {
#       abort( "MODIS_EVI_MOD13Q1 data download not possible.")
#     }
#   } else {
#     abort( "MODIS_EVI_MOD13Q1 data download not possible.")
#   }

#   return(error)

# }

##----------------------------------------------------------------   
## Calculates atm. pressure for a given elevation
## Ref:      Allen et al. (1998). Adopted from SPLASH.
## arguments: 
## elv : elevation above sea level, m
## patm : atmospheric pressure for a given elevation, Pa
##----------------------------------------------------------------   
calc_patm <- function( elv ){
  ## Parameters from SPLASH
  kPo = 101325  
  kL  = 0.0065  
  kTo = 288.15  
  kG  = 9.80665 
  kMa = 0.028963
  kR  = 8.3143  

  patm <- kPo*(1.0 - kL*elv/kTo)**(kG*kMa/(kR*kL))

  return( patm )

}

##-----------------------------------------------------------------------
## Output:   vapor pressure deficit, Pa (vpd)
## Features: Returns vapor pressure deficit
## Ref:      Eq. 5.1, Abtew and Meleese (2013), Ch. 5 Vapor Pressure 
##           Calculation Methods, in Evaporation and Evapotranspiration: 
##           Measurements and Estimations, Springer, London.
##             vpd = 0.611*exp[ (17.27 tc)/(tc + 237.3) ] - ea
##             where:
##                 tc = average daily air temperature, deg C
##                 eact  = actual vapor pressure, Pa
##-----------------------------------------------------------------------
## arguments
## eact  ## vapor pressure (Pa)
## qair  ## specific humidity (g g-1)
## tc    ## temperature, deg C
## tmin  ## (optional) min daily air temp, deg C 
## tmax  ## (optional) max daily air temp, deg C 
##
## function return variable
## vpd   ##  vapor pressure deficit, Pa  
##-----------------------------------------------------------------------
calc_vpd <- function( eact=NA, qair=NA, tc=NA, tmin=NA, tmax=NA, elv=NA ){

  kTo = 288.15   # base temperature, K (Prentice, unpublished)
  kR  = 8.3143   # universal gas constant, J/mol/K (Allen, 1973)
  kMv = 18.02    # molecular weight of water vapor, g/mol (Tsilingiris, 2008)
  kMa = 28.963   # molecular weight of dry air, g/mol (Tsilingiris, 2008)

  ## Warning: this is crude, if not wrong. 
  if ( !is.na(tmin) && !is.na(tmax) ) {

    my_tc <- 0.5 * (tmin + tmax)

  } else {

    my_tc <- tc

  }

  if (is.na(eact) && !is.na(qair) && !is.na(elv)){

    ## calculate the mass mising ratio of water vapor to dry air (dimensionless)
    wair <- qair / (1 - qair)

    ## calculate atmopheric pressure (Pa) assuming standard conditions at sea level (elv=0)
    patm <- calc_patm( elv )

    ## calculate water vapor pressure 
    rv <- kR / kMv
    rd <- kR / kMa
    eact = patm * wair * rv / (rd + wair * rv)

  }

  ## calculate saturation water vapour pressure in Pa
  esat <- 611.0 * exp( (17.27 * my_tc)/(my_tc + 237.3) )

  ## calculate VPD in units of Pa
  vpd <- ( esat - eact )    

  ## this empirical equation may lead to negative values for VPD (happens very rarely). assume positive...
  vpd <- max( 0.0, vpd )

  return( vpd )

}


extrapolate_missing_headtail <- function(ddf){
  ## extrapolate to missing values at head and tail using mean seasonal cycle
  ##--------------------------------------

  ## new: fill gaps at head
  idxs <- findna_head( ddf$var )
  if (length(idxs)>0) rlang::warn("Filling values with last available data point at head")
  ddf$var[idxs] <- ddf$var[max(idxs)+1]

  ## new: fill gaps at tail
  idxs <- findna_tail( ddf$var )
  if (length(idxs)>0) rlang::warn("Filling values with last available data point at tail.")
  ddf$var[idxs] <- ddf$var[min(idxs)-1]
  
  # ## get mean seasonal cycle
  # ddf_meandoy <- ddf %>% 
  #   dplyr::group_by( doy ) %>% 
  #   dplyr::summarise( meandoy = mean( var , na.rm=TRUE ) )
  # 
  # ## attach mean seasonal cycle as column 'meandoy' to daily dataframe
  # ddf <- ddf %>% 
  #   dplyr::left_join( ddf_meandoy, by="doy" )
  # 
  # ## fill gaps at head and tail
  # ddf$var[ idxs ] <- ddf$meandoy[ idxs ]

  vec <- ddf %>%
    dplyr::pull(var)
  
  return(vec)      
}


findna_headtail <- function( vec ){

  ## Remove (cut) NAs from the head and tail of a vector.
  ## Returns the indexes to be dropped from a vector

  idxs <- c(findna_head(vec), findna_tail(vec))
  
  return(idxs)

}

findna_head <- function( vec ){
  
  ## Remove (cut) NAs from the head and tail of a vector.
  ## Returns the indexes to be dropped from a vector
  
  ## Get indeces of consecutive NAs at head
  if (is.na(vec[1])){
    idx <- 0
    while ( idx < length(vec) ){
      idx <- idx + 1
      test <- head( vec, idx )
      if (any(!is.na(test))){
        ## first non-NA found at position idx
        cuthead <- idx - 1
        break
      }
    }
    idxs_head <- 1:cuthead
  } else {
    idxs_head <- c()
  }
  
  return(idxs_head)
  
}

findna_tail <- function( vec ){
  
  ## Remove (cut) NAs from the head and tail of a vector.
  ## Returns the indexes to be dropped from a vector
  
  ## Get indeces of consecutive NAs at tail
  if (is.na(vec[length(vec)])){
    idx <- 0
    while ( idx < length(vec) ){
      idx <- idx + 1
      test <- tail( vec, idx )
      if (any(!is.na(test))){
        ## first non-NA found at position idx, counting from tail
        cuttail <- idx - 1
        break
      }
    }
    idxs_tail <- (length(vec)-cuttail+1):length(vec)
  } else {
    idxs_tail <- c()
  }
  
  return(idxs_tail)
  
}

na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }

