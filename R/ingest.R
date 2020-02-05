#' Data ingest
#'
#' Ingests data for site scale simulations with rsofun (or any other Dynamic Vegetation Model).
#'
#' @param siteinfo A data frame containing site meta info. Required columns are: \code{"sitename", "date_start", "date_end", "lon", "lat", "elv"}.
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
#' @param overwrite_csv_climate_lev1 if \code{TRUE}, climate input CSV files created after collecting site-scale meteo data are overwritten.
#' @param overwrite_csv_climate_lev2 if \code{TRUE}, climate input CSV files created after collecting data from global field for all sites are overwritten.
#' @param overwrite_csv_climate_lev3 if \code{TRUE}, climate input CSV files created after deriving SOFUN-standard input (naming, units, and interpolation) are overwritten.
#' @param verbose if \code{TRUE}, additional messages are printed.
#'
#' @return A named list of data frames (tibbles) containing input data for each site is returned.
#' @import purrr dplyr
#' @export
#'
#' @examples inputdata <- prepare_input_sofun( settings_input = settings_input, settings_sims = settings_sims, overwrite_climate = FALSE, verbose = TRUE )
#'
ingest <- function(
	siteinfo,
	source,
	getvars,
	dir,
	settings,
	timescale                  = "d",
	overwrite_csv_climate_lev1 = FALSE,
	overwrite_csv_climate_lev2 = FALSE,
	overwrite_csv_climate_lev3 = FALSE,
	verbose                    = FALSE
  ){

	if (source == "fluxnet2015"){
	  #-----------------------------------------------------------
	  # Get data from sources given by site
	  #-----------------------------------------------------------
		ddf <- purrr::map(
		  as.list(seq(nrow(siteinfo))),
		  ~ingest_bysite( siteinfo$sitename[.],
											source                     = source,
											getvars                    = getvars,
											dir                        = dir,
											settings                   = settings,
											timescale                  = timescale,
											date_start                 = siteinfo$date_start[.],
											date_end                   = siteinfo$date_end[.],
											overwrite_csv_climate_lev1 = overwrite_csv_climate_lev1,
											overwrite_csv_climate_lev2 = overwrite_csv_climate_lev2,
											overwrite_csv_climate_lev3 = overwrite_csv_climate_lev3,
											verbose                    = verbose
		  )
		)

		names(ddf) <- siteinfo$sitename
		ddf <- ddf %>%
		  bind_rows(.id = "sitename")


	} else if (source == "cru" || source == "watch_wfdei"){
	  #-----------------------------------------------------------
	  # Get data from global fields
	  #-----------------------------------------------------------
    ddf <- get_input_sofun_climate_globalfields(siteinfo,
    																						source = source,
    																						dir = dir,
    																						getvars = getvars,
																					      overwrite_csv = overwrite_csv_climate_lev2,
																					      verbose = FALSE
																					     )

	}

	return(ddf)

}


##-----------------------------------------------------------
## Read climate data from files given by sites
##-----------------------------------------------------------
ingest_bysite <- function(
	sitename,
	source,
	getvars,
	dir,
	settings,
	timescale,
	date_start,
	date_end,
	overwrite_csv_climate_lev1,
	overwrite_csv_climate_lev2,
	overwrite_csv_climate_lev3,
	verbose = FALSE
	){

	## initialise data frame with all required dates
  ddf <- init_dates_dataframe(
    year(date_start),
    year(date_end),
    noleap = TRUE) %>%
    dplyr::select(-year_dec)

	##-----------------------------------------------------------
	## FLUXNET 2015 readin
	##-----------------------------------------------------------
	if (source == "fluxnet2015"){
    ddf <- get_obs_bysite_fluxnet2015(sitename,
                                      path_fluxnet2015 = dir,
                                      path_fluxnet2015_hh = settings$dir_hh,
                                      timescale        = timescale,
                                      getvars          = getvars,
                                      getswc           = settings$getswc,
                                      threshold_GPP    = settings$threshold_GPP,
                                      verbose          = verbose
                                      ) %>%
    			  	right_join(ddf, by = "date")

	}

  return( ddf )

}


##-----------------------------------------------------------
## Read climate data from files as global fields
##-----------------------------------------------------------
get_input_sofun_climate_globalfields <- function( siteinfo, source, dir, getvars, overwrite_csv=FALSE, verbose=FALSE ){

	## get a data frame with all dates for all sites
	ddf <- purrr::map(
		as.list(seq(nrow(siteinfo))),
		~init_dates_dataframe(
											    year(siteinfo$date_start[.]),
											    year(siteinfo$date_end[.]),
											    noleap = TRUE))
	names(ddf) <- siteinfo$sitename
  ddf <- ddf %>%
    bind_rows(.id = "sitename") %>%
    select(-year_dec)

	if (source=="watch_wfdei"){
    ##----------------------------------------------------------------------
    ## Read WATCH-WFDEI data (extracting from NetCDF files for this site)
    ##----------------------------------------------------------------------
    ## temperature
		if ("temp" %in% names(getvars)){
      ddf <- get_input_sofun_climate_globalfields_watch_byvar( ddf, siteinfo, dir, "Tair_daily" ) %>%
        dplyr::rename(temp = myvar) %>%
        dplyr::mutate(temp = temp - 273.15) %>%
        dplyr::right_join(ddf, by = c("sitename", "date"))
		}

    ## precipitation
		if ("prec" %in% names(getvars)){
      ddf <- get_input_sofun_climate_globalfields_watch_byvar( ddf, siteinfo, dir, "Rainf_daily" ) %>%
        dplyr::mutate( rain = myvar ) %>%
        left_join(
          get_input_sofun_climate_globalfields_watch_byvar( ddf, siteinfo, dir, "Snowf_daily" ) %>%
            dplyr::mutate( snow = myvar ),
          by = c("sitename", "date")
        ) %>%
        dplyr::rename(prec = (rain + snow) * 60 * 60 * 24 ) %>%  # kg/m2/s -> mm/day
        dplyr::right_join(ddf_prec, by = c("sitename", "date"))
		}

    ## humidity
    if ("vpd" %in% names(getvars)){
      ddf <- get_input_sofun_climate_globalfields_watch_byvar( ddf, siteinfo, dir, "Qair_daily" ) %>%
        dplyr::rename(qair = myvar) %>%
        dplyr::right_join(ddf_qair, by = c("sitename", "date"))
    }

    ## PPFD
    if ("ppfd" %in% names(getvars)){
      kfFEC <- 2.04
      ddf <- get_input_sofun_climate_globalfields_watch_byvar( ddf, siteinfo, dir, "SWdown_daily" ) %>%
        dplyr::rename(ppfd = myvar * kfFEC * 1.0e-6 * 60 * 60 * 24 ) %>%  # umol m-2 s-1 -> mol m-2 d-1
        dplyr::right_join(ddf_ppfd, by = c("sitename", "date"))
    }

	} else if (source=="cru"){
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
    if ("temp" %in% names(getvars) || "vpd" %in% names(getvars)){
      cruvars <- c(cruvars, "temp")
      mdf <- get_input_sofun_climate_globalfields_cru_byvar(siteinfo, dir, "tmp" ) %>%
        dplyr::select(sitename, date, myvar) %>%
        dplyr::rename(temp = myvar) %>%
        dplyr::mutate(year = lubridate::year(date), moy = lubridate::month(date)) %>%
        dplyr::select(-date) %>%
        dplyr::right_join(mdf, by = c("sitename", "year", "moy"))

    }

    ## precipitation
    if ("prec" %in% names(getvars)){
      cruvars <- c(cruvars, "prec")
      mdf <- get_input_sofun_climate_globalfields_cru_byvar(siteinfo, dir, "pre" ) %>%
        dplyr::select(sitename, date, myvar) %>%
        dplyr::rename(prec = myvar) %>%
        dplyr::mutate(year = lubridate::year(date), moy = lubridate::month(date)) %>%
        dplyr::select(-date) %>%
        dplyr::right_join(mdf, by = c("sitename", "year", "moy"))
    }

    ## vpd from vapour pressure
    if ("vpd" %in% names(getvars)){
      cruvars <- c(cruvars, "vap")
      mdf <- get_input_sofun_climate_globalfields_cru_byvar(siteinfo, dir, "vap" ) %>%
        dplyr::select(sitename, date, myvar) %>%
        dplyr::rename(vap = myvar) %>%
        dplyr::mutate(year = lubridate::year(date), moy = lubridate::month(date)) %>%
        dplyr::select(-date) %>%
        dplyr::right_join(mdf, by = c("sitename", "year", "moy"))
    }

    ## cloud cover
    if ("ccov" %in% names(getvars)){
      cruvars <- c(cruvars, "ccov")
      mdf <- get_input_sofun_climate_globalfields_cru_byvar(siteinfo, dir, "cld" ) %>%
        dplyr::select(sitename, date, myvar) %>%
        dplyr::rename(ccov = myvar) %>%
        dplyr::mutate(year = lubridate::year(date), moy = lubridate::month(date)) %>%
        # dplyr::select(-date) %>%
        dplyr::right_join(mdf, by = c("sitename", "year", "moy"))
    }

    ## wet days
    if ("wetd" %in% names(getvars)){
      cruvars <- c(cruvars, "wetd")
      mdf <- get_input_sofun_climate_globalfields_cru_byvar(siteinfo,  dir, "wet" ) %>%
        dplyr::select(sitename, date, myvar) %>%
        dplyr::rename(wetd = myvar) %>%
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
        mutate( vpd_vap_cru_temp_cru = calc_vpd( eact = 1e2 * vap, tc = temp ) )
    }

    ## expand monthly to daily data
    if (length(cruvars)>0){
      ddf <- expand_clim_cru_monthly( mdf, cruvars ) %>%
        right_join( ddf, by = "date" )
    }

	}

  return( ddf )

}


##--------------------------------------------------------------------
## Extract temperature time series for a set of sites at once (opening
## each file only once).
##--------------------------------------------------------------------
get_input_sofun_climate_globalfields_watch_byvar <- function( ddf, siteinfo, dir, varnam ){

  dirn <- paste0( dir, "/", varnam, "/" )

  ## loop over all year and months that are required
  year_start <- ddf %>%
    dplyr::pull(date) %>%
    min() %>%
    lubridate::year()

  year_end <- ddf %>%
    dplyr::pull(date) %>%
    max() %>%
    lubridate::year()

  allmonths <- 1:12
  allyears <- year_start:year_end

  ## construct data frame holding longitude and latitude info
  df_lonlat <- tibble(
    sitename = siteinfo$sitename,
    lon      = siteinfo$lon,
    lat      = siteinfo$lat
    )

  ## extract all the data
  df <- expand.grid(allmonths, allyears) %>%
    dplyr::as_tibble() %>%
    setNames(c("mo", "yr")) %>%
    rowwise() %>%
    dplyr::mutate(filename = paste0( dirn, "/", varnam, "_WFDEI_", sprintf( "%4d", yr ), sprintf( "%02d", mo ), ".nc" )) %>%
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
get_input_sofun_climate_globalfields_cru_byvar <- function( siteinfo, dir, varnam ){

  ## construct data frame holding longitude and latitude info
  df_lonlat <- tibble(
    sitename = siteinfo$sitename,
    lon      = siteinfo$lon,
    lat      = siteinfo$lat
    )

  ## extract the data
  filename <- list.files( dir, pattern=paste0( varnam, ".dat.nc" ) )
  df <- extract_pointdata_allsites( paste0(dir, filename), df_lonlat, get_time = TRUE ) %>%
    dplyr::mutate(data = purrr::map(data, ~setNames(., c("myvar", "date"))))

  ## rearrange to a monthly data frame
  mdf <- df %>%
    tidyr::unnest(data)

  return( mdf )
}


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

  startyr <- year(mdf$date) %>% unique() %>% first()
  endyr   <- year(mdf$date) %>% unique() %>% last()

  yr_pvy <- max(startyr, yr-1)
  yr_nxt <- min(endyr, yr+1)

  ## add first and last year to head and tail of 'mdf'
  first <- mdf[1:12,] %>% mutate( date = date - years(1) )
  last  <- mdf[(nrow(mdf)-11):nrow(mdf),] %>% mutate( date = date + years(1) )

  ddf <- init_dates_dataframe( yr, yr ) %>%
    dplyr::select(-year_dec)

  ##--------------------------------------------------------------------
  ## air temperature: interpolate using polynomial
  ##--------------------------------------------------------------------
  if ("temp" %in% cruvars){
    mtemp     <- dplyr::filter( mdf, year(date)==yr     )$temp
    mtemp_pvy <- dplyr::filter( mdf, year(date)==yr_pvy )$temp
    mtemp_nxt <- dplyr::filter( mdf, year(date)==yr_nxt )$temp
    if (length(mtemp_pvy)==0){
      mtemp_pvy <- mtemp
    }
    if (length(mtemp_nxt)==0){
      mtemp_nxt <- mtemp
    }

    ddf <- init_dates_dataframe( yr, yr ) %>%
           mutate( temp_int = monthly2daily( mtemp, "polynom", mtemp_pvy[nmonth], mtemp_nxt[1], leapyear = leap_year(yr) ) ) %>%
           right_join( ddf, by = c("date") ) %>%
    			 dplyr::select(-year_dec)
  }

  ##--------------------------------------------------------------------
  ## precipitation: interpolate using weather generator
  ##--------------------------------------------------------------------
  if ("prec" %in% cruvars){
    mprec <- dplyr::filter( mdf, year(date)==yr )$prec
    mwetd <- dplyr::filter( mdf, year(date)==yr )$wetd

    if (any(!is.na(mprec))&&any(!is.na(mwetd))){
      ddf <-  init_dates_dataframe( yr, yr ) %>%
              mutate( prec_gen = get_daily_prec( mprec, mwetd, leapyear = leap_year(yr) ) ) %>%
              right_join( ddf, by = c("date") ) %>%
					    dplyr::select(-year_dec)
    }
  }

  ##--------------------------------------------------------------------
  ## cloud cover: interpolate using polynomial
  ##--------------------------------------------------------------------
  if ("ccov" %in% cruvars){
    mccov     <- dplyr::filter( mdf, year(date)==yr     )$ccov
    mccov_pvy <- dplyr::filter( mdf, year(date)==yr_pvy )$ccov
    mccov_nxt <- dplyr::filter( mdf, year(date)==yr_nxt )$ccov
    if (length(mccov_pvy)==0){
      mccov_pvy <- mccov
    }
    if (length(mccov_nxt)==0){
      mccov_nxt <- mccov
    }

    ddf <-  init_dates_dataframe( yr, yr ) %>%
            mutate( ccov_int = monthly2daily( mccov, "polynom", mccov_pvy[nmonth], mccov_nxt[1], leapyear = leap_year(yr) ) ) %>%
            ## Reduce CCOV to a maximum 100%
            mutate( ccov_int = ifelse( ccov_int > 100, 100, ccov_int ) ) %>%
            right_join( ddf, by = c("date") ) %>%
				    dplyr::select(-year_dec)
  }

  ##--------------------------------------------------------------------
  ## VPD: interpolate using polynomial
  ##--------------------------------------------------------------------
  if ("vap" %in% cruvars){
    mvpd     <- dplyr::filter( mdf, year(date)==yr     )$vpd_vap_temp
    mvpd_pvy <- dplyr::filter( mdf, year(date)==yr_pvy )$vpd_vap_temp
    mvpd_nxt <- dplyr::filter( mdf, year(date)==yr_nxt )$vpd_vap_temp
    if (length(mvpd_pvy)==0){
      mvpd_pvy <- mvpd
    }
    if (length(mvpd_nxt)==0){
      mvpd_nxt <- mvpd
    }

    ddf <- init_dates_dataframe( yr, yr ) %>%
               mutate( vpd_vap_temp_int = monthly2daily( mvpd, "polynom", mvpd_pvy[nmonth], mvpd_nxt[1], leapyear = (yr %% 4 == 0) ) ) %>%
               right_join( ddf, by = c("date") ) %>%
						   dplyr::select(-year_dec)
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

##--------------------------------------------------------------------
## Extracts point data for a set of sites given by df_lonlat using
## functions from the raster package.
##--------------------------------------------------------------------
extract_pointdata_allsites <- function( filename, df_lonlat, get_time = FALSE ){

  ## load file using the raster library
  #print(paste("Creating raster brick from file", filename))
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
