#' Ingest from global fields
#'
#' Read climate data from files as global fields
#'
#' @param siteinfo A data frame with rows for each site and columns `lon` for longitude, `lat` for latitude, 
#' `date_start` and `date_end` specifying required dates.
#' @param source A character used as identifiyer for the type of data source
#' (\code{"watch_wfdei"}, or \code{"cru"}).
#' @param getvars A named list of characters specifying the variable names in
#' the source dataset corresponding to standard names \code{"temp"} for temperature,
#' @param dir A character specifying the directory where data is located.
#' \code{"prec"} for precipitation, \code{"patm"} for atmospheric pressure,
#' \code{"vpd"} for vapour pressure deficit, \code{"netrad"} for net radiation,
#' \code{"swin"} for shortwave incoming radiation.
#' @param timescale A character or vector of characters, specifying the time scale of data used from
#' the respective source (if multiple time scales are available, otherwise is disregarded).
#' @param standardise_units A logical specifying whether units in ingested data are to be standardised
#' following ingestr-standard units.
#' @param verbose if \code{TRUE}, additional messages are printed.
#'
#' @return A data frame (tibble) containing the time series of ingested data, nested for each site.
#' @import purrr dplyr
#' @export
#'
#' @examples \dontrun{inputdata <- ingest_bysite()}  
#'
ingest_globalfields <- function( siteinfo, source, getvars, dir, timescale, standardise_units = TRUE, verbose=FALSE ){
  
  if (source!="etopo1"){
    ## get a data frame with all dates for all sites
    ddf <- purrr::map(
      as.list(seq(nrow(siteinfo))),
      ~init_dates_dataframe(
        year(siteinfo$date_start[.]),
        year(siteinfo$date_end[.]),
        noleap = TRUE,
        freq = "days"))
    names(ddf) <- siteinfo$sitename
    ddf <- ddf %>%
      bind_rows(.id = "sitename") %>%
      select(-year_dec)
  } else {
    ddf <- tibble()
  }
  

  if (source=="watch_wfdei"){
    ##----------------------------------------------------------------------
    ## Read WATCH-WFDEI data (extracting from NetCDF files for this site)
    ##----------------------------------------------------------------------
    ## temperature
    if ("temp" %in% names(getvars)){
      ddf <- ingest_globalfields_watch_byvar( ddf, siteinfo, dir, "Tair_daily" ) %>%
        dplyr::rename(temp = myvar) %>%
        dplyr::mutate(temp = temp - 273.15) %>%
        dplyr::right_join(ddf, by = c("sitename", "date"))
    }
    
    ## precipitation
    if ("prec" %in% names(getvars)){
      ddf <- ingest_globalfields_watch_byvar( ddf, siteinfo, dir, "Rainf_daily" ) %>%
        dplyr::mutate( rain = myvar ) %>%
        left_join(
          ingest_globalfields_watch_byvar( ddf, siteinfo, dir, "Snowf_daily" ) %>%
            dplyr::mutate( snow = myvar ),
          by = c("sitename", "date")
        ) %>%
        dplyr::rename(prec = (rain + snow) * 60 * 60 * 24 ) %>%  # kg/m2/s -> mm/day
        dplyr::right_join(ddf, by = c("sitename", "date"))
    }
    
    ## humidity
    if ("vpd" %in% names(getvars)){
      ddf <- ingest_globalfields_watch_byvar( ddf, siteinfo, dir, "Qair_daily" ) %>%
        dplyr::rename(qair = myvar) %>%
        dplyr::right_join(ddf, by = c("sitename", "date"))
    }
    
    ## PPFD
    if ("ppfd" %in% names(getvars)){
      kfFEC <- 2.04
      ddf <- ingest_globalfields_watch_byvar( ddf, siteinfo, dir, "SWdown_daily" ) %>%
        dplyr::rename(ppfd = myvar * kfFEC * 1.0e-6 * 60 * 60 * 24 ) %>%  # umol m-2 s-1 -> mol m-2 d-1
        dplyr::right_join(ddf, by = c("sitename", "date"))
    }
    
    if (timescale=="m"){
      rlang::abort("ingest_globalfields(): aggregating WATCH-WFDEI to monthly not implemented yet.")
    }
    
  } else if (source=="cru"){
    ##----------------------------------------------------------------------
    ## Read CRU monthly data (extracting from NetCDF files for this site)
    ##----------------------------------------------------------------------
    ## create a monthly data frame    
    mdf <- ddf %>%
      dplyr::select(sitename, date) %>%
      dplyr::mutate(year = lubridate::year(date), moy = lubridate::month(date)) %>%
      dplyr::select(sitename, year, moy) %>%
      dplyr::distinct()
    
    cruvars <- c()
    
    ## temperature
    if ("temp" %in% names(getvars) || "vpd" %in% names(getvars)){
      cruvars <- c(cruvars, "temp")
      mdf <- ingest_globalfields_cru_byvar(siteinfo, dir, getvars[[ "temp" ]] ) %>%
        dplyr::select(sitename, date, myvar) %>%
        dplyr::rename(temp = myvar) %>%
        dplyr::mutate(year = lubridate::year(date), moy = lubridate::month(date)) %>%
        dplyr::select(-date) %>%
        dplyr::right_join(mdf, by = c("sitename", "year", "moy"))
      
    }
    
    ## precipitation
    if ("prec" %in% names(getvars)){
      cruvars <- c(cruvars, "prec")
      mdf <- ingest_globalfields_cru_byvar(siteinfo, dir, getvars[["prec"]] ) %>%
        dplyr::select(sitename, date, myvar) %>%
        dplyr::rename(prec = myvar) %>%
        dplyr::mutate(year = lubridate::year(date), moy = lubridate::month(date)) %>%
        dplyr::select(-date) %>%
        dplyr::right_join(mdf, by = c("sitename", "year", "moy"))
    }
    
    ## vpd from vapour pressure
    if ("vpd" %in% names(getvars)){
      cruvars <- c(cruvars, "vap")
      mdf <- ingest_globalfields_cru_byvar(siteinfo, dir, getvar[["vap"]] ) %>%
        dplyr::select(sitename, date, myvar) %>%
        dplyr::rename(vap = myvar) %>%
        dplyr::mutate(year = lubridate::year(date), moy = lubridate::month(date)) %>%
        dplyr::select(-date) %>%
        dplyr::right_join(mdf, by = c("sitename", "year", "moy"))
    }
    
    ## cloud cover
    if ("ccov" %in% names(getvars)){
      cruvars <- c(cruvars, "ccov")
      mdf <- ingest_globalfields_cru_byvar(siteinfo, dir, getvars[["ccov"]] ) %>%
        dplyr::select(sitename, date, myvar) %>%
        dplyr::rename(ccov = myvar) %>%
        dplyr::mutate(year = lubridate::year(date), moy = lubridate::month(date)) %>%
        # dplyr::select(-date) %>%
        dplyr::right_join(mdf, by = c("sitename", "year", "moy"))
    }
    
    ## wet days
    if ("wetd" %in% names(getvars)){
      cruvars <- c(cruvars, "wetd")
      mdf <- ingest_globalfields_cru_byvar(siteinfo,  dir, getvars[["wetd"]] ) %>%
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
    
  } else if (source == "etopo1"){
    
    filename <- list.files(dir, pattern = ".tif")
    if (length(filename) > 1) rlang::abort("ingest_globalfields(): Found more than 1 file for source 'etopo1'.") 
    if (length(filename) == 0) rlang::abort("ingest_globalfields(): Found no files for source 'etopo1' in the directory provided by argument 'dir'.") 

    ## re-construct this data frame (tibble) - otherwise SpatialPointsDataframe() won't work
    df_lonlat <- tibble(
      sitename = siteinfo$sitename,
      lon      = siteinfo$lon,
      lat      = siteinfo$lat
    )
        
    ddf <- extract_pointdata_allsites( paste0(dir, filename), df_lonlat, get_time = FALSE ) %>% 
      dplyr::select(-lon, -lat) %>% 
      tidyr::unnest(data) %>% 
      dplyr::rename(elv = V1) %>% 
      dplyr::select(sitename, elv) 

  }
  
  return( ddf )
  
}


##--------------------------------------------------------------------
## Extract temperature time series for a set of sites at once (opening
## each file only once).
##--------------------------------------------------------------------
ingest_globalfields_watch_byvar <- function( ddf, siteinfo, dir, varnam ){
  
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
ingest_globalfields_cru_byvar <- function( siteinfo, dir, varnam ){
  
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
  
  ddf <- purrr::map( as.list(unique(mdf$year)), ~expand_clim_cru_monthly_byyr( ., mdf, cruvars ) ) %>%
    bind_rows()
  
  return( ddf )
  
}


##--------------------------------------------------------------------
## Interpolates monthly data to daily data using polynomials or linear
## for a single year
##--------------------------------------------------------------------
expand_clim_cru_monthly_byyr <- function( yr, mdf, cruvars ){
  
  nmonth <- 12
  
  startyr <- mdf$year %>% first()
  endyr   <- mdf$year %>% last()
  
  yr_pvy <- max(startyr, yr-1)
  yr_nxt <- min(endyr, yr+1)
  
  ## add first and last year to head and tail of 'mdf'
  first <- mdf[1:12,] %>% mutate( year = year - 1)
  last  <- mdf[(nrow(mdf)-11):nrow(mdf),] %>% mutate( year = year + 1 )
  
  ddf <- init_dates_dataframe( yr, yr ) %>%
    dplyr::select(-year_dec)
  
  ##--------------------------------------------------------------------
  ## air temperature: interpolate using polynomial
  ##--------------------------------------------------------------------
  if ("temp" %in% cruvars){
    mtemp     <- dplyr::filter( mdf, year==yr     )$temp
    mtemp_pvy <- dplyr::filter( mdf, year==yr_pvy )$temp
    mtemp_nxt <- dplyr::filter( mdf, year==yr_nxt )$temp
    if (length(mtemp_pvy)==0){
      mtemp_pvy <- mtemp
    }
    if (length(mtemp_nxt)==0){
      mtemp_nxt <- mtemp
    }
    
    ddf <- init_dates_dataframe( yr, yr ) %>%
      mutate( temp = monthly2daily( mtemp, "polynom", mtemp_pvy[nmonth], mtemp_nxt[1], leapyear = leap_year(yr) ) ) %>%
      right_join( ddf, by = c("date") ) %>%
      dplyr::select(-year_dec)
  }
  
  ##--------------------------------------------------------------------
  ## precipitation: interpolate using weather generator
  ##--------------------------------------------------------------------
  if ("prec" %in% cruvars){
    mprec <- dplyr::filter( mdf, year==yr )$prec
    mwetd <- dplyr::filter( mdf, year==yr )$wetd
    
    if (any(!is.na(mprec))&&any(!is.na(mwetd))){
      ddf <-  init_dates_dataframe( yr, yr ) %>%
        mutate( prec = get_daily_prec( mprec, mwetd, leapyear = leap_year(yr) ) ) %>%
        right_join( ddf, by = c("date") ) %>%
        dplyr::select(-year_dec)
    }
  }
  
  ##--------------------------------------------------------------------
  ## cloud cover: interpolate using polynomial
  ##--------------------------------------------------------------------
  if ("ccov" %in% cruvars){
    mccov     <- dplyr::filter( mdf, year==yr     )$ccov
    mccov_pvy <- dplyr::filter( mdf, year==yr_pvy )$ccov
    mccov_nxt <- dplyr::filter( mdf, year==yr_nxt )$ccov
    if (length(mccov_pvy)==0){
      mccov_pvy <- mccov
    }
    if (length(mccov_nxt)==0){
      mccov_nxt <- mccov
    }
    
    ddf <-  init_dates_dataframe( yr, yr ) %>%
      mutate( ccov_int = monthly2daily( mccov, "polynom", mccov_pvy[nmonth], mccov_nxt[1], leapyear = leap_year(yr) ) ) %>%
      ## Reduce CCOV to a maximum 100%
      mutate( ccov = ifelse( ccov_int > 100, 100, ccov_int ) ) %>%
      right_join( ddf, by = c("date") ) %>%
      dplyr::select(-year_dec)
  }
  
  ##--------------------------------------------------------------------
  ## VPD: interpolate using polynomial
  ##--------------------------------------------------------------------
  if ("vap" %in% cruvars){
    mvpd     <- dplyr::filter( mdf, year==yr     )$vpd_vap_temp
    mvpd_pvy <- dplyr::filter( mdf, year==yr_pvy )$vpd_vap_temp
    mvpd_nxt <- dplyr::filter( mdf, year==yr_nxt )$vpd_vap_temp
    if (length(mvpd_pvy)==0){
      mvpd_pvy <- mvpd
    }
    if (length(mvpd_nxt)==0){
      mvpd_nxt <- mvpd
    }
    
    ddf <- init_dates_dataframe( yr, yr ) %>%
      mutate( vpd = monthly2daily( mvpd, "polynom", mvpd_pvy[nmonth], mvpd_nxt[1], leapyear = (yr %% 4 == 0) ) ) %>%
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
  
  df_lonlat <- raster::extract(
      rasta, 
      sp::SpatialPoints(dplyr::select(df_lonlat, lon, lat)), # , proj4string = rasta@crs
      sp = TRUE
      ) %>%
    as_tibble() %>%
    tidyr::nest(data = c(-lon, -lat)) %>%
    right_join(df_lonlat, by = c("lon", "lat")) %>%
    mutate( data = purrr::map(data, ~dplyr::slice(., 1)) ) %>%
    dplyr::mutate(data = purrr::map(data, ~t(.))) %>%
    dplyr::mutate(data = purrr::map(data, ~as_tibble(.)))
  
  ## xxx todo: use argument df = TRUE in the extract() function call in order to
  ## return a data frame directly (and not having to rearrange the data afterwards)
  ## xxx todo: implement the GWR method for interpolating using elevation as a 
  ## covariate here.
  
  if (get_time){
    timevals <- raster::getZ(rasta)
    df_lonlat <- df_lonlat %>%
      mutate( data = purrr::map(data, ~bind_cols(., tibble(date = timevals))))
  }
  
  return(df_lonlat)
}

#' Implements a weather generator
#'
#' Implements a weather generator to simulate daily precipitation, given the monthly total and the number of days with rain for each month.
#' 
#' @param mval_prec A vector of twelve numeric values for monthly values of total precipitation.
#' @param mval_wet A vector of twelve integer values for the number of wet days in each month.
#' @param set_seed A logical specifying whether a random seed is set.
#' @param leapyear A logical specifying whether interpolation is done for a leap year (with 366 days).
#'
#' @return A named list of data frames (tibbles) containing input data for each site is returned.
#' 
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


#' Interpolates monthly to daily values
#'
#' Implements different methods to interpolate from monthly to daily values, including fitting a polynomial.
#' 
#' @param mval A vector of twelve numeric values for monthly values.
#' @param method A character string specifying the method for interpolation. Defaults to \code{"polynom"} for using a polynomial.
#' @param mval_prev The monthly value of the month before the twelve months for which values are provided by argument \code{mval}.
#' @param mval_next The monthly value of the month after the twelve months for which values are provided by argument \code{mval}.
#' @param leapyear A logical specifying whether interpolation is done for a leap year (with 366 days).
#'
#' @return A named list of data frames (tibbles) containing input data for each site is returned.
#' 
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