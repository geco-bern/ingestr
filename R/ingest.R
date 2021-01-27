#' Data ingest
#'
#' Ingests data for site scale simulations with rsofun (or any other Dynamic Vegetation Model).
#'
#' @param siteinfo A data frame containing site meta info. Required columns are: \code{"sitename", "date_start", "date_end", "lon", "lat", "elv"}.
#' @param source A character used as identifiyer for the type of data source
#' (e.g., \code{"fluxnet"}). See vignette for a full description of available options.
#' @param getvars A named list of characters specifying the variable names in
#' the source dataset corresponding to standard names \code{"temp"} for temperature,
#' \code{"prec"} for precipitation, \code{"patm"} for atmospheric pressure,
#' \code{"vpd"} for vapour pressure deficit, \code{"netrad"} for net radiation,
#' \code{"swin"} for shortwave incoming radiation.
#' @param dir A character specifying the directory where data is located.
#' @param settings A list of additional settings used for reading original files.
#' @param timescale A character or vector of characters, specifying the time scale of data used from
#' the respective source (if multiple time scales are available, otherwise is disregarded).
#' @param parallel A logical specifying whether ingest is run as parallel jobs for each site. This option is
#' only available for \code{source = "modis"} and requires argument \code{ncores} to be set.
#' @param ncores An integer specifying the number of cores for parallel runs of ingest per site. Required only
#' if \code{parallel = TRUE}
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
	timescale = "d",
	parallel  = FALSE,
	ncores    = NULL,
	verbose   = FALSE
  ){

  if (!(source %in% c("hwsd", "etopo1", "wwf", "soilgrids", "wise"))){

    ## complement dates information
    if (!("year_start" %in% names(siteinfo))){
      if ("date_start" %in% names(siteinfo)){
        siteinfo <- siteinfo %>%
          mutate(year_start = lubridate::year(date_start))
      } else {
        rlang::abort("ingest(): Columns 'year_start' and 'date_start' missing in object provided by argument 'siteinfo'")
      }
    }
    if (!("year_end" %in% names(siteinfo))){
      if ("date_end" %in% names(siteinfo)){
        siteinfo <- siteinfo %>%
          mutate(year_end = lubridate::year(date_end))
      } else {
        rlang::abort("ingest(): Columns 'year_end' and 'date_end' missing in object provided by argument 'siteinfo'")
      }
    }

    if (!("date_start" %in% names(siteinfo))){
      if ("year_start" %in% names(siteinfo)){
        siteinfo <- siteinfo %>%
          mutate(date_start = lubridate::ymd(paste0(as.character(year_start), "-01-01")))
      } else {
        rlang::abort("ingest(): Columns 'year_start' and 'date_start' missing in object provided by argument 'siteinfo'")
      }
    }
    if (!("date_end" %in% names(siteinfo))){
      if ("year_end" %in% names(siteinfo)){
        siteinfo <- siteinfo %>%
          mutate(date_end = lubridate::ymd(paste0(as.character(year_end), "-12-31")))
      } else {
        rlang::abort("ingest(): Columns 'year_end' and 'date_end' missing in object provided by argument 'siteinfo'")
      }
    }

  }

	if (source == "fluxnet"){
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
											year_start                 = lubridate::year(siteinfo$date_start[.]),
											year_end                   = lubridate::year(siteinfo$date_end[.]),
											verbose                    = verbose
		  )
		) %>%
		bind_rows()


	} else if (source == "cru" || source == "watch_wfdei" || source == "ndep"){
	  #-----------------------------------------------------------
	  # Get data from global fields
	  #-----------------------------------------------------------
    ddf <- ingest_globalfields(siteinfo,
                               source = source,
                               dir = dir,
                               getvars = getvars,
                               timescale = timescale,
                               verbose = FALSE
    )

	} else if (source == "gee"){
	  #-----------------------------------------------------------
	  # Get data from the remote server
	  #-----------------------------------------------------------
	  ## Define years covered based on site meta info:
	  ## take all years used for at least one site.
	  year_start <- siteinfo %>%
	    pull(year_start) %>%
	    min()

	  year_end <- siteinfo %>%
	    pull(year_end) %>%
	    max()

	  ddf <- purrr::map(
	    as.list(seq(nrow(siteinfo))),
	    ~ingest_gee_bysite(
	      slice(siteinfo, .),
	      start_date           = paste0(as.character(year_start), "-01-01"),
	      end_date             = paste0(as.character(year_end), "-12-31"),
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
	      python_path          = settings$python_path,
	      gee_path             = settings$gee_path,
	      data_path            = settings$data_path,
	      method_interpol      = settings$method_interpol,
	      keep                 = settings$keep
	    )
	  )

	} else if (source == "modis"){
	  #-----------------------------------------------------------
	  # Get data from the remote server
	  #-----------------------------------------------------------
		if (parallel){

			if (is.null(ncores)) rlang::abort(paste("Aborting. Please provide number of cores for parallel jobs."))

	    cl <- multidplyr::new_cluster(ncores) %>%
	      multidplyr::cluster_assign(settings = settings) %>%
	      multidplyr::cluster_library(c("dplyr", "purrr", "rlang", "ingestr", "readr", "lubridate", "MODISTools", "tidyr"))

		  ## distribute to cores, making sure all data from a specific site is sent to the same core
		  ddf <- tibble(ilon = seq(nrow(siteinfo))) %>%
		    multidplyr::partition(cl) %>%
		    dplyr::mutate(data = purrr::map( ilon,
		                                    ~ingest_modis_bysite(
		                                    	slice(siteinfo, .),
				      														settings))) %>%
		    collect() %>%
		    tidyr::unnest(data)

		} else {

		  ddf <- purrr::map(
		    as.list(seq(nrow(siteinfo))),
		    ~ingest_modis_bysite(
		      slice(siteinfo, .),
		      settings
		      )
		  	)

		}


	} else if (source == "co2_mlo"){
	  #-----------------------------------------------------------
	  # Get CO2 data year, independent of site
	  #-----------------------------------------------------------
	  df_co2 <- climate::meteo_noaa_co2() %>%
	    dplyr::select(yy, co2_avg) %>%
	    dplyr::rename(year = yy) %>%
	    group_by(year) %>%
	    summarise(co2_avg = mean(co2_avg, na.rm = TRUE))

	  ddf <- purrr::map(
	    as.list(seq(nrow(siteinfo))),
	    ~expand_co2_bysite(
	      df_co2,
	      sitename = siteinfo$sitename[.],
	      year_start = lubridate::year(siteinfo$date_start[.]),
	      year_end   = lubridate::year(siteinfo$date_end[.])
	      )
	    )


	} else if (source == "etopo1"){
	  #-----------------------------------------------------------
	  # Get ETOPO1 elevation data. year_start and year_end not required
	  #-----------------------------------------------------------
	  ddf <- ingest_globalfields(siteinfo,
	                             source = source,
	                             dir = dir,
	                             getvars = NULL,
	                             timescale = NULL,
	                             verbose = FALSE
	  )

	} else if (source == "hwsd"){
	  #-----------------------------------------------------------
	  # Get HWSD soil data. year_start and year_end not required
	  #-----------------------------------------------------------
	  con <- rhwsd::get_hwsd_con()
	  ddf <- rhwsd::get_hwsd_siteset(x = dplyr::select(siteinfo, sitename, lon, lat), con = con, hwsd.bil = settings$fil ) %>%
	    dplyr::ungroup() %>%
	    dplyr::select(sitename, data) %>%
	    tidyr::unnest(data)

	} else if (source == "wwf"){
	  #-----------------------------------------------------------
	  # Get WWF ecoregion data. year_start and year_end not required
	  #-----------------------------------------------------------
	  ddf <- ingest_globalfields(siteinfo,
	                             source = source,
	                             dir = dir,
	                             getvars = NULL,
	                             timescale = NULL,
	                             verbose = FALSE,
	                             layer = settings$layer
	  )

	} else if (source == "soilgrids"){
	  #-----------------------------------------------------------
	  # Get SoilGrids soil data. year_start and year_end not required
	  # Code from https://git.wur.nl/isric/soilgrids/soilgrids.notebooks/-/blob/master/markdown/xy_info_from_R.md
	  #-----------------------------------------------------------
	  siteinfo <- siteinfo %>%
	    rename(id = sitename, longitude = lon, latitude = lat)

	  spdata <- sf::st_as_sf(siteinfo, coords = c("longitude", "latitude"), crs = 4326)

	  igh <- '+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs'
	  spdata_igh <- sf::st_transform(spdata, igh)

	  data_igh <- data.frame(sf::st_coordinates(spdata_igh), id = spdata_igh$id)

	  fun_pixel_values  <- function(rowPX, data, VOI, VOI_LYR){
	    as.numeric(
	      gdallocationinfo(
	        srcfile = paste0(settings$webdav_path, "/", VOI, "/", VOI_LYR, ".vrt"),
	        x = data[rowPX, "X"],
	        y = data[rowPX, "Y"],
	        geoloc = TRUE,
	        valonly = TRUE))
	  }

	  value_pixels <- unlist(lapply(1:nrow(siteinfo), function(x){fun_pixel_values(x, data_igh, settings$voi, settings$voi_layer)}))

	  ddf <- siteinfo %>%
	    as_tibble() %>%
	    mutate(var = value_pixels) %>%
	    dplyr::select(id, var) %>%
	    rename(sitename = id, !!settings$voi := var)

	} else if (source == "wise"){
	  #-----------------------------------------------------------
	  # Get WISE30secs soil data. year_start and year_end not required
	  #-----------------------------------------------------------
	  ddf <- purrr::map_dfc(as.list(settings$varnam), ~ingest_wise_byvar(., siteinfo, layer = settings$layer, dir = dir))
	  
	  if (length(settings$varnam) > 1){
	    ddf <- ddf %>% 
	      rename(lon = lon...1, lat = lat...2) %>% 
	      dplyr::select(-starts_with("lon..."), -starts_with("lat...")) %>% 
	      right_join(dplyr::select(siteinfo, sitename, lon, lat), by = c("lon", "lat")) %>% 
	      dplyr::select(-lon, -lat)
	    
	  } else {
	    ddf <- ddf %>% 
	      right_join(dplyr::select(siteinfo, sitename, lon, lat), by = c("lon", "lat")) %>% 
	      dplyr::select(-lon, -lat)
	
	  }
	    
	} else {

	  rlang::warn(paste("you selected source =", source))
	  rlang::abort("ingest(): Argument 'source' could not be identified. Use one of 'fluxnet', 'cru', 'watch_wfdei', 'co2_mlo', 'etopo1', or 'gee'.")

	}

  ddf <- ddf %>%
    bind_rows() %>%
    group_by(sitename) %>%
    nest()

  return(ddf)

}

## give each site and day within year the same co2 value
expand_co2_bysite <- function(df, sitename, year_start, year_end){

  ddf <- init_dates_dataframe( year_start, year_end ) %>%
    dplyr::mutate(year = lubridate::year(date)) %>%
    dplyr::left_join(
      df,
      by = "year"
    ) %>%
    dplyr::mutate(sitename = sitename) %>%
    dplyr::select(sitename, date, co2 = co2_avg)

  return(ddf)
}

