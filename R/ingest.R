#' Data ingest
#'
#' Ingests data for site scale simulations with rsofun (or any other Dynamic Vegetation Model).
#'
#' @param siteinfo A data frame containing site meta info. Required columns are: \code{"sitename", "date_start", "date_end", "lon", "lat", "elv"}.
#' @param source A character used as identifiyer for the type of data source
#' (e.g., \code{"fluxnet"}).
#' @param getvars A named list of characters specifying the variable names in
#' the source dataset corresponding to standard names \code{"temp"} for temperature,
#' \code{"prec"} for precipitation, \code{"patm"} for atmospheric pressure,
#' \code{"vpd"} for vapour pressure deficit, \code{"netrad"} for net radiation,
#' \code{"swin"} for shortwave incoming radiation.
#' @param dir A character specifying the directory where data is located.
#' @param settings A list of additional settings used for reading original files.
#' @param timescale A character or vector of characters, specifying the time scale of data used from
#' the respective source (if multiple time scales are available, otherwise is disregarded).
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
	verbose   = FALSE
  ){
  
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


	} else if (source == "cru" || source == "watch_wfdei"){
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
	  
	} else if (source == "co2"){
	  #-----------------------------------------------------------
	  # Get CO2 data per year, independent of site
	  #-----------------------------------------------------------
	  ddf <- purrr::map(
	    as.list(seq(nrow(siteinfo))),
	    ~ingest_bysite(
	      sitename = siteinfo$sitename[.],
	      source = source,
	      year_start = lubridate::year(siteinfo$date_start[.]),
	      year_end   = lubridate::year(siteinfo$date_end[.]),
	      verbose = FALSE,
	      settings = settings
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
	  
	}  else {
	  rlang::abort("ingest(): Argument 'source' could not be identified. Use one of 'fluxnet', 'cru', 'watch_wfdei', or 'gee'.")
	}
  
  ddf <- ddf %>% 
    bind_rows() %>%
    group_by(sitename) %>% 
    nest()
	
  return(ddf)

}
