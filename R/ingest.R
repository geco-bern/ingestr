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

	}
  
  ddf <- ddf %>% 
    group_by(sitename) %>% 
    nest()
	
  return(ddf)

}
