#' Complements the setup settings
#'
#' Complements the settings based on the site meta info CSV file or data frame.
#'
#' @param site_info A character string specifying the path to the site meta 
#' information file, or a data frame containing the site meta info. Required
#' columns are:
#' \describe{
#' \item{\code{sitename}}{Name of the site, must be the first column of the file.}
#' \item{\code{lon}}{Longitude of site.}
#' \item{\code{lat}}{Latitude of site.}
#' \item{\code{elv}}{Elevation of site, in m.}
#' \item{\code{year_start, year_end}}{Years for which the simulation is to be done,
#' corresponding to data availability from site.}
#' }
#' @param params_siml A named list containing the simulation parameters
#'  for SOFUN.
#'
#' @return A data frame (tibble) containing the site meta information, 
#' complemented by column \code{params_siml} which is a nested list 
#' of complemented simulation parameters.
#' @export
#' @importFrom magrittr %>%
#' @examples 
#' \dontrun{
#'  setup <- prepare_setup_sofun(
#'    site_info = site_info,
#'    params_siml = params_siml
#'   )
#' }

prepare_setup_sofun <- function(
  site_info,
  params_siml
  ){
  
  # predefine variables for CRAN check compliance
  year_end <- year_start <- . <- NULL
  
  ##-----------------------------------------------------------
  ## Ensemble: multiple site-scale simulations that "go together"
  ## In this case, <settings$name> is the name of the ensemble 
  ## (e.g., "fluxnet2015")
  ##-----------------------------------------------------------
  ## Read info that varies between sites from the meta information
  ## file <site_info>:
  ## - site name, must be: column number 1 
  ## - longitude of site, column must be named 'lon'
  ## - latitude of site, column must be named 'lat'
  ## - elevation of site, column must be named 'elv'  
  ## - years for which simulation is to be done 
  ## (corresponding to data availability from site), 
  ##   requires two columns named 'year_start' and 'year_end'.
  if (is.character(site_info)){
    if (!file.exists(site_info)){
      stop( "prepare_setup_sofun():
            Path specified by site_info does not exist." )
    } 
    site_info <- utils::read.csv2( site_info )
  }

  ##--------------------------------------
  ## Complement settings with meta info for each site
  ##--------------------------------------
  site_info <- site_info %>% 

    ## clean up and complement
    mutate(year_start = as.numeric(year_start),
           year_end = as.numeric(year_end)) %>% 
    mutate(date_start = lubridate::ymd( 
             paste0( as.character( site_info$year_start ), "-01-01" ) ),
           date_end   = lubridate::ymd( 
             paste0( as.character( site_info$year_end ), "-12-31" ) ))

  ## add simulation parameters as a list nested in 'site_info'
  site_info <- params_siml %>%
    dplyr::bind_cols(.) %>% 
    dplyr::mutate(tmp = 1) %>% 
    dplyr::right_join(mutate(site_info, tmp = 1), by = "tmp") %>% 
    dplyr::select(names(site_info), names(params_siml)) %>% 
    dplyr::mutate(nyeartrend = year_end - year_start + 1) %>%
    dplyr::rename(firstyeartrend = year_start) %>% 
    tidyr::nest(params_siml = c(names(params_siml),
                                "firstyeartrend", "nyeartrend"))
  
  return(site_info)

}
