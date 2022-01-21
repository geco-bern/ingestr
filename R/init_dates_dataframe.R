#' Initialises a data frame with dates
#'
#' Creates a dataframe (tibble) with rows for each date (ymd object from
#' library lubridate) from 'yrstart' to 'yrend'. Intervals of dates is
#' specified by argument 'freq'.
#'
#' @param yrstart An integer defining the start year of dates covered by the 
#'  dataframe
#' @param yrend An integer defining the end year of dates covered by 
#'  the dataframe
#' @param startmoy An integer defining the start month of dates covered by 
#'  the dataframe. Defaults to 1.
#' @param startdoy An integer defining the start day-of-year of dates covered 
#'  by the dataframe. Defaults to 1.
#' @param timescale A character string specifying the time steps of dates 
#'  (in rows). Defaults to \code{"d"}. Any of \code{"y", "m", "d", "h", "hh"}.
#' @param endmoy An integer defining the end month of dates covered by the 
#'  dataframe. Defaults to 12.
#' @param enddom An integer defining the end day-of-year of dates covered by 
#'  the dataframe. Defaults to 31.
#' @param noleap Whether leap years are ignored. Defaults to \code{FALSE}.
#'
#' @return A data frame (tibble) with dates.
#' @export
#'
#' @examples
#' \dontrun{
#'   ddf <- init_dates_dataframe(
#'   2000,
#'   2003,
#'   startmoy=1,
#'   startdoy=1,
#'   timescale="d",
#'   endmoy=12,
#'   enddom=31,
#'   noleap=FALSE
#'   )
#' }
#'
init_dates_dataframe <- function(
  yrstart,
  yrend,
  startmoy = 1,
  startdoy = 1,
  timescale = "d",
  endmoy = 12,
  enddom = 31,
  noleap = FALSE
  ) {
  
  if (timescale=="d"){
    freq = "days"
  } else if (timescale=="m"){
    freq = "months"
  } else if (timescale=="y"){
    freq = "years"
  } else if (timescale=="h"){
    freq = "hours"
  } else if (timescale=="hh"){
    freq = "30 mins"
  }

  if (freq=="days"){
    startdate <- lubridate::ymd(
      paste0( as.character(yrstart), "-", sprintf( "%02d", startmoy), "-01" ) )
    #+ lubridate::days( startdoy - 1 )
    enddate   <- lubridate::ymd(
      paste0( as.character(yrend  ), "-", sprintf( "%02d", endmoy  ), "-",
              sprintf( "%02d", enddom  ) ) )
  } else if (freq=="months"){
    ## date is always the 15th of each month
    startdate <- lubridate::ymd(
      paste0( as.character(yrstart), "-", sprintf( "%02d", startmoy), "-15" ) )
    enddate   <- lubridate::ymd(
      paste0( as.character(yrend  ), "-", sprintf( "%02d", endmoy  ), "-15" ) )
  } else if (freq=="years"){
    ## date is always the 15th of each month
    startdate <- lubridate::ymd(
      paste0( as.character(yrstart), "-", sprintf( "%02d", 1), "-01" ) )
    enddate   <- lubridate::ymd(
      paste0( as.character(yrend  ), "-", sprintf( "%02d", 7), "-01" ) )
  } else if (freq=="hours"){
    startdate <- lubridate::ymd_hms(
      paste0( as.character(yrstart), "-", sprintf( "%02d", startmoy),
              "-01 00:00:00" ) ) + lubridate::days( startdoy - 1 )
    enddate   <- lubridate::ymd_hms(
      paste0( as.character(yrend  ), "-", sprintf( "%02d", endmoy  ), "-",
              sprintf( "%02d", enddom  ), " 23:00:00" ) )
  } else if (freq=="30 mins"){
    startdate <- lubridate::ymd_hms(
      paste0( as.character(yrstart), "-", sprintf( "%02d", startmoy),
              "-01 00:00:00" ) ) + lubridate::days( startdoy - 1 )
    enddate   <- lubridate::ymd_hms(
      paste0( as.character(yrend  ), "-", sprintf( "%02d", endmoy  ), "-",
              sprintf( "%02d", enddom  ), " 23:30:00" ) )
  }
  
  if (startdate > enddate || is.null(startdate) || is.null(enddate )){
    ddf <- tibble(date = NA)
  } else {
    ddf <-  tibble( date = seq( from = startdate, to = enddate, by = freq ) )
    if (noleap) ddf <- ddf %>%
        dplyr::filter(
          !(lubridate::month(date) == 2 & lubridate::mday(date) == 29)
          )
  }

  return( ddf )

}
