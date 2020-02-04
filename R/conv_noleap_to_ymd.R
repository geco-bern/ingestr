#' Converts no-leapyear dates to lubridate::ymd object
#'
#' Converts timesteps from NetCDF file having 365 or 360 days calendar to POSIXt class timevector. 
#' The reason for this function is, that cannot simply add the timevector to the POSIX date we wish, 
#' as this would lead to complete nonsense data! Therefore we add the years and days seperately and 
#' leave out  may 31st, july 31st aug 31st, oct 31st and dec 31st (360 days) and march 31st if leapyear. 
#' For 365 days calendar we leave out feb 29th if being leap year.
#' Taken from https://github.com/cran/wux/blob/master/R/ReadNetCdfTimeData.R
#'
#' @param time  A vector with timesteps, which once was the time variable of a NetCDF file. 
#' These timesteps have to be on DAILY basis!
#' @param origin POSIXt or lubridate \code{ymd} object defining the date of zero-day of the time 
#' vector (argument \code{time})
#' @param calendar_days Character or numeric. Calendar type. Can be "365" or "360".
#'
#' @return lubridate::ymd vector of dates within NetCDF file, having same length as
#' NetCDF time dimension.
#' @export
#' 
conv_noleap_to_ymd <- function(time, origin, calendar_days=365 ) { 

  ## convert arguments to useful format
  calendar_days <- as.numeric(calendar_days)
  origin_date <- lubridate::ymd(origin)
  
  ## get vector containing the "years origin" and "day in that year"
  years_since_origin_numeric <- time / calendar_days   # numerical value of years since origin
  year_since_origin_integer <- floor(years_since_origin_numeric)
  
  time_year <- origin_date + lubridate::years(year_since_origin_integer)
  
  ## boolean vector indicating if year is leapyear
  is_leapyear <- lubridate::leap_year(time_year)

  ## getting days within this year
  time_day_of_year <- time %% calendar_days  ## OLD calculation: (years_since_origin_numeric - year) * calendar_days
  
  ## now comes the crucial part:
  ## we shift the days for 360days calendar and 365days calendar so, that:
  ## case 360days: all 31st (except jan and mar) disappear (for leapyears all
  ##               except jan)
  ## case 365days: for leapyears feb 29th disappears
  if (calendar_days == 360){
    time_day_of_year[time_day_of_year >= 150 & !is_leapyear] <- time_day_of_year[time_day_of_year >= 150 & !is_leapyear] + 1#may 31th
    time_day_of_year[time_day_of_year >= 211 & !is_leapyear] <- time_day_of_year[time_day_of_year >= 211 & !is_leapyear] + 1#jul 31th
    time_day_of_year[time_day_of_year >= 242 & !is_leapyear] <- time_day_of_year[time_day_of_year >= 242 & !is_leapyear] + 1#aug 31th
    time_day_of_year[time_day_of_year >= 303 & !is_leapyear] <- time_day_of_year[time_day_of_year >= 303 & !is_leapyear] + 1#oct 31th
    time_day_of_year[time_day_of_year >= 364 & !is_leapyear] <- time_day_of_year[time_day_of_year >= 364 & !is_leapyear] + 1#dec 31th

    time_day_of_year[time_day_of_year >= 90  & is_leapyear] <- time_day_of_year[time_day_of_year >= 90  & is_leapyear] + 1 #mar 31th
    time_day_of_year[time_day_of_year >= 151 & is_leapyear] <- time_day_of_year[time_day_of_year >= 151 & is_leapyear] + 1 #may 31th
    time_day_of_year[time_day_of_year >= 212 & is_leapyear] <- time_day_of_year[time_day_of_year >= 212 & is_leapyear] + 1 #jul 31th
    time_day_of_year[time_day_of_year >= 243 & is_leapyear] <- time_day_of_year[time_day_of_year >= 243 & is_leapyear] + 1 #aug 31th
    time_day_of_year[time_day_of_year >= 304 & is_leapyear] <- time_day_of_year[time_day_of_year >= 304 & is_leapyear] + 1 #oct 31th
    time_day_of_year[time_day_of_year >= 365 & is_leapyear] <- time_day_of_year[time_day_of_year >= 365 & is_leapyear] + 1 #dec 31th
  }
  else if (calendar_days == 365){
    time_day_of_year[time_day_of_year >= 60  & is_leapyear] <- time_day_of_year[time_day_of_year >= 60  & is_leapyear] + 1 #feb 29th
  }
  else {
    stop("INVALID CALENDAR TYPE IN conv_noleap_to_ymd")
  }

  ## add to ymd object
  time_date_out <- time_year + lubridate::days(time_day_of_year)

  return( time_date_out )
}
