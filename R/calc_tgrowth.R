#' Calculate growth temperature
#'
#' The growth temperature is estimated by approximating the 
#' diurnal temperature cycle  with a sine curve, where daylight hours are 
#' determined by the day-of-year and latitude following (Jones, 2013).
#'
#' @param tmin Minimum daily temperature (degrees Celsius)
#' @param tmax Maximum daily tempearture (degrees Celsius)
#' @param lat Latitude (degrees)
#' @param doy Day of year (integer between 1 and 365)
#'
#' @return Growth temperature (degrees Celsius), a numeric value. Growth 
#' temperature is calculated as
#' 
#' \deqn{
#' T_g = T_\text{max} \left( 1/2 + (1 – x^2)^{1/2}/(2 \cos^{–1} x) \right) 
#'     + T_\text{min} \left( 1/2 – (1 – x^2)^{1/2}/(2 \cos^{–1} x) \right)
#' }
#' with
#' \deqn{
#' x = – \tan \lambda \; \tan \delta 
#' }
#' where \eqn{\lambda} is latitude and \eqn{\delta} is the solar declination
#' angle. The solar declination angle is calculated as described in Davis et
#' al., 2017.
#' 
#' @references  
#' Davis, T. W. et al. Simple process-led algorithms for simulating habitats 
#' (SPLASH v.1.0): robust indices of radiation, evapotranspiration and 
#' plant-available moisture. Geosci. Model. Dev. 10, 689–708 (2017).
#' 
#' Jones, H. G. (2013). Plants and microclimate: a quantitative approach to 
#' environmental plant physiology. In Cambridge University Press.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#'   tgrowth <- calc_tgrowth(
#'    tmin = 10.0,
#'    tmax = 30.0,
#'    lat = 23.5,
#'    doy = 180 )
#' }

calc_tgrowth <- function(tmin, tmax, lat, doy){

	# As described in Peng et al. (submitted), the growth temperature $T_g$ 
	# can be estimated monthly by approximating the diurnal temperature cycle 
	# with a sine curve, where daylight hours are determined by month and latitude:
	# $$
	# T_g = T_\text{max} \left( 1/2 + (1 – x^2)^{1/2}/(2 \cos^{–1} x) \right) 
	#     + T_\text{min} \left( 1/2 – (1 – x^2)^{1/2}/(2 \cos^{–1} x) \right), \\
	# x = – \tan \lambda \; \tan \delta 
	# $$
	# where $\lambda$ is latitude and $\delta$ is the monthly average solar 
	# declination (Jones, 2013). Monthly values of $T_g$ can then be averaged 
	# over the thermal growing season, i.e., months with $T_g > 0^\circ$C. 


	# calculate solar declination angle
	out_berger <- get_berger_tls(doy)

	xx <- (-1.0) * tan(lat * pi / 180) * tan(out_berger$decl_angle * pi / 180)
  tgrowth <-  tmax * ( 0.5 + ((1 - xx^2)^(0.5))/(2 * acos(xx))) 
           +  tmin * ( 0.5 - ((1 - xx^2)^(0.5))/(2 * acos(xx)))

	return(tgrowth)

}

get_berger_tls <- function( day ){
  #----------------------------------------------------------------   
  # Returns true anomaly and true longitude for a given day
  # Reference: Berger, A. L. (1978), Long term variations of daily 
  # insolation and quaternary climatic changes, J. Atmos. Sci., 35, 
  # 2362-2367.
  #----------------------------------------------------------------   
  # arguments:
  # day: day of the year
  # komega: longitude of perihelion for 2000 CE, degrees (Berger, 1978)

	ndayyear <- 365

  # longitude of perihelion for 2000 CE, degrees (Berger, 1978)
	komega <- 283.0

  # eccentricity for 2000 CE (Berger, 1978)
  ke <- 0.0167
  
  # obliquity for 2000 CE, degrees (Berger, 1978)
  keps <- 23.44

  # Variable substitutes:
  xee <- ke^2 
  xec <- ke^3
  xse <- sqrt(1.0 - xee)

  # Mean longitude for vernal equinox:
  tmp1 <- (ke/2.0 + xec/8.0)*(1.0 + xse)*dgsin(komega)
  tmp2 <- xee/4.0*(0.5 + xse)*dgsin(2.0*komega)
  tmp3 <- xec/8.0*(1.0/3.0 + xse)*dgsin(3.0*komega)
  xlam <- tmp1 - tmp2 + tmp3
  xlam <- degrees(2.0*xlam)

  # Mean longitude for day of year:
  dlamm <- xlam + (day - 80.0)*(360.0/ndayyear)

  # Mean anomaly:
  anm <- dlamm - komega
  ranm <- radians(anm)

  # True anomaly:
  ranv <- (ranm + (2.0*ke - xec/4.0)*sin(ranm) + 5.0/4.0*xee*sin(2.0*ranm) + 13.0/12.0*xec*sin(3.0*ranm))
  anv <- degrees(ranv)

  out <- list()

  # True longitude:
  out$lambda <- anv + komega
  if (out$lambda < 0.0){
    out$lambda <- out$lambda + 360.0
  } else if (out$lambda > 360.0){
    out$lambda <- out$lambda - 360.0
  }

  # True anomaly:
  out$nu <- (out$lambda - komega)
  if (out$nu < 0.0){
    out$nu <- out$nu + 360.0
  }

  # Calculate declination angle (delta), degrees
  # Woolf (1968)
  out$decl_angle <- degrees( asin( dgsin( out$lambda ) * dgsin( keps ) ) )

  return(out)
}


dgsin <- function(x){
  #----------------------------------------------------------------   
  # Calculates the sinus of an angle given in degrees.
  #----------------------------------------------------------------
  sin(x*pi/180.0)
}

degrees <- function(x){
  #----------------------------------------------------------------   
  # Returns corresponding degrees if x is given in radians
  #----------------------------------------------------------------
  x*180.0/pi
}

radians <- function(x){
  #----------------------------------------------------------------   
  # Returns corresponding radians if x is given in degrees
  #----------------------------------------------------------------
  x*pi/180.0
}

