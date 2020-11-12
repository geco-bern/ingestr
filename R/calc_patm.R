#' Calculates atmospheric pressure
#'
#' Calculates atmospheric pressure as a function of elevation, by default assuming 
#' standard atmosphere (101325 Pa at sea level)
#'
#' @param elv Elevation above sea-level (m.a.s.l.)
#' @param patm0 (Optional) Atmospheric pressure at sea level (Pa), defaults to 101325 Pa.
#'
#' @details The elevation-dependence of atmospheric pressure is computed by 
#' assuming a linear decrease in temperature with elevation and a mean 
#' adiabatic lapse rate (Berberan-Santos et al., 1997):
#' \deqn{
#'    p(z) = p0 ( 1 - Lz / TK0) ^ ( g M / (RL) )
#' }
#' where \eqn{z} is the elevation above mean sea level (m, argument \code{elv}), 
#' \eqn{g} is the gravity constant (9.80665 m s-2), \eqn{p0} is the atmospheric 
#' pressure at 0 m a.s.l. (argument \code{patm0}, defaults to 101325 Pa), 
#' \eqn{L} is the mean adiabatic lapse rate (0.0065 K m-2), 
#' \eqn{M} is the molecular weight for dry air (0.028963 kg mol-1), 
#' \eqn{R} is the universal gas constant (8.3145 J mol-1 K-1), and \eqn{TK0}
#' is the standard temperature (298.15 K, corresponds to 25 deg C).
#'
#' @return A numeric value for \eqn{p}
#'
#' @examples print("Standard atmospheric pressure, in Pa, corrected for 1000 m.a.s.l.:")
#' print(calc_patm(1000))
#' 
#' @references  Allen, R. G., Pereira, L. S., Raes, D., Smith, M.: 
#'              FAO Irrigation and Drainage Paper No. 56, Food and 
#'              Agriculture Organization of the United Nations, 1998
#'
#' @export
#' 
calc_patm <- function( elv, patm0 = 101325 ){
  
  # Define constants:
  kTo <- 298.15    # base temperature, K (Prentice, unpublished)
  kL  <- 0.0065    # adiabiatic temperature lapse rate, K/m (Allen, 1973)
  kG  <- 9.80665   # gravitational acceleration, m/s^2 (Allen, 1973)
  kR  <- 8.3145    # universal gas constant, J/mol/K (Allen, 1973)
  kMa <- 0.028963  # molecular weight of dry air, kg/mol (Tsilingiris, 2008)
  
  # Convert elevation to pressure, Pa:
  patm <- patm0*(1.0 - kL*elv/kTo)^(kG*kMa/(kR*kL))
  
  return(patm)
}