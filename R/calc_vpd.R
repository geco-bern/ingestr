#' Calculate vapour pressure deficit from relative humidity
#'
#' Follows Abtew and Meleese (2013), Ch. 5 Vapor Pressure Calculation Methods,
#' in Evaporation and Evapotranspiration
#'
#' @param qair Air specific humidity (g g-1)
#' @param eact Water vapour pressure (Pa)
#' @param tc temperature, deg C
#' @param tmin (optional) min daily air temp, deg C 
#' @param tmax (optional) max daily air temp, deg C
#' @param patm Atmospehric pressure (Pa)
#' @param elv Elevation above sea level (m) (Used only if \code{patm} is missing 
#' for calculating it based on standard sea level pressure)
#'
#' @return vapor pressure deficit (Pa)
#' @export
#' 
calc_vpd <- function(
  qair = NA,
  eact = NA,
  tc = NA,
  tmin = NA,
  tmax = NA,
  patm = NA,
  elv = NA
) {
  
  ##-----------------------------------------------------------------------
  ## Ref:      Eq. 5.1, Abtew and Meleese (2013), Ch. 5 Vapor Pressure 
  ##           Calculation Methods, in Evaporation and Evapotranspiration: 
  ##           Measurements and Estimations, Springer, London.
  ##             vpd = 0.611*exp[ (17.27 tc)/(tc + 237.3) ] - ea
  ##             where:
  ##                 tc = average daily air temperature, deg C
  ##                 eact  = actual vapor pressure, Pa
  ##-----------------------------------------------------------------------

  ## calculate atmopheric pressure (Pa) assuming standard conditions at sea level (elv=0)
  # if (is.na(elv) && is.na(patm) && is.na(eact)){
  #   
  #   warning("calc_vpd(): Either patm or elv must be provided if eact is not given.")
  #   vpd <- NA
  #   
  # } else {
    
    # if (is.na(eact)){
      patm <- ifelse(is.na(patm),
                     ingestr::calc_patm(elv),
                     patm)
    # }
    
    ## Calculate VPD as mean of VPD based on Tmin and VPD based on Tmax if they are availble.
    ## Otherwise, use just tc for calculating VPD.
    vpd <- ifelse(
      !is.na(tmin) && !is.na(tmax),
      (calc_vpd_inst( qair=qair, eact=eact, tc=tmin, patm=patm) +
         calc_vpd_inst( qair=qair, eact=eact, tc=tmax, patm=patm))/2,
      calc_vpd_inst( qair=qair, eact=eact, tc=tc, patm=patm)
    )
  # }
  return( vpd )
}

#' Calculate instantenous VPD from ambient conditions
#' 
#' Follows Abtew and Meleese (2013), Ch. 5 Vapor Pressure Calculation Methods,
#' in Evaporation and Evapotranspiration
#' 
#' @param qair Air specific humidity (g g-1)
#' @param eact Water vapour pressure (Pa)
#' @param tc temperature, deg C
#' @param patm Atmospehric pressure (Pa)
#' @param elv Elevation above sea level (m) (Used only if \code{patm} is missing 
#' for calculating it based on standard sea level pressure)
#'
#' @return instantenous VPD from ambient conditions
#' @export

calc_vpd_inst <- function(
  qair=NA,
  eact=NA, 
  tc=NA,
  patm=NA,
  elv=NA
) {
  
  ##-----------------------------------------------------------------------
  ## Ref:      Eq. 5.1, Abtew and Meleese (2013), Ch. 5 Vapor Pressure 
  ##           Calculation Methods, in Evaporation and Evapotranspiration: 
  ##           Measurements and Estimations, Springer, London.
  ##             vpd = 0.611*exp[ (17.27 tc)/(tc + 237.3) ] - ea
  ##             where:
  ##                 tc = average daily air temperature, deg C
  ##                 eact  = actual vapor pressure, Pa
  ##-----------------------------------------------------------------------

  # if actual vapour pressure (eact) is not available, calculate it from
  # specific humidity
  eact <- ifelse(
    is.na(eact),
    calc_eact(qair, patm),
    eact
  )
  
  ## calculate saturation water vapour pressure in Pa
  esat <- 611.0 * exp( (17.27 * tc)/(tc + 237.3) )
  
  ## calculate VPD in units of Pa
  vpd <- ( esat - eact )    
  
  ## this empirical equation may lead to negative values for VPD
  ## (happens very rarely). assume positive...
  vpd <- ifelse(
    vpd < 0,
    0,
    vpd
  )
  
  return( vpd )
}

calc_eact <- function(qair, patm){
  
  # kTo <- 288.15   # base temperature, K (Prentice, unpublished)
  kR  <- 8.3143   # universal gas constant, J/mol/K (Allen, 1973)
  kMv <- 18.02    # molecular weight of water vapor, g/mol (Tsilingiris, 2008)
  kMa <- 28.963   # molecular weight of dry air, g/mol (Tsilingiris, 2008)
  
  ## calculate the mass mixing ratio of water vapor to dry air (dimensionless)
  wair <- qair / (1 - qair)
  
  ## calculate water vapor pressure 
  rv <- kR / kMv
  rd <- kR / kMa
  eact <- patm * wair * rv / (rd + wair * rv)
  
  return(eact)
}