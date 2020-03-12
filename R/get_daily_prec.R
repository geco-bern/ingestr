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
#' @export
#'
#' @examples \dontrun{}  
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