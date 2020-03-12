#' Interpolates monthly to daily values
#'
#' Implements different methods to interpolate from monthly to daily values, including fitting a polynomial.
#' 
#' @param mval A vector of twelve numeric values for monthly values.
#' @param method A character string specifying the method for interpolation. Defaults to \code{"polynom"} for using a polynomial.
#' @param mval_prev The monthly value of the month before the twelve months for which values are provided by argument \code{mval}.
#' @param mval_next The monthly value of the month after the twelve months for which values are provided by argument \code{mval}.
#' @param leapyear A logical specifying whether interpolation is done for a leap year (with 366 days).
#'
#' @return A named list of data frames (tibbles) containing input data for each site is returned.
#' @export
#'
#' @examples \dontrun{}  
#' 
monthly2daily <- function( mval, method="polynom", mval_prev=mval[nmonth], mval_next=mval[1], leapyear=FALSE ){
  
  # mval <- 20*sin( seq(0, 2*pi, 2*pi/11)-0.5*pi)
  # mval_prev <- mval[12]
  # mval_next <- mval[1]
  
  if (leapyear){
    ndaymonth <- c(31,29,31,30,31,30,31,31,30,31,30,31)
  } else {
    ndaymonth <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  }
  nmonth <- length(ndaymonth)
  dval <- rep(NA,sum(ndaymonth))
  
  if (method=="polynom"){
    
    # Starting conditons of december in previous year
    startt <- -30.5            # midpoint between Nov-Dec of previous year
    endt <- 0.5                # midpoint between Dec-Jan of this year
    dt <- 31.0                 # number of Dec days
    lastmonthtemp <- mval_prev # Dec mean temperature
    day <- 0                   # initialisation of this years days
    
    for (month in 1:nmonth){
      dtold <- dt
      dt <- (ndaymonth[month])
      startt <- endt
      endt <- endt + dt
      if (month<nmonth) {
        dtnew <- (ndaymonth[month+1])
        nextmonthtemp <- mval[month+1]
      } else {
        dtnew <- (ndaymonth[1])
        nextmonthtemp <- mval_next
      }
      
      starttemp <- (mval[month]*dt+lastmonthtemp*dtold)/(dt+dtold)
      endtemp <- (nextmonthtemp*dtnew+mval[month]*dt)/(dtnew+dt)
      deltatemp <- endtemp-starttemp
      
      # calculate vars for a,b,c coefficients in polynom y <- ax^2 +bx + c
      d2t <- endt^2.0 - startt^2.0
      d3t <- endt^3.0 - startt^3.0
      
      # Take a sheet of paper and try solve the polynom, well here is the outcome
      polya <- (mval[month]*dt - deltatemp*d2t/dt/2.0 - starttemp*dt + deltatemp*startt) / (d3t/3.0 - d2t^2.0/dt/2.0 - dt*startt^2.0 + startt*d2t)
      polyb <- deltatemp/dt - polya*(startt+endt)
      polyc <- starttemp - polya*startt^2.0 - polyb*startt
      
      # calculate daily values with the polynom function
      for (d in 1:ndaymonth[month]) {
        day <- day + 1
        dval[day] <- polya*(day)^2.0 + polyb*(day) + polyc
      }
      lastmonthtemp <- mval[month]
    }
    
    # calculate monthly means after interpolation - not absolutely identical to input
    mtempint <- rep(NA,nmonth)
    day <- 0
    for (m in 1:nmonth){
      mtempint[m] <- 0.0
      for (d in 1:ndaymonth[m]){
        day <- day + 1
        mtempint[m] <- mtempint[m]+dval[day]/(ndaymonth[m])
      }
    }
    
  } else if (method=="step"){
    
    dval[] <- rep( mval, times=ndaymonth )
    
  } else {
    print( "Method (2nd argument) not valid." )
  }
  
  return(dval) 
  
}


##-----------------------------------------------------------
## fills gaps (NAs) by (1.) linear interpolation, (2.) extending first/last to head/tail
##-----------------------------------------------------------
fill_gaps <- function( vec, is.prec = FALSE ){
  
  xvals <- seq(length(vec))
  
  if ( is.prec ){
    ## assume precipitation = 0 where missing
    if (any(is.na(vec))){
      vec[ is.na(vec) ] <- 0.0
    }
    
  } else {
    ## linear approximation
    if ( any(is.na(vec)) && any(!is.na(vec)) ){
      vec <- approx( xvals, vec, xout=xvals )$y
    }
    
    ## extend to missing in head and tail
    if ( any(is.na(vec))  && any(!is.na(vec)) ){
      for (idx in seq(length(vec))){
        if ( any( is.na( tail( vec, n=idx ) ) ) && any( !is.na( tail( vec, n=(idx+1) ) ) ) ){
          if (length(vec[ (length(vec)-idx) ])>0){
            vec[ (length(vec)-idx+1):length(vec) ] <- vec[ (length(vec)-idx) ]
          } else {
            vec[ (length(vec)-idx+1):length(vec) ] <- NA
          }
          break
        }
      }
    }  
    
  }
  
  return( vec )
  
}