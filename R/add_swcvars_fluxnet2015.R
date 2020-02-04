add_swcvars_fluxnet2015 <- function( sitename, fluxnet=NA, outdir="./" ){

  # ## XXX debug------------------------------------------
  # sitename <- "CH-Oe1"
  # fluxnet <- fluxnet
  # outdir <- "/alphadata01/bstocker/data/fluxnet_sofun/"
  # ##----------------------------------------------------

  syshome <- Sys.getenv( "HOME" )

  ddf <- fluxnet[[ sitename ]]$ddf
  print(paste("gap-filling for site", sitename))
  
  #---------------------------------------------------------
  # Gap-fill ET using a neural network with inputs: ppfd, temp, prec, vpd, (fapar)
  #---------------------------------------------------------
  predictors <- c("temp", "ppfd", "vpd", "aet_splash")
  
  data            <- ddf$inp
  data$le_f_mds   <- ddf$obs$le_f_mds  # take le_f_mds not le_f_mds_good because the latter has more gaps. this makes use of FLUXNET own gap-filling.
  data$aet_splash <- ddf$s11$aet

  if (any(is.na(data$le_f_mds))){

    ## some predictors may be NA. Approximate by linear interpolation.
    for (ipred in predictors){
      ## if first value is NA, fill with second
      if ( is.na(data[[ ipred ]][1]) ) { data[[ ipred ]][1] <- data[[ ipred ]][2] }

      ## if last value is NA then fill with second-last
      if ( is.na(data[[ ipred ]][nrow(data)]) ) { data[[ ipred ]][nrow(data)] <- data[[ ipred ]][nrow(data)-1] }

      ## fill NAs in between time series
      data[[ ipred ]][ which(is.na(data[[ ipred ]])) ] <- approx( data$year_dec, data[[ ipred ]] )$y[ which(is.na(data[[ ipred ]])) ]

    }

    ## some weird days still need some fixing
    data$prec[ is.na(data$prec) ] <- 0.0

    avl <- any(!is.na(data$le_f_mds))
    #---------------------------------------------------------
    if (avl) { data <- gapfill_nn( data, predictors, "le_f_mds", package="caret" ) }
    #---------------------------------------------------------

  } else {
    avl <- TRUE
  }
  # print(apply(data, 2, FUN = function (x) sum(is.na(x))))


  #---------------------------------------------------------
  # Run simple box model to get soil moisture
  #---------------------------------------------------------
  # soilm_from_et <- calc_soilm( data$prec, data$le_f_mds * ddf$s11$econ * 1e-12 * 1e3 )
  #                                          |----- J ------|   |-- m J-1 ---|
  #                                          |----- J ------|   |---- mm J-1 ----|
  #                                          |------------- mm ------------------|
  # constant energy-to-water conversion
  if (avl) { soilm_from_et            <- calc_soilm( data$prec, data$le_f_mds / 2.26476e6, method="bucket" ) } # 2.26e6 is in kJ / m = J / mm
  # soilm_from_et_keenan     <- calc_soilm( data$prec, data$le_f_mds / 2.26476e6, method="keenan" ) # 2.26e6 is in kJ / m = J / mm
  if (avl) { soilm_from_et_orthbucket <- calc_soilm( data$prec, data$le_f_mds / 2.26476e6, method="orthbucket" ) } # 2.26e6 is in kJ / m = J / mm


  #---------------------------------------------------------
  # Attach ET-data-driven soil moisture to dataframe
  #---------------------------------------------------------
  if (avl) { 
    fluxnet[[ sitename ]]$ddf$swc_by_etobs <- cbind( dplyr::select( data, year, moy, dom, year, year_dec, le_f_mds ), soilm_from_et, soilm_from_et_orthbucket )
    fluxnet[[ sitename ]]$ddf$obs$le_f_mds_mygapfilled <- data$le_f_mds
  } else {
    fluxnet[[ sitename ]]$swc_by_etobs <- NA
  }

  #---------------------------------------------------------
  # save data, now complemented with soil moisture data
  #---------------------------------------------------------
  filn <- paste( outdir, "modobs_fluxnet2015_", sitename, ".Rdata", sep="" )
  print( paste("saving data WITH SWC data to file", filn) )
  out <- fluxnet[[ sitename ]] 
  save( out, file=filn )

  return( fluxnet )

}
