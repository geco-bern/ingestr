##-----------------------------------------------------------
## Returns a list of two data frames, one with data at original
## modis dates (df), and one interpolated to all days (ddf).
##-----------------------------------------------------------
ingest_gee <- function( df_siteinfo, start_date,
                        end_date, settings_sims, settings_input,
                        overwrite_raw, overwrite_nice, overwrite_csv,
                        band_var, band_qc, prod, prod_suffix, varnam, productnam, scale_factor, 
                        period, do_plot_interpolated, python_path, gee_path){
  
  ##---------------------------------------------
  ## Define names
  ##---------------------------------------------
  sitename <- df_siteinfo$sitename[1]
  df_siteinfo <- slice(df_siteinfo, 1)
  #print(paste("getting fapar for site", sitename))
  
  dirnam_daily_csv <- paste0(settings_input$path_input, "sitedata/fapar/", sitename)
  dirnam_nice_csv <- settings_input[paste0("path_", stringr::str_replace(productnam ,"_gee", ""))] %>% unlist() %>% unname()
  dirnam_raw_csv <- paste0( dirnam_nice_csv, "/raw/" )
  
  if (!dir.exists(dirnam_daily_csv)) system( paste( "mkdir -p ", dirnam_daily_csv ) )
  if (!dir.exists(dirnam_nice_csv)) system( paste( "mkdir -p ", dirnam_nice_csv ) )
  if (!dir.exists(dirnam_raw_csv)) system( paste( "mkdir -p ", dirnam_raw_csv ) )
  
  filnam_daily_csv <- paste0( dirnam_daily_csv, "/fapar_daily_", sitename, ".csv" )
  filnam_nice_csv <- paste0( dirnam_nice_csv, "/", varnam, "_", productnam, "_", sitename, "_subset.csv" )
  filnam_raw_csv <- paste0( dirnam_raw_csv, sitename, "_", prod_suffix, "_gee_subset.csv" )
  
  out <- list()
  do_continue <- TRUE
  
  ## Save error code (0: no error, 1: error: file downloaded bu all data is NA, 2: file not downloaded)
  df_error <- tibble()
  
  if (file.exists(filnam_daily_csv) && !overwrite_csv){
    ##---------------------------------------------
    ## Read daily interpolated and gapfilled
    ##---------------------------------------------
    out$ddf <- readr::read_csv( filnam_daily_csv, col_types = cols() )
    
  } else {
    
    if (file.exists(filnam_nice_csv) && file.exists(filnam_daily_csv) && !overwrite_nice){
      ##---------------------------------------------
      ## Read nicely formatted 8-daily      
      ##---------------------------------------------
      out$df  <- readr::read_csv( filnam_nice_csv, col_types = cols() )
      out$ddf <- readr::read_csv( filnam_daily_csv, col_types = cols() )
      
    } else {
      
      if (file.exists(filnam_raw_csv) && !overwrite_raw){
        ## Raw downloaded file will be read separately
        # print( paste( "File exists already:", filnam_modis_raw_csv ) )
        # print(paste("site", sitename))
        
      } else {
        ##---------------------------------------------
        ## Download via Google Earth Engine using the python function
        ##---------------------------------------------
        path_info <- paste0(filnam_raw_csv, "info_lonlat.csv")
        write.csv( dplyr::select( df_siteinfo, site = sitename, latitude = lat, longitude = lon), file=path_info, row.names=FALSE )
        
        start = Sys.time()
        system(sprintf("%s %s/gee_subset.py -p %s -b %s %s -s %s -e %s -f %s -d %s -sc 30",
                       python_path,
                       gee_path,
                       prod,
                       band_var,
                       band_qc,
                       start_date,
                       end_date,
                       path_info,
                       filnam_raw_csv
        ), wait = TRUE)
        
        end = Sys.time()
        proc_time = as.vector(end - start)
        print( paste( "... completed in", format( proc_time, digits = 3), "sec" ) )
        
        ## Raw downloaded data is saved to file
        print( paste( "raw data file written:", filnam_raw_csv ) )
        
      }
      
      ##--------------------------------------------------------------------
      ## Read raw data and create a data frame holding the complete time series
      ## Note: 'date', 'doy', 'dom', etc. refer to the date, centered within 
      ## each N-day period. 'date_start' refers to the beginning of each 
      ## N-day period.
      ##--------------------------------------------------------------------
      if (file.exists(filnam_raw_csv)){
        
        df <- readr::read_csv( filnam_raw_csv, col_types = cols() ) %>%
          dplyr::mutate(  date_start = ymd(date) ) %>%
          dplyr::mutate(  date = date_start + days( as.integer(period/2) ),
                          doy = yday(date),
                          year = year(date)
          ) %>% 
          dplyr::mutate( ndayyear = ifelse( leap_year( date ), 366, 365 ) ) %>%
          dplyr::mutate( year_dec = year(date) + (yday(date)-1) / ndayyear ) %>% 
          dplyr::select( -longitude, -latitude, -product, -ndayyear )
        
        ## Apply scale factor, specific for each product
        if (any(!is.na(df[[band_var]]))){
          df[[band_var]] <- df[[band_var]] * scale_factor
        } else {
          do_continue <- FALSE
        }
        
      } else {
        
        print( paste( "WARNING: RAW DATA FILE NOT FOUND FOR SITE:", sitename ) )
        df_error <- df_error %>% bind_rows( tibble( mysitename=sitename, error=2 ) ) 
        out <- NA
        do_continue <- FALSE
        
      }      
      
      if (do_continue){
        ##---------------------------------------------
        ## Clean (gapfill and interpolate) full time series data to 8-days, daily, and monthly
        ##--------------------------------------------------------------------
        print("gapfilling and interpolating to daily ...")
        out <- gapfill_modis(
          df,
          sitename, 
          year_start = df_siteinfo$year_start,
          year_end   = df_siteinfo$year_end,
          qc_name = band_qc, 
          prod = prod_suffix,
          splined_fapar = settings_input$splined_fapar,
          do_interpolate = TRUE,
          do_plot_interpolated = do_plot_interpolated,
          dir = settings_input$path_input
        )
        
        ##---------------------------------------------
        ## save nicely formatted 8-daily to file
        ##---------------------------------------------
        readr::write_csv( out$df,  path=filnam_nice_csv )
        readr::write_csv( out$ddf, path=filnam_daily_csv )
        
      } else {
        ddf_out <- init_dates_dataframe(
          lubridate::year(start_date),
          lubridate::year(end_date)
        ) %>% 
          mutate(modisvar_interpol = NA)
      }
      
    }
  }
  
  if (do_continue){
    ##---------------------------------------------
    ## Write SOFUN-formatted input
    ##---------------------------------------------
    ## get mean seasonal cycle. This is relevant for years where no MODIS data is available.
    df_meandoy <- out$ddf %>% 
      group_by( doy ) %>% 
      summarise( meandoy = mean( modisvar_interpol , na.rm=TRUE ) ) 
    
    ## in separate formatted file 
    ddf_out <- out$ddf
    
    ## fill gaps with mean seasonal cycle (for pre-MODIS years, entire year is mean seasonal cycle)
    if (nrow(ddf_out)==0){
      ddf_out <- init_dates_dataframe( df_siteinfo$year_start, df_siteinfo$year_end ) %>% 
        mutate( modisvar_interpol = NA ) %>% 
        dplyr::filter( !( month(date)==2 & mday(date)==29 ) ) %>% 
        mutate( doy=yday(date) )
    }
    ddf_out <- ddf_out %>% left_join( df_meandoy, by="doy" )
    
    ## fill gaps with mean by DOY/MOY
    ddf_out$modisvar_interpol[ which( is.na( ddf_out$modisvar_interpol ) ) ] <- ddf_out$meandoy[ which( is.na( ddf_out$modisvar_interpol ) ) ]
    
  }
  
  df_error <- df_error %>% bind_rows( tibble( mysitename=sitename, error=0 ) ) 
  
  return(ddf_out) 
}


gapfill_modis <- function( df, sitename, year_start, year_end, qc_name, prod, splined_fapar, do_interpolate=FALSE, do_plot_interpolated=FALSE, dir = "./" ){
  ##--------------------------------------
  ## Returns data frame containing data 
  ## (and year, moy, doy) for all available
  ## months. Interpolated to mid-months
  ## from original 16-daily data.
  ##--------------------------------------
  
  # require( signal )  ## for sgolayfilt, masks filter()
  
  ##--------------------------------------
  ## CLEAN AND GAP-FILL
  ##--------------------------------------
  if (prod=="MOD13Q1"){
    ##--------------------------------------
    ## This is for MOD13Q1 EVI data downloaded from Google Earth Engine with gee_subset
    ## USED AS MODIS EVI GEE for P-model
    ##--------------------------------------
    ## QC interpreted according to https://explorer.earthengine.google.com/#detail/MODIS%2F006%2FMOD13Q1:
    ## 0: Good data, use with confidence
    ## 1: Marginal data, useful but look at detailed QA for more information
    ## 2: Pixel covered with snow/ice
    ## 3: Pixel is cloudy
    ##--------------------------------------
    df <- df %>%  mutate( good_quality  = ifelse( SummaryQA %in% c(0, 1, 2), TRUE, FALSE ) ) %>%
      rename( modisvar = EVI )
    
    ## Plot effect of filtering steps
    if (do_plot_interpolated){
      dir <- paste0(dir, "/fig_fapar_gapfilling/")
      if (!dir.exists(dir)) system(paste0("mkdir ", dir))
      plotfiln <- paste0( dir, "evi_MOD13Q1gee_", sitename, ".pdf" )
      print( paste( "Gapfilling illustrated in:", plotfiln ) )
      pdf( plotfiln, width=15, height=6 )
      par(xpd=TRUE)
      with( df, plot( year_dec, modisvar, pch=16, col='black', main=sitename, ylim=c(0,1), xlab="year", ylab="MOD13Q1gee", las=1 ) )
      with( dplyr::filter( df, good_quality & modisvar > 0.0), points( year_dec, modisvar, pch=16, col='blue' ) )
      
      legend( "topleft", 
              c( "initial", "good_quality" ), 
              col=c("black", "blue" ), pch=16, bty="n", inset = c(0,-0.2)
      )
      
    }
    
    ## Actually filter
    df <- df %>% mutate( modisvar = ifelse( good_quality, modisvar, NA )  )  ## replace by NA for values to be filtered out
    # df <- df %>% dplyr::filter( good_quality )
    
    ##--------------------------------------
    ## replace missing values with mean by DOY (mean seasonal cycle)
    ##--------------------------------------
    ## get mean seasonal cycle
    ddf_meandoy <- df %>% group_by( doy ) %>% summarise( meandoy = mean( modisvar , na.rm=TRUE ) )
    
    ## attach mean seasonal cycle as column 'meandoy' to daily dataframe
    df <- df %>% left_join( ddf_meandoy, by="doy" ) %>%
      
      ## fill gaps at head and tail
      mutate( modisvar = ifelse( is.na(modisvar), meandoy, modisvar ) )
    
    
    ##--------------------------------------
    ## This is for data downloaded using RModisTools
    ##--------------------------------------
    
    # ##--------------------------------------
    # ## data cleaning
    # ##--------------------------------------
    # ## Replace data points with quality flag = 2 (snow covered) by 0
    # # df_gapfld$centre[ which(df_gapfld$centre_qc==2) ] <- max( min( df_gapfld$centre ), 0.0 )
    # df_gapfld$raw <- df_gapfld$centre
    # df_gapfld$centre[ which(df_gapfld$centre<0) ] <- NA
    
    # if (!is.null(df_gapfld$centre_qc)){
    #   ## Drop all data with quality flag 3, 1 or -1
    #   df_gapfld$centre[ which(df_gapfld$centre_qc==3) ]  <- NA  # Target not visible, covered with cloud
    #   df_gapfld$centre[ which(df_gapfld$centre_qc==1) ]  <- NA  # Useful, but look at other QA information
    #   df_gapfld$centre[ which(df_gapfld$centre_qc==-1) ] <- NA  # Not Processed
    # }
    
    # ## open plot for illustrating gap-filling
    # if (do_plot_interpolated) pdf( paste("fig/evi_fill_", sitename, ".pdf", sep="" ), width=10, height=6 )
    # if (do_plot_interpolated) plot( df_gapfld$year_dec, df_gapfld$raw, pch=16, col='black', main=sitename, ylim=c(0,1), xlab="year", ylab="MODIS EVI 250 m", las=1 )
    # left <- seq(2000, 2016, 2)
    # right <- seq(2001, 2017, 2)
    # if (do_plot_interpolated) rect( left, -99, right, 99, border=NA, col=rgb(0,0,0,0.2) )
    # if (do_plot_interpolated) points( df_gapfld$year_dec, df_gapfld$centre, pch=16, col='red' )
    
    # # ## Drop all data identified as outliers = lie outside 5*IQR
    # # df_gapfld$centre <- remove_outliers( df_gapfld$centre, coef=5 ) ## maybe too dangerous - removes peaks
    
    # ## add points to plot opened before
    # if (do_plot_interpolated) points( df_gapfld$year_dec, df_gapfld$centre, pch=16, col='blue' )
    
    # ##--------------------------------------
    # ## get LOESS spline model for predicting daily values (below)
    # ##--------------------------------------
    # idxs    <- which(!is.na(df_gapfld$centre))
    # myloess <- try( with( df_gapfld, loess( centre[idxs] ~ year_dec[idxs], span=0.01 ) ))
    # i <- 0
    # while (class(myloess)=="try-error" && i<50){
    #   i <- i + 1
    #   print(paste("i=",i))
    #   myloess <- try( with( df_gapfld, loess( centre[idxs] ~ year_dec[idxs], span=(0.01+0.002*(i-1)) ) ))
    # }
    # print("ok now...")
    
    # ##--------------------------------------
    # ## get spline model for predicting daily values (below)
    # ##--------------------------------------
    # spline <- try( with( df_gapfld, smooth.spline( year_dec[idxs], centre[idxs], spar=0.001 ) ) )
    
    # ## aggregate by DOY
    # agg <- aggregate( centre ~ doy, data=df_gapfld, FUN=mean, na.rm=TRUE )
    # if (is.element("centre_meansurr", names(df_gapfld))){
    #   agg_meansurr <- aggregate( centre_meansurr ~ doy, data=df_gapfld, FUN=mean, na.rm=TRUE )
    #   agg <- agg %>% left_join( agg_meansurr ) %>% dplyr::rename( centre_meandoy=centre, centre_meansurr_meandoy=centre_meansurr )
    # } else {
    #   agg <- agg %>% dplyr::rename( centre_meandoy=centre )      
    # }
    # df_gapfld <- df_gapfld %>% left_join( agg )
    
    # ##--------------------------------------
    # ## gap-fill with information from surrounding pixels - XXX CHANGED THIS FOR AMERIWUE: NO INFO FROM MEAN OF SURROUNDINGS USED XXX
    # ##--------------------------------------
    # idxs <- which( is.na(df_gapfld$centre) )
    # if (is.element("centre_meansurr", names(df_gapfld))){
    #   ## get current anomaly of mean across surrounding pixels w.r.t. its mean annual cycle
    #   df_gapfld$anom_surr    <- df_gapfld$centre_meansurr / df_gapfld$centre_meansurr_meandoy
    #   # df_gapfld$centre[idxs] <- df_gapfld$centre_meandoy[idxs] * df_gapfld$anom_surr[idxs]
    # } else {
    #   # df_gapfld$centre[idxs] <- df_gapfld$centre_meandoy[idxs]
    # }
    # if (do_plot_interpolated) with( df_gapfld, points( year_dec[idxs], centre[idxs], pch=16, col='green' ) )
    # if (do_plot_interpolated) legend("topright", c("modis", "outliers", "after bad values dropped and outliers removed", "added from mean of surrounding" ), col=c("black", "red", "blue", "green" ), pch=16, bty="n" )
    # # legend("topleft", c("R LOESS smoothing with span=0.01", "R smooth.spline"), col=c("red", "dodgerblue"), lty=1, bty="n" )
    
    
    # # points( df_gapfld$yr_dec_read[idxs],  df_gapfld$centre[idxs],  pch=16 )
    # # points( df_gapfld$yr_dec_read[-idxs], df_gapfld$centre[-idxs], pch=16, col='blue' )
    
    # # ## Gap-fill remaining again by mean-by-DOY
    # # idxs <- which( is.na(df_gapfld$centre) )
    # # df_gapfld$centre[idxs] <- df_gapfld$centre_meandoy[idxs]
    # # # points( df_gapfld$yr_dec_read[idxs], df_gapfld$centre[idxs], pch=16, col='red' )
    
    # # ## Gap-fill still remaining by linear approximation
    # # idxs <- which( is.na(df_gapfld$centre) )
    # # if (length(idxs)>1){
    # #   df_gapfld$centre <- approx( df_gapfld$year_dec[-idxs], df_gapfld$centre[-idxs], xout=df_gapfld$year_dec )$y
    # # }
    
    # # points( df_gapfld$yr_dec_read[idxs], df_gapfld$centre[idxs], pch=16, col='green' )
    # # lines( df_gapfld$yr_dec_read, df_gapfld$centre )
    # # dev.off()
    
  } else if (prod=="MCD15A3H"){
    ##--------------------------------------
    ## This is for MCD15A3H FPAR data downloaded from Google Earth Engine with gee_subset
    ## USED AS MODIS FPAR GEE for P-model
    ##--------------------------------------
    ## QC interpreted according to https://explorer.earthengine.google.com/#detail/MODIS%2F006%2FMCD15A3H:
    ## Bit 0: MODLAND_QC bits
    ##   0: Good quality (main algorithm with or without saturation)
    ##   1: Other quality (back-up algorithm or fill values)
    ## Bit 1: Sensor
    ##   0: Terra
    ##   1: Aqua
    ## Bit 2: Dead detector
    ##   0: Detectors apparently fine for up to 50% of channels 1, 2
    ##   1: Dead detectors caused >50% adjacent detector retrieval
    ## Bits 3-4: Cloud state
    ##   0: Significant clouds NOT present (clear)
    ##   1: Significant clouds WERE present
    ##   2: Mixed cloud present in pixel
    ##   3: Cloud state not defined, assumed clear
    ## Bits 5-7: SCF_QC
    ##   0: Main (RT) method used with no saturation, best result possible
    ##   1: Main (RT) method used with saturation, good and very usable
    ##   2: Main (RT) method failed due to bad geometry, empirical algorithm used
    ##   3: Main (RT) method failed due to problems other than geometry, empirical algorithm used
    ##   4: Pixel not produced at all, value couldn't be retrieved (possible reasons: bad L1B data, unusable MOD09GA data)
    ##--------------------------------------
    
    ## This is interpreted according to https://lpdaac.usgs.gov/sites/default/files/public/product_documentation/mod15_user_guide.pdf, p.9
    ## see mod15_user_guide.pdf
    
    df$qc_bitname <- sapply( seq(nrow(df)), function(x) as.integer( intToBits( df$FparLai_QC[x] )[1:8] ) %>% rev() %>% as.character() %>% paste( collapse="" )  )
    
    ## MODLAND_QC bits
    ## 0: Good  quality (main algorithm with or without saturation)
    ## 1: Other quality (backup  algorithm or fill values)
    df$qc_bit0 <- substr( df$qc_bitname, start=8, stop=8 )
    
    ## Sensor
    ## 0: Terra
    ## 1: Aqua
    df$qc_bit1 <- substr( df$qc_bitname, start=7, stop=7 )
    
    ## Dead detector
    ## 0: Detectors apparently  fine  for up  to  50% of  channels  1,  2
    ## 1: Dead  detectors caused  >50%  adjacent  detector  retrieval
    df$qc_bit2 <- substr( df$qc_bitname, start=6, stop=6 )
    
    ## CloudState
    ## 00 0  Significant clouds  NOT present (clear)
    ## 01 1  Significant clouds  WERE  present
    ## 10 2  Mixed cloud present in  pixel
    ## 11 3  Cloud state not defined,  assumed clear
    df$qc_bit3 <- substr( df$qc_bitname, start=4, stop=5 )
    
    ## SCF_QC (five level confidence score)
    ## 000 0 Main (RT) method used, best result possible (no saturation)
    ## 001 1 Main (RT) method used with saturation. Good, very usable
    ## 010 2 Main (RT) method failed due to bad geometry, empirical algorithm used
    ## 011 3 Main (RT) method failed due to problems other than geometry, empirical algorithm used
    ## 100 4 Pixel not produced at all, value couldn???t be retrieved (possible reasons: bad L1B data, unusable MOD09GA data)
    df$qc_bit4 <- substr( df$qc_bitname, start=1, stop=3 )
    
    df <- df %>%  mutate(  good_quality  = ifelse( qc_bit0=="0", TRUE, FALSE ),
                           terra         = ifelse( qc_bit1=="0", TRUE, FALSE ),
                           dead_detector = ifelse( qc_bit2=="1", TRUE, FALSE ),
                           CloudState    = ifelse( qc_bit3=="00", 0, ifelse( qc_bit3=="01", 1, ifelse( qc_bit3=="10", 2, 3 ) ) ),
                           SCF_QC        = ifelse( qc_bit4=="000", 0, ifelse( qc_bit4=="001", 1, ifelse( qc_bit4=="010", 2, ifelse( qc_bit4=="011", 3, 4 ) ) ) )
    ) %>%
      select( -qc_bitname, -FparLai_QC, -qc_bit0, -qc_bit1, -qc_bit2, -qc_bit3, -qc_bit4 ) %>% 
      rename( modisvar = Fpar )
    
    ## Plot effect of filtering steps
    if (do_plot_interpolated){
      dir <- paste0(dir, "/fig_fapar_gapfilling/")
      if (!dir.exists(dir)) system(paste0("mkdir ", dir))
      plotfiln <- paste0( dir, "fpar_MCD15A3H_v2_fill_", sitename, ".pdf" )
      print( paste( "Gapfilling illustrated in:", plotfiln ) )
      pdf( plotfiln, width=15, height=6 )
      par(xpd=TRUE)
      with( df, plot( year_dec, modisvar, pch=16, col='black', main=sitename, ylim=c(0,1), xlab="year", ylab="MCD15A3H", las=1 ) )
      with( dplyr::filter( df, CloudState!=1, modisvar!=1.0  ), points( year_dec, modisvar, pch=16, col='blue' ) ) # good_quality &  & SCF_QC!=4  & SCF_QC %in% c(0,1)
      
      # with( dplyr::filter( df, good_quality), points( year_dec, modisvar, pch=16, col='blue' ) )
      # with( dplyr::filter( df, good_quality & CloudState==0), points( year_dec, modisvar, pch=16, col='springgreen3' ) )
      # with( dplyr::filter( df, good_quality & CloudState==0 & !dead_detector ), points( year_dec, modisvar, pch=16, col='orchid' ) )
      # with( dplyr::filter( df, good_quality & CloudState==0 & !dead_detector & SCF_QC==0 ), points( year_dec, modisvar, pch=16, col='black' ) )
      legend( "topleft", 
              c("initial", "cleaned"
                # "good_quality, but not CloudState==0 and not(dead_detector) and not(SCF_QC==0)", 
                # "good_quality and CloudState==0, but not(dead_detector) and not(SCF_QC==0)", 
                # "good_quality and CloudState==0 and not(dead_detector), but not not(SCF_QC==0)",
                # "remaining"
              ), 
              col=c("black", "blue"
                    # ,"springgreen3", "orchid", "black" 
              ), pch=16, bty="n", inset = c(0,-0.2)
      )
    }
    
    ## Actually filter
    # df <- df %>% dplyr::filter( CloudState!=1 )  #  good_quality & CloudState!=1 & SCF_QC!=4  & CloudState==0 & !dead_detector & SCF_QC==0
    df <- df %>% mutate( modisvar = ifelse( CloudState!=1 , modisvar, NA )  ) %>%  ## replace by NA for values to be filtered out   & SCF_QC %in% c(0,1)
      
      ## don't believe the hype
      mutate( modisvar = ifelse( modisvar==1.0, NA, modisvar ) ) %>%  
      
      ## Drop all data identified as outliers = lie outside 5*IQR
      mutate( modisvar = remove_outliers( modisvar, coef=3 ) )  # maybe too dangerous - removes peaks
    
    ##--------------------------------------
    ## replace missing values with mean by DOY (mean seasonal cycle)
    ##--------------------------------------
    ## get mean seasonal cycle
    ddf_meandoy <- df %>% group_by( doy ) %>% summarise( meandoy = mean( modisvar , na.rm=TRUE ) )
    
    ## attach mean seasonal cycle as column 'meandoy' to daily dataframe
    df <- df %>% left_join( ddf_meandoy, by="doy" ) %>%
      
      ## fill gaps at head and tail
      mutate( modisvar = ifelse( is.na(modisvar), meandoy, modisvar ) )
    
    
  }  else if (prod=="MOD15A2"){
    ## MOD15A2 contains fpar
    
    ##--------------------------------------
    ## data cleaning
    ##--------------------------------------
    # ## Drop all data with quality flag != 0
    # if (!is.null(df_gapfld$centre_qc)){
    #   df_gapfld$centre[ which(df_gapfld$centre_qc!=0) ] <- NA
    # }
    
    ## no quality info available for fpar from trevor
    
    ## remove values that are above 1
    df_gapfld$centre <- replace( df_gapfld$centre,  df_gapfld$centre>1.0, NA )
    
    ## open plot for illustrating gap-filling
    if (do_plot_interpolated) pdf( paste("fig/fpar_fill_", sitename, ".pdf", sep="" ), width=10, height=6 )
    if (do_plot_interpolated) plot( modis$year_dec, modis$centre, pch=16, col='black', main=sitename, ylim=c(0,1), xlab="year", ylab="MODIS FPAR 1 km", las=1 )
    left <- seq(2000, 2016, 2)
    right <- seq(2001, 2017, 2)
    if (do_plot_interpolated) rect( left, -99, right, 99, border=NA, col=rgb(0,0,0,0.2) )
    if (do_plot_interpolated) points( df_gapfld$year_dec, df_gapfld$centre, pch=16, col='red' )
    
    # ## Drop all data identified as outliers = lie outside 5*IQR
    # df_gapfld$centre <- remove_outliers( df_gapfld$centre, coef=5 ) ## maybe too dangerous - removes peaks
    
    ## add points to plot opened before
    if (do_plot_interpolated) points( df_gapfld$year_dec, df_gapfld$centre, pch=16, col='springgreen3' )
    
    ##--------------------------------------
    ## get LOESS spline model for predicting daily values (below)
    ##--------------------------------------
    idxs <- which(!is.na(df_gapfld$centre))
    if (length(idxs)>0){
      myloess <- try( with( df_gapfld, loess( centre[idxs] ~ year_dec[idxs], span=0.01 ) ))
      i <- 0
      while (class(myloess)=="try-error" && i<50){
        i <- i + 1
        print(paste("i=",i))
        myloess <- try( with( df_gapfld, loess( centre[idxs] ~ year_dec[idxs], span=(0.01+0.002*(i-1)) ) ))
      }
      print("ok now...")
    } else {
      missing <- TRUE
    }
    
    ##--------------------------------------
    ## get spline model for predicting daily values (below)
    ##--------------------------------------
    if (!missing){
      spline <- try( with( df_gapfld, smooth.spline( year_dec[idxs], centre[idxs], spar=0.001 ) ))
    }
    
    if (!missing){
      ## aggregate by DOY
      agg <- aggregate( centre ~ doy, data=df_gapfld, FUN=mean, na.rm=TRUE )
      if (is.element("centre_meansurr", names(df_gapfld))){
        agg_meansurr <- aggregate( centre_meansurr ~ doy, data=df_gapfld, FUN=mean, na.rm=TRUE )
        agg <- agg %>% left_join( agg_meansurr ) %>% dplyr::rename( centre_meandoy=centre, centre_meansurr_meandoy=centre_meansurr )
      } else {
        agg <- agg %>% dplyr::rename( centre_meandoy=centre )
      }
      df_gapfld <- df_gapfld %>% left_join( agg )
      
      ## get consecutive data gaps and fill only by mean seasonality if more than 3 consecutive dates are missing
      na_instances <- get_consecutive( is.na(df_gapfld$centre), leng_threshold=4, do_merge=FALSE )
      if (nrow(na_instances)>0){
        for (iinst in 1:nrow(na_instances)){
          idxs <- na_instances$idx_start[iinst]:(na_instances$idx_start[iinst]+na_instances$len[iinst]-1)
          if (is.element("centre_meansurr", names(df_gapfld))){
            ## get current anomaly of mean across surrounding pixels w.r.t. its mean annual cycle
            df_gapfld$anom_surr    <- df_gapfld$centre_meansurr / df_gapfld$centre_meansurr_meandoy
            df_gapfld$centre[idxs] <- df_gapfld$centre_meandoy[idxs] * df_gapfld$anom_surr[idxs]
          } else {
            df_gapfld$centre[idxs] <- df_gapfld$centre_meandoy[idxs]
          }
        }
      }
      
    }
    
    # ## Gap-fill still remaining by linear approximation
    # idxs <- which( is.na(df_gapfld$centre) )
    # if (length(idxs)>1){
    #   df_gapfld$centre <- approx( df_gapfld$year_dec[-idxs], df_gapfld$centre[-idxs], xout=df_gapfld$year_dec )$y
    # }
    
    # points( df_gapfld$year_dec[idxs], df_gapfld$centre[idxs], pch=16 )
    # points( df_gapfld$year_dec[-idxs], df_gapfld$centre[-idxs], pch=16, col='blue' )
    # lines(  df_gapfld$year_dec, df_gapfld$centre )
    
    # ## for pixels with low quality information, use mean of surroundings
    # if (is.element("centre_meansurr", names(df_gapfld))){
    #   for (idx in seq(nrow(df_gapfld))){
    #     if (df_gapfld_qc[idx,usecol]!=0) {
    #       df_gapfld$centre[idx] <- unname( apply( df_gapfld[idx,1:npixels], 1, FUN=mean, na.rm=TRUE ))
    #     }
    #   }
    # }
    
    # dev.off()
    
  } else if (prod=="MOD17A2H"){
    ## Contains MODIS GPP
    ## quality bitmap interpreted based on https://lpdaac.usgs.gov/dataset_discovery/modis/modis_products_table/mod17a2
    
    df$qc_bitname <- sapply( seq(nrow(df)), function(x) as.integer( intToBits( df$Psn_QC[x] )[1:8] ) %>% rev() %>% as.character() %>% paste( collapse="" )  )
    
    ## MODLAND_QC bits
    ## 0: Good  quality (main algorithm with  or without saturation)
    ## 1: Other quality (backup  algorithm or  fill  values)
    df$qc_bit0 <- substr( df$qc_bitname, start=8, stop=8 )
    # >>>>>>> 4a98b7722357887423c4ebe4f33497586902ab25
    
    ## Sensor
    ## 0: Terra
    ## 1: Aqua
    df$qc_bit1 <- substr( df$qc_bitname, start=7, stop=7 )
    
    ## Dead detector
    ## 0: Detectors apparently  fine  for up  to  50% of  channels  1,  2
    ## 1: Dead  detectors caused  >50%  adjacent  detector  retrieval
    df$qc_bit2 <- substr( df$qc_bitname, start=6, stop=6 )
    
    ## CloudState
    ## 00 0  Significant clouds  NOT present (clear)
    ## 01 1  Significant clouds  WERE  present
    ## 10 2  Mixed cloud present in  pixel
    ## 11 3  Cloud state not defined,  assumed clear
    df$qc_bit3 <- substr( df$qc_bitname, start=4, stop=5 )
    
    ## SCF_QC (five level confidence score)
    ## 000 0 Very best possible
    ## 001 1 Good, very usable, but not the best (saturation in FPAR/LAI has occurred)
    ## 010 2 Substandard due to geometry problems ??? use with caution
    ## 011 3 Substandard due to other than geometry problems ??? use with caution
    ## 100 4  Couldn't retrieve pixel (NOT PRODUCED AT ALL ??? non-terrestrial biome)
    ## 111 7  Fill Value
    df$qc_bit4 <- substr( df$qc_bitname, start=1, stop=3 )
    
    df <- df %>%  mutate(  good_quality  = ifelse( qc_bit0=="0", TRUE, FALSE ),
                           terra         = ifelse( qc_bit1=="0", TRUE, FALSE ),
                           dead_detector = ifelse( qc_bit2=="1", TRUE, FALSE ),
                           CloudState    = ifelse( qc_bit3=="00", 0, ifelse( qc_bit3=="01", 1, ifelse( qc_bit3=="10", 2, 3 ) ) ),
                           SCF_QC        = ifelse( qc_bit4=="000", 0, ifelse( qc_bit4=="001", 1, ifelse( qc_bit4=="010", 2, ifelse( qc_bit4=="011", 3, ifelse( qc_bit4=="111", 7, NA ) ) ) ) )
    ) %>%
      select( -qc_bitname, -Psn_QC, -qc_bit0, -qc_bit1, -qc_bit2, -qc_bit3, -qc_bit4 ) %>% 
      rename( modisvar = Gpp )
    
    ## Identify outliers, i.e. whether value is exceedingly high, i.e. if the distance of the value to the median is more than 5 times the distance of the distance of the 75% quantile to the median
    df <- df %>%  mutate( outlier = ifelse( modisvar - median( modisvar, na.rm=TRUE ) > 5 * ( quantile( modisvar, probs=0.75, na.rm=TRUE  ) - median( modisvar, na.rm=TRUE ) ), TRUE, FALSE ) )
    
    ## Plot effect of filtering steps
    dir <- paste0(dir, "/fig_fapar_gapfilling/")
    if (!dir.exists(dir)) system(paste0("mkdir ", dir))
    plotfiln <- paste0( dir, "gpp_MOD17A2H_fill_", sitename, ".pdf" )
    print( paste( "Gapfilling illustrated in:", plotfiln ) )
    pdf( plotfiln, width=15, height=6 )
    par(xpd=TRUE)
    with( df, plot( year_dec, modisvar, pch=16, col='red', main=sitename, xlab="year", ylab="MOD17A2H", las=1 ) )
    with( dplyr::filter( df, !outlier ), points( year_dec, modisvar, pch=16, col='black' ) )
    # with( dplyr::filter( df, good_quality), points( year_dec, modisvar, pch=16, col='blue' ) )
    # with( dplyr::filter( df, good_quality & CloudState==0), points( year_dec, modisvar, pch=16, col='springgreen3' ) )
    # with( dplyr::filter( df, good_quality & CloudState==0 & !dead_detector ), points( year_dec, modisvar, pch=16, col='orchid' ) )
    # with( dplyr::filter( df, good_quality & CloudState==0 & !dead_detector & SCF_QC==0 ), points( year_dec, modisvar, pch=16, col='black' ) )
    legend( "topleft", 
            c("initial", "!outlier"), 
            col=c( "red", "black" ), pch=16, bty="n", inset = c(0,-0.2)
    )
    
    ## Filter, i.e. replacing by NA in order to keep all dates
    df <- df %>% mutate( modisvar = ifelse( ( !outlier ), modisvar, NA ) )
    
  }
  
  if (do_interpolate){
    ##--------------------------------------
    ## Create daily dataframe
    ##--------------------------------------
    ddf <- init_dates_dataframe( year_start, year_end ) %>% dplyr::mutate( doy = lubridate::yday(date) )
    
    ## merge N-day dataframe into daily one. 
    ## Warning: here, 'date' must be centered within 4-day period - thus not equal to start date but (start date + 2)
    ddf <- ddf %>% left_join( dplyr::select( df, date, modisvar ), by="date" )
    
    ## extrapolate missing values at head and tail
    ddf$modisvar <- extrapolate_missing_headtail(dplyr::select(ddf, var = modisvar, doy))
    
    ##--------------------------------------
    ## get LOESS spline model for predicting daily values (used below)
    ##--------------------------------------
    idxs    <- which(!is.na(ddf$modisvar))
    myloess <- try( with( ddf, loess( modisvar[idxs] ~ year_dec[idxs], span=0.01 ) ))
    i <- 0
    while (class(myloess)=="try-error" && i<50){
      i <- i + 1
      # print(paste("i=",i))
      myloess <- try( with( ddf, loess( modisvar[idxs] ~ year_dec[idxs], span=(0.01+0.002*(i-1)) ) ))
    }
    
    ##--------------------------------------
    ## get SPLINE model for predicting daily values (used below)
    ##--------------------------------------
    idxs   <- which(!is.na(ddf$modisvar))
    spline <- try( with( ddf, smooth.spline( year_dec[idxs], modisvar[idxs], spar=0.01 ) ) )
    
    ## predict LOESS
    ##--------------------------------------
    tmp <- try( with( ddf, predict( myloess, year_dec ) ) )
    if (class(tmp)!="try-error"){
      ddf$data_loess <- tmp
    } else {
      ddf$data_loess <- rep( NA, nrow(ddf) )
    }
    
    ## predict SPLINE
    ##--------------------------------------
    tmp <- try( with( ddf, predict( spline, year_dec ) )$y)
    if (class(tmp)!="try-error"){
      ddf$spline <- tmp
    } else {
      ddf$spline <- rep( NA, nrow(ddf) )
    }
    
    ## LINEAR INTERPOLATION
    ##--------------------------------------
    ddf$interpl <- approx( ddf$year_dec, ddf$modisvar, xout=ddf$year_dec )$y 
    
    ## SAVITZKY GOLAY FILTER
    ##--------------------------------------
    ddf$sgfiltered <- rep( NA, nrow(ddf) )
    idxs <- which(!is.na(ddf$interpl))
    ddf$sgfiltered[idxs] <- signal::sgolayfilt( ddf$interpl[idxs], p=3, n=51 ) 
    
    ##--------------------------------------
    ## DEFINE STANDARD: LINEAR INTERPOLATION OR SPLINE
    ##--------------------------------------
    if (splined_fapar){
      ddf$modisvar_interpol <- ddf$spline
    } else {
      ddf$modisvar_interpol <- ddf$interpl
    }
    
    ## limit to within 0 and 1 (loess spline sometimes "explodes")
    ddf <- ddf %>% mutate( modisvar_interpol = replace( modisvar_interpol, modisvar_interpol<0, 0  ) ) %>%
      mutate( modisvar_interpol = replace( modisvar_interpol, modisvar_interpol>1, 1  ) )
    
    ## plot daily smoothed line and close plotting device
    if (do_plot_interpolated) with( ddf, lines( year_dec, modisvar_interpol, col='red', lwd=2 ) ) 
    if (do_plot_interpolated) with( ddf, lines( year_dec, sgfiltered, col='springgreen3', lwd=1 ) )
    if (do_plot_interpolated) with( ddf, lines( year_dec, spline, col='cyan', lwd=1 ) )
    legend( "topright", 
            c("Savitzky-Golay filter", "Spline", "Linear interpolation (standard)"), 
            col=c("springgreen3", "cyan", "red" ), lty=1, lwd=c(1,1,2), bty="n", inset = c(0,-0.2)
    )
    
    ## extrapolate missing values at head and tail again
    ##--------------------------------------
    ddf$modisvar_interpol <- extrapolate_missing_headtail(dplyr::select(ddf, var = modisvar_interpol, doy))
    
    
  } else {
    
    ddf = tibble()
    
  }
  
  dev.off()
  
  return( list( df=df, ddf=ddf ) )
  
}
