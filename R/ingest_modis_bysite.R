#' Download data by site
#'
#' Requires a list of sites, and a settings file.
#'
#' Returns a list of two data frames, one with data at original
#' modis dates (df), and one interpolated to all days (ddf).
#'
#' @param df_siteinfo site info data frame
#' @param settings download settings
#'
#' @return A named list containing information required for download from Google
#' Earth Engine.
#' @export
#'
#' @examples 
#' \dontrun{
#' settings_gee <- get_settings_gee( bundle = "modis_fpar" )
#' }
#' 
#'
ingest_modis_bysite <- function(
  df_siteinfo,
  settings
  ){

  # CRAN compliance, declaring unstated variables
  settings_modis <- calendar_date <- pixel <- band <- value <- NULL
  
  ##---------------------------------------------
  ## Define names
  ##---------------------------------------------
  ## this function is hacked to only do one site at a time
  sitename <- df_siteinfo$sitename[1]
  df_siteinfo <- slice(df_siteinfo, 1)

  dirnam_daily_csv <- paste0(settings$data_path, settings$productnam)
  dirnam_raw_csv <- paste0(settings$data_path, settings$productnam, "/raw/")

  if (!dir.exists(dirnam_daily_csv)){
    dir.create(
      dirnam_daily_csv,
      recursive = TRUE
    )
  }
  
  if (!dir.exists(dirnam_raw_csv)){
    dir.create(
      dirnam_raw_csv,
      recursive = TRUE
    )
  }

  if (settings$filename_with_year){
    filnam_daily_csv <- paste0(
      dirnam_daily_csv, "/",
      settings$productnam, "_daily_",
      sitename, "_",
      df_siteinfo$year_start, "_",
      df_siteinfo$year_end, ".csv"
      )
    
    filnam_raw_csv <- paste0(
      dirnam_raw_csv, "/",
      settings$productnam, "_",
      sitename, "_",
      df_siteinfo$year_start, "_",
      df_siteinfo$year_end, ".csv"
      )
    
  } else {
    filnam_daily_csv <- paste0(
      dirnam_daily_csv, "/",
      settings$productnam, "_daily_",
      sitename, ".csv"
      )
    
    filnam_raw_csv <- paste0(
      dirnam_raw_csv, "/",
      settings$productnam, "_",
      sitename, ".csv"
      )
  }

  do_continue <- TRUE

  ## Save error code (0: no error, 1: error: file downloaded bu all data is NA, 2: file not downloaded)
  df_error <- tibble()

  if (file.exists(filnam_daily_csv) && !settings$overwrite_interpol){
    ##---------------------------------------------
    ## Read daily interpolated and gapfilled
    ##---------------------------------------------
    ddf <- readr::read_csv( filnam_daily_csv )

  } else {

    if (!file.exists(filnam_raw_csv) || settings$overwrite_raw){
      ##---------------------------------------------
      ## Download from MODIS DAAC server
      ##---------------------------------------------
      if (is.na(settings$network)){
        
        part_of_network <- FALSE
        
      } else {
        ## check if site is available. 
        ## see alse here: https://modis.ornl.gov/sites/
        sites_avl <- try(
          do.call("rbind",
                  lapply(
                    settings_modis$network,
                    function(network){MODISTools::mt_sites(network = network)}
                         )
                  )
          )
        
        while (class(sites_avl) == "try-error"){
          Sys.sleep(3)
          warning("re-trying to get available sites...")
          sites_avl <- try(
            do.call("rbind",
                    lapply(
                      settings_modis$network,
                      function(network){MODISTools::mt_sites(network = network)}
                    )
            )
          )
          
          
        }
        
        part_of_network <- df_siteinfo$sitename %in% sites_avl$network_siteid
      }

      if (part_of_network){

        try_mt_subset <- function(x, df_siteinfo, settings){
          
          # grab required info
          site <- sites_avl$network_siteid[
            which(sites_avl$network_siteid %in% df_siteinfo$sitename)[1]
            ]
          network <- tolower(sites_avl$network[
            which(sites_avl$network_siteid %in% df_siteinfo$sitename)[1]
            ])
          
          # initial try
          message(paste("Initial try for band", x))
          message(paste("of site", site))
          message(paste("and network", network))
          
          df <- try(
            MODISTools::mt_subset(
              product   = settings$prod,          
              band      = x,
              start     = df_siteinfo$date_start, 
              end       = df_siteinfo$date_end,   
              site_id   = site,                   
              network   = network,
              internal  = TRUE,
              progress  = TRUE
            )
          )
          
          ## repeat if failed until it works
          while (class(df) == "try-error"){
            Sys.sleep(3)                    
            warning("re-trying...")
            df <- try(
              MODISTools::mt_subset(
                product   = settings$prod,  
                band      = x,
                start     = df_siteinfo$date_start,
                end       = df_siteinfo$date_end,  
                site_id   = site,                  
                network   = network,
                internal  = TRUE,
                progress  = TRUE
              )
            )
          }
          
          return(df)
        }
        

      } else {

        try_mt_subset <- function(x, df_siteinfo, settings){

          ## initial try
          message(paste("Initial try for band", x))
          
          df <- try(
            MODISTools::mt_subset(
              product   = settings$prod,        
              band      = x,
              lon       = df_siteinfo$lon,
              lat       = df_siteinfo$lat,
              start     = df_siteinfo$date_start, 
              end       = df_siteinfo$date_end,
              site_name = df_siteinfo$sitename,
              internal  = TRUE,
              progress  = TRUE
            )
          )
          
          ## repeat if failed until it works
          while (class(df) == "try-error"){
            Sys.sleep(3)                       
            warning("re-trying...")
            df <- try(
              MODISTools::mt_subset(
                product   = settings$prod,     
                band      = x,
                lon       = df_siteinfo$lon,
                lat       = df_siteinfo$lat,
                start     = df_siteinfo$date_start,
                end       = df_siteinfo$date_end,         
                site_name = df_siteinfo$sitename,         
                internal  = TRUE,
                progress  = TRUE
              )
            )
          }

          return(df)
        }
        
      }

      ## download for each band as a separate call - safer!
      df <- purrr::map(
        as.list(c(settings$band_var, settings$band_qc)),
        ~try_mt_subset(., df_siteinfo, settings)) %>%
        bind_rows() %>%
        as_tibble()
      
      # ## xxx check plot
      # df %>%
      #   mutate(calendar_date = lubridate::ymd(calendar_date)) %>%
      #   dplyr::filter(band == "sur_refl_b04") %>%
      #   group_by(calendar_date) %>%
      #   summarise(value = mean(value)) %>%
      #   ggplot(aes(calendar_date, value)) +
      #   geom_line()
      
      ## Raw downloaded data is saved to file
      message( paste( "raw data file written:", filnam_raw_csv ) )
      data.table::fwrite(df, file = filnam_raw_csv, sep = ",")
      # readr::write_csv(df, path = filnam_raw_csv)
      

    } else {

      ## read from file, faster with fread()
      # df <- readr::read_csv( filnam_raw_csv )
      df <- data.table::fread( filnam_raw_csv, sep = "," ) %>%
        as_tibble() %>%
        mutate(scale = ifelse(scale == "Not Available", NA, scale)) %>%
        mutate(scale = as.numeric(scale))

    }


    ##--------------------------------------------------------------------
    ## Reformat raw data
    ##--------------------------------------------------------------------
    df <- df %>%

      ## put QC info to a separate column
      dplyr::mutate(date = lubridate::ymd(calendar_date)) %>%
      dplyr::select(pixel, date, band, value) %>%
      tidyr::pivot_wider(values_from = value, names_from = band)

    ## Determine scale factor from band info and scale values
    bands <- MODISTools::mt_bands(product = settings$prod) %>%
      as_tibble()
    scale_factor <- bands %>%
      dplyr::filter(band %in% settings$band_var) %>%
      pull(scale_factor) %>%
      as.numeric() %>%
      unique()

    if (length(scale_factor)!=1){
      stop("Multiple scaling factors found for ingested bands")
    } else {
      scaleme <- function(x, scale_factor){x * scale_factor}
      df <- df %>%
        mutate(across(settings$band_var, scaleme, scale_factor = scale_factor))
        # mutate(value = scale_factor * value)
    }

    ## rename to standard variable name unless is sur_refl_b*
    if (settings$varnam != "srefl"){
      df <- df %>%
        rename(value = !!settings$band_var, qc = !!settings$band_qc)
    }

    ##--------------------------------------------------------------------
    ## Clean (gapfill and interpolate) full time series data to daily
    ##--------------------------------------------------------------------
    ddf <- gapfill_interpol(
      df,
      sitename,
      year_start      = lubridate::year(df_siteinfo$date_start),
      year_end        = lubridate::year(df_siteinfo$date_end),
      prod            = settings$prod,
      method_interpol = settings$method_interpol,
      keep            = settings$keep,
      n_focal         = settings$n_focal
    )

    ##---------------------------------------------
    ## save cleaned and interpolated data to file
    ##---------------------------------------------
    readr::write_csv( ddf, path = filnam_daily_csv )


  }

  ddf <- ddf %>%
    ungroup() %>%
    dplyr::mutate(sitename = sitename)

  return(ddf)
}

#' Gap fills a modis time series
#'
#' @param df data frame with time series data
#' @param sitename a site name
#' @param year_start a start year
#' @param year_end an end year
#' @param prod a MODIS product
#' @param method_interpol the interpolation method
#' @param keep keep original data
#' @param n_focal average across multiple pixels (if available)
#'
#' @return gap filled time series
#' @export

gapfill_interpol <- function(
  df,
  sitename,
  year_start,
  year_end,
  prod,
  method_interpol,
  keep,
  n_focal
  ){
  
  # CRAN compliance, predefine internal variables
  value <- modisvar <- qc <- qc_bitname <- vi_useful <- aerosol <- 
  adjcloud <- brdf_corr <- mixcloud <- snowice <- shadow <- qc_bit0 <- 
  qc_bit1 <- qc_bit2 <- qc_bit3 <- qc_bit4 <- CloudState <- modisvar_filtered <- 
  good_quality <- SCF_QC <- sur_refl_qc_500m <- modland_qc <- pixel <- 
  settings_modis <- approx <- prevdate <- modisvar_filled <- NULL
  
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
    ## This is for MOD13Q1 Vegetation indeces (NDVI, EVI) 
    ## data downloaded from MODIS LP DAAC
    ##--------------------------------------
    ## QC interpreted according to 
    ## https://vip.arizona.edu/documents/MODIS/MODIS_VI_UsersGuide_June_2015_C6.pdf
    df <- df %>%
      dplyr::rename(modisvar = value) %>%
      dplyr::mutate(modisvar_filtered = modisvar) %>%

      ## separate into bits
      rowwise() %>%
      mutate(
        qc_bitname = intToBits( qc ) %>%
          as.character() %>%
          paste(collapse = "")
        ) %>%

      ## Bits 0-1: VI Quality
      ##   00 VI produced with good quality
      ##   01 VI produced, but check other QA
      mutate(vi_quality = substr( qc_bitname, start=1, stop=2 )) %>%

      ## Bits 2-5: VI Usefulness
      ##   0000 Highest quality
      ##   0001 Lower quality
      ##   0010 Decreasing quality
      ##   0100 Decreasing quality
      ##   1000 Decreasing quality
      ##   1001 Decreasing quality
      ##   1010 Decreasing quality
      ##   1100 Lowest quality
      ##   1101 Quality so low that it is not useful
      ##   1110 L1B data faulty
      ##   1111 Not useful for any other reason/not processed
      mutate(vi_useful = substr( qc_bitname, start=3, stop=6 )) %>%
      dplyr::mutate(
        modisvar_filtered = ifelse(
          vi_useful %in% c("0000", "0001", "0010", "0100",
                           "1000", "1001", "1010", "1100"),
          modisvar, NA)) %>%

      ## Bits 6-7: Aerosol Quantity
      ##  00 Climatology
      ##  01 Low
      ##  10 Intermediate
      ##  11 High
      mutate(aerosol = substr( qc_bitname, start=7, stop=8 )) %>%
      dplyr::mutate(
        modisvar_filtered = ifelse(
          aerosol %in% c("00", "01", "10"), modisvar, NA)) %>%

      ## Bit 8: Adjacent cloud detected
      ##  0 No
      ##  1 Yes
      mutate(adjcloud = substr( qc_bitname, start=9, stop=9 )) %>%
      dplyr::mutate(
        modisvar_filtered = ifelse(
          adjcloud %in% c("0"), modisvar, NA)) %>%

      ## Bits 9: Atmosphere BRDF Correction
      ##   0 No
      ##   1 Yes
      mutate(brdf_corr = substr( qc_bitname, start=10, stop=10 )) %>%
      dplyr::mutate(
        modisvar_filtered = ifelse(
          brdf_corr %in% c("1"), modisvar, NA)) %>%

      ## Bits 10: Mixed Clouds
      ##   0 No
      ##   1 Yes
      mutate(mixcloud = substr( qc_bitname, start=11, stop=11 )) %>%
      dplyr::mutate(
        modisvar_filtered = ifelse(
          mixcloud %in% c("0"), modisvar, NA)) %>%

      ## Bits 11-13: Land/Water Mask
      ##  000 Shallow ocean
      ##  001 Land (Nothing else but land)
      ##  010 Ocean coastlines and lake shorelines
      ##  011 Shallow inland water
      ##  100 Ephemeral water
      ##  101 Deep inland water
      ##  110 Moderate or continental ocean
      ##  111 Deep ocean
      mutate(mask = substr( qc_bitname, start=12, stop=14 )) %>%

      ## Bits 14: Possible snow/ice
      ##  0 No
      ##  1 Yes
      mutate(snowice = substr( qc_bitname, start=15, stop=15 )) %>%
      dplyr::mutate(
        modisvar_filtered = ifelse(
          snowice %in% c("0"), modisvar, NA)) %>%

      ## Bits 15: Possible shadow
      ##  0 No
      ##  1 Yes
      mutate(shadow = substr( qc_bitname, start=16, stop=16 )) %>%
      dplyr::mutate(
        modisvar_filtered = ifelse(shadow %in% c("0"), modisvar, NA)) %>%

      ## drop it
      dplyr::select(-qc_bitname)


  } else if (prod=="MCD15A3H"){

    ## QC interpreted according to
    ##  https://explorer.earthengine.google.com/#detail/MODIS%2F006%2FMCD15A3H:

    ## This is interpreted according to 
    ## https://lpdaac.usgs.gov/documents/2/mod15_user_guide.pdf, p.9
    df <- df %>%

      dplyr::rename(modisvar = value) %>%
      dplyr::mutate(modisvar_filtered = modisvar) %>%

      ## separate into bits
      rowwise() %>%
      mutate(
        qc_bitname = intToBits( qc )[1:8] %>%
               rev() %>%
               as.character() %>%
               paste(collapse = "")
        ) %>%

      ## MODLAND_QC bits
      ## 0: Good  quality (main algorithm with or without saturation)
      ## 1: Other quality (backup  algorithm or fill values)
      mutate(qc_bit0 = substr( qc_bitname, start=8, stop=8 )) %>%
      mutate(good_quality = ifelse( qc_bit0=="0", TRUE, FALSE )) %>%

      ## Sensor
      ## 0: Terra
      ## 1: Aqua
      mutate(qc_bit1 = substr( qc_bitname, start=7, stop=7 )) %>%
      mutate(terra = ifelse( qc_bit1=="0", TRUE, FALSE )) %>%

      ## Dead detector
      ## 0: Detectors apparently  fine  for up  to  50% of  channels  1,  2
      ## 1: Dead  detectors caused  >50%  adjacent  detector  retrieval
      mutate(qc_bit2 = substr( qc_bitname, start=6, stop=6 )) %>%
      mutate(dead_detector = ifelse( qc_bit2=="1", TRUE, FALSE )) %>%

      ## CloudState
      ## 00 0  Significant clouds  NOT present (clear)
      ## 01 1  Significant clouds  WERE  present
      ## 10 2  Mixed cloud present in  pixel
      ## 11 3  Cloud state not defined,  assumed clear
      mutate(qc_bit3 = substr( qc_bitname, start=4, stop=5 )) %>%
      mutate(CloudState = ifelse(
        qc_bit3=="00", 0,
        ifelse( qc_bit3=="01", 1,
                ifelse( qc_bit3=="10", 2, 3 ) ) )
        ) %>%

      ## SCF_QC (five level confidence score)
      ## 000 0 Main (RT) method used, best result possible (no saturation)
      ## 001 1 Main (RT) method used with saturation. Good, very usable
      ## 010 2 Main (RT) method failed due to bad geometry, empirical algorithm used
      ## 011 3 Main (RT) method failed due to problems other than geometry, empirical algorithm used
      ## 100 4 Pixel not produced at all, value couldn???t be retrieved 
      ## (possible reasons: bad L1B data, unusable MOD09GA data)
      mutate(qc_bit4 = substr( qc_bitname, start=1, stop=3 )) %>%
      mutate(SCF_QC = ifelse(
        qc_bit4=="000", 0,
          ifelse( qc_bit4=="001", 1,
                  ifelse( qc_bit4=="010", 2,
                          ifelse( qc_bit4=="011", 3, 4 ) ) ) )
        ) %>%

      ## Actually do the filtering
      mutate(modisvar_filtered = ifelse(
        CloudState %in% c(0), modisvar_filtered, NA )) %>%
      mutate(modisvar_filtered = ifelse(
        good_quality, modisvar_filtered, NA )) %>%

      ## new addition 5.1.2021
      # mutate(modisvar_filtered = ifelse( !dead_detector, modisvar_filtered, NA )) %>%
      mutate(modisvar_filtered = ifelse(
        SCF_QC %in% c(0,1), modisvar_filtered, NA ))


  } else if (prod=="MOD17A2H"){
    ## Contains MODIS GPP
    ## quality bitmap interpreted based on 
    ## https://lpdaac.usgs.gov/dataset_discovery/modis/modis_products_table/mod17a2

    ## No filtering here!
    df <- df %>%
      dplyr::rename(modisvar = value) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(modisvar_filtered = modisvar)

  } else if (prod=="MOD09A1"){
    ##--------------------------------------
    ## Filter surface reflectance data
    ##--------------------------------------
    ## QC interpreted according to 
    ## https://modis-land.gsfc.nasa.gov/pdf/MOD09_UserGuide_v1.4.pdf,
    ## Section 3.2.2, Table 10 500 m, 1 km and Coarse Resolution Surface 
    ## Reflectance Band Quality Description (32-bit). Bit 0 is LSB.
    clean_sur_refl <- function(x, qc_binary){
      ifelse(qc_binary, x, NA)
    }

    df <- df %>%

      ## separate into bits
      rowwise() %>%
      mutate(qc_bitname = intToBits( sur_refl_qc_500m ) %>%
               as.character() %>%
               paste(collapse = "")
             ) %>%

      ## Bits 0-1: MODLAND QA bits
      ##   00 corrected product produced at ideal quality -- all bands
      ##   01 corrected product produced at less than ideal quality -- some or all bands
      ##   10 corrected product not produced due to cloud effects -- all bands
      ##   11 corrected product not produced for other reasons -- some or all bands, may be fill value (11) [Note that a value of (11) overrides a value of (01)].
      mutate(modland_qc = substr( qc_bitname, start=1, stop=2 )) %>%
      mutate(
        modland_qc_binary = ifelse(modland_qc %in% c("00"), TRUE, FALSE)
        ) %>%   # false for removing data
      mutate(across(starts_with("sur_refl_b"),
                    ~clean_sur_refl(., modland_qc_binary))) %>%

      # ## Bits 2-5: band 1 data quality, four bit range
      # ##   0000 highest quality
      # ##   0111 noisy detector
      # ##   1000 dead detector, data interpolated in L1B
      # ##   1001 solar zenith >= 86 degrees
      # ##   1010 solar zenith >= 85 and < 86 degrees
      # ##   1011 missing input
      # ##   1100 internal constant used in place of climatological data for at least one atmospheric constant
      # ##   1101 correction out of bounds, pixel constrained to extreme al- lowable value
      # ##   1110 L1B data faulty
      # ##   1111 not processed due to deep ocean or clouds
      # mutate(modland_qc_b01 = substr( qc_bitname, start=3, stop=6 )) %>%
      # mutate(modland_qc_b01_binary = ifelse(modland_qc_b01 %in% c("0000"), TRUE, FALSE)) %>%    # false for removing data
      # mutate(sur_refl_b01 = ifelse(modland_qc_b01_binary, sur_refl_b01, NA)) %>%
      #
      # ## Bits 6-9: band 2 data quality, four bit range
      # ##   0000 highest quality
      # ##   0111 noisy detector
      # ##   1000 dead detector, data interpolated in L1B
      # ##   1001 solar zenith >= 86 degrees
      # ##   1010 solar zenith >= 85 and < 86 degrees
      # ##   1011 missing input
      # ##   1100 internal constant used in place of climatological data for at least one atmospheric constant
      # ##   1101 correction out of bounds, pixel constrained to extreme al- lowable value
      # ##   1110 L1B data faulty
      # ##   1111 not processed due to deep ocean or clouds
      # mutate(modland_qc_b02 = substr( qc_bitname, start=7, stop=10 )) %>%
      # mutate(modland_qc_b02_binary = ifelse(modland_qc_b02 %in% c("0000"), TRUE, FALSE)) %>%    # false for removing data
      # mutate(sur_refl_b02 = ifelse(modland_qc_b02_binary, sur_refl_b02, NA)) %>%
      #
      # ## Bits 10-13: band 3 data quality, four bit range
      # ##   0000 highest quality
      # ##   0111 noisy detector
      # ##   1000 dead detector, data interpolated in L1B
      # ##   1001 solar zenith >= 86 degrees
      # ##   1010 solar zenith >= 85 and < 86 degrees
      # ##   1011 missing input
      # ##   1100 internal constant used in place of climatological data for at least one atmospheric constant
      # ##   1101 correction out of bounds, pixel constrained to extreme al- lowable value
      # ##   1110 L1B data faulty
      # ##   1111 not processed due to deep ocean or clouds
      # mutate(modland_qc_b03 = substr( qc_bitname, start=11, stop=14 )) %>%
      # mutate(modland_qc_b03_binary = ifelse(modland_qc_b03 %in% c("0000"), TRUE, FALSE)) %>%    # false for removing data
      # mutate(sur_refl_b03 = ifelse(modland_qc_b03_binary, sur_refl_b03, NA)) %>%
      #
      # ## Bits 14-17: band 4 data quality, four bit range
      # ##   0000 highest quality
      # ##   0111 noisy detector
      # ##   1000 dead detector, data interpolated in L1B
      # ##   1001 solar zenith >= 86 degrees
      # ##   1010 solar zenith >= 85 and < 86 degrees
      # ##   1011 missing input
      # ##   1100 internal constant used in place of climatological data for at least one atmospheric constant
      # ##   1101 correction out of bounds, pixel constrained to extreme al- lowable value
      # ##   1110 L1B data faulty
      # ##   1111 not processed due to deep ocean or clouds
      # mutate(modland_qc_b04 = substr( qc_bitname, start=15, stop=18 )) %>%
      # mutate(modland_qc_b04_binary = ifelse(modland_qc_b04 %in% c("0000"), TRUE, FALSE)) %>%    # false for removing data
      # mutate(sur_refl_b04 = ifelse(modland_qc_b04_binary, sur_refl_b04, NA)) %>%
      #
      # ## Bits 18-21: band 5 data quality, four bit range
      # ##   0000 highest quality
      # ##   0111 noisy detector
      # ##   1000 dead detector, data interpolated in L1B
      # ##   1001 solar zenith >= 86 degrees
      # ##   1010 solar zenith >= 85 and < 86 degrees
      # ##   1011 missing input
      # ##   1100 internal constant used in place of climatological data for at least one atmospheric constant
      # ##   1101 correction out of bounds, pixel constrained to extreme al- lowable value
      # ##   1110 L1B data faulty
      # ##   1111 not processed due to deep ocean or clouds
      # mutate(modland_qc_b05 = substr( qc_bitname, start=19, stop=22 )) %>%
      # mutate(modland_qc_b05_binary = ifelse(modland_qc_b05 %in% c("0000"), TRUE, FALSE)) %>%    # false for removing data
      # mutate(sur_refl_b05 = ifelse(modland_qc_b05_binary, sur_refl_b05, NA)) %>%
      #
      # ## Bits 22-25: band 6 data quality, four bit range
      # ##   0000 highest quality
      # ##   0111 noisy detector
      # ##   1000 dead detector, data interpolated in L1B
      # ##   1001 solar zenith >= 86 degrees
      # ##   1010 solar zenith >= 85 and < 86 degrees
      # ##   1011 missing input
      # ##   1100 internal constant used in place of climatological data for at least one atmospheric constant
      # ##   1101 correction out of bounds, pixel constrained to extreme al- lowable value
      # ##   1110 L1B data faulty
      # ##   1111 not processed due to deep ocean or clouds
      # mutate(modland_qc_b06 = substr( qc_bitname, start=23, stop=26 )) %>%
      # mutate(modland_qc_b06_binary = ifelse(modland_qc_b06 %in% c("0000"), TRUE, FALSE)) %>%    # false for removing data
      # mutate(sur_refl_b06 = ifelse(modland_qc_b06_binary, sur_refl_b06, NA)) %>%
      #
      # ## Bits 26-29: band 7 data quality, four bit range
      # ##   0000 highest quality
      # ##   0111 noisy detector
      # ##   1000 dead detector, data interpolated in L1B
      # ##   1001 solar zenith >= 86 degrees
      # ##   1010 solar zenith >= 85 and < 86 degrees
      # ##   1011 missing input
      # ##   1100 internal constant used in place of climatological data for at least one atmospheric constant
      # ##   1101 correction out of bounds, pixel constrained to extreme al- lowable value
      # ##   1110 L1B data faulty
      # ##   1111 not processed due to deep ocean or clouds
      # mutate(modland_qc_b07 = substr( qc_bitname, start=27, stop=30 )) %>%
      # mutate(modland_qc_b07_binary = ifelse(modland_qc_b07 %in% c("0000"), TRUE, FALSE)) %>%    # false for removing data
      # mutate(sur_refl_b07 = ifelse(modland_qc_b07_binary, sur_refl_b07, NA)) %>%

      # ## Bit 30: Atmospheric correction performed
      # ##   1 yes
      # ##   0 no
      # mutate(atm_corr_qc = substr( qc_bitname, start=31, stop=31 )) %>%
      # mutate(atm_corr_qc_binary = ifelse(atm_corr_qc == "1", TRUE, FALSE)) %>%     # false for removing data
      # mutate(across(starts_with("sur_refl_b"), ~clean_sur_refl(., atm_corr_qc_binary))) %>%
      #
      # ## Bit 31: Adjacency correction performed
      # ##   1 yes
      # ##   0 no
      # mutate(adj_corr_qc = substr( qc_bitname, start=32, stop=32 )) %>%
      # mutate(adj_corr_qc_binary = ifelse(adj_corr_qc == "1", TRUE, FALSE)) %>%     # false for removing data
      # mutate(across(starts_with("sur_refl_b"), ~clean_sur_refl(., adj_corr_qc_binary))) %>%

      ## drop it
      dplyr::select(-ends_with("_qc"), -ends_with("_qc_binary"))


  }

  ##--------------------------------------
  ## Create daily dataframe
  ##--------------------------------------
  ddf <- init_dates_dataframe( year_start, year_end ) %>%

      ## decimal date
      mutate(year_dec = lubridate::decimal_date(date))

  ##--------------------------------------
  ## Average across pixels by date
  ##--------------------------------------
  npixels <- df %>% pull(pixel) %>% unique() %>% length()
  n_side <- sqrt(npixels)

  ## determine across which pixels to average
  arr_pixelnumber <- matrix(1:npixels, n_side, n_side, byrow = TRUE)

  ## determine the distance from ("layer around") the focal point
  i_focal <- ceiling(n_side/2)
  j_focal <- ceiling(n_side/2)
  if (i_focal != j_focal){ stop("Aborting. Non-quadratic subset.") }
  if ((n_focal + 1) > i_focal){
    stop(
      paste("Aborting.
             Not enough pixels available for your choice of n_focal:",
            n_focal)) }

  arr_distance <- matrix(rep(NA, npixels), n_side, n_side, byrow = TRUE)
  for (i in seq(n_side)){
    for (j in seq(n_side)){
      arr_distance[i,j] <- max(abs(i - i_focal), abs(j - j_focal))
    }
  }

  ## create a mask based on the selected layer (0 for focal point only)
  arr_mask <- arr_pixelnumber
  arr_mask[which(arr_distance > n_focal)] <- NA
  vec_usepixels <- c(arr_mask) %>% stats::na.omit() %>% as.vector()

  message(paste("Number of available pixels: ", npixels))
  message(paste("Averaging across number of pixels: ",
                      length(vec_usepixels)))

  ## take mean across selected pixels
  if (prod=="MOD09A1"){
    varnams <- settings_modis$band_var
  } else {
    varnams <- c("modisvar", "modisvar_filtered")
  }
  
  # to control which pixel's information to be used.
  df <- df %>%
    group_by(date) %>%
    dplyr::filter(pixel %in% vec_usepixels) %>%    
    summarise(across(varnams, ~mean(., na.rm = TRUE)))

  # summarise(modisvar_filtered = mean(modisvar_filtered, na.rm = TRUE),
  #           modisvar = mean(modisvar, na.rm = TRUE))

  ##--------------------------------------
  ## merge N-day dataframe into daily one.
  ## Warning: here, 'date' must be centered within 4-day period - 
  ## thus not equal to start date but (start date + 2)
  ##--------------------------------------
  ddf <- ddf %>%
    left_join( df, by="date" )

  # ## extrapolate missing values at head and tail
  # ddf$modisvar <- extrapolate_missing_headtail(dplyr::select(ddf, var = modisvar, doy))

  ##--------------------------------------
  ## Interpolate
  ##--------------------------------------
  if (prod=="MOD09A1"){
    ## For reflectance data, linearly interpolate all bands to daily
    myapprox <- function(vec){
      x <- 1:length(vec)
      approx(x, vec, xout = x)$y
    }

    ddf <- ddf %>%
      mutate(across(settings_modis$band_var, ~myapprox(.)))

  } else {

    if (method_interpol == "loess" || keep){
      ##--------------------------------------
      ## get LOESS spline model for predicting daily values (used below)
      ##--------------------------------------
      message("loess...")

      ## determine periodicity
      period <- ddf %>%
        dplyr::filter(!is.na(modisvar_filtered)) %>%
        mutate(prevdate = lag(date)) %>%
        mutate(period = as.integer(difftime(date, prevdate))) %>%
        pull(period) %>%
        min(na.rm = TRUE)

      ## take a three-weeks window for locally weighted regression (loess)
      ## good explanation: 
      ## https://rafalab.github.io/dsbook/smoothing.html#local-weighted-regression-loess
      ndays_tot <- lubridate::time_length(diff(range(ddf$date)), unit = "day")
      span <- 100/ndays_tot # (20*period)/ndays_tot  # multiply with larger number to get smoother curve

      idxs    <- which(!is.na(ddf$modisvar_filtered))
      myloess <- try(stats::loess( modisvar_filtered ~ year_dec,
                             data = ddf[idxs,], span=span ) )

      ## predict LOESS to all dates with missing data
      tmp <- try( stats::predict( myloess, newdata = ddf ) )
      if (class(tmp)!="try-error"){
        ddf$loess <- tmp
      } else {
        ddf$loess <- rep( NA, nrow(ddf) )
      }
    }

    if (method_interpol == "spline" || keep){
      ##--------------------------------------
      ## get SPLINE model for predicting daily values (used below)
      ##--------------------------------------
      message("spline...")
      idxs   <- which(!is.na(ddf$modisvar_filtered))
      spline <- try(
        with(ddf,
             stats::smooth.spline(year_dec[idxs], modisvar_filtered[idxs], spar=0.01)))

      ## predict SPLINE
      tmp <- try( with( ddf, stats::predict( spline, year_dec ) )$y)
      if (class(tmp)!="try-error"){
        ddf$spline <- tmp
      } else {
        ddf$spline <- rep( NA, nrow(ddf) )
      }

    }

    if (method_interpol == "linear" || keep){
      ##--------------------------------------
      ## LINEAR INTERPOLATION
      ##--------------------------------------
      message("linear ...")
      tmp <- try(approx(ddf$year_dec, ddf$modisvar_filtered, xout=ddf$year_dec))
      if (class(tmp) == "try-error"){
        ddf <- ddf %>% mutate(linear = NA)
      } else {
        ddf$linear <- tmp$y
      }
    }

    # commented out to avoid dependency to 'signal'
    if (method_interpol == "sgfilter" || keep){
      ##--------------------------------------
      ## SAVITZKY GOLAY FILTER
      ##--------------------------------------
      message("sgfilter ...")
      ddf$sgfilter <- rep( NA, nrow(ddf) )
      idxs <- which(!is.na(ddf$modisvar_filtered))
      tmp <- try(signal::sgolayfilt( ddf$modisvar_filtered[idxs], p=3, n=51 ))
      if (class(tmp)!="try-error"){
        ddf$sgfilter[idxs] <- tmp
      }

    }

    ##--------------------------------------
    ## Define 'fapar'
    ##--------------------------------------
    if (method_interpol == "loess"){
      ddf$modisvar_filled <- ddf$loess
    } else if (method_interpol == "spline"){
      ddf$modisvar_filled <- ddf$spline
    } else if (method_interpol == "linear"){
      ddf$modisvar_filled <- ddf$linear
    } else if (method_interpol == "sgfilter"){
      ddf$modisvar_filled <- ddf$sgfilter
    }

    ## limit to within 0 and 1 (loess spline sometimes "explodes")
    ddf <- ddf %>%
      dplyr::mutate(
        modisvar_filled = replace(modisvar_filled, modisvar_filled<0, 0)) %>%
      dplyr::mutate(
        modisvar_filled = replace(modisvar_filled, modisvar_filled>1, 1))
  }

  return( ddf )

}



extrapolate_missing_headtail <- function(
  ddf
  ){
  
  ## extrapolate to missing values at head and tail using mean seasonal cycle
  ##--------------------------------------

  # define variables
  var <- NULL
  
  ## new: fill gaps at head
  idxs <- findna_head( ddf$var )
  if (length(idxs)>0){
    warning("Filling values with last available data point at head")
  }
  ddf$var[idxs] <- ddf$var[max(idxs)+1]

  ## new: fill gaps at tail
  idxs <- findna_tail( ddf$var )
  if (length(idxs)>0){
    warning("Filling values with last available data point at tail.")
  }
  ddf$var[idxs] <- ddf$var[min(idxs)-1]

  vec <- ddf %>%
    dplyr::pull(var)

  return(vec)
}

findna_headtail <- function(vec) {

  ## Remove (cut) NAs from the head and tail of a vector.
  ## Returns the indexes to be dropped from a vector

  idxs <- c(findna_head(vec), findna_tail(vec))

  return(idxs)

}

findna_head <- function(vec) {

  ## Remove (cut) NAs from the head and tail of a vector.
  ## Returns the indexes to be dropped from a vector
  
  ## Get indeces of consecutive NAs at head
  if (is.na(vec[1])){
    idx <- 0
    cuthead <- 1
    while ( idx < length(vec) ){
      idx <- idx + 1
      test <- stats::head( vec, idx )
      if (any(!is.na(test))){
        ## first non-NA found at position idx
        cuthead <- idx - 1
        break
      }
    }
    idxs_head <- 1:cuthead
  } else {
    idxs_head <- c()
  }
  
  return(idxs_head)
}

findna_tail <- function( vec ){

  ## Remove (cut) NAs from the head and tail of a vector.
  ## Returns the indexes to be dropped from a vector

  ## Get indeces of consecutive NAs at tail
  if (is.na(vec[length(vec)])){
    idx <- 0
    cuttail <- 1
    while ( idx < length(vec) ){
      idx <- idx + 1
      test <- stats::tail( vec, idx )
      if (any(!is.na(test))){
        ## first non-NA found at position idx, counting from tail
        cuttail <- idx - 1
        break
      }
    }
    idxs_tail <- (length(vec)-cuttail+1):length(vec)
  } else {
    idxs_tail <- c()
  }

  return(idxs_tail)

}

na.omit.list <- function(y) {
  return(y[!sapply(y, function(x) all(is.na(x)))]) 
}
