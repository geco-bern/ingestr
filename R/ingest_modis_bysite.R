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
#' df <- ingest_modis_bysite( bundle = "modis_fpar" )
#' }

ingest_modis_bysite <- function(
  df_siteinfo,
  settings
  ){

  calendar_date <- pixels <- band <- pixel <- value <- NULL
  
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

  ## Save error code (0: no error, 1: error: file downloaded bu all data is NA,
  ##  2: file not downloaded)
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
                    settings$network,
                    function(network){MODISTools::mt_sites(network = network)}
                         )
                  )
          )
        
        while (class(sites_avl) == "try-error"){
          Sys.sleep(3)
          rlang::warn("re-trying to get available sites...")
          sites_avl <- try(
            do.call("rbind",
                    lapply(
                      settings$network,
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
          rlang::inform(paste("Initial try for band", x))
          rlang::inform(paste("of site", site))
          rlang::inform(paste("and network", network))
          
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
            rlang::warn("re-trying...")
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
      
      ## Raw downloaded data is saved to file
      rlang::inform( paste( "raw data file written:", filnam_raw_csv ) )
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

    if (length(scale_factor) != 1){
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
