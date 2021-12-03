# Creation of ingest_globalfields_wfde5_byvar ####
## Copy-Paste of ingest_globalfields_watch_byvar as template: ####
ingest_globalfields_watch_byvar <- function( ddf, siteinfo, dir, varnam ){
  
  dirn <- paste0( dir, "/", varnam, "/" )
  
  ## loop over all year and months that are required
  year_start <- ddf %>%
    dplyr::pull(date) %>%
    min() %>%
    lubridate::year()
  
  year_end <- ddf %>%
    dplyr::pull(date) %>%
    max() %>%
    lubridate::year()
  
  ## check if data is required for years before 1979 (when watch wfdei is available)
  pre_data <- year_start < 1979
  
  ## if pre-1979 data are required, read at least 10 first years to get mean climatology
  if (pre_data){
    year_start_read <- 1979
    year_end_read <- max(1988, year_end)
  } else {
    year_start_read <- year_start
    year_end_read <- year_end
  }
  
  ## construct data frame holding longitude and latitude info
  df_lonlat <- tibble(
    sitename = siteinfo$sitename,
    lon      = siteinfo$lon,
    lat      = siteinfo$lat
  )
  
  if (varnam %in% c("Rainf_daily", "Snowf_daily")){
    addstring <- "_WFDEI_CRU_"
  } else {
    addstring <- "_WFDEI_"
  }
  
  ## extract all the data for all the dates (cutting to required dates by site is done in ingest())
  allmonths <- 1:12
  allyears <- year_start_read:year_end_read
  df <- expand.grid(allmonths, allyears) %>%
    dplyr::as_tibble() %>%
    setNames(c("mo", "yr")) %>%
    rowwise() %>%
    dplyr::mutate(filename = paste0( dirn, "/", varnam, addstring, sprintf( "%4d", yr ), sprintf( "%02d", mo ), ".nc" )) %>%
    ungroup() %>%
    dplyr::mutate(data = purrr::map(filename, ~extract_pointdata_allsites(., df_lonlat, get_time = FALSE ) ))
  
  ## rearrange to a daily data frame
  complement_df <- function(df){
    df <- df %>%
      setNames(., c("myvar")) %>%
      mutate( dom = 1:nrow(.))
    return(df)
  }
  ddf <- df %>%
    tidyr::unnest(data) %>%
    dplyr::mutate(data = purrr::map(data, ~complement_df(.))) %>%
    tidyr::unnest(data) %>%
    dplyr::select(sitename, mo, yr, dom, myvar) %>%
    dplyr::mutate(date = lubridate::ymd(paste0(as.character(yr), "-", sprintf( "%02d", mo), "-", sprintf( "%02d", dom))) ) %>%
    dplyr::select(-mo, -yr, -dom)
  
  ## create data frame containing all dates, using mean annual cycle (of 1979-1988) for all years before 1979
  if (pre_data){
    rlang::inform("Data for years before 1979 requested. Taking mean annual cycle of 10 years (1979-1988) for all years before 1979.")
    
    ## get mean seasonal cycle, averaged over 1979:1988
    ddf_meandoy <- ddf %>% 
      dplyr::filter(lubridate::year(date) %in% 1979:1988) %>% 
      mutate(doy = lubridate::yday(date)) %>% 
      group_by(sitename, doy) %>% 
      summarise(myvar = mean(myvar))
    
    ## get a data frame with all dates for all sites
    ddf_tmp <- purrr::map(
      as.list(seq(nrow(siteinfo))),
      ~ingestr::init_dates_dataframe(
        lubridate::year(siteinfo$date_start[.]),
        min(1978, lubridate::year(siteinfo$date_end[.])),
        noleap = TRUE,
        timescale = "d"))
    names(ddf_tmp) <- siteinfo$sitename
    ddf_pre <- ddf_tmp %>%
      bind_rows(.id = "sitename") %>%
      drop_na() %>% 
      mutate(doy = lubridate::yday(date)) %>%
      left_join(ddf_meandoy, by = c("sitename", "doy")) %>%
      dplyr::select(-doy)
    
    # ddf_pre <- init_dates_dataframe(year_start, min(1978, year_end)) %>% 
    #   mutate(doy = lubridate::yday(date)) %>% 
    #   left_join(ddf_pre, by = "doy") %>% 
    #   dplyr::select(-doy)
    
    ## combine the two along rows
    ddf <- left_join(
      ddf %>% 
        ungroup() %>% 
        group_by(sitename) %>% 
        nest(),
      ddf_pre %>% 
        ungroup() %>% 
        group_by(sitename) %>% 
        nest() %>% 
        rename(data_pre = data),
      by = "sitename") %>% 
      mutate(data = purrr::map2(data_pre, data, ~bind_rows(.x, .y))) %>% 
      dplyr::select(-data_pre) %>% 
      unnest(data) %>% 
      arrange(date) %>%   # to make sure
      distinct() # out of desperation
  }
  
  return( ddf )
}

#### Writing ingest_globalfields_wfde5_byvar
