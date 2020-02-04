library(dplyr)
library(readr)
load_dependencies_rsofun()

prepare_metainfo_fluxnet2015 <- function( origfilpath, dir_DD_fluxnet2015, overwrite=FALSE, filn_elv_watch=NA ){
  ##--------------------------------------------------------------------
  ## read meta info file and reshape to wide format
  ##--------------------------------------------------------------------
  widefiln <- paste0( dirname(origfilpath), "/FLX_AA-Flx_BIF_LATEST_WIDE.csv")
  
  if (!file.exists(origfilpath)){
    warn( "FLUXNET 2015 meta info file missing." )
    warn( paste0("Expected file path: ", longfiln ) )
    warn( "Get it from CX1 (manually) and place it at path above. See you soon.")
    abort("Aborting.")
  }
  
  if (!file.exists(widefiln) || overwrite){
    long <- read.csv( origfilpath, sep = ";" ) %>% as_tibble()
    wide <- purrr::map( as.list(unique(long$SITE_ID)), ~long_to_wide_fluxnet2015( ., long ) ) %>% 
      bind_rows() %>% 
      write_csv( path = widefiln )
    siteinfo <- wide
  } else {
    siteinfo <- read_csv( widefiln )
  }
  
  ##--------------------------------------------------------------------
  ## Complementing information on start year and end year from file names (more reliable data than in the meta info file)
  ##--------------------------------------------------------------------
  ## rename variables (columns) according to my gusto
  siteinfo <- dplyr::rename( siteinfo,
    elv=LOCATION_ELEV, mysitename=SITE_ID, lon=LOCATION_LONG, lat=LOCATION_LAT,
    year_start=FLUX_MEASUREMENTS_DATE_START, year_end=FLUX_MEASUREMENTS_DATE_END, classid=IGBP
    )
  
  ## over-write data as numeric
  siteinfo$lon <- as.numeric( siteinfo$lon )
  siteinfo$lat <- as.numeric( siteinfo$lat )
  siteinfo$elv <- as.numeric( siteinfo$elv )
  siteinfo$year_start <- as.numeric( siteinfo$year_start )
  siteinfo$year_end   <- as.numeric( siteinfo$year_end   )
  
  ## "Manually" get year start and year end from file names
  # moredata <- as.data.frame( read.table( paste0( settings_input$path_cx1data, "/FLUXNET-2015_Tier1/doc/filelist_DD.txt") ) )
  moredata <- list.files(dir_DD_fluxnet2015, pattern="FULLSET") 
  moredata <- moredata[ grepl("3.csv", moredata) ]
  moredata <- data.frame( filnam=moredata )
  moredata$mysitename <- substr( as.character(moredata$filnam), start=5, stop=10 )
  moredata$year_start <- substr( as.character(moredata$filnam), start=35, stop=38 )
  moredata$year_end   <- substr( as.character(moredata$filnam), start=40, stop=43 )

  missing_data_for_sites <- c()
  for (idx in seq(dim(siteinfo)[1])){
    tmp <- moredata[ which( as.character( as.character(siteinfo$mysitename[idx]) )==moredata$mysitename ), ]
    if (dim(tmp)[1]==0) {
      missing_data_for_sites <- c( missing_data_for_sites, as.character(siteinfo$mysitename[idx]) )
    } else {
      # print(paste("overwriting for site", tmp$mysitename," with year_start, year_end", tmp$year_start, tmp$year_end  ) )
      if (!is.na(tmp$year_start)) { siteinfo$year_start[idx] <- tmp$year_start }
      if (!is.na(tmp$year_end))   { siteinfo$year_end[idx]   <- tmp$year_end   }
    }
  }

  ## Some year_start and year_end data are given in a weird format (adding digits for months)
  ## Assume first 4 digits are representing the year, cut accordingly
  for (idx in seq(dim(siteinfo)[1])){
    if ( !is.na(siteinfo$year_start[idx]) ){
      if ( nchar( as.character( siteinfo$year_start[idx]) ) > 4 ) {
        siteinfo$year_start[idx] <- substr( as.character(siteinfo$year_start[idx]), start=1, stop=4 )
      }
    }
    if ( !is.na(siteinfo$year_end[idx])){
      if ( nchar( as.character(siteinfo$year_end[idx]) ) > 4 )   {
        siteinfo$year_end[idx]   <- substr( as.character(siteinfo$year_end[idx]), start=1, stop=4 )
      }
    }
  }

  # ## Exclude sites where not data is given (but just happen to appear in the meta info file)
  # siteinfo <- siteinfo[ !is.na(siteinfo$year_end), ]
  # siteinfo <- siteinfo[ !is.na(siteinfo$year_start), ]
  missing_metainfo_for_data <- c()
  for (idx in seq(dim(moredata)[1])){
    tmp <- siteinfo[ which( as.character( moredata$mysitename[idx] )==siteinfo$mysitename ), ]
    if (dim(tmp)[1]==0){
      missing_metainfo_for_data <- c( missing_metainfo_for_data, as.character(moredata$mysitename[idx]))
    }
  }

  ## Add number of years for which data is available
  siteinfo$years_data <- as.numeric( siteinfo$year_end ) - as.numeric( siteinfo$year_start ) + 1

  ## exclude sites for which no data is available
  siteinfo <- siteinfo[ which( !is.element( siteinfo$mysitename, missing_data_for_sites) ), ]

  ##--------------------------------------------------------------------
  ## Get C3/C4 information from an additional file
  ##--------------------------------------------------------------------
  filn <- "./inst/extdata/metainfo_tier1sites_fluxnet2015_C3C4.csv"
  rlang::inform( paste("Collecting C3/C4 information from file", filn ) )

  # if (!file.exists(filn)){
  #   download_file_cx1(  path_remote = "/work/bstocker/labprentice/data/FLUXNET-2015_Tier1/siteinfo_fluxnet_sofun_withC3C4info.csv", 
  #                       path_local  = paste0( settings_input$path_cx1data, "FLUXNET-2015_Tier1/" )
  #                       )
  # }

  # siteinfo <- read_delim( filn, delim = ";" ) %>%
  siteinfo <- read_csv( filn ) %>%
    dplyr::select( mysitename, c4 ) %>%
    left_join( siteinfo, by = "mysitename" )

  ##--------------------------------------------------------------------
  ## Add water holding capacity information
  ##--------------------------------------------------------------------
  filn <- "./inst/extdata/siteinfo_fluxnet2015_sofun+whc.csv"
  rlang::inform( paste("Collecting water holding capacity information from file", filn ) )

  # if (!file.exists(filn)){
  #   download_file_cx1(  path_remote = "/work/bstocker/labprentice/data/FLUXNET-2015_Tier1/siteinfo_fluxnet2015_sofun+whc.csv", 
  #                       path_local  = paste0( settings_input$path_cx1data, "FLUXNET-2015_Tier1/" )
  #                       )
  # }

  siteinfo <- read_csv( filn ) %>%
              dplyr::select( mysitename, whc ) %>%
              left_join( siteinfo, by = "mysitename" )

  ##--------------------------------------------------------------------
  ## Add elevation information by reading from WATCH-WFDEI elevation map
  ##--------------------------------------------------------------------
  if (!is.na(filn_elv_watch) && file.exists(filn_elv_watch)){

    # inform("Collecting elevation information from WATCH-WFDEI")
    # siteinfo$elv_watch <- purrr::map_dbl( as.list(1:nrow(siteinfo)), ~get_pointdata_elv_watch( siteinfo$lon[.], siteinfo$lat[.], filn_elv_watch ) )
    # siteinfo <- siteinfo %>% mutate( elv = ifelse( is.na(elv), elv_watch, elv ) )

    ## load file using the raster library
    print(paste("Creating raster brick from file", filn_elv_watch))
    if (!file.exists(filn_elv_watch)) rlang::abort(paste0("File not found: ", filn_elv_watch))
    rasta <- raster::brick(filn_elv_watch)
    
    siteinfo <- raster::extract(rasta, sp::SpatialPoints(dplyr::select(siteinfo, lon, lat)), sp = TRUE) %>% 
      as_tibble() %>% 
      dplyr::rename(elv_watch = layer) %>%
      right_join(siteinfo, by = c("lon", "lat"))
    
  }
  
  # ##--------------------------------------------------------------------
  # ## Add elevation information using the elevatr R package
  # ##--------------------------------------------------------------------
  # siteinfo <- sp::SpatialPoints(dplyr::select(siteinfo, lon, lat) %>% slice(1:3)) %>% 
  #   elevatr::get_elev_point(prj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>% 
  #   as_tibble() %>% 
  #   dplyr::rename(elv_elevatr = elevation) %>% 
  #   dplyr::mutate(elv_elevatr = ifelse(elv_elevatr==-1000000, NA, elv_elevatr)) %>%
  #   right_join(siteinfo, by = c("lon", "lat"))


  # ##--------------------------------------------------------------------
  # ## Get more stations which are in LaThuile (free-fair-use - FFU) dataset but not in 2015
  # ##--------------------------------------------------------------------
  # dataFFU <- read_csv( paste0( settings_input$path_cx1data, "/gepisat/fluxdata_free-fair-use/Fluxdata_Meta-Data.csv") )
  # dataFFU_accurate <- read_csv( paste0(settings_input$path_cx1data, "/gepisat/fluxdata_free-fair-use/CommonAnc_LATEST.csv") )
  
  # ## rename variables (columns) where necessary
  # dataFFU <- dplyr::rename( dataFFU, c( elv=ele, mysitename=stationid ) )
  
  # for (idx in seq(dim(dataFFU)[1])){
    
  #   if ( !is.element( dataFFU$mysitename[idx], siteinfo$mysitename ) ){
    
  #     print( paste( "adding station", dataFFU$mysitename[idx] ) )
  #     tmp <- siteinfo[1,]

  #     tmpFFU <- dataFFU[idx,]
  #     tmpFFU_accurate <- dataFFU_accurate[ which(dataFFU_accurate$Site.ID==tmpFFU$mysitename), ]

  #     tmp$mysitename <- tmpFFU$mysitename
  #     tmp$lon <-        tmpFFU_accurate$Longitude
  #     tmp$lat <-        tmpFFU_accurate$Latitude
  #     tmp$elv <-        ifelse( (tmpFFU_accurate$Elevation!=" TBD"), tmpFFU_accurate$Elevation, tmpFFU$elv )
  #     tmp$year_start <- tmpFFU$year_start
  #     tmp$year_end   <- tmpFFU$year_end
  #     tmp$years_data <- tmpFFU$years_data
  #     tmp$classid    <- tmpFFU$classid

  #     siteinfo <- rbind( siteinfo, tmp )
  #   }
  # }
  # siteinfo <- siteinfo[ order(siteinfo$mysitename), ]

  # # ## Get more accurate lon/lat/elv info for some sites
  # # for (idx in seq(dim(siteinfo)[1])){
  # #   tmpFFU_accurate <- dataFFU_accurate[ which(dataFFU_accurate$Site.ID==siteinfo$mysitename[idx]), ]
  # #   if (dim(tmpFFU_accurate)[1]==1){
  # #     siteinfo$lon[idx] <- tmpFFU_accurate$Longitude
  # #     siteinfo$lat[idx] <- tmpFFU_accurate$Latitude
  # #     siteinfo$elv[idx] <- ifelse( !is.na(as.numeric(tmpFFU_accurate$Elevation)), tmpFFU_accurate$Elevation, siteinfo$elv[idx] )
  # #   }
  # # }

  # siteinfo$elv[ siteinfo$elv==-9999 ] <- NA
  # siteinfo$elv <- as.numeric( siteinfo$elv )

  ##--------------------------------------------------------------------
  ## write to file
  ##--------------------------------------------------------------------
  # print( paste0("Writing (light) meta info file: ", settings_sims$siteinfo ) )
  # print( "Full and light meta info is returned by this function as list." )

  return( siteinfo )
}

add_metainfo_koeppengeiger_gtopo30_elv <- function( siteinfo, filn ){

  ## Get additional meta information for sites: Koeppen-Geiger Class
  ## The file "siteinfo_climate_koeppengeiger_flunxet2015.csv" was downloaded from downloaded from https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1530 (placed in my ~/data/FLUXNET-2015_Tier1/meta/)
  rlang::inform(paste0("Collecting koeppen-geiger climate information from file ", filn))
  tmp <-  read_csv(filn) %>%
    dplyr::select(-sitename) %>% 
    dplyr::rename( sitename = fluxnetid ) %>% 
    dplyr::select( sitename, koeppen_climate, gtopo30_elevation )
  
  meta <- tmp %>%
          mutate( koeppen_climate = str_split( koeppen_climate, " - " ) ) %>%
          mutate( koeppen_code = purrr::map( koeppen_climate, 1 ) ) %>%
          mutate( koeppen_word = purrr::map( koeppen_climate, 2 ) ) %>%
          unnest( koeppen_code )

  ## add info: number of data points (daily GPP)
  siteinfo <- siteinfo %>% left_join( meta, by = "sitename")
  
  ## create a legend for the koeppen geiger climate codes
  koeppen_legend <- tmp$koeppen_climate %>% as_tibble() %>% 
    filter( !is.na(value) ) %>%
    filter( value!="-" ) %>%
    mutate( koeppen_climate = str_split( value, " - " ) ) %>%
    mutate( koeppen_code = purrr::map( koeppen_climate, 1 ) ) %>%
    mutate( koeppen_word = purrr::map( koeppen_climate, 2 ) ) %>%
    unnest( koeppen_code ) %>% 
    unnest( koeppen_word ) %>% 
    dplyr::select( Code = koeppen_code, Climate = koeppen_word ) %>% 
    distinct( Code, .keep_all = TRUE ) %>%
    arrange( Code )

  ## write the koeppen_legend to a file
  add_filname <- "./data/koeppen_legend.Rdata"
  rlang::inform(paste0("Saving ", add_filname, " ..."))
  save( koeppen_legend, file = add_filname, version = 3 )
  
  ## Second, extract the class from a global map, complement missing in above
  ## File by Beck et al. (2018) Scientific Data, DOI: 10.1038/sdata.2018.214
  require(raster)
  kgclass <- raster("./inst/extdata/koeppen-geiger.tif")
  kglegend <- read_csv("./inst/extdata/koppen-geiger_legend.csv") %>% setNames( c("kgnumber", "koeppen_code_extr"))
  siteinfo <- siteinfo %>% mutate( kgnumber = raster::extract( kgclass, data.frame( x=.$lon, y=.$lat ) ) ) %>% 
    left_join( kglegend, by = "kgnumber" ) %>%
    mutate( koeppen_code = ifelse( is.na(koeppen_code), koeppen_code_extr, koeppen_code ) ) %>%
    dplyr::select( -koeppen_climate, -koeppen_word )
        
  # rlang::inform(paste0("Writing ", path, " ..."))
  # save( siteinfo, path = path )

  return( siteinfo )

}

long_to_wide_fluxnet2015 <- function( sitename, long ){

  sub <- long %>% dplyr::filter( SITE_ID==sitename )
    
  ## remove variable groups that have lots of duplicates w.r.t. variable
  sub <- sub %>% 
    dplyr::filter( VARIABLE_GROUP!="GRP_TEAM_MEMBER", VARIABLE!="NETWORK", VARIABLE_GROUP!="GRP_DM_FERT_M", VARIABLE_GROUP!="GRP_DM_AGRICULTURE", VARIABLE_GROUP!="GRP_DM_PESTICIDE", VARIABLE_GROUP!="GRP_DM_PLANTING", VARIABLE_GROUP!="GRP_DM_TILL"  )

  ## determine duplicates
  df_nduplicates <- sub %>% 
    group_by( SITE_ID, VARIABLE ) %>% 
    summarize( n = n()) %>% 
    filter( n>1 )

  ##  treat duplicates
  ## Use only first entry for 'reference paper'
  if (nrow(df_nduplicates)>0){
    if ("REFERENCE_PAPER" %in% df_nduplicates$VARIABLE %>% as.character()){
      varname <- "REFERENCE_PAPER"
      groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
      sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
    }

    ## Use only first entry for 'reference usage'
    df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
    if (nrow(df_nduplicates)>0){
      if ("REFERENCE_USAGE" %in% df_nduplicates$VARIABLE %>% as.character()){
        varname <- "REFERENCE_USAGE"
        groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
        sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
      }
    
      ## Use only first entry for 'reference usage'
      df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
      if (nrow(df_nduplicates)>0){
        if ("REFERENCE_DOI" %in% df_nduplicates$VARIABLE %>% as.character()){
          varname <- "REFERENCE_DOI"
          groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
          sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
        }

        ## Use only first entry for 'reference usage'
        df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
        if (nrow(df_nduplicates)>0){
          if ("REFERENCE_COMMENT" %in% df_nduplicates$VARIABLE %>% as.character()){
            varname <- "REFERENCE_COMMENT"
            groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
            sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
          }          

          ## Use only first entry for 'DOM_DIST_MGMT'
          df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
          if (nrow(df_nduplicates)>0){
            if ("DOM_DIST_MGMT" %in% df_nduplicates$VARIABLE %>% as.character()){
              varname <- "DOM_DIST_MGMT"
              groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
              sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
            }

            ## Use only first entry for 'DM_COMMENT'
            df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
            if (nrow(df_nduplicates)>0){
              if ("DM_COMMENT" %in% df_nduplicates$VARIABLE %>% as.character()){
                varname <- "DM_COMMENT"
                groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
                sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
              }

              ## Use only first entry for 'DM_DATE'
              df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
              if (nrow(df_nduplicates)>0){
                if ("DM_DATE" %in% df_nduplicates$VARIABLE %>% as.character()){
                  varname <- "DM_DATE"
                  groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
                  sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
                }

                ## Use only first entry for 'DM_DATE_UNC'
                df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                if (nrow(df_nduplicates)>0){
                  if ("DM_DATE_UNC" %in% df_nduplicates$VARIABLE %>% as.character()){
                    varname <- "DM_DATE_UNC"
                    groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
                    sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
                  }

                  ## Use only first entry for 'DM_SURF'
                  df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                  if (nrow(df_nduplicates)>0){
                    if ("DM_SURF" %in% df_nduplicates$VARIABLE %>% as.character()){
                      varname <- "DM_SURF"
                      groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
                      sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
                    }

                    ## Use only first entry for 'DM_SURF_MEAS_UNC'
                    df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                    if (nrow(df_nduplicates)>0){
                      if ("DM_SURF_MEAS_UNC" %in% df_nduplicates$VARIABLE %>% as.character()){
                        varname <- "DM_SURF_MEAS_UNC"
                        groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
                        sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
                      }

                      ## Use only first entry for 'DM_DATE_START'
                      df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                      if (nrow(df_nduplicates)>0){
                        if ("DM_DATE_START" %in% df_nduplicates$VARIABLE %>% as.character()){
                          varname <- "DM_DATE_START"
                          groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
                          sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
                        } 
                        

                        ## Use only first entry for 'DM_DATE_END'
                        df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                        if (nrow(df_nduplicates)>0){
                          if ("DM_DATE_END" %in% df_nduplicates$VARIABLE %>% as.character()){
                            varname <- "DM_DATE_END"
                            groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
                            sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
                          }

                          ## Use only first entry for 'DM_DATE_END'
                          df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                          if (nrow(df_nduplicates)>0){
                            if ("DM_GRAZE" %in% df_nduplicates$VARIABLE %>% as.character()){
                              varname <- "DM_GRAZE"
                              groupid <- sub %>% filter( VARIABLE == varname ) %>% dplyr::select( GROUP_ID ) %>% slice(1)
                              sub <- sub %>% filter( !(VARIABLE == varname & GROUP_ID!=groupid$GROUP_ID )  )
                            }

                            ## some towers had a new location (very slight) after re-installation
                            df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                            if (nrow(df_nduplicates)>0){

                              if ("LOCATION_COMMENT" %in% df_nduplicates$VARIABLE){
                                
                                groupid <- sub %>% filter( VARIABLE=="LOCATION_COMMENT") %>% filter( DATAVALUE!="Reinstallation after management activities" ) %>% dplyr::select( GROUP_ID )
                                
                                if (length(unlist(groupid))>1){
                                  print(sub %>% filter( VARIABLE=="LOCATION_COMMENT"))
                                  ans <- readline(prompt = "Enter row number of which group should be used ")
                                  groupid <- groupid[as.numeric(ans),]
                                }
                                
                                if (length(unlist(groupid))==1){
                                  
                                  ## determine variables available for this group id
                                  duplicatedvars <- sub %>% filter( GROUP_ID==groupid$GROUP_ID ) %>% dplyr::select( VARIABLE ) %>% unlist() %>% as.character()
                                  
                                  ## remove rows if VARIABLE is element of duplicatedvars and if its GROUP_ID is not equal to groupid
                                  sub <- sub %>% filter( !(VARIABLE %in% duplicatedvars & GROUP_ID!=groupid$GROUP_ID )  )
                                  
                                  ## determine remaining duplicates
                                  df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                                  
                                } else {
                                  abort("fuck")
                                }
                                
                              } 

                              df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                              if (nrow(df_nduplicates)>0){
                                
                                if ("FLUX_MEASUREMENTS_VARIABLE" %in% df_nduplicates$VARIABLE %>% as.character()){
                                  ## Often, info is given for measurements of multiple GHGs. Take only info relevant for CO2
                                  groupid <- sub %>% filter( VARIABLE=="FLUX_MEASUREMENTS_VARIABLE") %>% filter( DATAVALUE=="CO2" ) %>% dplyr::select( GROUP_ID )
                                  
                                  if ( length(unlist(groupid))>1 ){
                                    ## sometimes there are chamber measurements and eddy covariance, use only the latter
                                    tmp <- sub %>% filter( GROUP_ID %in% groupid$GROUP_ID & VARIABLE == "FLUX_MEASUREMENTS_METHOD" )
                                    tmp$DATAVALUE <- as.character(unlist(tmp$DATAVALUE))
                                    ## get group id for chambers method
                                    groupid_eddy <- tmp %>% filter( DATAVALUE=="Eddy Covariance") %>% dplyr::select( GROUP_ID )
                                    groupid <- groupid %>% filter( GROUP_ID==groupid_eddy$GROUP_ID )
                                    
                                    if ( length(unlist(groupid))>1 ){
                                      ## apparently, both are Eddy Covariance. Use only first.
                                      groupid <- groupid[1,]
                                    }
                                                                      
                                  }
                                  
                                }
                    
                                if (length(unlist(groupid))==1){
                                  
                                  ## determine variables available for this group id
                                  duplicatedvars <- sub %>% filter( GROUP_ID==groupid$GROUP_ID ) %>% dplyr::select( VARIABLE ) %>% unlist() %>% as.character()
                                  
                                  ## remove rows if VARIABLE is element of duplicatedvars and if its GROUP_ID is not equal to groupid
                                  sub <- sub %>% filter( !(VARIABLE %in% duplicatedvars & GROUP_ID!=groupid$GROUP_ID )  )
                                  
                                  ## determine remaining duplicates
                                  df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                                  
                                  if (nrow(df_nduplicates)>0){
                                    ## treat remaining duplicated variables
                                    
                                    ## all rows that have a duplicate w.r.t. VARIABLE
                                    tmp <- sub %>% dplyr::select( -VARIABLE_GROUP, -GROUP_ID )
                                    rownr_duplicated <- tmp %>% dplyr::select( VARIABLE ) %>% unlist() %>% as.character() %>% duplicated() %>% which()
                                    varname_duplicated <- tmp[ rownr_duplicated,]$VARIABLE %>% as.character()
                                    rownr_duplicated_all <- which( tmp$VARIABLE%in%varname_duplicated )
                                    
                                    print("duplicated rows:")
                                    print( tmp[rownr_duplicated_all,] )
                                    
                                    ans <- readline( prompt = "continue taking maximum w.r.t. DATAVALUE? (y/n) ")
                                    
                                    if (ans=="y"){
                                      
                                      tmp$DATAVALUE[rownr_duplicated_all] <- as.numeric( as.character( unlist( tmp[ rownr_duplicated_all, "DATAVALUE" ] ) ) )
                                      droprownr_duplicated <- rownr_duplicated_all[ which.min( tmp$DATAVALUE[rownr_duplicated_all] ) ]
                                      sub <- sub[-droprownr_duplicated,]
                                      
                                    } else {
                                      
                                      ans <- readline( prompt = "continue using only first occurrence of duplicates (y/n) ")
                                      if (ans=="y"){
                                        sub <- sub[-rownr_duplicated,]
                                      }
                                      
                                    }
                                  }
                                  
                                  ## determine remaining duplicates
                                  df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
                                  
                                  if (nrow(df_nduplicates)>0){
                                    abort("shitfuck.")
                                  }
                                }
                              }
                            }
                          }                          
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
    
  df_nduplicates <- sub %>% group_by( SITE_ID, VARIABLE ) %>% summarize( n = n()) %>% filter( n>1 )
  if (nrow(df_nduplicates)==0 && nrow(sub)>0){
    wide <- sub %>% dplyr::select( -VARIABLE_GROUP, -GROUP_ID ) %>% spread( VARIABLE, DATAVALUE )
  } else {
    abort("remaining duplicates")
    print(df_nduplicates)
    wide <- NULL
  }

  return(wide)

}

# get_pointdata_elv_watch <- function( lon, lat, filn ){
#   ##--------------------------------------------------------------------
#   ## Extract monthly data from files for each year and attach to the 
#   ## monthly dataframe (at the right location).
#   ## Original data in K, returns data in K
#   ##--------------------------------------------------------------------
#   if ( !file.exists( filn ) ) {
#     path_remote <- "WFDEI-elevation.nc" #"/work/bstocker/labprentice/data/watch_wfdei/WFDEI-elevation.nc"
#     path_local <- filn
#     download_file_cx1( path_remote, path_local )
#   }
# 
#   if ( file.exists( filn ) ){
#     
#     cmd <- paste( paste0( "./inst/bash/extract_pointdata_byfil.sh " ), filn, "elevation", "lon", "lat", sprintf( "%.2f", lon ), sprintf( "%.2f", lat ) )
#     system( cmd )
#     out <- read.table( "./out.txt" )$V1
# 
#   } else {
# 
#     abort("Could not find WATCH-WFDEI elevation file nor download it.")
# 
#   }
#   return( out )
# }

## Get complete meta info for all FLUXNET 2015 sites as Rdata file
metainfo_sites_fluxnet2015 <- prepare_metainfo_fluxnet2015( 
  origfilpath = "./inst/extdata/FLX_AA-Flx_BIF_LATEST.csv", 
  dir_DD_fluxnet2015 = "~/data/FLUXNET-2015_Tier1/20160128/point-scale_none_1d/original/unpacked",
  overwrite = FALSE, 
  filn_elv_watch = "./inst/extdata/WFDEI-elevation.nc" 
  )

save( metainfo_sites_fluxnet2015, file = "./data/metainfo_sites_fluxnet2015.Rdata", version = 3 )

## For Tier 1 sites, add Koeppen-Geiger climate information
metainfo_Tier1_sites_kgclimate_fluxnet2015 <- read_csv( "./inst/extdata/list_tier1_sites_fluxnet2015.csv" ) %>% 
  left_join( dplyr::select(metainfo_sites_fluxnet2015, sitename = mysitename, lon, lat, elv, elv_watch, year_start, year_end, years_data, classid, whc, c4), by = "sitename" ) %>%
  add_metainfo_koeppengeiger_gtopo30_elv( filn = "./inst/extdata/fluxnet_site_info_all.csv") %>% 
  
  ## complement elevation
  mutate(gtopo30_elevation = as.numeric(gtopo30_elevation)) %>% 
  mutate(elv = ifelse(is.na(elv), gtopo30_elevation, elv)) %>% 
  mutate(elv = ifelse(is.na(elv), elv_watch, elv)) %>% 
  # mutate(elv = ifelse(is.na(elv), elv_elevatr, elv))

  distinct()
  
save( metainfo_Tier1_sites_kgclimate_fluxnet2015, file = "./data/metainfo_Tier1_sites_kgclimate_fluxnet2015.Rdata", version = 3 )

# ## Create example CSV to be included in Package external data.
# out <- dplyr::filter( metainfo_Tier1_sites_kgclimate_fluxnet2015, sitename =="FR-Pue" ) %>%
#   mutate( year_start = 2007, year_end = 2014 ) %>%
#   mutate( years_data = year_end - year_start + 1 )
# write_csv( out, path = "./inst/extdata/siteinfo_example.csv" )

