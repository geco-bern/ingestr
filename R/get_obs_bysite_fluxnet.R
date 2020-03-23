#' Get FLUXNET observational data for one site.
#'
#' Function for reading observational GPP data from FLUXNET dataset
#' and defining calibration target (which flux decomposition method etc.)
#'
#' @param sitename A character string specifying the site name for which
#' FLUXNET data is searched (based on the site name appearing as part
#' of the respective file name). Defaults to NA.
#' @param dir A character string specifying the local path of
#' FLUXNET data.
#' @param dir_hh A character string specifying the local path of
#' half-hourly FLUXNET data, required to get daytime VPD. Defaults to
#' \code{NULL} (no daytime VPD is calculated).
#' @param dir_hr A character string specifying the local path of
#' hourly FLUXNET data, required to get daytime VPD. Defaults to
#' \code{NULL} (no daytime VPD is calculated).
#' @param timescale A character specifying the time scale of FLUXNET
#' data. Any of \code{c("d", "w", "m", "y")} for daily, weekly, monthly,
#' or yearly, respectively.
#' @param getvars A vector of character strings corresponding to the
#' FLUXNET variable names as used in the original data files. See
#' \url{https://fluxnet.fluxdata.org/data/fluxnet2015-dataset/}. If argument
#' \code{getswc==TRUE}, then soil water content data (variables starting with
#' \code{"SWC_}) are read.
#' @param getswc  If \code{getswc==TRUE}, then all soil water content data
#' (variables starting with \code{"SWC_}) are read. Defaults to \code{TRUE}.
#' @param threshold_GPP A numeric value (between 0 and 1 for daily, weekly,
#' monthly, and annual data or 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data). The value specifies the
#' threshold for excluding data during data cleaning. The threshold is
#' with respect to the data quality flag in the FLUXNET data, indicating
#' the fraction of measured and good quality gapfilled data for daily, weekly,
#' monthly, and annual data and 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data. Defaults to \code{threshold_GPP=0}
#' meaning no data is excluded.
#' @param threshold_LE A numeric value (between 0 and 1 for daily, weekly,
#' monthly, and annual data or 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data). The value specifies the
#' threshold for excluding data during data cleaning. The threshold is
#' with respect to the data quality flag in the FLUXNET data, indicating
#' the fraction of measured and good quality gapfilled data for daily, weekly,
#' monthly, and annual data and 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data. Defaults to \code{threshold_GPP=0}
#' meaning no data is excluded.
#' @param threshold_H A numeric value (between 0 and 1 for daily, weekly,
#' monthly, and annual data or 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data). The value specifies the
#' threshold for excluding data during data cleaning. The threshold is
#' with respect to the data quality flag in the FLUXNET data, indicating
#' the fraction of measured and good quality gapfilled data for daily, weekly,
#' monthly, and annual data and 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data. Defaults to \code{threshold_GPP=0}
#' meaning no data is excluded.
#' @param threshold_SWC A numeric value (between 0 and 1 for daily, weekly,
#' monthly, and annual data or 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data). The value specifies the
#' threshold for excluding data during data cleaning. The threshold is
#' with respect to the data quality flag in the FLUXNET data, indicating
#' the fraction of measured and good quality gapfilled data for daily, weekly,
#' monthly, and annual data and 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data. Defaults to \code{threshold_GPP=0}
#' meaning no data is excluded.
#' @param threshold_WS A numeric value (between 0 and 1 for daily, weekly,
#' monthly, and annual data or 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data). The value specifies the
#' threshold for excluding data during data cleaning. The threshold is
#' with respect to the data quality flag in the FLUXNET data, indicating
#' the fraction of measured and good quality gapfilled data for daily, weekly,
#' monthly, and annual data and 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data. Defaults to \code{threshold_GPP=0}
#' meaning no data is excluded.
#' @param threshold_USTAR A numeric value (between 0 and 1 for daily, weekly,
#' monthly, and annual data or 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data). The value specifies the
#' threshold for excluding data during data cleaning. The threshold is
#' with respect to the data quality flag in the FLUXNET data, indicating
#' the fraction of measured and good quality gapfilled data for daily, weekly,
#' monthly, and annual data and 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data. Defaults to \code{threshold_GPP=0}
#' meaning no data is excluded.
#' @param threshold_T A numeric value (between 0 and 1 for daily, weekly,
#' monthly, and annual data or 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data). The value specifies the
#' threshold for excluding data during data cleaning. The threshold is
#' with respect to the data quality flag in the FLUXNET data, indicating
#' the fraction of measured and good quality gapfilled data for daily, weekly,
#' monthly, and annual data and 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data. Defaults to \code{threshold_GPP=0}
#' meaning no data is excluded.
#' @param threshold_NETRAD A numeric value (between 0 and 1 for daily, weekly,
#' monthly, and annual data or 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data). The value specifies the
#' threshold for excluding data during data cleaning. The threshold is
#' with respect to the data quality flag in the FLUXNET data, indicating
#' the fraction of measured and good quality gapfilled data for daily, weekly,
#' monthly, and annual data and 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data. Defaults to \code{threshold_GPP=0}
#' meaning no data is excluded.
#' @param filter_ntdt A logical specifying whether agreement of daytime and nighttime-
#' based GPP estimates is to be used as a filter. Data points are removed 
#' where their difference is below the the 97.5% and above the 2.5% quantile of all 
#' difference values per site. Defaults to \code{FALSE}.
#' @param return_qc A logical specifying whether quality control variables
#' should be returned.
#' @param remove_neg A logical specifying whether negative GPP values are to
#' be removed (replaces with NA).
#'
#' @return A data frame (tibble) containing cleaned observational data,
#' named and in units corresponding to rsofun standard.
#' @export
#'
#' @examples \dontrun{df <- get_obs_bysite_fluxnet}
#'
get_obs_bysite_fluxnet <- function( sitename, dir, dir_hh=NULL,
                                    dir_hr = NULL, timescale, getvars, getswc=TRUE,
                                    threshold_GPP=0.0, threshold_LE=0.0, threshold_H=0.0, threshold_SWC=0.0,
                                    threshold_WS=0.0, threshold_USTAR=0.0, threshold_T=0.0, threshold_NETRAD=0.0, 
                                    filter_ntdt = FALSE, return_qc=FALSE,
                                    remove_neg = FALSE, verbose=TRUE ){
  
  if (verbose) print(paste("Getting FLUXNET data for", sitename, "..."))

  ##-----------------------------------------------------------------
  ## Define what exactly is to be read
  ##-----------------------------------------------------------------
  ## make a vector
  getvars_orig <- getvars
  getvars <- getvars %>% unlist() %>% unname()
  
  ## complement getvars if necessary, i.e. when filter_ntdt is TRUE
  added <- c("")
  if (is.null(filter_ntdt)) filter_ntdt <- FALSE
  if (filter_ntdt){
    if ("GPP_NT_VUT_REF" %in% getvars){
      toadd <- c("GPP_DT_VUT_REF", "NEE_VUT_REF_DAY_QC", "NEE_VUT_REF_NIGHT_QC")
      getvars <- c(getvars, toadd) %>% 
        unique()
      added <- c(added, toadd)
    }
    if ("GPP_DT_VUT_REF" %in% getvars){
      toadd <- c( "GPP_NT_VUT_REF", "NEE_VUT_REF_NIGHT_QC", "NEE_VUT_REF_DAY_QC")
      getvars <- c(getvars, toadd) %>% 
        unique()
      added <- c(added, toadd)
    }
  }
  if (any(grepl("GPP_", getvars))){
    if ("GPP_NT_VUT_REF" %in% getvars){
      toadd <- "NEE_VUT_REF_NIGHT_QC"
      getvars <- c(getvars, toadd) %>% 
        unique()
      added <- c(added, toadd)
    }
    if ("GPP_DT_VUT_REF" %in% getvars){
      toadd <- "NEE_VUT_REF_DAY_QC"
      getvars <- c(getvars, toadd) %>% 
        unique()
      added <- c(added, toadd)
    }
  }    
  
  ## Take only file for this site
  if (timescale=="d"){
    ## Daily
    filn <- list.files( dir,
      pattern = paste0( "FLX_", sitename, ".*_FLUXNET2015_FULLSET_DD.*.csv" ),
      recursive = TRUE
      )
  } else  if (timescale=="w"){
    ## Weekly
    filn <- list.files( dir,
      pattern = paste0( "FLX_", sitename, ".*_FLUXNET2015_FULLSET_WW.*.csv" ),
      recursive = TRUE
      )
  } else  if (timescale=="m"){
    ## Monthly
    filn <- list.files( dir,
      pattern = paste0("FLX_", sitename, ".*_FLUXNET2015_FULLSET_MM.*.csv"),
      recursive = TRUE
      )
  } else  if (timescale=="y" || timescale=="a"){
    ## Annual
    filn <- list.files( dir,
      pattern = paste0( "FLX_", sitename, ".*_FLUXNET2015_FULLSET_YY.*.csv" ),
      recursive = TRUE
      )
  } else  if (timescale=="hh"){
    ## half-hourly
    filn <- list.files( dir,
      pattern = paste0( "FLX_", sitename, ".*_FLUXNET2015_FULLSET_HH.*.csv" ),
      recursive = TRUE
      )
  }

  # ## Use also quality flag data for each variable in 'getvars'
  # obsvars <- tibble( getvars = getvars ) %>%
  #   dplyr::filter(!(stringr::str_detect(., "UNC"))) %>%
  #   dplyr::pull(getvars)
  # uncvars <- tibble( getvars = getvars ) %>%
  #   dplyr::filter(stringr::str_detect(., "UNC")) %>%
  #   dplyr::pull(getvars)
  # getvars <- c(obsvars, paste0(obsvars, "_QC"), uncvars)

  if (length(filn)==0) rlang::abort(paste0("No files found for timescale ", timescale, " in sub-directories of ", dir ) )
  if (length(filn)>1){
    file.info_getsize <- function(filn){
      file.info(filn)$size
    }
    rlang::warn("Reading only largest daily file available")
    path_dd <- paste0(dir, filn)
    size_vec <- purrr::map_dbl(as.list(path_dd), ~file.info_getsize(.))
    path_dd <- path_dd[which.max(size_vec)]
    filn <- basename(path_dd)
  }

  # if (length(filn)>1){
  #   filn <- filn[which(grepl("3.csv", filn))]
  #   # rlang::warn(paste0("Multiple files found for timsescale ", timescale, " in sub-directories of ", dir, ". Taking only ", filn ) )
  # }

  ##-----------------------------------------------------------------
  ## Actually read data
  ##-----------------------------------------------------------------
  ## This returns a data frame with columns (date, temp, prec, nrad, ppfd, vpd, ccov)
  df <- get_obs_fluxnet2015_raw( sitename,
    path = paste0(dir, filn),
    freq = timescale
    )

  ## For some sites, the NETRAD column is missing.
  if ("NETRAD" %in% getvars && !("NETRAD" %in% names(df))){
    df <- df %>% mutate(NETRAD = NA, NETRAD_QC = 0.0)
  }

  ##-----------------------------------------------------------------
  ## Get daytime VPD
  ##-----------------------------------------------------------------
  merge_df_vpd_day_dd <- FALSE
  if ("VPD_F_DAY" %in% getvars && !(timescale == "hh")){
    
    ## 1. Check whether daily file for daytime VPD is already available
    ##-----------------------------------------------------------------
    ## get file name(s) of file containing daily daytime VPD derived from half-hourly data
    filename_dd_vpd <- list.files( dir,
                                   pattern = paste0("FLX_", sitename, ".*_VPD_DAY.csv"),
                                   recursive = FALSE)
    
    # filename_dd_vpd <- filn_hh %>%
    #   stringr::str_replace("HH", "DD") %>%
    #   stringr::str_replace(".csv", "_VPD_DAY.csv")
    
    if (length(filename_dd_vpd)>0){
      ## Read available file
      ##-----------------------------------------------------------------
      if (length(filename_dd_vpd)>1){
        file.info_getsize <- function(filn){
          file.info(filn)$size
        }
        rlang::warn("Reading only largest daily VPD file available")
        path_dd_vpd <- paste0(dir_hh, filename_dd_vpd)
        size_vec <- purrr::map_dbl(as.list(path_dd_vpd), ~file.info_getsize(.))
        path_dd_vpd <- path_dd_vpd[which.max(size_vec)]
        filename_dd_vpd <- basename(path_dd_vpd)
      }

      ## read directly
      if (verbose) print(paste("Reading daytime VPD directly from:", paste0(dir_hh, filename_dd_vpd)))
      df_vpd_day_dd <- readr::read_csv(paste0(dir, filename_dd_vpd))
      merge_df_vpd_day_dd <- TRUE

    } else {
      ## Create new daily daytime-VPD file from half-hourly data
      ##-----------------------------------------------------------------
      if (is.null(dir_hh)){

        rlang::warn("Argument dir_hh is not provided. Daytime VPD could not be calculated.")

      } else {

        ## get half-hourly file name(s)
        filn_hh <- list.files( dir_hh,
                               pattern = paste0( "FLX_", sitename, ".*_FLUXNET2015_FULLSET_HH.*.csv" ),
                               recursive = TRUE
                              )
        
        if (length(filn_hh)>0){

          path_hh <- paste0(dir_hh, filn_hh)

          if (length(filn_hh)>1){
            file.info_getsize <- function(filn){
              file.info(filn)$size
            }
            rlang::warn("Reading only largest half-hourly file available")
            size_vec <- purrr::map_dbl(as.list(path_hh), ~file.info_getsize(.))
            path_hh <- path_hh[which.max(size_vec)]
          }

          rlang::inform("Reading half-hourly data to calculate daytime VPD ...")
          df_vpd_day_dd <- get_vpd_day_fluxnet2015_byfile(path_hh, write = TRUE)
          merge_df_vpd_day_dd <- TRUE

        } else {

          rlang::warn(paste0("No half-hourly data found in ", dir_hh, ". Looking for hourly data in ",  dir_hr, "..."))
          
          ## get hourly file name(s)
          filn_hr <- list.files( dir_hr,
                                 pattern = paste0( "FLX_", sitename, ".*_FLUXNET2015_FULLSET_HR.*.csv" ),
                                 recursive = TRUE
                                )
          if (length(filn_hr)>0){

            path_hr <- paste0(dir_hr, filn_hr)

            if (length(filn_hr)>1){
              file.info_getsize <- function(filn){
                file.info(filn)$size
              }
              rlang::warn("Reading only largest half-hourly file available")
              size_vec <- purrr::map_dbl(as.list(path_hr), ~file.info_getsize(.))
              path_hr <- path_hr[which.max(size_vec)]
            }

            rlang::inform("Reading hourly data to calculate daytime VPD ...")
            df_vpd_day_dd <- get_vpd_day_fluxnet2015_byfile(path_hr, write = TRUE)
            merge_df_vpd_day_dd <- TRUE

          }
        }
      }
    }
    
    if (merge_df_vpd_day_dd){
      
      if (timescale=="d"){
        
        # daily
        df <- df %>% dplyr::left_join(df_vpd_day_dd, by="date")
        
      } else if (timescale=="w"){
        
        # weekly
        df <- df_vpd_day_dd %>%
          dplyr::mutate(year = lubridate::year(date),
                        week = lubridate::week(date)) %>%
          dplyr::group_by(sitename, year, week) %>%
          dplyr::summarise(VPD_F_DAY        = mean(VPD_F, na.rm=TRUE),
                           VPD_F_DAY_QC     = mean(VPD_F_QC, na.rm=TRUE),
                           VPD_F_DAY_MDS    = mean(VPD_F_MDS, na.rm=TRUE),
                           VPD_F_DAY_MDS_QC = mean(VPD_F_MDS_QC, na.rm=TRUE),
                           VPD_DAY_ERA      = mean(VPD_ERA, na.rm=TRUE) ) %>%
          dplyr::right_join(df, by="date")
        
      } else if (timescale=="m"){
        
        # monthly
        df <- df_vpd_day_dd %>%
          dplyr::mutate(year = lubridate::year(date),
                        moy = lubridate::month(date)) %>%
          dplyr::group_by(sitename, year, moy) %>%
          dplyr::summarise(VPD_F_DAY        = mean(VPD_F, na.rm=TRUE),
                           VPD_F_DAY_QC     = mean(VPD_F_QC, na.rm=TRUE),
                           VPD_F_DAY_MDS    = mean(VPD_F_MDS, na.rm=TRUE),
                           VPD_F_DAY_MDS_QC = mean(VPD_F_MDS_QC, na.rm=TRUE),
                           VPD_DAY_ERA      = mean(VPD_ERA, na.rm=TRUE) ) %>%
          dplyr::right_join(df, by="date")
        
      } else if (timescale=="y"){
        
        # annual
        df <- df_vpd_day_dd %>%
          dplyr::mutate(year = lubridate::year(date)) %>%
          dplyr::group_by(sitename, year) %>%
          dplyr::summarise(VPD_F_DAY        = mean(VPD_F, na.rm=TRUE),
                           VPD_F_DAY_QC     = mean(VPD_F_QC, na.rm=TRUE),
                           VPD_F_DAY_MDS    = mean(VPD_F_MDS, na.rm=TRUE),
                           VPD_F_DAY_MDS_QC = mean(VPD_F_MDS_QC, na.rm=TRUE),
                           VPD_DAY_ERA      = mean(VPD_ERA, na.rm=TRUE) ) %>%
          dplyr::right_join(df, by="date")
        
      }
      
    }
  
  }

  ##----------------------------------------------------------
  ## Reduce data to getvars
  ##----------------------------------------------------------
  ## retain all getvars, plus soil moisture if required
  if (getswc){
    df <- df %>%
      dplyr::select( ., date, one_of(getvars), starts_with("SWC_") )
    # dplyr::mutate_at(., vars(starts_with("SWC_")), list(~as.numeric) ) # this has caused error
  } else {
    df <- df %>% dplyr::select( ., date, one_of(getvars) )
  }

  ##----------------------------------------------------------
  ## Filter / clean data
  ##----------------------------------------------------------
  ## air temperature
  TA_vars <- getvars[which(grepl("TA_", getvars))]
  TA_vars <- TA_vars[-which(grepl("_QC", TA_vars))]
  for (ivar in TA_vars){
    df <- df %>% clean_fluxnet_byvar(ivar, threshold_T)
  }

  ## wind speed
  WS_vars <- getvars[which(grepl("WS_", getvars))]
  WS_vars <- WS_vars[-which(grepl("_QC", WS_vars))]
  for (ivar in WS_vars){
    df <- df %>% clean_fluxnet_byvar(ivar, threshold_WS)
  }

  ## u-star
  USTAR_vars <- getvars[which(grepl("USTAR_", getvars))]
  USTAR_vars <- USTAR_vars[-which(grepl("_QC", USTAR_vars))]
  for (ivar in USTAR_vars){
    df <- df %>% clean_fluxnet_byvar(ivar, threshold_USTAR)
  }

  ## net radiation
  NETRAD_vars <- getvars[which(grepl("NETRAD", getvars))]
  NETRAD_vars <- NETRAD_vars[-which(grepl("_QC", NETRAD_vars))]
  for (ivar in NETRAD_vars){
    df <- df %>% clean_fluxnet_byvar(ivar, threshold_NETRAD)
  }

  energyvars <- df %>%
    dplyr::select(starts_with("LE_"), starts_with("H_")) %>%
    dplyr::select(-ends_with("_QC")) %>%
    names()

  df <- df %>%
    ## Unit conversion for sensible and latent heat flux: W m-2 -> J m-2 d-1
    dplyr::mutate_at( vars(one_of(energyvars)), convert_energy_fluxnet2015)

  ## clean GPP data
  if (any(grepl("GPP_", getvars))){
    df <- df %>%
      clean_fluxnet_gpp(threshold = threshold_GPP, remove_neg = remove_neg, filter_ntdt = filter_ntdt) %>% 
      dplyr::select(-res)
  }

  ## clean energy data (sensible and latent heat flux) data - often has spuriously equal values
  if (any(grepl("LE_", getvars))){
    if (any( !(c("LE_F_MDS", "LE_F_MDS_QC") %in% getvars) )) rlang::abort("Not all variables read from file that are needed for data cleaning.")
    df$LE_F_MDS <- clean_fluxnet_energy( df$LE_F_MDS, df$LE_F_MDS_QC, threshold=threshold_LE )
  }
  if (any(grepl("H_", getvars))){
    if (any( !(c("H_F_MDS", "H_F_MDS_QC") %in% getvars) )) rlang::abort("Not all variables read from file that are needed for data cleaning.")
    df$H_F_MDS  <- clean_fluxnet_energy( df$H_F_MDS, df$H_F_MDS_QC,   threshold=threshold_H )
  }

  ##----------------------------------------------------------
  ## Process soil moisture data
  ##----------------------------------------------------------
  ## Soil moisture related stuff for daily data
  if (timescale=="d" && getswc){
    tmp <- df %>% dplyr::select( starts_with("SWC") )
    if (ncol(tmp)>0){
      swcvars   <- tmp %>% dplyr::select( -ends_with("QC") ) %>% names()
      swcqcvars <- tmp %>% dplyr::select(  ends_with("QC") ) %>% names()

      # map( as.list(seq(length(swcvars))), ~clean_fluxnet_swc( df[[ swcvars[.] ]], df[[ swcqcvars[.] ]]) )
      if (length(swcvars)>0){
        for (ivar in 1:length(swcvars)){
          df[[ swcvars[ivar] ]] <- clean_fluxnet_swc( df[[ swcvars[ivar] ]], df[[ swcqcvars[ivar] ]], frac_data_thresh = threshold_SWC )
        }
      }

      df <- df %>%
        ## Normalise mean observational soil moisture to within minimum (=0) and maximum (=1), and
        dplyr::mutate_at( vars(one_of(swcvars)), list(~norm_to_max(.)) ) %>%

        ## get mean observational soil moisture across different depths (if available)
        dplyr::mutate( soilm_obs_mean = apply( dplyr::select( ., one_of(swcvars) ), 1, FUN = mean, na.rm = TRUE ) ) %>%
        dplyr::mutate( soilm_obs_mean = ifelse( is.nan(soilm_obs_mean), NA, soilm_obs_mean ) )
      if (verbose) rlang::warn("Converting: soilm_obs_mean = mean across different soil depths (SWC_F_MDS), with na.rm = TRUE" )

    }

  }

  ## check if anything is missing
  if (any(!(getvars %in% names(df)))){
    rlang::abort(paste("Not all getvars were found in file. Missing: ", getvars[which(!(getvars %in% names(df)))]))
  }

  if (!return_qc){
    df <- df %>% dplyr::select(-ends_with("_QC"))
  }

  ## Make unit conversions and shorter names
  outgetvars <- c()

  ##----------------------------------------------------------
  ## Rename variables to names provided by argument 'getvars' 
  ##----------------------------------------------------------
  rename_byvar <- function(df, list_var, verbose){
    name_in  <- list_var %>% unlist() %>% unname()
    name_out <- list_var %>% names()
    if (verbose) rlang::warn(paste0("Renaming: ", name_out, " = ", name_in, " \n"))
    df %>% 
      dplyr::rename_at( vars(matches({{name_in}})), list(~stringr::str_replace(., {{name_in}}, {{name_out}})) )
  }

  for (ivar in seq(length(getvars_orig))){
    df <- df %>% rename_byvar(getvars_orig[ivar], verbose = verbose)
  }


  ##----------------------------------------------------------
  ## Convert units to ingestr-standards
  ##----------------------------------------------------------
  ## conversion factor from SPLASH: flux to energy conversion, umol/J (Meek et al., 1984)
  kfFEC <- 2.04
  
  if ("vpd_day" %in% names(df)){
    if (verbose) rlang::warn("Converting: vpd_day = vpd_day * 1e2 (given in hPa, required in Pa) \n")
    df <- df %>% dplyr::mutate( vpd_day = vpd_day * 1e2 )
  }
  if ("vpd" %in% names(df)){
    if (verbose) rlang::warn("Converting: vpd = vpd * 1e2 (given in hPa, required in Pa) \n")
    df <- df %>% dplyr::mutate( vpd = vpd * 1e2 )
  }
  if ("patm" %in% names(df)){
    if (verbose) rlang::warn("Converting: patm = patm * 1e3 (given in kPa, required in Pa) \n")
    df <- df %>% dplyr::mutate( patm = patm * 1e3 )
  }
  if ("ppfd" %in% names(df)){
    if (verbose) rlang::warn("Converting: ppfd = ppfd * 60 * 60 * 24 (given in W m-2, required in J m-2 d-1) \n")
    df <- df %>% dplyr::mutate( ppfd = ppfd * 60 * 60 * 24 )
    if (verbose) rlang::warn("Converting: ppfd = ppfd * kfFEC * 1.0e-6 (convert from J/m2/d to mol/m2/d; kfFEC = 2.04 is the flux-to-energy conversion, micro-mol/J (Meek et al., 1984)) \n")
    df <- df %>% dplyr::mutate( ppfd = ppfd * kfFEC * 1.0e-6 )
  }
  if ("netrad" %in% names(df)){
    if (verbose) rlang::warn("Converting: netrad = NETRAD * 60 * 60 * 24 (given in W m-2 (avg.), required in J m-2 (daily total)) \n")
    df <- df %>% dplyr::mutate( netrad = netrad * 60 * 60 * 24 )
  }

  ## GPP
  # if ("gpp" %in% names(getvars_orig)){
  #   if (getvars_orig$gpp == "GPP_NT_VUT_REF"){
  #     df <- df %>% rename(gpp = GPP_NT_VUT_REF)
  #   }
  #   if (getvars_orig$gpp == "GPP_DT_VUT_REF"){
  #     df <- df %>% rename(gpp = GPP_DT_VUT_REF)
  #   }
  # }
  # if ("gpp_unc" %in% names(getvars_orig)){
  #   if (getvars_orig$gpp_unc == "GPP_NT_VUT_SE"){
  #     df <- df %>% rename(gpp_unc = GPP_NT_VUT_SE)
  #   }
  #   if (getvars_orig$gpp_unc == "GPP_DT_VUT_SE"){
  #     df <- df %>% rename(gpp_unc = GPP_DT_VUT_SE)
  #   }
  # }
  df <- df %>% 
    select(-one_of(added))

  # Crude fix for a crude problem: some FLUXNET2015 files end on Dec 30 in the last year available
  # Duplicate last row
  lastrow <- df %>% dplyr::slice(nrow(df))
  if (lubridate::month(lastrow$date)==12 && lubridate::mday(lastrow$date)==30){
    lastrow$date <- lastrow$date + lubridate::days(1)
    df <- df %>%
      bind_rows(lastrow)
  }

  # Crude fix for a crude problem: some FLUXNET2015 files have NA in first row for VPD
  if ("vdp_day" %in% getvars){
    if (is.na(df$vpd_day[1]) && !is.na(df$vpd_day[2])){
      df$vpd_day[1] <- df$vpd_day[2]
    }
  }

  return(df)

}

clean_fluxnet_byvar <- function(df, varnam, threshold){
  varnam_qc <- paste0(varnam, "_QC")
  df[[varnam]][which(df[[varnam_qc]] < threshold)] <- NA
  return(df)
}

##----------------------------------------------------------------------
## Function for reading observational GPP data from FLUXNET dataset
##----------------------------------------------------------------------
get_obs_bysite_wcont_fluxnet2015 <- function( sitename, dir, timescale ){

  getvars <- "SWC"

  ## Take only file for this site
  if (timescale=="d"){
    ## Daily
    filn <- list.files( dir,
      pattern = paste0( "FLX_", sitename, ".*_FLUXNET2015_FULLSET_DD.*.csv" ),
      recursive = TRUE
      )
  } else  if (timescale=="w"){
    ## Weekly
    filn <- list.files( dir,
      pattern = paste0( "FLX_", sitename, ".*_FLUXNET2015_FULLSET_WW.*.csv" ),
      recursive = TRUE
      )
  } else  if (timescale=="m"){
    ## Monthly
    filn <- list.files( dir,
      pattern = paste0(..., collapse = NULL),
      recursive = TRUE
      )
  } else  if (timescale=="y"){
    ## Annual
    filn <- list.files( dir,
      pattern = paste0( "FLX_", sitename, ".*_FLUXNET2015_FULLSET_YY.*.csv" ),
      recursive = TRUE
      )
  }

  if (length(filn)==0) abort(paste0("No files found for timescale ", timescale, "in sub-directories of ", dir ) )

  ## This returns a data frame with columns (date, temp, prec, nrad, ppfd, vpd, ccov)
  ddf <- get_obs_fluxnet2015_raw( sitename,
    path = paste0(dir, filn),
    freq = "d"
    )

  ## convert to numeric (weirdly isn't always) and subset (select)
  if ( identical( getvars , "SWC" ) ){
    df <- df %>% dplyr::mutate_at( vars(starts_with(getvars)), list(name = ~as.numeric)) %>%
                 dplyr::select( date, starts_with(getvars) )
  } else {
    df <- df %>%  dplyr::mutate_at( vars(one_of(getvars)), list(name = ~as.numeric)) %>%
                  dplyr::select( date, one_of(getvars) )
  }


  swcvars   <- dplyr::select( vars(ddf), starts_with("SWC") ) %>% dplyr::select( vars(ddf), !ends_with("QC") ) %>% names()
  swcqcvars <- dplyr::select( vars(ddf), starts_with("SWC") ) %>% dplyr::select( vars(ddf),  ends_with("QC") ) %>% names()

  # map( as.list(seq(length(swcvars))), ~clean_fluxnet_swc( ddf[[ swcvars[.] ]], ddf[[ swcqcvars[.] ]], frac_data_thresh=0.5 ) )

  if (length(swcvars)>0){
    for (ivar in 1:length(swcvars)){
      ddf[[ swcvars[ivar] ]] <- clean_fluxnet_swc( ddf[[ swcvars[ivar] ]], ddf[[ swcqcvars[ivar] ]], frac_data_thresh=0.5 )
    }
  }

  return(ddf)

}


get_obs_fluxnet2015_raw <- function( sitename, path, freq="d" ){
  ##--------------------------------------------------------------------
  ## Function returns a dataframe containing all the data of the FLUXNET
  ## 2015 data file of respective temporal resolution.
  ## Returns data in units given in the fluxnet dataset
  ##--------------------------------------------------------------------
  ## get data
  df <-  readr::read_csv( path, na="-9999" ) #, col_types = cols()

  ## get dates, their format differs slightly between temporal resolution
  if ( freq=="y" ){

    df <- df %>% dplyr::mutate( year = TIMESTAMP ) %>% dplyr::mutate( date = lubridate::ymd( paste0( as.character(year), "-01-01" ) ) )

  } else if ( freq=="w"){

    df <- df %>% dplyr::mutate( date_start = lubridate::ymd(TIMESTAMP_START), date_end = lubridate::ymd(TIMESTAMP_END) ) %>%
                 dplyr::mutate( date = date_start )

  } else if ( freq=="m" ){

    df <- df %>% dplyr::mutate( year = substr( TIMESTAMP, start = 1, stop = 4 ), month = substr( TIMESTAMP, start = 5, stop = 6 ) ) %>%
                 dplyr::mutate( date = lubridate::ymd( paste0( as.character(year), "-", as.character(month), "-01" ) ) )

  } else if ( freq=="d" ){

    df <- df %>% dplyr::mutate( date = lubridate::ymd( TIMESTAMP ) )

  } else if ( freq=="hh" ){

    df <- df %>%  dplyr::mutate( date_start = lubridate::ymd_hm( TIMESTAMP_START ),
                                 date_end   = lubridate::ymd_hm( TIMESTAMP_END ) ) %>%
                  dplyr::mutate( date = date_start )

  }

  return( df )

}

# ## Converts units of GPP variables from FLUXNET to SOFUN standard
# convert_gpp_fluxnet2015 <- function( gpp ){
#   # in FLUXNET given in umolCO2 m-2 s-1. converted to gC m-2 d-1
#   c_molmass <- 12.0107  # molar mass of C
#   gpp_coverted <- gpp * 1e-6 * 60 * 60 * 24 * c_molmass
#   return(gpp_coverted)

# }

## Converts units of latent energy (LE) variables from FLUXNET to SOFUN standard
convert_energy_fluxnet2015 <- function( le ){
  ## W m-2 -> J m-2 d-1
  le_converted <- as.numeric(le) * 60 * 60 * 24
  return(le_converted)
}

clean_fluxnet_gpp <- function(df, nam_gpp_nt, nam_gpp_dt, nam_nt_qc, nam_dt_qc, threshold, remove_neg = FALSE, filter_ntdt){
  ##--------------------------------------------------------------------
  ## Cleans daily data using criteria 1-4 as documented in Tramontana et al., 2016
  ## gpp_nt: based on nighttime flux decomposition ("NT")
  ## gpp_dt: based on daytime flux decomposition ("DT")
  ##--------------------------------------------------------------------
  replace_with_na_qc <- function(gpp, qc, threshold){
    gpp[which(qc < threshold)] <- NA
    return(gpp)
  }
  replace_with_na_neg <- function(gpp){
    gpp[which(gpp<0)] <- NA
    return(gpp)
  }
  replace_with_na_res <- function(gpp, res, q025, q975){
    gpp[ res > q975 | res < q025  ] <- NA
    return(gpp)
  }

  df <- df %>%
    mutate(GPP_NT_VUT_REF = replace_with_na_qc(GPP_NT_VUT_REF, NEE_VUT_REF_NIGHT_QC, threshold),
           GPP_DT_VUT_REF = replace_with_na_qc(GPP_DT_VUT_REF, NEE_VUT_REF_DAY_QC,   threshold))

  # ## Remove data points that are based on too much gap-filled data in the underlying half-hourly data
  # gpp_nt[ which(qflag_nt < threshold) ] <- NA  ## based on fraction of data based on gap-filled half-hourly
  # gpp_dt[ which(qflag_dt < threshold) ] <- NA  ## based on fraction of data based on gap-filled half-hourly

  if (filter_ntdt){
    ## Remove data points where the two flux decompositions are inconsistent,
    ## i.e. where the residual of their regression is above the 97.5% or below the 2.5% quantile.
    df <- df %>%
      mutate(res = GPP_NT_VUT_REF - GPP_DT_VUT_REF)
    
    q025 <- quantile( df$res, probs = 0.025, na.rm=TRUE )
    q975 <- quantile( df$res, probs = 0.975, na.rm=TRUE )
    
    
    ## remove data outside the quartiles of the residuals between the DT and NT estimates
    df <- df %>%
      mutate(GPP_NT_VUT_REF = replace_with_na_res(GPP_NT_VUT_REF, res, q025, q975),
             GPP_DT_VUT_REF = replace_with_na_res(GPP_DT_VUT_REF, res, q025, q975)
      )
  }

  ## remove outliers
  df <- df %>%
    mutate(GPP_NT_VUT_REF = remove_outliers(GPP_NT_VUT_REF, coef = 1.5),
           GPP_DT_VUT_REF = remove_outliers(GPP_DT_VUT_REF, coef = 1.5)
           )

  ## remove negative GPP
  if (remove_neg){
    df <- df %>%
      mutate(GPP_NT_VUT_REF = replace_with_na_neg(GPP_NT_VUT_REF),
             GPP_DT_VUT_REF = replace_with_na_neg(GPP_DT_VUT_REF)
      )
  }


  return(df)
}

clean_fluxnet_energy <- function( energyflux, qflag_energyflux, threshold ){
  ##--------------------------------------------------------------------
  ##--------------------------------------------------------------------
  ## Remove data points that are based on too much gap-filled data in the underlying half-hourly data
  # frac_data_thresh <- 0.2  ## fraction of data based on gap-filled half-hourly
  energyflux[ qflag_energyflux < threshold ] <- NA

  if ( any(!is.na(qflag_energyflux)) ){ energyflux[ is.na(qflag_energyflux) ] <- NA }

  energyflux <- identify_pattern( energyflux )

  return( energyflux )
}


clean_fluxnet_swc <- function( swc, qflag_swc, frac_data_thresh=1.0 ){
  ##--------------------------------------------------------------------
  ## frac_data_thresh: fraction of data based on gap-filled half-hourly
  ##--------------------------------------------------------------------
  ## Remove data points that are based on too much gap-filled data in the underlying half-hourly data
  swc[ which( qflag_swc < frac_data_thresh ) ] <- NA
  swc <- as.numeric( swc )

  return( swc )
}


norm_to_max <- function( vec ){
  vec <- ( vec - min( vec, na.rm=TRUE ) ) / ( max( vec, na.rm=TRUE ) - min( vec, na.rm=TRUE ) )
  return( vec )
}


identify_pattern <- function( vec ){

  eps <- 1e-4

  vec <- as.numeric(as.character(vec))

  ## identify all numbers that appear more than once (already suspicious)
  counts <- as.data.frame( table( vec ) )
  counts <- counts[ order(-counts$Freq),  ]
  counts <- counts[ which(counts$Freq>3), ]

  ## convert factors to numeric
  counts$vec  <- as.numeric(levels(counts$vec))[counts$vec]

  for (idx in 1:nrow(counts)){

    ## find where this value appears
    pos <- which( abs(vec-counts$vec[idx])<eps )

    ## replace all numbers that appear more than twice with NA (assuming they are suspicious/wrong)
    vec[ pos ] <- NA

  }

  return( vec )

}

