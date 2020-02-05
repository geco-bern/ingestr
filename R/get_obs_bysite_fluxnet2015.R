#' Get FLUXNET 2015 observational data for one site.
#'
#' Function for reading observational GPP data from FLUXNET 2015 dataset
#' and defining calibration target (which flux decomposition method etc.)
#'
#' @param sitename A character string specifying the site name for which
#' FLUXNET 2015 data is searched (based on the site name appearing as part
#' of the respective file name). Defaults to NA.
#' @param path_fluxnet2015 A character string specifying the local path of
#' FLUXNET 2015 data.
#' @param path_fluxnet2015_hh A character string specifying the local path of
#' half-hourly FLUXNET 2015 data, required to get daytime VPD. Defaults to
#' \code{NULL} (no daytime VPD is calculated).
#' @param timescale A character specifying the time scale of FLUXNET 2015
#' data. Any of \code{c("d", "w", "m", "y")} for daily, weekly, monthly,
#' or yearly, respectively.
#' @param getvars A vector of character strings corresponding to the
#' FLUXNET 2015 variable names as used in the original data files. See
#' \url{https://fluxnet.fluxdata.org/data/fluxnet2015-dataset/}. If argument
#' \code{getswc==TRUE}, then soil water content data (variables starting with
#' \code{"SWC_}) are read.
#' @param getswc  If \code{getswc==TRUE}, then all soil water content data
#' (variables starting with \code{"SWC_}) are read. Defaults to \code{TRUE}.
#' @param threshold_GPP A numeric value (between 0 and 1 for daily, weekly,
#' monthly, and annual data or 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data). The value specifies the
#' threshold for excluding data during data cleaning. The threshold is
#' with respect to the data quality flag in the FLUXNET 2015 data, indicating
#' the fraction of measured and good quality gapfilled data for daily, weekly,
#' monthly, and annual data and 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data. Defaults to \code{threshold_GPP=0}
#' meaning no data is excluded.
#' @param threshold_LE A numeric value (between 0 and 1 for daily, weekly,
#' monthly, and annual data or 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data). The value specifies the
#' threshold for excluding data during data cleaning. The threshold is
#' with respect to the data quality flag in the FLUXNET 2015 data, indicating
#' the fraction of measured and good quality gapfilled data for daily, weekly,
#' monthly, and annual data and 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data. Defaults to \code{threshold_GPP=0}
#' meaning no data is excluded.
#' @param threshold_H A numeric value (between 0 and 1 for daily, weekly,
#' monthly, and annual data or 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data). The value specifies the
#' threshold for excluding data during data cleaning. The threshold is
#' with respect to the data quality flag in the FLUXNET 2015 data, indicating
#' the fraction of measured and good quality gapfilled data for daily, weekly,
#' monthly, and annual data and 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data. Defaults to \code{threshold_GPP=0}
#' meaning no data is excluded.
#' @param threshold_SWC A numeric value (between 0 and 1 for daily, weekly,
#' monthly, and annual data or 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data). The value specifies the
#' threshold for excluding data during data cleaning. The threshold is
#' with respect to the data quality flag in the FLUXNET 2015 data, indicating
#' the fraction of measured and good quality gapfilled data for daily, weekly,
#' monthly, and annual data and 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data. Defaults to \code{threshold_GPP=0}
#' meaning no data is excluded.
#' @param threshold_WS A numeric value (between 0 and 1 for daily, weekly,
#' monthly, and annual data or 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data). The value specifies the
#' threshold for excluding data during data cleaning. The threshold is
#' with respect to the data quality flag in the FLUXNET 2015 data, indicating
#' the fraction of measured and good quality gapfilled data for daily, weekly,
#' monthly, and annual data and 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data. Defaults to \code{threshold_GPP=0}
#' meaning no data is excluded.
#' @param threshold_USTAR A numeric value (between 0 and 1 for daily, weekly,
#' monthly, and annual data or 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data). The value specifies the
#' threshold for excluding data during data cleaning. The threshold is
#' with respect to the data quality flag in the FLUXNET 2015 data, indicating
#' the fraction of measured and good quality gapfilled data for daily, weekly,
#' monthly, and annual data and 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data. Defaults to \code{threshold_GPP=0}
#' meaning no data is excluded.
#' @param threshold_T A numeric value (between 0 and 1 for daily, weekly,
#' monthly, and annual data or 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data). The value specifies the
#' threshold for excluding data during data cleaning. The threshold is
#' with respect to the data quality flag in the FLUXNET 2015 data, indicating
#' the fraction of measured and good quality gapfilled data for daily, weekly,
#' monthly, and annual data and 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data. Defaults to \code{threshold_GPP=0}
#' meaning no data is excluded.
#' @param threshold_NETRAD A numeric value (between 0 and 1 for daily, weekly,
#' monthly, and annual data or 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data). The value specifies the
#' threshold for excluding data during data cleaning. The threshold is
#' with respect to the data quality flag in the FLUXNET 2015 data, indicating
#' the fraction of measured and good quality gapfilled data for daily, weekly,
#' monthly, and annual data and 0 [measured], 1 [good quality gapfill], 2 [
#' medium], 3 [poor] for half-hourly data. Defaults to \code{threshold_GPP=0}
#' meaning no data is excluded.
#' @param return_qc A logical specifying whether quality control variables
#' should be returned.
#' @param remove_neg A logical specifying whether negative GPP values are to
#' be removed (replaces with NA).
#'
#' @return A data frame (tibble) containing cleaned observational data,
#' named and in units corresponding to rsofun standard.
#' @export
#'
#' @examples df <- get_obs_bysite_fluxnet2015
#'
get_obs_bysite_fluxnet2015 <- function( sitename, path_fluxnet2015, path_fluxnet2015_hh=NULL,
  timescale, getvars, getswc=TRUE,
  threshold_GPP=0.0, threshold_LE=0.0, threshold_H=0.0, threshold_SWC=0.0,
  threshold_WS=0.0, threshold_USTAR=0.0, threshold_T=0.0, threshold_NETRAD=0.0, return_qc=FALSE,
  remove_neg = FALSE, verbose=TRUE ){

  if (verbose) print(paste("Getting FLUXNET data for", sitename, "..."))

  ## make a vector
  getvars <- getvars %>% unlist() %>% unname()

  ## Take only file for this site
  if (timescale=="d"){
    ## Daily
    filn <- list.files( path_fluxnet2015,
      pattern = paste0( "FLX_", sitename, ".*_FLUXNET2015_FULLSET_DD.*.csv" ),
      recursive = TRUE
      )
  } else  if (timescale=="w"){
    ## Weekly
    filn <- list.files( path_fluxnet2015,
      pattern = paste0( "FLX_", sitename, ".*_FLUXNET2015_FULLSET_WW.*.csv" ),
      recursive = TRUE
      )
  } else  if (timescale=="m"){
    ## Monthly
    filn <- list.files( path_fluxnet2015,
      pattern = paste0("FLX_", sitename, ".*_FLUXNET2015_FULLSET_MM.*.csv"),
      recursive = TRUE
      )
  } else  if (timescale=="y" || timescale=="a"){
    ## Annual
    filn <- list.files( path_fluxnet2015,
      pattern = paste0( "FLX_", sitename, ".*_FLUXNET2015_FULLSET_YY.*.csv" ),
      recursive = TRUE
      )
  } else  if (timescale=="hh"){
    ## half-hourly
    filn <- list.files( path_fluxnet2015,
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

  if (length(filn)==0) rlang::abort(paste0("No files found for timescale ", timescale, " in sub-directories of ", path_fluxnet2015 ) )
  if (length(filn)>1){
    file.info_getsize <- function(filn){
      file.info(filn)$size
    }
    rlang::warn("Reading only largest daily file available")
    path_dd <- paste0(path_fluxnet2015, filn)
    size_vec <- purrr::map_dbl(as.list(path_dd), ~file.info_getsize(.))
    path_dd <- path_dd[which.max(size_vec)]
    filn <- basename(path_dd)
  }


  # if (length(filn)>1){
  #   filn <- filn[which(grepl("3.csv", filn))]
  #   # rlang::warn(paste0("Multiple files found for timsescale ", timescale, " in sub-directories of ", path_fluxnet2015, ". Taking only ", filn ) )
  # }

  ## This returns a data frame with columns (date, temp, prec, nrad, ppfd, vpd, ccov)
  df <- get_obs_fluxnet2015_raw( sitename,
    path = paste0(path_fluxnet2015, filn),
    freq = timescale
    )

  ## For some sites, the NETRAD column is missing.
  if ("NETRAD" %in% getvars && !("NETRAD" %in% names(df))){
    df <- df %>% mutate(NETRAD = NA, NETRAD_QC = 0.0)
  }

  ## Get daytime VPD separately
  merge_df_vpd_day_dd <- FALSE
  if ("VPD_F_DAY" %in% getvars && !(timescale == "hh")){

    if (is.null(path_fluxnet2015_hh)){

      rlang::warn("Argument path_fluxnet2015_hh is not provided. Daytime VPD could not be calculted.")

    } else {

      ## get half-hourly file name(s)
      filn_hh <- list.files( path_fluxnet2015_hh,
                             pattern = paste0( "FLX_", sitename, ".*_FLUXNET2015_FULLSET_HH.*.csv" ),
                             recursive = TRUE
      )
      if (length(filn_hh)>0){
        use_hh <- TRUE
      } else {
        ## get hourly file name(s)
        filn_hh <- list.files( path_fluxnet2015_hh,
                               pattern = paste0( "FLX_", sitename, ".*_FLUXNET2015_FULLSET_HR.*.csv" ),
                               recursive = TRUE
        )
        use_hh <- FALSE

      }

      ## get file name(s) of file containing daily daytime VPD derived from half-hourly data
      filename_dd_vpd <- list.files( path_fluxnet2015_hh,
                                     pattern = paste0("FLX_", sitename, ".*_VPD_DAY.csv"),
                                     recursive = FALSE)

      # filename_dd_vpd <- filn_hh %>%
      #   stringr::str_replace("HH", "DD") %>%
      #   stringr::str_replace(".csv", "_VPD_DAY.csv")

      if (length(filename_dd_vpd)>0){

        if (length(filename_dd_vpd)>1){
          file.info_getsize <- function(filn){
            file.info(filn)$size
          }
          rlang::warn("Reading only largest daily VPD file available")
          path_dd_vpd <- paste0(path_fluxnet2015_hh, filename_dd_vpd)
          size_vec <- purrr::map_dbl(as.list(path_dd_vpd), ~file.info_getsize(.))
          path_dd_vpd <- path_dd_vpd[which.max(size_vec)]
          filename_dd_vpd <- basename(path_dd_vpd)
        }

        ## read directly
        if (verbose) print(paste("Reading daytime VPD directly from:", paste0(path_fluxnet2015_hh, filename_dd_vpd)))
        df_vpd_day_dd <- readr::read_csv(paste0(path_fluxnet2015_hh, filename_dd_vpd))
        merge_df_vpd_day_dd <- TRUE

      } else {

        if (length(filn_hh)>0){

          path_hh <- paste0(path_fluxnet2015_hh, filn_hh)

          if (length(filn_hh)>1){
            file.info_getsize <- function(filn){
              file.info(filn)$size
            }
            rlang::warn("Reading only largest half-hourly file available")
            size_vec <- purrr::map_dbl(as.list(path_hh), ~file.info_getsize(.))
            path_hh <- path_hh[which.max(size_vec)]
          }

          df_vpd_day_dd <- get_vpd_day_fluxnet2015_byfile(path_hh, write = TRUE)
          merge_df_vpd_day_dd <- TRUE

        } else {

          if (length(filn_hh)==0) rlang::abort("No half-hourly files found (required to calculate VPD_DAY).")

          # if (verbose) rlang::warn("No half-hourly data available. Cannot get vpd_day for this site.")
          # getvars <- getvars[-which(str_detect(getvars, "VPD_F_DAY"))]
          # # getvars <- c(getvars, "VPD_F", "VPD_F_QC")

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

  }

  ## retain all getvars, plus soil moisture if required
  if (getswc){
    df <- df %>%
      dplyr::select( ., date, one_of(getvars), starts_with("SWC_") )
    # dplyr::mutate_at(., vars(starts_with("SWC_")), list(~as.numeric) ) # this has caused error
  } else {
    df <- df %>% dplyr::select( ., date, one_of(getvars) )
  }

  # ## convert to numeric (weirdly isn't always) and subset (select)
  # df <- df %>%
  #   dplyr::mutate_at( vars(one_of(getvars)), list(~as.numeric))

  ## Filter
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

    if (any( !(c("GPP_NT_VUT_REF", "GPP_DT_VUT_REF", "NEE_VUT_REF_NIGHT_QC", "NEE_VUT_REF_DAY_QC") %in% getvars) ) ||
        any(!(c("GPP_NT_VUT_REF", "GPP_DT_VUT_REF", "NEE_VUT_REF_NIGHT_QC", "NEE_VUT_REF_DAY_QC") %in% names(df)))){
      rlang::abort("Not all variables read from file that are needed for data cleaning.")
    }

    # out_clean <- clean_fluxnet_gpp(
    #   df$GPP_NT_VUT_REF,
    #   df$GPP_DT_VUT_REF,
    #   df$NEE_VUT_REF_NIGHT_QC,
    #   df$NEE_VUT_REF_DAY_QC,
    #   threshold=threshold_GPP
    # )
    # df$GPP_NT_VUT_REF <- out_clean$gpp_nt
    # df$GPP_DT_VUT_REF <- out_clean$gpp_dt

    df <- df %>%
      clean_fluxnet_gpp(threshold = threshold_GPP, remove_neg = remove_neg)
  }

  ## clean energy data (sensible and latent heat flux) data - often has spuriously equal values
  if (any(grepl("LE_", getvars))){
    if (any( !(c("LE_F_MDS", "LE_F_MDS_QC") %in% getvars) )) rlang::abort("Not all variables read from file that are needed for data cleaning.")
    #df$LE_F_MDS_good <- clean_fluxnet_energy( df$LE_F_MDS, df$LE_F_MDS_QC, threshold_LE=0.0 )
    df$LE_F_MDS <- clean_fluxnet_energy( df$LE_F_MDS, df$LE_F_MDS_QC, threshold=threshold_LE )
  }
  if (any(grepl("H_", getvars))){
    if (any( !(c("H_F_MDS", "H_F_MDS_QC") %in% getvars) )) rlang::abort("Not all variables read from file that are needed for data cleaning.")
    df$H_F_MDS  <- clean_fluxnet_energy( df$H_F_MDS, df$H_F_MDS_QC,   threshold=threshold_H )
  }

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

  ## conversion factor from SPLASH: flux to energy conversion, umol/J (Meek et al., 1984)
  kfFEC <- 2.04

  ## Make unit conversions and shorter names
  outgetvars <- c()

  ## Rename variables
  if ("TA_F_DAY" %in% getvars && !("TA_F" %in% getvars)){
    if (verbose) rlang::warn("Renaming: temp = TA_F_DAY \n")
    df <- df %>% dplyr::rename_at( vars(starts_with("TA_F_DAY")), list(~stringr::str_replace(., "TA_F_DAY", "temp")) )
  }
  if ("TA_F" %in% getvars && !("TA_F_DAY" %in% getvars)){
    if (verbose) rlang::warn("Renaming: temp = TA_F \n")
    df <- df %>% dplyr::rename_at( vars(starts_with("TA_F")), list(~stringr::str_replace(., "TA_F", "temp")) )
  }
  if ("TA_F" %in% getvars && "TA_F_DAY" %in% getvars){
    if (verbose) rlang::warn("Renaming: temp_day = TA_F_DAY \n")
    df <- df %>% dplyr::rename_at( vars(starts_with("TA_F_DAY")), list(~stringr::str_replace(., "TA_F_DAY", "temp_day")) )
    if (verbose) rlang::warn("Renaming: temp = TA_F \n")
    df <- df %>% dplyr::rename_at( vars(starts_with("TA_F")), list(~stringr::str_replace(., "TA_F", "temp")) )
  }
  if ("P_F" %in% getvars){
    if (verbose) rlang::warn("Renaming: prec = P_F (given and required in mm) \n")
    df <- df %>% dplyr::rename_at( vars(starts_with("P_F")), list(~stringr::str_replace(., "P_F", "prec")) )
  }
  if ("WS_F" %in% getvars){
    if (verbose) rlang::warn("Renaming: wspeed = WS_F (given and required in m s-1) \n")
    df <- df %>% dplyr::rename_at( vars(starts_with("WS_F")), list(~stringr::str_replace(., "WS_F", "wspeed")) )
  }
  if ("USTAR" %in% getvars){
    if (verbose) rlang::warn("Renaming: ustar = USTAR (given and required in m s-1) \n")
    df <- df %>% dplyr::rename_at( vars(starts_with("USTAR")), list(~stringr::str_replace(., "USTAR", "ustar")) )
  }
  if ("LE_F_MDS" %in% getvars){
    if (verbose) rlang::warn("Renaming: latenth = LE_F_MDS \n")
    df <- df %>% dplyr::rename_at( vars(starts_with("LE_F_MDS")), list(~stringr::str_replace(., "LE_F_MDS", "latenth")) )
  }
  if ("LE_RANDUNC" %in% getvars){
    if (verbose) rlang::warn("Renaming: latenth_unc = LE_RANDUNC \n")
    df <- df %>% dplyr::rename_at( vars(starts_with("LE_RANDUNC")), list(~stringr::str_replace(., "LE_RANDUNC", "latenth_unc")) )
  }
  if ("H_F_MDS" %in% getvars){
    if (verbose) rlang::warn("Renaming: sensibleh = H_F_MDS \n")
    df <- df %>% dplyr::rename_at( vars(starts_with("H_F_MDS")), list(~stringr::str_replace(., "H_F_MDS", "sensibleh" )) )
  }
  # if ("VPD_F_DAY" %in% getvars){
  #   if (verbose) rlang::warn("Renaming: vpd_day = VPD_F_DAY \n")
  #   df <- df %>% dplyr::rename_at( vars(starts_with("VPD_F_DAY")), list(~stringr::str_replace(., "VPD_F_DAY", "vpd_day" )) )
  # }
  # if ("VPD_F" %in% getvars){
  #   if (verbose) rlang::warn("Renaming: vpd = VPD_F \n")
  #   df <- df %>% dplyr::rename_at( vars(starts_with("VPD_F")), list(~stringr::str_replace(., "VPD_F", "vpd" )) )
  # }
  if ("VPD_F_DAY" %in% getvars && !("VPD_F" %in% getvars)){
    if (verbose) rlang::warn("Renaming: vpd = VPD_F_DAY \n")
    df <- df %>% dplyr::rename_at( vars(starts_with("VPD_F_DAY")), list(~stringr::str_replace(., "VPD_F_DAY", "vpd")) )
  }
  if ("VPD_F" %in% getvars && !("VPD_F_DAY" %in% getvars)){
    if (verbose) rlang::warn("Renaming: vpd = VPD_F \n")
    df <- df %>% dplyr::rename_at( vars(starts_with("VPD_F")), list(~stringr::str_replace(., "VPD_F", "vpd")) )
  }
  if ("VPD_F" %in% getvars && "VPD_F_DAY" %in% getvars){
    if (verbose) rlang::warn("Renaming: vpd_day = VPD_F_DAY \n")
    df <- df %>% dplyr::rename_at( vars(starts_with("VPD_F_DAY")), list(~stringr::str_replace(., "VPD_F_DAY", "vpd_day")) )
    if (verbose) rlang::warn("Renaming: vpd = VPD_F \n")
    df <- df %>% dplyr::rename_at( vars(starts_with("VPD_F")), list(~stringr::str_replace(., "VPD_F", "vpd")) )
  }
  if ("PA_F" %in% getvars){
    if (verbose) rlang::warn("Renaming: patm = PA_F \n")
    df <- df %>% dplyr::rename_at( vars(starts_with("PA_F")), list(~stringr::str_replace(., "PA_F", "patm" )) )
  }
  if ("SW_IN_F" %in% getvars){
    if (verbose) rlang::warn("Renaming: swin = SW_IN_F  \n")
    df <- df %>% dplyr::rename_at( vars(starts_with("SW_IN_F")), list(~stringr::str_replace(., "SW_IN_F", "swin" )) )
  }
  if ("NETRAD" %in% getvars){
    if (verbose) rlang::warn("Renaming: netrad = NETRAD \n")
    df <- df %>% dplyr::rename_at( vars(starts_with("NETRAD")), list(~stringr::str_replace(., "NETRAD", "netrad" )) )
  }

  ## Convert units
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
  if ("swin" %in% names(df)){
    if (verbose) rlang::warn("Converting: swin = swin * 60 * 60 * 24 (given in W m-2, required in J m-2 d-1) \n")
    df <- df %>% dplyr::mutate( swin = swin * 60 * 60 * 24 )
    if (verbose) rlang::warn("Converting: ppfd = swin * kfFEC * 1.0e-6 (convert from J/m2/d to mol/m2/d; kfFEC = 2.04 is the flux-to-energy conversion, micro-mol/J (Meek et al., 1984)) \n")
    df <- df %>% dplyr::mutate( ppfd = swin * kfFEC * 1.0e-6 )
  }
  if ("netrad" %in% names(df)){
    if (verbose) rlang::warn("Converting: netrad = NETRAD * 60 * 60 * 24 (given in W m-2 (avg.), required in J m-2 (daily total)) \n")
    df <- df %>% dplyr::mutate( netrad = netrad * 60 * 60 * 24 )
  }


  # ## GPP: mean over DT and NT
  # if ("GPP_NT_VUT_REF" %in% getvars){
  #   if ("GPP_DT_VUT_REF" %in% getvars){
  #     if (verbose) rlang::warn("Converting: gpp_obs = mean( GPP_NT_VUT_REF, GPP_DT_VUT_REF ) \n")
  #     df <- df %>% dplyr::mutate( gpp_obs = apply( dplyr::select( df, GPP_NT_VUT_REF, GPP_DT_VUT_REF ), 1, FUN = mean, na.rm = FALSE ) )
  #   } else {
  #     if (verbose) rlang::warn("Converting: gpp_obs = GPP_NT_VUT_REF \n")
  #     df <- df %>% dplyr::rename( gpp_obs = GPP_NT_VUT_REF )
  #   }
  # } else {
  #   if ("GPP_DT_VUT_REF" %in% getvars){
  #     if (verbose) rlang::warn("Converting: gpp_obs = GPP_DT_VUT_REF \n")
  #     df <- df %>% dplyr::rename( gpp_obs = GPP_DT_VUT_REF )
  #   }
  # }


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
## Function for reading observational GPP data from FLUXNET 2015 dataset
##----------------------------------------------------------------------
get_obs_bysite_wcont_fluxnet2015 <- function( sitename, path_fluxnet2015, timescale ){

  getvars <- "SWC"

  ## Take only file for this site
  if (timescale=="d"){
    ## Daily
    filn <- list.files( path_fluxnet2015,
      pattern = paste0( "FLX_", sitename, ".*_FLUXNET2015_FULLSET_DD.*.csv" ),
      recursive = TRUE
      )
  } else  if (timescale=="w"){
    ## Weekly
    filn <- list.files( path_fluxnet2015,
      pattern = paste0( "FLX_", sitename, ".*_FLUXNET2015_FULLSET_WW.*.csv" ),
      recursive = TRUE
      )
  } else  if (timescale=="m"){
    ## Monthly
    filn <- list.files( path_fluxnet2015,
      pattern = paste0(..., collapse = NULL),
      recursive = TRUE
      )
  } else  if (timescale=="y"){
    ## Annual
    filn <- list.files( path_fluxnet2015,
      pattern = paste0( "FLX_", sitename, ".*_FLUXNET2015_FULLSET_YY.*.csv" ),
      recursive = TRUE
      )
  }

  if (length(filn)==0) abort(paste0("No files found for timescale ", timescale, "in sub-directories of ", path_fluxnet2015 ) )

  ## This returns a data frame with columns (date, temp, prec, nrad, ppfd, vpd, ccov)
  ddf <- get_obs_fluxnet2015_raw( sitename,
    path = paste0(path_fluxnet2015, filn),
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
  ## Returns data in units given in the fluxnet 2015 dataset
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

# ## Converts units of GPP variables from FLUXNET 2015 to SOFUN standard
# convert_gpp_fluxnet2015 <- function( gpp ){
#   # in FLUXNET 2015 given in umolCO2 m-2 s-1. converted to gC m-2 d-1
#   c_molmass <- 12.0107  # molar mass of C
#   gpp_coverted <- gpp * 1e-6 * 60 * 60 * 24 * c_molmass
#   return(gpp_coverted)

# }

## Converts units of latent energy (LE) variables from FLUXNET 2015 to SOFUN standard
convert_energy_fluxnet2015 <- function( le ){
  ## W m-2 -> J m-2 d-1
  le_converted <- as.numeric(le) * 60 * 60 * 24
  return(le_converted)
}

clean_fluxnet_gpp <- function(df, nam_gpp_nt, nam_gpp_dt, nam_nt_qc, nam_dt_qc, threshold, remove_neg = FALSE){
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

  ## Remove data points where the two flux decompositions are inconsistent,
  ## i.e. where the residual of their regression is above the 97.5% or below the 2.5% quantile.
  df <- df %>%
    mutate(res = GPP_NT_VUT_REF - GPP_DT_VUT_REF)

  q025 <- quantile( df$res, probs = 0.025, na.rm=TRUE )
  q975 <- quantile( df$res, probs = 0.975, na.rm=TRUE )

  df <- df %>%

    ## remove data outside the quartiles of the residuals between the DT and NT estimates
    mutate(GPP_NT_VUT_REF = replace_with_na_res(GPP_NT_VUT_REF, res, q025, q975),
           GPP_DT_VUT_REF = replace_with_na_res(GPP_DT_VUT_REF, res, q025, q975)
           ) %>%

    ## remove outliers
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

