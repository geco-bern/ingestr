#' Get daytime VPD for all files in a directory.
#'
#' Wrapper function to derive daily daytime VPD from half-hourly data 
#' for all site-scale data files in a given directory (argument \code{dir}).
#' filtering hours where the shortwave incoming radiation (SW_IN_F) is
#' zero and aggregating taking the mean across remaining hours per day.
#'
#' @param dir A character string specifying the directory in which to look
#' for site-specific half-hourly data files.
#'
#' @return A list of outputs of the function \link{get_vpd_day_fluxnet2015_byfile}.
#' @export
#'
#' @examples 
#' \dontrun{
#' df <- get_vpd_day_fluxnet2015("./")
#' }
#' 
get_vpd_day_fluxnet2015 <- function(dir){
  
  ## loop over all HH files in the directory 'dir'
  out <- purrr::map( as.list(list.files(dir, pattern = "HH")),
              ~get_vpd_day_fluxnet2015_byfile(paste0(dir, .)))
  
  return(out)
}

#' Get daytime VPD
#'
#' Derive daily daytime VPD (vapour pressure deficit) from half-hourly 
#' data filtering hours where the shortwave incoming radiation (SW_IN_F) 
#' is greater than zero and aggregating taking the mean across remaining 
#' hours per day.
#'
#' @param filename_hh A character string specifying the file name containing
#' site-specific half-hourly data.
#' @param write A logical specifying whether daiily daytime VPD should be 
#' written to a file.
#'
#' @return A data frame (tibble) containing daily daytime VPD.
#' @export
#'
#' @examples 
#' \dontrun{
#' df <- get_vpd_day_fluxnet2015_byfile(
#'   "./FLX_BE-Vie_FLUXNET2015_FULLSET_HH_1996-2014_1-3.csv"
#'   )
#' }
#' 
#' 
get_vpd_day_fluxnet2015_byfile <- function(filename_hh, write=FALSE){
  
  # CRAN compliance, define variables
  TIMESTAMP_START <- TIMESTAMP_END <- date_start <- date_day <- TA_F <-
    TA_F_MDS <- TA_F_QC <- TA_F_MDS_QC <- TA_ERA <-
    SW_IN_F <- VPD_F <- VPD_F_QC <- VPD_F_MDS_QC <- VPD_ERA <- NULL
  
  filename_dd_vpd <- filename_hh %>% 
    stringr::str_replace("HH", "DD") %>% 
    stringr::str_replace(".csv", "_VPD_DAY.csv")
  
  if (file.exists(filename_dd_vpd)){
    ## Daytime VPD file is already available, reading from file
    # print(paste("Reading daytime VPD from:", filename_dd_vpd))
    message(paste("Reading file with calculated daytime VPD:", filename_dd_vpd))
    df <- readr::read_csv(filename_dd_vpd)
    
  } else {
    ## Get daytime VPD from half-hourly data
    ## read half-hourly data
    if (!file.exists(filename_hh)){
      stop(paste("Half-hourly file does not exist:", filename_hh))
    } 
    
    # else if (length(filename_hh)>1){
    #   warning("Reading only largest file available")
    #   file.info_getsize <- function(filn){
    #     file.info(filn)$size
    #   }
    #   size_vec <- purrr::map_dbl(as.list(filename_hh), ~file.info_getsize(.))
    #   filename_hh <- filename_hh[which.max(size_vec)]
    # }
    
    df <- readr::read_csv(filename_hh) %>% 
      dplyr::mutate( date_start = lubridate::ymd_hm( TIMESTAMP_START ),
                     date_end   = lubridate::ymd_hm( TIMESTAMP_END ) ) %>%
      dplyr::mutate( date = date_start ) %>% 
      
      ## retain only daytime data = when incoming shortwave radiation is positive
      dplyr::filter(SW_IN_F>0) %>% 
      
      ## take mean over daytime values
      dplyr::mutate(date_day = lubridate::as_date(date_start)) %>% 
      dplyr::group_by(date_day) %>%
      dplyr::summarise(VPD_F_DAY = mean(VPD_F, na.rm=TRUE),
                       VPD_F_DAY_QC = sum(is.element(VPD_F_QC, c(0,1)))/n(),
                       VPD_F_DAY_MDS = mean(VPD_F_MDS, na.rm=TRUE),
                       VPD_F_DAY_MDS_QC = sum(is.element(VPD_F_MDS_QC, c(0,1)))/n(),
                       VPD_DAY_ERA = mean(VPD_ERA, na.rm=TRUE) ) %>% 
      dplyr::rename(date = date_day)
    
    ## write to csv file  
    if (write){
      message(paste("Writing file with daytime VPD as:", filename_dd_vpd))
      readr::write_csv(df, path = filename_dd_vpd)
    }
    
  }
  
  return(df)
}
