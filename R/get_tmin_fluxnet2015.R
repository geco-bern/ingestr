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
#' @examples df <- get_vpd_day_fluxnet2015("./")
#' 
get_tmin_fluxnet2015 <- function(dir){
  
  ## loop over all HH files in the directory 'dir'
  out <- purrr::map( as.list(list.files(dir, pattern = "HH")),
              ~get_tmin_fluxnet2015_byfile(paste0(dir, .)))
  
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
#' @examples df <- get_tmin_fluxnet2015_byfile("./FLX_BE-Vie_FLUXNET2015_FULLSET_HH_1996-2014_1-3.csv")
#' 
get_tmin_fluxnet2015_byfile <- function(filename_hh, write=FALSE){
  
  filename_dd_tmin <- filename_hh %>% 
    stringr::str_replace("HH", "DD") %>% 
    stringr::str_replace(".csv", "_TMIN.csv")
  
  if (file.exists(filename_dd_tmin)){
    ## Daytime VPD file is already available, reading from file
    # print(paste("Reading daytime VPD from:", filename_dd_tmin))
    rlang::inform(paste("Reading file with calculated daily TMIN:", filename_dd_tmin))
    df <- readr::read_csv(filename_dd_tmin)
    
  } else {
    ## Get daytime VPD from half-hourly data
    ## read half-hourly data
    if (!file.exists(filename_hh)){
      rlang::abort(paste("Half-hourly file does not exist:", filename_hh))
    } 
    
    df <- readr::read_csv(filename_hh) %>% 
      dplyr::mutate( date_start = lubridate::ymd_hm( TIMESTAMP_START ),
                     date_end   = lubridate::ymd_hm( TIMESTAMP_END ) ) %>%
      dplyr::mutate( date = date_start ) %>% 
      
      ## take mean over daytime values
      dplyr::mutate(date_day = lubridate::as_date(date_start)) %>% 
      dplyr::group_by(date_day) %>%
      dplyr::summarise(TMIN_F = min(TA_F, na.rm=TRUE),
                       TMIN_F_QC = sum(is.element(TA_F_QC, c(0,1)))/n(),
                       TMIN_F_MDS = min(TA_F_MDS, na.rm=TRUE),
                       TMIN_F_MDS_QC = sum(is.element(TA_F_MDS_QC, c(0,1)))/n(),
                       TMIN_ERA = min(TA_ERA, na.rm=TRUE) ) %>% 
      dplyr::rename(date = date_day)
    
    ## write to csv file  
    if (write){
      rlang::inform(paste("Writing file with minimum temperature as:", filename_dd_tmin))
      readr::write_csv(df, path = filename_dd_tmin)
    }
    
  }
  
  return(df)
}
