#' Get daily maximum temperature for all files in a directory.
#'
#' Wrapper function to derive daily daily maximum temperature from half-hourly data 
#' for all site-scale data files in a given directory (argument \code{dir}).
#'
#' @param dir A character string specifying the directory in which to look
#' for site-specific half-hourly data files.
#'
#' @return A list of outputs of the function \link{get_tmax_fluxnet2015}.
#' @export
#'
#' @examples
#' \dontrun{
#'  df <- get_tmax_fluxnet2015("./")
#' }
#' 
get_tmax_fluxnet2015 <- function(dir){
  
  # CRAN compliance, define variables
  TIMESTAMP_START <- TIMESTAMP_END <- date_start <- date_day <- TA_F <-
    TA_F_MDS <- TA_F_QC <- TA_F_MDS_QC <- TA_ERA <- NULL
  
  ## loop over all HH files in the directory 'dir'
  out <- purrr::map( as.list(list.files(dir, pattern = "HH")),
              ~get_tmax_fluxnet2015_byfile(paste0(dir, .)))
  
  return(out)
}

#' Get daily maximum temperature
#'
#' Derive daily maximum temperature from half-hourly 
#' data.
#'
#' @param filename_hh A character string specifying the file name containing
#' site-specific half-hourly (or hourly) data.
#' @param write A logical specifying whether daiily daytime tmax should be 
#' written to a file.
#'
#' @return A data frame (tibble) containing daily daytime tmax.
#' @export
#'
#' @examples 
#' \dontrun{
#' df <- 
#' get_tmax_fluxnet2015_byfile(
#' "./FLX_BE-Vie_FLUXNET2015_FULLSET_HH_1996-2014_1-3.csv"
#' )
#' }

get_tmax_fluxnet2015_byfile <- function(filename_hh, write=FALSE){
  
  # CRAN compliance, define variables
  TIMESTAMP_START <- TIMESTAMP_END <- date_start <- date_day <- TA_F <-
    TA_F_MDS <- TA_F_QC <- TA_F_MDS_QC <- TA_ERA <- NULL
  
  filename_dd_tmax <- filename_hh %>% 
    stringr::str_replace("HH", "DD") %>% 
    stringr::str_replace(".csv", "_TMAX.csv")
  
  if (file.exists(filename_dd_tmax)){
    # Daytime tmax file is already available, reading from file
    # print(paste("Reading daytime tmax from:", filename_dd_tmax))
    message(paste("Reading file with calculated daily tmax:", filename_dd_tmax))
    df <- readr::read_csv(filename_dd_tmax)
    
  } else {
    # Get daytime tmax from half-hourly data
    # read half-hourly data
    if (!file.exists(filename_hh)){
      stop(paste("Half-hourly file does not exist:", filename_hh))
    } 
    
    df <- readr::read_csv(filename_hh) %>% 
      dplyr::mutate( date_start = lubridate::ymd_hm( TIMESTAMP_START ),
                     date_end   = lubridate::ymd_hm( TIMESTAMP_END ) ) %>%
      dplyr::mutate( date = date_start ) %>% 
      
      # take mean over daytime values
      dplyr::mutate(date_day = lubridate::as_date(date_start)) %>% 
      dplyr::group_by(date_day) %>%
      dplyr::summarise(TMAX_F = max(TA_F, na.rm=TRUE),
                       TMAX_F_QC = sum(is.element(TA_F_QC, c(0,1)))/n(),
                       TMAX_F_MDS = max(TA_F_MDS, na.rm=TRUE),
                       TMAX_F_MDS_QC = sum(is.element(TA_F_MDS_QC, c(0,1)))/n(),
                       TMAX_ERA = max(TA_ERA, na.rm=TRUE) ) %>% 
      dplyr::rename(date = date_day)
    
    # write to csv file  
    if (write){
      message(paste("Writing file with maximum temperature as:", filename_dd_tmax))
      readr::write_csv(df, path = filename_dd_tmax)
    }
  }
  
  return(df)
}
