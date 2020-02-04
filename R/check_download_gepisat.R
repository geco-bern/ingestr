#' GePiSaT data availability check
#'
#' Checks if GePiSaT files are available for this variable and initiates download if not.
#'
#' @param path A character string specifying the local path of GePiSaT data
#' @param sitename A character string specifying the site name for which GePiSaT data is searched (based on the site name appearing as part of the respective file name). Defaults to NA.
#'
#' @return Error
#' @export
#'
#' @examples error <- check_download_gepisat( "./" )
#' 
check_download_gepisat <- function( path, sitename = NA, path_remote = NA, uname = NA, address_remote = NA ){

  error <- 0
  
  ## Check if any data is available in the specified directory
  getfiles <- list.files( path, pattern = ".*_daily_GPP.txt" )

  if (length(getfiles)==0){
    ## No files found at specified location
    rlang::warn( paste0("No files found for GePiSaT in directory ", path) )

    ## Search at a different location?
    path <- readline( prompt="Would you like to search for files recursively from a certain directory? Enter the path from which search is to be done: ")
    getfiles <- list.files( path, pattern = ".*_daily_GPP.txt", recursive = TRUE )

    if (length(getfiles)==0){
      ## Search from home
      rlang::warn( paste0("Still nothing found at specified location ", path ) )

      ## Search recursively from home directory?
      ans <- readline( prompt="Would you like to search for files recursively from your home directory (y/n): ")
      if (ans=="y") getfiles <- list.files( "~/", pattern = ".*_daily_GPP.txt", recursive = TRUE )
      
      if (length(getfiles)==0){
        ## Still no files found at specified location. Try to download from remote server and place in <path>
        abort( "No GePiSaT files downloaded." )

      } else if (!is.na(uname)&&!is.na(address_remote)&&!is.na(path_remote)) {

        getfiles <- system( paste0( "ssh ", uname, "@", address_remote, " ls ", path_remote ), intern = TRUE )
        getfiles <- getfiles[ grepl("_daily_GPP", getfiles) ]

        ## Download only data for a specific site
        if (!is.na(sitename)) getfiles <- getfiles[ grepl(sitename, getfiles) ]

        ## Still no files found at specified location. Try to download from remote server and place in <path>
        rlang::warn( "Initiating download from remote server..." )
        error <- download_from_remote( 
          path_remote_gepisat,
          path,
          getfiles = getfiles,
          uname = uname, 
          address_remote = address_remote 
         )
        getfiles <- list.files( path, pattern = ".*_daily_GPP.txt" )
        if (length(getfiles)==0) abort("check_download_gepisat(): No files downloaded.")
      }

    }

  }
  return(error)
}
