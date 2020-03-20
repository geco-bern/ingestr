#' Plot fAPAR for one site
#'
#' Visualise the fAPR data filtering, imputing, and interpolation to daily values.
#' 
#' @param df The data frame returned by \code{ingest_bysite( source = "gee" )}
#' @param settings A list of additional settings used for reading original files.
#' @return A ggplot object.
#' @export
#'
#' @examples \dontrun{plot_fapar_ingestr(df, settings)}
#' 
plot_fapar_ingestr <- function(df, settings){
  
  out <- purrr::map(
    as.list(seq(nrow(df))),
    ~plot_fapar_ingestr_bysite(
      df$data[[.]],
      settings,
      sitename = df$sitename[[.]]
    )
  )
  
  return(out)
}


#' Plot fAPAR for one site
#'
#' Visualise the fAPR data filtering, imputing, and interpolation to daily values.
#' 
#' @param df The data frame returned by \code{ingest_bysite( source = "gee" )}
#' @param settings A list of additional settings used for reading original files.
#' @return A ggplot object.
#' @export
#'
#' @examples \dontrun{plot_fapar_ingestr(df, settings)}
#' 
plot_fapar_ingestr_bysite <- function(df, settings, sitename = NULL){
  
  if (is.null(sitename)) sitename <- df$sitename[[1]]
  
  if (settings$prod == "MODIS/006/MCD15A3H"){
    
    df <- df %>% 
      tidyr::drop_na(linear)
    
    gg <- ggplot() +
      geom_point(  data = df, aes(x = date, y = modisvar), col = 'red') +
      geom_point(  data = df, aes(x = date, y = modisvar_filled), col = 'blue') +
      geom_point(  data = df, aes(x = date, y = modisvar_filtered), col = 'black') +
      geom_line(  data = df, aes(x = date, y = linear), col = 'red') +
      geom_line(  data = df, aes(x = date, y = spline), col = 'cyan') +
      geom_line(  data = df, aes(x = date, y = loess), col = 'blue') +
      labs(x = "Date", y = "fAPAR", title = sitename, subtitle = "MODIS FPAR MODIS/006/MCD15A3H")
    
  }
  
  return(gg)
}

