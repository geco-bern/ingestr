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

  # CRAN compliance, define variables
  linear <- spline <- loess <- sgfilter <- modisvar <- 
    modisvar_filtered <- NULL
    
  if (is.null(sitename)) sitename <- df$sitename[[1]]

  ddf <- df %>%
    tidyr::pivot_longer(cols = c(linear, spline, loess, sgfilter),
                 names_to = "method", 
                 values_to = "modisvar_interpol")

  gg <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = df,
      ggplot2::aes(
        x = date,
        y = modisvar
        ),
      color = "red"
      ) +
    ggplot2::geom_point(
      data = df,
      ggplot2::aes(
        x = date,
        y = modisvar_filtered
        ),
      color = "black"
      ) +
    ggplot2::geom_line(
      data = ddf,
      ggplot2::aes(
        x = date,
        y = modisvar_interpol,
        color = method
        )
      ) +
    ggplot2::labs(
      x = "Date",
      y = settings$varnam,
      title = sitename,
      subtitle = paste(settings$prod, settings$band_var)
    ) +
    ggplot2::ylim(
      0,
      max(df$modisvar, na.rm = TRUE)
    )

  return(gg)
}

