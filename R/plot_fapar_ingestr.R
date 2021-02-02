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

  # df2 <- df %>%
  #   tidyr::drop_na(linear) %>%
  #   pivot_longer(c(linear, spline, loess), names_to = "interpolation_method", values_to = "data_interpolated") %>%
  #   left_join(
  #     df %>%
  #       tidyr::drop_na(linear) %>%
  #       pivot_longer(c(modisvar, modisvar_filtered), names_to = "filter_level", values_to = "data_modis"),
  #     by = "date"
  #   )

  # gg <- ggplot() +
  #   geom_point(data = df2, aes(x = date, y = data_modis, color = filter_level)) +
  #   geom_line(data = df2, aes(x = date, y = data_interpolated, color = interpolation_method))

  ddf <- df %>%
    pivot_longer(cols = c(linear, spline, loess, sgfilter), names_to = "method", values_to = "modisvar_interpol")

  gg <- ggplot() +
    geom_point(  data = df, aes(x = date, y = modisvar), color = "red") +
    # geom_point(  data = df, aes(x = date, y = modisvar_filled), col = 'blue') +
    geom_point(  data = df, aes(x = date, y = modisvar_filtered), color = "black") +
    geom_line(  data = ddf, aes(x = date, y = modisvar_interpol, color = method)) +
    # geom_line(  data = df, aes(x = date, y = linear, color = "linear")) +
    # geom_line(  data = df, aes(x = date, y = spline, color = "spline")) +
    # geom_line(  data = df, aes(x = date, y = loess, color = "loess")) +
    # geom_line(  data = df, aes(x = date, y = sgfilter), col = 'green') +
    labs(x = "Date", y = settings$varnam, title = sitename, subtitle = paste(settings$prod, settings$band_var)) +
    # scale_color_manual(name = "Interpolation",
    #                    breaks = c("linear", "loess"),
    #                    # breaks = c("linear", "spline", "loess"),
    #                    # values = c("linear" = "red", "spline" = "cyan", "loess" = "blue") )
    #                    values = c("linear" = "red", "loess" = "blue") )
    ylim(0, max(df$modisvar, na.rm = TRUE))

# scale_color_manual(name = "Filtering",
    #                    breaks = c("filtered", "kept"),
    #                    values = c("kept" = "black", "filtered" = "red") )


  # gg <- ggplot() +
  #   geom_point(  data = df, aes(x = date, y = modisvar), col = 'red') +
  #   # geom_point(  data = df, aes(x = date, y = modisvar_filled), col = 'blue') +
  #   geom_point(  data = df, aes(x = date, y = modisvar_filtered), col = 'black') +
  #   geom_line(  data = df, aes(x = date, y = linear), col = 'red') +
  #   geom_line(  data = df, aes(x = date, y = spline), col = 'cyan') +
  #   geom_line(  data = df, aes(x = date, y = loess), col = 'blue') +
  #   geom_line(  data = df, aes(x = date, y = sgfilter), col = 'green') +
  #   labs(x = "Date", y = "fAPAR", title = sitename, subtitle = settings$prod)

  return(gg)
}

