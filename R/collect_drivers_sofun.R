#' Collect all drivers
#'
#' Collect all drivers for site-level simulations 
#' into a nested data frame with one row for each site.
#'
#' @param site_info A data frame containing site meta info (rows for sites). 
#'  Required columns are: \code{"sitename", "year_start", 
#'  "year_end", "lon", "lat", "elv"}. See \code{\link{prepare_setup_sofun}} for
#'  details.
#' @param params_siml A nested data frame with rows for each site containing 
#'  simulation parameters for SOFUN. See \code{\link{run_pmodel_f_bysite}} or
#'  \code{\link{run_biomee_f_bysite}}.
#' @param meteo A nested data frame with rows for each site and meteorological
#'  forcing data time series nested inside a column named \code{"data"}.
#' @param fapar A nested data frame with rows for each site and fAPAR 
#'  forcing data time series nested inside a column named \code{"data"}.
#' @param co2 A nested data frame with rows for each site and CO2 
#'  forcing data time series nested inside a column named \code{"data"}.
#' @param params_soil Soil texture data descriptor, a data frame with columns
#' \code{"layer", "fsand", "fclay", "forg" } and \code{"fgravel"}.
#'
#' @return A \code{rsofun} input data frame (see \link{p_model_drivers} for a detailed
#' description of its structure and contents).
#' @export

collect_drivers_sofun <- function( 
  site_info,
  params_siml,
  meteo,
  fapar,
  co2,
  params_soil
  ){
  
  # declare variable bindings for CRAN compliance
  # these are internal variables created in mutate
  # statements mostly which need to be defined or
  # they will raise a NOTE in CRAN checks
  data <- sitename <- df_count <- n_not_missing <-
    ppfd <- rain <- rain_doy <- ppfd_doy <-
    snow <- snow_doy <- prec <- prec_doy <-
    temp <- temp_doy <- patm <- patm_doy <-
    vpd <- vpd_doy <- ccov <- ccov_doy <-
    fapar_doy <- co2_doy <- tmin <- tmin_doy <-
    tmax <- tmax_doy <- doy <- forcing <- . <- NULL
    
  # complement the setup settings
  site_info <- prepare_setup_sofun(
        site_info = site_info,
        params_siml = params_siml)
  
  # check if all required variables are available
  if (!("snow" %in% names(meteo$data[[1]]))) {
    warning("Variable 'snow' missing in meteo data frame. 
                Assuming zero for all dates. \n")
    meteo <- meteo %>% mutate(data = purrr::map(data,
                              ~dplyr::mutate(., snow = 0)))
  }
  
  if (!("rain" %in% names(meteo$data[[1]]))) {
    warning("Variable 'rain' missing in meteo data frame.
                Assuming equal to 'prec' for all dates. \n")
    meteo <- meteo %>%
     dplyr::mutate(data = purrr::map(data,
                              ~dplyr::mutate(., rain = prec)))
  }
  
  if (!("tmin" %in% names(meteo$data[[1]]))) {
    warning("Variable 'tmin' missing in meteo data frame.
                Assuming equal to 'temp' for all dates.
                (same goes for tmax as assumed paired)\n")
    meteo <- meteo %>%
      dplyr::mutate(data = purrr::map(data,
                               ~dplyr::mutate(., tmin = temp)),
             data = purrr::map(data,
                               ~dplyr::mutate(., tmax = temp)))
  }
  
  vars_req <- c("ppfd", "rain", "snow", "prec",
                "temp", "patm", "vpd", "ccov", "tmin", "tmax")
  
  vars_missing <- vars_req[
    !(vars_req %in% names(meteo %>% tidyr::unnest(data)))
    ]
  
  if (length(vars_missing)) {
   stop(paste("Aborting. Variables missing in meteo data frame:",
                      paste(vars_missing, collapse = ", ")))
   }
  
  # create mega-df containing all forcing data and parameters that
  # vary by site (not model parameters!)
  names_metainfo <- names(site_info)[-which(names(site_info) %in%
                                             c("sitename", "params_siml"))]
  df_mega <- site_info %>% 
    tidyr::nest(site_info = names_metainfo) %>% 
    dplyr::left_join(
      meteo %>% 
        dplyr::rename(meteo = data),
      by = "sitename"
    ) %>% 
    dplyr::left_join(
      fapar %>% 
        dplyr::rename(fapar = data),
      by = "sitename"
    ) %>% 
    dplyr::left_join(
      co2 %>% 
        dplyr::rename(co2 = data),
      by = "sitename"
    ) %>% 
    dplyr::mutate(
      params_soil = purrr::map(as.list(seq(nrow(.))),
                                       ~return(params_soil)))
  
  # use only interpolated fapar and combine meteo data and fapar
  # into a single nested column 'forcing'
  df_mega <- df_mega %>% 
    dplyr::mutate(fapar = purrr::map(fapar, ~dplyr::select(., date, fapar))) %>% 
    dplyr::mutate(co2   = purrr::map(co2  , ~dplyr::select(., date, co2))) %>% 
    dplyr::mutate(forcing = purrr::map2(meteo, fapar, ~dplyr::left_join( .x, .y, by = "date"))) %>% 
    dplyr::mutate(forcing = purrr::map2(forcing, co2, ~dplyr::left_join( .x, .y, by = "date"))) %>% 
    dplyr::select(-meteo, -fapar, -co2)
  
  # drop sites for which forcing data is missing for all dates
  count_notna <- function(df) {
    df %>% 
      dplyr::ungroup() %>% 
      dplyr::summarise(dplyr::across(
      c("ppfd", "rain", "snow", "prec", "temp",
        "patm", "vpd", "ccov", "fapar", "co2",
        "tmin","tmax"), ~sum(!is.na(.)))) %>% 
      tidyr::pivot_longer(cols = 1:12, names_to = "var",
                   values_to = "n_not_missing")
  }
  
  df_missing <- df_mega %>% 
    dplyr::mutate(df_count = purrr::map(forcing, ~count_notna(.))) %>% 
    dplyr::select(sitename, df_count) %>% 
    tidyr::unnest(df_count) %>% 
    dplyr::filter(n_not_missing < 365)
  
  if (nrow(df_missing) > 0) {
    warning("Missing values found in forcing data frame:")
    print(df_missing)
    warning("Respective sites are dropped from all drivers data frame.")
    df_mega <- df_mega %>% 
      dplyr::filter(!(sitename %in% pull(df_missing, sitename)))
  }
    
  ## interpolate to fill gaps in forcing time series
  myapprox <- function(vec){
    if(all(is.na(vec))){
      return(vec)
    } else {
      stats::approx(vec, xout = 1:length(vec))$y
    }
  }
  
  fill_na_forcing <- function(df) {
    
    # dummy variable for CRAN compliance
    ppdf_doy <- NULL
    
    vars <- names(df)[-which(names(df) == "date")]
    df <- df %>% 
      dplyr::mutate_at(vars, myapprox)
    
    ## fill remaining gaps with mean seasonal cycle
    add_doy <- function(string){paste0(string, "_doy")}
    
    df_meandoy <- df %>% 
      dplyr::mutate(doy = lubridate::yday(date)) %>% 
      dplyr::group_by(doy) %>% 
      dplyr::summarise(
        dplyr::across(
          tidyselect::vars_select_helpers$where(is.double),
          ~mean(.x, na.rm = TRUE)
          )
        ) %>% 
      dplyr::rename_with(.fn = add_doy, .cols = dplyr::one_of(
        "ppfd", "rain", "snow", "prec", "temp", "patm",
        "vpd", "ccov", "fapar", "co2", "tmin", "tmax")) %>% 
      dplyr::select(
        doy, 
        dplyr::one_of(
          "ppfd_doy", "rain_doy", "snow_doy", "prec_doy", "temp_doy",
           "patm_doy", "vpd_doy", "ccov_doy", "fapar_doy", "co2_doy",
           "tmin_doy", "tmax_doy"))
    
    df <- df %>% 
      dplyr::mutate(doy = lubridate::yday(date)) %>% 
      dplyr::left_join(df_meandoy, by = "doy") %>% 
      dplyr::mutate(ppfd = ifelse(is.na(ppfd), ppfd_doy, ppfd),
             rain = ifelse(is.na(rain), rain_doy, rain),
             snow = ifelse(is.na(snow), snow_doy, snow),
             prec = ifelse(is.na(prec), prec_doy, prec),
             temp = ifelse(is.na(temp), temp_doy, temp),
             patm = ifelse(is.na(patm), patm_doy, patm),
             vpd = ifelse(is.na(vpd), vpd_doy, vpd),
             ccov = ifelse(is.na(ccov), ccov_doy, ccov),
             fapar = ifelse(is.na(fapar), fapar_doy, fapar),
             co2 = ifelse(is.na(co2), co2_doy, co2),
             tmin = ifelse(is.na(tmin), tmin_doy, tmin),
             tmax = ifelse(is.na(tmax), tmax_doy, tmax)) %>% 
      dplyr::select(-ends_with("_doy"))
    
    return(df)
  }
  
  df_mega <- df_mega %>% 
    mutate(forcing = purrr::map(forcing, ~fill_na_forcing(.))) %>% 
    dplyr::select(sitename, forcing, params_siml, site_info, params_soil)

  return(df_mega)
}
