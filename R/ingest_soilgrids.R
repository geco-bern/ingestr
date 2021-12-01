#' SoilGrids point ingest
#'
#' SoilGrids point data ingest
#' This follows code outlined here: \url{https://git.wur.nl/isric/soilgrids/soilgrids.notebooks/-/blob/master/markdown/xy_info_from_R.md}
#'
#' @param sitename A character string specifying the site name (ID)
#' @param lon Longitude (numeric)
#' @param lat Latitude (numeric)
#' @param settings A list returned by a call to \link{get_settings_soilgrids}
#' @return A data frame with ingested data, nested in column data.
#' @export
#'
#' @examples \dontrun{settings <- ingest_soilgrids("id1", lon = 100, lat = 50)}
#'
ingest_soilgrids <- function(siteinfo, settings){
  
  # CRAN compliance, declaring unstated variables
  sitename <- lon <- lat <- layer <- depth <- depth_tot_cm <- 
    x <- y <- X <- Y <- value <- name <- value_wgt <- NULL
  
  siteinfo <- siteinfo %>% 
    dplyr::select(id = sitename,
                  longitude = lon,
                  latitude = lat)
  
  spdata <- sf::st_as_sf(siteinfo, coords = c("longitude", "latitude"), crs = 4326)
  
  igh <- '+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs'
  spdata_igh <- sf::st_transform(spdata, igh)
  
  data_igh <- data.frame(sf::st_coordinates(spdata_igh), id = spdata_igh$id)
  
  fun_pixel_values  <- function(data, VOI, VOI_LYR, factor){
    
    
    
    out <- try(gdalUtils::gdallocationinfo(
      srcfile = paste0(settings$webdav_path, "/", VOI, "/", VOI_LYR, ".vrt"),
      coords  = as.matrix(data[, c("X", "Y")]),
      geoloc  = TRUE,
      valonly = TRUE))
    
    soillayer_nam <- stringr::str_remove(VOI_LYR, paste0(VOI, "_"))
    
    if (length(out) > 0 && class(out) != "try-error"){
      df <- data %>% 
        mutate(value = as.numeric(out) * factor,
               name = !!VOI,
               soillayer = !!soillayer_nam)
    } else {
      df <- data %>% mutate(value = NA)
    }
    
    return(df)
  }
  
  ## for association of conversion factors
  df_layer_depth <- tibble(
    layer = 1:6, 
    depth = c(5, 10, 15, 30, 40, 100),
    soillayer = c("0-5cm_mean", "5-15cm_mean", "15-30cm_mean", "30-60cm_mean", "60-100cm_mean", "100-200cm_mean")
  )
  
  z_tot_use <- df_layer_depth %>%
    dplyr::filter(layer %in% unique(settings$layer)) %>%
    summarise(depth_tot_cm = sum(depth)) %>%
    pull(depth_tot_cm)
  
  ## extracting only for single site (data_igh has one row)
  df <- purrr::map(
    as.list(seq(length(settings$voi_layer))), 
    ~fun_pixel_values(data_igh, settings$voi[.], settings$voi_layer[.], settings$factor[.])) %>% 
    bind_rows() %>% 
    as_tibble() %>% 
    dplyr::select(-X, -Y) %>% 
    left_join(df_layer_depth, by = "soillayer") %>% 
    mutate(value_wgt = value * depth) %>% 
    group_by(id, name) %>% 
    summarise(value = sum(value_wgt)) %>% 
    mutate(value = value / z_tot_use) %>% 
    ungroup() %>% 
    tidyr::pivot_wider(names_from = "name", values_from = "value") %>% 
    rename(sitename = id)
  
  return(df)
}