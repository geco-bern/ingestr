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
#' @examples \dontrun{settings <- ingest_soilgrids_bysite("id1", lon = 100, lat = 50)}
#'
ingest_soilgrids_bysite <- function(sitename, lon, lat, settings){
  
  siteinfo <- data.frame(
    id = sitename,
    longitude = lon,
    latitude = lat
  )
  
  spdata <- sf::st_as_sf(siteinfo, coords = c("longitude", "latitude"), crs = 4326)
  
  igh <- '+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs'
  spdata_igh <- sf::st_transform(spdata, igh)
  
  data_igh <- data.frame(sf::st_coordinates(spdata_igh), id = spdata_igh$id)
  
  fun_pixel_values  <- function(rowPX, data, VOI, VOI_LYR, factor){
    
    out <- try(gdallocationinfo(
        srcfile = paste0(settings$webdav_path, "/", VOI, "/", VOI_LYR, ".vrt"),
        x = data[rowPX, "X"],
        y = data[rowPX, "Y"],
        geoloc = TRUE,
        valonly = TRUE))
    
    if (class(out) == "try-error"){
      return(rep(NA, length(rowPX)))      
    } else {
      return(as.numeric(out) * factor)
    }
    
  }
  
  #value_pixels <- unlist(lapply(1:nrow(siteinfo), function(x){fun_pixel_values(x, data_igh, settings$voi, settings$voi_layer)}))
  vec_values <- purrr::map_dbl(
    as.list(seq(length(settings$voi_layer))), 
    ~fun_pixel_values(1, data_igh, settings$voi[.], settings$voi_layer[.], settings$factor[.])
  )
  
  ## for association of conversion factors
  df_layer_depth <- tibble(
    layer = 1:6, 
    depth = c(5, 10, 15, 30, 40, 100)
  )
  
  df <- tibble(var = settings$voi, layer = settings$layer, value = vec_values) %>% 
    left_join(df_layer_depth, by = "layer")
  
  z_tot_use <- df_layer_depth %>%
    dplyr::filter(layer %in% unique(settings$layer)) %>%
    summarise(depth_tot_cm = sum(depth)) %>%
    pull(depth_tot_cm)
  
  df <- siteinfo %>%
    as_tibble() %>%
    bind_cols(.,
              df %>% 
                mutate(value_wgt = value * depth) %>% 
                group_by(var) %>% 
                summarise(value = sum(value_wgt)) %>% 
                mutate(value = value / z_tot_use) %>% 
                pivot_wider(id_cols = 1:2, names_from = "var", values_from = "value")) %>% 
    dplyr::select(-longitude, -latitude) %>%
    rename(sitename = id) %>%
    group_by(sitename) %>%
    nest()
  
  return(df)
}