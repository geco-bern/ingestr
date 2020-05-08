df_lonlat <- df_lonlat %>% 
  mutate(africa = FALSE, samerica = FALSE, namerica = FALSE, eurasia = FALSE, australia = FALSE) %>% 
  rowwise() %>% 
  mutate()


df_lonlat <- raster::extract(
  rasta, 
  sp::SpatialPoints(dplyr::select(df_lonlat, lon, lat)), # , proj4string = rasta@crs
  sp = TRUE ) %>%
  as_tibble() %>%
  tidyr::nest(data = c(-lon, -lat)) %>%
  right_join(df_lonlat, by = c("lon", "lat")) %>%
  mutate( data = purrr::map(data, ~dplyr::slice(., 1)) ) %>%
  dplyr::mutate(data = purrr::map(data, ~t(.))) %>%
  dplyr::mutate(data = purrr::map(data, ~as_tibble(.)))

is_eurasia <- function(lon, lat){
  if (-14.0 < lon && lon < 180 && lat > -10 && lat < 83){
    out <- true
  } else {
    out <- false
  }
  return(out)
}

is_africa <- function(lon, lat){
  if (-19 < lon && lon < 55 && lat > -35 && lat < 38){
    out <- true
  } else {
    out <- false
  }
  return(out)
}

is_samerica <- function(lon, lat){
  if (-93 < lon && lon < -32 && lat > -56 && lat < 15){
    out <- true
  } else {
    out <- false
  }
  return(out)
}

is_namerica <- function(lon, lat){
  if (-180 < lon && lon < -52 && lat > 5 && lat < 84){
    out <- true
  } else {
    out <- false
  }
  return(out)
}

is_australia <- function(lon, lat){
  if (112 < lon && lon < 180 && lat > -56 && lat < -10){
    out <- true
  } else {
    out <- false
  }
  return(out)
}

