#' WISE30sec ingest
#'
#' Ingests WISE30sec data.
#' This follows code outlined here:
#'  \url{https://gis.stackexchange.com/questions/246476/link-raster-tiff-with-attribute-table-txt-in-qgis-or-r}
#'
#' @param var A character string specifying the variable name
#'  (WISE30sec standard)
#' @param df_lonlat A data frame containing columns \code{lon} and
#'  \code{lat} specifying points for extraction.
#' @param layer An integer specifying soil layer.
#' See \url{https://www.isric.org/documents/document-type/isric-report-201501-world-soil-property-estimates-broad-scale-modelling} for available layers.
#' Defaults to \code{1}.
#' @param dir A character string specifying the path to the local directory
#'  containing WISE30sec data.
#' @return A data frame with ingested data
#' @export
#'
#' @examples \dontrun{settings <- ingest_wise_byvar("CNrt")}
#'
ingest_wise_byvar <- function(var, df_lonlat, layer = 1, dir){

  # CRAN compliance, variable declaration
  lon <- lat <- ID <- NEWSUID <- PROP <- Layer <- var_wgt <-
    depth_cm <- depth_tot_cm <- wise30sec_fin <- 
    modisvar_interpol <- NULL
  
  # read as a raster
  rasta <- raster::raster(paste0(dir, "/GISfiles/wise30sec_fin"))

  # read data table
  # variables are described in ISRIC_Report_2015_01.pdf, p. 53
  df <- readr::read_csv(paste0(dir, "/Interchangeable_format/HW30s_FULL.txt"))

  # extract the ID for a given location (lon/lat)
  df_out <- raster::extract(
    rasta,
    sp::SpatialPoints(df_lonlat %>% dplyr::select(lon, lat)),
    sp = TRUE
    ) %>%
    as_tibble() %>%
    rename(ID = wise30sec_fin)

  # get the NEWSUID for a given ID
  df_out <- raster::levels(rasta)[[1]] %>%
    as_tibble() %>%
    dplyr::filter(ID %in% df_out$ID) %>%
    right_join(df_out, by = "ID") %>%
    dplyr::select(lon, lat, NEWSUID)

  # map unit-area-weighted CN ratio for data table
  df <- df %>%
    dplyr::filter(NEWSUID %in% unique(as.vector(df_out$NEWSUID))) %>%
    mutate(PROP = PROP / 100) %>%
    rename(var := !!var) %>%
    mutate(var_wgt = var * PROP) %>%
    group_by(NEWSUID, Layer) %>%
    summarise(var := sum(var_wgt))

  # soil layer depth-weighted CN ratio
  uselayer <- paste0("D", layer)
  df_z_layer <- tibble(Layer = paste0("D", 1:7),
                       depth_cm = c(20, 20, 20, 20, 20, 50, 50))
  z_tot_use <- df_z_layer %>%
    dplyr::filter(Layer %in% uselayer) %>%
    summarise(depth_tot_cm = sum(depth_cm)) %>%
    pull(depth_tot_cm)

  # weighted sum, weighting by layer depth
  df <- df %>%
    left_join(df_z_layer, by = "Layer") %>%
    dplyr::filter(Layer %in% uselayer) %>%
    mutate(var_wgt = var * depth_cm / z_tot_use) %>%
    group_by(NEWSUID) %>%
    summarise(var := sum(var_wgt))

  # join to output table
  df_out <- df_out %>%
    left_join(df, by = "NEWSUID") %>%
    dplyr::select(-NEWSUID) %>%
    rename(!!var := var)

  return(df_out)
}
