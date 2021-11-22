#!/usr/bin/Rscript
#
# Compiles the meta-data table for all
# fluxnet 2015 sites

#---- System check & libraries ----
if(!grepl('eu-', Sys.info()['nodename'])){
  stop("You are not on Euler, source data unavailable - abort abort abort!")
}

# load libraries
library(tidyverse)
library(raster)
library(amerifluxr)

#----- load official site info -----

message("donwloading ameriflux site info")

siteinfo <- amerifluxr::amf_site_info()
siteinfo <- siteinfo %>%
  dplyr::rename(
    elv = LOCATION_ELEV, 
    sitename = SITE_ID, 
    lon = LOCATION_LONG, 
    lat = LOCATION_LAT,
    year_start = DATA_START, 
    year_end = DATA_END,
    classid = IGBP,
    koeppen_code = CLIMATE_KOEPPEN
  ) %>%
  dplyr::filter(
    DATA_POLICY != "LEGACY"
  ) %>%
  dplyr::select(
    sitename,
    lon,
    lat,
    elv,
    year_start,
    year_end,
    classid,
    koeppen_code
    )

#----- extract start and end date from the filenames -----

message("grabbing info from fluxnet filenames")

fluxnet2015 <- "~/data/flux_data_kit/fluxnet2015/"
flux_files <- data.frame(filename = list.files(
  fluxnet2015,
  pattern = "FULLSET_HH_",
  recursive = TRUE,
  full.names = FALSE
)) %>%
  dplyr::mutate(
    filename = basename(filename),
    sitename = substr(filename, start = 5, stop = 10),
    year_start_file = substr(filename, start = 34, stop = 37),
    year_end_file = substr(filename, start = 39, stop = 42)
  ) %>%
  dplyr::select(-filename)

siteinfo <- left_join(siteinfo, flux_files) %>%
  dplyr::mutate(
    year_start = ifelse(is.na(year_start), year_start_file, year_start),
    year_end = ifelse(is.na(year_end), year_end_file, year_end)
  ) %>%
  dplyr::select(
    -year_start_file,
    -year_end_file
  )

#----- fill values with etopo1 data -----

message("fixing elevation data with etopo1 data")

topo <- ingestr::ingest(
      siteinfo,
      source = "etopo1",
      dir = "~/data/etopo/"
    ) %>% 
    tidyr::unnest(data) %>%
    rename(
	elv_etopo = elv
	)

print(topo)

siteinfo <- siteinfo %>%
  left_join(topo,
    by = "sitename")

siteinfo <- siteinfo %>% 
  dplyr::mutate(elv = ifelse(is.na(elv), elv_etopo, elv)) %>%
  dplyr::select(-elv_etopo)

#----- assign C4 sites (limited number hence manual) -----

message("assigning C4/C3 labels")

c4sites <-  c(
  "AU-How",
  "DE-Kli",
  "FR-Gri",
  "IT-BCi",
  "US-Ne1",
  "US-Ne2",
  "US-Ne3"
  )

siteinfo <- siteinfo %>%
  mutate(c4 = ifelse(sitename %in% c4sites, TRUE, FALSE))

#----- get water holding capacity ----

message("assigning water holding capacity - from file")

siteinfo <- read_csv(
  "data-raw/fluxnet2015_metadata/siteinfo_fluxnet2015_sofun+whc.csv") %>%
			rename(sitename = mysitename) %>%
            dplyr::select( sitename, whc ) %>%
            left_join(siteinfo, by = "sitename")

#---- add fluxnet id ----
siteinfo_falge <- read_csv(
  "data-raw/fluxnet2015_metadata/fluxnet_site_info_all.csv") %>%
  dplyr::select(-sitename) %>% 
  dplyr::rename( sitename = fluxnetid ) %>% 
  dplyr::select( sitename, koeppen_climate, gtopo30_elevation ) %>%
  dplyr::mutate( koeppen_climate = str_split( koeppen_climate, " - " ) ) %>%
  dplyr::mutate( koeppen_code = purrr::map( koeppen_climate, 1 ) ) %>%
  dplyr::mutate( koeppen_word = purrr::map( koeppen_climate, 2 ) ) %>%
  unnest( koeppen_code ) %>%
  unnest( koeppen_word )

# add info: number of data points (daily GPP)
siteinfo <- siteinfo %>%
  left_join(
    meta,
    by = "sitename")

#---- create a legend for the koeppen geiger climate codes -----

# Second, extract the class from a global map, complement missing in above
# File by Beck et al. (2018) Scientific Data, DOI: 10.1038/sdata.2018.214
kg <- raster("data-raw/koeppen_geiger/koeppen-geiger.tif")
kg_class <- read_csv("data-raw/koeppen_geiger/koppen-geiger_legend.csv") %>% 
  setNames( c("kgnumber", "koeppen_code_extr"))

siteinfo <- siteinfo %>%
  mutate(
    kgnumber = raster::extract(kg, data.frame(x=.$lon, y=.$lat))
    ) %>% 
  left_join(kg_class, by = "kgnumber") %>%
  mutate(
    koeppen_code = ifelse(is.na(koeppen_code),koeppen_code_extr,koeppen_code)
    ) %>%
  dplyr::select(-koeppen_code_extr)

print(siteinfo)

#---- sort stuff -----

siteinfo <- siteinfo %>% 
  dplyr::select(
    sitename,
    lon,
    lat,
    elv,
    year_start,
    year_end,
    classid,
    c4,
    whc,
    koeppen_code,
    igbp_land_use,
    plant_functional_type
    )

#----- save the data as rda file -----
#save(siteinfo, file = "data/siteinfo_fluxnet2015.rda")
