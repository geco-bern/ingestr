# Pascals Testing File: ----
library(ingestr)
library(tidyverse)

# Cluster Checks: ----
## setup ----
source    <- "wfde5"
dir       <- "~/data/wfde5/"
timescale <- "h"
vars_out  <- c("wind", "ppfd", "temp")
vars_out  <- c("wind")
siteinfo  <- siteinfo_fluxnet2015 %>% filter(sitename == "FR-Pue") %>% mutate(year_start = 2007, year_end = 2007)

## ingest() calls ----
## .................................................................................................
for (i in c(1960, 1980, 2007)) {
  message("* Year: ", i, " | Vars: ", vars_out, " ------------------------ \n")
  message("** Without Bias Correction \n")
  
  i -> siteinfo$year_start -> siteinfo$year_end
  test_ingest    <- ingest(siteinfo = siteinfo, getvars = vars_out, source = source, dir = dir, timescale = timescale)
  test_ingest_by <- ingest_bysite(sitename  = siteinfo$sitename,  getvars = vars_out, source = source, dir = dir, year_start = siteinfo$year_start, year_end = siteinfo$year_end, lon = siteinfo$lon, lat = siteinfo$lat, elv = 400, verbose = TRUE, timescale  = timescale)
  
  v1 <- test_ingest %>% unnest(data) %>% pull(all_of(vars_out))
  message("*** ingest(): ", v1[length(v1)/2], "\n")
  
  v2 <- test_ingest_by %>% pull(all_of(vars_out))
  message("*** ingest_bysite(): ", v2[length(v2)/2], "\n")
  
  message("*** The same output? \n")
  unique(v1 == v2)
  
  
  message("** With Bias Correction \n")
  test_ingest    <- ingest(siteinfo = siteinfo, getvars = vars_out, source = source, dir = dir, timescale = timescale, settings  = list(correct_bias = "worldclim", dir_bias = "~/data/worldclim"))
  test_ingest_by <- ingest_bysite(sitename  = siteinfo$sitename,  getvars = vars_out, source = source, dir = dir, year_start = siteinfo$year_start, year_end = siteinfo$year_end, lon = siteinfo$lon, lat = siteinfo$lat, elv = 400, verbose = TRUE, timescale  = timescale, settings  = list(correct_bias = "worldclim", dir_bias = "~/data/worldclim"))
  
  v1 <- test_ingest %>% unnest(data) %>% pull(all_of(vars_out))
  message("*** ingest(): ", v1[length(v1)/2], "\n")
  
  v2 <- test_ingest_by %>% pull(all_of(vars_out))
  message("*** ingest_bysite(): ", v2[length(v2)/2], "\n")
  
  message("*** The same output? \n")
  unique(v1 == v2)
}

## .................................................................................................
## .................................................................................................
## .................................................................................................
# Local Checks: ####
## Get site information
library(ingestr)
library(tidyverse)
library(patchwork)

## Call for standard Watch-WFDEI data
df_watch <- ingest_bysite(
  sitename  = "FR-Pue",
  source    = "watch_wfdei",
  getvars   = c("temp"),
  dir       = "~/data/watch_wfdei/",
  timescale = "d",
  year_start = 2007,
  year_end  = 2007,
  lon       = 3.5958,
  lat       = 43.7414,
  verbose   = TRUE,
  settings  = list(correct_bias = "worldclim", dir_bias = "~/data/worldclim")
)

df_watch


## Call Structure ####
### 1.) ingest_bysite() ####
# Arguments:
sitename   <- "FR-Pue"
source     <- "watch_wfdei"
getvars    <- c("temp")
dir        <- "~/data/watch_wfdei/"
timescale  <- "d"
year_start <- 2007
year_end   <- 2007
lon        <- 3.5958
lat        <- 43.7414
verbose    <- TRUE
settings   <- list(correct_bias = "worldclim", dir_bias = "~/data/worldclim")

siteinfo <- tibble(
  sitename = sitename,
  lon = lon,
  lat = lat) %>%
  mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>%
  mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31")))

### 2.) ingest_globalfield() ####
df_tmp <- ingest_globalfields(siteinfo,
                              source = source,
                              getvars = getvars,
                              dir = dir,
                              timescale = timescale,
                              verbose = FALSE
)

### 3.) Get Hourly Dataframe ####
year_start <- 2007
year_end   <- 2007
timescale  <- "h"

df <- init_dates_dataframe(
  year_start,
  year_end,
  noleap = TRUE,
  timescale = timescale
)
# dplyr::select(-year_dec)

if (timescale=="m"){
  df <- df %>%
    mutate(month = lubridate::month(date), year = lubridate::year(date))
} else if (timescale=="y"){
  df <- df %>%
    mutate(year = lubridate::year(date))
}

### 4.) Get variables (here only for temperature copy-pasted) ####
# Call in ingest_bysite(): -> do not call here
df_tmp <- ingest_globalfields(siteinfo,
                              source = source,
                              getvars = getvars,
                              dir = dir,
                              timescale = timescale,
                              verbose = FALSE
)

source("~/projects/ingestr/R/ingest_globalfields.R") # to get ..._byvar functions

df_out <- purrr::map(
  as.list(seq(nrow(siteinfo))),
  ~ingestr::init_dates_dataframe(
    lubridate::year(siteinfo$date_start[.]),
    lubridate::year(siteinfo$date_end[.]),
    noleap = TRUE,
    timescale = timescale))

names(df_out) <- siteinfo$sitename

df_out <- df_out %>%
  bind_rows(.id = "sitename")

### 5.) Call ingest..._byvar() (temp. only here:) ####
# (do not call this here)
if ("temp" %in% getvars && !("temp" %in% names(df_out))){
  df_out <- ingest_globalfields_watch_byvar( df_out, siteinfo, dir, "Tair_daily" ) %>%
    dplyr::rename(temp = myvar) %>%
    dplyr::mutate(temp = temp - 273.15) %>%
    dplyr::right_join(df_out, by = c("sitename", "date"))
}

### 6.) Original ingest..._byvar(): ####

varnam  <- "Tair_daily"
ddf     <- df_out ## Function takes ddf as input which is same as df_out (see lines above)

dirn <- paste0( dir, "/", varnam, "/" )

## loop over all year and months that are required
year_start <- ddf %>%
  dplyr::pull(date) %>%
  min() %>%
  lubridate::year()

year_end <- ddf %>%
  dplyr::pull(date) %>%
  max() %>%
  lubridate::year()

## check if data is required for years before 1979 (when watch wfdei is available)
pre_data <- year_start < 1979

## if pre-1979 data are required, read at least 10 first years to get mean climatology
if (pre_data){
  year_start_read <- 1979
  year_end_read <- max(1988, year_end)
} else {
  year_start_read <- year_start
  year_end_read <- year_end
}

## construct data frame holding longitude and latitude info
df_lonlat <- tibble(
  sitename = siteinfo$sitename,
  lon      = siteinfo$lon,
  lat      = siteinfo$lat
)

if (varnam %in% c("Rainf_daily", "Snowf_daily")){
  addstring <- "_WFDEI_CRU_"
} else {
  addstring <- "_WFDEI_"
}

## extract all the data for all the dates (cutting to required dates by site is done in ingest())
allmonths <- 1:12
allyears <- year_start_read:year_end_read
df <- expand.grid(allmonths, allyears) %>%
  dplyr::as_tibble() %>%
  setNames(c("mo", "yr")) %>%
  rowwise() %>%
  dplyr::mutate(filename = paste0( dirn, "/", varnam, addstring, sprintf( "%4d", yr ), sprintf( "%02d", mo ), ".nc" )) %>%
  ungroup() %>%
  dplyr::mutate(data = purrr::map(filename, ~extract_pointdata_allsites(., df_lonlat, get_time = FALSE ) ))

## rearrange to a daily data frame
complement_df <- function(df){
  df <- df %>%
    setNames(., c("myvar")) %>%
    mutate( dom = 1:nrow(.))
  return(df)
}

ddf <- df %>%
  tidyr::unnest(data) %>%
  dplyr::mutate(data = purrr::map(data, ~complement_df(.))) %>%
  tidyr::unnest(data) %>%
  dplyr::select(sitename, mo, yr, dom, myvar) %>%
  dplyr::mutate(date = lubridate::ymd(paste0(as.character(yr), "-", sprintf( "%02d", mo), "-", sprintf( "%02d", dom))) ) %>%
  dplyr::select(-mo, -yr, -dom)

## create data frame containing all dates, using mean annual cycle (of 1979-1988) for all years before 1979
if (pre_data){
  rlang::inform("Data for years before 1979 requested. Taking mean annual cycle of 10 years (1979-1988) for all years before 1979.")
  
  ## get mean seasonal cycle, averaged over 1979:1988
  ddf_meandoy <- ddf %>% 
    dplyr::filter(lubridate::year(date) %in% 1979:1988) %>% 
    mutate(doy = lubridate::yday(date)) %>% 
    group_by(sitename, doy) %>% 
    summarise(myvar = mean(myvar))
  
  ## get a data frame with all dates for all sites
  ddf_tmp <- purrr::map(
    as.list(seq(nrow(siteinfo))),
    ~ingestr::init_dates_dataframe(
      lubridate::year(siteinfo$date_start[.]),
      min(1978, lubridate::year(siteinfo$date_end[.])),
      noleap = TRUE,
      timescale = "d"))
  names(ddf_tmp) <- siteinfo$sitename
  ddf_pre <- ddf_tmp %>%
    bind_rows(.id = "sitename") %>%
    drop_na() %>% 
    mutate(doy = lubridate::yday(date)) %>%
    left_join(ddf_meandoy, by = c("sitename", "doy")) %>%
    dplyr::select(-doy)
  
  # ddf_pre <- init_dates_dataframe(year_start, min(1978, year_end)) %>% 
  #   mutate(doy = lubridate::yday(date)) %>% 
  #   left_join(ddf_pre, by = "doy") %>% 
  #   dplyr::select(-doy)
  
  ## combine the two along rows
  ddf <- left_join(
    ddf %>% 
      ungroup() %>% 
      group_by(sitename) %>% 
      nest(),
    ddf_pre %>% 
      ungroup() %>% 
      group_by(sitename) %>% 
      nest() %>% 
      rename(data_pre = data),
    by = "sitename") %>% 
    mutate(data = purrr::map2(data_pre, data, ~bind_rows(.x, .y))) %>% 
    dplyr::select(-data_pre) %>% 
    unnest(data) %>% 
    arrange(date) %>%   # to make sure
    distinct() # out of desperation
}

# return( ddf )

## Creating ingest_..._wfde5_byvar(): ####
# Need different inputs

ddf      <- df_out ## Function takes ddf as input which is same as df_out (see lines above)
siteinfo <- siteinfo
varnam   <- "Tair" # name of folder
dir      <- "~/data/wfde5/"

## * * Ingestr Function Definition -------------------------------------------------------------
ingest_globalfields_wfde5_byvar <- function( ddf, siteinfo, dir, varnam ){
  
  dirn <- paste0( dir, "/", varnam, "/" )
  
  ## loop over all year and months that are required
  year_start <- ddf %>%
    dplyr::pull(date) %>%
    min() %>%
    lubridate::year()
  
  year_end <- ddf %>%
    dplyr::pull(date) %>%
    max() %>%
    lubridate::year()
  
  ## check if data is required for years before 1979 (when watch wfdei is available)
  pre_data <- year_start < 1979
  
  ## if pre-1979 data are required, read at least 10 first years to get mean climatology
  if (pre_data){
    year_start_read <- 1979
    year_end_read <- max(1988, year_end)
  } else {
    year_start_read <- year_start
    year_end_read <- year_end
  }
  
  ## construct data frame holding longitude and latitude info
  df_lonlat <- tibble(
    sitename = siteinfo$sitename,
    lon      = siteinfo$lon,
    lat      = siteinfo$lat
  )
  
  if (varnam %in% c("Rainf", "Snowf")){
    addstring <- "_WFDEI_CRU+GPCC_"
  } else {
    addstring <- "_WFDE5_CRU_"
  }
  
  if (varnam %in% c("Tair", "Qair")) {
    endstring <- "_v1.0"
  } else {
    endstring <- "_v1.1"
  }
  
  ## extract all the data for all the dates (cutting to required dates by site is done in ingest())
  alldays   <- 1:31
  allmonths <- 1:12
  allyears <- year_start_read:year_end_read
  df <- 
    # expand.grid(alldays, allmonths, allyears) %>%
    expand.grid(allmonths, allyears) %>%
    dplyr::as_tibble() %>%
    # setNames(c("dom", "mo", "yr")) %>%
    setNames(c("mo", "yr")) %>%
    rowwise() %>%
    # dplyr::mutate(filename = paste0( dirn, "/", varnam, addstring, sprintf( "%4d", yr ), sprintf( "%02d", mo ), endstring, ".nc" )) %>%
    dplyr::mutate(filename = paste0( dirn, "/", varnam, addstring, sprintf( "%4d", yr ), sprintf( "%02d", mo ), endstring, ".nc" )) %>%
    ungroup() %>%
    dplyr::mutate(data = purrr::map(filename, ~extract_pointdata_allsites(., df_lonlat, get_time = FALSE ) ))
  
  ## rearrange to a daily data frame
  complement_df <- function(df){
    df <- df %>%
      setNames(., c("myvar")) %>%
      mutate( hom = 1:nrow(.))
    return(df)
  }
  
  ddf <- df %>%
    tidyr::unnest(data) %>%
    dplyr::mutate(data = purrr::map(data, ~complement_df(.))) %>%
    tidyr::unnest(data) %>%
    cbind(ddf %>% dplyr::select(-sitename)) %>% 
    dplyr::select(sitename, myvar, date) %>% 
    dplyr::as_tibble()
  
  ## create data frame containing all dates, using mean annual cycle (of 1979-1988) for all years before 1979
  if (pre_data){
    rlang::inform("Data for years before 1979 requested. Taking mean annual cycle of 10 years (1979-1988) for all years before 1979.")
    
    ## get mean seasonal cycle, averaged over 1979:1988
    ddf_meandoy <- ddf %>% 
      dplyr::filter(lubridate::year(date) %in% 1979:1988) %>% 
      mutate(doy = lubridate::yday(date)) %>% 
      group_by(sitename, doy) %>% 
      summarise(myvar = mean(myvar))
    
    ## get a data frame with all dates for all sites
    ddf_tmp <- purrr::map(
      as.list(seq(nrow(siteinfo))),
      ~ingestr::init_dates_dataframe(
        lubridate::year(siteinfo$date_start[.]),
        min(1978, lubridate::year(siteinfo$date_end[.])),
        noleap = TRUE,
        timescale = "d"))
    names(ddf_tmp) <- siteinfo$sitename
    ddf_pre <- ddf_tmp %>%
      bind_rows(.id = "sitename") %>%
      drop_na() %>% 
      mutate(doy = lubridate::yday(date)) %>%
      left_join(ddf_meandoy, by = c("sitename", "doy")) %>%
      dplyr::select(-doy)
    
    # ddf_pre <- init_dates_dataframe(year_start, min(1978, year_end)) %>% 
    #   mutate(doy = lubridate::yday(date)) %>% 
    #   left_join(ddf_pre, by = "doy") %>% 
    #   dplyr::select(-doy)
    
    ## combine the two along rows
    ddf <- left_join(
      ddf %>% 
        ungroup() %>% 
        group_by(sitename) %>% 
        nest(),
      ddf_pre %>% 
        ungroup() %>% 
        group_by(sitename) %>% 
        nest() %>% 
        rename(data_pre = data),
      by = "sitename") %>% 
      mutate(data = purrr::map2(data_pre, data, ~bind_rows(.x, .y))) %>% 
      dplyr::select(-data_pre) %>% 
      unnest(data) %>% 
      arrange(date) %>%   # to make sure
      distinct() # out of desperation
  }
  
  return( ddf )
}

## Compare ingest_wfde5 to ingest_watch for temp ####
### wfde5 ####
## Get hourly data frame
timescale <- "h"

df_out <- purrr::map(
  as.list(seq(nrow(siteinfo))),
  ~ingestr::init_dates_dataframe(
    lubridate::year(siteinfo$date_start[.]),
    lubridate::year(siteinfo$date_end[.]),
    noleap = TRUE,
    timescale = timescale))

names(df_out) <- siteinfo$sitename

df_out <- df_out %>%
  bind_rows(.id = "sitename")

df_test_w5 <- 
  ingest_globalfields_wfde5_byvar(ddf       = df_out,
                                  siteinfo  = siteinfo,
                                  dir       = "~/data/wfde5/",
                                  varnam    = "Tair"
                                  ) %>%
  dplyr::rename(temp = myvar) %>%
  dplyr::mutate(temp = temp - 273.15) %>%
  dplyr::right_join(df_out, by = c("sitename", "date"))

### watch ####
## Get hourly data frame
timescale <- "d"

df_out <- purrr::map(
  as.list(seq(nrow(siteinfo))),
  ~ingestr::init_dates_dataframe(
    lubridate::year(siteinfo$date_start[.]),
    lubridate::year(siteinfo$date_end[.]),
    noleap = TRUE,
    timescale = timescale))

names(df_out) <- siteinfo$sitename

df_out <- df_out %>%
  bind_rows(.id = "sitename")

df_test_wa <-
  ingest_globalfields_watch_byvar( df_out,
                                   siteinfo,
                                   dir = "~/data/watch_wfdei/",
                                   "Tair_daily" ) %>%
  dplyr::rename(temp = myvar) %>%
  dplyr::mutate(temp = temp - 273.15) %>%
  dplyr::right_join(df_out, by = c("sitename", "date"))


### comparison plots
## hourly Resolution
p_h_base <- 
  ggplot() +
  geom_line(data = df_test_w5,
            aes(date, temp),
            size = 0.25,
            color = "black") +
  geom_line(data = df_test_wa %>% 
              mutate(date = lubridate::ymd_hms(paste0(date, "12:00:00"))),
            aes(date, temp),
            size = 0.25,
            color = "red")

p_h_inset <- 
  ggplot() +
  geom_line(data = df_test_w5,
            aes(date, temp),
            size = 0.25,
            color = "black") +
  geom_line(data = df_test_wa %>% 
              mutate(date = lubridate::ymd_hms(paste0(date, "12:00:00"))),
            aes(date, temp),
            size = 0.25,
            color = "red") +
  xlim(lubridate::ymd_hms("2007-06-01 00:00:00"),
       lubridate::ymd_hms("2007-07-01 00:00:00")) +
  ylim(10, 35) +
  xlab(NULL) +
  ylab(NULL)

p_h_base + inset_element(p_h_inset, left = 0.25, bottom = 0.05, right = 0.75, top = 0.25)



## Daily Resolution
p_d_base <-
  ggplot() +
  geom_line(data = df_test_w5 %>% 
              mutate(date = lubridate::floor_date(date, "day") %>% lubridate::ymd()) %>% 
              group_by(date) %>%
              summarize(temp = mean(temp)), 
            aes(date, temp),
            size = 0.25,
            color = "black") +
  geom_line(data = df_test_wa ,
            aes(date, temp),
            size = 0.25,
            color = "red")

p_d_inset <-
  ggplot() +
  geom_line(data = df_test_w5 %>% 
              mutate(date = lubridate::floor_date(date, "day") %>% lubridate::ymd()) %>% 
              group_by(date) %>%
              summarize(temp = mean(temp)), 
            aes(date, temp),
            size = 0.25,
            color = "black") +
  geom_line(data = df_test_wa ,
            aes(date, temp),
            size = 0.25,
            color = "red") +
  xlim(lubridate::ymd("2007-06-01"),
       lubridate::ymd("2007-07-01")) +
  ylim(10, 35) +
  xlab(NULL) +
  ylab(NULL)

p_d_base + inset_element(p_d_inset, left = 0.25, bottom = 0.05, right = 0.75, top = 0.25)


# * After integration into ingest_bysite() ----------------------------------------------------
# * * Get updated functions -------------------------------------------------------------------
source("~/projects/ingestr/R/ingest_bysite.R")
source("~/projects/ingestr/R/ingest_globalfields.R")


# * Run functions like in example vign. -------------------------------------------------------
vars_out <- c("temp", "ppfd", "vpd", "patm")
vars_out <- c("temp", "ppfd")
vars_out <- c("temp")

df_watch <- ingest_bysite(
  sitename  = "FR-Pue",
  source    = "watch_wfdei",
  getvars   = vars_out,
  dir       = "~/data/watch_wfdei/",
  timescale = "d",
  year_start = 1980,
  year_end  = 1980,
  lon       = 3.5958,
  lat       = 43.7414,
  verbose   = TRUE#,
  #settings  = list(correct_bias = "worldclim", dir_bias = "~/data/worldclim")
)
df_watch

df_wfde5 <- ingest_bysite(
  sitename  = "FR-Pue",
  source    = "wfde5",
  getvars   = vars_out,
  dir       = "~/data/wfde5/",
  timescale = "h",
  year_start = 1980,
  year_end  = 1980,
  lon       = 3.5958,
  lat       = 43.7414,
  elv       = 400,
  verbose   = TRUE#,
  #settings  = list(correct_bias = "worldclim", dir_bias = "~/data/worldclim")
)

df_wfde5


# * Test ingest() -----------------------------------------------------------------------------

out_bc <- ingest(siteinfo = siteinfo_fluxnet2015 %>%
         filter(sitename == "FR-Pue") %>% 
         mutate(year_start = 2007,
                year_end = 2007),
       source    = "wfde5",
       getvars   = c("ppfd"),
       dir       = "~/data/wfde5/",
       settings  = list(correct_bias = "worldclim", dir_bias = "~/data/worldclim"),
       timescale = "h")

## .................................................................................................
## 
# For fapar
## ingest_bysite()
ingest_bysite(
  sitename  = "FR-Pue",
  source    = "co2_mlo",
  timescale = "h",
  year_start = 1980,
  year_end  = 1980,
  lon       = 3.5958,
  lat       = 43.7414,
  elv       = 400,
  verbose   = TRUE#,
  #settings  = list(correct_bias = "worldclim", dir_bias = "~/data/worldclim")
)

## ingest()
fapar <- ingest(siteinfo = siteinfo_fluxnet2015 %>%
                filter(sitename == "FR-Pue") %>% 
                mutate(year_start = 2007,
                       year_end = 2007),
              source    = "fapar_unity",
              timescale = "h") %>% 
    unnest(data)

# For CO2
df_co2 <- ingest(
  siteinfo = siteinfo_fluxnet2015 %>%
    filter(sitename == "FR-Pue") %>% 
    mutate(year_start = 2007,
           year_end = 2007),
  source  = "co2_mlo",
  verbose = FALSE,
  timescale = "h") %>% 
  unnest(data) %>% 
  select(sitename, date, co2)

# Join test
out %>% 
  left_join(fapar) %>% 
  left_join(df_co2) %>% 
  mutate(ccov = 0,
         wind = 0) %>% 
  nest(forcing = any_of(c("date", "temp", "prec", "vpd", "ppfd", "patm", "ccov", "fapar", "co2", "wind"))) %>% 
  nest(siteinfo = !all_of(c("sitename", "forcing")))

# * * Comparison of extracted temp and ppfd ---------------------------------------------------
ggplot() +
  geom_line(data = df_wfde5,
            aes(date, temp),
            color = "black") +
  geom_line(data = df_watch %>% 
              mutate(date = lubridate::ymd_hms(paste0(date, "12:00:00"))),
            aes(date, temp),
            color = "red") +
  xlim(lubridate::ymd_hms("2007-06-01 00:00:00"),
       lubridate::ymd_hms("2007-07-15 00:00:00"))

ggplot() +
  geom_line(data = df_wfde5,
            aes(date, ppfd),
            color = "black") +
  geom_line(data = df_watch %>% 
              mutate(date = lubridate::ymd_hms(paste0(date, "12:00:00"))),
            aes(date, ppfd),
            color = "red") +
  xlim(lubridate::ymd_hms("2007-06-01 00:00:00"),
       lubridate::ymd_hms("2007-07-15 00:00:00"))


# * Reading in a NetCDF File ------------------------------------------------------------------
# set path and filename
library(ncdf4)
ncpath <- "~/data/wfde5/Tair/"
ncname <- "Tair_WFDE5_CRU_198002_v1.0"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "tmp"  # note: tmp means temperature (not temporary)
ncin <- nc_open(ncfname)
print(ncin)

