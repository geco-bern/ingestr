library(tidyverse)
library(ingestr)

settings_fluxnet <- list(getswc = FALSE)

df_fluxnet <- ingest_bysite(
  sitename = "FR-Pue",
  source = "fluxnet",
  getvars = list(temp = "TA_F",
                 prec = "P_F",
                 vpd  = "VPD_F",
                 ppfd =  "SW_IN_F",
                 netrad = "NETRAD",
                 patm = "PA_F"),
  dir = file.path(path.package("ingestr"), "extdata/"),  # example file delivered through package and located here
  settings = settings_fluxnet,
  timescale = "d",
  year_start = 2007,
  year_end = 2007,
  verbose = FALSE
)

settings_fluxnet <- list(
  getswc       = FALSE,
  filter_ntdt  = TRUE,
  threshold_GPP= 0.8,
  remove_neg   = FALSE,
  dir_hh = file.path(path.package("ingestr"), "extdata/")
)

siteinfo <- ingestr::siteinfo_fluxnet2015 %>%
  filter(sitename == "FR-Pue")

ddf_fluxnet <- ingest(
  siteinfo,
  source    = "fluxnet",
  getvars   = list(
    gpp = "GPP_NT_VUT_REF"
  ), 
  dir       = file.path(path.package("ingestr"), "extdata/"), 
  settings  = settings_fluxnet,
  timescale = "hh",
  verbose = TRUE
)