# helping draft a script to convert the plumber data
# to a fluxnet compatible format in order to process
# data using the current ingestr routines rather
# than writing another set of code for plumber data

library(tidyverse)
source("R/read_plumber.R")

# read in demo data
read_plumber(
  site = "AR-SLu",
  path = "~/Desktop/flux_data/",
  fluxnet_format = TRUE,
  meta_data = FALSE,
  out_path = "~/Desktop/flux_data/"
)

settings_fluxnet <- list(
  getswc       = FALSE,
  filter_ntdt  = TRUE,
  threshold_GPP= 0.8,
  remove_neg   = FALSE,
  dir_hh = "~/Desktop/flux_data/"
)

siteinfo <- ingestr::siteinfo_fluxnet2015 %>%
  filter(sitename == "AR-SLu")

siteinfo$year_start = 2010
siteinfo$year_end = 2010

df <- ingest(
  siteinfo,
  source    = "fluxnet",
  getvars   = list("gpp" = "GPP_VUT_REF"),
  dir       = "~/Desktop/flux_data/", 
  settings  = settings_fluxnet,
  timescale = "hh",
  verbose = TRUE
)

print(df$data)

