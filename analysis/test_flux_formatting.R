library(tidyverse)
library(ingestr)

settings_fluxnet <- list(
  getswc       = FALSE,
  filter_ntdt  = TRUE,
  threshold_GPP= 0.8,
  remove_neg   = FALSE,
  dir_hh = "~/Desktop/flux_data/"
)

siteinfo <- ingestr::siteinfo_fluxnet2015 %>%
  filter(sitename == "AU-Rob")

df <- ingest(
  siteinfo,
  source    = "fluxnet",
  getvars   = list(
    gpp = "GPP_NT_VUT_REF"),
  dir       = "~/Desktop/flux_data/", 
  settings  = settings_fluxnet,
  timescale = "hh",
  verbose = TRUE
)

print(df$data)
