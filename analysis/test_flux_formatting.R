library(tidyverse)
library(ingestr)

#---- test PLUMBER based FLUXNET 2015 data ----

settings_fluxnet <- list(
  getswc       = FALSE,
  filter_ntdt  = FALSE,
  threshold_GPP= 0.8,
  remove_neg   = FALSE
)

siteinfo <- ingestr::siteinfo_fluxnet2015 %>%
  filter(sitename == "AR-SLu") %>%
  ungroup()

df <- ingest(
  siteinfo,
  source    = "fluxnet",
  getvars   = list(
    gpp = "GPP_VUT_REF"
    ),
  dir       = "~/Desktop/flux_data/", 
  settings  = settings_fluxnet,
  timescale = "hh",
  verbose = TRUE
)

print(df$data)

#---- test true FLUXNET 2015 data ----

siteinfo <- ingestr::siteinfo_fluxnet2015 %>%
  filter(sitename == "FR-Pue")

settings_fluxnet <- list(
  getswc       = FALSE,
  filter_ntdt  = TRUE,
  threshold_GPP= 0.8,
  remove_neg   = FALSE,
  dir_hh = paste0(path.package("ingestr"), "/extdata/")
)

df <- ingestr::ingest(
  siteinfo,
  source    = "fluxnet",
  getvars   = list(gpp = "GPP_NT_VUT_REF", gpp_unc = "GPP_NT_VUT_SE"),
  dir       = paste0(path.package("ingestr"), "/extdata/"),
  settings  = settings_fluxnet,
  timescale = "hh",
  verbose = TRUE
)