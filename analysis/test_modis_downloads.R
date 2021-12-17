library(tidyverse)
library(ingestr)

settings_modis <- get_settings_modis(
  bundle            = "modis_lst_daily",
  data_path         = tempdir(),
  method_interpol   = "loess",
  keep              = TRUE,
  overwrite_raw     = TRUE,
  overwrite_interpol= TRUE,
  n_focal           = 0,
  network           = "FLUXNET"
)

df <- ingest_bysite(
  sitename  = "CH-Lae",
  source    = "modis",
  year_start= 2018,
  year_end  = 2019,
  settings  = settings_modis,
  verbose   = FALSE
)

print(head(df))

plot(df$date, df$spline, type = "l", col = "black", ylab = "LST")
lines(df$date, df$loess, col = "red")
points(df$date, df$sgfilter, col = "blue")
lines(df$date, df$linear, col = "green")