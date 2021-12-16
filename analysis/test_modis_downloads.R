settings_modis <- get_settings_modis(
  bundle            = "modis_lst_daily",
  data_path         = tempdir(),
  method_interpol   = "sgfilter",
  keep              = FALSE,
  overwrite_raw     = TRUE,
  overwrite_interpol= TRUE,
  n_focal           = 0,
  network           = "FLUXNET"
)

df <- ingest_bysite(
  sitename  = "CH-Lae",
  source    = "modis",
  year_start= 2018,
  year_end  = 2018,
  settings  = settings_modis,
  verbose   = FALSE
)

plot(df$date, df$lst)
