# helping draft a script to convert the plumber data
# to a fluxnet compatible format in order to process
# data using the current ingestr routines rather
# than writing another set of code for plumber data

library(tidyverse)
source("R/read_plumber.R")

# read in demo data
df <- read_plumber(
  site = "AR-SLu",
  path = "~/Desktop/flux_data/",
  fluxnet_format = TRUE,
  meta_data = FALSE,
  out_path = "~/Desktop/flux_data/"
)

print(str(df))


