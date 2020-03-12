# ingestr

The package `ingestr` provides functions to extract (ingest) point data (given longitude, latitude, and required dates) from large global files or remote data servers and create time series at user-specified temporal resolution. This can be done for a set of sites at once, given a data frame containing the meta info for each site (see data frame `siteinfo`, with columns `lon` for longitude, `lat` for latitude, `date_start` and `date_end` specifying required dates). The output for such a set of site-level data is a nested data frame with rows for each site and columns `lon`, `lat`, `date_start`, and `date_end` plus an added column where the time series of ingested data is nested inside.

Data can be ingested for different data types (argument `source` in several functions), each dealing with a specific format of the original data and specific functions to read from respective files or remote servers. The following data types can be handled currently (more to be added by you if you like):

Meteo data:

  - FLUXNET
  - WATCH-WFDEI
  - CRU

Data on Google Earth Engine (using Koen Hufken's `gee_suset` library):

  - MODIS FPAR
  - MODIS EVI
  - MODIS GPP

MODIS data:

  - RModisTools R package to access data on remote server ORNL DAAC (not yet implemented).
  
Examples to read data for a single site for each data type and handling ingestion for an ensemble of sites are descrbed in vignette `example`.

## Installation

To install and load the rsofun package using the latest release run the following command in your R terminal: 
```r
if(!require(devtools)){install.packages(devtools)}
devtools::install_github( "stineb/ingestr" )
library(ingestr)
```

### Dependencies

The `ingestr` package relies heavily on the tidyverse. Dependencies are 

- dplyr
- purrr
- lubridate
- tidyr
- raster
- lubridate

## Example

Are described in vignette `example`, available [here](https://rpubs.com/stineb/ingestr). 

