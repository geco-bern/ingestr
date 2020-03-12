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

### Usage and contribution

This package is designed to be extendible to ingesting other data types (sources). The developer (Beni Stocker) would appreciate if you made sure that your developments can be fed back to this repository. To do so, please use git. See [here](http://rogerdudler.github.io/git-guide/) for a brief introduction to git. 

I recommend the following steps if you would like to use this package (no development):

- In RStudio, do
```r
if(!require(devtools)){install.packages(devtools)}
devtools::install_github( "stineb/ingestr" )
library(ingestr)
```

I recommend the following steps if you would like to use and further develop the package:

1. Make sure you have a Github account.
2. Log on to Github, and go to [https://github.com/stineb/ingestr](https://github.com/stineb/ingestr) and click on 'Fork' in the upper right corner. This makes a copy of the repository that belongs to you.
3. Clone your fork to your local computer by entering in your terminal (here, it's cloned to your home directory):
```sh
cd home
git clone https://github.com/<your_github_username>/ingestr.git
```
4. In RStudio, create a new project in `~/ingestr/`. This opens the repository where you have access to the code where all ingestr-functions are implemented (see subdirectory `R`).
5. In RStudio, after having edited code, select the 'Build' tab and click on 'Install and Restart' to build the package again. For quick checks, you may simply source the edited files instead of re-building the whole package. If you like to add new functions, create new a source file in subdirectory `R`, write a nice roxygen header (see other source files as an example), then click on 'Build' -> 'More' -> 'Document', and then again on 'Install and Restart'.
6. If you're happy with your new edits and additions to the package, you may want to share it with the original repository. To do so, please create a new Pull Request in GitHub: Click on 'New pull request' on [the repository page](https://github.com/stineb/ingestr). Thanks!


