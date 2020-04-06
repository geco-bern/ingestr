# ingestr

The package `ingestr` provides functions to extract (ingest) point data (given longitude, latitude, and required dates) from large global files or remote data servers and create time series at user-specified temporal resolution. This can be done for a set of sites at once, given a data frame containing the meta info for each site (see data frame `siteinfo`, with columns `lon` for longitude, `lat` for latitude, `date_start` and `date_end` specifying required dates). The output for such a set of site-level data is a nested data frame with rows for each site and columns `lon`, `lat`, `date_start`, and `date_end` plus an added column where the time series of ingested data is nested inside.

Data can be ingested for different data types (argument `source` in several functions), each dealing with a specific format of the original data and specific functions to read from respective files or remote servers. The following data types can be handled currently (more to be added by you if you like):

Meteo data:

  - [FLUXNET](https://fluxnet.fluxdata.org/data/fluxnet2015-dataset/) (also flux data can be read in)
  - [WATCH-WFDEI](http://www.eu-watch.org/data_availability)
  - [CRU](https://crudata.uea.ac.uk/cru/data/hrg/)

Data on Google Earth Engine (using Koen Hufken's [gee_suset](https://khufkens.github.io/gee_subset/) library):

  - MODIS FPAR
  - MODIS EVI
  - MODIS GPP

MODIS data (not yet implemented):

  - [RModisTools](https://docs.ropensci.org/MODISTools/) R package to access data on remote server ORNL DAAC (not yet implemented).
  
Examples to read data for a single site for each data type are given in Section 'Examples for a single site'. Handling ingestion for multiple sites is descrbed in Section 'Example for a set of sites'.

**Note** that this package does not provide the original data. Please follow links to data sources above and cite original references when using this data.

## Installation

To install and load the rsofun package using the latest release run the following command in your R terminal: 
```r
if(!require(devtools)){install.packages(devtools)}
devtools::install_github("stineb/ingestr")
library(ingestr)
```

### Dependencies

The `ingestr` package relies heavily on the tidyverse. Dependencies are 

- dplyr
- purrr
- lubridate
- tidyr
- raster
- ncdf4
- lubridate

## Example

Are described in vignette `example`, available [here](https://rpubs.com/stineb/ingestr). 

### Usage and contribution

This package is designed to be extendible to ingesting other data types (sources). The developer (Beni Stocker) would appreciate if you made sure that your developments can be fed back to this repository. To do so, please use git. See [here](http://rogerdudler.github.io/git-guide/) for a brief introduction to git. 

I recommend the following steps if you would just like to use this package (no development):

- Directly install the package from the most up-to-date code on GitHub by
```r
devtools::install_github("stineb/ingestr")
```

I recommend the following steps if you would like to use and further develop the package (even just for your own application - But keep in mind: others may benefit from your efforts too!):

1. Make sure you have a Github account.
2. Log on to Github, and go to [https://github.com/stineb/ingestr](https://github.com/stineb/ingestr) and click on 'Fork' in the upper right corner. This makes a copy of the repository that belongs to you, meaning that you can modify, commit, and push changes back to your forked repository as you please.
3. Clone your fork to your local computer by entering in your terminal (here, it's cloned to a subdirectory `ingestr` placed in your home directory):
```sh
cd home
git clone https://github.com/<your_github_username>/ingestr.git
```
4. In RStudio, create a new project in your local directory `~/ingestr/`. This opens the repository in RStudio and you have access to the code where all ingestr-functions are implemented (see subdirectory `./R/`).
5. In RStudio, after having edited code, select the 'Build' tab and click on 'Install and Restart' to build the package again. For quick edits and checks, you may simply source the edited files instead of re-building the whole package. If you like to add new functions, create new a source file in subdirectory `./R/`, write a nice roxygen header (see other source files as an example), then click on 'Build' -> 'More' -> 'Document', and then again on 'Install and Restart'.
6. If you're happy with your new edits and additions to the package, you may want to have it fet back to the original repository. To do so, please create a new *pull request* in GitHub: Click on 'New pull request' on [the repository page](https://github.com/stineb/ingestr) and follow the inuitive steps. Thanks!

This package is still in its maturing phase. To stay up-to-date with the latest version, regularly re-install from GitHub (`devtools::install_github("stineb/ingestr")`), or - if you're building from a locally (*git*) cloned repository - regularly do a `git pull` and re-install the package.
