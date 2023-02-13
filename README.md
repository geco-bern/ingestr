[![R-CMD-check](https://github.com/geco-bern/ingestr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/geco-bern/ingestr/actions/workflows/R-CMD-check.yaml)

# ingestr

The package `ingestr` provides functions to extract (ingest) environmental point data (given longitude, latitude, and required dates) from large global files or remote data servers and create time series at user-specified temporal resolution (currently, just daily implemented). The main functionalities are:

-   Temporal downscaling from montly to daily resolution
-   Quality filtering, temporal interpolation and smoothing of remote sensing data
-   Handling of different APIs and file formats, returning ingested data in *tidy* format.

This is to make your life simpler when downloading and reading site-scale data, using a common interface with a single function for single-site and multi-site ingest, respectively, and a common and tidy format of ingested data across a variety of data *sources* and formats of original files. *Sources*, refers to both data sets hosted remotely and accessed through an API and local data sets. ingestr is particularly suited for preparing model forcing and offers a set of functionalities to transform original data into common standardized formats and units. This includes interpolation methods for converting monthly climate data (CRU TS currently) to daily time steps.

The key functions are `ingest_bysite()` and `ingest()` for a single-site data ingest and a multi-site data ingest, respectively. For the multi-site data ingest, site meta information is provided through the argument `siteinfo` which takes a data frame with columns `lon` for longitude, `lat` for latitude, and (for time series downloads) `year_start` and `year_end`, specifying required dates (including all days of respective years). Sites are organised along rows. An example site meta info data frame is provided as part of this package for sites included in the FLUXNET2015 Tier 1 data set (`siteinfo_fluxnet2015`, additional columns are not required by `ingest_bysite()` and `ingest()`).

The following *sources* can be handled currently:

| Data source                                                                                               | Data type              | Coverage | Source ID     | Reading from  | Remark                                                                                     |
|-----------------------------------------------------------------------------------------------------------|------------------------|----------|---------------|---------------|--------------------------------------------------------------------------------------------|
| [FLUXNET](https://fluxnet.fluxdata.org/data/fluxnet2015-dataset/)                                         | time series by site    | site     | `fluxnet`     | local files   | Extraction by site name                                                                    |
| [WATCH-WFDEI](http://www.eu-watch.org/data_availability)                                                  | time series raster map | global   | `watch_wfdei` | local files   |  |
| [WFDE5](https://cds.climate.copernicus.eu/cdsapp#!/dataset/10.24381/cds.20d54e34?tab=overview) | time series raster map | global | `wfde5` | local files | [Cucchi et al. (2020)](https://essd.copernicus.org/articles/12/2097/2020/) |
| [CRU](https://crudata.uea.ac.uk/cru/data/hrg/)                                                            | time series raster map | global   | `cru`         | local files   |                                                                                            |
| MODIS LP DAAC                                                                                             | time series raster map | global   | `modis`       | remote server | using [MODISTools](https://docs.ropensci.org/MODISTools/)                                  |
| Google Earth Engine                                                                                       | time series raster map | global   | `gee`         | remote server | using Koen Hufken's [gee_suset](https://khufkens.github.io/gee_subset/) library            |
| [ETOPO1](https://www.ngdc.noaa.gov/mgg/global/)                                                           | raster map             | global   | `etopo1`      | local files   |                                                                                            |
| [Mauna Loa CO2](https://www.esrl.noaa.gov/gmd/ccgg/trends/data.html)                                      | time series            | site     | `co2_mlo`     | remote server | using the [climate](https://github.com/bczernecki/climate) R package                       |
| [HWSD](https://webarchive.iiasa.ac.at/Research/LUC/External-World-soil-database/HTML/)                                                                                                      | raster map, database | global   | `hwsd`        | local files   | using an adaption of David Le Bauer's [rhwsd](https://github.com/dlebauer/rhwsd) R package |
| [WWF Ecoregions](https://databasin.org/datasets/68635d7c77f1475f9b6c1d1dbe0a4c4c)                         | shapefile map          | global   | `wwf`         | local files   | Olsen et al. (2001)                                                                        |
| [N deposition](https://link.springer.com/article/10.1007%2Fs10584-011-0155-0)                             | time series raster map | global   | `ndep`        | local files   | Lamarque et al. (2011)                                                                     |
| [SoilGrids](https://www.isric.org/explore/soilgrids)                                                      | raster map             | global   | `soilgrids`   | remote server | Hengl et al. (2017)                                                                        |
| [ISRIC WISE30sec](https://data.isric.org/geonetwork/srv/api/records/dc7b283a-8f19-45e1-aaed-e9bd515119bc) | raster map             | global   | `wise`        | local files   | [Batjes (2016)](http://dx.doi.org/10.1016/j.geoderma.2016.01.034)                          |
| [GSDE Soil](http://globalchange.bnu.edu.cn/research/soilwd.jsp)                                           | raster map             | global   | `gsde`        | local files   | [Shangguan et al. 2014](https://doi.org/10.1002/2013MS000293)                              |
| [WorldClim](https://www.worldclim.org/data/worldclim21.html)                                              | raster map             | global   | `gsde`        | local files   | [Fick & Hijmans, 2017](https://doi.org/10.1002/joc.5086)                                   |

Examples to read data for a single site for each data type are given in Section 'Examples for a single site'. Handling ingestion for multiple sites is described in Section 'Example for a set of sites'. Unless remarked otherwise, extraction goes by longitude/latitude values. **Note** that this package does not provide the original data. Please follow links to data sources above where data is read from local files, and always cite original references.

## Variable names and units

All ingested data follows standardized variable naming and SI units. For example:

| Variable                           | Variable name | Units                           |
|------------------------------------|---------------|---------------------------------|
| Gross primary production           | `gpp`         | g CO$^{-2}$ m$^{-2}$            |
| Air temperature                    | `temp`        | $^\\circ$C                      |
| Daily minimum air temperature      | `tmin`        | $^\\circ$C                      |
| Daily maximum air temperature      | `tmax`        | $^\\circ$C                      |
| Precipitation                      | `prec`        | mm s$^{-1}$                     |
| Vapour pressure deficit            | `vpd`         | Pa                              |
| Atmospheric pressure               | `patm`        | Pa                              |
| Net radiation                      | `netrad`      | J m$^{-2}$ s$^{-1}=$ W m$^{-2}$ |
| Photosynthetic photon flux density | `ppfd`        | mol m$^{-2}$ s$^{-1}$           |
| Elevation (altitude)               | `elv`         | m a.s.l.                        |

Use these variable names for specifying which variable names they correspond to in the original data source (see argument `getvars` to functions `ingest()` and `ingest_bysite()`). `gpp` is cumulative, corresponding to the time scale of the data. For example, if daily data is read, `gpp` is the total gross primary production per day (g CO$^{-2}$ m$^{-2}$ d$^{-1}$).

## Installation

To install and load the rsofun package using the latest release run the following command in your R terminal:

``` r
if(!require(devtools)){install.packages(devtools)}
devtools::install_github("geco-bern/ingestr")
library(ingestr)
```

### Dependencies

The `ingestr` package relies heavily on the tidyverse. Dependencies are dplyr, purrr, lubridate, tidyr, raster, lubridate, stringi, stringr, sp, ncdf4, signal, climate. To install all required packages, do:

``` r
list_pkgs <- c("dplyr", "purrr", "lubridate", "tidyr", "raster", "lubridate", "stringi", "stringr", "sp", "ncdf4", "signal", "climate", "rgdal", "hwsdr", "gdalUtils", "MODISTools")
new_pkgs <- list_pkgs[!(list_pkgs %in% installed.packages()[,"Package"])]
if(length(new_pkgs)) install.packages(new_pkgs)
```

## Example

Are described in vignette `example`, available [here](https://rpubs.com/stineb/ingestr).

### Usage and contribution

This package is designed to be extendible to ingesting other data types (sources). The developer (Beni Stocker) would appreciate if you made sure that your developments can be fed back to this repository. To do so, please use git. See [here](http://rogerdudler.github.io/git-guide/) for a brief introduction to git.

I recommend the following steps if you would just like to use this package (no development):

-   Directly install the package from the most up-to-date code on GitHub by

``` r
devtools::install_github("stineb/ingestr")
```

I recommend the following steps if you would like to use and further develop the package (even just for your own application - But keep in mind: others may benefit from your efforts too!):

1.  Make sure you have a Github account.
2.  Log on to Github, and go to <https://github.com/stineb/ingestr> and click on 'Fork' in the upper right corner. This makes a copy of the repository that belongs to you, meaning that you can modify, commit, and push changes back to your forked repository as you please.
3.  Clone your fork to your local computer by entering in your terminal (here, it's cloned to a subdirectory `ingestr` placed in your home directory):

``` sh
cd home
git clone https://github.com/<your_github_username>/ingestr.git
```

1.  In RStudio, create a new project in your local directory `~/ingestr/`. This opens the repository in RStudio and you have access to the code where all ingestr-functions are implemented (see subdirectory `./R/`).
2.  In RStudio, after having edited code, select the 'Build' tab and click on 'Install and Restart' to build the package again. For quick edits and checks, you may simply source the edited files instead of re-building the whole package. If you like to add new functions, create new a source file in subdirectory `./R/`, write a nice roxygen header (see other source files as an example), then click on 'Build' -\> 'More' -\> 'Document', and then again on 'Install and Restart'.
3.  If you're happy with your new edits and additions to the package, you may want to have it fet back to the original repository. To do so, please create a new *pull request* in GitHub: Click on 'New pull request' on [the repository page](https://github.com/stineb/ingestr) and follow the inuitive steps. Thanks!

This package is still in its maturing phase. To stay up-to-date with the latest version, regularly re-install from GitHub (`devtools::install_github("stineb/ingestr")`), or - if you're building from a locally (*git*) cloned repository - regularly do a `git pull` and re-install the package.
