#' Meta information of all FLUXNET 2015 sites
#'
#' User-friendly meta information for all FLUXNET 2015 sites as a data frame
#' with rows for sites and columns for variables.
#'
#' @format A data frame (tibble)
#' \describe{
#'   \item{sitename}{Site name}
#'   \item{lon}{Longitude of site}
#'   \item{lat}{Latitude of site}
#'   \item{c4}{C4 vegetation or not}
#'   \item{classid}{classification type}
#'   \item{elv}{elevation}
#'   \item{igbp_land_use}{IGBP land use}
#'   \item{whc}{water holding capacity}
#'   \item{year_end}{end year}
#'   \item{koeppen_code}{Koeppen Geiger code}
#'   \item{plant_functional_type}{Plant functional type}
#'   \item{year_start}{start year}
#' }
#' 
#' @source Based on the file FLX_AA-Flx_BIF_LATEST.csv, distributed via FLUXNET
#' 2015. Additional information for Koeppen Geiger classification based on
#' Falge et al. (\url{https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1530})
#' and complemented by information given in koeppen-geiger.tif.

"siteinfo_fluxnet2015"


#' Demo data
#'
#' Flux demo data
#'
#' @format A data frame (tibble)
#' \describe{
#'   \item{sitename}{Site name}
#'   \item{params_siml}{simulation parametrs}
#'   \item{siteinfo}{site info}
#'   \item{df_soiltexture}{soil texture info}
#'   \item{forcing}{climate forcing}
#' }

"df_drivers"


#' Demo data
#'
#' An example data set to illustrate
#' All the columns in the `siteinfo_globresp` data.frame are necessary to retrieve
#' the P-model drivers data: a site name for reference `sitename`, the coordinates
#' of the site location in degrees `lon` and `lat`, its elevation in meters `elv`,
#' and the first and last year of observations we want to collect `year_start` and 
#' `year_end`.
#' The data describes the location of some sites from Atkin et al. 2017
#' (the original data is available via the 
#' [TRY Plant Trait Database](https://www.try-db.org/de/Datasets.php)).

#'
#' @format A data frame (tibble)
#' \describe{
#'   \item{sitename}{Site name}
#'   \item{lon}{simulation parametrs}
#'   \item{lat}{site info}
#'   \item{elv}{soil texture info}
#'   \item{year_start}{climate forcing}
#'   \item{year_end}{climate forcing}
#' }

"siteinfo_globresp"