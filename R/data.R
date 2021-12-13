#' Meta information of all FLUXNET 2015 sites
#'
#' User-friendly meta information for all FLUXNET 2015 sites as a data frame
#' with rows for sites and columns for variables.
#'
#' @format A data frame (tibble)
#' \describe{
#'   \item{mysitename}{Site name}
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
