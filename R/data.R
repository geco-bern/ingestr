#' Meta information of all FLUXNET 2015 sites
#'
#' User-friendly meta information for all FLUXNET 2015 sites as a data frame with rows for sites and columns for variables.
#'
#' @format A data frame
#' \describe{
#'   \item{mysitename}{Site name}
#'   \item{lon}{Longitude of site}
#' }
#' @source Based on the file FLX_AA-Flx_BIF_LATEST.csv, distributed via FLUXNET 2015.
"metainfo_sites_fluxnet2015"

#' Reduced meta information of FLUXNET 2015 Tier 1 sites with additional information on the climate zone (Koeppen-Geiger classification)
#'
#' User-friendly meta information for FLUXNET 2015 Tier 1 sites as a data frame with rows for sites and columns for variables.
#'
#' @format A data frame
#' \describe{
#'   \item{mysitename}{Site name}
#'   \item{lon}{Longitude of site}
#' }
#' @source Koeppen Geiger classification based on \url{https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1530} and complemented by information given in koeppen-geiger.tif.
"metainfo_Tier1_sites_kgclimate_fluxnet2015"

#' Koeppen Geiger class legend
#'
#' A legend identifying the Koeppen Geiger class based on its code
#'
#' @format A data frame
#' \describe{
#'   \item{Code}{}
#'   \item{Climate}{}
#' }
#' @source Koeppen Geiger classification based on \url{https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1530} and complemented by information given in koeppen-geiger.tif.
"koeppen_legend"
