#' Remove outliers
#'
#' Removes outliers based on their distance from the inter-quartile range (IQR). Excludes all points beyond \code{coef} times the IQR. 
#' The function uses the command \code{boxplot.stats()} which uses the Tukey's method to identify the outliers ranged above and below the \code{coef*}IQR.
#' 
#' @param vec A vector of numeric values
#' @param coef A number specifying the maximum distance from the inter-quartile range of \code{vec} for which values in \code{vec} are not replaced with NA.
#' 
#' @return A vector of numeric values of length \code{length(vec)} whith all elements identical as in \code{vec} except that outliers are replaced by NA.
#' @export
#'
#' @examples vec <- remove_outliers( vec, coef=3 ) 
#' 
remove_outliers <- function( vec, coef=1.5 ) {
  ## use the command boxplot.stats()$out which use the Tukey's method to identify the outliers ranged above and below the <coef`>*IQR.
  outlier <- boxplot.stats( vec, coef=coef )$out
  vec[ which( is.element( vec, outlier ) ) ] <- NA
  return( vec )
}
