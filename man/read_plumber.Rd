% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_plumber.R
\name{read_plumber}
\alias{read_plumber}
\title{Reads plumber2 netcdf data}
\usage{
read_plumber(site = "AT-Neu", path = "~/Desktop")
}
\arguments{
\item{site}{fluxnet site name}

\item{path}{path with plumber2 data (both flux and meteo data files)}
}
\value{
data frame with merged meteo and flux data
}
\description{
Reads data in a given directory
by (fluxnet) site name
}