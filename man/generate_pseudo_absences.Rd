% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_pseudo_absences.R
\name{generate_pseudo_absences}
\alias{generate_pseudo_absences}
\title{Generate Pseudo-Absence Points Based on Presence Points, Covariates, and Study Area Polygon}
\usage{
generate_pseudo_absences(
  presences,
  study_area,
  raster_stack,
  predictor_variables,
  coords = c("decimalLongitude", "decimalLatitude"),
  decimal_digits = NULL,
  attempts = 100
)
}
\arguments{
\item{presences}{Data frame containing presence points.}

\item{study_area}{Spatial polygon defining the study area (`sf` object).}

\item{raster_stack}{`SpatRaster` object containing covariate data.}

\item{predictor_variables}{Character vector of the predictor variables selected for this species.}

\item{coords}{Character vector specifying the column names for latitude and longitude. Defaults to `c("decimalLongitude", "decimalLatitude")`.}

\item{decimal_digits}{An integer specifying the number of decimal places to which coordinates should be rounded.}

\item{attempts}{Integer specifying the number of attempts to generate exact pseudo-absences. Defaults to 100.}
}
\value{
Data frame containing both presence and pseudo-absence points.
}
\description{
This function generates pseudo-absence points within the study area.
}
