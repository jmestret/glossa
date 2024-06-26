% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bart_functions.R
\name{fit_bart_model}
\alias{fit_bart_model}
\title{Fit a BART Model Using Environmental Covariate Layers}
\usage{
fit_bart_model(pa_coords, layers, seed = NULL)
}
\arguments{
\item{pa_coords}{Data frame with coordinates of presences and absences to extract the values from the layers, and a column (named 'pa') indicating presence (1) or absence (0).}

\item{layers}{A SpatRaster with the environmental layers.}

\item{seed}{Random seed.}
}
\value{
A BART model object.
}
\description{
This function fits a Bayesian Additive Regression Trees (BART) model using
presence/absence data and environmental covariate layers.
}
