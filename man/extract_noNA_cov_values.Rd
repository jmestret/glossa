% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{extract_noNA_cov_values}
\alias{extract_noNA_cov_values}
\title{Extract Non-NA Covariate Values}
\usage{
extract_noNA_cov_values(data, covariate_layers, predictor_variables)
}
\arguments{
\item{data}{A data frame containing species occurrence data with columns x/long (first column) and y/lat (second column).}

\item{covariate_layers}{A list of raster layers representing covariates.}

\item{predictor_variables}{Variables to select from all the layers.}
}
\value{
A data frame containing species occurrence data with covariate values, excluding NA values.
}
\description{
This function extracts covariate values for species occurrences, excluding NA values.
}
\details{
This function extracts covariate values for each species occurrence location from the provided covariate layers. It returns a data frame containing species occurrence data with covariate values, excluding any NA values.
}
