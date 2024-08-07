% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_validate_files.R
\name{read_extent_polygon}
\alias{read_extent_polygon}
\title{Read and Validate Extent Polygon}
\usage{
read_extent_polygon(file_path, show_modal = FALSE)
}
\arguments{
\item{file_path}{Path to the polygon file containing the extent.}

\item{show_modal}{Optional. Logical. Whether to show a modal notification for warnings. Default is FALSE.}
}
\value{
A spatial object representing the extent if the file is valid, NULL otherwise.
}
\description{
This function reads and validates a polygon file containing the extent. It checks if the file has the correct format and extracts the geometry.
}
\keyword{internal}
