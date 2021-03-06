% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scaleMatrix.R
\name{scaleMatrix}
\alias{scaleMatrix}
\title{Function to scale the data}
\usage{
scaleMatrix(heatmapClass, type = "row")
}
\arguments{
\item{heatmapClass}{object of class heatmap (required)}

\item{type}{"row" or "column" scaling (default: row)}
}
\description{
takes an object of the class heatmapClass and scales the data using scale, either by row (default) or by column.
}
\examples{
heatmapClass <- scaleMatrix(heatmapClass, type = "row")
}
\keyword{scaling}
