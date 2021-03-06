% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggheatmap.R
\name{ggheatmap}
\alias{ggheatmap}
\title{Function to run pipeline}
\usage{
ggheatmap(
  matrix,
  scale = "row",
  Rowv = TRUE,
  Colv = TRUE,
  PRINT = TRUE,
  verbose = FALSE
)
}
\arguments{
\item{scale}{(row/col/none) how should the data be scaled (default: row)}

\item{Rowv}{(TRUE/FALSE) should the rows be clustered (default: TRUE)}

\item{Colv}{(TRUE/FALSE) should the columns be clustered (default: TRUE)}

\item{PRINT}{TRUE/FALSE should the plot be displayed (default: TRUE)}

\item{verbose}{TRUE/FALSE shows extra info (default: FALSE)}

\item{heatmapClass}{object of class heatmap (required)}
}
\description{
Runs the entire pipeline to generate a heatmap
}
\examples{
heatmapClass <- plotHeatmap(heatmapClass, scale="row", Rowv = TRUE, Colv = TRUE)
}
\keyword{heatmap}
\keyword{plot}
