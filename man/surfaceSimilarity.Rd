% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/surfaceEquivalency.R
\name{schoenersD}
\alias{schoenersD}
\alias{schoenersProjection}
\title{Compare two rasterLayer surfaces.}
\usage{
schoenersD(rast1, rast2)

schoenersProjection(rast1, rast2, abs = TRUE)
}
\arguments{
\item{rast1}{First input rasterLayer}

\item{rast2}{Second input rasterLayer}

\item{abs}{Specify whether to return the absolute or relative change between rast1 and rast2}
}
\description{
Functions to compare rasterLayers using Schoener's D-metric.
}
\details{
The function schoenersD' calculates similarity value of two RasterLayers
using Schoener's D-metric.

The function 'schoenersProjection' returns a third rasterLayer with the
cell-by-cell difference between surfaces.

RasterLayers must have identical resolutions and extents. Schoener's D assumes
surfaces each sum to 1.
}
