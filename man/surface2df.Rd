% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/surface2df.R
\name{surface2df}
\alias{surface2df}
\title{Cleanly convert a rasterLayer to dataframe format}
\usage{
surface2df(surface)
}
\arguments{
\item{surface}{rasterLayer to be converted to a dataframe}
}
\value{
A 3-column dataframe with coordinates and value.
}
\description{
This function makes a clean convert of a rasterLayer to a 3-column dataframe.
The first two columns will contain 'x' and 'y' coordinates, the third the
rasterLayer 'value'. NA's are automatically removed.
}
