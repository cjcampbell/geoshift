#' Example data
#'
#' A subset of occurence data derived from BirdLife International and Handbook of the Birds of the World (2020) for two species: Aeronautes saxatalis and Dumetella carolinensis.
#' Records have been projected into an equal-area projection, and coordinates slightly jittered.
#'
#' @format ## `example_points`
#' An sf data frame with 2,000 rows and 3 columns:
#' \describe{
#'   \item{species}{Species scientific binomial}
#'   \item{season}{Season for which observation was made; breeding or nonbreeding}
#'   \item{geometry}{Spatial geometry of record}
#'   \item{CRS}{+proj=laea +lat_0=16.72 +lon_0=-98.44 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs}
#'   ...
#' }
#' @source BirdLife International and Handbook of the Birds of the World (2020)
"example_points"


#' Example SDM output for <i>Dumetella carolinensis</i>.
#'
#' A GeoTIFF raster containing probability of occurrence model predictions for <i>Dumetella carolinensis</i>.
#' The layers reflect continuous probabilities for breeding and nonbreeding seasons, then binarized (presence/absence, or "PA") probabilties for breeding and nonbreeding seasons.
#'
#'
#' @format A single-layer GeoTIFF raster with the following properties:
#' \describe{
#'   \item{CRS}{+proj=laea +lat_0=16.72 +lon_0=-98.44 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs}
#'   \item{Resolution}{44950, 46150m (~50 km)}
#'   \item{Extent}{-2073193, 3365757, -1199002, 4431298}
#'   \item{Values}{Probablity of occurrence for Dumetella carolinensis in continuous and binarized ("PA") probabilities, for breeding and nonbreeding seasons}
#' }
#'
#' @source Derived from a model generated for Campbell & Belitz et al.; data from BirdLife International and Handbook of the Birds of the World (2020).
#' @name Dumetella_carolinensis.tif
#' @docType data
NULL


#' Example SDM output for <i>Aeronautes saxatalis</i>.
#'
#' A GeoTIFF raster containing probability of occurrence model predictions for <i>Aeronautes saxatalis</i>.
#' The layers reflect continuous probabilities for breeding and nonbreeding seasons, then binarized (presence/absence, or "PA") probabilties for breeding and nonbreeding seasons.
#'
#'
#' @format A single-layer GeoTIFF raster with the following properties:
#' \describe{
#'   \item{CRS}{+proj=laea +lat_0=16.72 +lon_0=-98.44 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs}
#'   \item{Resolution}{44950, 46150m (~50 km)}
#'   \item{Extent}{-2073193, 3365757, -1199002, 4431298}
#'   \item{Values}{Probablity of occurrence for Aeronautes saxatalis in continuous and binarized ("PA") probabilities, for breeding and nonbreeding seasons}
#' }
#'
#' @source Derived from a model generated for Campbell & Belitz et al.; data from BirdLife International and Handbook of the Birds of the World (2020).
#' @name Aeronautes_saxatalis.tif
#' @docType data
NULL

