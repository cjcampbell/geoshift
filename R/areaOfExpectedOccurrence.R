#' Calculate the area of a region of expected occurrence from a presence/absence surface
#'
#' This function calculates the geographic area of the occupied region of a
#' presence-absence model. The input must be a rasterLayer object with values
#' of NA (out of range), 0 (not expected to occur), or 1 (expected to occur).
#'
#' @param surface rasterLayer with values of NA, 0, and 1
#' @return A units object of area of expected occurrence
#'
#' @importFrom terra rast
#' @importFrom terra rast
#' @importFrom terra expanse
#'
#' @export
areaOfExpectedOccurrence <- function(surface, unit = "km") {
  stopifnot(
    "surface must be of class 'SpatRaster'" =
      { class(surface) == "SpatRaster" } ,
    "surface must be a binary surface of 0's, 1's, and NA's." =
      { all( unique(as.numeric(unique(values(surface)))) %in% c(NA, NaN, 0, 1)) } ,
    'Paramater "unit" must be one of: "m", "km", or "ha"' =
      { unit %in% c( "m", "km", "ha") }
  )

  # Retain only cells containing 1.
  r_clean <- terra::subst(surface, from = 0, to = NA)

  # Calculate the area of cells containing 1's.
  out <- terra::expanse(r_clean, unit = unit)$area

  return(out)
}

#' @example
#' # Generate dummy raster.
#' set.seed(42)
#' r <- terra::rast(matrix(data = sample(c(0,1,NA), size = 100, replace = T, prob = c(1,2,0.1)), nrow = 10, ncol = 10), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
#' areaOfExpectedOccurrence(r)
#'
#' # Load example presence-absence raster and calculate area in km (default).
#' Aeronautes_saxatalis_PA <- loadRaster()[[3:4]]
#' areaOfExpectedOccurrence(Aeronautes_saxatalis_PA)
#'
#' # Load an example raster for a second species, and calculate area in ha.
#' Dumetella_carolinensis_PA <- loadRaster("Dumetella_carolinensis.tif")[[3:4]]
#' areaOfExpectedOccurrence(Dumetella_carolinensis_PA,  unit = "ha")
