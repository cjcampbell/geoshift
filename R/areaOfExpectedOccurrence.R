#' Calculate the area of a region of expected occurrence from a presence/absence surface
#'
#' This function calculates the geographic area of the occupied region of a
#' presence-absence model. The input must be a rasterLayer object with values
#' of NA (out of range), 0 (not expected to occur), or 1 (expected to occur).
#'
#' @param surface rasterLayer with values of NA, 0, and 1
#' @return A units object of area of expected occurrence
#'
#' @importFrom raster raster
#' @importFrom raster reclassify
#' @importFrom stars st_as_stars
#' @importFrom sf st_as_sf
#' @importFrom sf st_area
#' @importFrom magrittr %>%
#'
#' @export
areaOfExpectedOccurrence <- function(surface) {
  stopifnot(
    "surface must be of class 'RasterLayer'" =
      { class(surface) == "RasterLayer" } ,
    "surface must be a binary surface of 0's, 1's, and NA's." =
      { all( unique(surface[]) %in% c(NA, 0, 1) ) }
  )

  surface %>%
    # Retain only area where presence == TRUE
    raster::reclassify( matrix(c(-Inf,0.5,NA), ncol = 3, byrow = T)) %>%
    # Convert to stars
    stars::st_as_stars() %>%
    # Convert to sf
    sf::st_as_sf(merge = T) %>%
    # Calculate sum of area(s).
    sf::st_area() %>%
    sum()
}

#' @example
#' r <- raster::raster(matrix(data = rep(c(0,1,NA),3), nrow = 3, ncol = 3))
#' areaOfExpectedOccurrence(r)
