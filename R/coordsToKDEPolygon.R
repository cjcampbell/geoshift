#' Determine the KDE of occurrence coordinates and convert to polygon at density threshold
#'
#' Calculate the kernel density estimate (KDE) of input coordinates. Convert to
#' an sf polygon at a specified density threshold.
#'
#' @importFrom sp SpatialPoints
#' @importFrom adehabitatHR kernelUD
#' @importFrom adehabitatHR getverticeshr
#' @importFrom sf st_as_sf
#' @importFrom sf st_combine
#' @importFrom sf st_make_valid
#' @importFrom sf st_crs
#'
#' @param coords Two-column data.frame, matrix, or SpatialPoints object containing longitude and latitude
#' @param percentile Numeric density threshold percentile at which to draw output polygon
#' @param proj4string Character string containing CRS information for coords.
#' @param extent Controls the extent of the grid for estimating KDE.
#'
#' @return An sf polygon object of specified proj4string
#'
#' @export
coordsToKDEPolygon <- function(coords, percentile = 50, proj4string, extent = 1) {
  stopifnot(
    { class(percentile) == "numeric" },
    { class(proj4string) == "character" }
  )

  if( any(class(coords) %in% c("data.frame", "matrix")) ) {
    coords_sp <- sp::SpatialPoints(coords)
  } else if(class(coords) == "SpatialPoints") {
    coords_sp <- coords
  } else stop("Class of coords argument must be data.frame, matrix, or SpatialPoints")

  o <- coords_sp %>%
    adehabitatHR::kernelUD(extent = extent) %>%
    adehabitatHR::getverticeshr(percent = percentile) %>%
    sf::st_as_sf(crs = st_crs(proj4string)) %>%
    sf::st_combine() %>%
    sf::st_make_valid()
  sf::st_crs(o) <- proj4string
  return(o)
}
