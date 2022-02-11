#' Calculate the convex hull of occurrence coordinates
#'
#' Convert occurrence coordinates to convex hull. Returned in same projection.
#'
#' @importFrom sp SpatialPoints
#' @importFrom sf st_as_sf
#' @importFrom sf st_union
#' @importFrom sf st_convex_hull
#' @importFrom sf st_crs
#'
#' @param coords Two-column data.frame, matrix, or SpatialPoints object containing longitude and latitude
#' @param proj4string Character string containing CRS information for coords.
#'
#' @return An sf polygon object of specified proj4string
#'
#' @export
coordsToConvexHull <- function(coords, proj4string) {
  stopifnot(
    { class(proj4string) == "character" }
  )

  if( any(class(coords) %in% c("data.frame", "matrix")) ) {
    coords_sp <- sp::SpatialPoints(coords)
  } else if(class(coords) == "SpatialPoints") {
    coords_sp <- coords
  } else stop("Class of coords argument must be data.frame, matrix, or SpatialPoints")

  o <- coords_sp %>%
    st_as_sf(crs = sf::st_crs(proj4string)) %>%
    st_union() %>%
    st_convex_hull()
  sf::st_crs(o) <- proj4string
  return(o)
}
