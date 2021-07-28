#'
#' Function to return data ellipse around a give proportion of points
#' (prop_points) sampled from a surface.
#'
#' @rdname DataEllipse
#' @param coords coordinates of a surface-as-dataframe
#' @param weights the value of the surface-as-dataframe, weights the subsampling.
#' @param n number of sampling replications
#' @param prop_points what proportion of the subsampled cells should be included in the ellipses
#' @param crsString string of crs coordinates, should match raster projections.
#'
#' @importFrom purrr map
#' @importFrom sp CRS Polygons Polygon SpatialPolygons
#' @importFrom car dataEllipse
#' @importFrom sf st_as_sf
#' @importFrom purrr map
#'
#' @return Returns a list of length 2. First item is a spatialPolygon, the second an sf object.
#' @export
makeDataEllipse <- function(coords, crsString, weights, n = 10000, prop_points) {

  set.seed(420)
  rownums <- sample(1:nrow(coords), size = n, replace = TRUE, prob = unlist(weights) )

  A <- coords[rownums, ]
  Y <- cbind(A[,1], A[,2]) # Make matrix format.

  ellipsePts <- car::dataEllipse(x = Y[,1], y = Y[,2], levels = prop_points)
  myEllipse <- list()
  myEllipse$st <- SpatialPolygons(
    list(sp::Polygons(list(with(list(x = ellipsePts[,2], y = ellipsePts[,1]),sp::Polygon(ellipsePts))),1)),
    proj4string = crsString
  )
  myEllipse$sf <- myEllipse$st %>%
    sf::st_as_sf()
  return(myEllipse)

}

#' makeEllipses
#'
#' Wrapper around makeDataEllipse to automatically call the correct columns.
#' Assumes columns are arranged as x,y,value.
#'
#' @param rast1 First input rasterLayer
#' @param rast2 Second input rasterLayer
#' @param n number of sampling replications
#' @param prop_points what proportion of the subsampled cells should be included in the ellipses
#' @param crsString string of crs coordinates, should match raster projections.
#'
#' @importFrom raster crs
#' @importFrom purrr map
#'
#' @return A list of ellipse objects for rast1 and rast2 respectively
#'
#' @export
makeEllipses <- function(rast1, rast2, prop_points = 0.5, n = 1000, crsString = FALSE) {

  if(class(rast1) == "RasterLayer" & class(rast2) == "RasterLayer" ) {
    myCrs <- raster::crs(rast1)
  } else {
    if(crsString == FALSE) stop("crsString can't be FALSE if class of rast1 and rast2 are not raster objects")
    myCrs <- crsString
  }

  if(class(rast1) == "RasterLayer") { rast1 <- surface2df(rast1) }
  if(class(rast2) == "RasterLayer") { rast2 <- surface2df(rast2) }

  list( rast1, rast2 ) %>%
    purrr::map(~{
      makeDataEllipse(coords = .[ ,1:2], weights = .[ ,3], crsString = myCrs, prop_points = prop_points, n = n)
    })
}
