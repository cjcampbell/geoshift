#'
#' Function to return data ellipse around a give proportion of points
#' (prop_points) sampled from a surface.
#'
#' @rdname DataEllipse
#' @param coords coordinates of a surface-as-dataframe
#' @param weights the value of the surface-as-dataframe, weights the subsampling.
#' @param n number of sampling replications
#' @param prop_points what proportion of the subsampled cells should be included in the ellipses
#'
#' @importFrom purrr map
#' @importFrom sp CRS Polygons SpatialPolygons
#' @importFrom car dataEllipse
#'
#' @return Returns a list of length 2. First item is a spatialPolygon, the second an sf object.
#' @export
makeDataEllipse <- function(coords,  weights, n = 10000, prop_points) {

  set.seed(420)
  rownums <- sample(1:nrow(coords), size = n, replace = TRUE, prob = unlist(weights) )

  A <- coords[rownums, ]
  Y <- cbind(A[,1], A[,2]) # Make matrix format.

  ellipsePts <- car::dataEllipse(x = Y[,1], y = Y[,2], levels = prop_points)
  myEllipse <- list()
  myEllipse$st <- SpatialPolygons(
    list(sp::Polygons(list(with(list(x = ellipsePts[,2], y = ellipsePts[,1]),Polygon(ellipsePts))),1)),
    proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
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
#' @param weights The value of the surface-as-dataframe (third column), weights the subsampling
#'
#' @export
makeEllipses <- function(rast1, rast2, weights) {
  list( rast1, rast2 ) %>%
    purrr::map(~{
      makeDataEllipse(coords = .[ ,1:2], weights = .[ ,3], prop = 0.5, n = 1000)
    })
}
