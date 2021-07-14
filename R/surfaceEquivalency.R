#' Compare two rasterLayer surfaces.
#'
#' Functions to compare rasterLayers using Schoener's D-metric.
#'
#' The function schoenersD' calculates similarity value of two RasterLayers
#' using Schoener's D-metric.
#'
#' The function 'schoenersProjection' returns a third rasterLayer with the
#' cell-by-cell difference between surfaces.
#'
#'
#' RasterLayers must have identical resolutions and extents. Schoener's D assumes
#' surfaces each sum to 1.
#'
#' @rdname surfaceSimilarity
#' @param rast1 First input rasterLayer
#' @param rast2 Second input rasterLayer
#'
#' @importFrom raster cellStats
#'
#' @export
schoenersD <- function(rast1, rast2) {
  if (class(rast1) != "RasterLayer" | class(rast2) != "RasterLayer")
    stop("arguments are not of class 'RasterLayer'")

  r1 <- rast1/raster::cellStats(rast1, stat = 'sum')
  r2 <- rast2/raster::cellStats(rast2, stat = 'sum')

  1 - (0.5 * raster::cellStats(abs(r1 - r2), stat = "sum"))
}

#' @rdname surfaceSimilarity
#'
#' @param rast1 First input rasterLayer
#' @param rast2 Second input rasterLayer
#' @param abs Specify whether to return the absolute or relative change between rast1 and rast2
#'
#' @export
schoenersProjection <- function(rast1, rast2, abs = TRUE) {
  #
  r1 <- rast1/raster::cellStats(rast1, stat = 'sum')
  r2 <- rast2/raster::cellStats(rast2, stat = 'sum')
  #
  if(abs == TRUE){
    out <- 1 - (0.5*abs(r1 - r2))
  } else {
    out <-  1 - (0.5*(r1 - r2))
  }
  return(out)
}
