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
#' @export
schoenersD <- function(rast1, rast2) {
  if (class(rast1) != "RasterLayer" | class(rast2) != "RasterLayer")
    stop("arguments are not of class 'RasterLayer'")
  1 - (0.5 * raster::cellStats(abs(rast1 - rast2), stat = "sum"))
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
  # Show difference btwn 2 surfaces
  #
  #
  if(abs == TRUE){
    out <- 1 - (0.5*abs(rast1 - rast2))
  } else {
    out <-  1 - (0.5*(rast1 - rast2))
  }
  return(out)
}
