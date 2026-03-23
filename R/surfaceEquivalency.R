#' Compare two raster surfaces.
#'
#' Functions to compare rasters using Schoener's D-metric.
#'
#' The function schoenersD' calculates similarity value of two spatRaster
#' using Schoener's D-metric.
#'
#' The function 'schoenersProjection' returns a third rasterLayer with the
#' cell-by-cell difference between surfaces.
#'
#'
#' SpatRasters must have identical resolutions and extents. Schoener's D assumes
#' surfaces each sum to 1.
#'
#' @rdname surfaceSimilarity
#' @param rast1 First input SpatRaster
#' @param rast2 Second input SpatRaster
#'
#' @importFrom terra global
#'
#' @export
schoenersD <- function(rast1, rast2) {
  if (class(rast1) != "SpatRaster" | class(rast2) != "SpatRaster")
    stop("arguments are not of class 'SpatRaster'")

  r1 <- rast1/{ terra::global(rast1, fun = 'sum', na.rm = T)[[1]]}
  r2 <- rast2/{ terra::global(rast2, fun = 'sum', na.rm = T)[[1]]}

  1 - (0.5 * {terra::global(abs(r1 - r2), fun = "sum", na.rm = T)[[1]]})
}

#' @rdname surfaceSimilarity
#'
#' @param rast1 First input spatRaster
#' @param rast2 Second input spatRaster
#' @param abs Specify whether to return the absolute or relative change between rast1 and rast2
#'
#' @export
schoenersProjection <- function(rast1, rast2, abs = TRUE) {

  r1 <- rast1/{ terra::global(rast1, fun = 'sum', na.rm = T)[[1]]}
  r2 <- rast2/{ terra::global(rast2, fun = 'sum', na.rm = T)[[1]]}

  if(abs == TRUE){
    out <- 1 - (0.5*abs(r1 - r2))
  } else {
    out <-  1 - (0.5*(r1 - r2))
  }
  return(out)
}
