#' Load example raster data
#'
#' This function loads example raster data from .tif files.
#'
#' @param rastName Name of raster to be loaded. Can be 'Aeronautes_saxatalis.tif' or 'Dumetella_carolinensis.tif'.
#'
#' @return A spatRaster with four layers: a continuous probability suface for breeding and nonbreeding seasons (layers 1-2) and a presence/absense ("PA") surface for breeding and nonbreeding seasons (layers 3-4).
#'
#' @importFrom terra rast
#'
#' @export
loadRaster <- function(rastName = "Aeronautes_saxatalis.tif") {

  stopifnot(
    'rastName must be one of: "Aeronautes_saxatalis.tif", "Dumetella_carolinensis.tif"' =
      rastName %in% c("Aeronautes_saxatalis.tif", "Dumetella_carolinensis.tif")
  )

  f <- system.file("extdata", rastName, package = "geoshift")
  terra::rast(f)
}
