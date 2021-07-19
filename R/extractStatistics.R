#' Extract comparative statistics from two rasterlayer surfaces.
#'
#'
#' Extract metrics of surface equivalency, ellipse overlap, ellipse centroid bearing and distance.
#'
#' RasterLayers must have identical resolutions and extents. Schoener's D assumes
#' surfaces each sum to 1.
#'
#' @rdname extractStatistics
#' @param rast1 First input rasterLayer
#' @param rast2 Second input rasterLayer
#' @param rastnames Character vector of layerNames to be compared (defaults to "Summer" and "Winter" for rast1 and rast2 respectively)
#' @param species Character vector of species, name of subdirectory where results are (optionally) saved.
#' @param myEllipses List output of 'makeDataEllipse' function. If not provided, calculations to fit dataEllipse will be rerun. Arguments for makeDataEllipse function should then be provided.
#' @param csvSavePath Optional file path to save csv object to.
#' @param prop_points Proportion of the points sampled from the surface to be included in ellipses drawn
#' @param ... Any additional arguments to be included in the output data.frame
#'
#' @importFrom utils write.csv
#' @importFrom geosphere bearing
#' @importFrom units set_units
#' @import sf
#'
#' @seealso makeDataEllipse
#' @seealso surfaceEquivalency
#'
#' @return A data.frame containing species name, layerNames to be compared (defaults to "Summer" and "Winter" for rast1 and rast2 respectively), metrics of surface equivalency, ellipse areas, ratio of ellipse areas, percent of ellipse intersected, centroid latitude and longitude for each ellipse, and distance and bearing between each centroid.
#'
#' @export
extractStatistics <- function(rast1, rast2, rastnames = c("Summer", "Winter"), species, myEllipses = NULL, csvSavePath = FALSE, prop_points = 0.5, ...) {

  if(is.null(myEllipses)) {
    df_surface_rast1 <- surface2df(rast1)
    df_surface_rast2 <- surface2df(rast2)
    myEllipses <- list(
      makeDataEllipse(coords = df_surface_rast1[,1:2], weights =  df_surface_rast1[,3], prop_points = prop_points),
      makeDataEllipse(coords = df_surface_rast2[,1:2], weights =  df_surface_rast2[,3], prop_points = prop_points)
    )
  }

  suppressMessages({suppressWarnings({
    a1 <- myEllipses[[1]]$sf %>% sf::st_area() %>% units::set_units("km^2")
    a2 <- myEllipses[[2]]$sf %>% sf::st_area() %>% units::set_units("km^2")
    overlapArea <- sf::st_intersection(myEllipses[[1]]$sf, myEllipses[[2]]$sf) %>%
      sf::st_area() %>% units::set_units("km^2")
    Ellipse.Area = c(a1, a2)
    Area.Ratio =  Ellipse.Area / min(Ellipse.Area)
    percent.Ellipse.intersected = overlapArea / c(a1, a2)*100
    if(length(percent.Ellipse.intersected) == 0) { percent.Ellipse.intersected <- c(0,0) }
    centroid1 <- sf::st_centroid( myEllipses[[1]]$sf )
    centroid2 <- sf::st_centroid( myEllipses[[2]]$sf )
    centroid.lat <- c( sf::st_coordinates(centroid1)[2], sf::st_coordinates(centroid2)[2] )
    centroid.lon <- c( sf::st_coordinates(centroid1)[1], sf::st_coordinates(centroid2)[1] )
    centroid.distance <- sf::st_distance(centroid1, centroid2) %>% units::set_units("km")
    centroid.bearing1 <- geosphere::bearing(sf::as_Spatial(centroid1), sf::as_Spatial(centroid2))
    centroid.bearing2 <- geosphere::bearing(sf::as_Spatial(centroid2), sf::as_Spatial(centroid1))
  })})

  mdf <- data.frame(
    species = rep(species,2),
    schoenersD = rep(schoenersD(rast1,rast2), 2),
    timeChunk = c(rastnames[1], rastnames[2]),
    Ellipse.Area, Area.Ratio, percent.Ellipse.intersected,
    centroid.lat, centroid.lon,
    centroid.distance = rep(centroid.distance, 2),
    centroid.bearing = c(centroid.bearing1, centroid.bearing2 ),
    ...
  )

  if(!isFALSE(csvSavePath)) { utils::write.csv( mdf, file = csvSavePath, row.names = FALSE ) }

  return(mdf)

}
