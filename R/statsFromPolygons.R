#' Extract statistics from two sf polygons.
#'
#' Extract metrics of range size change, seasonality, and centroid distance
#' from two sf polygons.
#'
#'@param sf1 sfc_MULTIPOLYGON representing some surface from time 1
#'@param sf2 sfc_MULTIPOLYGON representing some surface from time 2
#'@param ... Additional arguments to append to the output data.frame
#'
#'@return A data.frame containing comparison metrics between time 1 and 2.
#'
#'@importFrom sf st_intersection
#'@importFrom sf st_area
#'@importFrom sf st_union
#'@importFrom sf st_crs
#'@importFrom sf st_distance
#'@importFrom sf st_centroid
#'
#'
#'@export
statsFromPolygons <- function(sf1, sf2, ...) {
  stopifnot(
    {"sf1 and sf2 must be of class 'sfc_MULTIPOLYGON'." = all(class(sf1) %in% c(c( "sfc_MULTIPOLYGON", "sfc" ), c("sfc_POLYGON", "sfc")) )},
    {"sf1 and sf2 must both be of class 'sfc_MULTIPOLYGON'." = all(class(sf1) == class(sf2))}
  )

  # Range size change
  area_1 <- sf::st_area(sf1)
  area_2 <- sf::st_area(sf2)
  rangeSizeChange <- area_1 / area_2

  # Year round v.s. seasonal
  area_yrRound  <- sf::st_intersection( sf1, sf2 ) %>% sf::st_area() %>% as.numeric() %>% sum()
  if(length(area_yrRound) == 0) {area_yrRound <- 0}
  area_s1only   <- sf::st_difference(   sf1, sf2 ) %>% sf::st_area() %>% as.numeric() %>% sum()
  area_s2only   <- sf::st_difference(   sf2, sf1 ) %>% sf::st_area() %>% as.numeric() %>% sum()
  if(length(area_s1only) == 0) { area_s1only <- 0 }
  if(length(area_s2only) == 0) { area_s2only <- 0 }
  area_seasonal <- area_s1only + area_s2only
  area_all      <- sf::st_union( sf1, sf2 ) %>% sf::st_area() %>% as.numeric() %>% sum()
  # Mitigate against 0's--
  if(area_seasonal == area_all) {
    yrRoundVseasonal <- 1
  } else if(area_yrRound == area_all) {
    yrRoundVseasonal <- -1
  } else {
    yrRoundVseasonal <- as.numeric( (area_seasonal / area_all) - (area_yrRound / area_all) )
  }

  # Centroid distance.
  centroidDist <- st_distance(st_centroid(sf1), st_centroid(sf2))
  if(!is.na(sf::st_crs(sf1))) {
    units(centroidDist) <- "km"
  }

  # data.frame with returned details.
  df <- data.frame(
    ...,
    rangeSizeChange = rangeSizeChange,
    yrRoundVseasonal = yrRoundVseasonal,
    centroidDist_km = centroidDist
  )
  return(df)
}
