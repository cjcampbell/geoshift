#' Cleanly convert a rasterLayer to dataframe format
#'
#' This function makes a clean convert of a rasterLayer to a 3-column dataframe.
#' The first two columns will contain 'x' and 'y' coordinates, the third the
#' rasterLayer 'value'. NA's are automatically removed.
#'
#' @param surface rasterLayer to be converted to a dataframe
#' @return A 3-column dataframe with coordinates and value.
#'
#' @export
surface2df <- function(surface) {
  surface %>%
    raster::as.data.frame(
      xy = T, na.rm = T
    ) %>%
    dplyr::rename(value = 3 )
}
