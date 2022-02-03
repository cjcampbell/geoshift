#' Calculate the change in rasterLayer values between rast1 and rast2 at the presence points used to create each of those surfaces.
#'
#' This function extracts rasterLayer values for each time-associated rasterLayer, retaining a value for each cell where an occurrence point is found. It returns a data.frame with the difference in rasterLayer value for each retained point for its sampling timestep vs. the opposing timestep.
#'
#' @rdname calculateChangeAtPoints
#'
#' @param rast1 First input rasterLayer
#' @param rast2 Second input rasterLayer
#' @param rastnames Character vector of layerNames to be compared (defaults to "Summer" and "Winter" for rast1 and rast2 respectively)
#' @param pts1 Points used to create rast1
#' @param pts2 Points used to create rast2
#' @param ... Optional extra columns to include in output data.frame.
#'
#' @importFrom dplyr mutate case_when
#' @importFrom raster stack extract
#' @importFrom magrittr %>%
#'
#' @return A dataframe containing occurrence points categorized by their associated rasterLayer and the difference at that point between the associated and the opposing layer.
#'
#' @export
calculateChangeAtPoints <- function(rast1, rast2, rastnames, pts1, pts2, ...) {
  mystack <- raster::stack(rast1, rast2)
  names(mystack) <- rastnames

  # Time 1
  df1 <- data.frame(
    pts1, time = rastnames[1],
    raster::extract(mystack, pts1[,1:2], cellnumbers=TRUE)
    ) %>%
    dplyr::distinct(cells, .keep_all = TRUE)
  if( nrow(pts1) - nrow(df1) > 0 ) {
    warning(paste("omitting",  nrow(pts1) - nrow(df1), "pts for time", rastnames[1], "due to repeated sampling in a particular raster cell.") )
  }  # Time 2
  df2 <- data.frame(
    pts2, time = rastnames[2],
    raster::extract(mystack, pts2[,1:2], cellnumbers=TRUE)
    ) %>%
    dplyr::distinct(cells, .keep_all = TRUE)
  if( nrow(pts2) - nrow(df2) > 0 ) { warning(paste("omitting",  nrow(pts2) - nrow(df2), "pts for time", rastnames[2], "due to repeated sampling in a particular raster cell.") ) }

  # Combine and calculate difference across timesteps.
  mdf <- data.frame(rbind(df1, df2 ), ...) %>%
    dplyr::mutate(
      difference = dplyr::case_when(
        time == rastnames[1] ~ .data[[rastnames[1]]] - .data[[rastnames[2]]],
        time == rastnames[2] ~ .data[[rastnames[2]]] - .data[[rastnames[1]]]
      )
    )
  return(mdf)
}
