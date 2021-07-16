#' Calculate the change in rasterLayer values between rast1 and rast2 at the presence points used to create each of those surfaces.
#'
#'
#' @rdname calculateChangeAtPoints
#'
#' @param rast1 First input rasterLayer
#' @param rast2 Second input rasterLayer
#' @param rastnames Character vector of layerNames to be compared (defaults to "Summer" and "Winter" for rast1 and rast2 respectively)
#' @param pts1 Points used to create rast1
#' @param pts2 Points used to create rast2
#'
#' @importFrom dplyr mutate case_when
#' @importFrom raster stack extract
#' @importFrom magrittr %>%
#'
#' @return A dataframe containing occurrence points categorized by their associated rasterLayer and the difference at that point between the associated and the opposing layer.
#'
#' @export
calculateChangeAtPoints <- function(rast1, rast2, rastnames, pts1, pts2) {
  mystack <- raster::stack(rast1, rast2)
  names(mystack) <- rastnames
  pts_both <- rbind(
    data.frame(pts1, time = rastnames[1]),
    data.frame(pts2, time = rastnames[2])
  )
  mdf <- data.frame(
    pts_both,
    raster::extract(mystack, pts_both[,1:2])
  ) %>%
    dplyr::mutate(
      difference = dplyr::case_when(
        time == rastnames[1] ~ .data[[rastnames[1]]] - .data[[rastnames[2]]],
        time == rastnames[2] ~ .data[[rastnames[2]]] - .data[[rastnames[1]]]
      )
    )
  return(mdf)
}
