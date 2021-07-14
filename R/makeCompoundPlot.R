#' Make overall summary plot from two rasterlayer surfaces.
#'
#'
#' @rdname makeCompoundPlot
#' @param rast1 First input rasterLayer
#' @param rast2 Second input rasterLayer
#' @param rastnames Character vector of layerNames to be compared (defaults to "Summer" and "Winter" for rast1 and rast2 respectively)
#' @param species Character vector of species, name of subdirectory where results are (optionally) saved.
#' @param myEllipses List output of 'makeDataEllipse' function. If not provided, calculations to fit dataEllipse will be rerun. Arguments for makeDataEllipse function should then be provided.
#' @param csvSaveDir Optional file path to save csv object to.
#' @param ... DataFrame column nams to populated geom_tile(aes()), e.g., : x=x,y=y,fill=value
#'
#' @importFrom magrittr %>%
#' @import ggplot2
#' @importFrom gridExtra tableGrob arrangeGrob
#' @importFrom rgeos gCentroid
#' @import sf
#' @importFrom scales muted
#' @importFrom dplyr rename_all mutate_if funs
#'
#' @seealso makeDataEllipse
#' @seealso surfaceEquivalency
#'
#' @return A list containing species name, rastname, metrics of surface equivalency, ellipse areas, ratio of ellipse areas, percent of ellipse intersected, centroid latitude and longitude for each ellipse, and distance and bearing between each centroid.
#'
#' @export
makeCompoundPlot <- function(rast1, rast2, rastnames = c("Summer", "Winter"), myEllipses,
                             species, summaryTable, pngSaveDirectory = FALSE, ...) {

  similaritySurface <- schoenersProjection( rast2,rast1, abs = FALSE )
  similaritySurface_df <- surface2df(similaritySurface)
  p <- ggplot2::ggplot() +
    ggplot2::geom_tile(data = similaritySurface_df , aes(...)) +
    ggplot2::geom_sf(myEllipses[[1]]$sf, mapping = aes(), fill = NA, col = "red") +
    ggplot2::geom_sf(data = rgeos::gCentroid(myEllipses[[1]]$st) %>% sf::st_as_sf(), mapping = aes(), col = "red") +
    ggplot2::geom_sf(myEllipses[[2]]$sf, mapping = aes(), fill = NA, col = "blue") +
    ggplot2::geom_sf(data = rgeos::gCentroid(myEllipses[[2]]$st) %>% sf::st_as_sf(), mapping = aes(), col = "blue") +
    ggplot2::scale_fill_gradient2(
      name = NULL,
      midpoint = 1, labels = c("Winter", "Summer"),
      breaks = c(0.7, 1.3),
      limits = c(0.6,1.5),
      low = scales::muted("blue"),
      mid = "grey90",
      high = scales::muted("red")
    ) +
    ggplot2::coord_sf() +
    ggplot2::xlab(NULL) + ggplot2::ylab(NULL)+
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = element_blank(),
      legend.position = "bottom"
    )
  mytab <- summaryTable %>%
    dplyr::rename_all(dplyr::funs(gsub("[[:punct:]]", "\\n", make.names(names(summaryTable))))) %>%
    dplyr::mutate_if(is.numeric, round, digits = 2) %>%
    gridExtra::tableGrob(
      theme = ggplot2::theme_minimal(
        core = list(fg_params=list(cex = 0.5)),
        colhead = list(fg_params=list(cex = 0.5)),
        rowhead = list(fg_params=list(cex = 0.5)))
    )
  p2 <- gridExtra::arrangeGrob(p,mytab , nrow = 2, heights = c(5,1))
  if(!isFALSE(pngSaveDirectory)) {
    pngSubdir <- file.path(pngSaveDirectory, species)
    if(!dir.exists(pngSubdir) ) { dir.create(pngSubdir) }
    ggsave(plot = p2, width = 8, height = 6, units = "in", dpi = 300,
           filename = file.path(pngSubdir, paste0(paste(
             species, "combinationPlot", sep = "_" ), ".png") )
           )
  }
  return(p2)
}
