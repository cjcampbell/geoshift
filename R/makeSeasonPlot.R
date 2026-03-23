#' Make a seasonal plot, save to a specific path.
#'
#' @param surface Either a fortified raster (array data presented in a 3-column dataframe), or a single spatRaster.
#' @param title Title (character) to include in title. Optional. Typically a species name.
#' @param season season name (character) to specify plot color and file path. Defaults to muted red for specified seasons, and muted blue for other seasons. Specified seasons for default red color include case-invariant: "Summer", "Breeding", "1", "s1", "warm", and "red"
#' @param maxCol color (character) to overwrite season argument, sets maximum color.
#' @param miCol color(character) for minimum value. Default is the light grey, "grey90".
#' @param myCRS optional coordinate arguments to populate coord_sf() for when 'surface' is a data.frame.
#' @param pngSaveFileName optional (character) file name to which to save plot.
#' @param ... Aesthetic arguments to populate ggplot2::geom_tile(). Suggested use might include "x=x,y=y,fill=value" or similar, depending on the column names of the 'surface' parameter. Only relevant if 'surface' is a data.frame.
#'
#' @importFrom scales muted
#' @importFrom tidyterra geom_spatraster
#'
#' @return a ggplot object
#' @seealso surface2df
#'
#' @export
makePlot <- function(surface, season, title = NULL, maxCol = NULL, minColor = "grey90",myCRS = NULL, pngSaveFileName = NULL, ...) {

  if (tolower(season) %in% c("summer", "breeding", "warm", "s1", 1, "red")) {
    maxCol <- scales::muted("red")
  } else {
    maxCol <- scales::muted("blue")
  }

  if (!is.null(maxCol)) {
    maxCol <- maxCol
  }

  p <- ggplot()

  if (class(surface) == "data.frame") {
    p <- p + geom_tile(data = surface , aes(...))
  } else if (class(surface) == "SpatRaster") {
    p <- p + geom_spatraster(data = surface, mapping = aes())
  }
  if (!is.null(myCRS)) {
    p <- p + coord_sf(crs = myCRS)
  }
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }

   p <- p +
    scale_fill_gradient(
      name = "Probability of\nOccurrence",
      low = minColor,
      high = maxCol,
      na.value = NA
    ) +
    theme_minimal() +
    xlab(NULL) +
    ylab(NULL)+
    theme(
      axis.title = element_blank(),
      legend.position = "bottom"
    )



  if(!is.null(pngSaveFileName)){
    ggsave(plot = p,
           filename = pngSaveFileName,
           width = 8, height = 6, units = "in", dpi = 300)
  }

  return(p)

}

#' @example
#'
#' # Example using fortified raster (data frame format):
#' r <- loadRaster()[[1]]
#' df <- as.data.frame(r, xy = T)
#' names(df)[3] <- "value"
#' makePlot(surface = df, season = "summer", x = x, y=y, fill =value, myCRS = crs(r))
#'
#'
#' # Examples using spatRaster.
#' # Single species/season.
#' makePlot(surface = r, season = "breeding")
#'
#' # Multiple species + seasons.
#' library(patchwork)
#' Aeronautes_saxatalis_breeding      <- makePlot(
#'   surface = loadRaster("Aeronautes_saxatalis.tif")[[1]],
#'   title = "Aeronautes saxatalis: breeding season",
#'   season = "breeding"
#' )
#' Aeronautes_saxatalis_nonbreeding   <- makePlot(
#'   surface = loadRaster("Aeronautes_saxatalis.tif")[[2]],
#'   title = "Aeronautes saxatalis: nonbreeding season",
#'   season = "nonbreeding"
#' )
#' Dumetella_carolinensis_breeding    <- makePlot(
#'   surface = loadRaster("Dumetella_carolinensis.tif")[[1]],
#'   title = "Dumetella carolinensis: breeding season",
#'   season = "breeding"
#' )
#' Dumetella_carolinensis_nonbreeding <- makePlot(
#'   surface = loadRaster("Dumetella_carolinensis.tif")[[2]],
#'   title = "Dumetella carolinensis: nonbreeding season",
#'   season = "nonbreeding"
#' )
#' { Aeronautes_saxatalis_breeding | Aeronautes_saxatalis_nonbreeding } /
#'   { Dumetella_carolinensis_breeding | Dumetella_carolinensis_nonbreeding } +
#'   plot_layout(guides = "collect")
#'
#'

