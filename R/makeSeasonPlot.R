#' Make a seasonal plot, save to a specific path.
#'
#' @param surface array data presented in a 3-column dataframe
#' @param species species name (character) to include in title and file path
#' @param season season name (character) to specify plot color and file path. Defaults to muted red for "Summer" and muted blue for other seasons.
#' @param maxCol color (character) to overwrite season argument, sets maximum color.
#' @param coordArgs optional coordinate arguments to populate coord_sf.
#' @param savePath optional (character) file path to save plot.
#' @param ... Aesthetic arguments to populate ggplot2::geom_tile(). Suggested use might include "x=x,y=y,fill=value" or similar, depending on the column names of the 'surface' parameter.
#'
#' @importFrom scales muted
#'
#' @return a ggplot object
#' @seealso surface2df
#'
#' @export
makePlot <- function(surface, species, season, maxCol = NULL, coordArgs = NULL, savePath = NULL, ...) {

  if(tolower(season) == "summer") {
    maxCol <- scales::muted("red")
  } else {
    maxCol <- scales::muted("blue")
  }

  if(!is.null(maxCol)) {
    maxCol <- maxCol
  }

  p <- ggplot() +
    geom_tile(data = surface , aes(...)) +
    scale_fill_gradient(
      name = "Probability of\nOccurrence",
      low = "grey90",
      high = maxCol
    ) +
    coord_sf(coordArgs) +
    theme_minimal() +
    xlab(NULL) + ylab(NULL)+
    theme(
      axis.title = element_blank(),
      legend.position = "bottom"
    )

  if(!is.null(savePath)){
    ggsave(plot = p,
           filename = file.path(
             savePath, species,
             paste0( paste(species, "SDM", season, sep = "_"), ".png" )
           ),
           width = 8, height = 6, units = "in", dpi = 300)
  }

  return(p)

}

#' @example
#' # makePlot(mySurface, species = "LANO", season = "summer", x=x,y=y,fill=value )
#' # Additional aes arguments supplied: "x=x,y=y,fill=value"
