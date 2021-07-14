#' Get occurrence data and examine change across seasons (or other time periods) at those points.
#'
#'
#' @rdname UsingOccurrenceData
#' @param occDatPath Directory location where occurrence data are stored (in subdirectories with species names).
#' @param species Character vector of species, name of subdirectory where occurrence records are stored.
#' @param season Character vector to name the new column indicating time period (defaults to 'season').
#'
#' @importFrom utils read.csv write.csv
#' @importFrom dplyr mutate case_when filter
#' @importFrom raster stack extract
#' @importFrom magrittr %>%
#' @importFrom sp SpatialPoints
#'
#' @return A dataframe containing occurence points chategorized into "Summer" and "Winter" months.
#'
#' @export
getOccDat <- function(species, occDatPath, season = season) {
  utils::read.csv(
    list.files( file.path( occDatPath, species ), pattern = "csv$", full.names = T)
  ) %>%
    dplyr::mutate(
      season = dplyr::case_when(
        month %in% 6:8 ~ "Summer",
        month %in% c(12, 1:2) ~ "Winter",
        TRUE ~ as.character(NA)
      )
    ) %>%
    dplyr::filter(!is.na(season))
}

#' @rdname UsingOccurrenceData
#'
#' @param rast1 First input rasterLayer
#' @param rast2 Second input rasterLayer
#' @param rastnames Character vector of layerNames to be compared (defaults to "Summer" and "Winter" for rast1 and rast2 respectively)
#' @param species Character vector of species, name of subdirectory where occurrence records are stored.
#' @param speciesOccPts Occurrence points used in the model. In this example, requires column 'season'. Output of getOccDat function fits here.
#' @param saveDir Directory in which to save output.
#'
#' @importFrom sp SpatialPoints
#'
#' @return A dataframe containing occurence points chategorized into "Summer" and "Winter" months.
#'
#' @export
getSeasonalChangeAtSampleSites <- function(rast1, rast2, rastnames = c("Summer", "Winter"),
                                           species, speciesOccPts, saveDir) {

  mystack <- raster::stack(rast1, rast2)
  names(mystack) <- rastnames

  pts <- sp::SpatialPoints( cbind( speciesOccPts$decimalLongitude, speciesOccPts$decimalLatitude ) )

  mdf <- data.frame(
    speciesOccPts[, c("species", "decimalLongitude", "decimalLatitude", "season")],
    raster::extract(mystack, pts)
  ) %>%
    dplyr::mutate(
      difference = dplyr::case_when(
        season == "Summer" ~ Summer - Winter,
        season == "Winter" ~ Winter - Summer
      )
    )

  write.csv(
    mdf,
    file = file.path(saveDir, species, paste0(paste(species, "changeAtSites", sep = "_"), ".csv")
    )
  )
  return(mdf)
}