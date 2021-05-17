#' Get occurrence data and examine change across seasons (or other time periods) at those points.
#'
#'
#' @rdname UsingOccurrenceData
#' @param occDatPath Directory location where occurrence data are stored (in subdirectories with species names).
#' @param species Character vector of species, name of subdirectory where occurrence records are stored.
#'
#' @return A dataframe containing occurence points chategorized into "Summer" and "Winter" months.
#'
#' @export
getOccDat <- function(species, occDatPath) {
  read.csv(
    list.files( file.path( occDatPath, species ), pattern = "csv$", full.names = T)
  ) %>%
    dplyr::mutate(
      season = case_when(
        month %in% 6:8 ~ "Summer",
        month %in% c(12, 1:2) ~ "Winter",
        TRUE ~ as.character(NA)
      )
    ) %>%
    filter(!is.na(season))
}

#' @rdname UsingOccurrenceData
#' @param occDatPath Directory location where occurrence data are stored (in subdirectories with species names).
#' @param species Character vector of species, name of subdirectory where occurrence records are stored.
#' @param speciesOccPts Occurrence points used in the model. In this example, requires column 'season'. Output of getOccDat function fits here.
#'
#' @return A dataframe containing occurence points chategorized into "Summer" and "Winter" months.
#'
#' @export
getSeasonalChangeAtSampleSites <- function(rast1, rast2, rastnames = c("Summer", "Winter"),
                                           species, speciesOccPts) {

  mystack <-  stack(rast1, rast2)
  names(mystack) <- rastnames

  pts <- SpatialPoints( cbind( speciesOccPts$decimalLongitude, speciesOccPts$decimalLatitude ) )

  mdf <- data.frame(
    speciesOccPts[, c("species", "decimalLongitude", "decimalLatitude", "season")],
    raster::extract(mystack, pts)
  ) %>%
    dplyr::mutate(
      difference = case_when(
        season == "Summer" ~ Summer - Winter,
        season == "Winter" ~ Winter - Summer
      )
    )

  write.csv(
    mdf,
    file = file.path(
      wd$out, species, paste0(paste(species, "changeAtSites", sep = "_"), ".csv")
    )
  )
  return(mdf)
}
