## code to prepare `DATASET` dataset goes here

library(tidyverse)
library(sf)
library(terra)
library(data.table)
myCRS <- "+proj=laea +lon_0=-98.44 +lat_0=16.72 +datum=WGS84 +units=m +no_defs"

locationGeoshiftPerformanceTest <- "../geoshift_performanceTest"

# Occurrence records. ---------
Aeronautes_saxatalis_breeding_n500      <- read.csv(file.path(locationGeoshiftPerformanceTest, "data/occs_randomSubset/Aeronautes_saxatalis_breeding_n500.csv"))
Aeronautes_saxatalis_nonbreeding_n500   <- read.csv(file.path(locationGeoshiftPerformanceTest, "data/occs_randomSubset/Aeronautes_saxatalis_nonbreeding_n500.csv"))
Dumetella_carolinensis_breeding_n500    <- read.csv(file.path(locationGeoshiftPerformanceTest, "data/occs_randomSubset/Dumetella_carolinensis_breeding_n500.csv"))
Dumetella_carolinensis_nonbreeding_n500 <- read.csv(file.path(locationGeoshiftPerformanceTest, "data/occs_randomSubset/Dumetella_carolinensis_nonbreeding_n500.csv"))

set.seed(42)
example_points <-
  list(
    Aeronautes_saxatalis_breeding_n500,
    Aeronautes_saxatalis_nonbreeding_n500,
    Dumetella_carolinensis_breeding_n500,
    Dumetella_carolinensis_nonbreeding_n500) %>%
  rbindlist(use.names = T) %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = myCRS, remove = T) %>%
  dplyr::select(
    species, season
  ) %>%
  # Obscure exact coordinates.
  st_jitter(amount = 1e4)

ggplot() +
  geom_sf(data = example_points, mapping = aes(color = season)) +
  facet_wrap(~species)

usethis::use_data(example_points, overwrite = TRUE)


# Raster data. -------
# file.path(locationGeoshiftPerformanceTest, "out/SDMOutputDir")

myRastPaths <- paste0(c(
  rep("Aeronautes_saxatalis_", 4),
  rep("Dumetella_carolinensis_", 4)
), rep(paste0(
  c("breeding", "nonbreeding"), c(
    rep("_expertThresh_n500.tif", 2),
    rep("_expertThresh_n500_PA.tif", 2)
  )
), 2)) %>%
  file.path(locationGeoshiftPerformanceTest, "out/SDMOutputDir", .)

r1 <- myRastPaths[1:4] %>% rast() %>% aggregate(5, fun = "max")
names(r1) <- c(
  "Aeronautes_saxatalis_breeding",
  "Aeronautes_saxatalis_nonbreeding",
  "Aeronautes_saxatalis_breeding_PA",
  "Aeronautes_saxatalis_nonbreeding_PA"
)
writeRaster(r1, "inst/extdata/Aeronautes_saxatalis.tif", overwrite = T)

r2 <- myRastPaths[5:8] %>% rast() %>% aggregate(5, fun = "max")
names(r2) <- c(
  "Dumetella_carolinensis_breeding",
  "Dumetella_carolinensis_nonbreeding",
  "Dumetella_carolinensis_breeding_PA",
  "Dumetella_carolinensis_nonbreeding_PA"
)
writeRaster(r2, "inst/extdata/Dumetella_carolinensis.tif", overwrite = T)
