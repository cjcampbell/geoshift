
testthat::test_that("errors", {
  testthat::expect_error(
    areaOfExpectedOccurrence(raster::raster(matrix(data = 2, nrow = 3, ncol = 3))),
    "surface must be a binary surface of 0's, 1's, and NA's."
  )
})
