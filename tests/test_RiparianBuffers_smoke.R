library(testthat)
library(terra)

test_that("RiparianBuffers produces valid riparian fraction raster", {
  
  expect_true(exists("sim"))
  
  expect_true("Riparian" %in% names(sim))
  
  expect_true(inherits(sim$Riparian$riparianFraction, "SpatRaster"))
  
  vals <- terra::values(sim$Riparian$riparianFraction)
  vals <- vals[!is.na(vals)]
  
  expect_true(length(vals) > 0)
  expect_true(all(vals >= 0))
  expect_true(all(vals <= 1))
  
})
