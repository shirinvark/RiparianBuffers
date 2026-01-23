library(testthat)
library(SpaDES.core)
library(terra)
library(sf)

test_that("RiparianBuffers produces riparian raster", {
  
  ## 1. Dummy study area
  studyArea <- st_as_sf(
    st_sfc(
      st_polygon(list(matrix(
        c(0,0, 1000,0, 1000,1000, 0,1000, 0,0),
        ncol = 2, byrow = TRUE
      )))
    ),
    crs = 3857
  )
  
  ## 2. Planning raster
  PlanningRaster <- rast(vect(studyArea), resolution = 250)
  values(PlanningRaster) <- 1
  
  ## 3. Dummy stream
  streams <- vect(
    st_as_sf(
      st_sfc(
        st_linestring(matrix(
          c(500, 0,
            500, 1000),
          ncol = 2, byrow = TRUE
        ))
      ),
      crs = 3857
    )
  )
  
  Hydrology <- list(streams = streams)
  
  ## 4. Run module
  sim <- simInit(
    times   = list(start = 0, end = 1),
    modules = "RiparianBuffers",
    objects = list(
      PlanningRaster = PlanningRaster,
      Hydrology      = Hydrology
    ),
    params = list(
      RiparianBuffers= list(
        riparianBuffer_m = 30,
        hydroRaster_m    = 100
      )
    )
  )
  
  sim <- spades(sim)
  
  ## 5. Expectations
  expect_true("Riparian" %in% names(sim))
  expect_true("riparianFraction" %in% names(sim$Riparian))
  expect_s4_class(sim$Riparian$riparianFraction, "SpatRaster")
  
  vals <- terra::values(sim$Riparian$riparianFraction)
  expect_true(any(vals > 0, na.rm = TRUE))
})
