rm(list = ls())
gc()

library(terra)
library(sf)
library(SpaDES.core)
studyArea <- st_as_sf(
  st_sfc(
    st_polygon(list(matrix(
      c(0,0,
        1000,0,
        1000,1000,
        0,1000,
        0,0),
      ncol = 2, byrow = TRUE
    )))
  ),
  crs = 3857
)
PlanningRaster <- rast(vect(studyArea), resolution = 250)
values(PlanningRaster) <- 1
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

Hydrology <- list(
  source  = "manual_test",
  streams = streams
)
sim <- simInit(
  times   = list(start = 0, end = 1),
  modules = "EasternCanadaHydrology",
  objects = list(
    PlanningRaster = PlanningRaster,
    Hydrology      = Hydrology,
    studyArea      = studyArea
  ),
  params = list(
    EasternCanadaHydrology = list(
      riparianBuffer_m = 30,
      hydroRaster_m    = 100
    )
  )
)
sim <- spades(sim)
