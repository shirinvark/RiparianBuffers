rm(list = ls())
gc()

library(terra)
library(sf)

buildRiparianFraction <- function(
    PlanningRaster,
    streams,
    riparianBuffer_m = 30,
    hydroRaster_m    = 100
) {
  
  if (!terra::same.crs(streams, PlanningRaster)) {
    streams <- terra::project(streams, PlanningRaster)
  }
  
  streams_buf <- terra::buffer(streams, width = riparianBuffer_m)
  
  hydro_template <- terra::rast(
    terra::ext(PlanningRaster),
    resolution = hydroRaster_m,
    crs = terra::crs(PlanningRaster)
  )
  
  terra::values(hydro_template) <- 0
  
  riparian_fraction <- terra::rasterize(
    streams_buf,
    hydro_template,
    cover = TRUE,
    background = 0
  )
  
  return(riparian_fraction)
}
## STUDY AREA
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

## PLANNING RASTER (250 m)
PlanningRaster <- rast(vect(studyArea), resolution = 250)
values(PlanningRaster) <- 1

## STREAM (vertical line)
streams <- vect(
  st_as_sf(
    st_sfc(
      st_linestring(matrix(
        c(100, 0,
          100, 1000),
        ncol = 2, byrow = TRUE
      ))
    ),
    crs = 3857
  )
)

## RUN
riparian <- buildRiparianFraction(
  PlanningRaster   = PlanningRaster,
  streams          = streams,
  riparianBuffer_m = 30,
  hydroRaster_m    = 100
)

plot(riparian, main = "Riparian fraction (proportional)")

