rm(list = ls())
gc()

library(SpaDES.core)
library(SpaDES.project)
library(terra)
library(sf)

## -----------------------------------------------------
## PATHS
## -----------------------------------------------------
root <- "E:/RiparianBuffers_test"

dir.create(file.path(root, "modules"),  recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(root, "inputs"),   recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(root, "outputs"),  recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(root, "cache"),    recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(root, "scratch"),  recursive = TRUE, showWarnings = FALSE)

setPaths(
  modulePath  = file.path(root, "modules"),
  inputPath   = file.path(root, "inputs"),
  outputPath  = file.path(root, "outputs"),
  cachePath   = file.path(root, "cache"),
  scratchPath = file.path(root, "scratch")
)
getPaths()

## -----------------------------------------------------
## DOWNLOAD MODULE
## -----------------------------------------------------
getModule(
  "shirinvark/RiparianBuffers",
  modulePath = getPaths()$modulePath,
  overwrite  = TRUE
)


## -----------------------------------------------------
## DUMMY STUDY AREA
## -----------------------------------------------------
studyArea <- st_as_sf(
  st_sfc(
    st_polygon(list(matrix(
      c(0, 0,
        1000, 0,
        1000, 1000,
        0, 1000,
        0, 0),
      ncol = 2, byrow = TRUE
    )))
  ),
  crs = 3857
)

## -----------------------------------------------------
## PLANNING RASTER (250 m)
## -----------------------------------------------------
PlanningRaster <- rast(vect(studyArea), resolution = 250)
values(PlanningRaster) <- 1

## -----------------------------------------------------
## DUMMY STREAM (SpatVector)
## -----------------------------------------------------
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
  streams = streams
)

## -----------------------------------------------------
## DUMMY PROVINCES
## -----------------------------------------------------
Provinces_sf <- st_as_sf(
  data.frame(province_code = "ON"),
  geometry = st_as_sfc(st_bbox(studyArea)),
  crs = 3857
)
Provinces <- terra::vect(Provinces_sf)
class(Provinces)
# باید ببینی: "SpatVector"

plot(Provinces)

ext(Provinces)
ext(PlanningRaster)
plot(PlanningRaster, main = "PlanningRaster")
plot(Provinces, add = TRUE, border = "red", lwd = 2)
ط
## -----------------------------------------------------
## RIPARIAN POLICY (DUMMY BUT REQUIRED)
## -----------------------------------------------------
riparianPolicy <- data.frame(
  province_code = "ON",
  buffer_m = 150   # به‌جای 30
)


## -----------------------------------------------------
## RUN MODULE
## -----------------------------------------------------
sim <- simInit(
  times   = list(start = 0, end = 1),
  modules = "RiparianBuffers",
  objects = list(
    PlanningRaster = PlanningRaster,
    Hydrology      = Hydrology,
    Provinces      = Provinces
  ),
  params = list(
    RiparianBuffers = list(
      riparianPolicy = riparianPolicy,
      hydroRaster_m  = 100
    )
  )
)

sim <- spades(sim)

## -----------------------------------------------------
## CHECK OUTPUT
## -----------------------------------------------------
names(sim)
names(sim$Riparian)

summary(terra::values(sim$Riparian$riparianFraction))

plot(
  sim$Riparian$riparianFraction,
  main = "Riparian fraction (0–1)"
)
