rm(list = ls())
gc()

library(SpaDES.core)
library(SpaDES.project)

## -----------------------------------------------------
## PATHS
## -----------------------------------------------------
root <- "E:/RiparianBuffers"

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
## DOWNLOAD MODULE  ✅ (THIS IS THE FIX)
## -----------------------------------------------------
getModule(
  "shirinvark/RiparianBuffers",
  modulePath = getPaths()$modulePath,
  overwrite  = TRUE
)

## sanity check
list.files(getPaths()$modulePath)
library(SpaDES.core)
library(terra)
library(sf)

## -----------------------------------------------------
## DUMMY STUDY AREA
## -----------------------------------------------------
studyArea <- st_as_sf(
  st_sfc(
    st_polygon(list(matrix(
      c(0,0, 1000,0, 1000,1000, 0,1000, 0,0),
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
## DUMMY STREAM
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
## RUN MODULE
## -----------------------------------------------------
sim <- simInit(
  times   = list(start = 0, end = 1),
  modules = "RiparianBuffers",
  objects = list(
    PlanningRaster = PlanningRaster,
    Hydrology      = Hydrology
  ),
  params = list(
    RiparianBuffers = list(
      riparianBuffer_m = 30,
      hydroRaster_m    = 100
    )
  )
)

sim <- spades(sim)

## -----------------------------------------------------
## CHECK
## -----------------------------------------------------
names(sim)
names(sim$Riparian)

plot(sim$Riparian$riparianFraction, main = "Riparian fraction")
