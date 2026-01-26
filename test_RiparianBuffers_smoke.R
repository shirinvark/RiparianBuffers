############################################################
## Smoke test for RiparianBuffers module (FINAL)
############################################################

.rs.restartR()

rm(list = ls())
gc()

library(SpaDES.core)
library(SpaDES.project)
library(terra)
library(sf)

## ---------------------------------------------------------
## 1. Paths
## ---------------------------------------------------------
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

## ---------------------------------------------------------
## 2. Get module
## ---------------------------------------------------------
getModule(
  "shirinvark/RiparianBuffers",
  modulePath = getPaths()$modulePath,
  overwrite  = TRUE
)

## ---------------------------------------------------------
## 3. Load HydroRIVERS (REAL data)
## ---------------------------------------------------------
zip_path  <- "D:/HydroRIVERS_v10_na_shp (3).zip"
hydro_dir <- "D:/HydroRIVERS"

if (!dir.exists(hydro_dir)) {
  unzip(zip_path, exdir = hydro_dir)
}

hydro_shp <- file.path(
  hydro_dir,
  "HydroRIVERS_v10_na_shp",
  "HydroRIVERS_v10_na.shp"
)

streams_all <- terra::vect(hydro_shp)
streams_all <- terra::project(streams_all, "EPSG:3857")

stopifnot(nrow(streams_all) > 0)

## ---------------------------------------------------------
## 4. Small study area (from first rivers)
## ---------------------------------------------------------
studyArea <- terra::ext(streams_all[1:50, ]) |>
  terra::as.polygons(crs = "EPSG:3857") |>
  sf::st_as_sf()

## ---------------------------------------------------------
## 5. Planning raster (coarse)
## ---------------------------------------------------------
PlanningRaster <- terra::rast(
  terra::vect(studyArea),
  resolution = 250,
  crs = "EPSG:3857"
)
terra::values(PlanningRaster) <- 1

## ---------------------------------------------------------
## 6. Provinces (dummy but valid)
## ---------------------------------------------------------
Provinces <- terra::vect(studyArea)
Provinces$province_code <- "ON"

## ---------------------------------------------------------
## 7. Streams (cropped)
## ---------------------------------------------------------
streams <- terra::crop(streams_all, terra::vect(studyArea))
stopifnot(nrow(streams) > 0)

## ---------------------------------------------------------
## 8. simInit
## ---------------------------------------------------------
sim <- simInit(
  times   = list(start = 0, end = 1),
  modules = "RiparianBuffers",
  objects = list(
    PlanningRaster    = PlanningRaster,
    Provinces         = Provinces,
    Hydrology_streams = streams
  ),
  params = list(
    RiparianBuffers = list(
      hydroRaster_m = 100
    )
  ),
  options = list(
    spades.checkpoint = FALSE,
    spades.progress   = FALSE,
    spades.save       = FALSE
  )
)

ls(sim)

## ---------------------------------------------------------
## 9. Run
## ---------------------------------------------------------
sim <- spades(sim)

## ---------------------------------------------------------
## 10. Checks
## ---------------------------------------------------------
stopifnot(inherits(sim$RiparianFraction, "SpatRaster"))
stopifnot(is.list(sim$RiparianMeta))

summary(terra::values(sim$RiparianFraction))

plot(
  sim$RiparianFraction,
  main = "Riparian Fraction (Smoke Test)"
)

terra::global(
  sim$RiparianFraction,
  mean,
  na.rm = TRUE
)

hist(
  terra::values(sim$RiparianFraction),
  main = "Riparian fraction distribution",
  xlab = "fraction"
)

message("✅ RiparianBuffers smoke test PASSED")
############################################################
