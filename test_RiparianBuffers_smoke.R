############################################################
## Smoke test for RiparianBuffers module (with HydroLAKES)
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
rivers_zip <- "D:/HydroRIVERS_v10_na_shp (3).zip"
rivers_dir <- "D:/HydroRIVERS"

if (!dir.exists(rivers_dir)) {
  unzip(rivers_zip, exdir = rivers_dir)
}

river_shp <- list.files(
  rivers_dir,
  pattern = "\\.shp$",
  recursive = TRUE,
  full.names = TRUE
)

stopifnot(length(river_shp) == 1)

streams_all <- terra::vect(river_shp)
streams_all <- terra::project(streams_all, "EPSG:3857")

stopifnot(nrow(streams_all) > 0)

## ---------------------------------------------------------
## 3b. Load HydroLAKES (REAL data)
## ---------------------------------------------------------
lakes_zip <- "D:/HydroLAKES_polys_v10_shp.zip"
lakes_dir <- "D:/HydroLAKES"

if (!dir.exists(lakes_dir)) {
  unzip(lakes_zip, exdir = lakes_dir)
}

lake_shp <- list.files(
  lakes_dir,
  pattern = "\\.shp$",
  recursive = TRUE,
  full.names = TRUE
)

stopifnot(length(lake_shp) == 1)

lakes_all <- terra::vect(lake_shp)
lakes_all <- terra::project(lakes_all, "EPSG:3857")

stopifnot(nrow(lakes_all) > 0)

## ---------------------------------------------------------
## 4. Small study area (from first rivers)
## ---------------------------------------------------------
studyArea <- terra::ext(streams_all[1:50, ]) |>
  terra::as.polygons(crs = "EPSG:3857") |>
  sf::st_as_sf()

studyArea_v <- terra::vect(studyArea)

## ---------------------------------------------------------
## 5. Planning raster (coarse)
## ---------------------------------------------------------
PlanningRaster <- terra::rast(
  studyArea_v,
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
## 7. Streams & Lakes (cropped for smoke test only)
## ---------------------------------------------------------
streams <- terra::crop(streams_all, studyArea_v)
stopifnot(nrow(streams) > 0)

lakes <- terra::crop(lakes_all, studyArea_v)

## if no lakes in this small area, create a dummy lake
if (nrow(lakes) == 0) {
  message("No HydroLAKES in study area; creating dummy lake for smoke test.")
  lakes <- terra::buffer(streams[1], width = 300)
  lakes <- terra::as.polygons(lakes)
}

## ---------------------------------------------------------
## 8. simInit
## ---------------------------------------------------------
sim <- simInit(
  times   = list(start = 0, end = 1),
  modules = "RiparianBuffers",
  objects = list(
    PlanningRaster    = PlanningRaster,
    Provinces         = Provinces,
    Hydrology_streams = streams,
    Hydrology_lakes   = lakes
  ),
  params = list(
    RiparianBuffers = list(
      hydroRaster_m = 30
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
sim$Riparian

x11()
plot(
  sim$Riparian$riparianFraction,
  main = "Riparian fraction (0–1)",
  col = hcl.colors(20, "YlGnBu")
)
plot(lakes, add = TRUE, border = "red", lwd = 2)

hist(
  values(sim$Riparian$riparianFraction),
  breaks = 50,
  main = "Distribution of riparian fraction",
  xlab = "Riparian fraction"
)

x <- values(sim$Riparian$riparianFraction)
hist(
  x[x > 0],
  breaks = 30,
  main = "Riparian fraction (non-zero only)",
  xlab = "fraction"
)

mean(sim$Riparian$riparianFraction[] > 0, na.rm = TRUE)
