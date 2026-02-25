############################################################
## Minimal Smoke Test for RiparianBuffers (NEW ARCHITECTURE)
############################################################

#.rs.restartR()

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
## 3. Create SMALL study area (simple square)
## ---------------------------------------------------------
library(sf)
library(terra)

# Read real FMU boundary
studyArea_sf <- sf::st_read(
  "E:/EasternCanadaDataPrep/BOUNDARIES/Sudbury_FMU_5070.shp",
  quiet = TRUE
)

studyArea_sf <- sf::st_make_valid(studyArea_sf)

# اطمینان از CRS
sf::st_crs(studyArea_sf)

# تبدیل به terra SpatVector (پیشنهادی برای هماهنگی کامل با ماژول)
studyArea_v <- terra::vect(studyArea_sf)

# چک نهایی
terra::crs(studyArea_v)

## ---------------------------------------------------------
## 4. Create PlanningGrid_250m
## ---------------------------------------------------------
PlanningGrid_250m <- terra::rast(
  studyArea_v,
  resolution = 250,
  crs = terra::crs(studyArea_v)
)

terra::values(PlanningGrid_250m) <- 1

## ---------------------------------------------------------
## 5. simInit
## ---------------------------------------------------------
sim <- simInit(
  times   = list(start = 0, end = 1),
  modules = "RiparianBuffers",
  objects = list(
    studyArea         = studyArea_v,
    PlanningGrid_250m = PlanningGrid_250m
  ),
  params = list(
    RiparianBuffers = list(
      hydroRaster_m = 30
    )
  ),
  options = list(
    spades.checkpoint = FALSE,
    spades.progress   = TRUE,
    spades.save       = FALSE
  )
)
terra::crs(studyArea_v)
terra::crs(PlanningGrid_250m)
## ---------------------------------------------------------
## 6. Run
## ---------------------------------------------------------
sim <- spades(sim)

## ---------------------------------------------------------
## 7. Checks
## ---------------------------------------------------------
names(sim$Riparian)

summary(values(sim$Riparian$riparianFraction))

mean(sim$Riparian$riparianFraction[] > 0, na.rm = TRUE)

plot(
  sim$Riparian$riparianFraction,
  main = "Riparian fraction (0–1)"
)