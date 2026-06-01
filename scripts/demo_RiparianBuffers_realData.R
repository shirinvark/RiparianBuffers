############################################################
## Minimal Smoke Test for RiparianBuffers (REAL FMU)
############################################################

rm(list = ls())
gc()

library(SpaDES.core)
library(SpaDES.project)
library(terra)
library(sf)

## ---------------------------------------------------------
## 1. Portable Paths
## ---------------------------------------------------------
root <- file.path(tempdir(), "RiparianBuffers")

dir.create(root, recursive = TRUE, showWarnings = FALSE)

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
## 3. Read REAL study area
## ---------------------------------------------------------
studyArea <- sf::st_read(
  "D:/BOUNDARIES/Sudbury_FMU_5070.shp",
  quiet = TRUE
)

studyArea <- sf::st_make_valid(studyArea)

studyArea <- sf::st_union(studyArea)

studyArea <- sf::st_sf(
  id = 1,
  geometry = studyArea
)

## ---------------------------------------------------------
## 4. CRS check
## ---------------------------------------------------------
studyArea <- sf::st_transform(
  studyArea,
  5070
)

studyArea <- terra::vect(studyArea)

## ---------------------------------------------------------
## 5. Create sim
## ---------------------------------------------------------
sim <- simInit(
  
  times = list(
    start = 0,
    end   = 1
  ),
  
  params = list(
    RiparianBuffers = list()
  ),
  
  modules = "RiparianBuffers",
  
  objects = list(
    studyArea = studyArea
  )
)

## ---------------------------------------------------------
## 6. Run
## ---------------------------------------------------------
sim <- spades(sim)

## ---------------------------------------------------------
## 7. Quick checks
## ---------------------------------------------------------
print(names(sim$Riparian))

summary(
  values(sim$Riparian$riparianFraction)
)

mean(
  sim$Riparian$riparianFraction[] > 0,
  na.rm = TRUE
)

nrow(sim$Hydrology_streams)

nrow(sim$Hydrology_lakes)

plot(
  sim$Riparian$riparianFraction,
  main = "Riparian fraction (0-1)"
)

plot(
  sim$Hydrology_streams,
  add = TRUE,
  col = "blue"
)

plot(
  sim$Hydrology_lakes,
  add = TRUE,
  col = "cyan"
)
