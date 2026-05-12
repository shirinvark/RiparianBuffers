############################################################
## Minimal Smoke Test for RiparianBuffers (TRUE STANDALONE)
############################################################

rm(list = ls())
gc()

library(SpaDES.core)
library(SpaDES.project)
library(terra)
library(sf)

## ---------------------------------------------------------
## 1. Portable Paths (NO hardcoded drive letters)
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
## 2. Get module from GitHub
## ---------------------------------------------------------
getModule(
  "shirinvark/RiparianBuffers",
  modulePath = getPaths()$modulePath,
  overwrite  = TRUE
)

## ---------------------------------------------------------
## 3. Create SIMPLE artificial study area (NO external shapefile)
## ---------------------------------------------------------
studyArea_v <- terra::vect(
  sf::st_as_sf(
    sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(0,0,
          0,10000,
          10000,10000,
          10000,0,
          0,0),
        ncol = 2,
        byrow = TRUE
      ))),
      crs = 5070
    )
  )
)

## ---------------------------------------------------------
## 4. simInit (NO PlanningGrid supplied)
## ---------------------------------------------------------
sim <- simInit(
  times   = list(start = 0, end = 1),
  modules = "RiparianBuffers",
  objects = list(
    studyArea = studyArea_v
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

## ---------------------------------------------------------
## 5. Run
## ---------------------------------------------------------
sim <- spades(sim)

## ---------------------------------------------------------
## 6. Quick checks
## ---------------------------------------------------------
print(names(sim$Riparian))

summary(values(sim$Riparian$riparianFraction))

mean(sim$Riparian$riparianFraction[] > 0, na.rm = TRUE)

plot(sim$Riparian$riparianFraction,
     main = "Riparian fraction (0–1)")