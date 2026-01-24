############################################################
## Smoke test for RiparianBuffers module
## Purpose:
##  - Test fallback riparian policy (30 m)
##  - Trigger warning when a province_code is missing in policy
##  - Verify fractional (0–1) riparian output
############################################################

rm(list = ls())
gc()

library(SpaDES.core)
library(SpaDES.project)
library(terra)
library(sf)

## ---------------------------------------------------------
## 1. Paths
## ---------------------------------------------------------
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

## ---------------------------------------------------------
## 2. Download RiparianBuffers module
## ---------------------------------------------------------
getModule(
  "shirinvark/RiparianBuffers",
  modulePath = getPaths()$modulePath,
  overwrite  = TRUE
)

## ---------------------------------------------------------
## 3. Dummy study area and planning raster (250 m)
## ---------------------------------------------------------
studyArea <- st_as_sf(
  st_sfc(
    st_polygon(list(matrix(
      c(0,0, 1000,0, 1000,1000, 0,1000, 0,0),
      ncol = 2, byrow = TRUE
    )))
  ),
  crs = 3857
)

PlanningRaster <- rast(vect(studyArea), resolution = 250)
values(PlanningRaster) <- 1

## ---------------------------------------------------------
## 4. Provinces (one intentionally missing from policy)
## ---------------------------------------------------------
## ON has a policy, XX does not -> should trigger warning
Provinces <- vect(
  st_as_sf(
    data.frame(
      province_code = c("ON", "XX"),
      id = 1:2
    ),
    geometry = st_sfc(
      st_polygon(list(matrix(
        c(0,0, 500,0, 500,1000, 0,1000, 0,0),
        ncol = 2, byrow = TRUE
      ))),
      st_polygon(list(matrix(
        c(500,0, 1000,0, 1000,1000, 500,1000, 500,0),
        ncol = 2, byrow = TRUE
      )))
    ),
    crs = 3857
  )
)

## ---------------------------------------------------------
## 5. Dummy hydrology (single vertical stream)
## ---------------------------------------------------------
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

## ---------------------------------------------------------
## 6. Initialize and run RiparianBuffers
##    (no riparianPolicy provided -> fallback should be used)
## ---------------------------------------------------------
sim <- simInit(
  times   = list(start = 0, end = 1),
  modules = "RiparianBuffers",
  objects = list(
    PlanningRaster = PlanningRaster,
    Provinces      = Provinces,
    Hydrology      = Hydrology
  ),
  params = list(
    RiparianBuffers = list(
      hydroRaster_m = 100
    )
  )
)

## EXPECTED:
## - Warning about missing province_code (XX)
## - Default 30 m buffer applied elsewhere
sim <- spades(sim)

## ---------------------------------------------------------
## 7. Check outputs
## ---------------------------------------------------------
names(sim$Riparian)
# Expected:
# "riparianFraction" "raster_m" "policy"

summary(values(sim$Riparian$riparianFraction))
# Expected:
# Values between 0 and 1 (fractional)

plot(
  sim$Riparian$riparianFraction,
  main = "Riparian fraction (fallback policy, 30 m)"
)

############################################################
## END OF TEST
############################################################
