## Compute riparian influence as a fractional raster.
##
## Two mutually exclusive modes are supported:
## 1) Uniform buffer distance applied everywhere (riparianBuffer_m)
## 2) Spatially variable buffer distances supplied as a raster (bufferRaster),
##    typically derived from jurisdiction-specific policy
## Core riparian influence engine
## Designed to be policy-agnostic and reusable
## across different regulatory or ecological contexts.
buildRiparianFraction <- function(
    PlanningGrid,
    streams,
    lakes = NULL,
    riparianBuffer_m = NULL,   # buffer  
    bufferRaster     = NULL,   # buffer 
    hydroRaster_m    = 25
) {
  ## Enforce a single buffering strategy:
  ## either uniform (riparianBuffer_m) OR
  ## spatially variable (bufferRaster), but never both
  # --- sanity check ---
  if (is.null(streams)) {
    stop("streams (Hydrology_streams) must be provided to buildRiparianFraction.")
  }
  
  if (!inherits(streams, "SpatVector")) {
    stop("Hydrology$streams must be a SpatVector.")
  }
  
  if (is.null(riparianBuffer_m) && is.null(bufferRaster)) {
    stop("Either riparianBuffer_m or bufferRaster must be provided.")
  }
  if (!is.null(bufferRaster) && !inherits(bufferRaster, "SpatRaster")) {
    stop("bufferRaster must be a SpatRaster.")
  }
  if (!is.null(riparianBuffer_m) && !is.null(bufferRaster)) {
    stop("Provide only one of riparianBuffer_m or bufferRaster, not both.")
  }
  
  # --- CRS consistency ---
  if (!terra::same.crs(streams, PlanningGrid)) {
    streams <- terra::project(streams, PlanningGrid)
  }
  if (!is.null(bufferRaster) && 
      !terra::same.crs(bufferRaster, PlanningGrid)) {
    bufferRaster <- terra::project(bufferRaster, PlanningGrid)
  }
  # high-resolution template (shared)
  hydro_template <- terra::rast(
    ext = terra::ext(PlanningGrid),
    resolution = hydroRaster_m,
    crs = terra::crs(PlanningGrid)
  )
  terra::values(hydro_template) <- NA_real_
  
  
  
  ## Internal high-resolution raster used to compute
  ## proportional riparian influence.
  ##
  ## Resolution may differ from PlanningGrid to
  ## better capture narrow hydrological features.
  ## Performance note:
  ## hydroRaster_m controls the trade-off between
  ## spatial accuracy and computational cost.
  ## This is intentionally decoupled from PlanningGrid
  # =========================================================
  # CASE 1: UNIFORM BUFFER 
  # ===================================================
  ## Uniform buffer case:
  ## applies a single buffer distance to all streams.
  ## This preserves legacy behaviour and provides
  ## a simple baseline for testing and comparison.
  
  if (!is.null(riparianBuffer_m)) {
    
    streams_buf <- terra::buffer(streams, width = riparianBuffer_m)
    
    rip_hi <- terra::rasterize(
      streams_buf,
      hydro_template,
      cover = TRUE,
      background = 0
    )
    
    fact <- max(1, round(res(PlanningGrid)[1] / hydroRaster_m))
    
    riparian_fraction <- terra::aggregate(
      rip_hi,
      fact = fact,
      fun  = "mean",
      na.rm = TRUE
    )
    
    riparian_fraction <- terra::resample(
      riparian_fraction,
      PlanningGrid,
      method = "near"
    )
    
    riparian_fraction[is.na(riparian_fraction)] <- 0
    
    return(riparian_fraction)
  }
  
  #Case 2 =========================================================
  # aligned high-resolution template
  
  # CASE 2 =========================================================
  ## ---- FIX terra::ifel NA bug ----
  # hydro_r <- terra::rasterize(
  #  streams,
  #  hydro_template,
  # field = 1,
  #  background = NA
  #)
  
  hydro_r <- terra::rasterize(
    terra::buffer(streams, width = 100),
    hydro_template,
    field = 1,
    background = NA
  )
  
  
  if (!is.null(lakes)) {
    if (!terra::same.crs(lakes, PlanningGrid)) {
      lakes <- terra::project(lakes, PlanningGrid)
    }
    
    lakes_r <- terra::rasterize(
      lakes,
      hydro_template,
      field = 1,
      background = NA
    )
    
    hydro_r <- terra::cover(hydro_r, lakes_r)
  }
  dist_r <- terra::distance(hydro_r)
  message(
    "DEBUG dist_r non-NA = ",
    terra::global(
      !is.na(dist_r),
      "sum",
      na.rm = TRUE
    )[1,1]
  )
  
  message(
    "DEBUG bufferRaster non-NA = ",
    terra::global(
      !is.na(bufferRaster),
      "sum",
      na.rm = TRUE
    )[1,1]
  )
  max_dist <- terra::global(bufferRaster, "max", na.rm = TRUE)[1,1]
  
  if (!is.finite(max_dist)) {
    stop("bufferRaster contains no valid buffer distances.")
  }
  
  dist_r[is.na(bufferRaster)] <- NA
  dist_r[dist_r > max_dist] <- NA
  
  ## --- CHECK alignment ---
  stopifnot(
    terra::compareGeom(dist_r, bufferRaster, stopOnError = FALSE)
  )
  
  ## ---- SAFE riparian mask (NO ifel) ----
  
  cond <- dist_r <= bufferRaster
  message(
    "DEBUG cond TRUE cells = ",
    terra::global(
      cond,
      "sum",
      na.rm = TRUE
    )[1,1]
  )
  # هر NA → FALSE
  cond[is.na(cond)] <- FALSE
  
  # logical → numeric {0,1}
  rip_hi <- cond * 1
  print(
    terra::freq(rip_hi)
  )
  fact <- max(1, round(res(PlanningGrid)[1] / hydroRaster_m))
  #fact <- 1
  riparian_fraction <- terra::aggregate(
    rip_hi,
    fact = fact,
    fun  = "mean",
    na.rm = TRUE
  )
  print(
    terra::freq(riparian_fraction)
  )
  print(
    terra::global(
      riparian_fraction,
      "max",
      na.rm = TRUE
    )
  )
  
  print(
    terra::global(
      riparian_fraction,
      "min",
      na.rm = TRUE
    )
  )
  global(riparian_fraction, "max", na.rm = TRUE)
  
  global(riparian_fraction, "min", na.rm = TRUE)
  #riparian_fraction <- terra::resample(
  # riparian_fraction,
  #PlanningGrid,
  #method = "near"
  # )
  global(riparian_fraction, "max", na.rm = TRUE)
  
  global(riparian_fraction, "min", na.rm = TRUE)
  print(
    terra::freq(riparian_fraction)
  )
  
  riparian_fraction[is.na(riparian_fraction)] <- 0
  
  print(
    terra::freq(riparian_fraction)
  )
  
  print(
    terra::global(
      riparian_fraction,
      "max",
      na.rm = TRUE
    )
  )
  return(riparian_fraction)
}