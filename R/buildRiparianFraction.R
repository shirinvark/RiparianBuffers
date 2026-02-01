## Compute riparian influence as a fractional raster.
##
## Two mutually exclusive modes are supported:
## 1) Uniform buffer distance applied everywhere (riparianBuffer_m)
## 2) Spatially variable buffer distances supplied as a raster (bufferRaster),
##    typically derived from jurisdiction-specific policy.
## Core riparian influence engine.
## Designed to be policy-agnostic and reusable
## across different regulatory or ecological contexts.
buildRiparianFraction <- function(
    PlanningRaster,
    streams,
    lakes = NULL,
    riparianBuffer_m = NULL,   # buffer  
    bufferRaster     = NULL,   # buffer 
    hydroRaster_m    = 30
) {
  ## Enforce a single buffering strategy:
  ## either uniform (riparianBuffer_m) OR
  ## spatially variable (bufferRaster), but never both.
  # --- sanity check ---
  if (is.null(streams)) {
    stop("Hydrology_streams is missing. Supply streams upstream before RiparianBuffers. Run EasternCanadaDataPrep before RiparianBuffers
.")
  }
  
  if (!inherits(streams, "SpatVector")) {
    stop("Hydrology$streams must be a SpatVector.")
  }
  
  if (is.null(riparianBuffer_m) && is.null(bufferRaster)) {
    stop("Either riparianBuffer_m or bufferRaster must be provided.")
  }
  
  if (!is.null(riparianBuffer_m) && !is.null(bufferRaster)) {
    stop("Provide only one of riparianBuffer_m or bufferRaster, not both.")
  }
  
  # --- CRS consistency ---
  if (!terra::same.crs(streams, PlanningRaster)) {
    streams <- terra::project(streams, PlanningRaster)
  }
  # high-resolution template (shared)
  hydro_template <- terra::rast(
    ext = terra::ext(PlanningRaster),
    resolution = hydroRaster_m,
    crs = terra::crs(PlanningRaster)
  )
  terra::values(hydro_template) <- NA_real_
  
  
  
  ## Internal high-resolution raster used to compute
  ## proportional riparian influence.
  ##
  ## Resolution may differ from PlanningRaster to
  ## better capture narrow hydrological features.
  ## Performance note:
  ## hydroRaster_m controls the trade-off between
  ## spatial accuracy and computational cost.
  ## This is intentionally decoupled from PlanningRaster
  # =========================================================
  # CASE 1: UNIFORM BUFFER 
  # =========================================================
  ## Uniform buffer case:
  ## applies a single buffer distance to all streams.
  ## This preserves legacy behaviour and provides
  ## a simple baseline for testing and comparison.
  
  if (!is.null(riparianBuffer_m)) {
    
    streams_buf <- terra::buffer(streams, width = riparianBuffer_m)
    
    riparian_fraction <- terra::rasterize(
      streams_buf,
      hydro_template,
      cover = TRUE,
      background = 0
    )
    
    return(riparian_fraction)
  }
  
  #Case 2 =========================================================
  # aligned high-resolution template
  
  # CASE 2 =========================================================
  ## ---- FIX terra::ifel NA bug ----
  hydro_r <- terra::rasterize(
    streams,
    hydro_template,
    field = 1,
    background = NA
  )
  
  if (!is.null(lakes)) {
    if (!terra::same.crs(lakes, PlanningRaster)) {
      lakes <- terra::project(lakes, PlanningRaster)
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
  
  max_dist <- max(values(bufferRaster), na.rm = TRUE)
  dist_r[dist_r > max_dist] <- NA
  
  ## --- CHECK alignment ---
  stopifnot(
    terra::ext(dist_r) == terra::ext(bufferRaster),
    all(terra::res(dist_r) == terra::res(bufferRaster))
  )
  
  ## ---- SAFE riparian mask (NO ifel) ----
  
  cond <- dist_r <= bufferRaster
  
  # هر NA → FALSE
  cond[is.na(cond)] <- FALSE
  
  # logical → numeric {0,1}
  rip_hi <- cond * 1
  
  fact <- round(res(PlanningRaster)[1] / hydroRaster_m)
  fact <- max(1, fact)
  
  riparian_fraction <- terra::aggregate(
    rip_hi,
    fact = fact,
    fun  = "mean",
    na.rm = TRUE
  )
  
  riparian_fraction <- terra::resample(
    riparian_fraction,
    PlanningRaster,
    method = "near"
  )
  
  riparian_fraction[is.na(riparian_fraction)] <- 0
  
  
  return(riparian_fraction)
}